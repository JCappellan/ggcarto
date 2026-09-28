# ===========================================================================
# ScaleBinnedMap
#
# Why this exists.
#
# ggplot2's binned scales call their `breaks` function with the scale *limits*
# -- two numbers -- not with the data. That is fine for `pretty()`, and fatal
# for every classification algorithm a cartographer actually uses: quantile,
# natural breaks, Fisher, k-means and standard deviation all need the
# distribution, not its endpoints. Passing breaks_jenks() to
# ggplot2::scale_fill_binned() therefore produces equal-width classes and no
# warning.
#
# This ggproto keeps the trained values so the break function sees real data,
# and builds "10 to 20" interval labels, which need each class's lower edge --
# something a plain labelling function is never given.
# ===========================================================================

#' ggproto classes behind ggcarto's scales
#'
#' `ScaleBinnedMap` is a binned scale that keeps the trained data so a
#' class-interval algorithm can see the distribution, not just the limits,
#' and that writes "a to b" interval labels. `ScaleContinuousMap` is a
#' continuous scale that keeps the data so that a function `limits` (such as
#' [trim_limits()]) sees the values rather than the range, and
#' `ScaleContinuousRank` is its subclass whose legend breaks sit at quantiles
#' of the data, which is the only place a rank scale's ticks are honest. You
#' will not normally use any of them directly; [scale_map()] does.
#'
#' @format ggproto objects.
#' @usage NULL
#' @name ggcarto-ggproto
#' @keywords internal
#' @export
ScaleBinnedMap <- ggplot2::ggproto(
  "ScaleBinnedMap", ggplot2::ScaleBinned,

  # fields
  map_breaks_fn = NULL,     # function(x) -> numeric breaks, uses the data
  frozen_breaks = NULL,     # computed once, then reused
  data_values = NULL,       # trained values, subsampled
  max_values = 200000L,     # cap on retained values
  interval_labels = FALSE,  # render "a to b" instead of break values
  label_parts = NULL,       # formatting pieces from label_map()
  as_count = FALSE,         # integer classes, labelled "0 to 4", "5 to 9"
  unique_labels = FALSE,    # style = "unique": label each class by its value
  ggcarto = NULL,           # the call's choices, for diagnose_map()

  train = function(self, x) {
    if (length(x) == 0) return()
    self$range$train(x)
    if (is.function(self$map_breaks_fn)) {
      v <- x[is.finite(x)]
      keep <- self$max_values - length(self$data_values)
      if (keep > 0) {
        # Seeded, so a very large layer classifies the same way every draw.
        if (length(v) > keep) v <- with_seed(1L, sample(v, keep))
        self$data_values <- c(self$data_values, v)
      }
    }
  },

  # ggplot2 calls a function `limits` with the *range* -- two numbers -- so
  # trim_limits() would take quantiles of the minimum and maximum. Give it
  # the data instead, in data space, and hand ggplot2 the result in
  # transformed space, which is what it expects back.
  get_limits = function(self) {
    if (is.function(self$limits) && length(self$data_values)) {
      return(data_limits(self))
    }
    ggplot2::ggproto_parent(ggplot2::ScaleBinned, self)$get_limits()
  },

  get_breaks = function(self, limits = self$get_limits()) {
    if (is.function(self$map_breaks_fn)) {
      if (is.null(self$frozen_breaks)) {
        # Classify what the map will show: values squished (or censored)
        # into the limits, so `limits` never silently swallows classes.
        vals <- self$data_values
        if (length(vals) && is.function(self$oob)) {
          vals <- self$oob(vals, limits)
          vals <- vals[is.finite(vals)]
        }
        tr <- scale_transformation(self)
        # Values are trained in transformed space; classify in data space.
        if (!is.null(tr) && !identical(tr$name, "identity") && length(vals)) {
          vals <- tr$inverse(vals)
        }
        if (length(vals)) {
          b <- self$map_breaks_fn(vals)
          if (length(b) >= 2L) self$frozen_breaks <- b
        }
      }
      if (!is.null(self$frozen_breaks)) {
        # Hand ggplot2 the interior cut points only. A break sitting exactly on
        # a limit survives ggplot2's out-of-bounds filter and then draws an
        # extra, empty legend key; ggplot2 re-adds the terminal edges itself.
        self$breaks <- interior_breaks(self$frozen_breaks, self, limits)
      }
    }
    ggplot2::ggproto_parent(ggplot2::ScaleBinned, self)$get_breaks(limits)
  },

  get_labels = function(self, breaks = self$get_breaks()) {
    if (is.null(breaks) || !(isTRUE(self$interval_labels) || isTRUE(self$unique_labels))) {
      return(ggplot2::ggproto_parent(ggplot2::ScaleBinned, self)$get_labels(breaks))
    }
    p <- self$label_parts %||% attr(label_map(style = "interval"), "parts")

    tr <- scale_transformation(self)
    upper <- as.numeric(breaks)
    if (!is.null(tr) && !identical(tr$name, "identity")) upper <- tr$inverse(upper)

    if (isTRUE(self$unique_labels)) {
      # Each class holds one distinct value; the break above it is a midpoint,
      # so the value is the largest trained value below that break.
      vals <- self$data_values
      if (!is.null(tr) && !identical(tr$name, "identity")) vals <- tr$inverse(vals)
      vals <- sort(unique(vals))
      v <- vapply(upper, function(u) {
        below <- vals[vals <= u]
        if (length(below)) max(below) else NA_real_
      }, numeric(1))
      return(p$fmt(v, p$digits %||% auto_digits(v)))
    }

    lims <- self$get_limits()
    if (!is.null(tr) && !identical(tr$name, "identity")) lims <- tr$inverse(lims)

    # Every candidate edge, so each label's lower bound is the next edge down.
    edges <- sort(unique(c(lims, self$frozen_breaks, upper)))
    lower <- vapply(upper, function(v) {
      lo <- edges[edges < v]
      if (length(lo)) max(lo) else min(edges)
    }, numeric(1))

    # A label whose edges coincide is an artefact of being handed the lower
    # limit as though it were a class edge; blank it rather than print
    # "1.1 to 1.1".
    degenerate <- upper <= lower | !is.finite(lower)

    # One decimal count for every edge, so "8.0 to 10" cannot happen.
    d <- p$digits %||%
      (if (isTRUE(self$as_count)) 0 else auto_digits(c(lower, upper) * (p$scale %||% 1)))
    labs <- format_intervals(lower, upper, p, as_count = isTRUE(self$as_count),
                             digits = d)
    labs[degenerate] <- p$fmt(upper[degenerate], d)
    labs
  }
)

# ---------------------------------------------------------------------------
# ScaleContinuousRank
#
# A rank scale positions colour by a value's place in the distribution, so a
# legend tick must be positioned the same way. Ticks at evenly spaced *values*
# would sit in the wrong place and quietly misreport the map. Quantiles of the
# trained data are the breaks that land where they claim to -- and, like the
# interval styles above, they need the data rather than the limits.
# ---------------------------------------------------------------------------

#' @rdname ggcarto-ggproto
#' @format NULL
#' @usage NULL
#' @export
ScaleContinuousMap <- ggplot2::ggproto(
  "ScaleContinuousMap", ggplot2::ScaleContinuous,

  data_values = NULL,
  max_values = 200000L,
  ggcarto = NULL,

  train = function(self, x) {
    if (length(x) == 0) return()
    self$range$train(x)
    v <- x[is.finite(x)]
    keep <- self$max_values - length(self$data_values)
    if (keep > 0) {
      if (length(v) > keep) v <- with_seed(1L, sample(v, keep))
      self$data_values <- c(self$data_values, v)
    }
  },

  # As in ScaleBinnedMap: a function `limits` sees the data, not the range.
  get_limits = function(self) {
    if (is.function(self$limits) && length(self$data_values)) {
      return(data_limits(self))
    }
    ggplot2::ggproto_parent(ggplot2::ScaleContinuous, self)$get_limits()
  }
)

#' @rdname ggcarto-ggproto
#' @format NULL
#' @usage NULL
#' @export
ScaleContinuousRank <- ggplot2::ggproto(
  "ScaleContinuousRank", ScaleContinuousMap,

  n_ticks = 5L,

  get_breaks = function(self, limits = self$get_limits()) {
    if (is.waiver(self$breaks) && length(self$data_values)) {
      return(as.numeric(stats::quantile(
        self$data_values,
        probs = seq(0, 1, length.out = max(self$n_ticks, 2L)),
        names = FALSE
      )))
    }
    ggplot2::ggproto_parent(ggplot2::ScaleContinuous, self)$get_breaks(limits)
  }
)

is.waiver <- function(x) inherits(x, "waiver")

# Limits from a user function applied to the trained data in data space,
# returned in transformed space. Falls back on the range if the function
# misbehaves.
data_limits <- function(self) {
  tr <- scale_transformation(self)
  ident <- is.null(tr) || identical(tr$name, "identity")
  vals <- self$data_values
  if (!ident) vals <- tr$inverse(vals)
  lims <- tryCatch(as.numeric(self$limits(vals)), error = function(e) NULL)
  if (length(lims) != 2L || anyNA(lims)) return(self$range$range)
  lims <- sort(lims)
  if (!ident) lims <- tr$transform(lims)
  lims
}

interior_breaks <- function(b, self, limits) {
  tr <- scale_transformation(self)
  lims <- limits
  if (!is.null(tr) && !identical(tr$name, "identity")) lims <- tr$inverse(lims)
  lims <- range(lims, na.rm = TRUE)
  # ggplot2 draws one legend key per break for a binned scale, and one bin per
  # gap between breaks *within the limits*. The two counts agree only when the
  # top break sits exactly on the upper limit: a break above the limit is
  # dropped by ggplot2 (so the top class loses its key and the label below it
  # lies), and a break on the lower limit becomes a spare key. So keep the
  # strictly interior breaks and close the top class at the limit.
  inner <- b[b > lims[1] & b < lims[2]]
  sort(unique(c(inner, lims[2])))
}

# ggplot2 4.0 exposes get_transformation(); 3.x stores $trans.
scale_transformation <- function(self) {
  if (!is.null(self$get_transformation)) {
    out <- try(self$get_transformation(), silent = TRUE)
    if (!inherits(out, "try-error")) return(out)
  }
  self$trans
}
