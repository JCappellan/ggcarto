# ===========================================================================
# Class-interval (break) algorithms
#
# Every breaks_*() function returns a *function of x* returning a numeric
# vector of break points (length n + 1 for n classes, including both
# endpoints). That makes them drop-in for the `breaks` argument of any
# ggplot2 binned scale, not just this package's scales:
#
#   ggplot2::scale_fill_binned(breaks = breaks_jenks(7))
# ===========================================================================

#' Available class-interval styles
#'
#' @return A character vector of style names accepted by [breaks_style()] and
#'   by the `style` argument of the ggcarto scales.
#' @examples
#' map_styles()
#' @export
map_styles <- function() {
  c("equal", "pretty", "quantile", "sd", "jenks", "fisher", "kmeans",
    "hclust", "bclust", "headtails", "box", "dpih", "maximum", "log",
    "geometric", "unique", "fixed")
}

# Styles delegated to classInt::classIntervals()
classint_styles <- function() {
  c("jenks", "fisher", "kmeans", "hclust", "bclust", "headtails", "box",
    "dpih", "maximum")
}

#' Describe a class-interval style
#'
#' A short, plain-language note on what each style does and when it is the
#' right choice. Printed by [map_styles_info()].
#'
#' @param style Optional style name. If `NULL`, all styles are returned.
#' @return A data frame with columns `style`, `family` and `description`.
#' @examples
#' map_styles_info("jenks")
#' @export
map_styles_info <- function(style = NULL) {
  info <- rbind(
    c("equal", "range", "Equal-width classes. Honest about magnitude, poor with skew."),
    c("pretty", "range", "Equal-width classes rounded to human-readable numbers."),
    c("quantile", "rank", "Equal counts per class. Maximises contrast, hides magnitude."),
    c("sd", "moment", "Classes at multiples of the standard deviation either side of the mean."),
    c("jenks", "variance", "Jenks natural breaks; minimises within-class variance."),
    c("fisher", "variance", "Fisher-Jenks; the exact optimal version of jenks, and faster."),
    c("kmeans", "variance", "1-d k-means clustering of the values."),
    c("hclust", "variance", "Hierarchical clustering of the values."),
    c("bclust", "variance", "Bagged clustering; slower, more stable than kmeans."),
    c("headtails", "tail", "Head/tails breaks (Jiang 2013) for heavy-tailed data."),
    c("box", "robust", "Boxplot breaks: hinges, median and 1.5 IQR fences."),
    c("dpih", "density", "Direct-plugin bandwidth selection for histogram breaks."),
    c("maximum", "gap", "Maximum-gap breaks; splits at the widest empty intervals."),
    c("log", "transform", "Pretty breaks on a log scale (1, 2, 5 per decade)."),
    c("geometric", "transform", "Geometric progression between the data extremes."),
    c("unique", "exact", "One class per distinct value; for coarse integer data."),
    c("fixed", "exact", "Breaks you supply yourself via `breaks`.")
  )
  out <- data.frame(
    style = info[, 1], family = info[, 2], description = info[, 3],
    stringsAsFactors = FALSE
  )
  if (!is.null(style)) {
    style <- match.arg(style, map_styles(), several.ok = TRUE)
    out <- out[out$style %in% style, , drop = FALSE]
  }
  out
}

#' Class-interval break functions
#'
#' Each of these returns a function of `x` that computes break points. They
#' work inside any ggcarto scale (via `style =`) and inside plain ggplot2
#' binned scales (via `breaks =`).
#'
#' @param n Target number of classes. Algorithms that choose their own class
#'   count (`box`, `dpih`, `headtails`, `unique`) ignore it.
#' @param step For `breaks_sd()`, the width of each class in standard
#'   deviations.
#' @param thr For `breaks_headtails()`, the head/tail proportion threshold.
#' @param base For `breaks_log()`, the logarithm base.
#' @param style A style name from [map_styles()].
#' @param ... Additional arguments passed to [classInt::classIntervals()],
#'   plus `large_n` and `seed`, which are forwarded to [compute_breaks()].
#' @return A function of one numeric argument returning a numeric vector of
#'   break points.
#' @examples
#' x <- c(rlnorm(200, 3, 1))
#' breaks_quantile(5)(x)
#' breaks_jenks(5)(x)
#' breaks_sd()(x)
#'
#' library(ggplot2)
#' ggplot(faithfuld, aes(waiting, eruptions, fill = density)) +
#'   geom_raster() +
#'   scale_fill_binned(breaks = breaks_quantile(6))
#' @name breaks
NULL

#' @rdname breaks
#' @export
breaks_style <- function(style = "pretty", n = 5, ...) {
  style <- match.arg(style, map_styles())
  dots <- list(...)
  force(n)
  fn <- function(x) compute_breaks(x, style = style, n = n, args = dots)
  class(fn) <- c("ggcarto_breaks", "function")
  attr(fn, "style") <- style
  attr(fn, "n") <- n
  fn
}

#' @rdname breaks
#' @export
breaks_equal <- function(n = 5) breaks_style("equal", n)

#' @rdname breaks
#' @export
breaks_pretty_map <- function(n = 5) breaks_style("pretty", n)

#' @rdname breaks
#' @export
breaks_quantile <- function(n = 5) breaks_style("quantile", n)

#' @rdname breaks
#' @export
breaks_sd <- function(n = 6, step = 1) breaks_style("sd", n, step = step)

#' @rdname breaks
#' @export
breaks_jenks <- function(n = 5, ...) breaks_style("jenks", n, ...)

#' @rdname breaks
#' @export
breaks_fisher <- function(n = 5, ...) breaks_style("fisher", n, ...)

#' @rdname breaks
#' @export
breaks_kmeans <- function(n = 5, ...) breaks_style("kmeans", n, ...)

#' @rdname breaks
#' @export
breaks_hclust <- function(n = 5, ...) breaks_style("hclust", n, ...)

#' @rdname breaks
#' @export
breaks_bclust <- function(n = 5, ...) breaks_style("bclust", n, ...)

#' @rdname breaks
#' @export
breaks_headtails <- function(thr = 0.4) breaks_style("headtails", n = 5, thr = thr)

#' @rdname breaks
#' @export
breaks_box <- function(...) breaks_style("box", n = 5, ...)

#' @rdname breaks
#' @export
breaks_dpih <- function(...) breaks_style("dpih", n = 5, ...)

#' @rdname breaks
#' @export
breaks_maximum <- function(n = 5, ...) breaks_style("maximum", n, ...)

#' @rdname breaks
#' @export
breaks_log <- function(n = 5, base = 10) breaks_style("log", n, base = base)

#' @rdname breaks
#' @export
breaks_geometric <- function(n = 5) breaks_style("geometric", n)

#' @rdname breaks
#' @export
breaks_unique <- function() breaks_style("unique", n = 5)

#' @export
print.ggcarto_breaks <- function(x, ...) {
  cat(sprintf("<ggcarto breaks: %s, n = %s>\n", attr(x, "style"), attr(x, "n")))
  invisible(x)
}

# ---------------------------------------------------------------------------
# Workhorse
# ---------------------------------------------------------------------------

#' Compute class-interval breaks
#'
#' The function behind every `breaks_*()` factory. Most users want the
#' factories or the `style` argument of a scale; this is exported for
#' scripting and for use outside ggplot2 entirely.
#'
#' @param x Numeric vector.
#' @param style A style name from [map_styles()].
#' @param n Target number of classes.
#' @param args A named list of extra arguments for the chosen style.
#' @param large_n How the natural-breaks styles (`"fisher"`, `"jenks"`) treat
#'   large vectors. `"exact"` (default) classifies every value. `"sample"`
#'   reproduces classInt's shortcut -- above 3,000 values the classification
#'   is computed on a random sample of 10% of the values (at most 3,000) plus
#'   the extremes -- but draws the sample with `seed`, so it is reproducible.
#'   See Details.
#' @param seed Seed for every random step a style may take: the sample under
#'   `large_n = "sample"`, the random starts of `"kmeans"` and `"bclust"`, and
#'   the subsample `"hclust"` and `"bclust"` are run on above 5,000 values. The
#'   session's random-number state is left untouched. `NULL` uses the session
#'   RNG, which makes those styles irreproducible across runs.
#'
#' @details
#' **Determinism.** Every style is reproducible by default. This differs from
#' calling [classInt::classIntervals()] directly, which above 3,000 values
#' silently computes `"fisher"` and `"jenks"` on an unseeded random sample:
#' the same county map could classify differently on every run. ggcarto
#' passes `warnLargeN = FALSE` so Fisher is exact at any size. Exact Fisher
#' costs about 0.05 s for 3,000 values, 0.3 s for 10,000 and 3 s for 30,000
#' (it is quadratic in the number of values); `large_n = "sample"` is there
#' for the rare case where that is too slow.
#'
#' `"jenks"` is classInt's R implementation of the same optimisation Fisher
#' solves exactly, and it is far slower (about 7 s for 3,000 values, 30 s for
#' 6,000). Above 3,000 values ggcarto therefore solves it with the exact
#' Fisher routine and says so once per session; the objective is identical, so
#' the result is the optimal natural-breaks classification rather than an
#' approximation of it. Prefer `style = "fisher"` from the start.
#'
#' `"hclust"` and `"bclust"` need a full distance matrix, which is quadratic
#' in memory, so above 5,000 values they are run on a seeded sample of 5,000
#' plus the extremes. `"kmeans"` runs on all values with the seed set.
#' @param midpoint For `mirror`, the value the classes are built around: zero
#'   change, the regional average, 50%.
#' @param mirror Classify a diverging variable symmetrically about
#'   `midpoint`. `"none"` (default) ignores the midpoint. `"value"` computes
#'   `n` breaks on the distances `|x - midpoint|` and reflects them, so equal
#'   colour steps mean equal data distances on both sides -- the honest
#'   default for a diverging map. `"count"` classifies each side of the
#'   midpoint separately into `n` classes, so the two sides are balanced in
#'   count rather than width, the quantile answer. With either, **`n` is the
#'   number of classes per side**, and the break set is returned whole -- an
#'   outer class one side of the data never reaches is kept, so that the
#'   palette stays centred and [class_summary()] can show it empty.
#' @param center With `mirror`, `"break"` puts a break exactly on the
#'   midpoint; `"band"` makes a neutral middle class straddling it instead,
#'   `[midpoint - w, midpoint + w]`, drawn in the palette's neutral colour.
#' @param band For `center = "band"`, the band as two values `c(lower,
#'   upper)`, or one number, the half-width. `NULL` uses the innermost
#'   mirrored break on each side.
#'
#' @return A sorted numeric vector of break points. With `mirror` it also
#'   carries attributes `mirror`, `midpoint`, `per_side` and `band`.
#' @examples
#' compute_breaks(c(1, 2, 2, 3, 8, 40), style = "quantile", n = 3)
#'
#' # exact and reproducible on a county-sized vector
#' x <- rlnorm(3200, 3, 1)
#' identical(compute_breaks(x, "fisher", 5), compute_breaks(x, "fisher", 5))
#'
#' # a change variable, three classes each side of zero
#' chg <- c(rnorm(200, 0, 20), 90, 130, -60)
#' compute_breaks(chg, "fisher", n = 3, midpoint = 0, mirror = "value")
#' compute_breaks(chg, "quantile", n = 3, midpoint = 0, mirror = "count",
#'                center = "band", band = 5)
#' @export
compute_breaks <- function(x, style = "pretty", n = 5, args = list(),
                           large_n = c("exact", "sample"), seed = 1L,
                           midpoint = NULL, mirror = c("none", "value", "count"),
                           center = c("break", "band"), band = NULL) {
  style <- match.arg(style, map_styles())
  mirror <- match.arg(mirror)
  center <- match.arg(center)
  # Factories forward everything through `args`; let large_n and seed ride
  # along there too, so breaks_fisher(5, large_n = "sample") just works.
  if (!is.null(args$large_n)) { large_n <- args$large_n; args$large_n <- NULL }
  if ("seed" %in% names(args)) { seed <- args$seed; args$seed <- NULL }
  for (a in c("midpoint", "mirror", "center", "band")) {
    if (a %in% names(args)) { assign(a, args[[a]]); args[[a]] <- NULL }
  }
  large_n <- match.arg(large_n)
  x <- finite_num(x)

  if (mirror != "none") {
    if (is.null(midpoint) || !is.finite(midpoint)) {
      abort_ggcarto("`mirror` needs a `midpoint` to mirror around.")
    }
    return(mirror_breaks(x, style, n, args, large_n, seed, midpoint, mirror,
                         center, band))
  }

  if (!length(x)) return(numeric(0))
  rng <- range(x)
  if (isTRUE(all.equal(rng[1], rng[2]))) {
    # Degenerate: a single distinct value. Produce one usable class.
    eps <- max(abs(rng[1]) * 1e-6, 1e-9)
    return(c(rng[1] - eps, rng[1] + eps))
  }

  brks <- switch(style,
    equal = seq(rng[1], rng[2], length.out = n + 1L),
    pretty = as.numeric(pretty(x, n = n)),
    quantile = as.numeric(stats::quantile(
      x, probs = seq(0, 1, length.out = n + 1L), names = FALSE, type = 7
    )),
    sd = sd_breaks(x, n = n, step = args$step %||% 1),
    log = log_breaks_impl(x, n = n, base = args$base %||% 10),
    geometric = geometric_breaks(x, n = n),
    unique = unique_breaks(x),
    fixed = {
      fb <- args$fixedBreaks %||% args$breaks
      if (is.null(fb)) abort_ggcarto("style = \"fixed\" needs `breaks` to be supplied.")
      as.numeric(fb)
    },
    classint_breaks(x, style = style, n = n, args = args, large_n = large_n,
                    seed = seed)
  )

  brks <- sort(unique(as.numeric(brks[is.finite(brks)])))

  # Guarantee the data is covered; algorithms like `pretty` may fall inside.
  # A break within floating-point reach of an extreme (exp(log(x)) is not
  # quite x) is snapped onto it rather than doubled into a one-value class.
  if (length(brks) < 2L) brks <- rng
  tol <- 1e-9 * max(diff(rng), 1)
  if (abs(brks[1] - rng[1]) <= tol) brks[1] <- rng[1]
  if (abs(brks[length(brks)] - rng[2]) <= tol) brks[length(brks)] <- rng[2]
  if (brks[1] > rng[1]) brks <- c(rng[1], brks)
  if (brks[length(brks)] < rng[2]) brks <- c(brks, rng[2])

  brks <- sort(unique(brks))
  # sd breaks carry their reference, so label_map(style = "sd") can write
  # "-0.5 to +0.5 sd" from the breaks alone.
  if (style == "sd") {
    attr(brks, "mean") <- mean(x)
    attr(brks, "sd") <- stats::sd(x)
  }
  brks
}

# Thresholds behind the large-N policy documented in ?compute_breaks.
classint_large_n <- 3000L   # classInt's own `largeN`: where it starts sampling
classint_sample_n <- 3000L  # classInt's cap on the sample it draws
cluster_max_n <- 5000L      # hclust/bclust need an n x n distance matrix

# A sample that always keeps both extremes, so the outer breaks still cover
# the data. This is classInt's own recipe, minus the unseeded RNG.
sample_with_range <- function(x, size) {
  if (length(x) <= size + 2L) return(x)
  c(range(x), sample(x, size))
}

# ---------------------------------------------------------------------------
# Mirrored breaks for diverging variables
#
# A diverging variable has one break that is not negotiable: the midpoint. No
# classification method knows that, so gainers and losers end up sharing a
# class. Here the classes are built on each side of the midpoint and joined,
# and the whole symmetric set is kept even where one side's data stop short,
# so the palette's neutral colour stays on the midpoint.
# ---------------------------------------------------------------------------
mirror_breaks <- function(x, style, n, args, large_n, seed, midpoint, mirror,
                          center, band) {
  n <- max(as.integer(n), 1L)
  if (!length(x)) return(numeric(0))
  sub_breaks <- function(v, k) {
    if (length(v) < 2L || k < 1L) return(range(v))
    compute_breaks(v, style = style, n = k, args = args, large_n = large_n, seed = seed)
  }
  band_lims <- if (!is.null(band)) {
    band <- as.numeric(band)
    if (length(band) == 1L) midpoint + c(-1, 1) * abs(band) else sort(band[1:2])
  } else NULL
  if (center == "break") band_lims <- NULL

  if (mirror == "value") {
    d <- abs(x - midpoint)
    if (is.null(band_lims) && center == "band") {
      b <- sub_breaks(d, n)
      offsets <- b[-1]
      band_lims <- midpoint + c(-1, 1) * offsets[1]
    } else if (!is.null(band_lims)) {
      w <- max(abs(band_lims - midpoint))
      band_lims <- midpoint + c(-1, 1) * w
      outer <- d[d > w]
      b <- if (length(outer) >= 2L && n > 1L) sub_breaks(outer, n - 1L) else c(w, max(d, w))
      offsets <- unique(c(w, b[-1][b[-1] > w]))
    } else {
      b <- sub_breaks(d, n)
      offsets <- b[-1]
    }
    offsets <- offsets[offsets > 0]
    brks <- c(midpoint - rev(offsets), if (is.null(band_lims)) midpoint, midpoint + offsets)
  } else {
    lower <- x[x < midpoint]; upper <- x[x >= midpoint]
    if (is.null(band_lims) && center == "band") {
      bl <- sub_breaks(lower, n); bu <- sub_breaks(upper, n)
      lo_b <- bl[-length(bl)]; hi_b <- bu[-1]
      band_lims <- c(lo_b[length(lo_b)], hi_b[1])
      brks <- c(lo_b, hi_b)
    } else if (!is.null(band_lims)) {
      lower <- x[x < band_lims[1]]; upper <- x[x > band_lims[2]]
      # each side's innermost edge is the band edge, not its own extreme
      bl <- if (n > 1L && length(lower) >= 2L) sub_breaks(lower, n - 1L) else numeric(0)
      bu <- if (n > 1L && length(upper) >= 2L) sub_breaks(upper, n - 1L) else numeric(0)
      brks <- c(bl[-length(bl)], band_lims, bu[-1])
    } else {
      bl <- sub_breaks(lower, n); bu <- sub_breaks(upper, n)
      brks <- c(bl[-length(bl)], midpoint, bu[-1])
    }
  }
  brks <- sort(unique(as.numeric(brks[is.finite(brks)])))
  # cover the data, as elsewhere, without disturbing the symmetric interior
  rng <- range(x)
  if (brks[1] > rng[1]) brks <- c(rng[1], brks)
  if (brks[length(brks)] < rng[2]) brks <- c(brks, rng[2])
  structure(brks, mirror = mirror, midpoint = midpoint, per_side = n,
            band = band_lims)
}

is_mirrored <- function(b) !is.null(attr(b, "mirror"))

classint_breaks <- function(x, style, n, args = list(), large_n = "exact",
                            seed = 1L) {
  need_pkg("classInt", sprintf("for style = \"%s\"", style))
  N <- length(x)
  xs <- x

  if (style %in% c("fisher", "jenks") && N > classint_large_n) {
    if (large_n == "sample") {
      size <- min(as.integer(ceiling(0.1 * N)), classint_sample_n)
      xs <- with_seed(seed, sample_with_range(x, size))
    } else if (style == "jenks") {
      inform_once("jenks_large_n", paste0(
        "ggcarto: style = \"jenks\" on ", fmt_n(N), " values is solved with ",
        "the exact Fisher-Jenks routine (same objective, much faster). ",
        "Use style = \"fisher\" directly, or large_n = \"sample\" for ",
        "classInt's sampled jenks. This note is shown once per session."
      ))
      style <- "fisher"
    }
  }
  if (style %in% c("hclust", "bclust") && N > cluster_max_n) {
    inform_once(paste0(style, "_large_n"), paste0(
      "ggcarto: style = \"", style, "\" needs a distance matrix, so above ",
      fmt_n(cluster_max_n), " values it is computed on a seeded sample of ",
      fmt_n(cluster_max_n), " values plus the extremes (seed = ",
      seed %||% "NULL", "). This note is shown once per session."
    ))
    xs <- with_seed(seed, sample_with_range(x, cluster_max_n))
  }

  # e1071::bclust narrates its committee members to the console otherwise.
  if (style == "bclust" && is.null(args$verbose)) args$verbose <- FALSE

  # warnLargeN = FALSE is what switches classInt's own sampling off.
  call_args <- c(list(var = xs, n = n, style = style, warnLargeN = FALSE), args)
  call_args <- call_args[!vapply(call_args, is.null, logical(1))]
  ci <- with_seed(seed, try(
    suppressWarnings(suppressMessages(
      do.call(classInt::classIntervals, call_args)
    )),
    silent = TRUE
  ))
  if (inherits(ci, "try-error")) {
    warning(sprintf(
      "style = \"%s\" failed (%s); falling back to \"quantile\".",
      style, trimws(sub("^Error[^:]*:", "", as.character(ci)))
    ), call. = FALSE)
    return(stats::quantile(x, seq(0, 1, length.out = n + 1L), names = FALSE))
  }
  as.numeric(ci$brks)
}

# Classic standard-deviation choropleth classification: breaks sit at
# mean +/- k * sd, symmetric about the mean. n - 1 interior breaks.
sd_breaks <- function(x, n = 6, step = 1) {
  m <- mean(x)
  s <- stats::sd(x)
  if (!is.finite(s) || s <= 0) return(range(x))
  n <- max(as.integer(n), 2L)
  k <- (seq_len(n - 1L) - n / 2) * step
  b <- m + k * s
  b <- b[b > min(x) & b < max(x)]
  sort(unique(c(min(x), b, max(x))))
}

# Pretty breaks on a log scale: 1, 2, 5 per decade, trimmed to the data.
log_breaks_impl <- function(x, n = 5, base = 10) {
  xp <- x[x > 0]
  if (!length(xp)) return(range(x))
  lo <- floor(log(min(xp), base))
  hi <- ceiling(log(max(xp), base))
  decades <- lo:hi
  cand <- numeric(0)
  for (mults in list(c(1, 2, 5), c(1, 5), 1)) {
    cand <- sort(unique(as.numeric(outer(mults, base^decades))))
    cand <- cand[cand > min(xp) & cand < max(xp)]
    if (length(cand) <= n - 1L) break
  }
  if (length(cand) > n - 1L) {
    idx <- unique(round(seq(1, length(cand), length.out = max(n - 1L, 1L))))
    cand <- cand[idx]
  }
  sort(unique(c(min(xp), cand, max(xp))))
}

# One class per distinct value: breaks halfway between neighbouring values,
# with the outer breaks half a (mean) gap beyond the extremes, as classInt
# does. Breaks *at* the values would put the two lowest values in one class.
unique_breaks <- function(x) {
  v <- sort(unique(x))
  if (length(v) < 2L) return(range(x))
  d <- diff(v)
  c(v[1] - mean(d) / 2, v[-length(v)] + d / 2, v[length(v)] + mean(d) / 2)
}

geometric_breaks <- function(x, n = 5) {
  xp <- x[x > 0]
  if (length(xp) < 2L) return(range(x))
  lo <- min(xp); hi <- max(xp)
  exp(seq(log(lo), log(hi), length.out = n + 1L))
}
