# ===========================================================================
# Label formatting
#
# Map legends have their own conventions: thousands separators, a currency or
# percent affix, "10 to 20" ranges rather than bare break values, an open
# first and last class, and a named key for missing data.
# ===========================================================================

#' Format map legend labels
#'
#' Returns a labelling function for the `labels` argument of any ggcarto or
#' ggplot2 scale. With `style = "interval"` it turns *n + 1* break points into
#' *n* range labels, the way a choropleth legend is normally written.
#'
#' @param digits Number of decimal places. `NULL` picks a sensible number from
#'   the size of the break gaps.
#' @param big.mark,decimal.mark Passed to [base::format()].
#' @param prefix,suffix Strings placed before and after each number, e.g.
#'   `prefix = "$"` or `suffix = "%"`.
#' @param scale Multiply values by this before formatting, e.g. `scale = 100`
#'   with `suffix = "%"` for proportions, or `scale = 1/1e6` with
#'   `suffix = "M"`.
#' @param style `"value"` labels each break point; `"interval"` labels the
#'   class between break points.
#' @param separator Text between the two ends of an interval.
#' @param less_than,or_more Text used for an open first and last class when
#'   `open = TRUE`.
#' @param open If `TRUE`, the first and last classes read "Under x" and
#'   "x and above" instead of showing the data extremes.
#' @param right Kept for compatibility and not used: which side of a break
#'   is closed is decided where the classes are made, by the `closure`
#'   argument of [scale_map()], [class_summary()] and
#'   [classify_bivariate()].
#' @param na_label Label used for the missing-data key.
#' @return A function of a numeric vector returning a character vector.
#' @examples
#' label_map(prefix = "$", big.mark = ",")(c(1000, 25000, 1e6))
#' label_map(style = "interval")(c(0, 10, 20, 50))
#' label_map(style = "interval", open = TRUE)(c(0, 10, 20, 50))
#' @export
label_map <- function(digits = NULL,
                      big.mark = ",",
                      decimal.mark = ".",
                      prefix = "",
                      suffix = "",
                      scale = 1,
                      style = c("value", "interval"),
                      separator = " to ",
                      less_than = "Under ",
                      or_more = " and above",
                      open = FALSE,
                      right = TRUE,
                      na_label = "Missing") {
  style <- match.arg(style)
  force(digits); force(big.mark); force(decimal.mark); force(prefix)
  force(suffix); force(scale); force(separator); force(less_than)
  force(or_more); force(open); force(right); force(na_label)

  fmt <- function(x, d = NULL) {
    x <- as.numeric(x) * scale
    d <- d %||% digits %||% auto_digits(x)
    out <- format(
      round(x, d), big.mark = big.mark, decimal.mark = decimal.mark,
      nsmall = max(d, 0), trim = TRUE, scientific = FALSE
    )
    paste0(prefix, out, suffix)
  }

  fn <- function(x) {
    if (!length(x)) return(character(0))
    keep <- !is.na(x)
    if (style == "value") {
      out <- rep(na_label, length(x))
      out[keep] <- fmt(x[keep])
      return(out)
    }
    b <- as.numeric(x[keep])
    if (length(b) < 2L) return(fmt(b))
    lab <- fmt(b, digits %||% auto_digits(b * scale))
    n <- length(b) - 1L
    out <- paste0(lab[seq_len(n)], separator, lab[seq_len(n) + 1L])
    if (open) {
      out[1] <- paste0(less_than, lab[2])
      out[n] <- paste0(lab[n], or_more)
    }
    out
  }
  class(fn) <- c("ggcarto_labeller", "function")
  attr(fn, "style") <- style
  # Exposed so ScaleBinnedMap can build "a to b" labels, which need each
  # class's lower edge -- information a labelling function never receives.
  attr(fn, "parts") <- list(
    fmt = fmt, digits = digits, scale = scale, separator = separator, open = open,
    less_than = less_than, or_more = or_more, na_label = na_label
  )
  fn
}

#' @export
print.ggcarto_labeller <- function(x, ...) {
  cat(sprintf("<ggcarto labeller: %s>\n", attr(x, "style")))
  invisible(x)
}

# ---------------------------------------------------------------------------
# Interval labels from explicit edges.
#
# One routine writes every "a to b" label in the package -- the scale legend,
# class_summary(), diagnose_map() -- so they can never disagree. `lower` and
# `upper` are the edges of each class; `parts` is the formatting list a
# label_map() labeller carries. `as_count` writes left-closed integer classes
# the way tmap does: [0, 5) becomes "0 to 4", and a class holding one integer
# is written as that integer.
# ---------------------------------------------------------------------------
format_intervals <- function(lower, upper, parts = NULL, as_count = FALSE,
                             digits = NULL) {
  p <- parts %||% attr(label_map(style = "interval"), "parts")
  n <- length(upper)
  if (!n) return(character(0))
  hi <- upper
  if (isTRUE(as_count) && n > 1L) hi[-n] <- upper[-n] - 1
  d <- digits %||% p$digits %||%
    (if (isTRUE(as_count)) 0 else auto_digits(c(lower, upper) * (p$scale %||% 1)))
  lo_s <- p$fmt(lower, d)
  hi_s <- p$fmt(hi, d)
  labs <- paste0(lo_s, p$separator, hi_s)
  single <- is.finite(lower) & is.finite(hi) & (lo_s == hi_s)
  labs[single] <- lo_s[single]
  if (isTRUE(p$open)) {
    labs[1] <- paste0(p$less_than, p$fmt(upper[1], d))
    labs[n] <- paste0(p$fmt(lower[n], d), p$or_more)
  }
  labs
}

# Choose decimals from the tightest gap between values.
auto_digits <- function(x) {
  x <- finite_num(x)
  if (length(x) < 2L) return(if (length(x) && x == round(x)) 0 else 2)
  if (all(x == round(x))) return(0)
  gap <- min(diff(sort(unique(x))))
  if (!is.finite(gap) || gap <= 0) return(2)
  max(0, min(6, ceiling(-log10(gap)) + 1))
}

resolve_labels <- function(labels, breaks_style) {
  if (is.null(labels)) {
    return(if (identical(breaks_style, "interval")) {
      label_map(style = "interval")
    } else {
      label_map(style = "value")
    })
  }
  if (is.character(labels) && length(labels) == 1L &&
      labels %in% c("value", "values", "interval", "intervals", "range", "ranges")) {
    st <- if (labels %in% c("value", "values")) "value" else "interval"
    return(label_map(style = st))
  }
  labels
}
