# ===========================================================================
# class_summary(): the numbers behind a classification
#
# A legend shows the classes; it does not show how many observations each one
# holds, and that is usually the first thing a reader of the map would want to
# know. class_summary() runs the same break engine the scales use and tabulates
# the result, so what it reports is what the map draws.
# ===========================================================================

#' Summarise a classification
#'
#' Classes `x` with a ggcarto style and reports, per class, the bounds, the
#' legend label, the count and share of observations, and the observed
#' minimum and maximum. Uses [compute_breaks()], so the classes are exactly
#' the ones [scale_fill_intervals()] would draw.
#'
#' @param x Numeric vector.
#' @param style A style name from [map_styles()]. Ignored when `breaks` is
#'   given.
#' @param n Target number of classes. Styles that choose their own count
#'   (`headtails`, `box`, `dpih`, `unique`) ignore it.
#' @param breaks Explicit break points, which make `style = "fixed"`.
#' @param closure Which side of a break is closed: `"right"` (default, as in
#'   ggplot2's binned scales) puts a value equal to a break in the class
#'   *below* it, `(a, b]`; `"left"` puts it in the class above, `[a, b)`. The
#'   outermost classes are closed on both sides either way. This is where a
#'   policy threshold such as "10 or more" is decided, so choose it
#'   deliberately for integer data and rates with meaningful cut-offs.
#' @param as_count Treat `x` as counts (0, 1, 2, ...): breaks are rounded to
#'   integers, classes are left-closed, and labels read "0 to 4", "5 to 9"
#'   instead of "0 to 5", "5 to 10". Implies `closure = "left"`.
#' @param labels A [label_map()] labeller used to write the interval labels.
#'   `NULL` uses `label_map(style = "interval", open = TRUE)`.
#' @param ... Extra arguments for the style, passed to [compute_breaks()]
#'   (`step` for `sd`, `thr` for `headtails`, `large_n`, `seed`, ...).
#' @return A data frame of class `ggcarto_class_summary` with one row per
#'   class and columns `class`, `lower`, `upper`, `label`, `n`, `pct`,
#'   `cum_pct`, `min` and `max`. Attributes carry the whole picture: `style`,
#'   `n_requested`, `n_classes`, `breaks`, `n_total`, `n_valid`, `n_missing`,
#'   `n_nonfinite`, `closure` and `as_count`. Print it for a compact report;
#'   `as.data.frame()` gives the bare table.
#' @examples
#' x <- c(rlnorm(300, 3, 1), NA, NA)
#' class_summary(x, "fisher", 5)
#' class_summary(x, "quantile", 4, labels = label_map(digits = 0))
#'
#' # counts: integer breaks, "0 to 4" style labels
#' k <- rpois(200, 3)
#' class_summary(k, "quantile", 4, as_count = TRUE)
#'
#' # the table underneath
#' s <- class_summary(x, "equal", 5)
#' as.data.frame(s)
#' attr(s, "breaks")
#' @seealso [class_guide()] to choose `n`, [plot_breaks()] to see the breaks
#'   on the distribution, [diagnose_map()] for the same numbers read off a
#'   finished plot.
#' @export
class_summary <- function(x, style = "fisher", n = 5, breaks = NULL,
                          closure = c("right", "left"), as_count = FALSE,
                          labels = NULL, ...) {
  closure <- match.arg(closure)
  if (isTRUE(as_count)) closure <- "left"
  args <- list(...)
  if (!is.null(breaks)) {
    style <- "fixed"
    args$fixedBreaks <- as.numeric(breaks)
  }
  style <- match.arg(style, map_styles())
  v <- suppressWarnings(as.numeric(x))
  fin <- v[is.finite(v)]

  b <- if (length(fin)) compute_breaks(fin, style = style, n = n, args = args) else numeric(0)
  if (isTRUE(as_count)) b <- count_breaks(b, fin)

  tab <- class_table(fin, b, closure = closure, as_count = as_count,
                     labels = labels)
  # A unique-values class holds exactly one value; say which.
  if (style == "unique" && is.null(labels) && nrow(tab)) {
    tab$label <- (label_map())(tab$min)
  }
  structure(
    tab,
    class = c("ggcarto_class_summary", "data.frame"),
    style = style,
    n_requested = as.integer(n),
    n_classes = nrow(tab),
    breaks = b,
    n_total = length(v),
    n_valid = length(fin),
    n_missing = sum(is.na(v)),
    n_nonfinite = sum(is.infinite(v)),
    closure = closure,
    as_count = isTRUE(as_count)
  )
}

# Integer breaks for count data. Interior breaks are rounded up so that a
# left-closed class [a, b) starts on a whole number; the ends stay on the
# data extremes, which are integers already.
count_breaks <- function(b, x) {
  if (length(b) < 2L) return(b)
  k <- length(b)
  inner <- ceiling(b[-c(1L, k)])
  b <- c(floor(b[1]), inner, ceiling(b[k]))
  sort(unique(b))
}

# The table: one row per class between consecutive breaks. Shared by
# class_summary(), class_guide() and diagnose_scale().
class_table <- function(x, breaks, closure = "right", as_count = FALSE,
                        labels = NULL) {
  empty <- data.frame(
    class = integer(0), lower = numeric(0), upper = numeric(0),
    label = character(0), n = integer(0), pct = numeric(0),
    cum_pct = numeric(0), min = numeric(0), max = numeric(0),
    stringsAsFactors = FALSE
  )
  if (length(breaks) < 2L || !length(x)) return(empty)

  lab_fn <- labels %||% label_map(style = "interval", open = TRUE)
  parts <- attr(lab_fn, "parts") %||% attr(label_map(style = "interval"), "parts")

  # A constant vector arrives with breaks v -/+ eps; report it as one class
  # at the value itself rather than "5 to 5".
  constant <- length(breaks) == 2L && isTRUE(all.equal(min(x), max(x)))
  if (constant) {
    v <- x[1]
    return(data.frame(
      class = 1L, lower = v, upper = v, label = parts$fmt(v),
      n = length(x), pct = 100, cum_pct = 100, min = v, max = v,
      stringsAsFactors = FALSE
    ))
  }

  k <- length(breaks) - 1L
  cls <- assign_classes(x, breaks, closure)
  counts <- tabulate(cls, nbins = k)
  lower <- breaks[-(k + 1L)]
  upper <- breaks[-1L]
  labs <- if (is.function(lab_fn) && !inherits(lab_fn, "ggcarto_labeller")) {
    # an arbitrary function of the breaks: n + 1 values -> n labels, or
    # n + 1 labels of which we drop the last
    out <- lab_fn(breaks)
    if (length(out) == k + 1L) out[-1L] else rep_len(out, k)
  } else {
    format_intervals(lower, upper, parts, as_count = as_count)
  }
  mins <- vapply(seq_len(k), function(i) if (counts[i]) min(x[cls == i]) else NA_real_, numeric(1))
  maxs <- vapply(seq_len(k), function(i) if (counts[i]) max(x[cls == i]) else NA_real_, numeric(1))
  pct <- 100 * counts / length(x)
  data.frame(
    class = seq_len(k), lower = lower, upper = upper, label = labs,
    n = counts, pct = pct, cum_pct = cumsum(pct), min = mins, max = maxs,
    stringsAsFactors = FALSE
  )
}

#' @export
as.data.frame.ggcarto_class_summary <- function(x, ...) {
  out <- x
  attributes(out) <- attributes(out)[c("names", "row.names")]
  class(out) <- "data.frame"
  out
}

#' @export
print.ggcarto_class_summary <- function(x, ...) {
  a <- attributes(x)
  cat("ggcarto class summary\n")
  cat("Method: ", style_label(a$style), "\n", sep = "")
  obs <- paste0("Observations: ", fmt_n(a$n_valid))
  extra <- character(0)
  if (a$n_missing) extra <- c(extra, paste0(fmt_n(a$n_missing), " missing"))
  if (a$n_nonfinite) extra <- c(extra, paste0(fmt_n(a$n_nonfinite), " non-finite"))
  if (length(extra)) obs <- paste0(obs, " (", paste(extra, collapse = ", "), ")")
  cat(obs, "\n", sep = "")
  cls <- paste0("Classes: ", a$n_classes)
  if (a$style %in% c("headtails", "box", "dpih", "unique", "fixed")) {
    cls <- paste0(cls, " (chosen by the method)")
  } else if (a$n_classes != a$n_requested) {
    cls <- paste0(cls, " (requested ", a$n_requested, ")")
  }
  cat(cls, "\n", sep = "")
  if (length(a$breaks)) {
    cat("Breaks: ", paste(signif(a$breaks, 4), collapse = ", "), "\n", sep = "")
  }
  cat("Closure: ", if (a$closure == "right") "(a, b]" else "[a, b)",
      if (isTRUE(a$as_count)) ", counts" else "", "\n", sep = "")
  if (!nrow(x)) {
    cat("\nNo valid observations.\n")
    return(invisible(x))
  }
  cat("\n")
  print_class_table(x)
  invisible(x)
}

print_class_table <- function(tab, indent = "  ") {
  lab <- format(c("Class", tab$label))
  n <- format(c("n", fmt_n(tab$n)), justify = "right")
  pct <- format(c("%", sprintf("%.1f", tab$pct)), justify = "right")
  cum <- format(c("cum %", sprintf("%.1f", tab$cum_pct)), justify = "right")
  lines <- paste(indent, lab, " ", n, " ", pct, " ", cum)
  cat(lines, sep = "\n")
}
