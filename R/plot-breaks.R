# ===========================================================================
# plot_breaks() / compare_breaks(): where the breaks fall on the distribution
#
# A choropleth legend hides the one thing that would let a reader judge the
# classification: the shape of the data. These two functions put the breaks
# back on the histogram. They are deliberately plain -- a diagnostic, not a
# figure -- and share one drawing routine so that comparing styles compares
# nothing but the breaks.
# ===========================================================================

#' See where the breaks fall
#'
#' Draws the distribution of `x` as a histogram with the class breaks on top,
#' each class shaded and annotated with its count and share of observations.
#' `plot_breaks()` shows one classification; `compare_breaks()` shows several
#' styles as facets over the same histogram, so the panels differ only in
#' where the breaks fall.
#'
#' @inheritParams class_summary
#' @param styles For `compare_breaks()`, a character vector of style names.
#' @param log_x Draw the x axis on a log10 scale. Values at or below zero are
#'   dropped from the display (with a message); the breaks themselves are
#'   unchanged.
#' @param bins Number of histogram bins.
#' @param ... Extra arguments for the style(s), passed to [compute_breaks()].
#' @return A ggplot object.
#' @examples
#' x <- rlnorm(500, 3, 1)
#' plot_breaks(x, "fisher", 5)
#' plot_breaks(x, "quantile", 5, log_x = TRUE)
#' plot_breaks(x, breaks = c(0, 10, 50, 200, 2000))
#'
#' compare_breaks(x, c("equal", "quantile", "fisher", "headtails"), n = 5)
#' @seealso [class_summary()] for the same classes as a table,
#'   [class_guide()] for choosing `n`.
#' @export
plot_breaks <- function(x, style = "fisher", n = 5, breaks = NULL,
                        closure = c("right", "left"), as_count = FALSE,
                        log_x = FALSE, bins = 50, ...) {
  closure <- match.arg(closure)
  args <- list(...)
  if (!is.null(breaks)) {
    style <- "fixed"
    args$fixedBreaks <- as.numeric(breaks)
  }
  style <- match.arg(style, map_styles())
  d <- breaks_plot_data(x, style, n, args, closure, as_count)
  d$class$panel <- d$panel_label
  draw_breaks(x, d$class, log_x = log_x, bins = bins, facet = FALSE,
              closure = if (isTRUE(as_count)) "left" else closure)
}

#' @rdname plot_breaks
#' @export
compare_breaks <- function(x, styles = c("equal", "quantile", "fisher"), n = 5,
                           closure = c("right", "left"), as_count = FALSE,
                           log_x = FALSE, bins = 50, ...) {
  closure <- match.arg(closure)
  styles <- vapply(styles, function(s) match.arg(s, map_styles()), character(1),
                   USE.NAMES = FALSE)
  args <- list(...)
  parts <- lapply(styles, function(s) {
    d <- breaks_plot_data(x, s, n, args, closure, as_count)
    d$class$panel <- d$panel_label
    d$class
  })
  cls <- do.call(rbind, parts)
  cls$panel <- factor(cls$panel, levels = unique(vapply(parts, function(p) p$panel[1], character(1))))
  draw_breaks(x, cls, log_x = log_x, bins = bins, facet = TRUE,
              closure = if (isTRUE(as_count)) "left" else closure)
}

# Per-class rectangles plus a panel title such as "fisher, 5 classes".
breaks_plot_data <- function(x, style, n, args, closure, as_count) {
  v <- finite_num(x)
  if (!length(v)) abort_ggcarto("`x` has no finite values to plot.")
  b <- compute_breaks(v, style = style, n = n, args = args)
  if (isTRUE(as_count)) b <- count_breaks(b, v)
  tab <- class_table(v, b, closure = if (isTRUE(as_count)) "left" else closure,
                     as_count = as_count)
  k <- nrow(tab)
  label <- if (k == 1L) paste0(style, ", 1 class") else paste0(style, ", ", k, " classes")
  if (style == "fixed") label <- paste0("fixed breaks, ", k, " classes")
  tab$style <- style
  list(class = tab, panel_label = label, breaks = b)
}

draw_breaks <- function(x, cls, log_x = FALSE, bins = 50, facet = FALSE,
                        closure = "right") {
  v <- finite_num(x)
  if (isTRUE(log_x)) {
    bad <- sum(v <= 0)
    if (bad) {
      message(sprintf("plot_breaks: %s value(s) <= 0 not shown on the log axis.", fmt_n(bad)))
      v <- v[v > 0]
    }
    cls <- cls[cls$upper > 0, , drop = FALSE]
    cls$lower[cls$lower <= 0] <- min(v)
  }
  hist_df <- data.frame(x = v)
  cls$band <- factor(cls$class %% 2L)
  cls$mid <- if (isTRUE(log_x)) sqrt(cls$lower * cls$upper) else (cls$lower + cls$upper) / 2
  cls$note <- paste0(fmt_n(cls$n), "\n", sprintf("%.0f%%", cls$pct))
  # Interior breaks only; the extremes are the data range and add nothing.
  brk <- cls[, c("panel", "upper"), drop = FALSE]
  brk <- brk[stats::ave(seq_len(nrow(brk)), brk$panel, FUN = function(i) seq_along(i) < length(i)) == 1, , drop = FALSE]

  # Integer data with a modest range: one bar per value, aligned with the
  # closure, so a bar never straddles a break.
  integer_like <- !isTRUE(log_x) && all(v == round(v)) && diff(range(v)) <= bins
  hist_layer <- if (integer_like) {
    ggplot2::geom_histogram(
      data = hist_df, ggplot2::aes(x = .data$x), binwidth = 1, boundary = 0,
      closed = closure, fill = ggcarto_colours()[["slate"]], colour = NA
    )
  } else {
    ggplot2::geom_histogram(
      data = hist_df, ggplot2::aes(x = .data$x), bins = bins,
      fill = ggcarto_colours()[["slate"]], colour = NA
    )
  }

  k <- ggcarto_colours()
  p <- ggplot2::ggplot() +
    ggplot2::geom_rect(
      data = cls,
      ggplot2::aes(xmin = .data$lower, xmax = .data$upper, ymin = -Inf, ymax = Inf,
                   fill = .data$band),
      colour = NA
    ) +
    ggplot2::scale_fill_manual(values = c("0" = NA, "1" = k[["band"]]),
                               guide = "none", na.value = NA) +
    hist_layer +
    ggplot2::geom_vline(
      data = brk, ggplot2::aes(xintercept = .data$upper),
      colour = k[["accent"]], linewidth = 0.5
    ) +
    ggplot2::geom_text(
      data = cls, ggplot2::aes(x = .data$mid, y = Inf, label = .data$note),
      vjust = 1.3, size = 2.8, colour = k[["ink"]], lineheight = 0.95,
      check_overlap = TRUE
    ) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.3))) +
    ggplot2::labs(x = NULL, y = NULL) +
    theme_breaks()

  if (isTRUE(log_x)) {
    p <- p + ggplot2::scale_x_log10(labels = scales::label_comma(),
                                    expand = ggplot2::expansion(mult = 0.01))
  } else {
    p <- p + ggplot2::scale_x_continuous(labels = scales::label_comma(),
                                         expand = ggplot2::expansion(mult = 0.01))
  }
  if (isTRUE(facet)) {
    p <- p + ggplot2::facet_wrap(~ panel, ncol = 1, strip.position = "right")
  } else {
    p <- p + ggplot2::labs(subtitle = as.character(cls$panel[1]))
  }
  p
}

# The chart flavour of theme_ggcarto(): axes on, subtitle in the title weight.
theme_breaks <- function() {
  k <- ggcarto_colours()
  theme_ggcarto(axes = TRUE) +
    ggplot2::theme(
      plot.subtitle = ggplot2::element_text(colour = k[["ink"]], face = "bold",
                                            size = ggplot2::rel(0.95), hjust = 0)
    )
}
