# ===========================================================================
# A "no data" legend key for scales that cannot draw one.
#
# ggplot2's discrete scales put an NA key in the legend (na.translate). Its
# continuous and binned scales do not: a colour bar and a stepped bar have no
# slot for it, and guide_legend() on a binned scale only lists the breaks. A
# map needs the key regardless -- grey must be read as "no data", not "low".
#
# na_key() adds it through a side door: an invisible point layer carrying a
# throwaway `shape` mapping, whose legend key is overridden to draw a filled
# square. The point sits at (Inf, Inf) with alpha 0, so nothing is drawn on the
# map itself.
# ===========================================================================

#' Add a "no data" key to the legend
#'
#' ggplot2 draws an NA key only for discrete scales. For interval and
#' continuous scales, add this after the scale to get a filled square labelled
#' "No data" (or whatever you say) beneath the main legend.
#'
#' @param label Legend text.
#' @param fill Colour of the key. Match the scale's `na.value`.
#' @param colour Outline colour of the key, or `NA` for none.
#' @param order Legend order; higher numbers sit lower. `99` puts it last.
#' @return A list of ggplot2 components to add with `+`.
#' @examples
#' library(ggplot2)
#' df <- data.frame(x = 1:10, y = 1, z = c(1:9, NA))
#' ggplot(df, aes(x, y, fill = z)) +
#'   geom_tile() +
#'   scale_fill_intervals("viridis::mako", n = 3, na.value = "grey85") +
#'   na_key("No data")
#' @export
na_key <- function(label = "No data", fill = "grey85", colour = NA, order = 99) {
  force(label); force(fill); force(colour)
  list(
    ggplot2::geom_point(
      data = data.frame(.x = Inf, .y = Inf),
      ggplot2::aes(x = .data$.x, y = .data$.y, shape = label),
      inherit.aes = FALSE, alpha = 0, show.legend = TRUE, key_glyph = "rect",
      na.rm = TRUE
    ),
    ggplot2::scale_shape_manual(
      values = stats::setNames(15, label), name = NULL,
      guide = ggplot2::guide_legend(
        order = order,
        override.aes = list(fill = fill, colour = colour, alpha = 1, linewidth = 0)
      )
    )
  )
}
