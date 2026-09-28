# ===========================================================================
# Thin wrappers. Same engine, one word shorter, and they autocomplete next to
# ggplot2's own scales.
# ===========================================================================

#' @rdname scale_map
#' @export
scale_fill_map <- function(palette = NULL, ..., aesthetics = "fill") {
  scale_map(palette, ..., aesthetics = aesthetics)
}

#' @rdname scale_map
#' @export
scale_colour_map <- function(palette = NULL, ..., aesthetics = "colour") {
  scale_map(palette, ..., aesthetics = aesthetics)
}

#' @rdname scale_map
#' @export
scale_color_map <- scale_colour_map

# --- type-specific siblings ------------------------------------------------

#' Scales by mapping type
#'
#' Convenience wrappers over [scale_map()] with `type` fixed. `intervals`
#' classes a numeric variable, `cont` ramps it smoothly, `cat` and `ord` treat
#' it as unordered or ordered categories, and `rank` maps position in the
#' distribution.
#'
#' @inheritParams scale_map
#' @param ... Passed to [scale_map()].
#' @return A ggplot2 scale.
#' @examples
#' library(ggplot2)
#' ggplot(faithfuld, aes(waiting, eruptions, fill = density)) +
#'   geom_raster() +
#'   scale_fill_intervals("hcl::Batlow", style = "quantile", n = 6)
#' @name scale_types
NULL

#' @rdname scale_types
#' @export
scale_fill_intervals <- function(palette = NULL, style = "pretty", n = 5, ...) {
  scale_map(palette, type = "intervals", style = style, n = n, ...,
            aesthetics = "fill")
}

#' @rdname scale_types
#' @export
scale_colour_intervals <- function(palette = NULL, style = "pretty", n = 5, ...) {
  scale_map(palette, type = "intervals", style = style, n = n, ...,
            aesthetics = "colour")
}

#' @rdname scale_types
#' @export
scale_color_intervals <- scale_colour_intervals

#' @rdname scale_types
#' @export
scale_fill_cont <- function(palette = NULL, ...) {
  scale_map(palette, type = "continuous", ..., aesthetics = "fill")
}

#' @rdname scale_types
#' @export
scale_colour_cont <- function(palette = NULL, ...) {
  scale_map(palette, type = "continuous", ..., aesthetics = "colour")
}

#' @rdname scale_types
#' @export
scale_color_cont <- scale_colour_cont

#' @rdname scale_types
#' @export
scale_fill_cat <- function(palette = NULL, ...) {
  scale_map(palette, type = "categorical", ..., aesthetics = "fill")
}

#' @rdname scale_types
#' @export
scale_colour_cat <- function(palette = NULL, ...) {
  scale_map(palette, type = "categorical", ..., aesthetics = "colour")
}

#' @rdname scale_types
#' @export
scale_color_cat <- scale_colour_cat

#' @rdname scale_types
#' @export
scale_fill_ord <- function(palette = NULL, ...) {
  scale_map(palette, type = "ordinal", ..., aesthetics = "fill")
}

#' @rdname scale_types
#' @export
scale_colour_ord <- function(palette = NULL, ...) {
  scale_map(palette, type = "ordinal", ..., aesthetics = "colour")
}

#' @rdname scale_types
#' @export
scale_color_ord <- scale_colour_ord

#' @rdname scale_types
#' @export
scale_fill_rank <- function(palette = NULL, ...) {
  scale_map(palette, type = "rank", ..., aesthetics = "fill")
}

#' @rdname scale_types
#' @export
scale_colour_rank <- function(palette = NULL, ...) {
  scale_map(palette, type = "rank", ..., aesthetics = "colour")
}

#' @rdname scale_types
#' @export
scale_color_rank <- scale_colour_rank
