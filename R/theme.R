# ===========================================================================
# Package-wide palette defaults.
#
# ggplot2 4.0 lets a theme carry palettes (palette.fill.continuous and
# friends), so a project can set its colours once. ggcarto_palettes() sets the
# defaults this package reaches for when `palette = NULL`; theme_palettes()
# pushes the same choices into ggplot2's own default scales.
# ===========================================================================

#' Set default palettes for ggcarto
#'
#' Sets the palettes used whenever a ggcarto scale is called without an
#' explicit `palette`. Returns the previous settings invisibly, so it composes
#' with [base::on.exit()].
#'
#' @param seq,div,cat Palette specifications for sequential, diverging and
#'   categorical scales. `NULL` leaves that entry unchanged.
#' @return Invisibly, the previous list of defaults.
#' @examples
#' old <- ggcarto_palettes(seq = "hcl::Batlow", div = "brewer::BrBG")
#' scale_fill_map()
#' ggcarto_palettes(seq = old$seq, div = old$div)
#' @export
ggcarto_palettes <- function(seq = NULL, div = NULL, cat = NULL) {
  old <- getOption("ggcarto.palettes", list())
  new <- old
  if (!is.null(seq)) new$seq <- seq
  if (!is.null(div)) new$div <- div
  if (!is.null(cat)) new$cat <- cat
  options(ggcarto.palettes = new)
  invisible(old)
}

#' Carry ggcarto palettes in a theme
#'
#' On ggplot2 4.0 and later, palettes can live in the theme. This returns a
#' theme fragment that sets the colour and fill palettes for ggplot2's own
#' default scales, so unstyled layers match the ones you scale explicitly.
#'
#' @param seq,div,cat Palette specifications. `NULL` uses the current
#'   [ggcarto_palettes()] defaults.
#' @param ... Passed to [ggplot2::theme()].
#' @return A ggplot2 theme object.
#' @examples
#' library(ggplot2)
#' ggplot(mpg, aes(displ, hwy, colour = class)) +
#'   geom_point() +
#'   theme_palettes(cat = "brewer::Dark2")
#' @export
theme_palettes <- function(seq = NULL, div = NULL, cat = NULL, ...) {
  seq <- seq %||% default_palette("seq")
  cat <- cat %||% default_palette("cat")

  args <- list(...)
  supported <- names(ggplot2::theme())

  if ("palette.colour.continuous" %in% supported) {
    args$palette.colour.continuous <- scales::gradient_n_pal(as_palette(seq)(256L))
    args$palette.fill.continuous <- scales::gradient_n_pal(as_palette(seq)(256L))
    args$palette.colour.discrete <- as_palette(cat)
    args$palette.fill.discrete <- as_palette(cat)
  } else {
    warning(
      "Theme-level palettes need ggplot2 >= 4.0.0; returning an empty theme. ",
      "Use ggcarto_palettes() instead.", call. = FALSE
    )
  }
  do.call(ggplot2::theme, args)
}

# ===========================================================================
# theme_ggcarto(): a map theme
#
# Warm paper, navy titles, grey annotation, one orange accent, and nothing on
# the panel that is not the map. The same tokens style the package's own
# diagnostic plots, so a report built with ggcarto looks like one document.
# ===========================================================================

#' ggcarto's colours
#'
#' The handful of colours [theme_ggcarto()] and the diagnostic plots use:
#' `paper` (the background), `ink` (titles, the darkest text), `text`
#' (body text), `muted` (captions, axes), `line` (hairlines, gridlines),
#' `band` (a faint fill), `slate` (the histogram blue), `accent` (the orange
#' that marks a break or a suggestion) and `teal` (a second accent).
#'
#' @return A named character vector of hex colours.
#' @examples
#' ggcarto_colours()
#' scales::show_col(ggcarto_colours())
#' @export
ggcarto_colours <- function() {
  c(paper = "#f7f5ef", ink = "#1f2e46", text = "#555555", muted = "#8a8a8a",
    line = "#dcd8cd", band = "#ebe7dc", slate = "#2f4a70", accent = "#c9582b",
    teal = "#2a8a7c")
}

#' A theme for maps
#'
#' A complete ggplot2 theme for choropleths: a warm off-white background,
#' a bold navy title, grey subtitle and caption, no axes, no grid, and a
#' legend that sits under the map by default. It is what the maps in the
#' ggcarto tutorial use. With `axes = TRUE` it keeps a quiet y axis and
#' horizontal gridlines, which is how [plot_breaks()] and
#' [plot()][class_guide()] draw the diagnostic charts.
#'
#' @param base_size Base font size in points.
#' @param base_family Font family.
#' @param legend Where the legend goes: `"bottom"`, `"right"`, `"top"`,
#'   `"left"`, `"none"`, or a length-2 numeric for inside the panel.
#' @param axes Keep axis text and horizontal gridlines (for charts rather
#'   than maps).
#' @param paper Background colour; `NA` for transparent.
#' @return A ggplot2 theme.
#' @examples
#' library(ggplot2)
#' if (requireNamespace("sf", quietly = TRUE)) {
#'   ggplot(northeast_counties) +
#'     geom_sf(aes(fill = unemployment), colour = "white", linewidth = 0.1) +
#'     scale_fill_intervals("brewer::YlOrRd", "fisher", 5,
#'                          labels = label_map(style = "interval", suffix = "%",
#'                                             digits = 1, open = TRUE)) +
#'     labs(title = "Unemployment rate", subtitle = "Northeast counties, ACS 2015-2019",
#'          caption = "Source: American Community Survey", fill = NULL) +
#'     theme_ggcarto()
#' }
#' @export
theme_ggcarto <- function(base_size = 11, base_family = "", legend = "bottom",
                          axes = FALSE, paper = ggcarto_colours()[["paper"]]) {
  k <- ggcarto_colours()
  el <- ggplot2::element_text
  th <- ggplot2::theme_void(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = paper, colour = NA),
      panel.background = ggplot2::element_blank(),
      plot.title = el(colour = k[["ink"]], face = "bold", size = ggplot2::rel(1.5),
                      hjust = 0, margin = ggplot2::margin(b = 6)),
      plot.subtitle = el(colour = k[["text"]], size = ggplot2::rel(0.95), hjust = 0,
                         lineheight = 1.15, margin = ggplot2::margin(b = 10)),
      plot.caption = el(colour = k[["muted"]], size = ggplot2::rel(0.75), hjust = 0,
                        margin = ggplot2::margin(t = 10)),
      plot.title.position = "plot",
      plot.caption.position = "plot",
      plot.margin = ggplot2::margin(14, 16, 12, 16),
      legend.position = legend,
      legend.title = el(colour = k[["ink"]], size = ggplot2::rel(0.85), face = "bold"),
      legend.text = el(colour = k[["text"]], size = ggplot2::rel(0.8)),
      legend.key.size = grid::unit(10, "pt"),
      legend.key.spacing.x = grid::unit(10, "pt"),
      legend.background = ggplot2::element_blank(),
      legend.margin = ggplot2::margin(6, 0, 0, 0),
      strip.text = el(colour = k[["ink"]], face = "bold", size = ggplot2::rel(0.85),
                      hjust = 0, margin = ggplot2::margin(4, 6, 4, 6)),
      strip.text.y = el(colour = k[["ink"]], face = "bold", size = ggplot2::rel(0.85),
                        angle = 0, hjust = 0, margin = ggplot2::margin(4, 6, 4, 6)),
      text = el(colour = k[["text"]])
    )
  if (isTRUE(axes)) {
    th <- th + ggplot2::theme(
      axis.text = el(colour = k[["muted"]], size = ggplot2::rel(0.8)),
      axis.text.y = el(colour = k[["muted"]], size = ggplot2::rel(0.8), hjust = 1,
                       margin = ggplot2::margin(r = 4)),
      axis.text.x = el(colour = k[["muted"]], size = ggplot2::rel(0.8),
                       margin = ggplot2::margin(t = 4)),
      axis.title = el(colour = k[["muted"]], size = ggplot2::rel(0.8)),
      axis.title.y = el(colour = k[["muted"]], size = ggplot2::rel(0.8), angle = 90,
                        margin = ggplot2::margin(r = 6)),
      axis.title.x = el(colour = k[["muted"]], size = ggplot2::rel(0.8),
                        margin = ggplot2::margin(t = 6)),
      panel.grid.major.y = ggplot2::element_line(colour = k[["line"]], linewidth = 0.3),
      panel.spacing = grid::unit(8, "pt")
    )
  }
  th
}
