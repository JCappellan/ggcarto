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
# A newspaper graphic's discipline: white page, near-black title, greys for
# everything that is not data, one red accent, hairlines rather than boxes,
# and nothing on the panel that is not the map. The same tokens style the package's own
# diagnostic plots, so a report built with ggcarto looks like one document.
# ===========================================================================

#' ggcarto's colours
#'
#' The handful of colours [theme_ggcarto()] and the diagnostic plots use:
#' `paper` (the page, white), `ink` (titles, near-black), `text` (body
#' text), `muted` (captions, axes), `line` (hairlines, gridlines), `band` (a
#' faint fill), `slate` (the histogram blue), `accent` (the red that marks a
#' break or a suggestion) and `teal` (a second accent).
#'
#' @return A named character vector of hex colours.
#' @examples
#' ggcarto_colours()
#' scales::show_col(ggcarto_colours())
#' @export
ggcarto_colours <- function() {
  c(paper = "#ffffff", ink = "#121212", text = "#4a4a4a", muted = "#8c8c8c",
    line = "#e4e4e4", band = "#f4f4f4", slate = "#3f6e9c", accent = "#d0463b",
    teal = "#1f8a70")
}

#' A theme for maps
#'
#' A complete ggplot2 theme for choropleths in the manner of a good newspaper
#' graphic: a white page, a bold near-black title, a grey subtitle and a
#' small grey caption, no axes, no grid, no boxes, and a legend that sits
#' under the map at the left, where a reader starts. Colour is left to the
#' data. It is what the maps in the ggcarto tutorial use. For the typography
#' to match, set `base_family` to a clean sans (e.g. `"Helvetica Neue"`,
#' `"Inter"`, `"Source Sans 3"`) and `title_family` to a serif or a heavier
#' sans if you want the title to contrast; the device default is used
#' otherwise so nothing breaks without system fonts. `variant = "dark"` inverts it
#' for a dark page (reverse a sequential palette so its dark end stays
#' legible); `"print"` shrinks the type, moves the legend to the right and
#' tightens the margins for a figure in a two-column paper. With
#' `axes = TRUE` it keeps a quiet y axis and horizontal gridlines, which is
#' how [plot_breaks()] and [plot()][class_guide()] draw the diagnostic charts.
#'
#' Pair it with [guide_map()] for the legend's own layout (a framed, ticked
#' bar of a sensible length; keys in reading order), and with
#' `geom_sf(colour = "white", linewidth = 0.1)` for polygon borders that
#' recede.
#'
#' @param base_size Base font size in points. Title, subtitle, caption and
#'   legend text are scaled from it.
#' @param base_family Font family for all text; `""` is the device default.
#' @param title_family Font family for the title alone, e.g. a serif to
#'   contrast with a sans body. `NULL` inherits `base_family`.
#' @param legend Where the legend goes: `"bottom-left"` (default),
#'   `"bottom"`, `"bottom-right"`, `"right"`, `"left"`, `"top"`,
#'   `"top-left"`, `"top-right"`, `"inset"` (inside the panel, top left) or
#'   `"none"`. Legends along the bottom or top are laid out horizontally.
#' @param variant `"light"` (white page), `"dark"` (near-black page, light
#'   text) or `"print"` (smaller type, legend right, tight margins).
#' @param panel Background behind the map itself: `"none"` (the canvas) or
#'   `"grey"`, a light grey that makes very pale classes and white polygons
#'   visible.
#' @param axes Keep axis text and horizontal gridlines (for charts rather
#'   than maps).
#' @return A ggplot2 theme.
#' @examples
#' library(ggplot2)
#' if (requireNamespace("sf", quietly = TRUE)) {
#'   p <- ggplot(northeast_counties) +
#'     geom_sf(aes(fill = unemployment), colour = "white", linewidth = 0.1) +
#'     scale_fill_intervals("brewer::YlOrRd", "fisher", 5,
#'                          labels = label_map(style = "interval", suffix = "%", digits = 1),
#'                          guide = guide_map("keys")) +
#'     labs(title = "Unemployment rate", subtitle = "Northeast counties, ACS 2015-2019",
#'          caption = "Source: American Community Survey", fill = NULL)
#'   p + theme_ggcarto()
#'   p + theme_ggcarto(legend = "right", variant = "print")
#'   p + theme_ggcarto(variant = "dark")
#' }
#' @seealso [guide_map()], [ggcarto_colours()], [theme_palettes()]
#' @export
theme_ggcarto <- function(base_size = 11, base_family = "", title_family = NULL,
                          legend = c("bottom-left", "bottom", "bottom-right",
                                     "right", "left", "top", "top-left",
                                     "top-right", "inset", "none"),
                          variant = c("light", "dark", "print"),
                          panel = c("none", "grey"), axes = FALSE) {
  legend <- match.arg(legend)
  variant <- match.arg(variant)
  panel <- match.arg(panel)
  k <- ggcarto_colours()
  if (variant == "dark") {
    k[c("paper", "ink", "text", "muted", "line", "band")] <-
      c("#121212", "#f2f2f2", "#c9c9c9", "#8c8c8c", "#2e2e2e", "#1f1f1f")
  } else if (variant == "print") {
    base_size <- min(base_size, 10)
    if (legend == "bottom-left") legend <- "right"
  }
  panel_fill <- if (panel == "grey") (if (variant == "dark") "#1e1e1e" else "#f0f0f0") else NA
  margins <- if (variant == "print") c(8, 8, 6, 8) else c(14, 16, 12, 16)
  el <- ggplot2::element_text

  th <- ggplot2::theme_void(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = k[["paper"]], colour = NA),
      panel.background = ggplot2::element_rect(fill = panel_fill, colour = NA),
      plot.title = el(family = title_family %||% base_family, colour = k[["ink"]],
                      face = "bold", size = ggplot2::rel(1.6), hjust = 0,
                      lineheight = 1.05, margin = ggplot2::margin(b = 5)),
      plot.subtitle = el(colour = k[["text"]], size = ggplot2::rel(1), hjust = 0,
                         lineheight = 1.2, margin = ggplot2::margin(b = 12)),
      plot.caption = el(colour = k[["muted"]], size = ggplot2::rel(0.72), hjust = 0,
                        lineheight = 1.25, margin = ggplot2::margin(t = 12)),
      plot.title.position = "plot",
      plot.caption.position = "plot",
      plot.margin = ggplot2::margin(margins[1], margins[2], margins[3], margins[4]),
      legend.title = el(colour = k[["ink"]], size = ggplot2::rel(0.8), face = "bold",
                        margin = ggplot2::margin(b = 4)),
      legend.text = el(colour = k[["text"]], size = ggplot2::rel(0.78)),
      legend.key.size = grid::unit(9, "pt"),
      legend.key.spacing.x = grid::unit(12, "pt"),
      legend.background = ggplot2::element_blank(),
      legend.key = ggplot2::element_blank(),
      legend.margin = ggplot2::margin(6, 0, 0, 0),
      strip.text = el(colour = k[["ink"]], face = "bold", size = ggplot2::rel(0.85),
                      hjust = 0, margin = ggplot2::margin(4, 6, 4, 6)),
      strip.text.y = el(colour = k[["ink"]], face = "bold", size = ggplot2::rel(0.85),
                        angle = 0, hjust = 0, margin = ggplot2::margin(4, 6, 4, 6)),
      text = el(colour = k[["text"]])
    )
  if ("legend.ticks" %in% names(ggplot2::theme())) {
    th <- th + ggplot2::theme(
      legend.ticks = ggplot2::element_line(colour = k[["ink"]], linewidth = 0.4),
      legend.frame = ggplot2::element_rect(colour = k[["line"]], linewidth = 0.3)
    )
  }
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
      axis.line.x = ggplot2::element_line(colour = k[["line"]], linewidth = 0.4),
      panel.spacing = grid::unit(10, "pt")
    )
  }
  th + legend_placement(legend)
}

# Legend position as a theme fragment. ggplot2 3.5+ places an inside legend
# with legend.position = "inside"; older versions take a numeric position.
legend_placement <- function(legend) {
  horizontal <- ggplot2::theme(legend.direction = "horizontal", legend.box = "horizontal")
  vertical <- ggplot2::theme(legend.direction = "vertical")
  modern <- "legend.position.inside" %in% names(ggplot2::theme())
  switch(legend,
    "none" = ggplot2::theme(legend.position = "none"),
    "right" = vertical + ggplot2::theme(legend.position = "right", legend.justification = c(0, 1)),
    "left" = vertical + ggplot2::theme(legend.position = "left", legend.justification = c(1, 1)),
    "bottom" = horizontal + ggplot2::theme(legend.position = "bottom", legend.justification = "center"),
    "bottom-left" = horizontal + ggplot2::theme(legend.position = "bottom", legend.justification = c(0, 1)),
    "bottom-right" = horizontal + ggplot2::theme(legend.position = "bottom", legend.justification = c(1, 1)),
    "top" = horizontal + ggplot2::theme(legend.position = "top", legend.justification = "center"),
    "top-left" = horizontal + ggplot2::theme(legend.position = "top", legend.justification = c(0, 0)),
    "top-right" = horizontal + ggplot2::theme(legend.position = "top", legend.justification = c(1, 0)),
    "inset" = if (modern) {
      vertical + ggplot2::theme(legend.position = "inside",
                                legend.position.inside = c(0.02, 0.98),
                                legend.justification = c(0, 1))
    } else {
      vertical + ggplot2::theme(legend.position = c(0.02, 0.98), legend.justification = c(0, 1))
    }
  )
}

#' A legend guide for map scales
#'
#' The right ggplot2 guide for a ggcarto scale, with map-friendly defaults:
#' a framed, ticked colourbar of a sensible length for a continuous scale, a
#' stepped bar with one block per class and the limits labelled for an
#' interval scale, or one key per class in reading order. Horizontal by
#' default, to sit under the map as [theme_ggcarto()] places it.
#'
#' @param type `"bar"` (continuous colourbar), `"steps"` (stepped bar, one
#'   block per class) or `"keys"` (one key per class, for range labels).
#' @param direction `"horizontal"` or `"vertical"`. Decides which way the bar
#'   runs and, for keys, their order: vertical keys run high to low, as a
#'   column is read; horizontal keys run low to high.
#' @param length,thickness Size of the bar in centimetres along and across
#'   its direction.
#' @param title_position Where the title sits relative to the keys.
#' @param show_limits For `"steps"`, label the outer edges as well as the
#'   interior breaks.
#' @param frame,ticks Colours for the bar frame and tick marks; `NA` for none.
#' @param reverse For `"keys"`, reverse the key order; `NULL` follows
#'   `direction` as described above.
#' @param ... Passed to the underlying `guide_*()` constructor.
#' @return A ggplot2 guide, for the `guide` argument of a scale or
#'   [ggplot2::guides()].
#' @examples
#' library(ggplot2)
#' if (requireNamespace("sf", quietly = TRUE)) {
#'   ggplot(northeast_counties) +
#'     geom_sf(aes(fill = pct_bachelors), colour = "white", linewidth = 0.1) +
#'     scale_fill_cont("viridis::mako", limits = trim_limits(0.02, 0.98),
#'                     labels = label_map(suffix = "%"), guide = guide_map()) +
#'     theme_ggcarto()
#' }
#' @seealso [theme_ggcarto()]
#' @export
guide_map <- function(type = c("bar", "steps", "keys"),
                      direction = c("horizontal", "vertical"),
                      length = 6, thickness = 0.35, title_position = "top",
                      show_limits = TRUE, frame = ggcarto_colours()[["line"]],
                      ticks = ggcarto_colours()[["ink"]], reverse = NULL, ...) {
  type <- match.arg(type)
  direction <- match.arg(direction)
  if (type == "keys") {
    return(ggplot2::guide_legend(
      title.position = title_position, direction = direction,
      reverse = reverse %||% (direction == "vertical"), ...
    ))
  }
  w <- if (direction == "horizontal") length else thickness
  h <- if (direction == "horizontal") thickness else length
  modern <- "theme" %in% names(formals(ggplot2::guide_colourbar))
  size_args <- if (modern) {
    list(theme = ggplot2::theme(legend.key.width = grid::unit(w, "cm"),
                                legend.key.height = grid::unit(h, "cm")))
  } else {
    list(barwidth = grid::unit(w, "cm"), barheight = grid::unit(h, "cm"))
  }
  common <- c(list(title.position = title_position, direction = direction,
                   frame.colour = frame, ticks.colour = ticks), size_args, list(...))
  common <- common[!vapply(common, is.null, logical(1))]
  if (type == "bar") {
    do.call(ggplot2::guide_colourbar, common)
  } else {
    do.call(ggplot2::guide_coloursteps, c(common, list(even.steps = TRUE,
                                                       show.limits = show_limits)))
  }
}
