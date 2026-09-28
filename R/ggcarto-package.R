#' ggcarto: cartographic colour and fill scales for ggplot2
#'
#' ggcarto gives ggplot2 one consistent way to colour a variable, built around
#' the things thematic mapping actually needs.
#'
#' * **One engine.** [scale_map()] covers continuous ramps, classed intervals,
#'   categories, ordered categories and ranks. Switching between them is a
#'   change of `type`, not a rewrite.
#' * **Real class intervals.** Quantile, natural breaks, Fisher, k-means,
#'   standard deviation, head/tails, boxplot, maximum-gap and more, via
#'   [breaks_style()] and friends. They are plain functions, so they also drop
#'   into [ggplot2::scale_fill_binned()].
#' * **Palettes from everywhere.** [as_palette()] resolves grDevices,
#'   RColorBrewer, viridis, cols4all and paletteer names through one syntax,
#'   with `reverse` and `range` trimming.
#' * **Bivariate maps.** [classify_bivariate()], [scale_fill_bivariate()]
#'   and [legend_bivariate()] take two variables from raw numbers to a
#'   coloured map with a grid legend.
#' * **Map-shaped defaults.** Out-of-bounds values squish rather than vanish,
#'   [trim_limits()] tames outliers, [symmetric_limits()] keeps a diverging
#'   scale honest, and [label_map()] writes legend labels the way a legend is
#'   normally written. [theme_ggcarto()] is a complete map theme.
#' * **Defensible choices.** The package shows its working, in the order
#'   *inspect, choose, classify, map, diagnose*: [compare_breaks()] and
#'   [plot_breaks()] draw the breaks on the distribution; [class_guide()]
#'   suggests a number of classes with its reasons; [class_summary()]
#'   tabulates the classes; [check_palette()] and [plot_palette()] test a
#'   palette for map use; [diagnose_map()] reads every decision back off the
#'   finished plot and says what a reader would get wrong.
#'
#' @section Which function for what:
#' \describe{
#'   \item{Colouring a variable}{[scale_fill_map()] / [scale_colour_map()]
#'     (the engine, `type` picks the mapping); [scale_fill_intervals()],
#'     [scale_fill_cont()], [scale_fill_cat()], [scale_fill_ord()],
#'     [scale_fill_rank()] (the same engine with `type` fixed).}
#'   \item{Breaks}{[compute_breaks()] (the numbers); [breaks_fisher()],
#'     [breaks_quantile()] and the other factories (functions of the data,
#'     usable in any ggplot2 binned scale); [map_styles()],
#'     [map_styles_info()] (what exists and what each style is for).}
#'   \item{Choosing and checking a classification}{[class_guide()] (how
#'     many classes), [class_summary()] (what is in each), [plot_breaks()],
#'     [compare_breaks()] (where the breaks fall).}
#'   \item{Palettes}{[as_palette()] (any source, one syntax),
#'     [map_palettes()], [palette_sources()] (finding one), [show_palette()],
#'     [plot_palette()] (looking at one), [check_palette()] (testing one),
#'     [ggcarto_palettes()], [theme_palettes()] (project defaults).}
#'   \item{Legend and limits}{[label_map()], [na_key()], [trim_limits()],
#'     [symmetric_limits()].}
#'   \item{Bivariate maps}{[classify_bivariate()], [scale_fill_bivariate()],
#'     [bivariate_palette()], [bivariate_palettes()],
#'     [plot_bivariate_palettes()], [legend_bivariate()], [inset_legend()].}
#'   \item{The finished map}{[diagnose_map()], [diagnose_scale()],
#'     [theme_ggcarto()], [ggcarto_colours()].}
#'   \item{Data}{[northeast_counties].}
#' }
#' The vignette `vignette("ggcarto")` walks through the workflow;
#' `vignette("good-maps")` explains the conventions behind every metric.
#'
#' @keywords internal
#' @importFrom ggplot2 .data
"_PACKAGE"
