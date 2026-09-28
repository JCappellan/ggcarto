# ===========================================================================
# The engine. Every user-facing scale in this package funnels through
# scale_map(), so `type` is the only thing that changes when you switch
# between a smooth ramp, classed intervals, categories and ranks.
# ===========================================================================

#' Map colour and fill scales
#'
#' One scale for every way a map colours a variable. `type` chooses the
#' mapping: a smooth ramp (`"continuous"`), classed intervals
#' (`"intervals"`), unordered or ordered categories (`"categorical"`,
#' `"ordinal"`) or position in the distribution (`"rank"`). Everything else --
#' palette, limits, out-of-bounds handling, transparency, labels, missing
#' values -- works the same way across all of them.
#'
#' @param palette Palette specification. A colour vector, a function of `n`,
#'   or a name such as `"viridis::mako"`, `"brewer::RdBu"`, `"hcl::Zissou 1"`,
#'   `"c4a::hcl.blues3"` or any `"package::palette"` known to paletteer. See
#'   [as_palette()] and [map_palettes()]. `NULL` uses a sensible default for
#'   the palette type.
#' @param type One of `"continuous"`, `"intervals"`, `"categorical"`,
#'   `"ordinal"`, `"rank"`.
#' @param style Class-interval algorithm used when `type = "intervals"`. One
#'   of [map_styles()]; see [map_styles_info()] for what each does.
#' @param n Target number of classes for `type = "intervals"`.
#' @param breaks Explicit breaks. A numeric vector, a function of the data
#'   (such as [breaks_jenks()]), or `NULL` to use `style` and `n`.
#' @param midpoint Value anchored to the centre of a diverging palette. Use
#'   `midpoint = 0` with a diverging palette; `NULL` for sequential palettes.
#'   For `type = "intervals"` it also puts a class break exactly on the
#'   midpoint, and with `mirror` it is what the classes are built around.
#' @param mirror,center,band For `type = "intervals"` with a `midpoint`:
#'   classify each side of the midpoint symmetrically, with `n` classes *per
#'   side*; see [compute_breaks()]. The palette is sampled symmetrically over
#'   the whole mirrored set, so its neutral colour sits on the midpoint (or
#'   on the neutral `band`) even when one side's data stop short, and the
#'   classes one side never reaches are simply not drawn.
#' @param limits Length-2 numeric, a function of the data (see
#'   [trim_limits()], [symmetric_limits()]), or `NULL` for the data range.
#' @param oob What to do with values outside `limits`: `"squish"` (default,
#'   the cartographic convention -- clamp into the end classes), `"censor"`
#'   (render as missing) or `"keep"`. A function is also accepted.
#' @param range Length-2 numeric within `[0, 1]` trimming the palette to a
#'   sub-range, e.g. `c(0.1, 0.9)` to avoid the near-white end of a ramp.
#' @param reverse Reverse the palette.
#' @param na.value Colour for missing values.
#' @param na.label Legend label for the missing key.
#' @param na.show Show a key for missing values. Applies to categorical and
#'   ordinal scales; ggplot2's binned and continuous legends have no slot for
#'   one, so for interval and continuous scales add [na_key()] to the plot.
#' @param labels Label specification: `NULL` (sensible default), the strings
#'   `"values"`, `"ranges"` or `"sd"`, a [label_map()] result, a function, or
#'   a character vector. `"sd"` (or `label_map(style = "sd")`) writes each
#'   class as its distance from the mean, "-0.5 to +0.5 sd (24% to 35%)",
#'   which is what a `style = "sd"` classification is about; see
#'   [label_map()].
#' @param closure For `type = "intervals"`, which side of a break is closed.
#'   `"right"` (default, ggplot2's convention) puts a value equal to a break in
#'   the class below it, `(a, b]`; `"left"` puts it in the class above,
#'   `[a, b)`. It matters for integer data and for rates with a policy
#'   threshold: with breaks at 10, `closure = "left"` is what makes "10 and
#'   above" literally true.
#' @param as_count For `type = "intervals"`, treat the variable as counts:
#'   breaks are rounded to whole numbers, classes are left-closed, and
#'   interval labels read "0 to 4", "5 to 9" rather than "0 to 5", "5 to 10".
#'   Implies `closure = "left"` and `labels = "ranges"` unless labels are
#'   given.
#' @param interpolate For `type = "intervals"`, how palette colours are
#'   assigned to classes. `"even"` (default) gives each class an evenly spaced
#'   colour, as GIS software and tmap do. `"value"` picks the colour at each
#'   class's position along the data range, so wide classes sit further apart.
#' @param drop For categorical scales, drop unused factor levels.
#' @param transform A transformation name or object, e.g. `"log10"` or
#'   `"sqrt"`.
#' @param guide Guide specification. `NULL` picks a suitable default:
#'   a colourbar for continuous, evenly stepped bars for intervals, a legend
#'   for categories and for interval scales labelled with ranges.
#' @param name Legend title.
#' @param aesthetics Aesthetic(s) the scale applies to.
#' @param ... Passed through to the underlying ggplot2 scale constructor.
#'
#' @return A ggplot2 scale.
#'
#' @examples
#' library(ggplot2)
#'
#' # a smooth ramp
#' ggplot(faithfuld, aes(waiting, eruptions, fill = density)) +
#'   geom_raster() +
#'   scale_fill_map("viridis::mako")
#'
#' # the same variable in seven natural-breaks classes, range labels
#' ggplot(faithfuld, aes(waiting, eruptions, fill = density)) +
#'   geom_raster() +
#'   scale_fill_map("viridis::mako", type = "intervals",
#'                  style = "fisher", n = 7, labels = "ranges")
#'
#' # a diverging variable anchored at zero, outliers squished
#' df <- data.frame(x = 1:100, y = 1, z = c(rnorm(99), 40))
#' ggplot(df, aes(x, y, fill = z)) +
#'   geom_tile() +
#'   scale_fill_map("brewer::RdBu", midpoint = 0,
#'                  limits = trim_limits(0.02, 0.98))
#' @export
scale_map <- function(palette = NULL,
                      type = c("continuous", "intervals", "categorical",
                               "ordinal", "rank"),
                      style = "pretty",
                      n = 5,
                      breaks = NULL,
                      midpoint = NULL,
                      limits = NULL,
                      oob = "squish",
                      range = c(0, 1),
                      reverse = FALSE,
                      na.value = "grey85",
                      na.label = "Missing",
                      na.show = TRUE,
                      labels = NULL,
                      interpolate = c("even", "value"),
                      closure = c("right", "left"),
                      as_count = FALSE,
                      mirror = c("none", "value", "count"),
                      center = c("break", "band"),
                      band = NULL,
                      drop = FALSE,
                      transform = "identity",
                      guide = NULL,
                      name = ggplot2::waiver(),
                      aesthetics = "fill",
                      ...) {
  type <- match.arg(type)
  interpolate <- match.arg(interpolate)
  closure <- match.arg(closure)
  mirror <- match.arg(mirror)
  center <- match.arg(center)
  if (mirror != "none" && is.null(midpoint)) {
    abort_ggcarto("`mirror` needs a `midpoint`.")
  }
  if (isTRUE(as_count)) {
    closure <- "left"
    if (is.null(labels)) labels <- "ranges"
  }

  palette_kind <- if (!is.null(midpoint)) "div"
    else if (type %in% c("categorical", "ordinal")) "cat"
    else "seq"
  if (is.null(palette)) palette <- default_palette(palette_kind)
  pal_fn <- as_palette(palette, reverse = reverse, range = range)

  sc <- switch(type,
    continuous  = map_scale_continuous(pal_fn, midpoint, limits, oob, na.value,
                                       labels, transform, guide, name,
                                       aesthetics, ...),
    rank        = map_scale_rank(pal_fn, limits, na.value, labels, guide, name,
                                 aesthetics, ...),
    intervals   = map_scale_binned(pal_fn, style, n, breaks, midpoint, limits,
                                   oob, na.value, na.label, na.show, labels,
                                   interpolate, transform, guide, name,
                                   aesthetics, closure, as_count,
                                   mirror, center, band, ...),
    map_scale_discrete(pal_fn, type, breaks, limits, na.value, na.label,
                       na.show, labels, drop, guide, name, aesthetics, ...)
  )

  # The choices behind the scale, kept for diagnose_map(). ggplot2 clones
  # scales by inheritance, so the field survives training.
  sc$ggcarto <- list(
    type = type, palette = palette, palette_kind = palette_kind,
    pal_fn = pal_fn, style = style, n = n, breaks = breaks,
    midpoint = midpoint, limits = limits, oob = oob,
    range = range, reverse = reverse, na.value = na.value,
    na.label = na.label, interpolate = interpolate, transform = transform,
    closure = closure, as_count = isTRUE(as_count),
    mirror = mirror, center = center, band = band
  )
  sc
}

# --- continuous -----------------------------------------------------------

map_scale_continuous <- function(pal_fn, midpoint, limits, oob, na.value,
                                 labels, transform, guide, name, aesthetics,
                                 ...) {
  cols <- pal_fn(256L)
  pal <- scales::gradient_n_pal(cols)

  rescaler <- if (is.null(midpoint)) {
    scales::rescale
  } else {
    force(midpoint)
    function(x, to = c(0, 1), from = range(x, na.rm = TRUE)) {
      scales::rescale_mid(x, to = to, from = from, mid = midpoint)
    }
  }

  lab <- resolve_labels(labels, "value")
  sc <- call_scale(ggplot2::continuous_scale, c(list(
    aesthetics = aesthetics,
    palette = pal,
    name = name,
    labels = lab,
    limits = limits,
    rescaler = rescaler,
    oob = resolve_oob(oob),
    na.value = na.value,
    transform = transform,
    guide = guide %||% "colourbar",
    super = ScaleContinuousMap
  ), list(...)))
  sc$label_parts <- attr(lab, "parts")
  sc
}

# --- rank -----------------------------------------------------------------

rescale_rank <- function(x, to = c(0, 1), from = range(x, na.rm = TRUE)) {
  r <- rank(x, na.last = "keep", ties.method = "average")
  n <- sum(!is.na(x))
  if (n <= 1L) return(rep(mean(to), length(x)))
  scales::rescale(r, to = to, from = c(1, n))
}

map_scale_rank <- function(pal_fn, limits, na.value, labels, guide, name,
                           aesthetics, ...) {
  cols <- pal_fn(256L)
  call_scale(ggplot2::continuous_scale, c(list(
    aesthetics = aesthetics,
    palette = scales::gradient_n_pal(cols),
    name = name,
    labels = resolve_labels(labels, "value"),
    limits = limits,
    rescaler = rescale_rank,
    oob = scales::oob_squish,
    na.value = na.value,
    # A colourbar cannot show this scale honestly: ggplot2's bar guide places
    # ticks linearly in value, while the colours are placed by rank, so the
    # labels would point at the wrong colours. Keys carry no such assumption.
    guide = guide %||% ggplot2::guide_legend(reverse = TRUE),
    # Breaks at quantiles of the data, so the keys are quantile keys.
    super = ScaleContinuousRank
  ), list(...)))
}

# --- intervals ------------------------------------------------------------

map_scale_binned <- function(pal_fn, style, n, breaks, midpoint, limits, oob,
                             na.value, na.label, na.show, labels, interpolate,
                             transform, guide, name, aesthetics,
                             closure = "right", as_count = FALSE,
                             mirror = "none", center = "break", band = NULL, ...) {
  brk <- resolve_breaks(breaks, style, n, midpoint, mirror, center, band)
  if (isTRUE(as_count)) {
    inner <- brk
    brk <- if (is.function(inner)) function(x) count_breaks(inner(x), x)
           else count_breaks(inner, inner)
  }

  # ggplot2 hands a binned palette the rescaled midpoint of every class, so
  # length(x) is the class count. That is what lets "even" work.
  pal <- if (interpolate == "even") {
    function(x) pal_fn(length(x))
  } else {
    ramp_cols <- pal_fn(256L)
    function(x) scales::gradient_n_pal(ramp_cols)(x)
  }

  lab <- resolve_labels(labels, "value")
  wants_ranges <- inherits(lab, "ggcarto_labeller") &&
    attr(lab, "style") %in% c("interval", "sd")
  # One class per value: keys labelled with the values, not a stepped bar
  # whose ticks would sit between them.
  wants_unique <- identical(style, "unique") && is.null(breaks) &&
    (is.null(labels) || identical(labels, "values") || identical(labels, "value") ||
       (inherits(lab, "ggcarto_labeller") && identical(attr(lab, "style"), "value")))

  default_guide <- if (wants_ranges || wants_unique) {
    ggplot2::guide_legend(reverse = TRUE)
  } else {
    "coloursteps"
  }

  sc <- call_scale(ggplot2::binned_scale, c(list(
    aesthetics = aesthetics,
    palette = pal,
    name = name,
    # A placeholder; the real breaks are resolved against the trained data
    # and limits inside ScaleBinnedMap$get_breaks(), for fixed breaks too, so
    # that a break outside the data range is closed at the limit instead of
    # silently dropped (which would leave the top class without a key).
    breaks = ggplot2::waiver(),
    labels = if (wants_ranges || wants_unique) ggplot2::waiver() else lab,
    limits = limits,
    oob = resolve_oob(oob),
    na.value = na.value,
    nice.breaks = FALSE,
    right = identical(closure, "right"),
    # A stepped colourbar wants a tick at both ends; a keyed legend does not.
    show.limits = !(wants_ranges || wants_unique),
    transform = transform,
    guide = guide %||% default_guide,
    super = ScaleBinnedMap
  ), list(...)))

  sc$map_breaks_fn <- if (is.function(brk)) brk else function(x) brk
  sc$interval_labels <- wants_ranges
  sc$unique_labels <- wants_unique
  sc$label_parts <- attr(lab, "parts")
  sc$as_count <- isTRUE(as_count)
  sc$palette_fn <- pal_fn      # for symmetric sampling over mirrored breaks
  sc
}

resolve_breaks <- function(breaks, style, n, midpoint = NULL, mirror = "none",
                           center = "break", band = NULL) {
  if (is.function(breaks)) return(breaks)
  if (is.numeric(breaks)) return(sort(unique(as.numeric(breaks))))
  force(style); force(n); force(midpoint); force(mirror); force(center); force(band)
  function(x) {
    if (mirror != "none") {
      return(compute_breaks(x, style = style, n = n, midpoint = midpoint,
                            mirror = mirror, center = center, band = band))
    }
    b <- compute_breaks(x, style = style, n = n)
    if (!is.null(midpoint)) b <- anchor_breaks(b, midpoint)
    b
  }
}

# Nudge a break set so that one break sits exactly on the midpoint; without
# this a diverging palette's neutral colour lands in the middle of a class.
anchor_breaks <- function(b, mid) {
  if (!length(b) || mid <= min(b) || mid >= max(b)) return(b)
  i <- which.min(abs(b - mid))
  if (i == 1L || i == length(b)) return(sort(unique(c(b, mid))))
  b[i] <- mid
  sort(unique(b))
}

# --- categorical / ordinal ------------------------------------------------

map_scale_discrete <- function(pal_fn, type, breaks, limits, na.value,
                               na.label, na.show, labels, drop, guide, name,
                               aesthetics, ...) {
  pal <- function(n) pal_fn(n)

  lab <- labels
  if (is.null(lab) && isTRUE(na.show)) {
    force(na.label)
    lab <- function(x) {
      x <- as.character(x)
      x[is.na(x)] <- na.label
      x
    }
  }

  call_scale(ggplot2::discrete_scale, c(list(
    aesthetics = aesthetics,
    palette = pal,
    name = name,
    breaks = breaks %||% ggplot2::waiver(),
    labels = lab %||% ggplot2::waiver(),
    limits = limits,
    na.translate = isTRUE(na.show),
    na.value = na.value,
    drop = drop,
    guide = guide %||% "legend"
  ), list(...)))
}
