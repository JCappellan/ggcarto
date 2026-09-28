# ggcarto

<!-- badges: start -->
[![R-CMD-check](https://github.com/jcapellan/ggcarto/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/jcapellan/ggcarto/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

**Cartographic class intervals, palettes and bivariate maps for ggplot2.**

ggplot2 has excellent scales. It does not have the thing a map needs: a way to
say *"seven natural-breaks classes, a diverging ramp anchored at zero, outliers
squished into the end classes, legend keys reading `$10,000 to $25,000`, and
grey for the counties with no data."* Today that is a hand-rolled
`classInt::classIntervals()` call, a `cut()`, a `scale_fill_manual()`, and a
legend you fix by hand.

ggcarto is that missing layer. It also does bivariate choropleths -- two
variables, one map, a grid legend -- without leaving ggplot2.

```r
ggplot(counties) +
  geom_sf(aes(fill = median_income)) +
  scale_fill_intervals("brewer::BuPu", style = "jenks", n = 7,
                       labels = label_map(prefix = "$", style = "interval"),
                       na.value = "grey90") +
  na_key("No data", fill = "grey90")
```

## Why it exists

ggplot2 hands a binned scale's `breaks` function only the **limits** — two
numbers. Every classification algorithm a cartographer uses needs the
*distribution*. So this,

```r
ggplot2::scale_fill_binned(breaks = breaks_jenks(7))   # looks right
```

quietly produces equal-width classes, with no warning. ggcarto ships a
`ScaleBinnedMap` ggproto that retains the trained values, so `style = "jenks"`
actually means jenks. That one fix is most of the package's reason to exist;
everything else is the ergonomics around it.

## Defend the choices

Every decision inside that scale can be inspected before the map is drawn and
read back off it afterwards. The package is organised around one sequence:
**inspect → choose → classify → map → diagnose**.

```r
x <- northeast_counties$median_income      # bundled sf data, 217 counties

compare_breaks(x, c("equal", "quantile", "fisher"), n = 5)   # inspect
class_guide(x, style = "fisher")                              # choose n
class_summary(x, "fisher", 5)                                 # classify
plot_breaks(x, "fisher", 5)

map <- ggplot(northeast_counties) +                           # map
  geom_sf(aes(fill = median_income)) +
  scale_fill_intervals("brewer::YlGnBu", "fisher", 5, range = c(0.15, 1),
                       labels = label_map(style = "interval", prefix = "$",
                                          big.mark = ",", digits = 0, open = TRUE))

diagnose_map(map)                                             # diagnose
check_palette("brewer::YlGnBu", 5)
```

`class_guide()` makes a *suggestion*, with its reasons and the metrics behind
them; it does not claim an optimum, because there is none. `diagnose_map()`
reports the classes drawn and their counts, the palette and its type, limits,
squished values, missing data and the legend labels, and flags what a reader
would get wrong. Classification is exact and reproducible at any size --
classInt's silent sampling above 3,000 values is switched off.

## Install

```r
# install.packages("remotes")
remotes::install_github("jcapellan/ggcarto")
```

Requires ggplot2 4.0.0 or later, and classInt. `cols4all` and `paletteer` are
optional and unlock several thousand more palettes.

## One engine, five mappings

`scale_fill_map()` / `scale_colour_map()` cover everything; `type` is the only
thing that changes when you switch how a variable is read.

| `type` | What it does | Shorthand |
|---|---|---|
| `"continuous"` | smooth ramp | `scale_fill_cont()` |
| `"intervals"` | classed into `n` bins by `style` | `scale_fill_intervals()` |
| `"categorical"` | unordered categories | `scale_fill_cat()` |
| `"ordinal"` | ordered categories | `scale_fill_ord()` |
| `"rank"` | position in the distribution | `scale_fill_rank()` |

```r
# swapping mapping is a one-word edit, not a rewrite
scale_fill_map("viridis::mako", type = "continuous")
scale_fill_map("viridis::mako", type = "intervals", style = "fisher", n = 7)
```

## Class intervals

`style =` accepts any of these; `map_styles_info()` explains each one.

| Family | Styles |
|---|---|
| Range | `equal`, `pretty` |
| Rank | `quantile` |
| Moment | `sd` (classes at multiples of the standard deviation) |
| Variance | `jenks`, `fisher`, `kmeans`, `hclust`, `bclust` |
| Tail | `headtails` (Jiang 2013, for heavy-tailed data) |
| Robust | `box` (hinges, median, 1.5 IQR fences) |
| Density | `dpih` |
| Gap | `maximum` |
| Transform | `log`, `geometric` |
| Exact | `unique`, `fixed` |

Each is also a standalone function of the data, so it drops into **anyone's**
binned scale:

```r
ggplot2::scale_fill_binned(breaks = breaks_quantile(6))
breaks_jenks(7)(x)        # just the numbers
compute_breaks(x, "sd", n = 6)
```

## Palettes from everywhere, one syntax

```r
as_palette("hcl::Zissou 1")       # grDevices, ~110 palettes, no dependencies
as_palette("brewer::RdBu")        # RColorBrewer
as_palette("viridis::mako")       # viridisLite
as_palette("c4a::hcl.blues3")     # cols4all (the palettes behind tmap)
as_palette("scico::roma")         # anything paletteer knows, ~2,500 palettes
as_palette(c("#440154", "#fde725"))
```

With `reverse`, and `range` for trimming off the washed-out end of a
sequential ramp:

```r
scale_fill_map("brewer::YlOrRd", range = c(0.15, 1))
```

`map_palettes()` searches them; `show_palette()` draws one.

## The map-shaped details

```r
scale_fill_map(
  "brewer::RdBu",
  midpoint = 0,                       # anchor the neutral colour, and put a
                                      #   class break exactly on it
  limits   = trim_limits(0.02, 0.98), # let two outliers not eat the ramp
  oob      = "squish",                # (the default) clamp, don't drop
  na.value = "grey90",
  na.label = "No data",
  labels   = label_map(prefix = "$", big.mark = ",", style = "interval",
                       open = TRUE)   # "Under $10,000" ... "$95,000 and above"
)
```

`symmetric_limits(0)` keeps a diverging scale honest — equal data distance
either side of the midpoint gets equal colour distance.

`interpolate` decides how colours attach to classes: `"even"` (default) gives
every class an evenly spaced colour the way GIS software does; `"value"` places
each class's colour at its position along the data range, so a wide class sits
further along the ramp.

## Bivariate maps

Two variables on one map: class each into `n` intervals, colour the
`n x n` combinations from a palette grid, and draw the grid as the legend.

```r
library(dplyr)
counties <- counties |>
  mutate(cls = classify_bivariate(median_income, pct_degree, n = 3,
                                  style = "quantile"))

map <- ggplot(counties) +
  geom_sf(aes(fill = cls), colour = "white", linewidth = 0.1) +
  scale_fill_bivariate("purple-green", n = 3) +
  theme_void()

legend <- legend_bivariate("purple-green", classes = counties$cls,
                           style = "diamond", ends = FALSE,
                           xlab = "Income", ylab = "% with a degree",
                           labels = label_map(prefix = "$", big.mark = ","))

inset_legend(map, legend, position = "bottomright", width = 0.35)
```

`classes = ` hands the legend the break points, so its edges are labelled
with the real values. `style = "diamond"` is the rotated grid newspapers use;
`style = "square"` has an arrowed axis on each side; `corner_labels = ` swaps
the numbers for a phrase at each corner ("High diversity, big increase"). Palettes come from `bivariate_palettes()`, from four
corner colours, or from any two sequential ramps:

```r
bivariate_palette("blue-red", n = 3)
bivariate_palette(c("#f3f3f3", "steelblue", "firebrick", "#2b1b2b"), n = 4)
bivariate_palette(list(x = "brewer::Blues", y = "brewer::Reds"), n = 3)
```

## Project defaults

```r
ggcarto_palettes(seq = "hcl::Batlow", div = "brewer::BrBG", cat = "brewer::Dark2")
# ... and push the same choices into ggplot2's own default scales
last_plot() + theme_palettes()
```

## Diverging variables, honestly

```r
# three classes each side of zero, palette centred on zero, legend open-ended
scale_fill_intervals("brewer::BrBG", style = "pretty", n = 3,
                     midpoint = 0, mirror = "value",
                     labels = label_map(suffix = "%"))
```

`mirror` builds the classes symmetrically about the midpoint and keeps the
whole set, so the neutral colour sits on zero even when losers stop at -40%
and gainers run to +200%. A colourbar whose limits squish data labels its
ends "100% or more" without being asked (`label_map(open = "auto")`).

## Look

`theme_ggcarto()` is a complete map theme -- warm paper, navy title, grey
annotation, legend where the reader starts, with dark and print variants --
and `guide_map()` sizes the legend for it. The package's diagnostic charts
are drawn in the same style. Bivariate maps get 17 corner presets plus the
Stevens 3 x 3 grids (`plot_bivariate_palettes()` shows them all).

## Vignette and tutorial

`vignettes/good-maps.qmd` explains what a good choropleth has and which
metric checks each convention. `vignettes/ggcarto.qmd` opens with the inspect-to-diagnose workflow on the
bundled `northeast_counties` and then works through every decision on a second
dataset (437 midwestern counties from `ggplot2::midwest`). Render it with
`quarto render vignettes/ggcarto.qmd`. `docs/ggcarto-tutorial.qmd` is the long
form: the same sequence on the Northeast with `geom_sf()`, with the cartography
behind each step and its references.

## Status

v0.4.0. The API is settled enough to use and not yet settled enough to promise.
Not yet implemented: `size`/`linewidth` visual variables, an NA key on
continuous colourbars, a native `guide_bivariate()` (the inset legend stays
the recommended route), and a `guide_map()` with tmap-style legend layout
options.

## Prior art, and what it does differently

`paletteer` and `cols4all` solved palette *access*, and ggcarto reads from
both rather than re-collecting anything. `tmap` solved classification, but only
inside tmap; ggcarto borrows its ideas (one scale, many mapping types; class
intervals as a first-class argument) with its own function names, inside
ordinary ggplot2. `classInt` computes the breaks, but leaves you to wire them
into a scale and write the legend yourself. `biscale` does bivariate maps well
and was the reference for the legend design here; ggcarto's version shares the
package's break styles and palette syntax so a bivariate map is not a separate
workflow.

## Licence

MIT.
