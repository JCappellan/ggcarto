# ggcarto 0.4.0

The map tells the reader what the scale did to the data: where the classes
sit around "no change", where the ramp was capped, and how the page is laid
out.

## Breaking

* The `alpha` argument has been removed from `scale_map()`, its wrappers and
  `as_palette()`. Transparency belongs to the layer (`geom_sf(alpha = )`),
  not the scale; a translucent legend key misreports the map.
* `label_map()` now defaults to `open = "auto"`: inside a ggcarto scale an
  end class or limit tick opens ("$107k and above", "-100% or less") exactly
  when values were squished past that limit. Legends of maps with `limits`
  will change wording; that is the point. `open = FALSE` restores the old
  labels. Outside a scale `"auto"` is closed, so `label_map()` used directly
  is unchanged.

## Diverging variables

* `mirror = c("none", "value", "count")` on `compute_breaks()`,
  `scale_map()` (intervals), `class_summary()`, `class_guide()`,
  `plot_breaks()` and `compare_breaks()`: classes built symmetrically about
  `midpoint`, `n` per side, with `center = "band"` and `band` for a neutral
  middle class. The scale samples the palette over the whole mirrored set
  (a new `map()` on `ScaleBinnedMap`), so the neutral colour stays on the
  midpoint even when one side's data stop short; the classes that side
  never reaches are kept, reported by `class_summary()` and
  `diagnose_map()`, and exempt from `class_guide()`'s balance rules.

## Standard-deviation classes

* `label_map(style = "sd")`, and the shorthand `labels = "sd"` in the
  interval scales and `class_summary()`, label each class by its distance
  from the mean -- "-0.5 to +0.5 sd (25% to 36%)" -- so a `style = "sd"`
  map says what it classified, instead of printing the cut points as
  though they were chosen values. The mean and sd come from the data the
  scale classified; breaks from `compute_breaks(style = "sd")` carry them
  as attributes so the labeller works on its own too.

## Layout

* `theme_ggcarto()`: a complete map theme in the manner of a newspaper graphic -- white page, near-black title, grey
  annotation, no axes, legend bottom-left -- with `legend` positions
  (including `"inset"`), `variant = "dark"` and `"print"`, `panel = "grey"`
  and `title_family`. Its colours are `ggcarto_colours()`, and the
  diagnostic charts are drawn in the same style.
* `guide_map()`: a framed, ticked colourbar or stepped bar of a sensible
  length, or keys in reading order, sized for the theme's horizontal legend.
* `ScaleContinuousMap` gains `get_breaks()`/`get_labels()` that put the
  limits among the ticks and label them open when data were squished.
  `diagnose_map()` reports squishing as information when the legend says so
  and as a warning when it does not.
* Bivariate palettes: 17 corner presets (nine new) plus five Stevens 3 x 3
  grids (`"stevens-purpleblue"`, `"stevens-violet"`, `"stevens-greenpink"`,
  `"stevens-cyanred"`, `"stevens-brown"`), resampled for any `n`.
  `bivariate_palette()` also accepts a full `k * k` grid and, with cols4all
  installed, `"c4a::<name>"`. `plot_bivariate_palettes()` draws them all.
* New article `vignette("good-maps")`: what a good choropleth has and which
  ggcarto metric checks each convention.

# ggcarto 0.3.0

The theme of this release is *defensible choices*: the package now shows its
working, so a classification, a class count and a palette can each be
inspected, argued for, and read back off a finished map.

## New: inspect -> choose -> classify -> map -> diagnose

* `class_summary(x, style, n)` tabulates a classification: bounds, legend
  label, count, share, cumulative share and observed min/max per class, with
  the totals, missing count, requested and actual class counts and breaks as
  attributes. Uses the same break engine as the scales.
* `plot_breaks()` and `compare_breaks()` draw the breaks on the histogram,
  one style or several as facets, each class shaded and annotated with its
  count. Integer data get one bar per value aligned with the closure;
  `log_x = TRUE` for heavy tails.
* `class_guide(x, style, n = 3:9)` evaluates candidate class counts on
  statistical fit (Jenks and Caspall's goodness of variance fit), class
  balance, break separation and perceptual load, and makes a *suggestion*
  with stated, rule-based reasons. The rules differ by style family: the
  variance styles look for the elbow in fit; `quantile` looks for breaks too
  close to mean anything; `equal` counts the classes skew has emptied;
  `headtails`, `box` and `dpih` report the count they chose and why; `sd`
  reasons in distances from the mean; `fixed` declines to recommend. `print()`,
  `summary()` and `plot()` (fit curve or class balance) methods. The word
  "optimal" does not appear.
* `diagnose_map(p)` reads a finished ggplot and reports the decisions inside
  its ggcarto scale: classification, classes drawn and their counts, palette
  and its type, colours, limits, squished and censored values, midpoint,
  transform, missing values and their colour, legend labels -- with warnings
  phrased as what a reader would get wrong. `diagnose_scale(scale, x)` does
  the same from a scale and a vector. Continuous, rank, categorical and
  ordinal scales are diagnosed on their own terms.
* `check_palette()` measures a palette for map use -- lightness order,
  neutral centre, adjacent-class distinguishability, colour-vision
  deficiencies (via colorspace, in Suggests), contrast against the background,
  similarity to the missing-data colour -- and `plot_palette()` shows the
  classes under each deficiency.
* `northeast_counties`: the 217 counties of the Census Northeast region as an
  sf object with ACS 2015-2019 variables, for the tutorial and examples.

## New: interval semantics and counts

* `closure = c("right", "left")` on `scale_map()` and its wrappers,
  `class_summary()`, `class_guide()`, `plot_breaks()` and
  `classify_bivariate()` decides where a value exactly on a break goes:
  `(a, b]` (the default, ggplot2's convention) or `[a, b)` (classInt's and
  tmap's).
* `as_count = TRUE` treats a variable as counts: integer breaks, left-closed
  classes and labels that read "0 to 4", "5 to 9".

## New: bivariate transforms

* `classify_bivariate()` gains `style_x`, `style_y`, `trans_x`, `trans_y` and
  `closure`. A variable can be classed on a log (or any scales transform)
  while its breaks are stored -- and the legend labelled -- in the original
  units. Values outside a transform's domain become missing, with a warning.
  `style` (length 1 or 2) still works.
* `legend_bivariate(counts = TRUE)` writes the number of observations in each
  cell, so an empty cell is visible.

## Reproducibility: large vectors

* classInt silently computes `fisher` and `jenks` on an unseeded random
  sample above 3,000 values, and ggcarto suppressed its warning, so a map of
  the ~3,100 US counties could classify differently on every run. Fisher is
  now exact at any size (`warnLargeN = FALSE`); `jenks` above 3,000 values is
  solved with the exact Fisher routine (same objective) with a one-time note;
  `kmeans` and `bclust` are seeded; `hclust` and `bclust` run on a seeded
  sample of 5,000 above that size. `compute_breaks()` gains `large_n =
  c("exact", "sample")` and `seed`, also accepted by the `breaks_*()`
  factories. The scale's own retention cap (200,000 values) is seeded too.
  The session RNG is left untouched.

## Fixed

* A function passed as `limits` (such as `trim_limits()`) was called by
  ggplot2 with the *range* -- two numbers -- so it took quantiles of the
  minimum and maximum. It now sees the data, in data space, on every ggcarto
  scale (`ScaleContinuousMap` is the new base class for continuous and rank
  scales).
* Interval scales classified the full data and then dropped the breaks that
  fell outside `limits`, so `n = 6` with trimmed limits could silently draw
  four classes. Classification now happens on the squished (or censored)
  values, so `n` classes are drawn inside the limits.
* `style = "unique"` put breaks *at* the distinct values, which merged the two
  lowest values into one class. Breaks are now midpoints between values
  (classInt's approach) and the legend keys are labelled with the values.
* A break within floating-point reach of the data extremes (e.g. `geometric`)
  no longer creates a spurious one-value class.

## Documentation

* `docs/ggcarto-tutorial.qmd` is rebuilt around `northeast_counties` and
  `geom_sf()`, in the order inspect -> choose -> classify -> map -> diagnose.
* The vignette opens with the same workflow on one page.

# ggcarto 0.2.0

* Renamed from ggscale to **ggcarto** to avoid confusion with the scales
  package. Every `ggscale_*` name is now `ggcarto_*`
  (`ggcarto_palettes()`, the `ggcarto.palettes` option, the
  `ggcarto_breaks` and `ggcarto_labeller` classes).
* **Bivariate maps.** `classify_bivariate()` classes two variables into
  `n * n` combinations using any class-interval style;
  `scale_fill_bivariate()` / `scale_colour_bivariate()` colour them from an
  `n x n` grid; `legend_bivariate()` draws the grid legend in a square or
  rotated diamond layout, with break values along the edges or a phrase at
  each corner; `inset_legend()` places it on the map.
* `bivariate_palette()` builds the grid from a named preset
  (`bivariate_palettes()`), from four corner colours (bilinear interpolation
  in CIELAB), or by multiplying two sequential ramps
  (`list(x = "brewer::Blues", y = "brewer::Reds")`).
* `na_key()` adds a "No data" legend key to interval and continuous scales,
  which ggplot2's binned and continuous legends cannot draw themselves.
* Fixed: an interval scale whose breaks extended past the data range (fixed
  breaks like `c(0, 5, 10, 31)` on data within 1..30, or `pretty` breaks)
  drew one more class on the map than it showed in the legend, and the top
  legend label was wrong. The top class is now always closed at the scale
  limit, so keys and classes agree.
* Fixed: interval labels chose decimals separately for each edge ("8.0 to
  10"); one decimal count now applies to the whole legend.
* `docs/ggcarto-tutorial.qmd`: a cited tutorial on the contiguous US.
* NAMESPACE is now generated by roxygen2.

# ggcarto 0.1.0

First release.

* `scale_map()` and the `scale_fill_*()` / `scale_colour_*()` family: one
  engine covering continuous, interval, categorical, ordinal and rank
  mappings.
* `ScaleBinnedMap` retains trained data so class-interval algorithms see the
  distribution rather than only the scale limits. Without it, passing a
  data-driven break function to a ggplot2 binned scale silently yields
  equal-width classes.
* 17 class-interval styles via `breaks_style()` and the `breaks_*()`
  factories, usable in any ggplot2 binned scale.
* `as_palette()` resolves palettes from grDevices, RColorBrewer, viridisLite,
  cols4all and paletteer through one `"source::name"` syntax, with `reverse`,
  `range` and `alpha`.
* `label_map()` for legend labels, including `"a to b"` interval labels with
  open first and last classes.
* `trim_limits()` and `symmetric_limits()` for outlier-tolerant and
  midpoint-honest limits; `oob = "squish"` by default.
* `ScaleContinuousRank` gives rank scales quantile breaks and a keyed legend.
  A colour bar cannot show a rank scale honestly: ggplot2 places bar ticks
  linearly in value while the colours sit by rank, so the labels point at the
  wrong colours.
* Vignette `vignettes/ggcarto.qmd`, working through county-level census data
  (`ggplot2::midwest` joined to `maps` county polygons).
