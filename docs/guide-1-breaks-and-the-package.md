> **Superseded.** The full version of this guide, with worked maps of the
> contiguous US, citations and a tutorial of every function, is
> `ggcarto-tutorial.qmd` in this folder (render with `quarto render`).
> This file is kept as the short summary.

# Guide 1 — Class intervals, colour, and how ggcarto is put together

This is the "why" behind the package. Read it once and the function
arguments will make sense; skim it again when you have to defend a map.

## Part A. Why maps class their data

### The problem a map has that a chart does not

On a scatterplot, position carries the number. Your eye can read "that dot is
about twice as far up as this one" with decent accuracy. Colour on a
choropleth is the only channel carrying the number, and colour is a *weak*
channel: people can tell light from dark, but not "this shade is 1.7 times
that shade". Studies on lightness perception put the number of reliably
distinguishable steps in a single-hue ramp at about five to seven.

That is the whole argument. A continuous colour ramp promises precision the
reader cannot extract. Classing the data into a handful of intervals is
honest about what colour can actually communicate, and it gives the reader a
legend they can hold in their head: "dark means over $50,000".

Charts get away with continuous ramps because colour is usually a *secondary*
channel there (position already encodes the value) and because the audience
is often reading trend, not values. On a map, colour is the primary channel
and the reader really does want to know what each area's value is.

### Why the choice of breaks matters so much

Classing throws away information. Where you put the break points decides
*which* information survives. The same county data, five classes, four
methods, four different-looking maps — all "correct". So the method is an
argument you are making, and you should be able to say why.

A good mental test: what question is the map answering?

* "Where are the high values?" — you want classes that separate the top of
  the distribution from the rest.
* "How is this spread across the country?" — you want classes of equal
  membership so the map has visual balance.
* "Which areas are above the average?" — you want a break at the average.
* "How does this compare to last year's map?" — you want the *same* breaks
  both times, whatever they are.

### The styles, and when each is the honest choice

**Equal interval** (`equal`, `pretty`). Break the range into equal-width
slices. Honest about magnitude: a step in colour is a step in value. Fails
badly on skewed data (almost everything in social science): one class holds
90% of the areas and the map is a single colour with three dark specks.
`pretty` is the same idea with breaks rounded to numbers humans like.

**Quantile** (`quantile`). Equal *counts* per class. Every class is populated,
the map always looks balanced, and the legend can be read as "top fifth",
"bottom fifth". Dishonest about magnitude: the top class may span $40,000 to
$250,000 while the second spans $38,000 to $40,000, and the map hides that.
It is the default in many GIS tools because it always produces something
readable, which is not the same as being right.

**Natural breaks / Jenks / Fisher** (`jenks`, `fisher`, `kmeans`). Find breaks
that minimise variance within classes and maximise it between them; in other
words, put the breaks in the gaps the data already has. Usually the best
general choice for a single map because it follows the data's own structure.
The cost: the breaks are specific to this dataset, so two maps classed with
Jenks are not comparable, and the legend numbers look arbitrary ($13,417 to
$28,903). `fisher` is the exact algorithm; `jenks` is the classic
approximation. They give nearly identical results; `fisher` is faster.

**Standard deviation** (`sd`). Breaks at the mean and at ±1, ±2 standard
deviations. This is the one to use when the question is "above or below
average, and by how much". Only makes sense with a diverging palette and
roughly symmetric data.

**Head/tails** (`headtails`). For heavy-tailed distributions (city sizes,
income, anything with a long right tail). It splits at the mean, keeps
splitting the upper part at its mean, and stops when the head is no longer a
minority. Produces few low classes and several high ones, which is what a
power-law distribution actually looks like.

**Box** (`box`). Quartiles plus the 1.5·IQR outlier fences. A statistician's
classing: median in the middle, outliers in their own classes.

**Fixed** (`fixed`, or just `breaks = c(...)`). Breaks you choose: policy
thresholds, round numbers, last year's breaks. When comparability or meaning
matters more than fit, this is the right answer, and it is also what a reader
finds easiest to understand.

**Log / geometric** (`log`, `geometric`). Multiplicative steps (1, 10, 100, or
a constant ratio). For data that spans orders of magnitude.

A rule of thumb that survives most situations: start with `fisher` or
`jenks`, look at the legend numbers, then round them to something readable
and re-run with `fixed`. You get natural breaks' fit and a legend a reader can
repeat out loud.

### How many classes

Five is the default for a reason. Three is the minimum for a pattern to be
visible; seven is about the most a reader can match back to a legend. With a
diverging palette use an odd number so there is a neutral middle class.

### Things cartographers do that data-vis people often skip

* **Squish, don't drop.** An outlier beyond the limits gets the end colour,
  not `NA` grey. Removing an area because its value is extreme is the wrong
  message. ggcarto's `oob = "squish"` is the default for this reason.
* **Show "no data" as its own key**, in a neutral grey that cannot be
  confused with a low value.
* **Label classes as ranges** ("$10,000 to $25,000") and open the end classes
  ("Under $10,000", "$95,000 and above"). Bare break values make the reader
  do the pairing.
* **Round.** Nobody needs 13,417.28 in a legend.
* **Anchor diverging palettes on a meaningful value** (zero, the national
  rate), and make the limits symmetric around it so the two colours have the
  same intensity for the same distance.

## Part B. Colour

### Three kinds of palette

* **Sequential**: one hue, light → dark. Low → high. The workhorse.
* **Diverging**: two hues meeting at a neutral. Above/below a reference.
  Wrong for data with no meaningful middle.
* **Categorical (qualitative)**: distinct hues, no order. For land use,
  party, region. Never for a numeric variable.

Ordered categories ("low / medium / high") take a sequential palette, not a
categorical one.

### Where the palettes come from

Nobody needs to invent palettes. ColorBrewer (Cynthia Brewer's, designed
specifically for maps and tested for colour-blind safety and print), viridis
(perceptually uniform, colour-blind safe), the HCL palettes in base R
(grDevices, over 100 of them, no dependency), cols4all (the palettes behind
tmap, with quality scores), and paletteer (an index of ~2,500 palettes from
everywhere). ggcarto does not carry its own; it reaches all of these through
one `"source::name"` syntax.

Two tricks worth knowing. A sequential ramp's lightest colour is often
nearly white and invisible against a white background: `range = c(0.15, 1)`
trims it. And a 9-colour palette sampled at 5 colours looks different from
the designed 5-colour version: Brewer palettes are picked per `n` for this
reason, and ggcarto asks for exactly `n` when it can.

### Bivariate colour

A bivariate map shows two variables by crossing two sequential ramps into an
`n × n` grid. The convention is 3 × 3: nine colours is about the limit of
what a reader can match to a legend. Two ways to build the grid:

* **Four corners.** Choose colours for low-low (near white), x-high, y-high,
  and high-high (dark), and interpolate between them. This is what the
  named presets in `bivariate_palettes()` do.
* **Multiply two ramps.** Overlay a light-to-blue ramp on a light-to-red
  ramp the way a printer overlays inks. `list(x = ..., y = ...)` does this.

The legend has to be a grid, and it has to be *near* the map: readers
cannot hold nine colours in memory while their eyes travel. Two layouts are
in common use. The **square** with an arrow on each side reads like a chart
axis. The **diamond** — the square rotated 45°, both-low at the bottom,
both-high at the top — is what the NYT and Washington Post use; it makes the
"both high" corner the visual peak, and the two variables' edges are
symmetric so neither looks primary. Label the diamond either with the cut
values along the two lower edges (drop the data min and max; they are noise)
or with a short phrase at each corner ("High diversity, big increase") and no
numbers at all. The corner-phrase version is the most readable for a general
audience because it says what the colour *means* rather than what the
numbers are.
Quantile classing is standard for bivariate maps because it guarantees every
row and column of the legend is populated; an empty row in a 3 × 3 legend is
confusing.

## Part C. How ggcarto is built

### The layers

```
compute_breaks()                 numbers in, break points out. Pure function.
  └─ breaks_*()                  factories: breaks_jenks(7) returns a
                                 function(x), usable in ANY binned scale.

as_palette()                     any palette spec → function(n) → n colours.
  └─ map_palettes()              searches; show_palette() draws.

label_map()                      formats legend text: $, %, ranges, "Under".

scale_map()                      THE ENGINE. type = continuous | intervals |
  ├─ scale_fill_intervals()        categorical | ordinal | rank. The wrappers
  ├─ scale_fill_cont() ...         just fix `type`.
  └─ ScaleBinnedMap              ggproto: keeps the data so breaks can see it.

classify_bivariate()             two vectors → one "i-j" factor
scale_fill_bivariate()           factor → n×n colour grid
legend_bivariate()               the grid as a small ggplot
inset_legend()                   puts it on the map (patchwork)
```

### The one thing that makes the package necessary

ggplot2's binned scales call their `breaks` function with the *limits* — two
numbers — not the data. Every classing algorithm above needs the distribution.
So `scale_fill_binned(breaks = some_jenks_function)` silently gives you
equal-interval classes. `ScaleBinnedMap` is a small ggproto subclass that
holds on to the trained values (subsampled to 200,000) and hands them to the
break function. That is 100 lines of code, and without it nothing else in the
package would be true.

The same trick gives "10 to 20" interval labels: a labelling function is only
ever handed the break values, never each class's lower edge, so the scale
builds those labels itself.

### Why one engine

tmap's good idea, borrowed: the choice between a smooth ramp, classed
intervals, categories and ranks is a *mapping type*, not a different function
with a different argument list. Everything else — palette, limits, squishing,
transparency, NA colour, labels — should work identically across them.
`scale_map(type = )` is that; the `scale_fill_*()` wrappers exist so the
names autocomplete next to ggplot2's own.

### Naming rules the package follows

* `scale_fill_*` / `scale_colour_*` — because that is what ggplot2 users
  type. `scale_color_*` aliases exist.
* `breaks_<style>()` — mirrors ggplot2/scales' `breaks_pretty()`,
  `breaks_log()`.
* `label_map()` — mirrors scales' `label_dollar()`, `label_percent()`.
* `*_bivariate` as the suffix for the bivariate trio, so they sort together.
* Style names (`jenks`, `quantile`, `sd` ...) are the names in the
  literature and in classInt, not invented ones.

### Where things live

| File | What is in it |
|---|---|
| `R/breaks.R` | every classing algorithm; `compute_breaks()` is the only real function, the rest are factories |
| `R/palettes.R` | `as_palette()` and the source lookup |
| `R/labels.R` | `label_map()` |
| `R/scale-map.R` | `scale_map()` and the four private builders it dispatches to |
| `R/scale-binned-map.R` | the two ggproto classes |
| `R/scale-wrappers.R` | the `scale_fill_*()` one-liners |
| `R/bivariate.R` | the bivariate trio plus palettes |
| `R/theme.R` | project-wide default palettes |
| `R/utils.R` | `trim_limits()`, `symmetric_limits()`, the ggplot2 3.x/4.x shim |
