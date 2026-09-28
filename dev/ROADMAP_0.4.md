# ggcarto 0.4 — design notes

_Drafted 2026-09-20 from teaching LiveStudio2 (P8371). Four things the NYC
daytime-population map wanted and the package could not give it. Each section
states the need, what the code did in 0.3.0, a proposed API, implementation
notes against the source, and open questions._

**Status (2026-09-20, later the same day): all four built, in the order
§4 → §2 → §1 → §3, on ggplot2 3.4.4. See NEWS.md for what shipped and
NEXT_STEPS.md for what to verify on 4.0. Departures from this plan:**

* §1: `alpha_ramp()` takes `around = NULL` meaning "the scale's midpoint, else
  0" -- silent, with the rule printed by `diagnose_map()` so it is visible.
  The theme does not switch the panel to grey when a rule is present; the
  diagnosis warns instead, as suggested.
* §2: `mirror`, `center` and `band` shipped; no separate `pretty = TRUE` --
  use `style = "pretty"` for round numbers. `n` is per side, as proposed.
  The mirrored break set is kept whole (empty outer classes included) so
  `class_summary()` shows them; the scale draws only the classes within the
  data and samples the palette from the full set.
* §3: one `theme_carto()` with `variant =`, plus `panel = "grey"`; no
  `update_geom_defaults()` side effects. `guide_map()` shipped alongside.
* §4: `open = "auto"` is the default and applies to interval end classes as
  well as colourbar ticks; `or_less` added for the low end.

Theme for the release: **the map tells the reader what the scale did to the
data** — where colour was faded, where the classes sit around "no change",
and where the ramp was capped.

---

## 1. Value-driven transparency (`alpha`)

### The need

On the NYC map, most neighborhoods sit between −40% and 0%; a handful gain
100–2,000%. We wanted to fade the small movers and let the big ones stand out.
Today the only route is ggplot's own `aes(alpha = abs(pct_change)) +
scale_alpha_continuous()`, which (a) is a second scale to explain, (b) cannot
be told "fade only values near zero, leave the rest alone", (c) has no say in
how the fade ramps, and (d) is invisible to `diagnose_map()`.

### Today

`alpha` is a single number applied to every colour in `as_palette()`
(`R/palettes.R:149`), re-applied after `gradient_n_pal()` strips it
(`R/scale-map.R:241-246`). Constant, palette-wide.

### Proposed API

`alpha` keeps accepting a number, and additionally accepts a *rule*:

```r
# fade toward the midpoint, full opacity beyond +-50, linear in between
scale_fill_cont("brewer::BrBG", midpoint = 0,
                alpha = alpha_ramp(from = 0, to = 50, range = c(0.3, 1)))

# only touch values inside a band; everything else stays opaque
scale_fill_cont(..., alpha = alpha_within(-10, 10, to = 0.35))

# intervals: one alpha per class, recycled or by name
scale_fill_intervals(..., n = 6, alpha = c(1, 1, 0.5, 0.5, 1, 1))

# any function of the data value -> [0, 1]
scale_fill_cont(..., alpha = function(x) pmin(1, abs(x) / 100))
```

Helpers (all return closures with a class so `print()` and `diagnose_map()`
can describe them):

* `alpha_ramp(from, to, range = c(0.3, 1), shape = c("linear", "sqrt",
  "square", "step"), around = NULL)` — opacity rises from `range[1]` at
  `|x - around| <= from` to `range[2]` at `|x - around| >= to`. `around`
  defaults to the scale's `midpoint` if set, else `from` is read as an
  absolute value. `shape` controls the curve; `"step"` is a hard cutoff.
* `alpha_within(lower, upper, to)` — opacity `to` inside the band, 1 outside.
* `alpha_by(breaks, values)` — piecewise constant, for when the user wants
  exact control.

### Implementation notes

* Continuous and rank: apply alpha at **map time**, not in the palette.
  `ScaleContinuousMap` already retains trained values; override `map()` so
  that after `pal(rescaled)` returns colours, `scales::alpha(cols,
  rule(x_original))` is applied. The rescaler already knows the midpoint;
  expose it to the rule.
* Intervals: `ScaleBinnedMap` maps to a class index; alpha per class is
  `rule(class_midpoint)` or the explicit vector. Apply in `map()` as above.
* Legend: `guide_colourbar` draws the bar by calling the scale's `map()` on a
  grid of values, so the fade **shows in the bar for free** — this is the
  point; the reader sees where colour was faded. For binned legends, keys
  inherit the class alpha.
* `diagnose_map()`: add an "Alpha" line — rule type, band, range — and a
  check: "values between −10 and 10 are drawn at 35% opacity; on a light
  background they will read as missing" (compare to `na.value`, reuse the
  dE test in `palette-check.R`).
* Keep the scalar path untouched so `alpha = 0.8` behaves exactly as 0.3.0.

### Open questions

* Should `alpha_ramp()` default `around = midpoint` silently, or require it?
  (Silent is convenient; explicit is defensible. Lean explicit with a message.)
* Interaction with `range =` (palette trimming) — orthogonal, but document.
* Background: a fade only works on a mid-tone panel. Should the theme (§3)
  set `panel.background` to a light grey when an alpha rule is present?
  Probably not automatically; warn from `diagnose_map()` instead.

---

## 2. Classifying around a midpoint (diverging intervals)

### The need

Every classification method finds breaks in the data; none of them knows
that a diverging variable has one break that is non-negotiable. On NYC,
Fisher n = 7 gave `-22.4, 7.15, ...`: gainers and losers shared a class. Today
we hand-write `breaks = c(-65, -40, -20, 0, 25, 100, 2240)`. The package should
be able to *propose* that.

### Today

`midpoint` on an intervals scale calls `anchor_breaks()`
(`R/scale-map.R:309`), which nudges the break nearest the midpoint onto it.
That guarantees a break at 0 but does nothing for balance: the two sides can
have very different class counts and widths, and the palette is then centred
on the class count, not on zero.

### Proposed API

```r
# n classes PER SIDE, computed on |x - midpoint| and mirrored
scale_fill_intervals("brewer::BrBG", midpoint = 0,
                     style = "quantile", n = 3, mirror = TRUE)

# same breaks each side of zero (symmetric in value), open ends
compute_breaks(x, style = "fisher", n = 3, midpoint = 0, mirror = "value")

# a neutral band straddling the midpoint instead of a break on it
scale_fill_intervals(..., midpoint = 0, mirror = TRUE, center = "band",
                     band = c(-5, 5))
```

New arguments on `compute_breaks()`, `scale_map()` (intervals only),
`class_summary()`, `class_guide()`, `plot_breaks()`, `compare_breaks()`:

* `mirror = c("none", "value", "count")`
  * `"value"`: compute breaks on `abs(x - midpoint)` with `style` and `n`,
    then reflect: `midpoint ± b`. Equal colour steps mean equal data
    distance either side — the honest default for a diverging variable.
  * `"count"`: classify each side separately with `style`, `n` classes per
    side. Balanced counts, unequal widths; the quantile-style answer.
* `center = c("break", "band")`: a break exactly on the midpoint (today's
  behaviour) or a neutral middle class `[midpoint - band, midpoint + band]`
  drawn in the palette's neutral colour. Band width from `band =` or, if
  missing, from the innermost mirrored break.
* `n` becomes classes **per side** when `mirror != "none"`; say so loudly in
  the docs and in `class_summary()` output ("3 + 3 classes around 0").
* Open ends: with `mirror`, the outermost breaks come from the data range,
  which is what makes "100% and above" labels natural; pass `open = TRUE`
  through to `label_map()` by default when `mirror` is set.

### Implementation notes

* `resolve_breaks()` is the seam: when `mirror != "none"` return a function
  that computes on the folded data and reflects. `anchor_breaks()` stays for
  `mirror = "none"`.
* Palette assignment: with `mirror`, colours must be taken symmetrically from
  the diverging ramp (`k` colours each side of the neutral), regardless of
  whether the data actually reach the outer classes on both sides. Today the
  ramp is sampled evenly over the class count; add a branch in
  `map_scale_binned()` that samples `2k (+1)` colours centred on the neutral.
* `class_guide()`: a `mirror` mode evaluates `n` per side; readability
  ceiling applies to the total (2n or 2n+1), so 3 per side is "good", 4 per
  side is "fair".
* `diagnose_map()`: report "mirrored around 0, 3 + 3 classes, neutral band
  −5 to 5" and warn when one side has empty classes ("no neighborhood loses
  more than 65%; the two darkest browns are never drawn").
* Rounding: mirrored breaks from Fisher are ugly numbers. Add
  `pretty = TRUE` to snap mirrored breaks to `pretty()`-style values (needed
  for the "round numbers" rule).

### Open questions

* Is `n` per side the right convention, or `n` total with a forced odd/even?
  Per side is easier to reason about and matches how people talk ("three
  classes each way").
* `center = "band"` and `closure` interact: the band is closed on both sides.
  Document that this is the one class that ignores `closure`.

---

## 3. A modern theme: `theme_carto()`

### The need

`theme_void()` plus a dozen `theme()` lines is what every studio script ends
with. The look we want is closer to a good newspaper graphic than a default
ggplot: quiet, high contrast, typographic hierarchy, the legend where the eye
starts. Academic enough to go in a paper.

### Today

`theme_palettes()` (`R/theme.R`) only pushes palettes into a theme. No layout
theme ships.

### Proposed API

```r
theme_carto(base_size = 12, base_family = "", title_family = NULL,
            legend = c("bottom-left", "bottom", "right", "top-right", "inset"),
            variant = c("light", "dark", "print"))
```

Design spec (light variant):

* **Canvas**: pure white plot and panel background, no grid, no axes, no
  border. `plot.margin` 18/16/12/16 pt.
* **Typography**: title 18 pt, bold, ink `#1c1c1c`, left-aligned to the plot
  edge (`plot.title.position = "plot"`), tight leading. Subtitle 12.5 pt,
  regular, `#4a4a4a`, up to two lines, 8 pt below the title. Caption 9 pt,
  `#7a7a7a`, left-aligned, top margin 10 pt; source on the first line, notes
  on the second. `title_family` lets a serif carry the title (Georgia,
  Source Serif, or the user's) while the body stays sans; default inherits
  `base_family` so nothing breaks without system fonts.
* **Legend**: horizontal by default, bottom-left, key height 0.35 cm, bar
  width 6 cm; title 10 pt semi-bold in small caps (`legend.title` with
  `letter-spacing` is not available in grid — use uppercase + 9.5 pt), labels
  9 pt grey `#4a4a4a`; ticks dark grey, frame `grey40` 0.3 pt (this is what
  reads as "capped" in a squished bar). For binned scales: keys touching, a
  thin white keyline, labels *between* keys (`guide_coloursteps(even.steps =
  TRUE, show.limits = TRUE)`).
* **Geometry defaults** (via `update_geom_defaults()` inside the theme
  function, reversible): `geom_sf` linewidth 0.15, colour white for
  polygons; outline layers the user adds stay theirs.
* **Dark variant**: `#111417` canvas, ink `#f2f2f2`, caption `#9aa0a6`; the
  same palettes read well on it only if sequential ramps are reversed —
  document, don't automate.
* **Print variant**: base 10 pt, legend right, margins 8 pt, no background
  fill (for figures placed in a two-column paper).

### Implementation notes

* Pure `ggplot2::theme()` composition on top of `theme_void()`; no new
  dependencies. Fonts: never hard-code a family; if `title_family` is not
  registered, fall back with a one-time message (`systemfonts` in Suggests
  for the check).
* Ship `guide_map()` at the same time, since the legend defaults above are
  half theme, half guide: `guide_map(position, direction, width, height,
  show_limits, na_key)` returns the right `guide_colourbar` /
  `guide_coloursteps` for the scale type, with the NA key on continuous bars
  from the 0.3 "known gaps" list.
* The studio scripts' `theme()` block becomes `theme_carto(legend =
  "bottom")`. Rewrite LiveStudio1/2 to use it as the acceptance test.

### Open questions

* One theme with variants, or `theme_carto()` / `theme_carto_dark()` /
  `theme_carto_print()`? Variants are fewer names; separate functions
  autocomplete better. Lean variants.
* Should `theme_carto()` also set `ggcarto_palettes()` (e.g. seq =
  `hcl::Batlow`, div = `brewer::BrBG`)? No — theme is layout, palettes are a
  separate decision the diagnostics reason about.

---

## 4. Open-ended labels on continuous scales

### The need

With `limits = c(-100, 100)` and `oob = "squish"`, the top of the colourbar
is not "100%", it is "100% or more". Today the honest legend needs a
hand-written labeller (or a subtitle). `label_map(open = TRUE)` exists but
only for interval labels (`R/labels.R:82-85`, `129-132`); the value style
ignores it.

### Proposed API

```r
# explicit
scale_fill_cont(..., limits = c(-100, 100), oob = "squish",
                labels = label_map(suffix = "%", open = TRUE))
#> ticks: -100%  -50%  0%  50%  100% or more

# automatic: the scale knows values were squished, so it says so
scale_fill_cont(..., limits = c(-100, 100))      # open = "auto" (default)
```

* `label_map(open = c("auto", TRUE, FALSE), less_than = "Under ", or_more =
  " or more")` — for value style, `open` applies to the **limit** ticks only,
  and only on the side where data actually exceed the limit (`"auto"`), or on
  both limit ticks regardless (`TRUE`).
* `breaks = "limits"` (or `show_limits = TRUE`): guarantee that the limit
  values are among the ticks, otherwise there is nothing to label "or more".
  Merge with ggplot's computed breaks; drop a computed break that sits
  within 5% of a limit so "100" and "95" do not collide.
* Wording: `or_more` and `less_than` already exist on `label_map()`; reuse.
  For value style the affix wraps the formatted value: "100% or more",
  "Under −100%".

### Implementation notes

* The labeller alone cannot do `"auto"`: it receives tick values, not the
  data. Do it in `ScaleContinuousMap$get_labels()`: the scale has `limits`,
  `oob`, and the retained training range, so it can decide which end was
  squished and post-process the labeller's output for the limit ticks. The
  `ggcarto_labeller` class exposes `attr(fn, "parts")` already; add
  `open`, `or_more`, `less_than` to `parts` so the scale can read the user's
  wording.
* `diagnose_map()`: today it warns "10 values are squished to the ends of the
  ramp". When the legend now says so, downgrade to information: "10 values
  above 100 are drawn in the end colour; the legend reads '100% or more'".
* Also apply to `scale_*_rank()` (limits are ranks; "top 5% or more" makes
  less sense — probably skip) and to the binned scale's outer classes (already
  handled by interval `open`).

### Open questions

* Default `open = "auto"` changes existing legends when limits squish data.
  That is the behaviour we want, but it is a visible change — flag it in
  NEWS under "Breaking".
* Percent affix placement: "100% or more" vs "≥ 100%". Words; the symbol
  fails in small legends and screen readers.

---

## Order of work

1. **§4 open labels** — smallest, most immediately useful, touches one method
   and the labeller. Half a day.
2. **§2 mirrored breaks** — the teaching payoff; `compute_breaks()` +
   `resolve_breaks()` + palette sampling + diagnostics. Two days with tests.
3. **§1 alpha rules** — new ggproto behaviour in `map()`; test on both
   `geom_sf` and `geom_spatraster`. Two days.
4. **§3 theme + guide_map** — design-heavy; iterate on the LiveStudio maps.
   Two days, plus the docs screenshots.

Each lands with: tests in `tests/testthat/`, a NEWS entry, a paragraph in the
vignette, and a before/after on `northeast_counties`.

## Acceptance test

`LiveStudio2.R` Part 3 should reduce to:

```r
ggplot() +
  geom_sf(data = nta_map, aes(fill = pct_change)) +
  scale_fill_intervals("brewer::BrBG", midpoint = 0, mirror = "value",
                       style = "pretty", n = 3,
                       alpha = alpha_ramp(from = 0, to = 40, range = c(0.4, 1)),
                       labels = label_map(suffix = "%")) +
  geom_sf(data = nyc, fill = NA, colour = "black", linewidth = 0.3) +
  labs(title = ..., subtitle = ..., caption = ...) +
  theme_carto(legend = "bottom-left")
```

with a legend that reads "Under −40%  ·  −40 to −20  ·  −20 to 0  ·  0 to 20
·  20 to 40  ·  40% or more", and `diagnose_map()` reporting the mirror, the
band, the alpha rule and the open ends without a single warning.
