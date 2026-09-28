# Picking this up on your machine

Version 0.3.0: diagnostics (`class_summary()`, `plot_breaks()`,
`compare_breaks()`, `class_guide()`, `diagnose_map()`, `check_palette()`),
interval closure and count handling, bivariate transforms, deterministic
large-N classification, the `northeast_counties` dataset and a rebuilt
tutorial. See NEWS.md for the full list and the bugs fixed on the way.

This build was exercised against R 4.3.3, ggplot2 4.0.3, scales 1.4.0,
classInt 0.4-10, sf 1.0-15, patchwork 1.3.2 and colorspace 2.1-0: 15 test
files pass, the vignette and the tutorial render, and `R CMD check` is clean
apart from Suggests packages (cols4all, paletteer) that were not installable
there.

## First five minutes

```r
install.packages(c("devtools", "roxygen2", "testthat", "classInt", "sf",
                   "patchwork", "colorspace", "quarto", "tidyr"))
devtools::document()
devtools::test()
devtools::check()
devtools::install()          # the vignette needs an *installed* package
quarto::quarto_render("vignettes/ggcarto.qmd")
quarto::quarto_render("docs/ggcarto-tutorial.qmd")
```

`devtools::document()` will rewrite `RoxygenNote` with your roxygen2 version;
that is expected.

## Things to eyeball

1. `class_guide()` on a few of your own variables: do the suggestions read as
   sensible, and do the reasons say why? The thresholds live in one place,
   `guide_thresholds()` in `R/class-guide.R`, and are stated in `?class_guide`.
2. `diagnose_map()` on a plot with `geom_sf()` and on one with a layer-level
   `data =`; it recovers the mapped variable by evaluating the `aes()` on the
   layer data, falling back on the scale's retained values.
3. Palette-check thresholds (`palette_thresholds()` in `R/palette-check.R`):
   CIE76 dE 10 / 5, WCAG contrast 1.2. Conservative on purpose.

## Known gaps, roughly in order of value

* A native `guide_bivariate()` as a ggplot2 4.0 `Guide` subclass. Deferred
  deliberately: doing it well means ggplot2 internals that are still moving;
  `inset_legend()` is the supported route.
* NA key on continuous colourbars (needs a custom guide).
* `guide_map()` -- tmap-style legend layout options.
* `scale_size_map()` / `scale_linewidth_map()` on the same engine.
* `class_guide()` could take a palette and fold `check_palette()` into the
  readability rating; today readability is by class count alone.
* `label_map(right = )` is accepted for compatibility but closure is decided
  on the scale (`closure =`); consider removing the argument in 1.0.
