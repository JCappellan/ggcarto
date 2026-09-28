skip_if_not_installed("sf")
p <- ggplot2::ggplot(northeast_counties) +
  ggplot2::geom_sf(ggplot2::aes(fill = unemployment)) +
  scale_fill_intervals("brewer::YlOrRd", "fisher", 5, labels = "ranges")

test_that("theme_ggcarto builds in every variant and legend position", {
  for (v in c("light", "dark", "print")) {
    th <- theme_ggcarto(variant = v)
    expect_s3_class(th, "theme")
    expect_silent(ggplot2::ggplot_build(p + th))
  }
  for (l in c("bottom-left", "bottom", "bottom-right", "right", "left", "top",
              "top-left", "top-right", "inset", "none")) {
    expect_s3_class(ggplot2::ggplot_gtable(ggplot2::ggplot_build(p + theme_ggcarto(legend = l))), "gtable")
  }
  th <- theme_ggcarto()
  expect_equal(th$plot.background$fill, ggcarto_colours()[["paper"]])
  expect_equal(th$legend.position, "bottom")
  expect_equal(theme_ggcarto(variant = "print")$legend.position, "right")
  expect_equal(theme_ggcarto(variant = "dark")$plot.background$fill, "#14181d")
  expect_equal(theme_ggcarto(panel = "grey")$panel.background$fill, "#ebe8e0")
  expect_true(is.character(ggcarto_colours()) && all(is_colour(ggcarto_colours())))
})

test_that("guide_map returns the right guide with map sizing", {
  g <- guide_map()
  expect_true(inherits(g, "GuideColourbar") || inherits(g, "guide"))
  gs <- guide_map("steps")
  expect_true(inherits(gs, "GuideColoursteps") || inherits(gs, "guide"))
  gk <- guide_map("keys")
  expect_true(inherits(gk, "GuideLegend") || inherits(gk, "guide"))
  # keys run low to high horizontally, high to low vertically
  expect_false(isTRUE(guide_map("keys")$params$reverse))
  expect_true(isTRUE(guide_map("keys", direction = "vertical")$params$reverse))
  for (g in list(guide_map(), guide_map("steps"), guide_map("keys"),
                 guide_map("bar", direction = "vertical"))) {
    q <- ggplot2::ggplot(northeast_counties) +
      ggplot2::geom_sf(ggplot2::aes(fill = pct_bachelors)) +
      scale_fill_cont("viridis::mako", limits = trim_limits(0.02, 0.98), guide = g) +
      theme_ggcarto()
    expect_s3_class(ggplot2::ggplot_gtable(ggplot2::ggplot_build(q)), "gtable")
  }
})
