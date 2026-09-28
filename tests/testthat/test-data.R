test_that("northeast_counties is complete and well-formed", {
  skip_if_not_installed("sf")
  d <- northeast_counties
  expect_s3_class(d, "sf")
  expect_equal(nrow(d), 217L)
  expect_setequal(unique(d$state), c("CT", "ME", "MA", "NH", "NJ", "NY", "PA", "RI", "VT"))
  expect_true(all(nchar(d$fips) == 5L))
  expect_false(anyDuplicated(d$fips) > 0)
  for (v in c("pop", "median_income", "unemployment", "pct_bachelors", "area", "density")) {
    expect_false(anyNA(d[[v]]), info = v)
  }
  expect_equal(sum(is.na(d$pct_poverty)), 49L)
  expect_true(all(sf::st_is_valid(d)))
  expect_false(is.na(sf::st_crs(d)))
  # the tutorial's headline numbers
  expect_gt(max(d$pop), 2.5e6)
  expect_lt(min(d$pop), 5000)
})

test_that("the workflow runs end to end on the bundled data with geom_sf", {
  skip_if_not_installed("sf")
  x <- northeast_counties$median_income
  g <- class_guide(x, "fisher")
  expect_false(is.na(g$suggested))
  s <- class_summary(x, "fisher", g$suggested)
  expect_equal(sum(s$n), 217L)
  p <- ggplot2::ggplot(northeast_counties) +
    ggplot2::geom_sf(ggplot2::aes(fill = median_income)) +
    scale_fill_intervals("brewer::YlGnBu", "fisher", g$suggested, labels = "ranges")
  d <- diagnose_map(p)
  expect_equal(d$classes$n, s$n)
  expect_equal(d$n_classes, g$suggested)
})
