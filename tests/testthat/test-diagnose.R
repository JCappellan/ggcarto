set.seed(5)
z <- c(rlnorm(297, 3, 1), NA, NA, 4000)
df <- data.frame(x = 1:300, y = 1, z = z,
                 g = factor(sample(letters[1:4], 300, TRUE)))
tile <- function(sc, var = "z") {
  ggplot2::ggplot(df, ggplot2::aes(x, y, fill = .data[[var]])) +
    ggplot2::geom_tile() + sc
}

test_that("diagnose_map reports what the interval scale draws", {
  p <- tile(scale_fill_intervals("brewer::YlGnBu", "fisher", 6,
                                 limits = trim_limits(0.02, 0.98),
                                 na.value = "grey85", labels = "ranges"))
  d <- diagnose_map(p)
  expect_s3_class(d, "ggcarto_diagnosis")
  expect_equal(d$type, "intervals")
  expect_equal(d$style, "fisher")
  expect_equal(d$n_requested, 6L)
  expect_equal(d$n_classes, 6L)
  expect_equal(d$n_missing, 2L)
  expect_equal(d$n_valid, 298L)
  expect_equal(unname(d$limits), unname(stats::quantile(z, c(0.02, 0.98), na.rm = TRUE)))
  expect_equal(sum(d$n_squished), 12L)
  expect_equal(d$oob, "squish")
  expect_equal(d$palette_type, "seq")
  expect_equal(d$na_colour, "grey85")
  expect_length(d$colours, 6L)
  expect_length(d$labels, 6L)
  expect_match(d$labels[1], " to ")   # labels = "ranges" is not open-ended
  # the class table agrees with the colours actually drawn
  fills <- ggplot2::ggplot_build(p)$data[[1]]$fill
  drawn <- table(factor(fills, levels = d$colours))
  expect_equal(as.integer(drawn), d$classes$n)
  expect_equal(sum(d$classes$n), 298L)
  expect_true(any(grepl("squished", d$warnings)))
  expect_true(any(grepl("missing", d$warnings)))
  expect_output(print(d), "ggcarto map diagnosis")
})

test_that("diagnose_scale works without a plot and agrees with diagnose_map", {
  sc <- scale_fill_intervals("viridis::mako", "quantile", 5)
  d1 <- diagnose_scale(sc, z)
  d2 <- diagnose_map(tile(scale_fill_intervals("viridis::mako", "quantile", 5)))
  expect_equal(d1$breaks, d2$breaks)
  expect_equal(d1$classes$n, d2$classes$n)
  expect_equal(d1$colours, d2$colours)
  expect_equal(d1$breaks, compute_breaks(z, "quantile", 5))
})

test_that("warnings name the cartographic consequence", {
  # empty and tiny classes with fixed breaks
  d <- diagnose_scale(scale_fill_intervals(breaks = c(0, 10, 20, 3000, 3500, 4000)), z)
  expect_true(any(grepl("no observations", d$warnings)))
  expect_true(any(grepl("fewer than", d$warnings)))
  # too many classes
  d8 <- diagnose_scale(scale_fill_intervals(style = "quantile", n = 9), z)
  expect_true(any(grepl("9 ordered classes", d8$warnings)))
  # diverging palette without a midpoint, and a midpoint outside the data
  dd <- diagnose_scale(scale_fill_intervals("brewer::RdBu", "quantile", 5), z)
  expect_true(any(grepl("without `midpoint`", dd$warnings)))
  dm <- diagnose_scale(scale_fill_intervals("brewer::RdBu", "quantile", 5, midpoint = -50), z)
  expect_true(any(grepl("outside the data", dm$warnings)))
  # heavy squishing
  ds <- diagnose_scale(scale_fill_intervals("viridis::mako", "quantile", 5,
                                            limits = c(10, 50)), z)
  expect_gt(sum(ds$n_squished), 0.05 * 298)
  expect_true(any(grepl("say so in the legend", ds$warnings)))
  # censoring
  dc <- diagnose_scale(scale_fill_intervals("viridis::mako", "quantile", 5,
                                            limits = c(10, 50), oob = "censor"), z)
  expect_equal(dc$n_censored, sum(ds$n_squished))
  expect_true(any(grepl("censored", dc$warnings)))
  # NA colour close to a class
  dn <- diagnose_scale(scale_fill_intervals("brewer::Greys", "quantile", 5,
                                            na.value = "grey85"), z)
  expect_true(any(grepl("missing-data colour is close", dn$warnings)))
})

test_that("continuous, rank and discrete scales are diagnosed on their own terms", {
  dc <- diagnose_map(tile(scale_fill_map("brewer::RdBu", midpoint = 0)))
  expect_equal(dc$type, "continuous")
  expect_true(any(grepl("outside the data", dc$warnings)))
  expect_true(is.numeric(dc$breaks))          # legend ticks
  dr <- diagnose_map(tile(scale_fill_rank("viridis::mako")))
  expect_equal(dr$type, "rank")
  expect_true(any(grepl("rank scale", dr$warnings)))
  dg <- diagnose_map(tile(scale_fill_cat("brewer::Blues"), "g"))
  expect_equal(dg$type, "categorical")
  expect_equal(dg$n_classes, 4L)
  expect_true(any(grepl("sequential palette", dg$warnings)))
  do <- diagnose_map(tile(scale_fill_ord("brewer::Set2"), "g"))
  expect_true(any(grepl("qualitative palette", do$warnings)))
  dok <- diagnose_map(tile(scale_fill_cat("brewer::Set2"), "g"))
  expect_false(any(grepl("palette", dok$warnings)))
})

test_that("diagnose_map finds the right scale and fails helpfully", {
  p <- ggplot2::ggplot(df, ggplot2::aes(x, y, colour = z)) + ggplot2::geom_point() +
    scale_colour_intervals("viridis::mako", "fisher", 4)
  d <- diagnose_map(p, aesthetic = "color")
  expect_equal(d$n_classes, 4L)
  expect_error(diagnose_map(p, aesthetic = "fill"), "No ggcarto scale for")
  expect_error(diagnose_map(ggplot2::ggplot(df, ggplot2::aes(x, y)) + ggplot2::geom_point()),
               "No ggcarto scale")
  expect_error(diagnose_scale(ggplot2::scale_fill_viridis_c(), z), "not a ggcarto scale")
  expect_error(diagnose_map("nope"), "ggplot")
})

test_that("closure and as_count are reported and applied", {
  k <- rpois(300, 3)
  dk <- diagnose_scale(scale_fill_intervals("viridis::mako", "quantile", 4, as_count = TRUE), k)
  expect_true(dk$as_count)
  expect_equal(dk$closure, "left")
  expect_true(all(dk$breaks == round(dk$breaks)))
  expect_equal(dk$classes$n, class_summary(k, "quantile", 4, as_count = TRUE)$n)
})

test_that("a function limits sees the data, not the range", {
  # trim_limits() inside a scale must equal trim_limits() on the vector
  for (sc in list(scale_fill_intervals("viridis::mako", "fisher", 5, limits = trim_limits(0.05, 0.95)),
                  scale_fill_map("viridis::mako", limits = trim_limits(0.05, 0.95)),
                  scale_fill_rank("viridis::mako", limits = trim_limits(0.05, 0.95)))) {
    built <- ggplot2::ggplot_build(tile(sc))$plot$scales$scales[[1]]
    expect_equal(built$get_limits(), unname(stats::quantile(z, c(0.05, 0.95), na.rm = TRUE)))
  }
  # and the interval scale still draws n classes inside those limits
  p <- tile(scale_fill_intervals("viridis::mako", "fisher", 5, limits = trim_limits(0.05, 0.95)))
  fills <- ggplot2::ggplot_build(p)$data[[1]]$fill
  expect_equal(length(setdiff(unique(fills), "grey85")), 5L)
  # with a transform, limits round-trip through data space
  pl <- tile(scale_fill_map("viridis::mako", limits = trim_limits(0.05, 0.95), transform = "log10"))
  built <- ggplot2::ggplot_build(pl)$plot$scales$scales[[1]]
  expect_equal(10^built$get_limits(), unname(stats::quantile(z, c(0.05, 0.95), na.rm = TRUE)))
})

test_that("diagnose_map reports the legend labels the plot actually shows", {
  p <- tile(scale_fill_intervals("brewer::YlGnBu", "fisher", 5,
                                 limits = trim_limits(0.02, 0.98),
                                 labels = label_map(style = "interval", prefix = "$",
                                                    big.mark = ",", digits = 0, open = TRUE)))
  d <- diagnose_map(p)
  shown <- ggplot2::ggplot_build(p)$plot$scales$scales[[1]]$get_labels()
  expect_identical(d$labels, shown)
  expect_true(all(grepl("\\$", d$labels)))
  expect_length(d$labels, 5L)
})
