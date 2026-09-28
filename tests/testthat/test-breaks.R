set.seed(42)
x_skew <- c(rlnorm(500, meanlog = 3, sdlog = 1.1), 50000)
x_norm <- rnorm(1000, mean = 10, sd = 2)

test_that("every style returns sorted breaks covering the data", {
  for (st in setdiff(map_styles(), "fixed")) {
    b <- compute_breaks(x_skew, style = st, n = 5)
    expect_true(is.numeric(b), info = st)
    expect_gte(length(b), 2L)
    expect_false(is.unsorted(b), info = st)
    expect_lte(b[1], min(x_skew) + 1e-8)
    expect_gte(b[length(b)], max(x_skew) - 1e-8)
    expect_false(any(duplicated(b)), info = st)
  }
})

test_that("quantile breaks put roughly equal counts in each class", {
  b <- compute_breaks(x_skew, "quantile", n = 5)
  counts <- table(cut(x_skew, b, include.lowest = TRUE))
  expect_equal(length(counts), 5L)
  expect_lt(max(counts) - min(counts), 0.1 * length(x_skew))
})

test_that("equal breaks are equally spaced", {
  b <- compute_breaks(x_skew, "equal", n = 5)
  expect_equal(length(b), 6L)
  expect_true(all(abs(diff(diff(b))) < 1e-6))
})

test_that("quantile and equal disagree on skewed data", {
  expect_false(isTRUE(all.equal(
    compute_breaks(x_skew, "quantile", 5),
    compute_breaks(x_skew, "equal", 5)
  )))
})

test_that("sd breaks sit at multiples of the standard deviation", {
  b <- compute_breaks(x_norm, "sd", n = 6)
  interior <- b[-c(1, length(b))]
  m <- mean(x_norm); s <- stats::sd(x_norm)
  mult <- (interior - m) / s
  expect_true(all(abs(mult - round(mult)) < 1e-6))
})

test_that("log breaks are positive and increasing", {
  b <- compute_breaks(x_skew, "log", n = 5)
  expect_true(all(b > 0))
  expect_false(is.unsorted(b))
})

test_that("degenerate and empty inputs do not error", {
  expect_length(compute_breaks(rep(5, 10), "quantile", 5), 2L)
  expect_length(compute_breaks(numeric(0)), 0L)
  expect_length(compute_breaks(c(NA_real_, NA_real_)), 0L)
  expect_length(compute_breaks(c(1, NA, Inf, 2, 3), "equal", 2), 3L)
})

test_that("fixed style requires breaks", {
  expect_error(compute_breaks(x_skew, "fixed"), "needs `breaks`")
  expect_equal(
    compute_breaks(x_skew, "fixed", args = list(fixedBreaks = c(0, 100, 50000))),
    c(0, 100, 50000)
  )
})

test_that("unknown style is rejected", {
  expect_error(compute_breaks(x_skew, "definitely-not-a-style"))
})

test_that("factories return functions with the requested class count", {
  expect_length(breaks_quantile(6)(x_skew), 7L)
  expect_length(breaks_equal(4)(x_skew), 5L)
  expect_s3_class(breaks_jenks(5), "ggcarto_breaks")
  expect_output(print(breaks_jenks(5)), "jenks")
})

test_that("break functions work in plain ggplot2 binned scales", {
  p <- ggplot2::ggplot(ggplot2::faithfuld,
                       ggplot2::aes(waiting, eruptions, fill = density)) +
    ggplot2::geom_raster() +
    ggplot2::scale_fill_binned(breaks = breaks_quantile(5))
  expect_silent(ggplot2::ggplot_build(p))
})

test_that("map_styles_info documents every style", {
  expect_setequal(map_styles_info()$style, map_styles())
  expect_equal(nrow(map_styles_info("jenks")), 1L)
})

test_that("unique breaks give one class per distinct value", {
  x <- c(0, 0, 1, 1, 1, 2, 4)
  b <- compute_breaks(x, "unique")
  expect_length(b, 5L)                                   # 4 values -> 4 classes
  cls <- cut(x, b, labels = FALSE, include.lowest = TRUE)
  expect_equal(as.integer(table(cls)), c(2L, 3L, 1L, 1L))
  # the breaks sit strictly between the values
  expect_true(all(b[2:4] > c(0, 1, 2) & b[2:4] < c(1, 2, 4)))
  s <- class_summary(x, "unique")
  expect_equal(s$label, c("0", "1", "2", "4"))
})

test_that("a break within rounding of an extreme does not spawn a class", {
  set.seed(4)
  x <- rlnorm(300, 10, 1)
  expect_length(compute_breaks(x, "geometric", 5), 6L)
  expect_length(compute_breaks(x, "log", 5), length(unique(compute_breaks(x, "log", 5))))
  for (st in c("equal", "quantile", "fisher", "kmeans", "sd", "headtails")) {
    b <- compute_breaks(x, st, 5)
    expect_equal(b[1], min(x), info = st)
    expect_equal(b[length(b)], max(x), info = st)
  }
})
