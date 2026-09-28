set.seed(12)
x <- c(rlnorm(400, 3, 1), 9000)

layer_classes <- function(p) {
  b <- ggplot2::ggplot_build(p)
  # first layer is the class rectangles
  b$data[[1]]
}

test_that("plot_breaks draws one band per class and the interior breaks", {
  p <- plot_breaks(x, "fisher", 5)
  expect_s3_class(p, "ggplot")
  b <- ggplot2::ggplot_build(p)
  expect_equal(nrow(b$data[[1]]), 5L)            # bands
  expect_equal(nrow(b$data[[3]]), 4L)            # vlines: interior breaks
  expect_equal(sort(b$data[[3]]$xintercept),
               compute_breaks(x, "fisher", 5)[2:5])
  expect_s3_class(ggplot2::ggplot_gtable(b), "gtable")
})

test_that("explicit breaks and log axes work", {
  p <- plot_breaks(x, breaks = c(0, 10, 100, 1000, 10000))
  expect_equal(nrow(layer_classes(p)), 4L)
  pl <- plot_breaks(x, "quantile", 5, log_x = TRUE)
  expect_s3_class(pl, "ggplot")
  expect_silent(ggplot2::ggplot_build(pl))
  expect_message(ggplot2::ggplot_build(plot_breaks(c(0, x), "quantile", 4, log_x = TRUE)),
                 "not shown")
})

test_that("compare_breaks facets by style over the same histogram", {
  p <- compare_breaks(x, c("equal", "quantile", "fisher"), n = 5)
  b <- ggplot2::ggplot_build(p)
  expect_equal(length(unique(b$data[[1]]$PANEL)), 3L)
  # the histogram layer holds identical bars in every panel
  h <- b$data[[2]]
  counts <- split(h$count, h$PANEL)
  expect_equal(counts[[1]], counts[[2]])
  expect_equal(counts[[2]], counts[[3]])
  expect_s3_class(ggplot2::ggplot_gtable(b), "gtable")
})

test_that("count data get one bar per integer", {
  k <- rpois(200, 2)
  p <- plot_breaks(k, "quantile", 3, as_count = TRUE)
  h <- ggplot2::ggplot_build(p)$data[[2]]
  expect_equal(unique(round(h$xmax - h$xmin, 6)), 1)
  expect_equal(sum(h$count), 200)
})

test_that("empty input fails helpfully", {
  expect_error(plot_breaks(c(NA, NA)), "no finite values")
})
