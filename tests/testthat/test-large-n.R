# Reproducibility on vectors larger than classInt's sampling threshold
# (largeN = 3000). A US county map has 3,1xx counties, so this is the
# realistic case, not an edge case.

set.seed(2024)
big <- rlnorm(3500, 3, 1)
huge <- rlnorm(12000, 3, 1)

test_that("fisher is exact and identical across runs above 3,000 values", {
  b1 <- compute_breaks(big, "fisher", 5)
  b2 <- compute_breaks(big, "fisher", 5)
  expect_identical(b1, b2)
  exact <- classInt::classIntervals(big, 5, "fisher", warnLargeN = FALSE)$brks
  expect_equal(b1, sort(unique(exact)))
  # and the order of the data does not matter
  expect_equal(compute_breaks(rev(big), "fisher", 5), b1)
})

test_that("fisher classification does not disturb the session RNG", {
  set.seed(7); before <- runif(3)
  set.seed(7); invisible(compute_breaks(big, "fisher", 5)); after <- runif(3)
  expect_identical(before, after)
  set.seed(7); invisible(compute_breaks(big, "kmeans", 5)); after2 <- runif(3)
  expect_identical(before, after2)
})

test_that("large_n = 'sample' is reproducible, seed-dependent and approximate", {
  s1 <- compute_breaks(big, "fisher", 5, large_n = "sample")
  s2 <- compute_breaks(big, "fisher", 5, large_n = "sample")
  expect_identical(s1, s2)
  s3 <- compute_breaks(big, "fisher", 5, large_n = "sample", seed = 99)
  expect_false(identical(s1, s3))
  expect_false(identical(s1, compute_breaks(big, "fisher", 5)))
  # the extremes are always kept
  expect_equal(s1[1], min(big)); expect_equal(s1[length(s1)], max(big))
  # the factory forwards it
  expect_identical(breaks_fisher(5, large_n = "sample")(big), s1)
})

test_that("jenks above 3,000 values is solved exactly, with one note", {
  assign("jenks_large_n", FALSE, envir = ggcarto_env)
  expect_message(j <- compute_breaks(big, "jenks", 5), "Fisher-Jenks")
  expect_silent(j2 <- compute_breaks(big, "jenks", 5))
  expect_identical(j, j2)
  expect_equal(j, compute_breaks(big, "fisher", 5))
  # below the threshold classInt's own jenks is used, deterministically
  small <- big[1:800]
  expect_identical(compute_breaks(small, "jenks", 4), compute_breaks(small, "jenks", 4))
})

test_that("kmeans and bclust are seeded, hclust/bclust are capped", {
  expect_identical(compute_breaks(huge, "kmeans", 5), compute_breaks(huge, "kmeans", 5))
  assign("hclust_large_n", FALSE, envir = ggcarto_env)
  expect_message(h1 <- compute_breaks(huge, "hclust", 5), "seeded sample")
  h2 <- compute_breaks(huge, "hclust", 5)
  expect_identical(h1, h2)
  expect_equal(h1[1], min(huge)); expect_equal(h1[6], max(huge))
  expect_identical(compute_breaks(big, "bclust", 4), compute_breaks(big, "bclust", 4))
})

test_that("the scale classifies a county-sized layer identically every build", {
  df <- data.frame(x = seq_along(big), y = 1, z = big)
  sc <- function() {
    ggplot2::ggplot_build(
      ggplot2::ggplot(df, ggplot2::aes(x, y, fill = z)) + ggplot2::geom_tile() +
        scale_fill_intervals("viridis::mako", "fisher", 5)
    )$plot$scales$scales[[1]]$frozen_breaks
  }
  expect_identical(sc(), sc())
  expect_equal(sc(), compute_breaks(big, "fisher", 5))
})
