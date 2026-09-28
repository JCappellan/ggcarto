set.seed(21)
x_skew <- c(rlnorm(3000, 2, 0.5), rep(NA, 10))
x_norm <- rnorm(800, 50, 10)
x_count <- rpois(400, 2)

test_that("class_guide returns a complete, inspectable object", {
  g <- class_guide(x_skew, "fisher")
  expect_s3_class(g, "ggcarto_class_guide")
  expect_equal(g$style, "fisher")
  expect_equal(g$family, "variance")
  expect_true(is.integer(g$suggested))
  expect_true(g$suggested %in% 3:9)
  expect_true(length(g$reasons) >= 3)
  expect_equal(g$table$n, 3:9)
  expect_true(all(c("n", "actual_classes", "fit", "gain", "smallest_class_n",
                    "smallest_class_pct", "largest_class_pct", "empty_classes",
                    "min_spacing", "spacing_pct", "readability", "warnings") %in%
                    names(g$table)))
  expect_equal(g$distribution$n_missing, 10L)
  expect_equal(g$distribution$n, 3000L)
  expect_equal(names(g$breaks), as.character(3:9))
  expect_equal(unname(vapply(g$counts, sum, numeric(1))), rep(3000, 7))
})

test_that("fit is the goodness of variance fit and gain its difference", {
  g <- class_guide(x_skew, "fisher", n = 3:6)
  v <- x_skew[is.finite(x_skew)]
  for (k in 3:6) {
    b <- compute_breaks(v, "fisher", k)
    cls <- cut(v, b, include.lowest = TRUE, labels = FALSE)
    ssw <- sum(tapply(v, cls, function(z) sum((z - mean(z))^2)))
    sst <- sum((v - mean(v))^2)
    expect_equal(g$table$fit[g$table$n == k], 1 - ssw / sst, tolerance = 1e-10)
  }
  expect_equal(g$table$gain[-1], diff(g$table$fit))
  expect_true(is.na(g$table$gain[1]))
  # fit is monotone non-decreasing in n for the exact optimiser
  expect_true(all(diff(g$table$fit) >= -1e-12))
})

test_that("the recommendation is stable and follows the stated rules", {
  g1 <- class_guide(x_skew, "fisher")
  g2 <- class_guide(x_skew, "fisher")
  expect_identical(g1$suggested, g2$suggested)
  expect_identical(g1$table, g2$table)
  # order of the data does not matter
  g3 <- class_guide(rev(x_skew), "fisher")
  expect_identical(g1$suggested, g3$suggested)
  # the suggested n is eligible: no empty or tiny class
  row <- g1$table[g1$table$n == g1$suggested, ]
  expect_equal(row$empty_classes, 0L)
  expect_gte(row$smallest_class_n, 3L)
  # and either sits where the gain flattens or is the best readable
  th <- g1$thresholds
  nxt <- g1$table$gain[g1$table$n == g1$suggested + 1L]
  expect_true(is.na(nxt) || nxt < th$gain_min || row$fit < th$fit_min ||
                g1$suggested <= th$max_readable)
})

test_that("readability ratings follow the class count", {
  g <- class_guide(x_norm, "quantile", n = 3:9)
  expect_equal(g$table$readability,
               c("excellent", "excellent", "excellent", "good", "fair", "poor", "poor"))
})

test_that("quantile guidance reasons about balance, not fit", {
  g <- class_guide(x_norm, "quantile")
  expect_equal(g$family, "quantile")
  expect_true(any(grepl("equal counts by construction", g$reasons)))
  expect_true(g$suggested %in% 4:7)
  # a coarse variable cannot form many quantile classes; the guide says so
  gc <- class_guide(x_count, "quantile")
  expect_true(all(gc$table$actual_classes <= length(unique(x_count))))
  expect_equal(gc$table$actual_classes[gc$table$n == gc$suggested], gc$suggested)
})

test_that("equal intervals on skewed data are warned about", {
  g <- class_guide(x_skew, "equal")
  expect_equal(g$family, "range")
  expect_true(any(grepl("skew", g$reasons)))
  expect_true(any(grepl("thin", g$table$warnings)))
  # symmetric data: equal intervals are fine and get a real suggestion
  g2 <- class_guide(x_norm, "equal")
  expect_false(is.na(g2$suggested))
})

test_that("self-sizing, sd, fixed and unique styles are treated on their own terms", {
  h <- class_guide(x_skew, "headtails")
  expect_equal(h$family, "self")
  expect_equal(nrow(h$table), 1L)
  expect_equal(h$suggested, h$table$actual_classes[1])
  expect_true(any(grepl("split 1", h$reasons)))
  expect_equal(h$suggested, length(compute_breaks(x_skew, "headtails")) - 1L)

  s <- class_guide(x_norm, "sd")
  expect_equal(s$family, "sd")
  expect_true(any(grepl("standard deviations", s$reasons)))
  expect_lte(s$suggested, 6L)

  f <- class_guide(x_skew, "fixed", breaks = c(0, 5, 10, 20, 100))
  expect_true(is.na(f$suggested))
  expect_true(any(grepl("thresholds you supplied", f$reasons)))
  expect_equal(f$table$actual_classes[1], 4L)
  expect_error(class_guide(x_skew, "fixed"), "needs")

  u <- class_guide(x_count, "unique")
  expect_equal(u$suggested, length(unique(x_count)))
  u2 <- class_guide(x_norm, "unique")
  expect_true(is.na(u2$suggested))
})

test_that("degenerate inputs give no suggestion rather than an error", {
  g <- class_guide(rep(4, 10), "fisher")
  expect_true(is.na(g$suggested))
  expect_true(all(g$table$actual_classes == 1L))
  g2 <- class_guide(c(NA, NA), "fisher")
  expect_true(is.na(g2$suggested))
  expect_true(all(is.na(g2$table$fit)))
  g3 <- class_guide(c(1, 2, 3), "fisher")
  expect_true(is.na(g3$suggested))
  expect_output(print(g2), "no number of classes")
})

test_that("methods print, summarise and plot", {
  g <- class_guide(x_skew, "fisher")
  out <- capture.output(print(g))
  expect_match(out[1], "ggcarto class guide")
  expect_true(any(grepl("^Suggested: [0-9]+ classes", out)))
  expect_false(any(grepl("optimal", out, ignore.case = TRUE) & !grepl("not an optimum", out)))
  expect_true(any(grepl("<--", out)))
  s <- summary(g)
  expect_s3_class(s, "data.frame")
  expect_equal(sum(s$suggested), 1L)
  p1 <- plot(g)
  p2 <- plot(g, type = "balance")
  expect_s3_class(p1, "ggplot"); expect_s3_class(p2, "ggplot")
  expect_silent(ggplot2::ggplot_build(p1))
  expect_silent(ggplot2::ggplot_build(p2))
})

test_that("closure and as_count reach the metrics", {
  ga <- class_guide(x_count, "quantile", as_count = TRUE)
  expect_equal(ga$closure, "left")
  expect_true(all(unlist(ga$breaks) == round(unlist(ga$breaks))))
})
