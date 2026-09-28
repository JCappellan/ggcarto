set.seed(11)
x_skew <- c(rlnorm(600, 3, 1), 5000)
x_na <- c(x_skew, NA, NA, NaN)

test_that("class_summary tabulates exactly what the break engine produces", {
  s <- class_summary(x_na, "fisher", 5)
  expect_s3_class(s, "ggcarto_class_summary")
  expect_s3_class(s, "data.frame")
  expect_equal(attr(s, "breaks"), compute_breaks(x_skew, "fisher", 5))
  expect_equal(nrow(s), 5L)
  expect_equal(sum(s$n), length(x_skew))
  expect_equal(sum(s$pct), 100)
  expect_equal(s$cum_pct[5], 100)
  expect_equal(attr(s, "n_missing"), 3L)
  expect_equal(attr(s, "n_valid"), length(x_skew))
  expect_equal(attr(s, "n_total"), length(x_na))
  expect_equal(attr(s, "n_requested"), 5L)
  expect_equal(attr(s, "n_classes"), 5L)
  # every observation's class min/max sits inside the class bounds
  expect_true(all(s$min >= s$lower - 1e-8))
  expect_true(all(s$max <= s$upper + 1e-8))
  # labels are open-ended by default
  expect_match(s$label[1], "^Under")
  expect_match(s$label[5], "above$")
})

test_that("counts match a manual cut with the same closure", {
  b <- compute_breaks(x_skew, "quantile", 4)
  s_r <- class_summary(x_skew, "quantile", 4, closure = "right")
  s_l <- class_summary(x_skew, "quantile", 4, closure = "left")
  expect_equal(s_r$n, as.integer(table(cut(x_skew, b, include.lowest = TRUE, right = TRUE))))
  expect_equal(s_l$n, as.integer(table(cut(x_skew, b, include.lowest = TRUE, right = FALSE))))
})

test_that("a value exactly on a break goes where closure says", {
  x <- c(1, 5, 10, 10, 10, 15, 20)
  r <- class_summary(x, breaks = c(0, 10, 20), closure = "right")
  l <- class_summary(x, breaks = c(0, 10, 20), closure = "left")
  expect_equal(r$n, c(5L, 2L))   # 10 in (0, 10]
  expect_equal(l$n, c(2L, 5L))   # 10 in [10, 20]
  expect_equal(attr(r, "style"), "fixed")
})

test_that("as_count gives integer breaks, left closure and count labels", {
  k <- rpois(300, 3)
  s <- class_summary(k, "quantile", 4, as_count = TRUE)
  expect_equal(attr(s, "closure"), "left")
  expect_true(all(attr(s, "breaks") == round(attr(s, "breaks"))))
  expect_equal(sum(s$n), 300L)
  # a class holding a single integer is labelled with that integer
  single <- s$upper - s$lower == 1 & seq_len(nrow(s)) < nrow(s) & seq_len(nrow(s)) > 1
  if (any(single)) expect_false(any(grepl(" to ", s$label[single])))
  # "a to b" classes read as inclusive integer ranges
  wide <- which(s$upper - s$lower > 1 & seq_len(nrow(s)) < nrow(s) & seq_len(nrow(s)) > 1)
  for (i in wide) {
    expect_equal(s$label[i], sprintf("%d to %d", s$lower[i], s$upper[i] - 1))
  }
})

test_that("degenerate inputs are handled without error", {
  s0 <- class_summary(c(NA, NA), "fisher", 5)
  expect_equal(nrow(s0), 0L)
  expect_equal(attr(s0, "n_missing"), 2L)
  expect_output(print(s0), "No valid observations")

  s1 <- class_summary(rep(7, 20), "quantile", 5)
  expect_equal(nrow(s1), 1L)
  expect_equal(s1$n, 20L)
  expect_equal(s1$label, "7")

  s2 <- class_summary(c(1, 2), "quantile", 5)
  expect_equal(sum(s2$n), 2L)

  s3 <- class_summary(c(1, 1, 1, 2, 2, 3, 100), "quantile", 5)
  expect_equal(sum(s3$n), 7L)
  expect_lte(nrow(s3), 5L)   # duplicated quantile breaks collapse classes

  s4 <- class_summary(c(x_skew, Inf, -Inf), "fisher", 4)
  expect_equal(attr(s4, "n_nonfinite"), 2L)
  expect_equal(sum(s4$n), length(x_skew))
})

test_that("self-sizing styles report their own class count", {
  s <- class_summary(x_skew, "headtails")
  expect_equal(attr(s, "n_classes"), length(attr(s, "breaks")) - 1L)
  expect_output(print(s), "chosen by the method")
})

test_that("labels can be customised and the table is a plain data frame", {
  s <- class_summary(x_skew, "quantile", 3,
                     labels = label_map(style = "interval", suffix = "%", digits = 0))
  expect_true(all(grepl("%", s$label)))
  df <- as.data.frame(s)
  expect_identical(class(df), "data.frame")
  expect_null(attr(df, "breaks"))
  expect_equal(names(df), c("class", "lower", "upper", "label", "n", "pct",
                            "cum_pct", "min", "max"))
})

test_that("printing is compact and informative", {
  s <- class_summary(x_na, "fisher", 5)
  out <- capture.output(print(s))
  expect_match(out[1], "ggcarto class summary")
  expect_true(any(grepl("Fisher-Jenks", out)))
  expect_true(any(grepl("3 missing", out)))
  expect_true(any(grepl("cum %", out)))
})
