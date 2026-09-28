test_that("value labels format numbers", {
  expect_equal(label_map(prefix = "$", digits = 0)(c(1000, 25000)),
               c("$1,000", "$25,000"))
  expect_equal(label_map(scale = 100, suffix = "%", digits = 1)(0.256), "25.6%")
  expect_equal(label_map(big.mark = "")(c(1, 2, 3)), c("1", "2", "3"))
  expect_equal(label_map(digits = 2, big.mark = "")(1.005), "1.00")
})

test_that("interval labels turn n + 1 breaks into n ranges", {
  expect_equal(label_map(style = "interval", digits = 0)(c(0, 10, 20)),
               c("0 to 10", "10 to 20"))
  expect_length(label_map(style = "interval")(c(0, 1, 2, 3, 4)), 4L)
})

test_that("open ends replace the extreme classes", {
  l <- label_map(style = "interval", open = TRUE, digits = 0)(c(0, 10, 20, 30))
  expect_match(l[1], "^Under")
  expect_match(l[3], "above$")
})

test_that("separator and affixes are respected", {
  l <- label_map(style = "interval", separator = "-", prefix = "$",
                 digits = 0)(c(0, 10))
  expect_equal(l, "$0-$10")
})

test_that("digits are chosen sensibly when unset", {
  expect_equal(label_map()(c(1, 2, 3)), c("1", "2", "3"))
  expect_true(grepl("\\.", label_map()(c(0.125, 0.25))[1]))
})

test_that("missing values get the na label", {
  expect_equal(label_map(na_label = "No data")(c(1, NA))[2], "No data")
})

test_that("labeller carries its formatting parts", {
  p <- attr(label_map(style = "interval", separator = " – "), "parts")
  expect_true(is.function(p$fmt))
  expect_equal(p$separator, " – ")
})

test_that("string shorthands resolve", {
  expect_equal(attr(resolve_labels("ranges", "value"), "style"), "interval")
  expect_equal(attr(resolve_labels("values", "value"), "style"), "value")
  expect_equal(attr(resolve_labels(NULL, "value"), "style"), "value")
})

test_that("printing is informative", {
  expect_output(print(label_map(style = "interval")), "interval")
})

test_that("open has three states and or_less wording", {
  f <- label_map(style = "interval", digits = 0)
  expect_equal(f(c(0, 10, 20)), c("0 to 10", "10 to 20"))          # "auto" is closed outside a scale
  expect_equal(label_map(style = "interval", open = TRUE, digits = 0)(c(0, 10, 20)),
               c("Under 10", "10 and above"))
  p <- attr(label_map(or_less = " or under", open = TRUE), "parts")
  expect_equal(p$or_less, " or under"); expect_true(isTRUE(p$open))
  expect_error(label_map(open = "sometimes"), "open")
})

# --- sd labels -------------------------------------------------------------

test_that("label_map(style = 'sd') writes classes in sd units", {
  set.seed(1)
  x <- rnorm(500, 30, 11)
  b <- compute_breaks(x, "sd", 5)
  expect_equal(attr(b, "mean"), mean(x))
  expect_equal(attr(b, "sd"), sd(x))

  labs <- label_map(style = "sd", show_values = FALSE)(b)
  expect_equal(labs, c("Below -1.5 sd", "-1.5 to -0.5 sd", "-0.5 to +0.5 sd",
                       "+0.5 to +1.5 sd", "+1.5 sd and above"))

  with_vals <- label_map(style = "sd", digits = 0)(b)
  expect_match(with_vals[3], "^-0\\.5 to \\+0\\.5 sd \\([0-9]+ to [0-9]+\\)$")
  expect_match(with_vals[1], "\\(Under [0-9]+\\)$")

  # open = FALSE writes the real distance of the extremes
  closed <- label_map(style = "sd", open = FALSE, show_values = FALSE)(b)
  expect_match(closed[1], "^-[0-9.]+ to -1\\.5 sd$")

  # breaks without a reference fall back to interval labels
  plain <- label_map(style = "sd")(as.numeric(b))
  expect_equal(plain, label_map(style = "interval")(as.numeric(b)))

  # step and n change the multiples
  b2 <- compute_breaks(x, "sd", 4, args = list(step = 0.5))
  expect_equal(label_map(style = "sd", show_values = FALSE)(b2),
               c("Below -0.5 sd", "-0.5 to 0 sd", "0 to +0.5 sd", "+0.5 sd and above"))
})

test_that("sd labels reach class_summary and the scale", {
  set.seed(2)
  x <- rnorm(400, 50, 10)
  s <- class_summary(x, "sd", 5, labels = "sd")
  expect_equal(s$label[3], sprintf("-0.5 to +0.5 sd (%s to %s)",
                                   round(mean(x) - 0.5 * sd(x)), round(mean(x) + 0.5 * sd(x))))
  # the sd part is open; the value part shows the extremes outside a scale
  expect_match(s$label[1], "^Below -1.5 sd \\(-?[0-9]+ to [0-9]+\\)$")

  # any classification can be described in sd units
  q <- class_summary(x, "quantile", 4, labels = "sd")
  expect_true(all(grepl(" sd", q$label)))

  d <- data.frame(x = seq_along(x), y = 1, v = x)
  p <- ggplot2::ggplot(d, ggplot2::aes(x, y, fill = v)) + ggplot2::geom_tile() +
    scale_fill_intervals(style = "sd", n = 5, labels = "sd")
  sc <- ggplot2::ggplot_build(p)$plot$scales$get_scales("fill")
  expect_equal(sc$get_labels(), s$label)
  expect_true(inherits(sc$guide, "GuideLegend") || identical(sc$guide, "legend") ||
                inherits(sc$guide, "Guide"))
  expect_equal(diagnose_map(p)$labels, s$label)
})
