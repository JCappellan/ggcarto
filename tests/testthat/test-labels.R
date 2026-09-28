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
