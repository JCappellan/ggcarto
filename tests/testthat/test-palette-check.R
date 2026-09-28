test_that("check_palette measures a sequential palette correctly", {
  ck <- check_palette("brewer::YlGnBu", 5)
  expect_s3_class(ck, "ggcarto_palette_check")
  expect_equal(ck$type, "seq")
  expect_equal(ck$type_inferred, "seq")
  expect_length(ck$colours, 5L)
  expect_length(ck$lightness, 5L)
  expect_true(all(diff(ck$lightness) < 0))          # light to dark
  expect_length(ck$delta_e, 4L)
  expect_true(all(ck$delta_e > 10))
  expect_length(ck$contrast, 5L)
  # the pale end vanishes against white: flagged
  expect_true(any(grepl("contrast against white", ck$issues)))
  expect_false(ck$ok)
  expect_output(print(ck), "ggcarto palette check")
})

test_that("diverging palettes are recognised and their centre checked", {
  ck <- check_palette("brewer::RdBu", 7)
  expect_equal(ck$type, "div")
  expect_equal(ck$type_inferred, "div")
  expect_false(any(grepl("centre colour is not neutral", ck$issues)))
  expect_false(any(grepl("contrast against", ck$issues)))   # neutral centre is exempt
  even <- check_palette("brewer::RdBu", 6)
  expect_true(any(grepl("even number of classes", even$issues)))
  spectral <- check_palette("brewer::Spectral", 7)
  expect_true(any(grepl("not neutral", spectral$issues)))
})

test_that("the wrong kind of palette is called out", {
  ck <- check_palette("brewer::Set2", 5, type = "seq")
  expect_true(any(grepl("meant to be sequential", ck$issues)))
  ck2 <- check_palette("brewer::Blues", 5, type = "cat")
  expect_true(any(grepl("meant to be categorical", ck2$issues)))
  ck3 <- check_palette("brewer::Blues", 5, type = "div")
  expect_true(any(grepl("meant to be diverging", ck3$issues)))
  ok <- check_palette("brewer::Set2", 5, type = "cat")
  expect_false(any(grepl("meant to be", ok$issues)))
})

test_that("adjacent classes that merge are detected", {
  ck <- check_palette(c("#000000", "#020202", "#ffffff"), 3, type = "seq")
  expect_true(any(grepl("effectively the same colour", ck$issues)))
  ck9 <- check_palette("brewer::Blues", 9)
  expect_true(any(grepl("hard to tell apart", ck9$issues)))
})

test_that("the missing-data colour is compared with the classes", {
  ck <- check_palette("brewer::Greys", 5, na.value = "grey85")
  expect_length(ck$na_delta_e, 5L)
  expect_true(any(grepl("missing-data colour", ck$issues)))
  ok <- check_palette("brewer::YlGnBu", 5, na.value = "grey85")
  expect_false(any(grepl("missing-data colour", ok$issues)))
})

test_that("colour-vision simulation runs when colorspace is present", {
  skip_if_not_installed("colorspace")
  ck <- check_palette("brewer::RdYlGn", 7)
  expect_s3_class(ck$cvd, "data.frame")
  expect_setequal(ck$cvd$deficiency, c("deutan", "protan", "tritan"))
  expect_true(any(grepl("deutan", ck$issues)))     # red-green is what fails
  p <- plot_palette("brewer::RdYlGn", 7)
  expect_s3_class(p, "ggplot")
  b <- ggplot2::ggplot_build(p)
  expect_equal(nrow(b$data[[1]]), 7L * 5L)         # normal + 3 cvd + grey
  p2 <- plot_palette("brewer::YlGnBu", 5, cvd = "none")
  expect_equal(nrow(ggplot2::ggplot_build(p2)$data[[1]]), 5L)
})

test_that("colour helpers agree with known values", {
  expect_equal(contrast_ratio("white", "black"), 21, tolerance = 1e-6)
  expect_equal(contrast_ratio("white", "white"), 1)
  expect_equal(unname(delta_e("white", "white")), 0)
  expect_gt(unname(delta_e("white", "black")), 99)
  L <- colour_lab(c("black", "grey50", "white"))[, "L"]
  expect_true(all(diff(L) > 0))
  expect_equal(infer_palette_type(c(95, 70, 45, 20)), "seq")
  expect_equal(infer_palette_type(c(40, 70, 95, 70, 40)), "div")
  expect_equal(infer_palette_type(c(60, 65, 58, 62)), "cat")
  expect_equal(palette_type_lookup("brewer::RdBu"), "div")
  expect_equal(palette_type_lookup("Zissou 1"), "div")
  expect_null(palette_type_lookup(c("red", "blue")))
})
