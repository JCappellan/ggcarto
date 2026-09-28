test_that("palettes resolve from every built-in source", {
  expect_length(as_palette("hcl::Blues 3")(5), 5L)
  expect_length(as_palette("Zissou 1")(7), 7L)
  expect_length(as_palette("viridis::mako")(4), 4L)
  skip_if_not_installed("RColorBrewer")
  expect_length(as_palette("brewer::RdBu")(9), 9L)
})

test_that("brewer palettes interpolate beyond maxcolors", {
  skip_if_not_installed("RColorBrewer")
  expect_length(as_palette("brewer::Set2")(20), 20L)
})

test_that("colour vectors and functions are accepted", {
  expect_length(as_palette(c("red", "white", "blue"))(11), 11L)
  # Asking for exactly as many colours as were supplied returns them untouched.
  expect_equal(as_palette(c("red", "white", "blue"))(3),
               c("red", "white", "blue"))
  expect_length(as_palette(function(n) rep("red", n))(3), 3L)
  expect_equal(as_palette("steelblue")(4), rep("steelblue", 4))
})

test_that("reverse flips the palette", {
  a <- as_palette("viridis::viridis")(5)
  b <- as_palette("viridis::viridis", reverse = TRUE)(5)
  expect_identical(a, rev(b))
})

test_that("range trims the palette and keeps the requested length", {
  a <- as_palette("viridis::viridis")(5)
  b <- as_palette("viridis::viridis", range = c(0.2, 0.8))(5)
  expect_length(b, 5L)
  expect_false(identical(a, b))
  expect_error(as_palette("viridis::viridis", range = c(1, 0)), "range")
  expect_error(as_palette("viridis::viridis", range = c(-1, 2)), "range")
})

test_that("unknown palettes fail with a useful message", {
  expect_error(as_palette("no_such_palette_xyz"), "Could not find")
  expect_error(as_palette(1:3), "colour vector")
})

test_that("palette discovery works", {
  expect_gt(nrow(map_palettes()), 100L)
  expect_true(all(map_palettes("div")$type == "div"))
  expect_true(all(grepl("^brewer::", map_palettes(source = "brewer")$name)))
  expect_true(all(grepl("Rd", map_palettes(pattern = "Rd")$name, ignore.case = TRUE)))
  expect_true(all(c("prefix", "source", "package", "installed") %in%
                    names(palette_sources())))
})

test_that("ggcarto_palettes sets and restores defaults", {
  old <- ggcarto_palettes(seq = "brewer::Greens")
  expect_equal(default_palette("seq"), "brewer::Greens")
  options(ggcarto.palettes = old)
})
