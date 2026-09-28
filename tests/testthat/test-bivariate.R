set.seed(3)
n_obs <- 400
a <- rlnorm(n_obs, 3, 1)
b <- runif(n_obs)

test_that("bivariate_palette returns n*n named, valid colours", {
  for (n in 2:5) {
    pal <- bivariate_palette("blue-red", n)
    expect_length(pal, n * n)
    expect_equal(names(pal), as.vector(t(outer(1:n, 1:n, paste, sep = "-"))))
    expect_true(all(is_colour(unclass(pal))))
    expect_equal(attr(pal, "n"), n)
  }
})

test_that("corner colours land in the corners", {
  corners <- c("#f3f3f3", "#4a90c2", "#d0473b", "#3b1f3d")
  pal <- bivariate_palette(corners, n = 3)
  expect_equal(tolower(unname(pal["1-1"])), corners[1])
  expect_equal(tolower(unname(pal["3-1"])), corners[2])
  expect_equal(tolower(unname(pal["1-3"])), corners[3])
  expect_equal(tolower(unname(pal["3-3"])), corners[4])
})

test_that("every preset builds, and unknown names fail helpfully", {
  for (nm in names(bivariate_palettes())) {
    expect_length(bivariate_palette(nm, 3), 9L)
  }
  expect_error(bivariate_palette("nope"), "Unknown bivariate palette")
  expect_error(bivariate_palette("blue-red", n = 1), "at least 2")
})

test_that("two sequential ramps blend by multiplication", {
  pal <- bivariate_palette(list(x = c("white", "blue"), y = c("white", "red")), 3)
  expect_length(pal, 9L)
  expect_equal(tolower(unname(pal["1-1"])), "#ffffff")
  # blue * red = black
  expect_equal(tolower(unname(pal["3-3"])), "#000000")
})

test_that("reverse flips the right axis", {
  pal <- bivariate_palette("blue-red", 3)
  rx <- bivariate_palette("blue-red", 3, reverse_x = TRUE)
  ry <- bivariate_palette("blue-red", 3, reverse_y = TRUE)
  expect_equal(unname(rx["1-1"]), unname(pal["3-1"]))
  expect_equal(unname(ry["1-1"]), unname(pal["1-3"]))
})

test_that("classify_bivariate makes n*n levels with quantile balance", {
  cls <- classify_bivariate(a, b, n = 3)
  expect_s3_class(cls, "factor")
  expect_length(levels(cls), 9L)
  expect_length(attr(cls, "breaks_x"), 4L)
  expect_length(attr(cls, "breaks_y"), 4L)
  # quantile classes: each marginal class holds about a third
  x_class <- as.integer(sub("-.*", "", as.character(cls)))
  expect_true(all(abs(table(x_class) / n_obs - 1 / 3) < 0.02))
})

test_that("classify_bivariate handles NA, explicit breaks and squishing", {
  a2 <- a; a2[1:5] <- NA
  cls <- classify_bivariate(a2, b, n = 3)
  expect_true(all(is.na(cls[1:5])))
  expect_false(anyNA(cls[-(1:5)]))

  cls2 <- classify_bivariate(a, b, n = 2, breaks_x = c(0, 20, 1e9),
                             breaks_y = c(0, 0.5, 1))
  expect_equal(attr(cls2, "breaks_x"), c(0, 20, 1e9))
  # value beyond the top break is squished into the last class, not dropped
  cls3 <- classify_bivariate(c(-5, 1, 500), c(0.1, 0.2, 0.9), n = 2,
                             breaks_x = c(0, 10, 100), breaks_y = c(0, 0.5, 1))
  expect_equal(as.character(cls3), c("1-1", "1-1", "2-2"))
  expect_error(classify_bivariate(a, b[-1]), "same length")
  expect_error(classify_bivariate(rep(1, n_obs), b, n = 3), "Could not form")
})

test_that("scale_fill_bivariate colours the map from the same grid", {
  df <- data.frame(x = seq_len(n_obs), y = 1, cls = classify_bivariate(a, b, 3))
  p <- ggplot2::ggplot(df, ggplot2::aes(x, y, fill = cls)) +
    ggplot2::geom_tile() +
    scale_fill_bivariate("teal-pink", 3)
  built <- ggplot2::ggplot_build(p)
  pal <- bivariate_palette("teal-pink", 3)
  got <- built$data[[1]]$fill
  expect_equal(unname(got), unname(unclass(pal)[as.character(df$cls)]))
  expect_s3_class(ggplot2::ggplot_gtable(built), "gtable")
})

test_that("legend_bivariate returns a ggplot that renders", {
  cls <- classify_bivariate(a, b, 3)
  leg <- legend_bivariate("blue-red", classes = cls, xlab = "a", ylab = "b",
                          labels = label_map(digits = 1))
  expect_s3_class(leg, "ggplot")
  expect_s3_class(ggplot2::ggplot_gtable(ggplot2::ggplot_build(leg)), "gtable")
  expect_equal(nrow(leg$data), 9L)
  bare <- legend_bivariate("grey-blue", 4)
  expect_equal(nrow(bare$data), 16L)
})

test_that("inset_legend composes with patchwork", {
  skip_if_not_installed("patchwork")
  df <- data.frame(x = seq_len(n_obs), y = 1, cls = classify_bivariate(a, b, 3))
  map <- ggplot2::ggplot(df, ggplot2::aes(x, y, fill = cls)) +
    ggplot2::geom_tile() + scale_fill_bivariate()
  out <- inset_legend(map, legend_bivariate(), position = "topleft", width = 0.2)
  expect_s3_class(out, "patchwork")
  expect_silent(ggplot2::ggplot_build(out))
})

test_that("diamond legends render with ticks, shared origin, and corner labels", {
  cls <- classify_bivariate(a, b, 3)
  d1 <- legend_bivariate("blue-red", classes = cls, style = "diamond",
                         xlab = "a", ylab = "b")
  expect_s3_class(ggplot2::ggplot_gtable(ggplot2::ggplot_build(d1)), "gtable")
  # same first break on both axes -> one shared origin label
  d2 <- legend_bivariate("blue-red", 4, style = "diamond",
                         breaks_x = c(0, 5, 20, 50, 95),
                         breaks_y = c(0, 5, 20, 50, 95))
  expect_silent(ggplot2::ggplot_build(d2))
  d3 <- legend_bivariate("blue-yellow", 3, style = "diamond",
                         corner_labels = c("ll", "xh", "yh", "hh"))
  expect_silent(ggplot2::ggplot_build(d3))
  expect_error(legend_bivariate(corner_labels = c("a", "b")), "four strings")
})

test_that("transforms class in transformed space and report original units", {
  set.seed(9)
  inc <- rlnorm(300, 10, 0.5); edu <- runif(300, 5, 60)
  cls <- classify_bivariate(inc, edu, n = 3, style_x = "equal",
                            style_y = "quantile", trans_x = "log10")
  bx <- attr(cls, "breaks_x")
  expect_length(bx, 4L)
  expect_equal(attr(cls, "trans_x"), "log-10")
  expect_equal(attr(cls, "trans_y"), "identity")
  expect_equal(attr(cls, "style_x"), "equal")
  expect_equal(attr(cls, "style_y"), "quantile")
  # equal intervals on log10 means equal ratios in original units
  expect_equal(diff(log10(bx)), rep(diff(log10(bx))[1], 3), tolerance = 1e-8)
  expect_equal(bx, 10^compute_breaks(log10(inc), "equal", 3), tolerance = 1e-8)
  # the same classes as doing it by hand
  by_hand <- cut(log10(inc), compute_breaks(log10(inc), "equal", 3),
                 labels = FALSE, include.lowest = TRUE)
  expect_equal(as.integer(sub("-.*", "", as.character(cls))), by_hand)
  # legend labels in original units, automatically
  leg <- legend_bivariate("blue-red", classes = cls, xlab = "a", ylab = "b",
                          labels = label_map(digits = 0))
  built <- ggplot2::ggplot_build(leg)
  labs <- unlist(lapply(built$data, function(d) d$label))
  expect_true(any(grepl(format(round(bx[2]), big.mark = ","), labs)))
  # transformation objects work too, and `style` (length 2) still does
  cls2 <- classify_bivariate(inc, edu, n = 3, style = c("equal", "quantile"),
                             trans_x = scales::transform_log10())
  expect_identical(as.character(cls2), as.character(cls))
})

test_that("values outside a transform's domain become missing, with a warning", {
  inc <- c(0, rlnorm(99, 10, 0.5)); edu <- runif(100)
  expect_warning(cls <- classify_bivariate(inc, edu, n = 3, trans_x = "log10"),
                 "outside the domain")
  expect_true(is.na(cls[1]))
  expect_false(anyNA(cls[-1]))
  # user breaks in original units need no transform and lose nothing
  expect_silent(cls2 <- classify_bivariate(inc, edu, n = 2, trans_x = "log10",
                                           breaks_x = c(0, 20000, 1e6),
                                           breaks_y = c(0, 0.5, 1)))
  expect_false(anyNA(cls2))
  expect_equal(attr(cls2, "breaks_x"), c(0, 20000, 1e6))
})

test_that("closure moves values sitting on a break, and empty cells are kept", {
  x <- c(1, 5, 10, 10, 20); y <- c(1, 1, 1, 1, 1)
  r <- classify_bivariate(x, y, n = 2, breaks_x = c(0, 10, 20), breaks_y = c(0, 0.5, 1))
  l <- classify_bivariate(x, y, n = 2, breaks_x = c(0, 10, 20), breaks_y = c(0, 0.5, 1),
                          closure = "left")
  expect_equal(as.character(r), c("1-2", "1-2", "1-2", "1-2", "2-2"))
  expect_equal(as.character(l), c("1-2", "1-2", "2-2", "2-2", "2-2"))
  expect_equal(attr(l, "closure"), "left")
  # every level survives even when empty, and the legend can show the counts
  expect_length(levels(r), 4L)
  expect_equal(as.integer(table(r)), c(0L, 4L, 0L, 1L))
  leg <- legend_bivariate("blue-red", classes = r, counts = TRUE)
  expect_s3_class(leg, "ggplot")
  expect_silent(ggplot2::ggplot_build(leg))
  legd <- legend_bivariate("blue-red", classes = r, counts = TRUE, style = "diamond")
  expect_silent(ggplot2::ggplot_build(legd))
  expect_error(legend_bivariate("blue-red", counts = TRUE), "needs `classes`")
})

test_that("grid presets and k x k vectors resample to any n", {
  p3 <- bivariate_palette("stevens-purpleblue", 3)
  expect_equal(unname(p3["1-1"]), "#e8e8e8")
  expect_equal(unname(p3["3-3"]), "#3b4994")
  p5 <- bivariate_palette("stevens-purpleblue", 5)
  expect_length(p5, 25L)
  expect_equal(tolower(unname(p5["1-1"])), "#e8e8e8")
  expect_equal(tolower(unname(p5["5-5"])), "#3b4994")
  # a plain 9-colour vector is accepted as a grid
  p4 <- bivariate_palette(unclass(p3)[1:9], 4)
  expect_length(p4, 16L)
  expect_error(bivariate_palette(c("red", "blue", "green"), 3), "4 \\(corners\\) or k \\* k")
  for (nm in names(bivariate_palettes())) expect_length(bivariate_palette(nm, 3), 9L)
  expect_s3_class(plot_bivariate_palettes(), "ggplot")
})
