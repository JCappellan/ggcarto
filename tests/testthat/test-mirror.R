set.seed(8)
chg <- c(rnorm(300, 0, 20), 90, 130, -60)
short <- c(pmax(rnorm(300, 0, 20), -30), 90, 130, 150)   # the low side stops early
tile <- function(sc, z = chg) {
  ggplot2::ggplot(data.frame(x = seq_along(z), y = 1, z = z), ggplot2::aes(x, y, fill = z)) +
    ggplot2::geom_tile() + sc
}

test_that("mirror = 'value' reflects breaks about the midpoint", {
  b <- compute_breaks(chg, "fisher", 3, midpoint = 0, mirror = "value")
  expect_equal(attr(b, "mirror"), "value")
  expect_equal(attr(b, "per_side"), 3L)
  expect_length(b, 7L)
  expect_true(0 %in% b)
  off <- b[b > 0]
  expect_equal(sort(-off), b[b < 0])                       # symmetric
  expect_equal(max(abs(b)), max(abs(chg)))                  # reaches the far side
  # the same offsets as classing the distances
  d <- compute_breaks(abs(chg), "fisher", 3)
  expect_equal(unname(off), d[-1])
  # a different midpoint: shifting the data shifts the breaks
  b5 <- compute_breaks(chg + 5, "fisher", 3, midpoint = 5, mirror = "value")
  expect_equal(as.numeric(b5) - 5, as.numeric(b))
})

test_that("mirror = 'count' classes each side separately", {
  b <- compute_breaks(chg, "quantile", 3, midpoint = 0, mirror = "count")
  expect_length(b, 7L)
  expect_equal(b[4], 0)
  expect_equal(b[1], min(chg)); expect_equal(b[7], max(chg))
  cls <- assign_classes(chg, b)
  lo <- table(cls[chg < 0]); hi <- table(cls[chg >= 0])
  expect_equal(length(lo), 3L); expect_equal(length(hi), 3L)
  expect_lt(diff(range(lo)), 3); expect_lt(diff(range(hi)), 3)  # balanced
})

test_that("center = 'band' replaces the midpoint break with a neutral class", {
  b <- compute_breaks(chg, "fisher", 3, midpoint = 0, mirror = "value", center = "band")
  expect_false(0 %in% b)
  expect_length(b, 6L)
  expect_equal(unname(attr(b, "band")), c(-b[4], b[4]))
  b2 <- compute_breaks(chg, "fisher", 3, midpoint = 0, mirror = "value", center = "band", band = 5)
  expect_true(all(c(-5, 5) %in% b2))
  expect_equal(unname(attr(b2, "band")), c(-5, 5))
  b3 <- compute_breaks(chg, "quantile", 3, midpoint = 0, mirror = "count", center = "band",
                       band = c(-5, 10))
  expect_true(all(c(-5, 10) %in% b3))
  expect_false(any(b3 > -5 & b3 < 10))
  expect_error(compute_breaks(chg, "fisher", 3, mirror = "value"), "midpoint")
})

test_that("the scale samples the palette over the whole mirrored set", {
  p <- tile(scale_fill_intervals("brewer::BrBG", "fisher", 3, midpoint = 0, mirror = "value"), short)
  b <- ggplot2::ggplot_build(p)
  sc <- b$plot$scales$scales[[1]]
  full <- sc$full_breaks
  expect_length(full, 7L)
  pal <- as_palette("brewer::BrBG")(6)
  fills <- b$data[[1]]$fill
  cls <- assign_classes(short, as.numeric(full))
  expect_equal(fills, pal[cls])                             # colour by full-set class
  expect_false(pal[1] %in% fills)                           # the unreached class is not drawn
  expect_true(pal[6] %in% fills)
  # the neutral pair straddles zero
  expect_true(all(fills[short > -1 & short < 0] == pal[3]))
  expect_true(all(fills[short > 0 & short < 1] == pal[4]))
  # range labels open both ends
  pr <- tile(scale_fill_intervals("brewer::BrBG", "fisher", 3, midpoint = 0, mirror = "value",
                                  labels = "ranges"))
  labs <- ggplot2::ggplot_build(pr)$plot$scales$scales[[1]]$get_labels()
  expect_match(labs[1], "^Under"); expect_match(labs[length(labs)], "above$")
  expect_error(scale_fill_intervals(mirror = "value"), "midpoint")
})

test_that("class_summary, class_guide and diagnose_map understand mirrored sets", {
  s <- class_summary(short, "fisher", 3, midpoint = 0, mirror = "value")
  expect_equal(attr(s, "mirror"), "value")
  expect_equal(nrow(s), 6L)
  expect_equal(s$n[1], 0L)                                  # the unreached outer class
  expect_output(print(s), "3 per side around 0")
  g <- class_guide(short, "fisher", n = 2:4, midpoint = 0, mirror = "value")
  expect_true(g$mirrored)
  expect_equal(g$table$actual_classes, c(4L, 6L, 8L))
  expect_true(all(g$table$empty_classes == 0))              # outer classes are exempt
  expect_output(print(g), "per side")
  d <- diagnose_map(tile(scale_fill_intervals("brewer::BrBG", "fisher", 3, midpoint = 0,
                                              mirror = "value", labels = "ranges"), short))
  expect_equal(d$mirror, "value")
  expect_equal(d$n_classes, 6L)
  expect_true(any(d$never_drawn))
  expect_true(any(grepl("never drawn", d$warnings)))
  expect_false(any(grepl("even number of classes", d$warnings)))
  expect_output(print(d), "mirrored by value")
})

test_that("open ends appear on continuous and stepped legends only where values were squished", {
  sc <- function(...) ggplot2::ggplot_build(tile(...))$plot$scales$scales[[1]]
  s1 <- sc(scale_fill_cont("brewer::BrBG", midpoint = 0, limits = c(-50, 50),
                           labels = label_map(suffix = "%")))
  expect_equal(range(s1$get_breaks()), c(-50, 50))
  labs <- s1$get_labels()
  expect_match(labs[1], "or less$"); expect_match(labs[length(labs)], "and above$")
  # squished on one side only
  s2 <- sc(scale_fill_cont("brewer::BrBG", midpoint = 0, limits = c(-100, 50)))
  labs2 <- s2$get_labels()
  expect_false(grepl("less", labs2[1])); expect_match(labs2[length(labs2)], "above$")
  # no squish: nothing changes
  s3 <- sc(scale_fill_cont("brewer::BrBG", midpoint = 0))
  expect_false(any(grepl("above|less", s3$get_labels())))
  # open = FALSE keeps the ticks plain; TRUE opens both even without squishing
  s4 <- sc(scale_fill_cont("viridis::mako", limits = c(-50, 50), labels = label_map(open = FALSE)))
  expect_false(any(grepl("above|less", s4$get_labels())))
  s5 <- sc(scale_fill_cont("viridis::mako", labels = label_map(open = TRUE)))
  expect_match(s5$get_labels()[length(s5$get_labels())], "above$")
  # a plain ggplot2 labeller is left alone
  s6 <- sc(scale_fill_cont("viridis::mako", limits = c(-50, 50), labels = scales::label_comma()))
  expect_false(any(grepl("above|less", s6$get_labels())))
  # rank scales never open
  s7 <- sc(scale_fill_rank("viridis::mako", limits = c(-50, 50)))
  expect_false(any(grepl("above|less", s7$get_labels())))
  # stepped bar ticks
  s8 <- sc(scale_fill_intervals("viridis::mako", "quantile", 4, limits = c(-50, 50)))
  l8 <- s8$get_labels()
  expect_match(l8[length(l8)], "above$")
  # the interval labels stay closed when nothing is squished
  s9 <- sc(scale_fill_intervals("viridis::mako", "quantile", 4, labels = "ranges"))
  expect_false(any(grepl("Under|above", s9$get_labels())))
})
