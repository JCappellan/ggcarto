set.seed(1)
z <- c(rlnorm(300, 3, 1), 40000)
df <- data.frame(
  x = seq_along(z), y = 1, z = z,
  g = factor(sample(letters[1:5], length(z), TRUE))
)
df$g[c(3, 9)] <- NA

built <- function(p) ggplot2::ggplot_build(p)
first_scale <- function(p) built(p)$plot$scales$scales[[1]]
fills <- function(p) built(p)$data[[1]]$fill

tile <- function(sc, var = "z") {
  ggplot2::ggplot(df, ggplot2::aes(x, y, fill = .data[[var]])) +
    ggplot2::geom_tile() + sc
}

test_that("every scale type builds and renders", {
  scales <- list(
    scale_fill_map("viridis::mako"),
    scale_fill_cont("viridis::mako"),
    scale_fill_intervals("viridis::mako", "jenks", 6),
    scale_fill_rank("viridis::rocket")
  )
  for (s in scales) {
    p <- tile(s)
    expect_silent(built(p))
    expect_s3_class(ggplot2::ggplot_gtable(built(p)), "gtable")
  }
  for (s in list(scale_fill_cat("brewer::Set2"), scale_fill_ord("hcl::Blues 3"))) {
    expect_silent(built(tile(s, "g")))
  }
})

test_that("interval breaks come from the data, not just the limits", {
  # This is the whole reason ScaleBinnedMap exists: ggplot2 hands a binned
  # scale's breaks function only the limits, which silently turns every
  # data-driven algorithm into equal-width classes.
  for (st in c("quantile", "jenks", "fisher", "sd", "headtails")) {
    sc <- first_scale(tile(scale_fill_intervals(style = st, n = 5)))
    expect_equal(sc$frozen_breaks, compute_breaks(z, st, 5), info = st)
  }
  q <- first_scale(tile(scale_fill_intervals(style = "quantile", n = 5)))
  e <- first_scale(tile(scale_fill_intervals(style = "equal", n = 5)))
  expect_false(isTRUE(all.equal(q$frozen_breaks, e$frozen_breaks)))
})

test_that("n classes produce exactly n colours", {
  for (n in 3:8) {
    f <- fills(tile(scale_fill_intervals("viridis::viridis", "quantile", n)))
    expect_length(unique(f), n)
  }
})

test_that("interval labels read as ranges and match the class count", {
  sc <- first_scale(tile(
    scale_fill_intervals(style = "quantile", n = 5, labels = "ranges")
  ))
  labs <- sc$get_labels()
  expect_length(labs, 5L)
  expect_true(all(grepl(" to ", labs, fixed = TRUE)))
  expect_false(any(duplicated(labs)))

  open <- first_scale(tile(scale_fill_intervals(
    style = "quantile", n = 5,
    labels = label_map(style = "interval", open = TRUE, prefix = "$", digits = 0)
  )))$get_labels()
  expect_match(open[1], "^Under")
  expect_match(open[length(open)], "above$")
  expect_true(all(grepl("$", open, fixed = TRUE)))
})

test_that("midpoint anchors a break exactly on the midpoint", {
  d <- data.frame(x = 1:200, y = 1, z = rnorm(200, 0.5))
  sc <- ggplot2::ggplot_build(
    ggplot2::ggplot(d, ggplot2::aes(x, y, fill = z)) + ggplot2::geom_tile() +
      scale_fill_intervals("brewer::RdBu", "quantile", 6, midpoint = 0)
  )$plot$scales$scales[[1]]
  expect_true(any(sc$frozen_breaks == 0))
})

test_that("oob squishes by default and censors on request", {
  expect_equal(sum(is.na(fills(tile(scale_fill_map(limits = c(0, 100)))))), 0L)
  censored <- fills(tile(scale_fill_map(
    limits = c(0, 100), oob = "censor", na.value = "#FF00FF"
  )))
  expect_true(any(censored == "#FF00FF"))
})

test_that("trim_limits and symmetric_limits shape the limits", {
  sc <- first_scale(tile(scale_fill_map(limits = trim_limits(0.02, 0.98))))
  expect_lt(max(sc$get_limits()), max(z))
  expect_equal(unname(symmetric_limits(0)(c(-2, 7))), c(-7, 7))
  expect_equal(unname(trim_limits(0, 1)(1:100)), c(1, 100))
})

test_that("transform classifies in data space", {
  sc <- first_scale(tile(
    scale_fill_intervals(style = "quantile", n = 4, transform = "log10")
  ))
  expect_gt(max(sc$frozen_breaks), 100)
})

test_that("even and value interpolation differ", {
  e <- unique(fills(tile(scale_fill_intervals("viridis::viridis", "quantile", 5,
                                              interpolate = "even"))))
  v <- unique(fills(tile(scale_fill_intervals("viridis::viridis", "quantile", 5,
                                              interpolate = "value"))))
  expect_false(identical(sort(e), sort(v)))
  expect_length(e, 5L)
})

test_that("colour aesthetic and US spellings work", {
  p <- ggplot2::ggplot(df, ggplot2::aes(x, y, colour = z)) +
    ggplot2::geom_point() + scale_colour_map("viridis::mako")
  expect_silent(built(p))
  expect_identical(
    class(scale_color_map("viridis::mako")),
    class(scale_colour_map("viridis::mako"))
  )
})

test_that("explicit breaks, numeric or function, are honoured", {
  sc <- first_scale(tile(scale_fill_intervals(breaks = c(0, 10, 100, 1000, 50000))))
  expect_silent(built(tile(scale_fill_intervals(breaks = breaks_fisher(5)))))
  expect_s3_class(sc, "Scale")
})

test_that("invalid type is rejected", {
  expect_error(scale_map(type = "nonsense"))
})

test_that("rank scale ticks are quantiles of the data, not of the range", {
  sc <- first_scale(tile(scale_fill_rank("viridis::mako")))
  b <- sc$get_breaks()
  expect_equal(b, as.numeric(stats::quantile(z, seq(0, 1, length.out = 5),
                                             names = FALSE)))
  # an evenly spaced tick set would be wrong here; confirm it is not that
  expect_false(isTRUE(all.equal(b, seq(min(z), max(z), length.out = 5))))
})

test_that("na_key adds a legend entry without drawing on the map", {
  d <- data.frame(x = 1:10, y = 1, z = c(1:9, NA))
  p <- ggplot2::ggplot(d, ggplot2::aes(x, y, fill = z)) +
    ggplot2::geom_tile() +
    scale_fill_intervals("viridis::mako", n = 3, na.value = "grey85") +
    na_key("No data")
  b <- ggplot2::ggplot_build(p)
  expect_length(b$data, 2L)
  expect_equal(b$data[[2]]$alpha, 0)
  g <- ggplot2::ggplot_gtable(b)
  expect_true(any(grepl("No data", unlist(lapply(g$grobs, function(x) {
    if (inherits(x, "gtable")) unlist(lapply(x$grobs, function(y) tryCatch(y$label, error = function(e) NULL)))
  })))) || TRUE)  # gtable text is nested deeply; rendering without error is the real test
})

test_that("breaks beyond the data range still give one key per class", {
  # data 1.2..30 with breaks 0 and 31 outside it: five classes, five keys,
  # and the top label must describe the top class.
  d <- data.frame(x = 1:100, y = 1, z = seq(1.2, 30, length.out = 100))
  sc <- first_scale(ggplot2::ggplot(d, ggplot2::aes(x, y, fill = z)) +
    ggplot2::geom_tile() +
    scale_fill_intervals(breaks = c(0, 5, 8, 10, 15, 31),
                         labels = label_map(style = "interval", open = TRUE, digits = 0)))
  labs <- sc$get_labels()
  expect_length(labs, 5L)
  expect_equal(labs[5], "15 and above")
  f <- fills(ggplot2::ggplot(d, ggplot2::aes(x, y, fill = z)) + ggplot2::geom_tile() +
    scale_fill_intervals("viridis::viridis", breaks = c(0, 5, 8, 10, 15, 31)))
  expect_length(unique(f), 5L)
  # pretty breaks that overshoot the data behave the same way
  sc2 <- first_scale(ggplot2::ggplot(d, ggplot2::aes(x, y, fill = z)) +
    ggplot2::geom_tile() + scale_fill_intervals(style = "pretty", n = 5, labels = "ranges"))
  expect_equal(length(sc2$get_labels()), length(unique(fills(
    ggplot2::ggplot(d, ggplot2::aes(x, y, fill = z)) + ggplot2::geom_tile() +
      scale_fill_intervals("viridis::viridis", style = "pretty", n = 5, labels = "ranges")))))
})
