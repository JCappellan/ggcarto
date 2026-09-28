# Ad-hoc smoke test: source the package and exercise every public entry point.
# Not part of the package build (dev/ is in .Rbuildignore).
suppressMessages({
  library(ggplot2)
})
for (f in list.files("R", pattern = "[.]R$", full.names = TRUE)) source(f)

ok <- function(label, expr) {
  res <- tryCatch({ force(expr); "PASS" },
                  error = function(e) paste("FAIL:", conditionMessage(e)),
                  warning = function(w) paste("WARN:", conditionMessage(w)))
  cat(sprintf("%-58s %s\n", label, res))
  invisible(res)
}

set.seed(42)
x <- c(rlnorm(500, meanlog = 3, sdlog = 1.1), 50000)
z <- c(rnorm(200), 40)

cat("\n== break algorithms ==\n")
for (st in setdiff(map_styles(), "fixed")) {
  ok(paste0("compute_breaks(", st, ")"), {
    b <- compute_breaks(x, style = st, n = 5)
    stopifnot(is.numeric(b), length(b) >= 2, !is.unsorted(b))
    stopifnot(b[1] <= min(x) + 1e-8, b[length(b)] >= max(x) - 1e-8)
  })
}
ok("compute_breaks(fixed)", {
  b <- compute_breaks(x, "fixed", args = list(fixedBreaks = c(0, 10, 100, 1e5)))
  stopifnot(length(b) == 4)
})
ok("degenerate single value", {
  b <- compute_breaks(rep(5, 10), "quantile", 5); stopifnot(length(b) == 2)
})
ok("empty input", stopifnot(length(compute_breaks(numeric(0))) == 0))
ok("all-NA input", stopifnot(length(compute_breaks(c(NA, NA))) == 0))
ok("breaks_sd symmetric about mean", {
  b <- breaks_sd(6)(rnorm(1000, 10, 2))
  stopifnot(length(b) >= 3)
})
ok("breaks_jenks factory", stopifnot(length(breaks_jenks(4)(x)) == 5))
ok("breaks_quantile factory", stopifnot(length(breaks_quantile(6)(x)) == 7))
ok("breaks_* usable in plain ggplot2", {
  p <- ggplot(faithfuld, aes(waiting, eruptions, fill = density)) +
    geom_raster() + scale_fill_binned(breaks = breaks_quantile(5))
  invisible(ggplot_build(p))
})
ok("print.ggcarto_breaks", capture.output(print(breaks_jenks(5))))

cat("\n== palettes ==\n")
ok("hcl by bare name", stopifnot(length(as_palette("Zissou 1")(7)) == 7))
ok("hcl:: prefix", stopifnot(length(as_palette("hcl::Blues 3")(5)) == 5))
ok("brewer:: prefix", stopifnot(length(as_palette("brewer::RdBu")(9)) == 9))
ok("brewer beyond maxcolors", stopifnot(length(as_palette("brewer::Set2")(20)) == 20))
ok("viridis:: prefix", stopifnot(length(as_palette("viridis::mako")(4)) == 4))
ok("colour vector", stopifnot(length(as_palette(c("red", "white", "blue"))(11)) == 11))
ok("function palette", stopifnot(length(as_palette(function(n) rep("red", n))(3)) == 3))
ok("reverse", {
  a <- as_palette("viridis::viridis")(5)
  b <- as_palette("viridis::viridis", reverse = TRUE)(5)
  stopifnot(identical(a, rev(b)))
})
ok("range trimming changes colours", {
  a <- as_palette("viridis::viridis")(5)
  b <- as_palette("viridis::viridis", range = c(0.2, 0.8))(5)
  stopifnot(!identical(a, b), length(b) == 5)
})
ok("alpha applied", {
  b <- as_palette("viridis::viridis", alpha = 0.5)(3)
  stopifnot(all(nchar(b) == 9))
})
ok("bad range rejected", {
  r <- tryCatch(as_palette("viridis::viridis", range = c(1, 0)), error = function(e) "err")
  stopifnot(identical(r, "err"))
})
ok("unknown palette errors clearly", {
  r <- tryCatch(as_palette("not_a_palette_xyz"), error = function(e) conditionMessage(e))
  stopifnot(grepl("Could not find", r))
})
ok("map_palettes()", stopifnot(nrow(map_palettes()) > 100))
ok("map_palettes(div)", stopifnot(all(map_palettes("div")$type == "div")))
ok("palette_sources()", stopifnot(nrow(palette_sources()) >= 5))

cat("\n== labels ==\n")
ok("label_map values", {
  stopifnot(identical(label_map(prefix = "$", digits = 0)(c(1000, 25000)),
                      c("$1,000", "$25,000")))
})
ok("label_map intervals", {
  stopifnot(identical(label_map(style = "interval", digits = 0)(c(0, 10, 20)),
                      c("0 to 10", "10 to 20")))
})
ok("label_map open ends", {
  l <- label_map(style = "interval", open = TRUE, digits = 0)(c(0, 10, 20, 30))
  stopifnot(grepl("^Under", l[1]), grepl("above$", l[3]))
})
ok("label_map scale + suffix", {
  stopifnot(identical(label_map(scale = 100, suffix = "%", digits = 1)(0.256), "25.6%"))
})
ok("auto digits on integers", stopifnot(identical(label_map()(c(1, 2, 3)), c("1", "2", "3"))))

cat("\n== scales build ==\n")
df <- data.frame(x = seq_along(z), y = 1, z = z,
                 g = factor(sample(letters[1:5], length(z), TRUE)))
df$g[c(3, 9)] <- NA

build <- function(p) invisible(ggplot_build(p))

ok("continuous", build(ggplot(df, aes(x, y, fill = z)) + geom_tile() +
  scale_fill_map("viridis::mako")))
ok("continuous + midpoint + trim_limits", build(ggplot(df, aes(x, y, fill = z)) +
  geom_tile() + scale_fill_map("brewer::RdBu", midpoint = 0,
                               limits = trim_limits(0.02, 0.98))))
ok("continuous + symmetric_limits", build(ggplot(df, aes(x, y, fill = z)) +
  geom_tile() + scale_fill_map("brewer::RdBu", midpoint = 0,
                               limits = symmetric_limits(0))))
ok("continuous + alpha", build(ggplot(df, aes(x, y, fill = z)) + geom_tile() +
  scale_fill_map("viridis::mako", alpha = 0.6)))
ok("continuous + transform", build(ggplot(data.frame(x = 1:50, y = 1, z = exp(1:50 / 5)),
  aes(x, y, fill = z)) + geom_tile() + scale_fill_map(transform = "log10")))
ok("intervals jenks", build(ggplot(df, aes(x, y, fill = z)) + geom_tile() +
  scale_fill_intervals("hcl::Batlow", style = "jenks", n = 6)))
ok("intervals quantile + ranges", build(ggplot(df, aes(x, y, fill = z)) + geom_tile() +
  scale_fill_intervals(style = "quantile", n = 5, labels = "ranges")))
ok("intervals sd", build(ggplot(df, aes(x, y, fill = z)) + geom_tile() +
  scale_fill_intervals(style = "sd", n = 6)))
ok("intervals headtails", build(ggplot(data.frame(x = seq_along(x), y = 1, z = x),
  aes(x, y, fill = z)) + geom_tile() + scale_fill_intervals(style = "headtails")))
ok("intervals interpolate=value", build(ggplot(df, aes(x, y, fill = z)) + geom_tile() +
  scale_fill_intervals(style = "quantile", n = 5, interpolate = "value")))
ok("intervals explicit breaks", build(ggplot(df, aes(x, y, fill = z)) + geom_tile() +
  scale_fill_intervals(breaks = c(-5, -1, 0, 1, 5, 45))))
ok("intervals breaks fn", build(ggplot(df, aes(x, y, fill = z)) + geom_tile() +
  scale_fill_intervals(breaks = breaks_fisher(5))))
ok("intervals midpoint anchored", build(ggplot(df, aes(x, y, fill = z)) + geom_tile() +
  scale_fill_intervals("brewer::RdBu", style = "quantile", n = 6, midpoint = 0)))
ok("categorical", build(ggplot(df, aes(x, y, fill = g)) + geom_tile() +
  scale_fill_cat("brewer::Set2")))
ok("categorical NA key", build(ggplot(df, aes(x, y, fill = g)) + geom_tile() +
  scale_fill_cat("brewer::Set2", na.value = "grey80", na.label = "No data")))
ok("ordinal", build(ggplot(df, aes(x, y, fill = g)) + geom_tile() +
  scale_fill_ord("hcl::Blues 3")))
ok("rank", build(ggplot(df, aes(x, y, fill = z)) + geom_tile() +
  scale_fill_rank("viridis::rocket")))
ok("colour aesthetic", build(ggplot(df, aes(x, y, colour = z)) + geom_point() +
  scale_colour_map("viridis::mako")))
ok("scale_color_map alias", build(ggplot(df, aes(x, y, colour = z)) + geom_point() +
  scale_color_map()))
ok("oob censor", build(ggplot(df, aes(x, y, fill = z)) + geom_tile() +
  scale_fill_map(limits = c(-2, 2), oob = "censor")))
ok("oob squish default", build(ggplot(df, aes(x, y, fill = z)) + geom_tile() +
  scale_fill_map(limits = c(-2, 2))))
ok("default palette when NULL", build(ggplot(df, aes(x, y, fill = z)) + geom_tile() +
  scale_fill_map()))
ok("ggcarto_palettes() changes default", {
  old <- ggcarto_palettes(seq = "brewer::Greens")
  on.exit(options(ggcarto.palettes = old))
  build(ggplot(df, aes(x, y, fill = z)) + geom_tile() + scale_fill_map())
})

cat("\n== rendering (gtable) ==\n")
ok("interval legend renders", {
  p <- ggplot(df, aes(x, y, fill = z)) + geom_tile() +
    scale_fill_intervals(style = "quantile", n = 5, labels = "ranges")
  invisible(ggplot_gtable(ggplot_build(p)))
})
ok("coloursteps legend renders", {
  p <- ggplot(df, aes(x, y, fill = z)) + geom_tile() +
    scale_fill_intervals(style = "quantile", n = 5)
  invisible(ggplot_gtable(ggplot_build(p)))
})
ok("colourbar renders", {
  p <- ggplot(df, aes(x, y, fill = z)) + geom_tile() + scale_fill_map()
  invisible(ggplot_gtable(ggplot_build(p)))
})

cat("\n== colour correctness ==\n")
ok("even interpolation gives n distinct class colours", {
  p <- ggplot(df, aes(x, y, fill = z)) + geom_tile() +
    scale_fill_intervals("viridis::viridis", style = "quantile", n = 5)
  d <- ggplot_build(p)$data[[1]]
  stopifnot(length(unique(d$fill)) == 5)
})
ok("squish maps extremes to end colours", {
  p <- ggplot(df, aes(x, y, fill = z)) + geom_tile() +
    scale_fill_map("viridis::viridis", limits = c(-2, 2))
  d <- ggplot_build(p)$data[[1]]
  stopifnot(sum(is.na(d$fill)) == 0)
})
ok("censor makes out-of-range NA-coloured", {
  p <- ggplot(df, aes(x, y, fill = z)) + geom_tile() +
    scale_fill_map("viridis::viridis", limits = c(-2, 2), oob = "censor",
                   na.value = "#FF00FF")
  d <- ggplot_build(p)$data[[1]]
  stopifnot(any(d$fill == "#FF00FF"))
})

cat("\ndone\n")
