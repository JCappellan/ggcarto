# ===========================================================================
# Bivariate choropleths
#
# A bivariate map colours each area by two variables at once. Each variable is
# classed into n intervals, giving n x n combinations, and each combination
# gets one colour from an n x n grid. ggplot2 has no idea any of this is
# happening: from its point of view the fill is an ordinary factor with
# levels "1-1", "1-2", ..., "n-n" mapped through a manual scale. That is the
# whole trick, and it is why the legend has to be drawn separately -- a
# ggplot2 legend is a column of keys, not a grid.
#
# Three pieces:
#   classify_bivariate()   two numeric vectors -> one factor of "i-j" classes
#   scale_fill_bivariate() maps those classes to an n x n colour grid
#   legend_bivariate()     draws the grid as a small ggplot, with
#   inset_legend()         to put it on the map
# ===========================================================================

#' Built-in bivariate palettes
#'
#' Two kinds of preset. *Corner* palettes are defined by four colours -- both
#' variables low, `x` high, `y` high, both high -- and [bivariate_palette()]
#' fills the grid in between by interpolation, so they work at any `n`.
#' *Grid* palettes are complete 3 x 3 schemes: the ones Joshua Stevens
#' published for bivariate choropleths (Stevens 2015), as popularised by the
#' biscale package; they are resampled for other `n`. In every preset the
#' both-low corner is pale, `x` and `y` pull towards two different hues, and
#' the both-high corner is the darkest cell.
#'
#' @return A named list. Corner palettes are length-4 character vectors in the
#'   order low-low, x-high, y-high, high-high; grid palettes are length-9
#'   vectors in `"1-1", "1-2", ..., "3-3"` order (first index the class of
#'   `x`). [plot_bivariate_palettes()] draws them all.
#' @references Stevens, J. (2015). Bivariate choropleth maps: a how-to guide.
#'   \url{https://www.joshuastevens.net/cartography/make-a-bivariate-choropleth-map/}
#' @examples
#' names(bivariate_palettes())
#' bivariate_palettes()[["blue-red"]]
#' @export
bivariate_palettes <- function() {
  list(
    # corner palettes: low-low, x-high, y-high, high-high
    "blue-red"      = c("#f3f3f3", "#4a90c2", "#d0473b", "#3b1f3d"),
    "purple-green"  = c("#f3f3f3", "#8e5fa8", "#4faa6e", "#2b4a4d"),
    "green-purple"  = c("#f3f3f3", "#4faa6e", "#8e5fa8", "#2b4a4d"),
    "teal-pink"     = c("#f3f3f3", "#3aa6a6", "#d65f9a", "#3f2a4b"),
    "pink-teal"     = c("#f3f3f3", "#d65f9a", "#3aa6a6", "#3f2a4b"),
    "blue-orange"   = c("#f3f3f3", "#3b7fc4", "#e58a2b", "#4a3a2a"),
    "orange-blue"   = c("#f3f3f3", "#e58a2b", "#3b7fc4", "#4a3a2a"),
    "blue-yellow"   = c("#f3f3f3", "#4c6fb5", "#e0c23b", "#3d4a2b"),
    "red-teal"      = c("#f3f3f3", "#c9582b", "#2a8a7c", "#2f2f3a"),
    "brown-teal"    = c("#f3f3f3", "#a8743a", "#3a9a9a", "#3a3d38"),
    "gold-purple"   = c("#f3f3f3", "#d1a92e", "#7c5aa8", "#3b2d3f"),
    "purple-orange" = c("#f3f3f3", "#7c5aa8", "#e0862a", "#4a3238"),
    "green-blue"    = c("#f3f3f3", "#5aa85a", "#3b6fc4", "#1f3d4a"),
    "grey-blue"     = c("#f3f3f3", "#7a7a7a", "#4a90c2", "#1f3a52"),
    "grey-red"      = c("#f3f3f3", "#7a7a7a", "#d0473b", "#4d2222"),
    "grey-green"    = c("#f3f3f3", "#7a7a7a", "#4faa6e", "#1f3d2c"),
    "grey-purple"   = c("#f3f3f3", "#7a7a7a", "#8e5fa8", "#3a2a48"),
    # Stevens grids: 1-1, 1-2, 1-3, 2-1, ..., 3-3 (x-major)
    "stevens-purpleblue" = c("#e8e8e8", "#dfb0d6", "#be64ac",
                             "#ace4e4", "#a5add3", "#8c62aa",
                             "#5ac8c8", "#5698b9", "#3b4994"),
    "stevens-violet"     = c("#cabed0", "#bc7c8f", "#ae3a4e",
                             "#89a1c8", "#806a8a", "#77324c",
                             "#4885c1", "#3e6b86", "#3f2949"),
    "stevens-greenpink"  = c("#e8e8e8", "#b8d6be", "#73ae80",
                             "#b5c0da", "#90b2b3", "#5a9178",
                             "#6c83b5", "#567994", "#2a5a5b"),
    "stevens-cyanred"    = c("#e8e8e8", "#e4acac", "#c85a5a",
                             "#b0d5df", "#ad9ea5", "#985356",
                             "#64acbe", "#627f8c", "#574249"),
    "stevens-brown"      = c("#e8e8e8", "#cbb8d7", "#9972af",
                             "#e4d9ac", "#c8ada0", "#976b82",
                             "#c8b35a", "#ad9c53", "#804d36")
  )
}

#' Draw every built-in bivariate palette
#'
#' @param n Classes per variable to draw each palette with.
#' @return A ggplot object: one small grid per palette.
#' @examples
#' plot_bivariate_palettes()
#' @export
plot_bivariate_palettes <- function(n = 3) {
  pals <- bivariate_palettes()
  cells <- do.call(rbind, lapply(names(pals), function(nm) {
    pal <- bivariate_palette(nm, n)
    g <- expand.grid(x = seq_len(n), y = seq_len(n))
    g$fill <- unclass(pal)[paste(g$x, g$y, sep = "-")]
    g$palette <- nm
    g
  }))
  cells$palette <- factor(cells$palette, levels = names(pals))
  ggplot2::ggplot(cells, ggplot2::aes(x = .data$x, y = .data$y)) +
    ggplot2::geom_tile(ggplot2::aes(fill = .data$fill),
                       colour = ggcarto_colours()[["paper"]], linewidth = 0.6) +
    ggplot2::scale_fill_identity() +
    ggplot2::coord_fixed(expand = FALSE) +
    ggplot2::facet_wrap(~ palette, ncol = 5) +
    ggplot2::labs(title = "Bivariate palettes",
                  subtitle = "x increases to the right, y upwards; both-high is the top-right cell") +
    theme_ggcarto() +
    ggplot2::theme(strip.text = ggplot2::element_text(hjust = 0.5, face = "plain",
                                                      size = ggplot2::rel(0.75)),
                   panel.spacing = grid::unit(10, "pt"), strip.clip = "off",
                   plot.title = ggplot2::element_text(size = ggplot2::rel(1.1)))
}

#' Build an n x n bivariate colour grid
#'
#' @param palette One of:
#'   * a name from [bivariate_palettes()], e.g. `"blue-red"` or
#'     `"stevens-purpleblue"`;
#'   * four colours: low-low, x-high, y-high, high-high. The grid is filled in
#'     by bilinear interpolation between them;
#'   * a complete `k * k` grid of colours in `"1-1", "1-2", ..., "k-k"` order
#'     (x-major, as [bivariate_palette()] itself returns and as biscale
#'     palettes are laid out), resampled to `n` if `k != n`;
#'   * a list with elements `x` and `y`, each a palette specification
#'     accepted by [as_palette()]. The two sequential ramps are blended by
#'     multiplying their colours, the way a printer overlays two inks. Works
#'     best when both ramps start near white;
#'   * `"c4a::<name>"` for a bivariate palette from cols4all (its `bivs`,
#'     `bivd`, `bivc` and `bivg` types), if that package is installed.
#' @param n Number of classes per variable. `3` is the convention; `4` is
#'   readable; anything above `5` is hard to read on a map.
#' @param reverse_x,reverse_y Flip the direction of either variable.
#' @return A character vector of `n * n` colours named `"1-1"`, `"1-2"`, ...,
#'   `"n-n"`, where the first index is the class of `x` and the second the
#'   class of `y`. Class `"1-1"` is both low; `"n-n"` is both high. The vector
#'   carries an attribute `n` and has class `"ggcarto_bipal"`, and prints as a
#'   grid.
#' @examples
#' bivariate_palette("blue-red", n = 3)
#' bivariate_palette("stevens-purpleblue", n = 4)   # a 3 x 3 grid resampled
#' bivariate_palette(c("white", "steelblue", "firebrick", "black"), n = 4)
#' bivariate_palette(list(x = "brewer::Blues", y = "brewer::Reds"))
#' @export
bivariate_palette <- function(palette = "blue-red", n = 3,
                              reverse_x = FALSE, reverse_y = FALSE) {
  n <- as.integer(n)
  if (is.na(n) || n < 2L) abort_ggcarto("`n` must be at least 2.")
  if (inherits(palette, "ggcarto_bipal")) {
    if (attr(palette, "n") != n) {
      abort_ggcarto("This palette was built for n = ", attr(palette, "n"),
                    ", not n = ", n, ".")
    }
    return(palette)
  }

  grid <- if (is.list(palette)) {
    if (!all(c("x", "y") %in% names(palette))) {
      abort_ggcarto("A list `palette` needs elements `x` and `y`.")
    }
    blend_multiply(as_palette(palette$x)(n), as_palette(palette$y)(n))
  } else {
    spec <- if (is.character(palette) && length(palette) == 1L) {
      if (startsWith(palette, "c4a::")) {
        c4a_bivariate(sub("^c4a::", "", palette), n)
      } else {
        pals <- bivariate_palettes()
        if (!palette %in% names(pals)) {
          abort_ggcarto(
            "Unknown bivariate palette \"", palette, "\". Choose one of: ",
            paste(names(pals), collapse = ", "), "."
          )
        }
        pals[[palette]]
      }
    } else if (is.character(palette)) {
      if (!all(is_colour(palette))) abort_ggcarto("`palette` contains invalid colours.")
      palette
    } else {
      abort_ggcarto("`palette` must be a palette name, four corner colours, ",
                    "a k * k grid of colours, or list(x = , y = ).")
    }
    if (is.matrix(spec)) {
      resample_grid(spec, n)
    } else if (length(spec) == 4L) {
      blend_bilinear(spec, n)
    } else {
      k <- sqrt(length(spec))
      if (k != round(k) || k < 2) {
        abort_ggcarto("A colour vector `palette` must have 4 (corners) or k * k (grid) colours.")
      }
      resample_grid(matrix(spec, k, k, byrow = TRUE), n)
    }
  }

  if (isTRUE(reverse_x)) grid <- grid[n:1, , drop = FALSE]
  if (isTRUE(reverse_y)) grid <- grid[, n:1, drop = FALSE]

  out <- as.vector(t(grid))   # x-major: 1-1, 1-2, ..., 1-n, 2-1, ...
  names(out) <- bivariate_levels(n)
  attr(out, "n") <- n
  class(out) <- c("ggcarto_bipal", "character")
  out
}

# grid[i, j]: i = x class, j = y class
blend_bilinear <- function(corners, n) {
  ll <- corners[1]; xh <- corners[2]; yh <- corners[3]; hh <- corners[4]
  u <- seq(0, 1, length.out = n)
  bottom <- scales::colour_ramp(c(ll, xh))(u)   # y low, x varies
  top    <- scales::colour_ramp(c(yh, hh))(u)   # y high, x varies
  grid <- matrix(NA_character_, n, n)
  for (i in seq_len(n)) {
    grid[i, ] <- scales::colour_ramp(c(bottom[i], top[i]))(u)
  }
  grid
}

# A k x k grid (grid[i, j]: i = x class, j = y class) resampled to n x n:
# ramp along each row, then along each column of the result.
resample_grid <- function(grid, n) {
  k <- nrow(grid)
  if (k == n) return(grid)
  u <- seq(0, 1, length.out = n)
  rows <- t(vapply(seq_len(k), function(i) scales::colour_ramp(grid[i, ])(u), character(n)))
  out <- matrix(NA_character_, n, n)
  for (j in seq_len(n)) out[, j] <- scales::colour_ramp(rows[, j])(u)
  out
}

# cols4all bivariate palettes come back as an n x n matrix (rows = first
# variable). Only reachable when cols4all is installed.
c4a_bivariate <- function(nm, n) {
  need_pkg("cols4all", "for \"c4a::\" bivariate palettes")
  m <- tryCatch(cols4all::c4a(nm, n = n), error = function(e) NULL)
  if (is.null(m)) m <- tryCatch(cols4all::c4a(nm, n = 3), error = function(e) NULL)
  if (!is.matrix(m)) {
    abort_ggcarto("\"c4a::", nm, "\" is not a bivariate cols4all palette ",
                  "(see cols4all::c4a_palettes(type = \"bivs\")).")
  }
  unname(m)
}

blend_multiply <- function(xcols, ycols) {
  n <- length(xcols)
  xr <- grDevices::col2rgb(xcols) / 255
  yr <- grDevices::col2rgb(ycols) / 255
  grid <- matrix(NA_character_, n, n)
  for (i in seq_len(n)) for (j in seq_len(n)) {
    m <- xr[, i] * yr[, j]
    grid[i, j] <- grDevices::rgb(m[1], m[2], m[3])
  }
  grid
}

bivariate_levels <- function(n) {
  as.vector(t(outer(seq_len(n), seq_len(n), paste, sep = "-")))
}

#' @export
print.ggcarto_bipal <- function(x, ...) {
  n <- attr(x, "n")
  cat(sprintf("<ggcarto bivariate palette: %d x %d>\n", n, n))
  m <- matrix(unclass(x), n, n, byrow = TRUE,
              dimnames = list(paste0("x", seq_len(n)), paste0("y", seq_len(n))))
  print(m[n:1, , drop = FALSE], quote = FALSE)  # x high at the top, like the legend
  invisible(x)
}

# ---------------------------------------------------------------------------
# Classification
# ---------------------------------------------------------------------------

#' Class two variables into bivariate categories
#'
#' Cuts `x` and `y` into `n` classes each and combines them into one factor
#' with levels `"1-1"`, `"1-2"`, ..., `"n-n"` (first index is the class of
#' `x`). Feed the result to `aes(fill = )` with [scale_fill_bivariate()].
#'
#' @param x,y Numeric vectors of equal length.
#' @param n Number of classes per variable.
#' @param style Class-interval algorithm applied to each variable. Any style
#'   from [map_styles()]; `"quantile"` is the usual choice for bivariate maps
#'   because it guarantees every row and column of the legend is populated.
#'   A length-2 vector applies different styles to `x` and `y`.
#' @param breaks_x,breaks_y Explicit break points (length `n + 1`) for either
#'   variable, in the original units, overriding `style`.
#' @param style_x,style_y Styles for each variable separately; they take
#'   precedence over `style`.
#' @param trans_x,trans_y A transformation to class the variable under:
#'   a name such as `"log10"`, `"sqrt"` or `"identity"`, or a transformation
#'   object from [scales::as.transform()]. Breaks are computed in transformed
#'   space and stored in the original units, so [legend_bivariate()] labels
#'   the legend in the units you started with. Values outside the
#'   transformation's domain (zero under a log) become missing, with a
#'   warning.
#' @param closure Which side of a break is closed; see [class_summary()].
#' @param ... Extra arguments passed to [compute_breaks()].
#' @return A factor with `n * n` levels (all retained, even if empty) and
#'   attributes `breaks_x`, `breaks_y` (in original units), `n`, `style_x`,
#'   `style_y`, `trans_x`, `trans_y` and `closure`, which
#'   [legend_bivariate()] can read to label the legend. `table()` on it
#'   counts the cells; `matrix(table(cls), n, n, byrow = TRUE)` lays them
#'   out as the legend grid.
#' @examples
#' set.seed(1)
#' inc <- rlnorm(100, 10, 0.5)
#' edu <- runif(100, 5, 60)
#' cls <- classify_bivariate(inc, edu, n = 3)
#' table(cls)
#' attr(cls, "breaks_x")
#'
#' # income classed on a log scale, breaks still reported in dollars
#' cls2 <- classify_bivariate(inc, edu, n = 3, style_x = "equal",
#'                            style_y = "quantile", trans_x = "log10")
#' attr(cls2, "breaks_x")
#' @export
classify_bivariate <- function(x, y, n = 3, style = "quantile",
                               breaks_x = NULL, breaks_y = NULL,
                               style_x = NULL, style_y = NULL,
                               trans_x = "identity", trans_y = "identity",
                               closure = c("right", "left"), ...) {
  if (length(x) != length(y)) abort_ggcarto("`x` and `y` must have the same length.")
  closure <- match.arg(closure)
  n <- as.integer(n)
  if (is.na(n) || n < 2L) abort_ggcarto("`n` must be at least 2.")
  style <- rep_len(style, 2L)
  style_x <- match.arg(style_x %||% style[1], map_styles())
  style_y <- match.arg(style_y %||% style[2], map_styles())
  args <- list(...)

  cx <- classify_one(x, n, style_x, breaks_x, trans_x, closure, args, "x")
  cy <- classify_one(y, n, style_y, breaks_y, trans_y, closure, args, "y")

  out <- factor(paste(cx$idx, cy$idx, sep = "-"), levels = bivariate_levels(n))
  out[is.na(cx$idx) | is.na(cy$idx)] <- NA
  attr(out, "breaks_x") <- cx$breaks
  attr(out, "breaks_y") <- cy$breaks
  attr(out, "n") <- n
  attr(out, "style_x") <- if (is.null(breaks_x)) style_x else "fixed"
  attr(out, "style_y") <- if (is.null(breaks_y)) style_y else "fixed"
  attr(out, "trans_x") <- cx$trans
  attr(out, "trans_y") <- cy$trans
  attr(out, "closure") <- closure
  out
}

# One variable: transform, break, cut, and report the breaks in original
# units. Cutting happens in whichever space the breaks are native to, so a
# value sitting exactly on a user-supplied break is never moved by rounding.
classify_one <- function(v, n, style, breaks, trans, closure, args, which) {
  tr <- scales::as.transform(trans)
  ident <- identical(tr$name, "identity")
  v <- suppressWarnings(as.numeric(v))
  if (!is.null(breaks)) {
    # Breaks in original units: cut there; the transform is not needed.
    b <- check_bibreaks(breaks, n, which)
    idx <- assign_classes(v, b, closure)
  } else {
    vt <- if (ident) v else suppressWarnings(tr$transform(v))
    lost <- is.finite(v) & !is.finite(vt)
    if (any(lost)) {
      warning(sprintf(
        "%d value%s of `%s` %s outside the domain of the \"%s\" transform and %s treated as missing.",
        sum(lost), if (sum(lost) > 1) "s" else "", which,
        if (sum(lost) > 1) "fall" else "falls", tr$name,
        if (sum(lost) > 1) "are" else "is"
      ), call. = FALSE)
      vt[lost] <- NA
    }
    bt <- compute_breaks(vt, style = style, n = n, args = args)
    bt <- check_bibreaks(bt, n, which)
    idx <- assign_classes(vt, bt, closure)
    b <- if (ident) bt else tr$inverse(bt)
  }
  list(idx = idx, breaks = b, trans = tr$name)
}

check_bibreaks <- function(b, n, which) {
  b <- sort(unique(as.numeric(b)))
  if (length(b) != n + 1L) {
    abort_ggcarto(sprintf(
      "Could not form %d classes for `%s`: got %d distinct break points (%s). %s",
      n, which, length(b), paste(signif(b, 4), collapse = ", "),
      "Try a smaller `n`, a different `style`, or supply breaks yourself."
    ))
  }
  b
}

# ---------------------------------------------------------------------------
# Scale
# ---------------------------------------------------------------------------

#' Bivariate fill and colour scales
#'
#' Maps the classes from [classify_bivariate()] to the colours of
#' [bivariate_palette()]. Every one of the `n * n` classes keeps its colour
#' even if no observation falls in it, so the map always agrees with the
#' legend. The scale draws no legend of its own -- use [legend_bivariate()].
#'
#' @inheritParams bivariate_palette
#' @param na.value Colour for areas missing either variable.
#' @param guide Passed to the scale; `"none"` because a bivariate legend is
#'   drawn separately with [legend_bivariate()].
#' @param aesthetics Aesthetic(s) the scale applies to.
#' @param ... Passed to [ggplot2::scale_fill_manual()].
#' @return A ggplot2 scale.
#' @examples
#' library(ggplot2)
#' set.seed(2)
#' df <- expand.grid(x = 1:20, y = 1:20)
#' df$a <- df$x + rnorm(400, sd = 4)
#' df$b <- df$y + rnorm(400, sd = 4)
#' df$cls <- classify_bivariate(df$a, df$b, n = 3)
#'
#' map <- ggplot(df, aes(x, y, fill = cls)) +
#'   geom_tile() +
#'   scale_fill_bivariate("blue-red", n = 3)
#'
#' leg <- legend_bivariate("blue-red", n = 3, xlab = "a", ylab = "b",
#'                         classes = df$cls)
#' if (requireNamespace("patchwork", quietly = TRUE)) {
#'   inset_legend(map, leg, position = "topleft")
#' }
#' @export
scale_fill_bivariate <- function(palette = "blue-red", n = 3,
                                 reverse_x = FALSE, reverse_y = FALSE,
                                 na.value = "grey85", guide = "none",
                                 aesthetics = "fill", ...) {
  pal <- bivariate_palette(palette, n, reverse_x, reverse_y)
  vals <- unclass(pal)
  attr(vals, "n") <- NULL
  ggplot2::scale_fill_manual(
    values = vals, limits = names(vals), drop = FALSE,
    na.value = na.value, guide = guide, aesthetics = aesthetics, ...
  )
}

#' @rdname scale_fill_bivariate
#' @export
scale_colour_bivariate <- function(palette = "blue-red", n = 3,
                                   reverse_x = FALSE, reverse_y = FALSE,
                                   na.value = "grey85", guide = "none",
                                   aesthetics = "colour", ...) {
  scale_fill_bivariate(palette, n, reverse_x, reverse_y, na.value, guide,
                       aesthetics = aesthetics, ...)
}

#' @rdname scale_fill_bivariate
#' @export
scale_color_bivariate <- scale_colour_bivariate

# ---------------------------------------------------------------------------
# Legend
# ---------------------------------------------------------------------------

#' Draw a bivariate legend
#'
#' Returns the `n x n` colour grid as a small ggplot. Two layouts: a square
#' with an arrowed axis on each side, or a diamond rotated 45 degrees --
#' the layout newspapers use -- with the values written along the two lower
#' edges, or a short phrase at each corner. Place it on the map with
#' [inset_legend()], or beside it with patchwork or cowplot.
#'
#' @inheritParams bivariate_palette
#' @param xlab,ylab Axis titles.
#' @param style `"square"` or `"diamond"`.
#' @param classes The factor returned by [classify_bivariate()]. If given, its
#'   break points are used to label the grid edges and its `n` is used.
#' @param breaks_x,breaks_y Break points to label the edges with, if
#'   `classes` is not given. `NULL` labels nothing.
#' @param corner_labels Four short phrases for the corners, in the order
#'   low-low, x-high, y-high, high-high, e.g. `c("Low income, few
#'   graduates", "High income, few graduates", "Low income, many graduates",
#'   "High income, many graduates")`. When given, break values and axis
#'   titles are omitted: the corners say it all. `"\n"` breaks a line.
#' @param labels A [label_map()] labeller (or any function) used to format the
#'   break values. `NULL` uses [label_map()] with sensible digits.
#' @param ends Label the outermost break of each axis (the data minimum and
#'   maximum when breaks come from `classes`). `FALSE` labels only the
#'   interior cut points, which is cleaner and what most published maps do.
#' @param arrows Draw an arrowhead on each axis line (square style only).
#' @param text_size Font size in points.
#' @param text_colour Colour of all text and axis lines.
#' @param border Colour of the grid-cell borders, or `NA` for none.
#' @param background Fill behind the legend, e.g. `"white"` or
#'   `scales::alpha("white", 0.8)` when it sits on top of a map. `NA` for
#'   transparent.
#' @param counts Write the number of observations in each cell, from
#'   `classes`. Empty cells are the thing to look for: a legend colour that
#'   appears nowhere on the map.
#' @return A ggplot object.
#' @examples
#' legend_bivariate("purple-green", n = 3,
#'                  xlab = "Median income", ylab = "% with a degree")
#'
#' # from classified data, interior cut points only
#' cls <- classify_bivariate(rlnorm(200, 10), runif(200), n = 3)
#' legend_bivariate("teal-pink", classes = cls, style = "diamond",
#'                  xlab = "Income", ylab = "Share", ends = FALSE,
#'                  labels = label_map(digits = 1))
#'
#' legend_bivariate("blue-red", n = 4, style = "diamond",
#'                  xlab = "% Black", ylab = "% Hispanic",
#'                  breaks_x = c(0, 5, 20, 50, 95),
#'                  breaks_y = c(0, 5, 20, 50, 95),
#'                  labels = label_map(suffix = "%"))
#'
#' legend_bivariate("blue-yellow", n = 3, style = "diamond",
#'                  corner_labels = c("Low diversity\nLittle change",
#'                                    "High diversity\nLittle change",
#'                                    "Low diversity\nBig increase",
#'                                    "High diversity\nBig increase"))
#' @export
legend_bivariate <- function(palette = "blue-red", n = 3,
                             xlab = "x", ylab = "y",
                             style = c("square", "diamond"),
                             classes = NULL,
                             breaks_x = NULL, breaks_y = NULL,
                             corner_labels = NULL,
                             labels = NULL, ends = TRUE, arrows = TRUE,
                             text_size = 9, text_colour = "grey30",
                             border = "white", background = NA,
                             reverse_x = FALSE, reverse_y = FALSE,
                             counts = FALSE) {
  style <- match.arg(style)
  cell_counts <- NULL
  if (!is.null(classes)) {
    n <- attr(classes, "n") %||% n
    breaks_x <- breaks_x %||% attr(classes, "breaks_x")
    breaks_y <- breaks_y %||% attr(classes, "breaks_y")
    if (isTRUE(counts)) {
      cell_counts <- table(factor(classes, levels = bivariate_levels(n)))
    }
  } else if (isTRUE(counts)) {
    abort_ggcarto("`counts = TRUE` needs `classes`.")
  }
  if (!is.null(corner_labels)) {
    if (length(corner_labels) != 4L) {
      abort_ggcarto("`corner_labels` needs four strings: low-low, x-high, ",
                    "y-high, high-high.")
    }
    breaks_x <- NULL; breaks_y <- NULL
  }
  pal <- bivariate_palette(palette, n, reverse_x, reverse_y)
  n <- attr(pal, "n")
  sz <- text_size / ggplot2::.pt          # points -> mm, for annotate()

  # Format labels up front; a blank string hides an end without moving the
  # others, so positions stay aligned with the cell edges.
  fmt0 <- if (is.null(labels)) label_map() else labels
  fmt <- function(b) {
    out <- fmt0(b)
    if (!isTRUE(ends) && length(out) > 2L) out[c(1L, length(out))] <- ""
    out
  }

  if (style == "diamond") {
    return(legend_diamond(pal, n, xlab, ylab, breaks_x, breaks_y,
                          corner_labels, fmt, sz, text_colour, border,
                          background, ends, cell_counts))
  }

  cells <- expand.grid(x = seq_len(n), y = seq_len(n))
  cells$cls <- paste(cells$x, cells$y, sep = "-")
  cells$fill <- unclass(pal)[cells$cls]
  if (!is.null(cell_counts)) {
    cells$count <- as.integer(cell_counts[cells$cls])
    cells$text <- ifelse(colour_lab(strip_alpha(cells$fill))[, "L"] > 60, "grey15", "white")
  }
  has_labs <- !is.null(breaks_x) || !is.null(breaks_y)

  # Everything -- tick labels, arrows, titles -- is drawn inside the panel in
  # cell units, so the legend is self-contained and survives being inset at
  # any size. Distances scale with n so a 5 x 5 grid gets the same margins as
  # a 3 x 3 one.
  gap   <- 0.04 * n                        # grid edge -> first element
  lab_h <- if (has_labs) 0.10 * n else 0   # room for tick labels
  y_arrow <- 0.5 - gap - lab_h
  y_title <- y_arrow - 0.09 * n
  lo <- y_title - 0.06 * n                 # panel edge, low side
  hi <- n + 0.5 + (if (has_labs) 0.16 * n else 0.02 * n)  # room for end labels
  edge <- seq(0.5, n + 0.5, by = 1)
  arrow_style <- if (isTRUE(arrows)) {
    grid::arrow(length = grid::unit(1.5, "mm"), type = "closed")
  } else NULL

  p <- ggplot2::ggplot(cells, ggplot2::aes(x = .data$x, y = .data$y)) +
    ggplot2::geom_tile(ggplot2::aes(fill = .data$fill), colour = border,
                       linewidth = 0.3) +
    ggplot2::scale_fill_identity() +
    ggplot2::annotate("segment", x = 0.5, xend = n + 0.5, y = y_arrow,
                      yend = y_arrow, linewidth = 0.3, arrow = arrow_style,
                      colour = text_colour, linejoin = "mitre") +
    ggplot2::annotate("segment", x = y_arrow, xend = y_arrow, y = 0.5,
                      yend = n + 0.5, linewidth = 0.3, arrow = arrow_style,
                      colour = text_colour, linejoin = "mitre") +
    ggplot2::annotate("text", x = n / 2 + 0.5, y = y_title, label = xlab,
                      size = sz, vjust = 1, colour = text_colour) +
    ggplot2::annotate("text", x = y_title, y = n / 2 + 0.5, label = ylab,
                      size = sz, vjust = 0, angle = 90, colour = text_colour) +
    ggplot2::coord_fixed(xlim = c(lo, hi), ylim = c(lo, hi),
                         expand = FALSE, clip = "off") +
    ggplot2::theme_void() +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = background, colour = NA),
      plot.margin = ggplot2::margin(4, 4, 4, 4)
    )

  if (!is.null(breaks_x)) {
    p <- p + ggplot2::annotate(
      "text", x = edge, y = 0.5 - gap, label = fmt(breaks_x),
      size = sz * 0.8, vjust = 1, colour = text_colour
    )
  }
  if (!is.null(breaks_y)) {
    p <- p + ggplot2::annotate(
      "text", x = 0.5 - gap, y = edge, label = fmt(breaks_y),
      size = sz * 0.8, hjust = 1, colour = text_colour
    )
  }
  if (!is.null(cell_counts)) {
    p <- p + ggplot2::geom_text(
      ggplot2::aes(label = .data$count, colour = .data$text), size = sz * 0.8
    ) + ggplot2::scale_colour_identity()
  }
  p
}

# The diamond: the square rotated 45 degrees so both-low sits at the bottom,
# x-high at the upper left, y-high at the upper right, both-high at the top.
# Cells are polygons in rotated coordinates; all text sits along the two lower
# edges or at the corners.
legend_diamond <- function(pal, n, xlab, ylab, breaks_x, breaks_y,
                           corner_labels, fmt, sz, text_colour, border,
                           background, ends, cell_counts = NULL) {
  rot <- function(u, v) {
    data.frame(X = (v - u) / sqrt(2), Y = (u + v) / sqrt(2))
  }
  # one polygon per cell
  cells <- do.call(rbind, lapply(seq_len(n), function(i) {
    do.call(rbind, lapply(seq_len(n), function(j) {
      corners <- rot(c(i - 1, i, i, i - 1), c(j - 1, j - 1, j, j))
      corners$cls <- paste(i, j, sep = "-")
      corners
    }))
  }))
  cells$fill <- unclass(pal)[cells$cls]

  has_corners <- !is.null(corner_labels)
  has_ticks <- !has_corners && (!is.null(breaks_x) || !is.null(breaks_y))
  half <- n / sqrt(2)                      # half-width of the diamond
  pad_tick <- 0.08 * n                     # edge -> tick label
  pad_title <- if (has_ticks) 0.19 * n else 0.08 * n
  pad_corner <- 0.06 * n

  p <- ggplot2::ggplot(cells) +
    ggplot2::geom_polygon(
      ggplot2::aes(x = .data$X, y = .data$Y, group = .data$cls,
                   fill = .data$fill),
      colour = border, linewidth = 0.3
    ) +
    ggplot2::scale_fill_identity() +
    ggplot2::theme_void()
  if (!is.null(cell_counts)) {
    centres <- expand.grid(i = seq_len(n), j = seq_len(n))
    ctr <- rot(centres$i - 0.5, centres$j - 0.5)
    ctr$cls <- paste(centres$i, centres$j, sep = "-")
    ctr$count <- as.integer(cell_counts[ctr$cls])
    ctr$fill <- unclass(pal)[ctr$cls]
    ctr$text <- ifelse(colour_lab(strip_alpha(ctr$fill))[, "L"] > 60, "grey15", "white")
    p <- p + ggplot2::geom_text(
      data = ctr, ggplot2::aes(x = .data$X, y = .data$Y, label = .data$count,
                               colour = .data$text), size = sz * 0.8
    ) + ggplot2::scale_colour_identity()
  }
  p <- p +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = background, colour = NA),
      plot.margin = ggplot2::margin(4, 4, 4, 4)
    )

  if (has_corners) {
    cp <- rot(c(0, n, 0, n), c(0, 0, n, n))
    off <- pad_corner
    p <- p +
      ggplot2::annotate("text", x = cp$X[1], y = cp$Y[1] - off,
                        label = corner_labels[1], size = sz, vjust = 1,
                        colour = text_colour, lineheight = 0.9) +
      ggplot2::annotate("text", x = cp$X[2] - off, y = cp$Y[2],
                        label = corner_labels[2], size = sz, hjust = 1,
                        colour = text_colour, lineheight = 0.9) +
      ggplot2::annotate("text", x = cp$X[3] + off, y = cp$Y[3],
                        label = corner_labels[3], size = sz, hjust = 0,
                        colour = text_colour, lineheight = 0.9) +
      ggplot2::annotate("text", x = cp$X[4], y = cp$Y[4] + off,
                        label = corner_labels[4], size = sz, vjust = 0,
                        colour = text_colour, lineheight = 0.9)
    xlim <- c(-half - 0.9 * n, half + 0.9 * n)
    ylim <- c(-0.35 * n, 2 * half + 0.35 * n)
  } else {
    # titles along the two lower edges, outside the tick labels
    tx <- rot(n / 2, -pad_title); ty <- rot(-pad_title, n / 2)
    p <- p +
      ggplot2::annotate("text", x = tx$X, y = tx$Y, label = xlab, size = sz,
                        angle = -45, colour = text_colour) +
      ggplot2::annotate("text", x = ty$X, y = ty$Y, label = ylab, size = sz,
                        angle = 45, colour = text_colour)
    # Both axes meet at the bottom corner; when they start at the same value
    # write it once, straight, under the corner, instead of twice on top of
    # itself.
    shared <- isTRUE(ends) && !is.null(breaks_x) && !is.null(breaks_y) &&
      isTRUE(all.equal(breaks_x[1], breaks_y[1]))
    from <- if (shared) 1 else 0
    drop_first <- function(v) if (shared) v[-1] else v
    if (shared) {
      p <- p + ggplot2::annotate("text", x = 0, y = -pad_tick * 0.9,
                                 label = fmt(breaks_x)[1], size = sz * 0.8,
                                 vjust = 1, colour = text_colour)
    }
    if (!is.null(breaks_x)) {
      kx <- rot(seq(from, n, by = 1), -pad_tick)
      p <- p + ggplot2::annotate("text", x = kx$X, y = kx$Y,
                                 label = drop_first(fmt(breaks_x)),
                                 size = sz * 0.8, angle = -45,
                                 colour = text_colour)
    }
    if (!is.null(breaks_y)) {
      ky <- rot(-pad_tick, seq(from, n, by = 1))
      p <- p + ggplot2::annotate("text", x = ky$X, y = ky$Y,
                                 label = drop_first(fmt(breaks_y)),
                                 size = sz * 0.8, angle = 45,
                                 colour = text_colour)
    }
    m <- if (has_ticks) 0.36 * n else 0.22 * n
    xlim <- c(-half - m, half + m)
    ylim <- c(-m, 2 * half + 0.05 * n)
  }
  p + ggplot2::coord_fixed(xlim = xlim, ylim = ylim, expand = FALSE, clip = "off")
}

#' Place a legend on a map
#'
#' Overlays one ggplot (typically [legend_bivariate()]) on another using
#' patchwork's [patchwork::inset_element()]. Positions are given as a corner
#' name plus a size, or as explicit `left`/`bottom`/`right`/`top` fractions of
#' the map panel.
#'
#' @param map The main ggplot.
#' @param legend The ggplot to overlay.
#' @param position One of `"bottomright"`, `"bottomleft"`, `"topright"`,
#'   `"topleft"`.
#' @param width,height Size of the inset as a fraction of the map.
#' @param left,bottom,right,top Explicit placement, overriding `position`.
#' @param align_to Passed to [patchwork::inset_element()]: `"panel"` (default),
#'   `"plot"` or `"full"`.
#' @return A patchwork object, which prints like a ggplot.
#' @examples
#' library(ggplot2)
#' map <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
#' leg <- legend_bivariate("teal-pink", 3, "weight", "mpg")
#' if (requireNamespace("patchwork", quietly = TRUE)) {
#'   inset_legend(map, leg, position = "topright", width = 0.3)
#' }
#' @export
inset_legend <- function(map, legend,
                         position = c("bottomright", "bottomleft",
                                      "topright", "topleft"),
                         width = 0.25, height = width,
                         left = NULL, bottom = NULL, right = NULL, top = NULL,
                         align_to = "panel") {
  need_pkg("patchwork", "to inset a legend on a map")
  position <- match.arg(position)
  pad <- 0.02
  if (is.null(left) && is.null(right)) {
    if (grepl("right", position)) { right <- 1 - pad; left <- right - width }
    else { left <- pad; right <- left + width }
  }
  if (is.null(bottom) && is.null(top)) {
    if (grepl("top", position)) { top <- 1 - pad; bottom <- top - height }
    else { bottom <- pad; top <- bottom + height }
  }
  left <- left %||% (right - width); right <- right %||% (left + width)
  bottom <- bottom %||% (top - height); top <- top %||% (bottom + height)

  map + patchwork::inset_element(
    legend, left = left, bottom = bottom, right = right, top = top,
    align_to = align_to, ignore_tag = TRUE
  )
}
