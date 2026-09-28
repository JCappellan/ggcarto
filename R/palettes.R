# ===========================================================================
# Palette resolution
#
# One entry point, many sources. A palette may be given as:
#   * a character vector of colours          c("#440154", "#21918c", "#fde725")
#   * a function(n) returning n colours      viridisLite::viridis
#   * "source::name"                         "scico::roma", "brewer::RdBu"
#   * a bare name                            "viridis", "Blues 3", "RdBu"
#
# Built-in sources need nothing beyond base R and the packages scales already
# depends on. cols4all and paletteer are optional and unlock ~700 and ~2500
# further palettes respectively.
# ===========================================================================

#' Palette sources known to ggcarto
#'
#' @return A data frame of source prefixes, the package that provides them and
#'   whether that package is currently installed.
#' @examples
#' palette_sources()
#' @export
palette_sources <- function() {
  src <- rbind(
    c("hcl", "grDevices", "grDevices"),
    c("brewer", "RColorBrewer", "RColorBrewer"),
    c("viridis", "viridisLite", "viridisLite"),
    c("c4a", "cols4all", "cols4all"),
    c("base", "grDevices", "grDevices")
  )
  out <- data.frame(
    prefix = src[, 1], source = src[, 2], package = src[, 3],
    stringsAsFactors = FALSE
  )
  out$installed <- vapply(
    out$package, function(p) requireNamespace(p, quietly = TRUE), logical(1)
  )
  # Any other prefix is treated as a paletteer "package::palette" reference.
  rbind(out, data.frame(
    prefix = "<other>", source = "paletteer", package = "paletteer",
    installed = requireNamespace("paletteer", quietly = TRUE),
    stringsAsFactors = FALSE
  ))
}

#' List available palettes
#'
#' @param type One or more of `"seq"`, `"div"`, `"cat"` (sequential, diverging,
#'   categorical), or `"all"`.
#' @param source Restrict to one source prefix, e.g. `"hcl"` or `"brewer"`.
#' @param pattern Optional regular expression to filter names.
#' @return A data frame with columns `name`, `source` and `type`.
#' @examples
#' head(map_palettes("div"))
#' map_palettes(source = "brewer", pattern = "Rd")
#' @export
map_palettes <- function(type = "all", source = NULL, pattern = NULL) {
  out <- palette_index()
  if (!identical(type, "all")) {
    type <- match.arg(type, c("seq", "div", "cat"), several.ok = TRUE)
    out <- out[out$type %in% type, , drop = FALSE]
  }
  if (!is.null(source)) out <- out[out$source %in% source, , drop = FALSE]
  if (!is.null(pattern)) {
    out <- out[grepl(pattern, out$name, ignore.case = TRUE), , drop = FALSE]
  }
  rownames(out) <- NULL
  out
}

palette_index <- function() {
  rows <- list()

  # grDevices::hcl.colors -- ~110 palettes, zero dependencies.
  hcl_names <- grDevices::hcl.pals()
  hcl_type <- rep("seq", length(hcl_names))
  hcl_type[hcl_names %in% grDevices::hcl.pals("diverging")] <- "div"
  hcl_type[hcl_names %in% grDevices::hcl.pals("divergingx")] <- "div"
  hcl_type[hcl_names %in% grDevices::hcl.pals("qualitative")] <- "cat"
  rows[[1]] <- data.frame(
    name = paste0("hcl::", hcl_names), source = "hcl", type = hcl_type,
    stringsAsFactors = FALSE
  )

  if (requireNamespace("RColorBrewer", quietly = TRUE)) {
    bi <- RColorBrewer::brewer.pal.info
    rows[[2]] <- data.frame(
      name = paste0("brewer::", rownames(bi)),
      source = "brewer",
      type = c(div = "div", qual = "cat", seq = "seq")[as.character(bi$category)],
      stringsAsFactors = FALSE
    )
  }

  vir <- c("viridis", "magma", "plasma", "inferno", "cividis", "mako",
           "rocket", "turbo")
  rows[[3]] <- data.frame(
    name = paste0("viridis::", vir), source = "viridis",
    type = "seq", stringsAsFactors = FALSE
  )

  if (requireNamespace("cols4all", quietly = TRUE)) {
    ok <- try({
      p <- cols4all::c4a_palettes()
      data.frame(name = paste0("c4a::", p), source = "c4a",
                 type = NA_character_, stringsAsFactors = FALSE)
    }, silent = TRUE)
    if (!inherits(ok, "try-error")) rows[[4]] <- ok
  }

  do.call(rbind, rows)
}

#' Resolve a palette specification to a colour-generating function
#'
#' @param palette A palette specification: a colour vector, a function of `n`,
#'   or a name (optionally `"source::name"`).
#' @param reverse Reverse the palette order.
#' @param range Length-2 numeric in `[0, 1]`; trim the palette to a sub-range,
#'   e.g. `c(0.15, 0.85)` to drop the near-white and near-black ends of a
#'   sequential ramp.
#' @return A function of `n` returning `n` colours.
#' @examples
#' pal <- as_palette("brewer::RdBu", reverse = TRUE)
#' pal(5)
#' as_palette("hcl::Zissou 1", range = c(0.1, 0.9))(3)
#' @export
as_palette <- function(palette, reverse = FALSE, range = c(0, 1)) {
  base_fn <- palette_fun(palette)
  force(reverse); force(range)

  if (!is.numeric(range) || length(range) != 2 ||
      any(range < 0 | range > 1) || range[1] >= range[2]) {
    abort_ggcarto("`range` must be two increasing numbers within [0, 1].")
  }

  function(n) {
    n <- max(as.integer(n), 1L)
    if (identical(range, c(0, 1))) {
      cols <- base_fn(n)
    } else {
      # Over-sample then interpolate, so trimming is smooth rather than
      # dropping whole swatches.
      dense <- base_fn(256L)
      idx <- seq(range[1], range[2], length.out = n)
      cols <- scales::gradient_n_pal(dense)(idx)
    }
    if (isTRUE(reverse)) cols <- rev(cols)
    cols
  }
}

palette_fun <- function(palette) {
  if (is.function(palette)) return(function(n) palette(n))

  if (is.character(palette) && length(palette) > 1L) {
    cols <- palette
    if (!all(is_colour(cols))) {
      abort_ggcarto("`palette` looks like a colour vector but contains invalid colours.")
    }
    return(function(n) {
      if (n == length(cols)) cols else scales::gradient_n_pal(cols)(seq(0, 1, length.out = n))
    })
  }

  if (!is.character(palette) || length(palette) != 1L) {
    abort_ggcarto("`palette` must be a colour vector, a function, or a palette name.")
  }

  # A single valid colour name/hex is a (degenerate) palette.
  if (is_colour(palette) && !grepl("::", palette, fixed = TRUE)) {
    cols <- palette
    return(function(n) rep(cols, length.out = n))
  }

  parts <- strsplit(palette, "::", fixed = TRUE)[[1]]
  if (length(parts) == 2L) {
    src <- parts[1]
    nm <- parts[2]
  } else {
    src <- guess_source(palette)
    nm <- palette
  }

  switch(src,
    hcl = function(n) grDevices::hcl.colors(n, palette = nm),
    base = function(n) grDevices::hcl.colors(n, palette = nm),
    brewer = function(n) {
      need_pkg("RColorBrewer", "for \"brewer::\" palettes")
      bi <- RColorBrewer::brewer.pal.info
      if (!nm %in% rownames(bi)) abort_ggcarto("Unknown brewer palette: ", nm)
      maxn <- bi[nm, "maxcolors"]
      cols <- RColorBrewer::brewer.pal(max(3L, min(n, maxn)), nm)
      if (n <= maxn && n >= 3L) cols
      else scales::gradient_n_pal(cols)(seq(0, 1, length.out = n))
    },
    viridis = function(n) {
      if (requireNamespace("viridisLite", quietly = TRUE)) {
        viridisLite::viridis(n, option = nm)
      } else {
        grDevices::hcl.colors(n, palette = viridis_to_hcl(nm))
      }
    },
    c4a = function(n) {
      need_pkg("cols4all", "for \"c4a::\" palettes")
      cols4all::c4a(nm, n = n)
    },
    paletteer = function(n) {
      need_pkg("paletteer", sprintf("for the palette \"%s\"", palette))
      as.character(paletteer::paletteer_c(palette, n = n))
    },
    abort_ggcarto("Unknown palette source: ", src)
  )
}

viridis_to_hcl <- function(nm) {
  switch(tolower(nm),
    viridis = "Viridis", magma = "Inferno", inferno = "Inferno",
    plasma = "Plasma", cividis = "Cividis", mako = "Mako",
    rocket = "Rocket", turbo = "Viridis", "Viridis"
  )
}

guess_source <- function(nm) {
  if (nm %in% c("viridis", "magma", "plasma", "inferno", "cividis",
                "mako", "rocket", "turbo")) return("viridis")
  if (requireNamespace("RColorBrewer", quietly = TRUE) &&
      nm %in% rownames(RColorBrewer::brewer.pal.info)) return("brewer")
  if (nm %in% grDevices::hcl.pals()) return("hcl")
  if (requireNamespace("cols4all", quietly = TRUE)) {
    ok <- try(nm %in% cols4all::c4a_palettes(), silent = TRUE)
    if (!inherits(ok, "try-error") && isTRUE(ok)) return("c4a")
  }
  abort_ggcarto(
    "Could not find a palette called \"", nm, "\".\n",
    "Use map_palettes() to search, or qualify the source, e.g. \"brewer::RdBu\"."
  )
}

is_colour <- function(x) {
  vapply(x, function(v) {
    if (is.na(v)) return(FALSE)
    !inherits(try(grDevices::col2rgb(v), silent = TRUE), "try-error")
  }, logical(1), USE.NAMES = FALSE)
}

#' Preview a palette
#'
#' @param palette A palette specification, as accepted by [as_palette()].
#' @param n Number of colours to draw.
#' @param ... Passed to [as_palette()].
#' @return Invisibly, the character vector of colours. Called for the plot.
#' @examples
#' \donttest{
#' show_palette("brewer::RdBu", 9)
#' }
#' @export
show_palette <- function(palette, n = 9, ...) {
  cols <- as_palette(palette, ...)(n)
  op <- graphics::par(mar = c(2, 1, 2, 1))
  on.exit(graphics::par(op), add = TRUE)
  graphics::image(
    seq_len(n), 1, as.matrix(seq_len(n)), col = cols,
    xlab = "", ylab = "", axes = FALSE,
    main = if (is.character(palette) && length(palette) == 1) palette else "palette"
  )
  invisible(cols)
}

# Default palettes by scale type, overridable via options().
default_palette <- function(type) {
  opt <- getOption("ggcarto.palettes", list())
  switch(type,
    div = opt$div %||% "brewer::RdBu",
    cat = opt$cat %||% "brewer::Set2",
    opt$seq %||% "viridis::viridis"
  )
}
