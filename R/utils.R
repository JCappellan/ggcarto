#' @keywords internal
`%||%` <- function(x, y) if (is.null(x)) y else x

is_waive <- function(x) inherits(x, "waiver")

# Does formal `arg` of function `f` exist and have no default?
arg_required <- function(f, arg) {
  fm <- formals(f)
  arg %in% names(fm) && identical(fm[[arg]], quote(expr = ))
}

has_arg <- function(f, arg) arg %in% names(formals(f))

need_pkg <- function(pkg, why) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(sprintf(
      "Package '%s' is required %s.\nInstall it with: install.packages(\"%s\")",
      pkg, why, pkg
    ), call. = FALSE)
  }
  invisible(TRUE)
}

abort_ggcarto <- function(...) stop(paste0(...), call. = FALSE)

finite_num <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  x[is.finite(x)]
}

# ---------------------------------------------------------------------------
# Reproducibility helpers
# ---------------------------------------------------------------------------

# Evaluate `code` with the RNG seeded, then put the caller's RNG state back
# exactly as it was, so a classification never disturbs a user's simulation.
# `seed = NULL` runs the code against the session RNG untouched.
with_seed <- function(seed, code) {
  if (is.null(seed)) return(code)
  genv <- globalenv()
  had <- exists(".Random.seed", envir = genv, inherits = FALSE)
  old <- if (had) get(".Random.seed", envir = genv, inherits = FALSE)
  on.exit({
    if (had) {
      assign(".Random.seed", old, envir = genv)
    } else if (exists(".Random.seed", envir = genv, inherits = FALSE)) {
      rm(".Random.seed", envir = genv)
    }
  }, add = TRUE)
  set.seed(seed)
  code
}

# Session-level memory, so a note about e.g. sampling is printed once, not on
# every redraw of a plot. options(ggcarto.quiet = TRUE) silences them.
ggcarto_env <- new.env(parent = emptyenv())

inform_once <- function(key, msg) {
  if (isTRUE(getOption("ggcarto.quiet", FALSE))) return(invisible(FALSE))
  if (isTRUE(ggcarto_env[[key]])) return(invisible(FALSE))
  ggcarto_env[[key]] <- TRUE
  message(msg)
  invisible(TRUE)
}

# ---------------------------------------------------------------------------
# Classes from breaks
# ---------------------------------------------------------------------------

# Integer class index for every value, 1 .. length(breaks) - 1. Values beyond
# the outer breaks are squished into the end classes, as the scales do;
# missing values stay missing. `closure` decides where a value sitting
# exactly on a break goes: "right" -> (a, b], "left" -> [a, b).
assign_classes <- function(x, breaks, closure = c("right", "left")) {
  closure <- match.arg(closure)
  x <- suppressWarnings(as.numeric(x))
  k <- length(breaks) - 1L
  if (k < 1L) return(rep(NA_integer_, length(x)))
  idx <- cut(x, breaks = breaks, labels = FALSE, include.lowest = TRUE,
             right = closure == "right")
  ok <- !is.na(x)
  idx[ok & x < breaks[1]] <- 1L
  idx[ok & x > breaks[k + 1L]] <- k
  as.integer(idx)
}

bullet <- function() if (isTRUE(l10n_info()[["UTF-8"]])) "\u2022" else "*"

fmt_n <- function(n) format(n, big.mark = ",", trim = TRUE, scientific = FALSE)

# Plain-language name for a style, for printed summaries.
style_label <- function(style) {
  lab <- c(
    equal = "Equal interval", pretty = "Pretty (equal interval)",
    quantile = "Quantile", sd = "Standard deviation",
    jenks = "Jenks natural breaks", fisher = "Fisher-Jenks",
    kmeans = "k-means", hclust = "Hierarchical clustering",
    bclust = "Bagged clustering", headtails = "Head/tails",
    box = "Boxplot", dpih = "Plug-in bandwidth (dpih)",
    maximum = "Maximum gap", log = "Log", geometric = "Geometric",
    unique = "Unique values", fixed = "Fixed breaks"
  )
  unname(lab[style]) %||% style
}

# Which recommendation logic a style follows in class_guide().
style_family <- function(style) {
  switch(style,
    fisher = , jenks = , kmeans = , hclust = , bclust = , maximum = "variance",
    quantile = "quantile",
    equal = , pretty = , log = , geometric = "range",
    headtails = , box = , dpih = "self",
    sd = "sd",
    fixed = "fixed",
    unique = "unique",
    "variance"
  )
}

# ---------------------------------------------------------------------------
# ggplot2 constructor shim.
#
# ggplot2 4.0 deprecated `scale_name` and renamed `trans` -> `transform`.
# ggplot2 3.x requires `scale_name` and only knows `trans`. This lets the same
# call site work on both, so the package can be developed and tested against
# whichever version happens to be installed.
# ---------------------------------------------------------------------------
call_scale <- function(fn, args) {
  fm <- names(formals(fn))

  if (arg_required(fn, "scale_name")) {
    args$scale_name <- args$scale_name %||% "ggcarto"
  } else {
    args$scale_name <- NULL
  }

  if (!"transform" %in% fm && "transform" %in% names(args)) {
    args$trans <- args$transform
    args$transform <- NULL
  }
  if (!"trans" %in% fm && "trans" %in% names(args)) {
    args$transform <- args$trans
    args$trans <- NULL
  }

  # Drop anything this ggplot2 version does not understand rather than erroring.
  known <- names(args) %in% fm
  if ("..." %in% fm) known <- known | TRUE
  do.call(fn, args[known])
}

# ---------------------------------------------------------------------------
# Out-of-bounds handling
# ---------------------------------------------------------------------------
resolve_oob <- function(oob) {
  if (is.function(oob)) return(oob)
  oob <- match.arg(as.character(oob), c("squish", "censor", "keep", "infinite"))
  switch(oob,
    squish   = scales::oob_squish,
    censor   = scales::oob_censor,
    keep     = scales::oob_keep,
    infinite = scales::oob_squish_infinite
  )
}

#' Data-driven limits that trim outliers
#'
#' Returns a function suitable for the `limits` argument of any ggcarto scale
#' (or any ggplot2 continuous scale). Combined with the default
#' `oob = "squish"`, this is the usual cartographic treatment of a long tail:
#' clamp the extremes into the top and bottom classes rather than letting a
#' single outlier flatten the whole ramp.
#'
#' @param lower,upper Quantile probabilities, between 0 and 1.
#' @param type Quantile algorithm passed to [stats::quantile()].
#' @return A function of one argument returning a length-2 numeric vector.
#' @examples
#' f <- trim_limits(0.05, 0.95)
#' f(c(1:100, 10000))
#' @export
trim_limits <- function(lower = 0.02, upper = 0.98, type = 7) {
  force(lower); force(upper); force(type)
  function(x) {
    x <- finite_num(x)
    if (!length(x)) return(c(NA_real_, NA_real_))
    as.numeric(stats::quantile(x, c(lower, upper), names = FALSE, type = type))
  }
}

#' Symmetric limits around a midpoint
#'
#' Makes a diverging scale honest: the colour distance above and below the
#' midpoint reflects the same data distance.
#'
#' @param mid The value to centre on.
#' @return A function of one argument returning a length-2 numeric vector.
#' @examples
#' symmetric_limits(0)(c(-2, 7))
#' @export
symmetric_limits <- function(mid = 0) {
  force(mid)
  function(x) {
    x <- finite_num(x)
    if (!length(x)) return(c(NA_real_, NA_real_))
    d <- max(abs(x - mid))
    c(mid - d, mid + d)
  }
}
