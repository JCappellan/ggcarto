# ===========================================================================
# Palette accessibility
#
# A small, cartographic reading of colour science. The measurements are
# standard -- CIELAB lightness and colour difference, WCAG contrast, CVD
# simulation from colorspace -- and they are interpreted for one purpose:
# will a reader of the map tell the classes apart, read the ramp in the right
# direction, and see the neutral colour as neutral? Nothing here tries to be
# a general palette package; cols4all and colorspace already are.
# ===========================================================================

#' Check a palette for map use
#'
#' Measures whether `n` colours from a palette will work as map classes:
#' does lightness run in one direction (sequential) or out from a neutral
#' centre (diverging); can adjacent classes be told apart, with normal vision
#' and under the common colour-vision deficiencies; does the palest class
#' vanish against the background or the missing-data colour. Findings are
#' reported as cartographic consequences, not just numbers.
#'
#' @param palette A palette specification accepted by [as_palette()], or a
#'   vector of colours.
#' @param n Number of classes to draw from it.
#' @param type What the palette is meant to be: `"seq"`, `"div"` or `"cat"`.
#'   `NULL` looks the name up in [map_palettes()] where possible and otherwise
#'   infers it from the lightness profile.
#' @param background Colour the map is drawn on, for contrast.
#' @param na.value The missing-data colour, if the map has one, so it can be
#'   compared with the classes.
#' @param ... Passed to [as_palette()] (`reverse`, `range`).
#' @details
#' Colour differences are CIE76 \eqn{\Delta E} in CIELAB, computed with
#' [grDevices::convertColor()]. Adjacent classes below 10 are flagged as
#' hard to tell apart and below 5 as effectively identical; those thresholds
#' are conservative for small map polygons, which are harder to compare than
#' legend swatches. Colour-vision deficiencies are simulated with the
#' colorspace package (deuteranopia, protanopia, tritanopia) when it is
#' installed. Contrast against the background is the WCAG luminance ratio; a
#' class below 1.2 is close to invisible on that background.
#' @return An object of class `ggcarto_palette_check`: a list with `colours`,
#'   `type` (declared or looked up), `type_inferred`, `lightness`, `chroma`,
#'   `delta_e` (between adjacent classes), `cvd` (a data frame with the
#'   smallest adjacent difference under each deficiency, or `NULL` if
#'   colorspace is unavailable), `contrast` (against `background`),
#'   `na_delta_e`, `issues` (character; empty when nothing was found) and
#'   `ok`.
#' @examples
#' check_palette("brewer::YlGnBu", 5)
#' check_palette("brewer::RdBu", 7, type = "div")
#' check_palette("brewer::Set2", 5, type = "seq")   # the wrong kind
#' check_palette("viridis::mako", 9, na.value = "grey85")
#' @seealso [plot_palette()] to see the same colours under each deficiency,
#'   [diagnose_map()] which runs this on a finished map.
#' @export
check_palette <- function(palette, n = 5, type = NULL, background = "white",
                          na.value = NULL, ...) {
  cols <- as_palette(palette, ...)(n)
  cols_opaque <- strip_alpha(cols)
  declared <- if (!is.null(type)) match.arg(type, c("seq", "div", "cat"))
              else palette_type_lookup(palette)
  lab <- colour_lab(cols_opaque)
  L <- lab[, "L"]; chroma <- sqrt(lab[, "a"]^2 + lab[, "b"]^2)
  inferred <- infer_palette_type(L)
  type_used <- declared %||% inferred
  de <- adjacent_delta_e(cols_opaque)
  contrast <- vapply(cols_opaque, function(cc) contrast_ratio(cc, background), numeric(1))
  na_de <- if (!is.null(na.value)) delta_e(strip_alpha(na.value), cols_opaque) else NULL
  cvd <- cvd_table(cols_opaque)

  issues <- character(0)
  th <- palette_thresholds()

  # Kind
  if (!is.null(declared) && !is.na(inferred) && declared != inferred) {
    issues <- c(issues, switch(paste(declared, inferred),
      "seq cat" = , "seq div" = "meant to be sequential, but lightness does not run in one direction: readers cannot rank the classes by darkness",
      "div seq" = "meant to be diverging, but lightness runs in one direction: there is no visible neutral centre, so the map reads as low-to-high",
      "div cat" = "meant to be diverging, but lightness is not lowest at both ends: readers will not see a centre",
      "cat seq" = , "cat div" = "meant to be categorical, but lightness is ordered: readers will infer a ranking that does not exist"
    ))
  }
  # Sequential monotonicity
  if (identical(type_used, "seq") && n > 2) {
    d <- diff(L)
    if (!(all(d <= 0.5) || all(d >= -0.5))) {
      issues <- c(issues, sprintf("lightness is not monotone (steps: %s): at least one class breaks the light-to-dark order",
                                  paste(round(d), collapse = ", ")))
    }
  }
  # Diverging centre
  if (identical(type_used, "div") && n >= 3) {
    mid <- (n + 1) / 2
    mid_i <- if (n %% 2 == 1) mid else c(floor(mid), ceiling(mid))
    if (max(chroma[mid_i]) > th$neutral_chroma) {
      issues <- c(issues, sprintf("the centre colour is not neutral (chroma %.0f): readers may not read the middle class as 'around the midpoint'",
                                  max(chroma[mid_i])))
    }
    if (n %% 2 == 0) {
      issues <- c(issues, "an even number of classes puts no class on the midpoint; use an odd n or make sure a break sits exactly on it")
    }
  }
  # Adjacent distinguishability
  if (length(de)) {
    worst <- which.min(de)
    if (min(de) < th$de_identical) {
      issues <- c(issues, sprintf("classes %d and %d are effectively the same colour (dE %.1f): the map shows one class fewer than the legend claims",
                                  worst, worst + 1L, min(de)))
    } else if (min(de) < th$de_hard) {
      issues <- c(issues, sprintf("classes %d and %d are hard to tell apart (dE %.1f), especially on small polygons",
                                  worst, worst + 1L, min(de)))
    }
  }
  # CVD
  if (!is.null(cvd)) {
    bad <- cvd[cvd$min_delta_e < th$de_hard, , drop = FALSE]
    if (nrow(bad)) {
      who <- if (any(bad$deficiency %in% c("deutan", "protan"))) "about 1 in 12 men" else "a small share of readers"
      issues <- c(issues, sprintf("under %s adjacent classes become hard to tell apart; %s would lose that distinction on the map",
                                  paste(sprintf("%s (classes %s, dE %.1f)", bad$deficiency, bad$worst_pair, bad$min_delta_e),
                                        collapse = ", "), who))
    }
    # Non-adjacent confusions matter too: on a map the classes are not lined up.
    far <- cvd[cvd$min_any < th$de_hard & cvd$closest != cvd$worst_pair, , drop = FALSE]
    if (nrow(far)) {
      issues <- c(issues, sprintf("under %s non-adjacent classes look alike; on the map, where classes are not lined up, those areas would be misread",
                                  paste(sprintf("%s (classes %s, dE %.1f)", far$deficiency, far$closest, far$min_any),
                                        collapse = ", ")))
    }
  }
  # Background. The neutral centre of a diverging palette is pale on purpose.
  check_idx <- seq_len(n)
  if (identical(type_used, "div") && n >= 3) {
    mid <- (n + 1) / 2
    check_idx <- setdiff(check_idx, c(floor(mid), ceiling(mid)))
  }
  faint <- check_idx[contrast[check_idx] < th$contrast_min]
  if (length(faint)) {
    issues <- c(issues, sprintf("class %s has almost no contrast against %s (ratio %.2f): areas in it will look like background or like no data; trim the pale end with range = c(0.15, 1) or draw polygon outlines",
                                paste(faint, collapse = ", "), background, min(contrast[faint])))
  }
  # NA colour
  if (!is.null(na_de) && min(na_de) < th$de_hard) {
    issues <- c(issues, sprintf("the missing-data colour is close to class %d (dE %.1f): 'no data' will be read as a value",
                                which.min(na_de), min(na_de)))
  }

  structure(list(
    palette = if (is.character(palette) && length(palette) == 1L) palette else "<colours>",
    n = n, colours = cols, type = declared, type_inferred = inferred,
    lightness = unname(L), chroma = unname(chroma), delta_e = de,
    cvd = cvd, contrast = unname(contrast), background = background,
    na.value = na.value, na_delta_e = na_de,
    issues = issues, ok = !length(issues)
  ), class = "ggcarto_palette_check")
}

palette_thresholds <- function() {
  list(de_identical = 5, de_hard = 10, neutral_chroma = 15, contrast_min = 1.2)
}

#' @export
print.ggcarto_palette_check <- function(x, ...) {
  cat("ggcarto palette check: ", x$palette, ", ", x$n, " classes\n", sep = "")
  cat("Colours: ", paste(x$colours, collapse = " "), "\n", sep = "")
  kind <- function(t) c(seq = "sequential", div = "diverging", cat = "categorical")[t]
  cat("Type: ", if (!is.null(x$type)) paste0(kind(x$type), " (declared)") else "not declared",
      "; reads as ", if (is.na(x$type_inferred)) "unclear" else kind(x$type_inferred), "\n", sep = "")
  cat("Lightness: ", paste(round(x$lightness), collapse = " > "), "\n", sep = "")
  if (length(x$delta_e)) {
    cat(sprintf("Adjacent classes: smallest dE %.1f (classes %d-%d)\n",
                min(x$delta_e), which.min(x$delta_e), which.min(x$delta_e) + 1L))
  }
  if (!is.null(x$cvd)) {
    cat("Colour vision: ", paste(sprintf("%s dE %.1f", x$cvd$deficiency, x$cvd$min_delta_e), collapse = ", "), "\n", sep = "")
  } else {
    cat("Colour vision: not simulated (install colorspace)\n")
  }
  cat(sprintf("Against %s: lowest contrast %.2f (class %d)\n", x$background,
              min(x$contrast), which.min(x$contrast)))
  if (!is.null(x$na_delta_e)) {
    cat(sprintf("Missing-data colour %s: closest class dE %.1f\n", x$na.value, min(x$na_delta_e)))
  }
  if (x$ok) {
    cat("\nNo issues found.\n")
  } else {
    cat("\nIssues:\n")
    cat(paste0("  ", bullet(), " ", x$issues), sep = "\n")
  }
  invisible(x)
}

#' Show a palette as map classes, with colour-vision simulation
#'
#' Draws `n` swatches from a palette in a row, and the same row as seen with
#' deuteranopia, protanopia and tritanopia (via colorspace) and in greyscale,
#' so you can see which class boundaries survive.
#'
#' @inheritParams check_palette
#' @param cvd Which simulations to draw. `NULL` draws all that are available;
#'   `"none"` draws only the palette. Any of `"deutan"`, `"protan"`,
#'   `"tritan"`, `"grey"`.
#' @param labels Print the hex code in each swatch.
#' @return A ggplot object.
#' @examples
#' plot_palette("brewer::YlGnBu", 5)
#' plot_palette("brewer::Spectral", 7, cvd = c("deutan", "grey"))
#' @export
plot_palette <- function(palette, n = 5, cvd = NULL, labels = TRUE, ...) {
  cols <- as_palette(palette, ...)(n)
  base <- strip_alpha(cols)
  rows <- list(normal = cols)
  wanted <- if (is.null(cvd)) c("deutan", "protan", "tritan", "grey")
            else if (identical(cvd, "none")) character(0)
            else match.arg(cvd, c("deutan", "protan", "tritan", "grey"), several.ok = TRUE)
  if (length(wanted)) {
    if (any(wanted != "grey")) need_pkg("colorspace", "to simulate colour-vision deficiencies")
    for (w in wanted) {
      rows[[w]] <- switch(w,
        deutan = colorspace::deutan(base),
        protan = colorspace::protan(base),
        tritan = colorspace::tritan(base),
        grey   = grey_of(base)
      )
    }
  }
  df <- do.call(rbind, lapply(names(rows), function(nm) {
    data.frame(row = nm, class = seq_len(n), fill = rows[[nm]],
               stringsAsFactors = FALSE)
  }))
  df$row <- factor(df$row, levels = rev(names(rows)))
  df$text <- ifelse(colour_lab(strip_alpha(df$fill))[, "L"] > 60, "grey15", "white")
  title <- if (is.character(palette) && length(palette) == 1L) palette else "palette"

  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$class, y = .data$row)) +
    ggplot2::geom_tile(ggplot2::aes(fill = .data$fill), colour = ggcarto_colours()[["paper"]],
                       linewidth = 1.2) +
    ggplot2::scale_fill_identity() +
    ggplot2::scale_x_continuous(breaks = seq_len(n), expand = c(0, 0)) +
    ggplot2::labs(x = "class", y = NULL, title = title) +
    ggplot2::coord_fixed() +
    theme_ggcarto(axes = TRUE) +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank(),
                   plot.title = ggplot2::element_text(size = ggplot2::rel(1.1)),
                   axis.text.y = ggplot2::element_text(colour = ggcarto_colours()[["ink"]]))
  if (isTRUE(labels)) {
    p <- p + ggplot2::geom_text(ggplot2::aes(label = toupper(substr(.data$fill, 1, 7)),
                                             colour = .data$text), size = 2.6) +
      ggplot2::scale_colour_identity()
  }
  p
}

# ---------------------------------------------------------------------------
# Colour arithmetic (base R only)
# ---------------------------------------------------------------------------

strip_alpha <- function(cols) {
  m <- grDevices::col2rgb(cols)
  grDevices::rgb(m[1, ], m[2, ], m[3, ], maxColorValue = 255)
}

colour_lab <- function(cols) {
  m <- t(grDevices::col2rgb(cols)) / 255
  out <- grDevices::convertColor(m, from = "sRGB", to = "Lab")
  colnames(out) <- c("L", "a", "b")
  out
}

delta_e <- function(a, b) {
  la <- colour_lab(a); lb <- colour_lab(b)
  sqrt(rowSums((lb - matrix(la, nrow(lb), 3, byrow = TRUE))^2))
}

adjacent_delta_e <- function(cols) {
  if (length(cols) < 2L) return(numeric(0))
  lab <- colour_lab(cols)
  sqrt(rowSums(diff(lab)^2))
}

relative_luminance <- function(col) {
  v <- grDevices::col2rgb(col)[, 1] / 255
  lin <- ifelse(v <= 0.03928, v / 12.92, ((v + 0.055) / 1.055)^2.4)
  sum(lin * c(0.2126, 0.7152, 0.0722))
}

contrast_ratio <- function(a, b) {
  la <- relative_luminance(a); lb <- relative_luminance(b)
  (max(la, lb) + 0.05) / (min(la, lb) + 0.05)
}

grey_of <- function(cols) {
  L <- colour_lab(cols)[, "L"]
  grDevices::gray(pmin(pmax(L / 100, 0), 1))
}

# Smallest colour difference between any two classes; a diverging palette's
# two ends can collapse under a deficiency even when every adjacent pair is
# fine, and that confuses "high" with "low".
closest_pair <- function(cols) {
  lab <- colour_lab(cols)
  n <- nrow(lab)
  best <- c(Inf, NA, NA)
  for (i in seq_len(n - 1L)) for (j in (i + 1L):n) {
    d <- sqrt(sum((lab[i, ] - lab[j, ])^2))
    if (d < best[1]) best <- c(d, i, j)
  }
  best
}

cvd_table <- function(cols) {
  if (length(cols) < 2L || !requireNamespace("colorspace", quietly = TRUE)) return(NULL)
  sims <- list(deutan = colorspace::deutan(cols), protan = colorspace::protan(cols),
               tritan = colorspace::tritan(cols))
  do.call(rbind, lapply(names(sims), function(nm) {
    de <- adjacent_delta_e(sims[[nm]])
    i <- which.min(de)
    any <- closest_pair(sims[[nm]])
    data.frame(deficiency = nm, min_delta_e = min(de),
               worst_pair = paste0(i, "-", i + 1L),
               min_any = any[1], closest = paste0(any[2], "-", any[3]),
               stringsAsFactors = FALSE)
  }))
}

# Sequential: lightness runs one way. Diverging: both ends darker (or both
# lighter) than the middle, monotone on each side. Otherwise categorical.
infer_palette_type <- function(L) {
  n <- length(L)
  if (n < 2L) return(NA_character_)
  d <- diff(L)
  tol <- 1.5
  if (all(d >= -tol) || all(d <= tol)) {
    return(if (abs(L[n] - L[1]) >= 20) "seq" else "cat")
  }
  if (n >= 3L) {
    v_shape <- function(centre, up) {
      if (centre %in% c(1L, n)) return(FALSE)
      left <- d[seq_len(centre - 1L)]; right <- d[centre:(n - 1L)]
      if (up) all(left >= -tol) && all(right <= tol) && max(L[1], L[n]) < L[centre] - 15
      else all(left <= tol) && all(right >= -tol) && min(L[1], L[n]) > L[centre] + 15
    }
    if (v_shape(which.max(L), TRUE) || v_shape(which.min(L), FALSE)) return("div")
  }
  "cat"
}

# The type a named palette is registered with, or NULL.
palette_type_lookup <- function(palette) {
  if (!is.character(palette) || length(palette) != 1L) return(NULL)
  idx <- tryCatch(palette_index(), error = function(e) NULL)
  if (is.null(idx)) return(NULL)
  nm <- if (grepl("::", palette, fixed = TRUE)) palette else {
    src <- tryCatch(guess_source(palette), error = function(e) NULL)
    if (is.null(src)) return(NULL)
    paste0(src, "::", palette)
  }
  hit <- idx$type[match(nm, idx$name)]
  if (length(hit) && !is.na(hit)) hit else NULL
}
