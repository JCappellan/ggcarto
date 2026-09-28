# ===========================================================================
# diagnose_map() / diagnose_scale(): the decisions hidden inside a scale
#
# A finished choropleth shows colours. It does not show that the fourth class
# holds nine counties, that 6% of the values were squished into the top
# class, that the neutral colour sits at a midpoint outside the data, or that
# the missing-data grey is a near match for class 2. diagnose_scale() reads
# all of that off a ggcarto scale and the data it will see; diagnose_map()
# finds both inside a ggplot.
# ===========================================================================

#' Diagnose a map's colour scale
#'
#' Reports the decisions a ggcarto scale makes for a particular variable --
#' classification, class counts, palette, limits, squishing, missing data,
#' legend labels -- and flags the ones that commonly mislead a reader.
#' `diagnose_map()` takes a ggplot and finds the scale and the mapped
#' variable itself; `diagnose_scale()` takes a scale and the values.
#'
#' @param p A ggplot with a ggcarto fill or colour scale ([scale_map()] or
#'   any of its wrappers).
#' @param aesthetic Which aesthetic to diagnose, `"fill"` or `"colour"`.
#'   `NULL` takes the first ggcarto scale found.
#' @param scale A ggcarto scale object.
#' @param x The values the scale will see: numeric for continuous, interval
#'   and rank scales; a factor or character vector for categorical and ordinal
#'   ones.
#' @param background Colour the map is drawn on, for the palette check.
#' @details
#' The warnings are heuristics with stated thresholds, phrased as
#' consequences for the reader rather than as faults. For interval scales:
#' a class with no observations or fewer than 3; a class under 2\% of
#' observations; more than 7 classes; more than 5\% of values squished into
#' the end classes; a diverging palette with no `midpoint`, or a midpoint
#' outside the data; a missing-data colour within a CIE76 \eqn{\Delta E} of
#' 10 of a class colour; adjacent class colours within 10. Categorical scales
#' are flagged for sequential palettes (they imply an order) and ordinal
#' scales for qualitative ones (they hide it). See [check_palette()] for the
#' palette measurements.
#' @return An object of class `ggcarto_diagnosis`: a list with `type`,
#'   `style`, `n_requested`, `n_classes`, `breaks`, `classes` (the
#'   [class_summary()] table), `labels`, `palette`, `palette_type`,
#'   `colours`, `limits`, `range`, `midpoint`, `oob`, `n_squished`
#'   (below and above the limits), `n_censored`, `n_missing`, `na_colour`,
#'   `closure`, `as_count`, `transform`, `palette_check` (a
#'   [check_palette()] result) and `warnings` (character). Print it for a
#'   report.
#' @examples
#' library(ggplot2)
#' set.seed(1)
#' df <- data.frame(x = 1:300, y = 1, z = c(rlnorm(297, 3, 1), NA, NA, 4000))
#' p <- ggplot(df, aes(x, y, fill = z)) +
#'   geom_tile() +
#'   scale_fill_intervals("brewer::YlGnBu", "fisher", 6,
#'                        limits = trim_limits(0.02, 0.98), na.value = "grey85")
#' diagnose_map(p)
#'
#' # the same, without building a plot
#' diagnose_scale(scale_fill_map("brewer::RdBu", midpoint = 0), df$z)
#' @export
diagnose_map <- function(p, aesthetic = NULL, background = "white") {
  if (!inherits(p, "ggplot")) abort_ggcarto("`p` must be a ggplot object.")
  built <- ggplot2::ggplot_build(p)
  scales <- built$plot$scales$scales
  is_ours <- vapply(scales, function(s) !is.null(s$ggcarto), logical(1))
  if (!any(is_ours)) {
    abort_ggcarto("No ggcarto scale found in this plot. Add scale_fill_map() ",
                  "or one of its wrappers first.")
  }
  if (!is.null(aesthetic)) {
    aesthetic <- if (aesthetic == "color") "colour" else aesthetic
    hit <- is_ours & vapply(scales, function(s) aesthetic %in% s$aesthetics, logical(1))
    if (!any(hit)) abort_ggcarto("No ggcarto scale for `", aesthetic, "` in this plot.")
  } else {
    hit <- is_ours
  }
  sc <- scales[[which(hit)[1]]]
  aes_name <- intersect(sc$aesthetics, c("fill", "colour"))[1]
  x <- mapped_values(built$plot, aes_name)
  if (is.null(x)) {
    # Fall back on what the scale retained while training.
    x <- sc$data_values
    if (is.null(x)) {
      abort_ggcarto("Could not recover the values mapped to `", aes_name,
                    "`; call diagnose_scale(scale, x) with the vector directly.")
    }
    tr <- scale_transformation(sc)
    if (!is.null(tr) && !identical(tr$name, "identity")) x <- tr$inverse(x)
  }
  diagnose_scale(sc, x, background = background)
}

# The values behind an aesthetic: evaluate the mapping in the layer data,
# plot-level mapping and data as fallbacks. Base R only -- a quosure is a
# formula with an environment attached.
mapped_values <- function(plot, aesthetic) {
  eval_map <- function(mapping, data) {
    q <- mapping[[aesthetic]]
    if (is.null(q)) return(NULL)
    expr <- if (inherits(q, "formula") || inherits(q, "quosure")) unclass(q)[[2]] else q
    env <- attr(q, ".Environment") %||% globalenv()
    if (is.null(data) || is_waive(data) || !is.data.frame(data)) return(NULL)
    # `.data[["z"]]` and `.data$z` work on a data frame without rlang.
    out <- tryCatch(eval(expr, c(as.list(data), list(.data = data)), env),
                    error = function(e) NULL)
    if (!is.null(out) && length(out) == nrow(data)) out else NULL
  }
  for (layer in plot$layers) {
    data <- layer$data
    if (is.function(data)) data <- tryCatch(data(plot$data), error = function(e) NULL)
    if (is.null(data) || is_waive(data)) data <- plot$data
    v <- eval_map(layer$mapping, data)
    if (is.null(v) && isTRUE(layer$inherit.aes)) v <- eval_map(plot$mapping, data)
    if (!is.null(v)) return(v)
  }
  NULL
}

#' @rdname diagnose_map
#' @export
diagnose_scale <- function(scale, x, background = "white") {
  meta <- scale$ggcarto
  if (is.null(meta)) {
    abort_ggcarto("`scale` is not a ggcarto scale; build it with scale_map() ",
                  "or one of its wrappers.")
  }
  switch(meta$type,
    intervals = diagnose_intervals(scale, meta, x, background),
    continuous = , rank = diagnose_continuous(scale, meta, x, background),
    diagnose_discrete(scale, meta, x, background)
  )
}

# --- shared pieces ---------------------------------------------------------

resolve_limits <- function(limits, v) {
  if (is.function(limits)) limits <- limits(v)
  if (is.null(limits) || all(is.na(limits))) return(range(v))
  lims <- as.numeric(limits)
  lims[is.na(lims)] <- range(v)[is.na(lims)]
  sort(lims)
}

oob_name <- function(oob) {
  if (is.character(oob)) return(oob)
  if (identical(oob, scales::oob_squish)) return("squish")
  if (identical(oob, scales::oob_censor)) return("censor")
  if (identical(oob, scales::oob_keep)) return("keep")
  if (identical(oob, scales::oob_squish_infinite)) return("infinite")
  "custom"
}

palette_name <- function(meta) {
  p <- meta$palette
  if (is.character(p) && length(p) == 1L) p
  else if (is.character(p)) paste0("<", length(p), " colours>")
  else "<function>"
}

trained_clone <- function(scale, v) {
  sc <- scale$clone()
  # A clone inherits the parent's trained values and frozen breaks; start
  # clean so the labels describe `v` and nothing else.
  sc$data_values <- NULL
  sc$frozen_breaks <- NULL
  tr <- scale_transformation(sc)
  xt <- if (!is.null(tr) && !identical(tr$name, "identity")) tr$transform(v) else v
  sc$train(xt[is.finite(xt)])
  sc
}

# --- intervals -------------------------------------------------------------

diagnose_intervals <- function(scale, meta, x, background) {
  th <- guide_thresholds()
  v <- suppressWarnings(as.numeric(x))
  fin <- v[is.finite(v)]
  n_missing <- sum(is.na(v))
  if (!length(fin)) abort_ggcarto("`x` has no finite values.")

  lims <- resolve_limits(meta$limits, fin)
  oob <- oob_name(meta$oob)
  below <- sum(fin < lims[1]); above <- sum(fin > lims[2])
  n_out <- below + above

  # The classes the scale will draw: values squished or censored into the
  # limits, then classified, then closed at the limits -- exactly what
  # ScaleBinnedMap$get_breaks() does.
  vv <- switch(oob,
    censor = fin[fin >= lims[1] & fin <= lims[2]],
    pmin(pmax(fin, lims[1]), lims[2])
  )
  n_censored <- if (oob == "censor") n_out else 0L
  b_all <- scale$map_breaks_fn(vv)
  mirrored <- is_mirrored(b_all)
  eff <- if (mirrored) as.numeric(b_all)
         else sort(unique(c(lims[1], b_all[b_all > lims[1] & b_all < lims[2]], lims[2])))
  tab <- class_table(vv, eff, closure = meta$closure, as_count = meta$as_count)
  k <- nrow(tab)
  # Mirrored sets: the outer classes one side never reaches are expected;
  # report them as such rather than as empty classes.
  never_drawn <- rep(FALSE, k)
  if (mirrored && k > 2L) {
    never_drawn <- (cumsum(tab$n) == 0) | rev(cumsum(rev(tab$n)) == 0)
  }

  # Legend labels as the scale itself would write them.
  labels <- tryCatch({
    sc <- trained_clone(scale, fin)
    sc$get_labels()
  }, error = function(e) tab$label)
  if (length(labels) != k) {
    labels <- if (mirrored && length(labels) == sum(!never_drawn)) {
      out <- tab$label; out[!never_drawn] <- labels; out[never_drawn] <- "(not drawn)"; out
    } else tab$label
  }
  open_ends <- squished_ends(fin, lims)
  legend_says_so <- !is.null(scale$label_parts) && any(open_ends) &&
    all(resolve_open(scale$label_parts$open, open_ends)[open_ends]) &&
    (isTRUE(scale$interval_labels) || any(grepl(scale$label_parts$or_more, labels, fixed = TRUE)) ||
       any(grepl(scale$label_parts$or_less, labels, fixed = TRUE)))

  cols <- if (meta$interpolate == "even") meta$pal_fn(k) else {
    mids <- (tab$lower + tab$upper) / 2
    scales::gradient_n_pal(meta$pal_fn(256L))(scales::rescale(mids, from = lims))
  }
  ptype <- palette_type_lookup(meta$palette)
  pc <- check_palette(cols, k, type = ptype, background = background,
                      na.value = meta$na.value)
  ptype <- ptype %||% pc$type_inferred

  w <- character(0)
  if (mirrored) {
    w <- c(w, sprintf("classes are mirrored around %s (%d per side, by %s%s); the palette is sampled over all %d so its neutral colour sits on the midpoint",
                      format(attr(b_all, "midpoint")), attr(b_all, "per_side"), attr(b_all, "mirror"),
                      if (!is.null(attr(b_all, "band"))) sprintf(", neutral band %s to %s",
                        format(signif(attr(b_all, "band")[1], 4)), format(signif(attr(b_all, "band")[2], 4))) else "", k))
    if (any(never_drawn)) {
      w <- c(w, sprintf("%d class%s on the %s side %s never drawn: no area reaches that far, so the %s end of the palette does not appear on the map",
                        sum(never_drawn), if (sum(never_drawn) > 1) "es" else "",
                        if (never_drawn[1]) "low" else "high",
                        if (sum(never_drawn) > 1) "are" else "is",
                        if (never_drawn[1]) "low" else "high"))
    }
  }
  empty <- tab$n == 0 & !never_drawn
  if (any(empty)) {
    w <- c(w, sprintf("%d class%s contain%s no observations: %s a legend key with no area on the map",
                      sum(empty), if (sum(empty) > 1) "es" else "",
                      if (sum(empty) > 1) "" else "s",
                      if (sum(empty) > 1) "each is" else "that is"))
  }
  tiny <- tab$n > 0 & tab$n < th$tiny_n
  if (any(tiny)) {
    w <- c(w, sprintf("class %s holds fewer than %d observations: a colour the reader will hunt for",
                      paste(which(tiny), collapse = ", "), th$tiny_n))
  }
  thin <- tab$n >= th$tiny_n & tab$pct < th$thin_pct
  if (any(thin)) {
    w <- c(w, sprintf("class %s holds under %g%% of observations (%s): it will barely appear on the map",
                      paste(which(thin), collapse = ", "), th$thin_pct,
                      paste(fmt_n(tab$n[thin]), collapse = ", ")))
  }
  if (k > th$max_readable) {
    w <- c(w, sprintf("%d ordered classes: more than most readers can match back to the legend", k))
  }
  if (n_out && oob %in% c("squish", "infinite")) {
    pct <- 100 * n_out / length(fin)
    msg <- if (legend_says_so) {
      sprintf("%s of %s values (%.1f%%) lie outside the limits and are drawn in the end classes; the legend says so (\"%s\")",
              fmt_n(n_out), fmt_n(length(fin)), pct,
              if (open_ends[2]) labels[k] else labels[1])
    } else {
      sprintf("%s of %s values (%.1f%%) lie outside the limits and are squished into the end classes; the map cannot show how extreme they are%s",
              fmt_n(n_out), fmt_n(length(fin)), pct,
              if (pct > 5) " -- at this share, say so in the legend (label_map(open = TRUE))" else "")
    }
    w <- c(w, msg)
  }
  if (n_censored) {
    w <- c(w, sprintf("%s values outside the limits are censored: they will be drawn in the missing-data colour, indistinguishable from no data",
                      fmt_n(n_censored)))
  }
  if (identical(ptype, "div") && is.null(meta$midpoint)) {
    w <- c(w, "a diverging palette without `midpoint`: the neutral colour falls wherever the middle class happens to be, which means nothing")
  }
  if (!is.null(meta$midpoint)) {
    if (meta$midpoint < min(fin) || meta$midpoint > max(fin)) {
      w <- c(w, sprintf("midpoint %s lies outside the data (%s to %s): every area is on one side of it, so the map is one-signed and half the palette is unused",
                        format(meta$midpoint), format(signif(min(fin), 4)), format(signif(max(fin), 4))))
    }
    if (!identical(ptype, "div")) {
      w <- c(w, "a `midpoint` with a sequential palette: nothing in the colours marks it")
    }
  }
  if (n_missing) {
    w <- c(w, sprintf("%s missing values (%.1f%%) are drawn in %s; make sure the legend has a key for them (na_key())",
                      fmt_n(n_missing), 100 * n_missing / length(v), meta$na.value))
  }
  issues <- pc$issues
  # A break sitting on the midpoint makes the even-class warning moot.
  if (!is.null(meta$midpoint) && any(abs(eff - meta$midpoint) < 1e-9 * max(1, abs(meta$midpoint)))) {
    issues <- issues[!grepl("even number of classes", issues)]
  }
  w <- c(w, issues)

  structure(list(
    type = "intervals", style = if (is.null(meta$breaks)) meta$style else "fixed",
    mirror = if (mirrored) attr(b_all, "mirror") else NULL,
    per_side = if (mirrored) attr(b_all, "per_side") else NULL,
    band = if (mirrored) attr(b_all, "band") else NULL,
    never_drawn = never_drawn,
    n_requested = if (is.null(meta$breaks)) as.integer(meta$n) else k,
    n_classes = k, breaks = eff, classes = tab, labels = labels,
    palette = palette_name(meta), palette_type = ptype, colours = cols,
    interpolate = meta$interpolate, range = range(fin), limits = lims,
    midpoint = meta$midpoint, oob = oob,
    n_squished = c(below = below, above = above), n_censored = n_censored,
    n_valid = length(fin), n_missing = n_missing, na_colour = meta$na.value,
    closure = meta$closure, as_count = meta$as_count,
    transform = transform_name(meta$transform), palette_check = pc,
    warnings = w
  ), class = "ggcarto_diagnosis")
}

transform_name <- function(tr) {
  if (is.character(tr)) tr else if (!is.null(tr$name)) tr$name else "custom"
}

# --- continuous and rank ---------------------------------------------------

diagnose_continuous <- function(scale, meta, x, background) {
  v <- suppressWarnings(as.numeric(x))
  fin <- v[is.finite(v)]
  n_missing <- sum(is.na(v))
  if (!length(fin)) abort_ggcarto("`x` has no finite values.")
  lims <- resolve_limits(meta$limits, fin)
  oob <- if (meta$type == "rank") "squish" else oob_name(meta$oob)
  below <- sum(fin < lims[1]); above <- sum(fin > lims[2])
  n_out <- below + above
  cols <- meta$pal_fn(7L)
  ptype <- palette_type_lookup(meta$palette)
  pc <- check_palette(cols, 7L, type = ptype, background = background,
                      na.value = meta$na.value)
  ptype <- ptype %||% pc$type_inferred
  tick_labels <- character(0)
  ticks <- tryCatch({
    sc <- trained_clone(scale, fin)
    b <- sc$get_breaks()
    tick_labels <- sc$get_labels(b)
    tr <- scale_transformation(sc)
    if (!is.null(tr) && !identical(tr$name, "identity")) b <- tr$inverse(b)
    b[is.finite(b)]
  }, error = function(e) NULL)

  w <- character(0)
  if (meta$type == "continuous" && n_out && oob %in% c("squish", "infinite")) {
    pct <- 100 * n_out / length(fin)
    open_ends <- squished_ends(fin, lims)
    says_so <- !is.null(scale$label_parts) && length(tick_labels) > 1L &&
      all(resolve_open(scale$label_parts$open, open_ends)[open_ends])
    w <- c(w, if (says_so) {
      sprintf("%s values (%.1f%%) lie outside the limits and are drawn in the end colours; the limit ticks say so (\"%s\")",
              fmt_n(n_out), pct, paste(tick_labels[c(1, length(tick_labels))][open_ends], collapse = "\", \""))
    } else {
      sprintf("%s values (%.1f%%) lie outside the limits and are squished to the ends of the ramp; the colourbar does not say so",
              fmt_n(n_out), pct)
    })
  }
  if (meta$type == "continuous" && n_out && oob == "censor") {
    w <- c(w, sprintf("%s values outside the limits are censored and will look like missing data", fmt_n(n_out)))
  }
  if (meta$type == "continuous" && is.null(meta$limits)) {
    s <- stats::sd(fin)
    if (is.finite(s) && s > 0) {
      skew <- mean((fin - mean(fin))^3) / s^3
      if (abs(skew) > 2) {
        w <- c(w, sprintf("the variable is extremely skewed (skewness %.1f): on a continuous ramp most areas share one end of the palette; consider intervals, `trim_limits()` or `type = \"rank\"`", skew))
      }
    }
  }
  if (identical(ptype, "div") && is.null(meta$midpoint) && meta$type == "continuous") {
    w <- c(w, "a diverging palette without `midpoint`: the neutral colour sits at the middle of the range, which means nothing")
  }
  if (!is.null(meta$midpoint) && (meta$midpoint < min(fin) || meta$midpoint > max(fin))) {
    w <- c(w, sprintf("midpoint %s lies outside the data: the whole map is on one side of it", format(meta$midpoint)))
  }
  if (!is.null(meta$midpoint) && !identical(ptype, "div")) {
    w <- c(w, "a `midpoint` with a sequential palette: nothing in the colours marks it")
  }
  if (!is.null(meta$midpoint) && is.numeric(meta$limits) && !isTRUE(all.equal(mean(lims), meta$midpoint))) {
    w <- c(w, "limits are not symmetric about the midpoint: equal colour distances mean unequal data distances on the two sides (see symmetric_limits())")
  }
  if (meta$type == "rank") {
    w <- c(w, "a rank scale colours by position, not value: two neighbouring colours may differ by a hair or by an order of magnitude; the legend is keyed at quantiles for that reason")
  }
  if (n_missing) {
    w <- c(w, sprintf("%s missing values (%.1f%%) are drawn in %s; a colourbar has no key for them (na_key())",
                      fmt_n(n_missing), 100 * n_missing / length(v), meta$na.value))
  }
  w <- c(w, pc$issues)

  structure(list(
    type = meta$type, style = NULL, n_requested = NA_integer_, n_classes = NA_integer_,
    breaks = ticks, classes = NULL, labels = tick_labels,
    palette = palette_name(meta), palette_type = ptype, colours = cols,
    range = range(fin), limits = lims, midpoint = meta$midpoint, oob = oob,
    n_squished = c(below = below, above = above),
    n_censored = if (oob == "censor") n_out else 0L,
    n_valid = length(fin), n_missing = n_missing, na_colour = meta$na.value,
    transform = transform_name(meta$transform), palette_check = pc, warnings = w
  ), class = "ggcarto_diagnosis")
}

# --- categorical and ordinal -----------------------------------------------

diagnose_discrete <- function(scale, meta, x, background) {
  f <- if (is.factor(x)) x else factor(x)
  n_missing <- sum(is.na(f))
  lev <- levels(droplevels(f))
  k <- length(lev)
  counts <- as.integer(table(f))
  cols <- meta$pal_fn(max(k, 1L))
  ptype <- palette_type_lookup(meta$palette)
  pc <- check_palette(cols, max(k, 1L), type = ptype, background = background,
                      na.value = meta$na.value)
  ptype <- ptype %||% pc$type_inferred

  w <- character(0)
  if (meta$type == "categorical" && identical(ptype, "seq")) {
    w <- c(w, "unordered categories on a sequential palette: readers will rank the categories by darkness, and the ranking is arbitrary")
  }
  if (meta$type == "ordinal" && identical(ptype, "cat")) {
    w <- c(w, "ordered categories on a qualitative palette: the order is invisible; use a sequential palette so the ramp carries it")
  }
  if (k > 9L) {
    w <- c(w, sprintf("%d categories: more hues than a reader can match to a legend; group them or map fewer", k))
  }
  if (any(counts == 0)) {
    w <- c(w, sprintf("%d level%s with no observations", sum(counts == 0), if (sum(counts == 0) > 1) "s" else ""))
  }
  if (n_missing) {
    w <- c(w, sprintf("%s missing values (%.1f%%) are drawn in %s", fmt_n(n_missing),
                      100 * n_missing / length(f), meta$na.value))
  }
  w <- c(w, pc$issues)

  tab <- data.frame(class = seq_len(k), label = lev, n = counts[match(lev, levels(f))],
                    stringsAsFactors = FALSE)
  tab$pct <- 100 * tab$n / sum(tab$n)

  structure(list(
    type = meta$type, style = NULL, n_requested = NA_integer_, n_classes = k,
    breaks = NULL, classes = tab, labels = lev,
    palette = palette_name(meta), palette_type = ptype, colours = cols,
    range = NULL, limits = NULL, midpoint = NULL, oob = NULL,
    n_squished = c(below = 0L, above = 0L), n_censored = 0L,
    n_valid = sum(!is.na(f)), n_missing = n_missing, na_colour = meta$na.value,
    transform = NULL, palette_check = pc, warnings = w
  ), class = "ggcarto_diagnosis")
}

# --- print -----------------------------------------------------------------

#' @export
print.ggcarto_diagnosis <- function(x, ...) {
  kind <- function(t) if (is.null(t) || is.na(t)) "unknown" else c(seq = "sequential", div = "diverging", cat = "categorical")[t]
  cat("ggcarto map diagnosis\n\n")
  cat("Mapping: ", x$type, "\n", sep = "")
  if (x$type == "intervals") {
    cat("Classification: ", style_label(x$style),
        if (!is.null(x$mirror)) sprintf(", mirrored by %s around %s: %d per side, %d in all%s",
                                        x$mirror, format(x$midpoint), x$per_side, x$n_classes,
                                        if (any(x$never_drawn)) sprintf(" (%d not drawn)", sum(x$never_drawn)) else "")
        else if (x$style != "fixed") sprintf(", %d requested, %d drawn", x$n_requested, x$n_classes)
        else sprintf(", %d classes", x$n_classes), "\n", sep = "")
    cat("Breaks: ", paste(signif(x$breaks, 4), collapse = ", "), "\n", sep = "")
    cat("Closure: ", if (x$closure == "right") "(a, b]" else "[a, b)",
        if (x$as_count) ", counts" else "", "\n", sep = "")
  }
  if (x$type %in% c("intervals", "continuous", "rank")) {
    cat("Values: ", fmt_n(x$n_valid), " (", fmt_n(x$n_missing), " missing), range ",
        format(signif(x$range[1], 4)), " to ", format(signif(x$range[2], 4)), "\n", sep = "")
    cat("Limits: ", format(signif(x$limits[1], 4)), " to ", format(signif(x$limits[2], 4)),
        "; oob = ", x$oob, "; ", fmt_n(sum(x$n_squished)), " outside (",
        fmt_n(x$n_squished[["below"]]), " below, ", fmt_n(x$n_squished[["above"]]), " above)\n", sep = "")
    if (!is.null(x$midpoint)) cat("Midpoint: ", format(x$midpoint), "\n", sep = "")
    if (!is.null(x$transform) && !identical(x$transform, "identity")) cat("Transform: ", x$transform, "\n", sep = "")
  } else {
    cat("Values: ", fmt_n(x$n_valid), " (", fmt_n(x$n_missing), " missing), ",
        x$n_classes, " categories\n", sep = "")
  }
  cat("Palette: ", x$palette, " (", kind(x$palette_type), ")",
      if (!is.null(x$interpolate)) paste0(", interpolate = \"", x$interpolate, "\"") else "", "\n", sep = "")
  cat("Colours: ", paste(x$colours, collapse = " "), "\n", sep = "")
  cat("Missing colour: ", x$na_colour, "\n", sep = "")
  if (x$type == "intervals" && nrow(x$classes)) {
    cat("\n")
    tab <- x$classes
    tab$label <- x$labels
    lab <- format(c("Class", tab$label))
    n <- format(c("n", fmt_n(tab$n)), justify = "right")
    pct <- format(c("%", sprintf("%.1f", tab$pct)), justify = "right")
    col <- c("colour", x$colours)
    cat(paste("  ", lab, " ", n, " ", pct, "  ", col), sep = "\n")
  } else if (x$type %in% c("categorical", "ordinal") && nrow(x$classes)) {
    cat("\n")
    lab <- format(c("Level", x$classes$label))
    n <- format(c("n", fmt_n(x$classes$n)), justify = "right")
    pct <- format(c("%", sprintf("%.1f", x$classes$pct)), justify = "right")
    col <- c("colour", x$colours)
    cat(paste("  ", lab, " ", n, " ", pct, "  ", col), sep = "\n")
  } else if (!is.null(x$breaks)) {
    cat("Legend ticks: ", paste(signif(x$breaks, 4), collapse = ", "), "\n", sep = "")
    if (length(x$labels)) cat("Tick labels: ", paste(x$labels, collapse = " | "), "\n", sep = "")
  }
  if (length(x$warnings)) {
    cat("\nThings to check:\n")
    cat(paste0("  ", bullet(), " ", x$warnings), sep = "\n")
  } else {
    cat("\nNothing flagged.\n")
  }
  invisible(x)
}
