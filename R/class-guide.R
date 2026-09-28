# ===========================================================================
# class_guide(): how many classes?
#
# There is no optimal number of classes. There is a trade-off between how
# faithfully the classes reproduce the distribution (statistical fit), whether
# every class has enough areas in it to be worth a colour (balance), whether
# the break values are far enough apart to mean anything (separation), and
# how many ordered colours a reader can tell apart (perception). class_guide()
# computes the four for every candidate n, applies a small set of stated rules
# that differ by style family, and returns a *suggestion* with its reasons.
# Every number behind the suggestion is in the returned table.
# ===========================================================================

#' Suggest a number of classes
#'
#' Evaluates candidate class counts for one classification style and suggests
#' one, with reasons. The suggestion weighs statistical fit, class balance,
#' break separation and perceptual load; it is a judgement made by stated
#' rules, not an optimum, and the table of metrics is returned so you can
#' disagree with it.
#'
#' @inheritParams class_summary
#' @param n Candidate class counts. Styles that choose their own count
#'   (`headtails`, `box`, `dpih`, `unique`) and `fixed` ignore it and report
#'   the count they produce.
#' @param ... Extra arguments for the style, passed to [compute_breaks()];
#'   `breaks = ` for `style = "fixed"`; `midpoint`, `mirror`, `center` and
#'   `band` for a diverging variable classed symmetrically, in which case
#'   every candidate `n` is classes *per side* and the readability rating
#'   applies to the total.
#'
#' @details
#' **Metrics.** For each candidate `n` the data are classed with
#' [compute_breaks()] and the following are computed:
#'
#' * `fit`: the goodness of variance fit, \eqn{1 - SSW / SST}, where SSW is the
#'   sum of squared deviations from the class means and SST from the grand
#'   mean (Jenks and Caspall 1971). It is 0 for one class and 1 when every
#'   class is constant. It is the quantity Fisher-Jenks maximises, so it is the
#'   natural yardstick for the variance-based styles and merely descriptive
#'   for the others.
#' * `gain`: the increase in fit over the previous candidate.
#' * `smallest_class_n`, `smallest_class_pct`, `largest_class_pct`,
#'   `empty_classes`: class balance.
#' * `min_spacing`, `spacing_pct`: the narrowest gap between adjacent breaks,
#'   absolute and as a share of the data range.
#' * `readability`: a perceptual rating by class count alone -- up to 5
#'   ordered colours are `"excellent"`, 6 `"good"`, 7 `"fair"`, more `"poor"`
#'   (the 5-to-7 ceiling is the usual guidance for sequential schemes).
#'
#' **Rules.** The style families reason differently:
#'
#' * *Variance styles* (`fisher`, `jenks`, `kmeans`, `hclust`, `bclust`,
#'   `maximum`): a candidate is eligible if it produced the requested number of
#'   classes and none is empty or holds fewer than 3 observations. The
#'   suggestion is the smallest eligible `n` whose fit is at least 0.80 and
#'   whose gain to the next candidate is below 0.05 -- where the fit curve
#'   flattens. If the suggestion has a thin class (under 2\% of observations)
#'   and the candidate below loses less than 0.05 of fit, it steps down. More
#'   than 7 classes is never suggested when a 7-or-fewer solution is within
#'   0.05 of fit. The next `n` is offered for consideration when its gain is
#'   still at least 0.02.
#' * *Quantile*: classes are balanced by construction, so fit does not decide.
#'   Candidates with adjacent breaks closer than 1\% of the data range are
#'   flagged -- the legend would draw distinctions the data cannot support --
#'   and 5 classes is suggested when it is clean, otherwise the nearest clean
#'   count in 4 to 7.
#' * *Range styles* (`equal`, `pretty`, `log`, `geometric`): candidates with
#'   empty classes are ineligible and thin classes count against a candidate,
#'   because each is a colour in the legend that appears on almost no area.
#'   Among the rest the variance rule applies, and skew is reported, since
#'   skew is what empties equal-width classes.
#' * *Self-sizing styles* (`headtails`, `box`, `dpih`): the count the method
#'   chose is reported along with why (for `headtails`, the head share at
#'   each split); `n` is not optimised.
#' * *Standard deviation*: classes are distances from the mean, so the
#'   suggestion is the largest candidate up to 6 whose outer classes are not
#'   empty, and the share of values beyond two standard deviations and the
#'   skewness are reported, since `sd` classes assume rough symmetry.
#' * *Fixed*: no suggestion; the classes are defined by the thresholds you
#'   supplied.
#' * *Unique*: one class per distinct value; suggested only when there are
#'   at most 9 of them.
#'
#' @return An object of class `ggcarto_class_guide`: a list with `style`,
#'   `family`, `suggested` (an integer, or `NA` for `fixed`), `reasons`
#'   (character), `consider` (character or `NULL`), `distribution` (a list
#'   describing `x`), `table` (a data frame with one row per candidate and the
#'   metrics above, plus a `warnings` column), `breaks` and `counts` (lists by
#'   candidate). `print()` gives the narrative and a compact table,
#'   `summary()` the full metrics table, and `plot()` the fit curve
#'   (`type = "fit"`) or the class balance (`type = "balance"`).
#' @references Jenks, G. F. and Caspall, F. C. (1971). Error on choroplethic
#'   maps: definition, measurement, reduction. *Annals of the Association of
#'   American Geographers* 61(2), 217--244.
#' @examples
#' x <- rlnorm(800, 3, 0.8)
#' g <- class_guide(x, "fisher")
#' g
#' summary(g)
#' plot(g)
#' plot(g, type = "balance")
#'
#' class_guide(x, "quantile", n = 3:8)
#' class_guide(x, "equal")
#' class_guide(x, "headtails")
#' @seealso [class_summary()] for the classes at the chosen `n`,
#'   [plot_breaks()] to see them on the distribution.
#' @export
class_guide <- function(x, style = "fisher", n = 3:9,
                        closure = c("right", "left"), as_count = FALSE, ...) {
  closure <- match.arg(closure)
  if (isTRUE(as_count)) closure <- "left"
  style <- match.arg(style, map_styles())
  args <- list(...)
  if (style == "fixed" && is.null(args$breaks) && is.null(args$fixedBreaks)) {
    abort_ggcarto("style = \"fixed\" needs `breaks = ` to be supplied.")
  }
  family <- style_family(style)
  v <- suppressWarnings(as.numeric(x))
  fin <- v[is.finite(v)]
  dist <- describe_distribution(fin)
  dist$n_total <- length(v)
  dist$n_missing <- sum(is.na(v))
  dist$n_nonfinite <- sum(is.infinite(v))

  n <- sort(unique(as.integer(n)))
  n <- n[!is.na(n) & n >= 2L]
  if (family %in% c("self", "fixed", "unique")) n <- n[1] %||% 5L  # one pass

  rows <- lapply(n, function(k) guide_row(fin, style, k, args, closure, as_count))
  tab <- do.call(rbind, lapply(rows, `[[`, "row"))
  tab$gain <- c(NA_real_, diff(tab$fit))
  tab$readability <- readability_rating(tab$actual_classes)
  mirrored <- !is.null(args$mirror) && !identical(args$mirror, "none")
  tab <- tab[, c("n", "actual_classes", "expected_classes", "fit", "gain",
                 "smallest_class_n", "smallest_class_pct", "largest_class_pct",
                 "empty_classes", "min_spacing", "spacing_pct", "readability",
                 "warnings")]
  if (!mirrored) tab$expected_classes <- NULL
  rownames(tab) <- NULL

  rec <- switch(family,
    variance = recommend_variance(tab, dist),
    quantile = recommend_quantile(tab, dist),
    range    = recommend_range(tab, dist, style),
    self     = recommend_self(tab, dist, style, fin, args),
    sd       = recommend_sd(tab, dist, fin, args),
    fixed    = recommend_fixed(tab, dist),
    unique   = recommend_unique(tab, dist)
  )

  if (mirrored && !is.na(rec$suggested)) {
    rec$reasons <- c(rec$reasons,
      "n counts classes per side of the midpoint; the readability rating and the class counts above are for the whole mirrored set")
  }

  structure(list(
    style = style,
    style_label = style_label(style),
    family = family,
    suggested = rec$suggested,
    reasons = rec$reasons,
    consider = rec$consider,
    distribution = dist,
    table = tab,
    breaks = stats::setNames(lapply(rows, `[[`, "breaks"), tab$n),
    counts = stats::setNames(lapply(rows, `[[`, "counts"), tab$n),
    closure = closure,
    as_count = isTRUE(as_count),
    mirrored = mirrored,
    thresholds = guide_thresholds()
  ), class = "ggcarto_class_guide")
}

# The constants the rules use, in one place so the documentation and the code
# cannot drift apart.
guide_thresholds <- function() {
  list(
    fit_min = 0.80,      # a classification below this is not describing the data
    gain_min = 0.05,     # gains below this are "small"
    consider_gain = 0.02,# gains above this earn a "consider n + 1"
    thin_pct = 2,        # a class under this share of observations is thin
    tiny_n = 3,          # a class under this many observations is unusable
    close_pct = 1,       # adjacent breaks closer than this share of the range
    max_readable = 7,    # the usual ceiling for ordered colours
    preferred = 4:7
  )
}

# ---------------------------------------------------------------------------
# Metrics
# ---------------------------------------------------------------------------

goodness_of_fit <- function(x, cls) {
  if (length(x) < 2L) return(NA_real_)
  sst <- sum((x - mean(x))^2)
  if (sst <= 0) return(1)
  ssw <- sum(vapply(split(x, cls), function(v) sum((v - mean(v))^2), numeric(1)))
  1 - ssw / sst
}

readability_rating <- function(k) {
  out <- rep("poor", length(k))
  out[k <= 7] <- "fair"
  out[k <= 6] <- "good"
  out[k <= 5] <- "excellent"
  out[is.na(k)] <- NA_character_
  out
}

guide_row <- function(v, style, n, args, closure, as_count) {
  th <- guide_thresholds()
  na_row <- data.frame(
    n = n, actual_classes = NA_integer_, expected_classes = NA_integer_, fit = NA_real_,
    smallest_class_n = NA_integer_, smallest_class_pct = NA_real_,
    largest_class_pct = NA_real_, empty_classes = NA_integer_,
    min_spacing = NA_real_, spacing_pct = NA_real_,
    warnings = "could not compute breaks", stringsAsFactors = FALSE
  )
  if (length(v) < 2L) return(list(row = na_row, breaks = numeric(0), counts = integer(0)))

  b <- tryCatch(compute_breaks(v, style = style, n = n, args = args),
                error = function(e) NULL, warning = function(w) NULL)
  if (is.null(b) || length(b) < 2L) {
    return(list(row = na_row, breaks = numeric(0), counts = integer(0)))
  }
  if (isTRUE(as_count)) b <- count_breaks(b, v)
  k <- length(b) - 1L
  cls <- assign_classes(v, b, closure)
  counts <- tabulate(cls, nbins = k)
  pct <- 100 * counts / length(v)
  # Mirrored sets keep the outer classes one side never reaches, on purpose,
  # so the balance rules look at the classes the data actually span.
  judge <- rep(TRUE, k)
  w <- character(0)
  if (is_mirrored(b) && k > 2L) {
    lead <- cumsum(counts) == 0
    trail <- rev(cumsum(rev(counts)) == 0)
    judge <- !(lead | trail)
    if (any(lead)) w <- c(w, sprintf("%d outer class%s on the low side never drawn (the data do not reach that far)",
                                     sum(lead), if (sum(lead) > 1) "es" else ""))
    if (any(trail)) w <- c(w, sprintf("%d outer class%s on the high side never drawn", sum(trail), if (sum(trail) > 1) "es" else ""))
  }
  counts_j <- counts[judge]; pct_j <- pct[judge]
  rng <- diff(range(v))
  spacing <- min(diff(b))

  expected <- if (is_mirrored(b)) 2L * n - (if (is.null(attr(b, "band"))) 0L else 1L) else n
  if (k != expected && !style %in% c("headtails", "box", "dpih", "unique", "fixed")) {
    w <- c(w, sprintf("only %d distinct classes could be formed", k))
  }
  if (any(counts_j == 0)) {
    w <- c(w, sprintf("%d empty class%s", sum(counts_j == 0), if (sum(counts_j == 0) > 1) "es" else ""))
  }
  tiny <- counts_j > 0 & counts_j < th$tiny_n
  if (any(tiny)) {
    w <- c(w, sprintf("%d class%s with fewer than %d observations",
                      sum(tiny), if (sum(tiny) > 1) "es" else "", th$tiny_n))
  }
  thin <- counts_j >= th$tiny_n & pct_j < th$thin_pct
  if (any(thin)) {
    w <- c(w, sprintf("%d thin class%s (under %g%% of observations)",
                      sum(thin), if (sum(thin) > 1) "es" else "", th$thin_pct))
  }
  if (rng > 0 && 100 * spacing / rng < th$close_pct) {
    w <- c(w, sprintf("adjacent breaks only %s apart (%.1f%% of the range)",
                      format(signif(spacing, 3)), 100 * spacing / rng))
  }
  if (k > th$max_readable) {
    w <- c(w, sprintf("%d ordered colours are hard to tell apart", k))
  }

  row <- data.frame(
    n = n, actual_classes = k, expected_classes = expected, fit = goodness_of_fit(v, cls),
    smallest_class_n = min(counts_j), smallest_class_pct = min(pct_j),
    largest_class_pct = max(pct_j), empty_classes = sum(counts_j == 0),
    min_spacing = spacing,
    spacing_pct = if (rng > 0) 100 * spacing / rng else NA_real_,
    warnings = paste(w, collapse = "; "), stringsAsFactors = FALSE
  )
  list(row = row, breaks = b, counts = counts)
}

describe_distribution <- function(v) {
  n <- length(v)
  if (n < 3L) {
    return(list(n = n, skewness = NA_real_, description = "too few values to describe",
                n_distinct = length(unique(v))))
  }
  s <- stats::sd(v)
  skew <- if (is.finite(s) && s > 0) mean((v - mean(v))^3) / s^3 else 0
  side <- if (skew < 0) "left" else "right"
  a <- abs(skew)
  desc <- if (a < 0.5) "roughly symmetric"
    else if (a < 1) paste0("moderately ", side, "-skewed")
    else if (a < 2) paste0("strongly ", side, "-skewed")
    else paste0("extremely ", side, "-skewed")
  nd <- length(unique(v))
  if (nd <= 12L) desc <- paste0(desc, ", coarse (", nd, " distinct values)")
  list(n = n, skewness = skew, description = desc, n_distinct = nd,
       range = range(v), mean = mean(v), sd = s)
}

# ---------------------------------------------------------------------------
# Rules
# ---------------------------------------------------------------------------

num_word <- function(k) {
  w <- c("one", "two", "three", "four", "five", "six", "seven", "eight",
         "nine", "ten", "eleven", "twelve")
  if (!is.na(k) && k >= 1 && k <= 12) w[k] else as.character(k)
}

fraction_word <- function(k) {
  w <- c("2" = "halves", "3" = "thirds", "4" = "quarters", "5" = "fifths",
         "6" = "sixths", "7" = "sevenths", "8" = "eighths", "9" = "ninths",
         "10" = "tenths")
  w[as.character(k)] %||% paste0("1/", k, "ths")
  if (is.na(w[as.character(k)])) paste0("1/", k, "ths") else unname(w[as.character(k)])
}

readability_phrase <- function(k) {
  switch(readability_rating(k),
    excellent = sprintf("%s ordered colours remain easy to distinguish", num_word(k)),
    good = sprintf("%s ordered colours are still readable, with care in the palette", num_word(k)),
    fair = sprintf("%s ordered colours are at the limit of what readers separate reliably", num_word(k)),
    poor = sprintf("%s ordered colours are more than most readers can separate", num_word(k))
  )
}

eligible_rows <- function(tab, th, need_exact = TRUE) {
  ok <- !is.na(tab$fit) & tab$empty_classes == 0 & tab$smallest_class_n >= th$tiny_n
  expected <- tab$expected_classes %||% tab$n
  if (need_exact) ok <- ok & tab$actual_classes == expected
  ok
}

no_candidate <- function(tab, why) {
  list(suggested = NA_integer_, reasons = why, consider = NULL)
}

recommend_variance <- function(tab, dist, extra_reasons = character(0)) {
  th <- guide_thresholds()
  ok <- eligible_rows(tab, th)
  if (!any(ok)) {
    return(no_candidate(tab, c(
      "no candidate produced the requested classes with every class populated",
      "the data may have too few distinct values for interval classing; see the table"
    )))
  }
  next_gain <- c(tab$fit[-1] - tab$fit[-nrow(tab)], NA)
  flat <- ok & tab$fit >= th$fit_min & (is.na(next_gain) | next_gain < th$gain_min)
  reasons <- character(0)

  if (any(flat)) {
    i <- which(flat)[1]
  } else {
    # fit never reaches the floor, or keeps climbing: the best readable one
    cand <- which(ok & tab$actual_classes <= th$max_readable)
    if (!length(cand)) cand <- which(ok)
    i <- cand[which.max(tab$fit[cand])]
    reasons <- c(reasons, if (tab$fit[i] < th$fit_min)
      sprintf("no candidate reaches a fit of %.2f; %d classes is the best readable option (fit %.2f)",
              th$fit_min, tab$n[i], tab$fit[i])
      else sprintf("fit keeps improving through %d classes; more would cost readability", tab$n[i]))
  }

  # Thin class: step down if it costs little fit.
  if (tab$smallest_class_pct[i] < th$thin_pct && i > 1L && ok[i - 1L] &&
      tab$fit[i] - tab$fit[i - 1L] < th$gain_min) {
    reasons <- c(reasons, sprintf(
      "%d classes would leave one class with only %s observations (%.1f%%); %d loses just %.2f of fit",
      tab$n[i], fmt_n(tab$smallest_class_n[i]), tab$smallest_class_pct[i],
      tab$n[i - 1L], tab$fit[i] - tab$fit[i - 1L]))
    i <- i - 1L
  }
  # Readability: never suggest more than 7 when a readable solution is close.
  if (tab$actual_classes[i] > th$max_readable) {
    alt <- which(ok & tab$actual_classes <= th$max_readable)
    if (length(alt) && tab$fit[i] - max(tab$fit[alt]) < th$gain_min) {
      j <- alt[which.max(tab$fit[alt])]
      reasons <- c(reasons, sprintf(
        "%d classes would fit marginally better (%.2f vs %.2f) but exceed what readers separate",
        tab$n[i], tab$fit[i], tab$fit[j]))
      i <- j
    }
  }

  s <- tab$n[i]
  if (i > 1L) {
    reasons <- c(reasons, sprintf("classification fit improves substantially through %d classes (%.2f)", s, tab$fit[i]))
  } else {
    reasons <- c(reasons, sprintf("classification fit is already %.2f at %d classes", tab$fit[i], s))
  }
  if (!is.na(next_gain[i])) {
    reasons <- c(reasons, sprintf("the gain from %d to %d classes is %s (%.2f)",
                                  s, tab$n[i + 1L], if (next_gain[i] < th$gain_min) "small" else "still noticeable", next_gain[i]))
  }
  reasons <- c(reasons, if (tab$smallest_class_pct[i] >= th$thin_pct)
    sprintf("all %s classes hold enough observations (smallest: %s, %.1f%%)",
            num_word(s), fmt_n(tab$smallest_class_n[i]), tab$smallest_class_pct[i])
    else sprintf("the smallest class holds only %s observations (%.1f%%); it will be a rare colour on the map",
                 fmt_n(tab$smallest_class_n[i]), tab$smallest_class_pct[i]))
  reasons <- c(reasons, readability_phrase(tab$actual_classes[i]))
  if (s <= 3L) reasons <- c(reasons, "the data support only broad distinctions at this fit; fewer classes is a legitimate choice")

  consider <- NULL
  if (i < nrow(tab) && ok[i + 1L] && !is.na(next_gain[i]) &&
      next_gain[i] >= th$consider_gain && tab$actual_classes[i + 1L] <= th$max_readable) {
    consider <- sprintf("Consider %d if finer local variation is substantively important (fit %.2f).",
                        tab$n[i + 1L], tab$fit[i + 1L])
  }
  list(suggested = as.integer(s), reasons = c(extra_reasons, reasons), consider = consider)
}

recommend_quantile <- function(tab, dist) {
  th <- guide_thresholds()
  ok <- eligible_rows(tab, th)
  close <- !is.na(tab$spacing_pct) & tab$spacing_pct < th$close_pct
  clean <- ok & !close
  reasons <- "quantile classes hold equal counts by construction, so fit is not the deciding criterion"
  if (!any(ok)) {
    return(no_candidate(tab, c(reasons,
      "no candidate produced the requested classes: the variable has too many tied values for quantiles; consider `unique` or `as_count`")))
  }
  pref <- which(clean & tab$n %in% th$preferred)
  i <- if (any(clean & tab$n == 5L)) which(clean & tab$n == 5L)
       else if (length(pref)) pref[which.min(abs(tab$n[pref] - 5L))]
       else { c2 <- which(clean); if (length(c2)) c2[which.min(abs(tab$n[c2] - 5L))] else which(ok)[1] }
  s <- tab$n[i]
  reasons <- c(reasons, sprintf("%s classes reads as %s of the distribution, which is how a quantile legend is understood",
                                num_word(s), fraction_word(s)))
  reasons <- c(reasons, sprintf("its breaks are well separated (narrowest gap %.1f%% of the range)", tab$spacing_pct[i]))
  if (any(close)) {
    j <- which(close)[1]
    reasons <- c(reasons, sprintf("at %d classes two adjacent breaks are only %s apart (%.1f%% of the range): the legend would draw a distinction the data cannot support",
                                  tab$n[j], format(signif(tab$min_spacing[j], 3)), tab$spacing_pct[j]))
  }
  reasons <- c(reasons, readability_phrase(tab$actual_classes[i]),
               sprintf("descriptive fit at %d classes is %.2f; more classes add contrast, not accuracy", s, tab$fit[i]))
  consider <- NULL
  if (i < nrow(tab) && clean[i + 1L] && tab$n[i + 1L] <= th$max_readable) {
    consider <- sprintf("Consider %d if you want more contrast in the middle of the distribution.", tab$n[i + 1L])
  }
  list(suggested = as.integer(s), reasons = reasons, consider = consider)
}

recommend_range <- function(tab, dist, style) {
  th <- guide_thresholds()
  ok <- eligible_rows(tab, th, need_exact = !style %in% c("pretty", "log"))
  thin_n <- vapply(seq_len(nrow(tab)), function(i) {
    if (is.na(tab$fit[i])) return(NA_integer_)
    w <- tab$warnings[i]
    as.integer(sum(as.numeric(c(
      if (grepl("thin class", w)) sub(".*?(\\d+) thin class.*", "\\1", w) else 0,
      if (grepl("fewer than", w)) sub(".*?(\\d+) class(es)? with fewer.*", "\\1", w) else 0
    ))))
  }, integer(1))
  extra <- character(0)
  if (!is.na(dist$skewness) && abs(dist$skewness) >= 1) {
    extra <- sprintf("the distribution is %s (skewness %.1f); equal-width classes follow the range, so most of them cover the tail where few areas sit",
                     dist$description, dist$skewness)
  }
  # Prefer candidates with no thin classes at all; among those, the variance rule.
  clean <- ok & !is.na(thin_n) & thin_n == 0
  if (any(clean)) {
    sub_tab <- tab; sub_tab$empty_classes[!clean] <- 1L  # make the others ineligible
    rec <- recommend_variance(sub_tab, dist, extra_reasons = extra)
  } else if (any(ok)) {
    i <- which(ok)[which.min(thin_n[ok] / tab$actual_classes[ok])]
    rec <- list(suggested = as.integer(tab$n[i]), consider = NULL, reasons = c(extra,
      sprintf("every candidate has thin classes; %d classes wastes the smallest share of the palette (%d of %d classes under %g%%)",
              tab$n[i], thin_n[i], tab$actual_classes[i], th$thin_pct),
      readability_phrase(tab$actual_classes[i])))
  } else {
    rec <- no_candidate(tab, c(extra, "every candidate leaves at least one class empty or nearly so"))
  }
  if (!is.na(dist$skewness) && abs(dist$skewness) >= 1 && style %in% c("equal", "pretty")) {
    rec$reasons <- c(rec$reasons, "for a skewed variable, `quantile` or `fisher` would populate every class; `log` keeps equal widths on a log scale")
  }
  rec
}

recommend_self <- function(tab, dist, style, v, args) {
  k <- tab$actual_classes[1]
  reasons <- switch(style,
    headtails = {
      thr <- args$thr %||% 0.4
      tr <- headtails_trace(v, thr)
      c(sprintf("head/tails breaks split at the mean while the head (values above the mean) holds no more than %.0f%% of the values", 100 * thr),
        vapply(seq_len(nrow(tr)), function(i) sprintf("split %d at %s: %.0f%% of the remaining values lie above it",
                                                       i, format(signif(tr$mean[i], 4), big.mark = ","), 100 * tr$head_prop[i]), character(1)),
        sprintf("%s split%s give%s %d classes; `n` is not part of this method, `thr` is",
                num_word(nrow(tr)), if (nrow(tr) > 1) "s" else "", if (nrow(tr) > 1) "" else "s", k),
        readability_phrase(k))
    },
    box = c("boxplot breaks are the median, the hinges and the 1.5 IQR fences: six classes by definition, fewer if fences fall outside the data",
            sprintf("%d classes were formed", k), readability_phrase(k)),
    dpih = c("the class count follows the plug-in histogram bandwidth, which is a property of the data, not a choice",
             sprintf("%d classes were formed", k), readability_phrase(k))
  )
  if (!is.na(tab$smallest_class_pct[1]) && tab$smallest_class_pct[1] < guide_thresholds()$thin_pct) {
    reasons <- c(reasons, sprintf("the smallest class holds %s observations (%.1f%%); with a heavy tail that is expected",
                                  fmt_n(tab$smallest_class_n[1]), tab$smallest_class_pct[1]))
  }
  list(suggested = as.integer(k), reasons = reasons, consider = NULL)
}

# Replays the head/tails recursion so its stopping rule can be explained.
headtails_trace <- function(v, thr = 0.4) {
  out <- list()
  x <- v
  repeat {
    m <- mean(x)
    head <- x[x > m]
    prop <- length(head) / length(x)
    out[[length(out) + 1L]] <- data.frame(mean = m, head_prop = prop)
    if (!(prop <= thr && length(head) > 1L)) break
    x <- head
  }
  do.call(rbind, out)
}

recommend_sd <- function(tab, dist, v, args) {
  th <- guide_thresholds()
  ok <- !is.na(tab$fit) & tab$empty_classes == 0
  step <- args$step %||% 1
  beyond2 <- if (is.finite(dist$sd) && dist$sd > 0) 100 * mean(abs(v - dist$mean) > 2 * dist$sd) else NA
  reasons <- c(
    sprintf("classes are %g-sd bands either side of the mean (%s); fit is not the criterion, distance from the mean is",
            step, format(signif(dist$mean, 4), big.mark = ",")),
    sprintf("%.1f%% of values lie beyond two standard deviations", beyond2)
  )
  if (!is.na(dist$skewness) && abs(dist$skewness) >= 1) {
    reasons <- c(reasons, sprintf("the distribution is %s (skewness %.1f); sd classes assume rough symmetry, so the classes on one side of the mean will be sparse",
                                  dist$description, dist$skewness))
  }
  if (!any(ok)) return(no_candidate(tab, c(reasons, "every candidate leaves an outer band empty")))
  cand <- which(ok & tab$actual_classes == tab$n & tab$n <= 6L)
  if (!length(cand)) cand <- which(ok)
  i <- cand[which.max(tab$n[cand])]
  s <- tab$n[i]
  reasons <- c(reasons,
               sprintf("%d is the largest candidate up to 6 whose outer bands both contain values", s),
               readability_phrase(tab$actual_classes[i]))
  list(suggested = as.integer(s), reasons = reasons, consider = NULL)
}

recommend_fixed <- function(tab, dist) {
  list(suggested = NA_integer_, consider = NULL, reasons = c(
    "classes are defined by the thresholds you supplied, so there is no number of classes to choose",
    sprintf("the supplied breaks form %d classes; fit %.2f, smallest class %s (%.1f%%)",
            tab$actual_classes[1], tab$fit[1], fmt_n(tab$smallest_class_n[1]), tab$smallest_class_pct[1]),
    if (!is.na(tab$empty_classes[1]) && tab$empty_classes[1] > 0)
      sprintf("%d class%s contain no observations: a legend entry with no area", tab$empty_classes[1], if (tab$empty_classes[1] > 1) "es" else ""),
    readability_phrase(tab$actual_classes[1])
  ))
}

recommend_unique <- function(tab, dist) {
  k <- dist$n_distinct
  if (k <= 9L) {
    list(suggested = as.integer(k), consider = NULL, reasons = c(
      sprintf("the variable takes %d distinct values, so one class per value is honest and complete", k),
      readability_phrase(k)))
  } else {
    list(suggested = NA_integer_, consider = NULL, reasons = c(
      sprintf("the variable takes %d distinct values; one class per value is unreadable", k),
      "use an interval style (`fisher`, `quantile`) or `as_count = TRUE` with a quantile style"))
  }
}

# ---------------------------------------------------------------------------
# Methods
# ---------------------------------------------------------------------------

#' @export
print.ggcarto_class_guide <- function(x, ...) {
  d <- x$distribution
  cat("ggcarto class guide\n\n")
  cat("Method: ", x$style_label, "\n", sep = "")
  vals <- paste0("Values: ", fmt_n(d$n))
  if (d$n_missing) vals <- paste0(vals, " (", fmt_n(d$n_missing), " missing)")
  cat(vals, "\n", sep = "")
  cat("Distribution: ", d$description,
      if (!is.na(d$skewness)) sprintf(" (skewness %.1f)", d$skewness) else "",
      "\n\n", sep = "")
  if (is.na(x$suggested)) {
    cat("Suggested: no number of classes", if (x$family == "fixed") " (fixed breaks)" else "", "\n\n", sep = "")
  } else if (isTRUE(x$mirrored)) {
    tot <- x$table$actual_classes[x$table$n == x$suggested]
    cat("Suggested: ", x$suggested, " classes per side (", tot, " in all, mirrored)\n\n", sep = "")
  } else {
    cat("Suggested: ", x$suggested, " classes\n\n", sep = "")
  }
  cat("Why:\n")
  cat(paste0("  ", bullet(), " ", x$reasons), sep = "\n")
  if (!is.null(x$consider)) cat("\n", x$consider, "\n", sep = "")
  cat("\n")
  print_guide_table(x)
  cat("\nA suggestion, not an optimum. summary() shows every metric.\n")
  invisible(x)
}

print_guide_table <- function(x) {
  t <- x$table
  flag <- vapply(t$warnings, function(w) {
    f <- character(0)
    if (grepl("empty", w)) f <- c(f, "empty")
    if (grepl("fewer than", w)) f <- c(f, "tiny")
    if (grepl("thin", w)) f <- c(f, "thin")
    if (grepl("apart", w)) f <- c(f, "close")
    if (grepl("could not", w)) f <- c(f, "failed")
    paste(f, collapse = ",")
  }, character(1), USE.NAMES = FALSE)
  mark <- ifelse(!is.na(x$suggested) & t$n == x$suggested, "<--", "")
  cols <- list(
    n = c("n", t$n),
    classes = c("classes", ifelse(is.na(t$actual_classes), "-", t$actual_classes)),
    fit = c("fit", ifelse(is.na(t$fit), "-", sprintf("%.2f", t$fit))),
    gain = c("gain", ifelse(is.na(t$gain), "-", sprintf("%.2f", t$gain))),
    small = c("smallest class", ifelse(is.na(t$smallest_class_pct), "-",
                                       paste0(sprintf("%.1f", t$smallest_class_pct), "%"))),
    read = c("readability", ifelse(is.na(t$readability), "-", t$readability)),
    flag = c("", flag),
    mark = c("", mark)
  )
  right <- c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE)
  cols <- Map(function(v, r) format(v, justify = if (r) "right" else "left"), cols, right)
  lines <- sub("\\s+$", "", do.call(paste, c(cols, sep = "  ")))
  cat(paste0("  ", lines), sep = "\n")
}

#' @export
summary.ggcarto_class_guide <- function(object, ...) {
  t <- object$table
  t$suggested <- !is.na(object$suggested) & t$n == object$suggested
  t
}

#' @export
#' @rdname class_guide
#' @param type For `plot()`: `"fit"` shows fit and gain against `n`;
#'   `"balance"` shows the share of observations in every class at every `n`.
plot.ggcarto_class_guide <- function(x, type = c("fit", "balance"), ...) {
  type <- match.arg(type)
  t <- x$table
  t <- t[!is.na(t$fit), , drop = FALSE]
  if (!nrow(t)) abort_ggcarto("Nothing to plot: no candidate could be classified.")
  sug <- x$suggested
  k <- ggcarto_colours()
  if (type == "fit") {
    long <- rbind(
      data.frame(n = t$n, metric = "fit", value = t$fit),
      data.frame(n = t$n, metric = "gain", value = t$gain)
    )
    long$metric <- factor(long$metric, levels = c("fit", "gain"))
    long$suggested <- !is.na(sug) & long$n == sug
    p <- ggplot2::ggplot(long, ggplot2::aes(x = .data$n, y = .data$value)) +
      ggplot2::geom_hline(
        data = data.frame(metric = factor(c("fit", "gain"), levels = c("fit", "gain")),
                          y = c(x$thresholds$fit_min, x$thresholds$gain_min)),
        ggplot2::aes(yintercept = .data$y), linetype = "dotted", colour = k[["muted"]]
      ) +
      ggplot2::geom_col(data = long[long$metric == "gain" & !is.na(long$value), ],
                        fill = k[["slate"]], width = 0.55) +
      ggplot2::geom_line(data = long[long$metric == "fit", ], colour = k[["slate"]],
                         linewidth = 0.6) +
      ggplot2::geom_point(
        data = long[long$metric == "fit", ],
        ggplot2::aes(fill = .data$suggested), shape = 21, size = 3.2,
        colour = k[["slate"]], stroke = 0.7
      ) +
      ggplot2::scale_fill_manual(values = c(`TRUE` = k[["accent"]], `FALSE` = k[["paper"]]),
                                 guide = "none") +
      ggplot2::scale_x_continuous(breaks = t$n) +
      ggplot2::facet_wrap(~ metric, ncol = 1, scales = "free_y") +
      ggplot2::labs(x = "number of classes", y = NULL,
                    title = paste0(x$style_label, ": fit by number of classes"),
                    subtitle = if (!is.na(sug)) paste0("Suggested: ", sug, " classes. Dotted lines are the rule thresholds.") else NULL) +
      theme_ggcarto(axes = TRUE) +
      ggplot2::theme(plot.title = ggplot2::element_text(size = ggplot2::rel(1.1)))
    if (!is.na(sug)) {
      p <- p + ggplot2::geom_vline(xintercept = sug, colour = k[["accent"]],
                                   linetype = "dashed", linewidth = 0.4)
    }
    return(p)
  }

  # balance: one stacked bar per candidate
  shares <- do.call(rbind, lapply(as.character(t$n), function(k2) {
    cnt <- x$counts[[k2]]
    data.frame(n = as.integer(k2), class = seq_along(cnt), pct = 100 * cnt / sum(cnt))
  }))
  shares$suggested <- !is.na(sug) & shares$n == sug
  p <- ggplot2::ggplot(shares, ggplot2::aes(x = factor(.data$n), y = .data$pct,
                                            group = .data$class)) +
    ggplot2::geom_col(ggplot2::aes(fill = .data$class), colour = k[["paper"]],
                      linewidth = 0.5, width = 0.7,
                      position = ggplot2::position_stack(reverse = TRUE)) +
    ggplot2::scale_fill_gradient(low = "#d6dde8", high = k[["ink"]], guide = "none") +
    ggplot2::geom_hline(yintercept = x$thresholds$thin_pct, linetype = "dotted",
                        colour = k[["muted"]]) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.08))) +
    ggplot2::labs(x = "number of classes", y = "share of observations (%)",
                  title = paste0(x$style_label, ": class balance"),
                  subtitle = "Each bar is one class, lowest values at the bottom.") +
    theme_ggcarto(axes = TRUE) +
    ggplot2::theme(plot.title = ggplot2::element_text(size = ggplot2::rel(1.1)),
                   panel.grid.major.y = ggplot2::element_blank())
  if (!is.na(sug)) {
    p <- p + ggplot2::annotate("text", x = factor(sug, levels = levels(factor(shares$n))),
                               y = 101, label = "suggested", vjust = 0, size = 3,
                               colour = k[["accent"]], fontface = "bold")
  }
  p
}
