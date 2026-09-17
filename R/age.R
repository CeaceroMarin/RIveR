# Assessment of age as a continuous covariate ----------------------------
# RIveR v0.12.11
#
# Principles:
# - age is treated first as a continuous covariate, not as a category;
# - statistical significance alone does not justify a change in the RI;
# - effect magnitude, improvement in fit, model diagnostics, and
#   the shape of change across age are considered together;
# - rpart only proposes candidate cut-points;
# - a discrete partition is recommended only if the pattern is step-like and
# the segment-specific RIs pass direct partition validation.
#
# The approach follows RefValAdv for covariates: model the mean and
# dispersion across the covariate, transform when needed, and validate the model
# using the residuals. GAMLSS is used as a flexible engine to obtain
# the computed percentile curves and P50.

age_make_bins <- function(age, value, max_bins = 18, min_per_bin = NULL, reference_design = NULL) {
  design <- normalize_reference_design(reference_design)
  ok <- is.finite(age) & is.finite(value)
  age <- age[ok]; value <- value[ok]
  n <- length(age)
  if (n < 120 || length(unique(age)) < 10) return(NULL)
  if (is.null(min_per_bin)) min_per_bin <- max(25L, floor(n / 18))
  probs <- seq(0, 1, length.out = min(max_bins + 1L, floor(n / min_per_bin) + 1L))
  br <- unique(as.numeric(stats::quantile(age, probs, na.rm = TRUE, type = 7)))
  if (length(br) < 5) return(NULL)
  bin <- cut(age, breaks = br, include.lowest = TRUE, labels = FALSE)
  spl <- split(seq_along(age), bin)
  rows <- lapply(spl, function(idx) {
    if (length(idx) < max(15L, floor(min_per_bin * .60))) return(NULL)
    data.frame(
      age_min = min(age[idx]), age_max = max(age[idx]),
      age_mid = stats::median(age[idx]), n = length(idx),
      p025 = as.numeric(stats::quantile(value[idx], design$pair_percentiles[["lower"]], names = FALSE, type = 6)),
      p50 = stats::median(value[idx]),
      p975 = as.numeric(stats::quantile(value[idx], design$pair_percentiles[["upper"]], names = FALSE, type = 6)),
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  if (is.null(out) || nrow(out) < 4) return(NULL)
  rownames(out) <- NULL
  out
}

age_gamlss_family <- function(y) {
  if (all(y > 0, na.rm = TRUE)) "BCCG" else "NO"
}

# Prediction of GAMLSS model parameters and explicit construction of
# percentiles. In v0.12.2, centiles.pred() was a single point of failure: the model
# could be fitted, but in some environments it did not return percentiles. Since
# v0.12.3+, the primary pathway is predict(..., what=...) plus the quantile function of the
# family (qBCCG/qNO). centiles.pred() remains only as a secondary fallback.
age_predict_parameter <- function(fit, what, grid, original_data = NULL) {
  nd <- data.frame(age = as.numeric(grid))
  pred <- tryCatch({
    ans <- suppressWarnings(suppressMessages(
      stats::predict(fit, what = what, newdata = nd, type = "response", data = original_data)
    ))
    list(ok = TRUE, value = suppressWarnings(as.numeric(ans)), reason = NULL)
  }, error = function(e) {
    list(ok = FALSE, value = rep(NA_real_, length(grid)), reason = conditionMessage(e))
  })
  if (!isTRUE(pred$ok)) return(pred)
  if (length(pred$value) != length(grid) || any(!is.finite(pred$value))) {
    return(list(ok = FALSE, value = rep(NA_real_, length(grid)),
                reason = paste0("Incomplete GAMLSS prediction for parameter ", what, ".")))
  }
  pred
}

age_gamlss_curves_from_parameters <- function(fit, family, grid, original_data = NULL, reference_design = NULL) {
  design <- normalize_reference_design(reference_design)
  mu <- age_predict_parameter(fit, "mu", grid, original_data)
  sigma <- age_predict_parameter(fit, "sigma", grid, original_data)
  if (!isTRUE(mu$ok) || !isTRUE(sigma$ok)) {
    return(list(ok = FALSE, reason = paste(na.omit(c(mu$reason, sigma$reason)), collapse = " ")))
  }
  if (any(sigma$value <= 0)) {
    return(list(ok = FALSE, reason = "The GAMLSS model produced non-positive sigma values."))
  }

  pars <- list(mu = mu$value, sigma = sigma$value)
  if (identical(family, "BCCG")) {
    nu <- age_predict_parameter(fit, "nu", grid, original_data)
    if (!isTRUE(nu$ok)) return(list(ok = FALSE, reason = nu$reason))
    pars$nu <- nu$value
  }

  qfun <- tryCatch(get(paste0("q", family), envir = asNamespace("gamlss.dist")),
                   error = function(e) NULL)
  if (is.null(qfun)) {
    return(list(ok = FALSE, reason = paste0("The quantile function q", family, " could not be found in gamlss.dist.")))
  }

  qcalc <- function(prob) {
    args <- c(list(p = prob), pars)
    out <- tryCatch(suppressWarnings(do.call(qfun, args)), error = function(e) NULL)
    out <- suppressWarnings(as.numeric(out))
    if (is.null(out) || length(out) != length(grid) || any(!is.finite(out))) return(NULL)
    out
  }
  p025 <- qcalc(design$pair_percentiles[["lower"]]); p50 <- qcalc(.50); p975 <- qcalc(design$pair_percentiles[["upper"]])
  if (is.null(p025) || is.null(p50) || is.null(p975)) {
    return(list(ok = FALSE, reason = "Could not calculate the lower percentile, P50, and upper percentile defined by the reference design from the GAMLSS parameters."))
  }
  if (any(p025 > p50 | p50 > p975)) {
    return(list(ok = FALSE, reason = "The calculated GAMLSS percentiles do not preserve the order lower limit ≤ P50 ≤ upper limit."))
  }

  list(ok = TRUE,
       curves = data.frame(age = grid, p025 = p025, p50 = p50, p975 = p975,
                           stringsAsFactors = FALSE),
       parameters = pars,
       source = "GAMLSS parameters + family quantile function",
       reason = NULL)
}

age_gamlss_curves_centiles_fallback <- function(fit, grid, reference_design = NULL) {
  design <- normalize_reference_design(reference_design)
  cent <- tryCatch(
    suppressWarnings(suppressMessages(
      gamlss::centiles.pred(fit, xname = "age", xvalues = grid,
                           cent = c(100*design$pair_percentiles[["lower"]], 50, 100*design$pair_percentiles[["upper"]]), plot = FALSE)
    )), error = function(e) NULL)
  if (is.null(cent)) return(list(ok = FALSE, reason = "centiles.pred() also did not return percentiles."))
  cent <- tryCatch(as.data.frame(cent), error = function(e) NULL)
  if (is.null(cent) || ncol(cent) < 4 || nrow(cent) != length(grid)) {
    return(list(ok = FALSE, reason = "Incomplete output from centiles.pred()."))
  }
  out <- cent[, 1:4, drop = FALSE]
  names(out) <- c("age", "p025", "p50", "p975")
  for (nm in names(out)) out[[nm]] <- suppressWarnings(as.numeric(out[[nm]]))
  if (any(!is.finite(as.matrix(out))) || any(out$p025 > out$p50 | out$p50 > out$p975)) {
    return(list(ok = FALSE, reason = "Invalid percentiles returned by centiles.pred()."))
  }
  list(ok = TRUE, curves = out, parameters = NULL,
       source = "centiles.pred() (secondary fallback)", reason = NULL)
}

fit_direct_age_gamlss <- function(data, reference_design = NULL) {
  design <- normalize_reference_design(reference_design)
  if (!pkg_available("gamlss") || !pkg_available("gamlss.dist")) {
    return(list(ok = FALSE, reason = "The gamlss and gamlss.dist packages are required."))
  }
  d <- data.frame(y = data$value, age = data$age)
  d <- d[is.finite(d$y) & is.finite(d$age), , drop = FALSE]
  if (nrow(d) < 120 || length(unique(d$age)) < 10 || diff(range(d$age)) < 5) {
    return(list(ok = FALSE, reason = "There are insufficient data or age range to fit a continuous model."))
  }

  fam <- age_gamlss_family(d$y)
  fits <- list()
  labels <- character(0)
  if (identical(fam, "BCCG")) {
    fits$null <- tryCatch(
      suppressWarnings(suppressMessages(gamlss::gamlss(
        y ~ 1, sigma.formula = ~1, nu.formula = ~1,
        family = gamlss.dist::BCCG, data = d, trace = FALSE))), error = function(e) NULL)
    fits$mean <- tryCatch(
      suppressWarnings(suppressMessages(gamlss::gamlss(
        y ~ gamlss::pb(age), sigma.formula = ~1, nu.formula = ~1,
        family = gamlss.dist::BCCG, data = d, trace = FALSE))), error = function(e) NULL)
    fits$mean_sigma <- tryCatch(
      suppressWarnings(suppressMessages(gamlss::gamlss(
        y ~ gamlss::pb(age), sigma.formula = ~ gamlss::pb(age), nu.formula = ~1,
        family = gamlss.dist::BCCG, data = d, trace = FALSE))), error = function(e) NULL)
    fits$full <- tryCatch(
      suppressWarnings(suppressMessages(gamlss::gamlss(
        y ~ gamlss::pb(age), sigma.formula = ~ gamlss::pb(age),
        nu.formula = ~ gamlss::pb(age), family = gamlss.dist::BCCG,
        data = d, trace = FALSE))), error = function(e) NULL)
    labels <- c(null="No age effect", mean="Age effect on location", mean_sigma="Age effect on location + dispersion", full="Age effect on location + dispersion + shape")
  } else {
    fits$null <- tryCatch(
      suppressWarnings(suppressMessages(gamlss::gamlss(
        y ~ 1, sigma.formula = ~1,
        family = gamlss.dist::NO, data = d, trace = FALSE))), error = function(e) NULL)
    fits$mean <- tryCatch(
      suppressWarnings(suppressMessages(gamlss::gamlss(
        y ~ gamlss::pb(age), sigma.formula = ~1,
        family = gamlss.dist::NO, data = d, trace = FALSE))), error = function(e) NULL)
    fits$mean_sigma <- tryCatch(
      suppressWarnings(suppressMessages(gamlss::gamlss(
        y ~ gamlss::pb(age), sigma.formula = ~ gamlss::pb(age),
        family = gamlss.dist::NO, data = d, trace = FALSE))), error = function(e) NULL)
    labels <- c(null="No age effect", mean="Age effect on location", mean_sigma="Age effect on location + dispersion")
  }
  fits <- fits[!vapply(fits, is.null, logical(1))]
  if (!"null" %in% names(fits) || length(fits) < 2) {
    return(list(ok = FALSE, reason = "The reference GAMLSS model and at least one age-dependent model could not both be fitted stably."))
  }

  aics <- vapply(fits, function(f) tryCatch(stats::AIC(f), error=function(e) Inf), numeric(1))
  age_names <- setdiff(names(fits), "null")
  finite_age <- age_names[is.finite(aics[age_names])]
  if (!length(finite_age)) return(list(ok = FALSE, reason = "No age-dependent GAMLSS model produced an evaluable AIC."))
  best_name <- finite_age[which.min(aics[finite_age])]
  null_fit <- fits$null
  age_fit <- fits[[best_name]]

  grid <- seq(min(d$age), max(d$age), length.out = 160)
  curve_fit <- age_gamlss_curves_from_parameters(age_fit, fam, grid, original_data = d, reference_design = design)
  if (!isTRUE(curve_fit$ok)) {
    # Secondary fallback for compatibility with versions/environments of GAMLSS.
    fallback <- age_gamlss_curves_centiles_fallback(age_fit, grid, reference_design = design)
    if (!isTRUE(fallback$ok)) {
      return(list(ok = FALSE,
                  reason = paste0("Could not obtain percentiles from the continuous model. Primary pathway: ",
                                  curve_fit$reason %||% "unspecified error", " Fallback: ",
                                  fallback$reason %||% "unspecified error")))
    }
    curve_fit <- fallback
  }
  curves <- curve_fit$curves

  zres <- tryCatch(as.numeric(stats::residuals(age_fit)), error = function(e) rep(NA_real_, nrow(d)))
  fitted_mu <- tryCatch(as.numeric(stats::fitted(age_fit)), error = function(e) rep(NA_real_, nrow(d)))
  if (length(zres) != nrow(d)) zres <- rep(NA_real_, nrow(d))
  if (length(fitted_mu) != nrow(d)) fitted_mu <- rep(NA_real_, nrow(d))

  ad <- anderson_darling_normality(zres)
  rho_age <- suppressWarnings(stats::cor(zres, d$age, method = "spearman", use = "complete.obs"))
  rho_fit <- suppressWarnings(stats::cor(zres, fitted_mu, method = "spearman", use = "complete.obs"))
  rho_abs_age <- suppressWarnings(stats::cor(abs(zres), d$age, method = "spearman", use = "complete.obs"))

  aic_null <- aics["null"]
  aic_age <- aics[best_name]
  aic_gain <- if (is.finite(aic_null) && is.finite(aic_age)) aic_null - aic_age else NA_real_

  list(
    ok = TRUE, family = fam, model_name = best_name,
    model_label = labels[best_name] %||% best_name,
    candidate_aic = aics,
    data = d, null_fit = null_fit, fit = age_fit,
    curves = curves, curve_parameters = curve_fit$parameters %||% NULL,
    curve_source = curve_fit$source %||% "—", residuals = zres, fitted = fitted_mu,
    residual_ad_p = ad$p.value,
    residual_age_rho = rho_age,
    residual_fitted_rho = rho_fit,
    abs_residual_age_rho = rho_abs_age,
    aic_null = aic_null, aic_age = aic_age, aic_gain = aic_gain
  )
}

age_curve_metrics <- function(curves, window_years = 8) {
  if (is.null(curves) || nrow(curves) < 5) return(NULL)
  w <- curves$p975 - curves$p025
  typical_width <- stats::median(w[is.finite(w) & w > 0], na.rm = TRUE)
  ch_low <- diff(range(curves$p025, na.rm = TRUE))
  ch_mid <- diff(range(curves$p50, na.rm = TRUE))
  ch_up <- diff(range(curves$p975, na.rm = TRUE))
  limit_change <- max(ch_low, ch_up, na.rm = TRUE)
  relative_limit_change <- if (is.finite(typical_width) && typical_width > 0) limit_change / typical_width else NA_real_
  relative_median_change <- if (is.finite(typical_width) && typical_width > 0) ch_mid / typical_width else NA_real_

  delta <- abs(diff(curves$p50)) + 0.5 * abs(diff(curves$p025)) + 0.5 * abs(diff(curves$p975))
  delta[!is.finite(delta)] <- 0
  total_delta <- sum(delta)
  step_concentration <- NA_real_
  step_age <- NA_real_
  if (total_delta > 0) {
    dx <- stats::median(diff(curves$age), na.rm = TRUE)
    k <- max(1L, round(window_years / dx))
    k <- min(k, length(delta))
    roll <- vapply(seq_along(delta), function(i) {
      j <- min(length(delta), i + k - 1L)
      sum(delta[i:j])
    }, numeric(1))
    imax <- which.max(roll)
    step_concentration <- max(roll) / total_delta
    jmax <- min(nrow(curves), imax + k)
    step_age <- stats::median(curves$age[imax:jmax])
  }
  list(
    typical_width = typical_width,
    lower_change = ch_low, median_change = ch_mid, upper_change = ch_up,
    relative_limit_change = relative_limit_change,
    relative_median_change = relative_median_change,
    step_concentration = step_concentration, step_age = step_age
  )
}


# The GAMLSS model is deliberately smooth. A true step-like change can
# be attenuated in the modeled curves and, therefore, should not be assessed
# solely from GAMLSS. This metric evaluates the concentration of
# change in empirical percentiles across age bands. It is a diagnostic of
# shape, not a published partitioning criterion.
age_binned_step_metrics <- function(bins, window_years = 8) {
  if (is.null(bins) || nrow(bins) < 5) return(NULL)
  b <- bins[order(bins$age_mid), , drop = FALSE]
  edge_age <- (b$age_mid[-nrow(b)] + b$age_mid[-1]) / 2
  delta <- abs(diff(b$p50)) + 0.5 * abs(diff(b$p025)) + 0.5 * abs(diff(b$p975))
  delta[!is.finite(delta)] <- 0
  total <- sum(delta)
  if (!is.finite(total) || total <= 0 || !length(edge_age)) {
    return(list(step_concentration = NA_real_, step_age = NA_real_, total_change = total))
  }
  roll <- vapply(seq_along(edge_age), function(i) {
    idx <- which(edge_age >= edge_age[i] & edge_age <= edge_age[i] + window_years)
    sum(delta[idx], na.rm = TRUE)
  }, numeric(1))
  ibest <- which.max(roll)
  idxbest <- which(edge_age >= edge_age[ibest] & edge_age <= edge_age[ibest] + window_years)
  if (length(idxbest) && sum(delta[idxbest]) > 0) {
    step_age <- stats::weighted.mean(edge_age[idxbest], w = delta[idxbest], na.rm = TRUE)
  } else step_age <- edge_age[which.max(delta)]
  list(step_concentration = max(roll) / total, step_age = step_age, total_change = total)
}

# -------------------------------------------------------------------------
# Operational adaptation for a LIS that does not support a continuous function
# -------------------------------------------------------------------------
# GAMLSS remains the scientific reference model. The discretization
# does not attempt to demonstrate biological subpopulations; it is an operational approximation
# using age bands. Therefore, Lahti/Harris-Boyd are not used as a mandatory gateway
# in this conversion (and remain relevant when assessing
# a true biological partition). Discretization quality is summarized
# by the maximum deviation of the constant limits from the continuous curve.

age_sil_annual_table <- function(result) {
  c <- result$continuous$curves %||% NULL
  if (is.null(c) || !nrow(c)) return(NULL)
  design <- normalize_reference_design(result$reference_design %||% NULL)
  amin <- min(c$age, na.rm = TRUE); amax <- max(c$age, na.rm = TRUE)
  ages <- seq(ceiling(amin), floor(amax), by = 1)
  if (!length(ages)) ages <- sort(unique(round(c$age, 1)))

  # v1.0.1: the exportable table respects the clinically active tail. The names
  # p025/p975 are historical/internal; in a one-sided design they contain P5/P95
  # (or the percentile defined by the coverage), not P2.5/P97.5.
  out <- data.frame(Age = ages, stringsAsFactors = FALSE, check.names = FALSE)
  if (identical(design$tail, "two_sided")) {
    # Regression safeguard: in two-sided mode, the v1.0.0 interface is retained exactly.
    out$LRL <- stats::approx(c$age, c$p025, xout = ages, rule = 2)$y
    out$URL <- stats::approx(c$age, c$p975, xout = ages, rule = 2)$y
  } else if (isTRUE(design$active[["lower"]])) {
    out[[reference_percentile_label(design$pair_percentiles[["lower"]])]] <-
      stats::approx(c$age, c$p025, xout = ages, rule = 2)$y
  } else if (isTRUE(design$active[["upper"]])) {
    out[[reference_percentile_label(design$pair_percentiles[["upper"]])]] <-
      stats::approx(c$age, c$p975, xout = ages, rule = 2)$y
  }
  out
}


age_binomial_coverage_ci <- function(success, n, expected = 0.95, conf_level = 0.95) {
  success <- as.integer(success); n <- as.integer(n)
  if (!is.finite(success) || !is.finite(n) || n <= 0 || success < 0 || success > n ||
      !is.finite(expected) || expected <= 0 || expected >= 1) {
    return(list(lower = NA_real_, upper = NA_real_, compatible = NA, p_value = NA_real_))
  }
  bt <- tryCatch(
    stats::binom.test(success, n, p = expected, alternative = "two.sided", conf.level = conf_level),
    error = function(e) NULL
  )
  if (is.null(bt)) return(list(lower = NA_real_, upper = NA_real_, compatible = NA, p_value = NA_real_))
  ci <- as.numeric(bt$conf.int[1:2])
  list(
    lower = ci[1], upper = ci[2],
    compatible = expected >= ci[1] && expected <= ci[2],
    p_value = as.numeric(bt$p.value)
  )
}

age_sil_geometry_state <- function(error, tolerance = 0.10, display_digits = 1L) {
  if (!is.finite(error) || !is.finite(tolerance)) return("not_evaluable")
  if (error <= tolerance) return("pass")
  # "Borderline" is used only when the actual value exceeds the threshold but
  # standard table rounding makes it appear equal to the threshold.
  shown_error <- formatC(100 * error, format = "f", digits = display_digits)
  shown_limit <- formatC(100 * tolerance, format = "f", digits = display_digits)
  if (identical(shown_error, shown_limit)) return("borderline")
  "fail"
}

age_sil_overall_state <- function(error, tolerance, coverage_global_ok) {
  gs <- age_sil_geometry_state(error, tolerance)
  if (identical(gs, "pass") && isTRUE(coverage_global_ok)) return("pass")
  if (identical(gs, "borderline") && isTRUE(coverage_global_ok)) return("borderline")
  "fail"
}

age_sil_segment_info <- function(curves, age_values, value_values, i, j, typical_width,
                                 expected_coverage = 0.95, coverage_conf_level = 0.95,
                                 reference_design = NULL) {
  design <- normalize_reference_design(reference_design, coverage = expected_coverage)
  m <- nrow(curves)
  if (i < 1 || j > m || i > j) return(NULL)
  lo_boundary <- if (i == 1) -Inf else mean(curves$age[c(i - 1L, i)])
  hi_boundary <- if (j == m) Inf else mean(curves$age[c(j, j + 1L)])
  idx <- is.finite(age_values) & is.finite(value_values) & age_values >= lo_boundary & age_values < hi_boundary
  low <- curves$p025[i:j]; up <- curves$p975[i:j]
  if (!length(low) || !length(up) || any(!is.finite(c(low, up)))) return(NULL)
  const_low <- (min(low) + max(low)) / 2
  const_up <- (min(up) + max(up)) / 2

  # v1.0.1: geometric error is assessed only for clinically active limits.
  # In two-sided mode, the historical behavior is retained exactly.
  active_errors <- numeric(0)
  if (isTRUE(design$active[["lower"]])) active_errors <- c(active_errors, abs(low - const_low))
  if (isTRUE(design$active[["upper"]])) active_errors <- c(active_errors, abs(up - const_up))
  if (!length(active_errors) || any(!is.finite(active_errors))) return(NULL)
  abs_err <- max(active_errors, na.rm = TRUE)
  rel_err <- if (is.finite(typical_width) && typical_width > 0) abs_err / typical_width else NA_real_

  vals <- value_values[idx]
  inside_n <- if (!length(vals)) 0L else if (identical(design$tail, "lower")) {
    sum(vals >= const_low, na.rm = TRUE)
  } else if (identical(design$tail, "upper")) {
    sum(vals <= const_up, na.rm = TRUE)
  } else {
    sum(vals >= const_low & vals <= const_up, na.rm = TRUE)
  }
  coverage <- if (length(vals)) inside_n / length(vals) else NA_real_
  coverage_ci <- age_binomial_coverage_ci(inside_n, length(vals), expected = expected_coverage,
                                         conf_level = coverage_conf_level)
  list(
    i = i, j = j, n = sum(idx), lower_boundary = lo_boundary, upper_boundary = hi_boundary,
    age_min = if (any(idx)) min(age_values[idx], na.rm = TRUE) else curves$age[i],
    age_max = if (any(idx)) max(age_values[idx], na.rm = TRUE) else curves$age[j],
    lrl = const_low, url = const_up, abs_error = abs_err, rel_error = rel_err,
    empirical_coverage = coverage, inside_n = inside_n,
    coverage_ci_lower = coverage_ci$lower, coverage_ci_upper = coverage_ci$upper,
    coverage_compatible = coverage_ci$compatible,
    coverage_p_value = coverage_ci$p_value
  )
}

age_sil_reconstruct_segments <- function(k, m, prev, seg_cache, key) {
  if (!is.finite(k) || k < 1 || k > nrow(prev)) return(NULL)
  segments_idx <- vector("list", k)
  j <- m
  for (kk in k:1) {
    if (kk == 1) i <- 1L else i <- prev[kk, j]
    if (!is.finite(i)) return(NULL)
    segments_idx[[kk]] <- c(i, j)
    j <- i - 1L
  }
  segs <- lapply(segments_idx, function(z) seg_cache[[key(z[1], z[2])]])
  if (any(vapply(segs, is.null, logical(1)))) return(NULL)
  segs
}

age_sil_solution_summary <- function(k, m, dp, prev, seg_cache, key, tolerance,
                                     coverage_conf_level = 0.95) {
  if (!is.finite(dp[k, m])) return(NULL)
  segs <- age_sil_reconstruct_segments(k, m, prev, seg_cache, key)
  if (is.null(segs) || !length(segs)) return(NULL)
  err <- max(vapply(segs, `[[`, numeric(1), "rel_error"), na.rm = TRUE)

  # Exact 95% CIs are retained as an individual diagnostic for each band.
  # The overall coverage decision controls multiplicity: it tests
  # H0: coverage = 95% in each band, with Holm-adjusted p-values.
  cov_individual <- vapply(segs, function(sg) isTRUE(sg$coverage_compatible), logical(1))
  n_cov_individual <- sum(cov_individual)
  p_raw <- vapply(segs, function(sg) as.numeric(sg$coverage_p_value %||% NA_real_), numeric(1))
  p_holm <- rep(NA_real_, length(p_raw))
  finite_p <- is.finite(p_raw)
  if (any(finite_p)) p_holm[finite_p] <- stats::p.adjust(p_raw[finite_p], method = "holm")
  alpha <- 1 - coverage_conf_level
  global_ok <- length(p_holm) == length(segs) && all(is.finite(p_holm)) && all(p_holm >= alpha)

  for (ii in seq_along(segs)) {
    segs[[ii]]$coverage_p_holm <- p_holm[ii]
    segs[[ii]]$coverage_global_compatible <- is.finite(p_holm[ii]) && p_holm[ii] >= alpha
  }

  overall_state <- age_sil_overall_state(err, tolerance, global_ok)
  list(
    k = k, segments = segs, overall_error = err,
    all_coverage_compatible = all(cov_individual),
    n_coverage_compatible = n_cov_individual,
    coverage_global_compatible = global_ok,
    min_coverage_p_holm = if (all(is.finite(p_holm))) min(p_holm) else NA_real_,
    geometry_state = age_sil_geometry_state(err, tolerance),
    overall_state = overall_state,
    acceptable = identical(overall_state, "pass")
  )
}

age_sil_discrete_adaptation <- function(result, tolerance = 0.10, max_groups = 10L, min_per_band = 50L,
                                        expected_coverage = NULL, coverage_conf_level = 0.95) {
  design <- normalize_reference_design(result$reference_design %||% NULL)
  if (is.null(expected_coverage)) expected_coverage <- design$coverage
  curves <- result$continuous$curves %||% NULL
  md <- result$model$data %||% NULL
  if (is.null(curves) || nrow(curves) < 10 || is.null(md) || !all(c("age", "y") %in% names(md))) {
    return(list(ok = FALSE, status = "grey", reason = "There is no complete continuous curve from which to derive discrete bands."))
  }
  age_values <- as.numeric(md$age); value_values <- as.numeric(md$y)
  typical_width <- stats::median(curves$p975 - curves$p025, na.rm = TRUE)
  if (!is.finite(typical_width) || typical_width <= 0) {
    return(list(ok = FALSE, status = "grey", reason = "Could not define a typical width for the continuous RI."))
  }
  max_groups <- max(2L, as.integer(max_groups))
  min_per_band <- max(20L, as.integer(min_per_band))
  m <- nrow(curves)

  # Geometric cost of each segment: maximum relative deviation between the limit
  # constant band limit and the continuous curve. Empirical coverage is calculated
  # for each segment as well, but it does not enter the cost function; it is used as a
  # second criterion of validation of each solution complete.
  cost <- matrix(Inf, nrow = m, ncol = m)
  seg_cache <- vector("list", m * m)
  key <- function(i, j) (i - 1L) * m + j
  for (i in seq_len(m)) {
    for (j in i:m) {
      sg <- age_sil_segment_info(
        curves, age_values, value_values, i, j, typical_width,
        expected_coverage = expected_coverage, coverage_conf_level = coverage_conf_level,
        reference_design = design
      )
      if (!is.null(sg) && sg$n >= min_per_band && is.finite(sg$rel_error)) {
        cost[i, j] <- sg$rel_error
        seg_cache[[key(i, j)]] <- sg
      }
    }
  }

  # Programming dynamic minimax: for each number of bands is minimizes the
  # worst geometric error. Each complete solution is then reviewed to determine whether
  # observed coverages are globally compatible with the expected coverage.
  # The exact 95% binomial CI is shown for each band; the joint decision
  # uses exact two-sided binomial tests with Holm adjustment.
  dp <- matrix(Inf, nrow = max_groups, ncol = m)
  prev <- matrix(NA_integer_, nrow = max_groups, ncol = m)
  for (j in seq_len(m)) dp[1, j] <- cost[1, j]
  if (max_groups >= 2) {
    for (k in 2:max_groups) {
      for (j in seq_len(m)) {
        if (j < k) next
        best <- Inf; best_i <- NA_integer_
        for (i in k:j) {
          left <- dp[k - 1L, i - 1L]
          right <- cost[i, j]
          if (!is.finite(left) || !is.finite(right)) next
          val <- max(left, right)
          if (val < best) { best <- val; best_i <- i }
        }
        dp[k, j] <- best; prev[k, j] <- best_i
      }
    }
  }

  solutions <- lapply(2:max_groups, function(k) age_sil_solution_summary(
    k, m, dp, prev, seg_cache, key, tolerance,
    coverage_conf_level = coverage_conf_level
  ))
  names(solutions) <- as.character(2:max_groups)
  valid_solutions <- Filter(Negate(is.null), solutions)
  if (!length(valid_solutions)) {
    return(list(ok = FALSE, status = "yellow",
                reason = "Could not construct bands with a size minimum sufficient for the check operational."))
  }

  acceptable_k <- as.integer(names(valid_solutions)[vapply(valid_solutions, function(z) isTRUE(z$acceptable), logical(1))])
  if (length(acceptable_k)) {
    chosen_k <- min(acceptable_k)
  } else {
    geom_k <- as.integer(names(valid_solutions)[vapply(valid_solutions, function(z) is.finite(z$overall_error) && z$overall_error <= tolerance, logical(1))])
    if (length(geom_k)) {
      cand <- valid_solutions[as.character(geom_k)]
      min_p <- vapply(cand, function(z) as.numeric(z$min_coverage_p_holm %||% NA_real_), numeric(1))
      if (any(is.finite(min_p))) {
        best_p <- max(min_p[is.finite(min_p)])
        chosen_k <- min(geom_k[is.finite(min_p) & min_p == best_p])
      } else {
        cov_n <- vapply(cand, `[[`, numeric(1), "n_coverage_compatible")
        best_cov <- max(cov_n)
        chosen_k <- min(geom_k[cov_n == best_cov])
      }
    } else {
      errs <- vapply(valid_solutions, `[[`, numeric(1), "overall_error")
      chosen_k <- as.integer(names(valid_solutions)[which.min(errs)])
    }
  }

  chosen <- valid_solutions[[as.character(chosen_k)]]
  segs <- chosen$segments
  if (is.null(segs) || !length(segs)) {
    return(list(ok = FALSE, status = "yellow", reason = "Could not reconstruct the optimal discretization."))
  }

  fmt_age <- function(x) formatC(x, format = "f", digits = 1, decimal.mark = ",")
  rows <- lapply(seq_along(segs), function(ii) {
    sg <- segs[[ii]]
    row <- data.frame(
      Band = paste0(fmt_age(sg$age_min), "–", fmt_age(sg$age_max), " years"),
      n = sg$n,
      stringsAsFactors = FALSE, check.names = FALSE
    )
    if (identical(design$tail, "two_sided")) {
      row$LRL <- sg$lrl
      row$URL <- sg$url
    } else if (isTRUE(design$active[["lower"]])) {
      row[[reference_percentile_label(design$pair_percentiles[["lower"]])]] <- sg$lrl
    } else if (isTRUE(design$active[["upper"]])) {
      row[[reference_percentile_label(design$pair_percentiles[["upper"]])]] <- sg$url
    }
    row[["Error maximum vs model"]] <- sg$rel_error
    row[["Coverage observed"]] <- sg$empirical_coverage
    row[["95% CI coverage lower"]] <- sg$coverage_ci_lower
    row[["95% CI coverage upper"]] <- sg$coverage_ci_upper
    row[["95% CI includes coverage expected"]] <- isTRUE(sg$coverage_compatible)
    row[["p fitted (Holm)"]] <- sg$coverage_p_holm %||% NA_real_
    row
  })
  tab <- do.call(rbind, rows)
  overall_error <- chosen$overall_error
  coverage_ok <- isTRUE(chosen$coverage_global_compatible)
  acceptable <- isTRUE(chosen$acceptable)
  status <- if (acceptable) "green" else "yellow"

  tradeoff_rows <- lapply(valid_solutions, function(z) {
    data.frame(
      Bands = z$k,
      `Error maximum vs model` = z$overall_error,
      `Geometric status` = z$geometry_state,
      `Bands with 95% CI compatible` = paste0(z$n_coverage_compatible, "/", z$k),
      `p fitted (Holm) minimum` = z$min_coverage_p_holm,
      `Coverage overall compatible (Holm)` = isTRUE(z$coverage_global_compatible),
      `Status` = z$overall_state,
      stringsAsFactors = FALSE, check.names = FALSE
    )
  })
  tradeoff <- do.call(rbind, tradeoff_rows)
  tradeoff <- tradeoff[order(tradeoff$Bands), , drop = FALSE]

  simpler_borderline_k <- as.integer(names(valid_solutions)[vapply(
    valid_solutions,
    function(z) identical(z$overall_state %||% "", "borderline") && z$k < chosen_k,
    logical(1)
  )])
  borderline_note <- ""
  if (length(simpler_borderline_k)) {
    bk <- min(simpler_borderline_k)
    bz <- valid_solutions[[as.character(bk)]]
    borderline_note <- paste0(
      " The ", bk, "-band solution is borderline: actual error ",
      formatC(100 * bz$overall_error, format = "f", digits = 3, decimal.mark = ","),
      " % > ", formatC(100 * tolerance, format = "f", digits = 2, decimal.mark = ","),
      " %, even if rounding to one decimal displays it as 10.0%. ",
      "This is not considered automatic compliance; it should only be adopted through an explicit and justified specialist decision."
    )
  }

  if (acceptable) {
    rec <- paste0(
      "If the LIS only allows age bands, the simplest solution that meets both internal criteria uses ",
      chosen_k, " bands: maximum error of the active limit(s) ", round(100 * overall_error, 1),
      " % of the typical computational width between percentiles (limit ", round(100 * tolerance, 0),
      " %) and overall coverage compatible with the expected ", round(100 * expected_coverage, 1), " % (exact two-sided binomial tests with Holm adjustment; ",
      round(100 * coverage_conf_level, 0), " % confidence level). The individual 95% CIs are shown as a diagnostic for each band. ",
      "The limits are derived from the overall model; they are not independently re-established RIs for each band.",
      borderline_note
    )
  } else if (is.finite(overall_error) && overall_error <= tolerance && !coverage_ok) {
    rec <- paste0(
      "The selected discretization reproduces the GAMLSS geometry with a maximum error of ",
      round(100 * overall_error, 1), " %, but overall coverage is not compatible with the expected ", round(100 * expected_coverage, 1), " % ",
      "after Holm adjustment. Therefore, the solution is not approved solely on the geometric criterion; review the trade-off table before implementing it in the LIS."
    )
  } else {
    rec <- paste0(
      "With a maximum of ", max_groups, " bands, no solution simultaneously met the geometric-error criterion ≤",
      round(100 * tolerance, 0), " % and overall coverage compatibility with the expected ", round(100 * expected_coverage, 1), " % after Holm adjustment. ",
      "An explicit clinical tolerance or another implementation solution should be considered."
    )
  }

  list(
    ok = TRUE, status = status, acceptable = acceptable, tolerance = tolerance,
    expected_coverage = expected_coverage, coverage_conf_level = coverage_conf_level,
    coverage_multiplicity_method = "Holm",
    reference_design = design,
    coverage_rule = switch(design$tail, lower = "y >= LRL", upper = "y <= URL", two_sided = "LRL <= y <= URL"),
    coverage_ok = coverage_ok, max_groups = max_groups, min_per_band = min_per_band,
    n_groups = chosen_k, overall_error = overall_error, table = tab, segments = segs,
    tradeoff_table = tradeoff, annual_table = age_sil_annual_table(result), recommendation = rec,
    note = "Operational discretization derived from GAMLSS. It requires an internal geometric-error criterion and overall coverage compatibility using exact binomial tests with Holm adjustment; individual 95% CIs are retained as diagnostics. Lahti/Harris-Boyd are not mandatory criteria because a biological partition is not being postulated."
  )
}

age_sil_display_table <- function(result) {
  a <- result$sil_adaptation %||% NULL
  tab <- a$table %||% NULL
  if (is.null(tab) || !nrow(tab)) return(NULL)
  out <- tab
  design <- normalize_reference_design(result$reference_design %||% a$reference_design %||% NULL)
  limit_cols <- c(
    if (isTRUE(design$active[["lower"]])) reference_percentile_label(design$pair_percentiles[["lower"]]) else character(0),
    if (isTRUE(design$active[["upper"]])) reference_percentile_label(design$pair_percentiles[["upper"]]) else character(0),
    "LRL", "URL"
  )
  for (nm in intersect(limit_cols, names(out))) out[[nm]] <- signif(out[[nm]], 6)
  if ("Error maximum vs model" %in% names(out)) out[["Error maximum vs model"]] <- vapply(out[["Error maximum vs model"]], format_percent, character(1), digits = 1)
  if ("Coverage observed" %in% names(out)) out[["Coverage observed"]] <- vapply(out[["Coverage observed"]], format_percent, character(1), digits = 1)
  if (all(c("95% CI coverage lower", "95% CI coverage upper") %in% names(out))) {
    lo <- out[["95% CI coverage lower"]]; hi <- out[["95% CI coverage upper"]]
    out[["95% CI coverage"]] <- ifelse(
      is.finite(lo) & is.finite(hi),
      paste0(vapply(lo, format_percent, character(1), digits = 1), "–", vapply(hi, format_percent, character(1), digits = 1)),
      "—"
    )
    out[["95% CI coverage lower"]] <- NULL
    out[["95% CI coverage upper"]] <- NULL
  }
  if ("95% CI includes coverage expected" %in% names(out)) out[["95% CI includes coverage expected"]] <- ifelse(out[["95% CI includes coverage expected"]], "Yes", "No")
  if ("p fitted (Holm)" %in% names(out)) {
    out[["p fitted (Holm)"]] <- ifelse(
      is.finite(out[["p fitted (Holm)"]]),
      formatC(out[["p fitted (Holm)"]], format = "f", digits = 3, decimal.mark = ","),
      "—"
    )
  }
  out
}

age_sil_tradeoff_display_table <- function(result) {
  a <- result$sil_adaptation %||% NULL
  tab <- a$tradeoff_table %||% NULL
  if (is.null(tab) || !nrow(tab)) return(NULL)
  out <- tab
  tol <- a$tolerance %||% 0.10

  if ("Error maximum vs model" %in% names(out)) {
    raw_err <- out[["Error maximum vs model"]]
    gs <- if ("Geometric status" %in% names(out)) out[["Geometric status"]] else
      vapply(raw_err, age_sil_geometry_state, character(1), tolerance = tol)
    out[["Error maximum vs model"]] <- vapply(seq_along(raw_err), function(i) {
      if (!is.finite(raw_err[i])) return("—")
      if (identical(gs[i], "borderline")) {
        paste0(format_percent(raw_err[i], digits = 3), " (> ", format_percent(tol, digits = 2), ")")
      } else format_percent(raw_err[i], digits = 1)
    }, character(1))
  }

  if ("Geometric status" %in% names(out)) {
    out[["Geometric status"]] <- vapply(out[["Geometric status"]], function(z) {
      switch(as.character(z),
             pass = status_symbol_text("green", "Meets"),
             borderline = status_symbol_text("yellow", "Borderline"),
             fail = status_symbol_text("red", "Does not meet"),
             status_symbol_text("grey", "Not evaluable"))
    }, character(1))
  }

  if ("p fitted (Holm) minimum" %in% names(out)) {
    out[["p fitted (Holm) minimum"]] <- ifelse(
      is.finite(out[["p fitted (Holm) minimum"]]),
      formatC(out[["p fitted (Holm) minimum"]], format = "f", digits = 3, decimal.mark = ","),
      "—"
    )
  }
  if ("Coverage overall compatible (Holm)" %in% names(out)) {
    out[["Coverage overall compatible (Holm)"]] <- ifelse(out[["Coverage overall compatible (Holm)"]], "Yes", "No")
  }
  if ("Status" %in% names(out)) {
    out[["Status"]] <- vapply(out[["Status"]], function(z) {
      switch(as.character(z),
             pass = status_symbol_text("green", "Meets"),
             borderline = status_symbol_text("yellow", "Borderline: actual error >10 %"),
             fail = status_symbol_text("red", "Does not meet"),
             status_symbol_text("grey", "Not evaluable"))
    }, character(1))
  }
  out
}

age_candidate_splits_v2 <- function(bins) {
  if (is.null(bins) || nrow(bins) < 6 || !pkg_available("rpart")) return(numeric(0))
  dat <- data.frame(age_mid = bins$age_mid, p50 = bins$p50, n = bins$n)
  fit <- tryCatch(
    rpart::rpart(p50 ~ age_mid, data = dat, weights = n,
                 control = rpart::rpart.control(cp = 0.005, minsplit = 4, maxdepth = 3)),
    error = function(e) NULL)
  if (is.null(fit) || is.null(fit$splits)) return(numeric(0))
  vals <- suppressWarnings(as.numeric(fit$splits[, "index"]))
  sort(unique(vals[is.finite(vals)]))
}

age_pick_step_cut <- function(candidates, target_age, age_range) {
  cand <- sort(unique(candidates[is.finite(candidates)]))
  if (!length(cand)) return(NA_real_)
  margin <- max(2, 0.05 * diff(age_range))
  cand <- cand[cand > age_range[1] + margin & cand < age_range[2] - margin]
  if (!length(cand)) return(NA_real_)
  if (is.finite(target_age)) cand[which.min(abs(cand - target_age))] else stats::median(cand)
}

# rpart defines a candidate zone but does not set the final cut-point. From
# Initial cut-point refinement explores real boundaries between observed ages and selects
# this zone, the cut-point that most reduces robust within-group dispersion relative to the medians is selected.
# This provides a robust location for a level change, not a partition test:
# Lahti, Harris-Boyd, precision, and specialist review are then applied.
age_refine_step_cut <- function(data, seed_cut, target_age = NA_real_, min_n = 120L,
                                search_half_width = NULL) {
  d <- data[is.finite(data$age) & is.finite(data$value), , drop = FALSE]
  if (nrow(d) < 2 * min_n || !is.finite(seed_cut)) {
    return(list(ok = FALSE, seed_cut = seed_cut, selected_cut = seed_cut,
                reason = "There are insufficient data for refining the cut-point."))
  }
  ar <- range(d$age, na.rm = TRUE)
  span <- diff(ar)
  if (is.null(search_half_width) || !is.finite(search_half_width)) search_half_width <- max(3, min(8, 0.08 * span))
  anchors <- c(seed_cut, target_age)
  anchors <- anchors[is.finite(anchors)]
  lo <- max(ar[1], min(anchors) - search_half_width)
  hi <- min(ar[2], max(anchors) + search_half_width)

  ua <- sort(unique(d$age))
  if (length(ua) < 3) return(list(ok = FALSE, seed_cut = seed_cut, selected_cut = seed_cut,
                                 reason = "There are insufficient distinct age values to refine the cut-point."))
  cuts <- (ua[-length(ua)] + ua[-1]) / 2
  cuts <- cuts[cuts >= lo & cuts <= hi]
  if (!length(cuts)) return(list(ok = FALSE, seed_cut = seed_cut, selected_cut = seed_cut,
                                 reason = "No observed boundaries were found within the candidate zone."))

  global_med <- stats::median(d$value, na.rm = TRUE)
  global_sad <- sum(abs(d$value - global_med), na.rm = TRUE)
  global_iqr <- stats::IQR(d$value, na.rm = TRUE)
  rows <- lapply(cuts, function(cc) {
    left <- d$value[d$age <= cc]
    right <- d$value[d$age > cc]
    if (length(left) < min_n || length(right) < min_n) return(NULL)
    ml <- stats::median(left); mr <- stats::median(right)
    within_sad <- sum(abs(left - ml), na.rm = TRUE) + sum(abs(right - mr), na.rm = TRUE)
    gain <- if (is.finite(global_sad) && global_sad > 0) (global_sad - within_sad) / global_sad else NA_real_
    gap <- abs(mr - ml)
    gap_std <- if (is.finite(global_iqr) && global_iqr > 0) gap / global_iqr else NA_real_
    data.frame(
      `Cut-point` = cc, `n left` = length(left), `n right` = length(right),
      `Median left` = ml, `Median right` = mr,
      `Robust improvement` = gain, `Median separation / IQR` = gap_std,
      `Distance to rpart` = abs(cc - seed_cut),
      `Distance to the empirical change` = if (is.finite(target_age)) abs(cc - target_age) else NA_real_,
      stringsAsFactors = FALSE, check.names = FALSE
    )
  })
  tab <- do.call(rbind, Filter(Negate(is.null), rows))
  if (is.null(tab) || !nrow(tab)) {
    return(list(ok = FALSE, seed_cut = seed_cut, selected_cut = seed_cut,
                reason = "No cut-point in the candidate zone retains n≥120 on both sides."))
  }
  target_dist <- tab[["Distance to the empirical change"]]
  target_dist[!is.finite(target_dist)] <- Inf
  ord <- order(-tab[["Robust improvement"]], target_dist, tab[["Distance to rpart"]], tab[["Cut-point"]])
  tab <- tab[ord, , drop = FALSE]
  selected <- tab[["Cut-point"]][1]
  tab$Selected <- tab[["Cut-point"]] == selected
  seed_row <- which.min(abs(tab[["Cut-point"]] - seed_cut))
  improvement <- tab[["Robust improvement"]][1] - tab[["Robust improvement"]][seed_row]
  list(
    ok = TRUE, seed_cut = seed_cut, selected_cut = selected,
    target_age = target_age, search_min = lo, search_max = hi,
    search_half_width = search_half_width, improvement = improvement,
    table = tab
  )
}

age_boundary_outlier_guard <- function(data, cut, candidates, near_years = NULL) {
  empty <- data.frame()
  if (is.null(candidates) || !is.data.frame(candidates) || !nrow(candidates) ||
      !is.finite(cut) || !all(c("age", "value") %in% names(data))) {
    return(list(protected = empty, ordinary = candidates %||% empty, near_years = NA_real_))
  }
  d <- data[is.finite(data$age) & is.finite(data$value), , drop = FALSE]
  if (!nrow(d)) return(list(protected = empty, ordinary = candidates, near_years = NA_real_))
  ar <- range(d$age, na.rm = TRUE)
  if (is.null(near_years) || !is.finite(near_years)) near_years <- max(1, min(3, 0.03 * diff(ar)))

  cc <- candidates
  if (!"row_id" %in% names(cc)) return(list(protected = empty, ordinary = cc, near_years = near_years))
  if (".ri_row_id" %in% names(d)) {
    pos <- match(as.character(cc$row_id), as.character(d$.ri_row_id))
  } else {
    pos <- suppressWarnings(as.integer(cc$row_id))
    pos[!is.finite(pos) | pos < 1 | pos > nrow(d)] <- NA_integer_
  }
  cc$age <- ifelse(is.na(pos), NA_real_, d$age[pos])
  cc$distance_to_cut <- abs(cc$age - cut)
  cc$boundary_protected <- FALSE
  cc$neighbor_compatible <- FALSE

  for (ii in seq_len(nrow(cc))) {
    if (!is.finite(cc$age[ii]) || !is.finite(cc$value[ii]) || cc$distance_to_cut[ii] > near_years) next
    neighbor <- if (cc$age[ii] <= cut) d$value[d$age > cut] else d$value[d$age <= cut]
    neighbor <- neighbor[is.finite(neighbor)]
    if (length(neighbor) < 20) next
    tk <- tukey_outlier_assessment(neighbor)
    compatible <- is.finite(tk$lower_inner) && is.finite(tk$upper_inner) &&
      cc$value[ii] >= tk$lower_inner && cc$value[ii] <= tk$upper_inner
    cc$neighbor_compatible[ii] <- isTRUE(compatible)
    cc$boundary_protected[ii] <- isTRUE(compatible)
  }
  protected <- cc[cc$boundary_protected %in% TRUE, , drop = FALSE]
  ordinary <- cc[!(cc$boundary_protected %in% TRUE), , drop = FALSE]
  list(protected = protected, ordinary = ordinary, near_years = near_years)
}

age_operational_cut_info <- function(data, cut) {
  d <- data[is.finite(data$age), , drop = FALSE]
  if (!nrow(d) || !is.finite(cut)) return(NULL)
  left <- d$age[d$age <= cut]
  right <- d$age[d$age > cut]
  if (!length(left) || !length(right)) return(NULL)
  left_max <- max(left, na.rm = TRUE)
  right_min <- min(right, na.rm = TRUE)
  if (!is.finite(left_max) || !is.finite(right_min) || left_max >= right_min) return(NULL)

  # Find the simplest decimal boundary that preserves exactly the same
  # classification as the statistical cut-point. With the convention < cut-point / >= cut-point,
  # Any value within (maximum left, minimum right] is equivalent.
  op <- NA_real_; digits <- NA_integer_
  for (dd in 0:4) {
    scale <- 10^dd
    cand <- ceiling((left_max + 1e-12) * scale) / scale
    if (is.finite(cand) && cand > left_max + 1e-10 && cand <= right_min + 1e-10) {
      op <- cand; digits <- dd; break
    }
  }
  if (!is.finite(op)) {
    op <- right_min
    digits <- 6L
    for (dd in 0:6) {
      if (abs(op - round(op, dd)) < 1e-10) { digits <- dd; break }
    }
  }
  fmt <- formatC(op, format = "f", digits = digits, decimal.mark = ",")
  list(
    cut = op, digits = digits, left_max = left_max, right_min = right_min,
    lower_label = paste0("< ", fmt, " years"),
    upper_label = paste0("≥ ", fmt, " years"),
    display = paste0("< ", fmt, " / ≥ ", fmt, " years"),
    preserves_groups = isTRUE(op > left_max && op <= right_min + 1e-10)
  )
}

age_group_operational_label <- function(result, group) {
  info <- result$operational_cut_info %||% NULL
  g <- as.character(group %||% "")
  if (is.null(info) || !isTRUE(info$preserves_groups)) return(g)
  if (startsWith(g, "≤")) return(info$lower_label)
  if (startsWith(g, ">")) return(info$upper_label)
  g
}

validate_direct_age_cut <- function(data, cut, seed = 1201, reviewed_extreme_values = numeric(0), reference_design = NULL) {
  design <- normalize_reference_design(reference_design)
  d <- data[is.finite(data$age) & is.finite(data$value), , drop = FALSE]
  if (!is.finite(cut) || nrow(d) < 240) return(NULL)
  d$age_segment <- ifelse(d$age <= cut,
                          paste0("≤", formatC(cut, format = "fg", digits = 5)),
                          paste0(">", formatC(cut, format = "fg", digits = 5)))
  ns <- table(d$age_segment)
  if (length(ns) != 2 || any(ns < 120)) {
    return(list(status = "yellow", decision = "indeterminate", cut = cut,
                recommendation = "The candidate cut-point leaves at least one segment with n<120; it is not automatically valid."))
  }
  pr <- tryCatch(assess_categorical_partition(d, "age_segment", route = "direct", seed = seed,
                                               reviewed_extreme_values = reviewed_extreme_values,
                                               reference_design = design),
                 error = function(e) NULL)
  if (is.null(pr)) return(NULL)

  # Specific boundary protection: before presenting a subgroup extreme value
  # as an exclusion candidate, it is checked for proximity to the cut-point and compatibility
  # with the neighboring group distribution. If compatible, it is protected and requires review
  # of the cut-point location first.
  guard <- age_boundary_outlier_guard(d, cut, pr$subgroup_outlier_candidates %||% data.frame())
  pr$boundary_outlier_candidates <- guard$protected
  pr$boundary_outlier_pending <- is.data.frame(guard$protected) && nrow(guard$protected) > 0
  pr$boundary_guard_years <- guard$near_years
  pr$subgroup_outlier_candidates <- guard$ordinary
  pr$subgroup_outlier_pending <- is.data.frame(guard$ordinary) && nrow(guard$ordinary) > 0

  if (!is.null(pr$subgroup_outlier_summary) && nrow(pr$subgroup_outlier_summary)) {
    pr$subgroup_outlier_summary$`Protected boundary observations` <- 0L
    if (isTRUE(pr$boundary_outlier_pending) && "group" %in% names(pr$boundary_outlier_candidates)) {
      bc <- table(as.character(pr$boundary_outlier_candidates$group))
      for (ii in seq_len(nrow(pr$subgroup_outlier_summary))) {
        gg <- as.character(pr$subgroup_outlier_summary$Group[ii])
        if (gg %in% names(bc)) pr$subgroup_outlier_summary$`Protected boundary observations`[ii] <- as.integer(bc[[gg]])
      }
    }
    pr$subgroup_outlier_summary$`Review pending` <- ifelse(
      pr$subgroup_outlier_summary$`Protected boundary observations` > 0,
      "Review cut-point", pr$subgroup_outlier_summary$`Review pending`
    )
  }

  if (isTRUE(pr$boundary_outlier_pending)) {
    pr$pre_boundary_status <- pr$status
    pr$pre_boundary_decision <- pr$decision
    pr$pre_boundary_recommendation <- pr$recommendation
    pr$status <- "yellow"
    pr$decision <- "indeterminate"
    pr$recommendation <- paste0(
      "Detected at least an extreme observation very near to the cut-point that is compatible with the distribution of the neighboring group. ",
      "Possible observation misclassified because of cut-point location. Review the cut-point first before considering exclusion."
    )
  }

  rows <- lapply(sort(unique(d$age_segment)), function(g) {
    z <- d[d$age_segment == g, , drop = FALSE]
    rr <- tryCatch(run_direct_establishment(z$value, bootstrap_R = 1000, seed = seed + nrow(z), reference_design = design), error = function(e) NULL)
    data.frame(
      Group = g, `Age minimum` = min(z$age), `Age maximum` = max(z$age), n = nrow(z),
      LRL = if (is.null(rr)) NA_real_ else rr$ri[1],
      URL = if (is.null(rr)) NA_real_ else rr$ri[2],
      `Precision LRL` = if (is.null(rr)) NA_real_ else rr$precision_ratio[1],
      `Precision URL` = if (is.null(rr)) NA_real_ else rr$precision_ratio[2],
      stringsAsFactors = FALSE, check.names = FALSE
    )
  })
  pr$cut <- cut
  pr$segment_table <- do.call(rbind, rows)
  pr
}

age_model_diagnostics_ok <- function(model) {
  if (is.null(model) || !isTRUE(model$ok)) return(FALSE)
  p_ok <- is.finite(model$residual_ad_p) && model$residual_ad_p >= 0.05
  age_ok <- !is.finite(model$residual_age_rho) || abs(model$residual_age_rho) < 0.15
  fit_ok <- !is.finite(model$residual_fitted_rho) || abs(model$residual_fitted_rho) < 0.15
  spread_ok <- !is.finite(model$abs_residual_age_rho) || abs(model$abs_residual_age_rho) < 0.15
  isTRUE(p_ok && age_ok && fit_ok && spread_ok)
}

assess_age_pattern <- function(data, route = c("direct", "indirect"), n_bootstrap = 200, seed = 1201,
                               reviewed_extreme_values = numeric(0), reference_design = NULL) {
  route <- match.arg(route)
  design <- normalize_reference_design(reference_design)
  if (!"age" %in% names(data)) {
    return(list(status = "grey", decision = "not_evaluable", recommendation = "Age is not available."))
  }
  d <- data[is.finite(data$age) & is.finite(data$value), , drop = FALSE]
  if (nrow(d) < 120 || length(unique(d$age)) < 10 || diff(range(d$age)) < 5) {
    return(list(status = "grey", decision = "not_evaluable",
                recommendation = "There are insufficient data or age range to assess continuous dependence reliably."))
  }

  bins <- age_make_bins(d$age, d$value, reference_design = design)
  if (is.null(bins)) {
    return(list(status = "grey", decision = "not_evaluable",
                recommendation = "Could not construct sufficiently informative age bands for the initial diagnostic."))
  }

  rho <- suppressWarnings(stats::cor(bins$age_mid, bins$p50, method = "spearman", use = "complete.obs"))
  overall_iqr <- stats::IQR(d$value, na.rm = TRUE)
  median_range <- diff(range(bins$p50, na.rm = TRUE))
  normalized_median_change <- if (is.finite(overall_iqr) && overall_iqr > 0) median_range / overall_iqr else NA_real_
  candidates <- age_candidate_splits_v2(bins)

  if (route == "indirect") {
    # The indirect pathway is retained: indirect continuous methods
    # (refineR + GAMLSS) will be validated in a dedicated subsequent module.
    if (is.finite(normalized_median_change) && normalized_median_change < 0.25 &&
        (!is.finite(rho) || abs(rho) < 0.30)) {
      return(list(status = "green", decision = "common",
                  recommendation = "No sufficiently important age effect was detected to abandon a common RI in indirect screening.",
                  bins = bins, spearman_rho = rho, normalized_median_change = normalized_median_change,
                  candidate_splits = candidates, route = route))
    }
    return(list(status = "yellow", decision = "indeterminate",
                recommendation = "An age-dependence pattern is observed in the indirect data. Continuous indirect modeling will be completed in the dedicated refineR + GAMLSS module; arbitrary age groups are not recommended.",
                bins = bins, spearman_rho = rho, normalized_median_change = normalized_median_change,
                candidate_splits = candidates, route = route))
  }

  model <- fit_direct_age_gamlss(d, reference_design = design)
  if (!isTRUE(model$ok)) {
    small_effect <- is.finite(normalized_median_change) && normalized_median_change < 0.25 &&
      (!is.finite(rho) || abs(rho) < 0.30)
    return(list(
      status = if (small_effect) "green" else "yellow",
      decision = if (small_effect) "common" else "indeterminate",
      recommendation = if (small_effect)
        "The initial diagnostic does not show an age effect of relevant magnitude; a common RI is retained. GAMLSS could not be completed, which is documented."
      else paste0("A possible age effect is observed, but the continuous GAMLSS model could not be completed: ", model$reason %||% "unspecified error", ". Do not implement arbitrary partitions."),
      bins = bins, spearman_rho = rho, normalized_median_change = normalized_median_change,
      candidate_splits = candidates, continuous = NULL, model = model, route = route
    ))
  }

  metrics <- age_curve_metrics(model$curves)
  empirical_step <- age_binned_step_metrics(bins)
  model_ok <- age_model_diagnostics_ok(model)
  relevant <- is.finite(model$aic_gain) && model$aic_gain >= 10 &&
    is.finite(metrics$relative_limit_change) && metrics$relative_limit_change >= 0.10
  model_step_conc <- metrics$step_concentration %||% NA_real_
  empirical_step_conc <- empirical_step$step_concentration %||% NA_real_
  step_signal <- max(c(model_step_conc, empirical_step_conc), na.rm = TRUE)
  if (!is.finite(step_signal)) step_signal <- NA_real_
  step_like <- isTRUE(relevant) && is.finite(step_signal) &&
    step_signal >= 0.35 && length(candidates) > 0

  rpart_cut <- NA_real_
  cut <- NA_real_
  target_step_age <- NA_real_
  cut_refinement <- NULL
  cut_validation <- NULL
  operational_cut_info <- NULL
  if (step_like) {
    target_step_age <- if (is.finite(empirical_step_conc) && empirical_step_conc >= model_step_conc) empirical_step$step_age else metrics$step_age
    rpart_cut <- age_pick_step_cut(candidates, target_step_age, range(d$age, na.rm = TRUE))
    if (is.finite(rpart_cut)) {
      cut_refinement <- age_refine_step_cut(d, rpart_cut, target_age = target_step_age, min_n = 120L)
      cut <- if (isTRUE(cut_refinement$ok) && is.finite(cut_refinement$selected_cut)) cut_refinement$selected_cut else rpart_cut
      operational_cut_info <- age_operational_cut_info(d, cut)
      cut_validation <- validate_direct_age_cut(d, cut, seed = seed + 200,
                                                reviewed_extreme_values = reviewed_extreme_values,
                                                reference_design = design)
    }
  }

  subgroup_outlier_pending <- !is.null(cut_validation) && isTRUE(cut_validation$subgroup_outlier_pending)
  boundary_outlier_pending <- !is.null(cut_validation) && isTRUE(cut_validation$boundary_outlier_pending)

  if (isTRUE(boundary_outlier_pending)) {
    status <- "yellow"
    decision <- "indeterminate"
    rec <- paste0(
      "The age pattern suggests a step, but at least one extreme observation near the cut-point is compatible with the neighboring segment distribution. ",
      "Possible observation misclassified because of cut-point location. Review the cut-point first before considering exclusion."
    )
  } else if (isTRUE(subgroup_outlier_pending)) {
    status <- "yellow"
    decision <- "indeterminate"
    rec <- paste0(
      "The age pattern suggests a candidate partition, but extreme values were detected within at least one segment after refining the change-point. ",
      "Review them before deciding between a discrete partition and a continuous model. Any justified exclusion requires recalculation of the cut-point, segment RIs, Lahti, Harris-Boyd, and precision."
    )
  } else if (!isTRUE(relevant)) {
    status <- "green"
    decision <- "common"
    rec <- paste0(
      "No variation of the RI with age is detected that is large enough to justify an age-dependent model. ",
      "The AIC improvement of the age model is ", ifelse(is.finite(model$aic_gain), round(model$aic_gain, 1), "not evaluable"),
      " and the maximum change in the limits is approximately ",
      ifelse(is.finite(metrics$relative_limit_change), paste0(round(100 * metrics$relative_limit_change, 1), " %"), "—"),
      " of the typical RI width. A common RI is recommended."
    )
  } else if (!is.null(cut_validation) && identical(cut_validation$decision, "partition") &&
             isTRUE(cut_validation$subgroup_precision_ok) && isTRUE(cut_validation$subgroup_standard)) {
    status <- "green"
    decision <- "partition"
    stat_cut_txt <- formatC(cut, format = "f", digits = 2, decimal.mark = ",")
    op_txt <- if (!is.null(operational_cut_info) && isTRUE(operational_cut_info$preserves_groups))
      paste0(" The cut-point operational equivalent is ", operational_cut_info$display,
             "; it exactly preserves the classification of the statistical cut-point.") else ""
    rec <- paste0(
      "Variation with age is relevant and concentrated in an abrupt change. rpart has delimited the candidate zone",
      if (is.finite(rpart_cut)) paste0(" around ", formatC(rpart_cut, format="f", digits=2, decimal.mark=","), " years") else "",
      ", and the robust search across observed age boundaries placed the statistical cut-point at ", stat_cut_txt, " years.", op_txt, " ",
      "Direct validation of the two segments (Lahti + Harris-Boyd + precision) supports partitioning. ",
      "RIveR proposes using the two age-specific RIs shown in the table; the operational cut-point still requires biological plausibility and specialist approval."
    )
  } else if (isTRUE(model_ok)) {
    status <- "green"
    decision <- "continuous"
    rec <- paste0(
      "The age-related variation in the limits is relevant, but it does not behave as a single validated step. ",
      "The continuous GAMLSS model improves fit (ΔAIC ≈ ", round(model$aic_gain, 1),
      ") and the residual diagnostics are acceptable. Priority should be given to a continuous age-dependent RI. ",
      "If the LIS does not support a continuous RI, discrete bands should be derived from the model and subsequently validated; they should not be set arbitrarily for convenience."
    )
  } else {
    status <- "yellow"
    decision <- "indeterminate"
    rec <- paste0(
      "Age appears to modify the RI, but the continuous model does not pass all residual diagnostics and a discrete partition has not been validated. ",
      "Do not implement an age-dependent RI until the model shape, data, and possible subpopulations have been reviewed."
    )
  }

  out <- list(
    status = status, decision = decision, recommendation = rec, reference_design = design,
    bins = bins, spearman_rho = rho, normalized_median_change = normalized_median_change,
    candidate_splits = candidates,
    continuous = list(curves = model$curves, fit = model$fit, family = model$family),
    model = model, metrics = metrics, empirical_step_metrics = empirical_step, model_ok = model_ok,
    effect_relevant = relevant, step_like = step_like,
    subgroup_outlier_pending = subgroup_outlier_pending, boundary_outlier_pending = boundary_outlier_pending,
    rpart_cut = rpart_cut, target_step_age = target_step_age, cut_refinement = cut_refinement,
    selected_cut = cut, operational_cut_info = operational_cut_info, cut_validation = cut_validation,
    auto_partition = if (!is.null(cut_validation)) list(
      status = cut_validation$status, decision = cut_validation$decision,
      recommendation = cut_validation$recommendation %||% rec,
      table = cut_validation$segment_table %||% NULL,
      cuts = if (is.finite(cut)) cut else numeric(0),
      validation = cut_validation
    ) else NULL,
    route = route,
    note = "v0.12.7: rpart only defines the candidate zone. The statistical cut-point is refined on observed age boundaries; when a simpler operational boundary preserves exactly the same subjects, it is shown separately. Aberrant values are then reviewed and Lahti/Harris-Boyd/precision are validated."
  )
  if (identical(decision, "continuous")) {
    out$sil_adaptation <- age_sil_discrete_adaptation(out, tolerance = 0.10, max_groups = 10L, min_per_band = 50L)
    if (isTRUE(out$sil_adaptation$ok)) {
      out$recommendation <- paste0(out$recommendation, " ", out$sil_adaptation$recommendation)
    }
  }
  out
}

age_diagnostics_display_table <- function(result) {
  if (is.null(result)) return(NULL)
  m <- result$model %||% NULL
  met <- result$metrics %||% NULL
  if (is.null(m) || !isTRUE(m$ok)) {
    return(data.frame(
      Assessment = c("n with age", "Age range", "Change in median / IQR"),
      Result = c(
        fmt_integer(sum(result$bins$n %||% 0)),
        if (!is.null(result$bins)) paste0(round(min(result$bins$age_min),1), "–", round(max(result$bins$age_max),1), " years") else "—",
        if (is.finite(result$normalized_median_change %||% NA_real_)) formatC(result$normalized_median_change, format="f", digits=2, decimal.mark=",") else "—"
      ),
      Interpretation = c("Data available", "Covariate coverage", "Initial diagnostic"),
      check.names = FALSE, stringsAsFactors = FALSE
    ))
  }
  fmt <- function(x, d=2) if (is.finite(x)) formatC(x, format="f", digits=d, decimal.mark=",") else "—"
  design <- normalize_reference_design(result$reference_design %||% NULL)
  curve_label <- if (identical(design$tail, "two_sided")) {
    paste0("Estimated ", reference_percentile_label(design$pair_percentiles[["lower"]]), "/P50/", reference_percentile_label(design$pair_percentiles[["upper"]]))
  } else {
    paste0("P50 + ", reference_design_percentile_text(design), " active")
  }
  data.frame(
    Assessment = c(
      "Family / structure GAMLSS", curve_label, "AIC · model without age", "AIC · model with age", "ΔAIC in favor of age",
      "Maximum change in limits / typical RI width", "Concentration of modeled change within 8 years",
      "Concentration of empirical change within 8 years", "AD of normalized residuals", "Spearman residual ~ age", "Spearman residual ~ fitted",
      "Spearman |residual| ~ age", "Overall model diagnostic"
    ),
    Result = c(
      paste0(m$family %||% "—", " · ", m$model_label %||% "—"), m$curve_source %||% "—", fmt(m$aic_null,1), fmt(m$aic_age,1), fmt(m$aic_gain,1),
      if (is.finite(met$relative_limit_change %||% NA_real_)) paste0(round(100*met$relative_limit_change,1), " %") else "—",
      if (is.finite(met$step_concentration %||% NA_real_)) paste0(round(100*met$step_concentration,1), " %") else "—",
      if (is.finite(result$empirical_step_metrics$step_concentration %||% NA_real_)) paste0(round(100*result$empirical_step_metrics$step_concentration,1), " %") else "—",
      format_p_value(m$residual_ad_p), fmt(m$residual_age_rho,3), fmt(m$residual_fitted_rho,3),
      fmt(m$abs_residual_age_rho,3), if (isTRUE(result$model_ok)) "Adequate" else "Requires review"
    ),
    Interpretation = c(
      "Distribution used for the model", "Continuous-curve calculation pathway", "Reference model", "Model dependent on age",
      if (is.finite(m$aic_gain) && m$aic_gain >= 10) "Clear improvement in fit" else "No clear improvement",
      if (is.finite(met$relative_limit_change) && met$relative_limit_change >= .10) "Potentially relevant change" else "Small change",
      if (is.finite(met$step_concentration %||% NA_real_) && met$step_concentration >= .35) "Concentrated modeled pattern" else "Modeled pattern not concentrated",
      if (is.finite(result$empirical_step_metrics$step_concentration %||% NA_real_) && result$empirical_step_metrics$step_concentration >= .35) "Empirical pattern concentrated / possible step" else "Empirical pattern not concentrated",
      if (is.finite(m$residual_ad_p) && m$residual_ad_p >= .05) "Residual normality not questioned" else "Residual normality questioned",
      if (!is.finite(m$residual_age_rho) || abs(m$residual_age_rho) < .15) "No important residual trend" else "Residual trend",
      if (!is.finite(m$residual_fitted_rho) || abs(m$residual_fitted_rho) < .15) "No important residual trend" else "Residual trend",
      if (!is.finite(m$abs_residual_age_rho) || abs(m$abs_residual_age_rho) < .15) "Stable residual width" else "Possible residual heteroscedasticity",
      if (isTRUE(result$model_ok)) status_symbol_text("green", "Assumptions defensible") else status_symbol_text("yellow", "Review the model")
    ),
    check.names = FALSE, stringsAsFactors = FALSE
  )
}

age_partition_display_table <- function(result) {
  tab <- result$cut_validation$segment_table %||% result$auto_partition$table %||% NULL
  if (is.null(tab) || !nrow(tab)) return(NULL)
  design <- normalize_reference_design(result$reference_design %||% NULL)
  out <- tab
  if ("Group" %in% names(out)) out$Group <- vapply(out$Group, function(g) age_group_operational_label(result, g), character(1))
  if (!isTRUE(design$active[["lower"]])) out <- out[, setdiff(names(out), c("LRL", "Precision LRL")), drop=FALSE]
  if (!isTRUE(design$active[["upper"]])) out <- out[, setdiff(names(out), c("URL", "Precision URL")), drop=FALSE]
  for (nm in intersect(c("LRL", "URL"), names(out))) out[[nm]] <- signif(out[[nm]], 6)
  for (nm in intersect(c("Precision LRL", "Precision URL"), names(out))) out[[nm]] <- vapply(out[[nm]], format_percent, character(1), digits = 1)
  out
}

age_cut_validation_display_table <- function(result) {
  cv <- result$cut_validation %||% NULL
  if (is.null(cv) || !is.finite(result$selected_cut %||% NA_real_)) return(NULL)
  design <- normalize_reference_design(result$reference_design %||% NULL)
  rows <- list()
  add <- function(criterion, result, threshold, conclusion) {
    rows[[length(rows) + 1L]] <<- data.frame(
      Criterion = criterion, Result = result, `Rule / threshold` = threshold,
      Conclusion = conclusion, stringsAsFactors = FALSE, check.names = FALSE
    )
  }
  fmt2 <- function(x) if (is.finite(x)) formatC(x, format="f", digits=2, decimal.mark=",") else "—"
  add("rpart candidate zone", paste0(fmt2(result$rpart_cut %||% NA_real_), " years"),
      "Exploratory screening; not the final cut-point", status_symbol_text("grey", "Candidate zone"))
  add("Refined statistical cut-point", paste0(fmt2(result$selected_cut), " years"),
      "Maximum robust reduction in dispersion within the candidate zone", status_symbol_text("green", "Localized"))
  op <- result$operational_cut_info %||% NULL
  if (!is.null(op) && isTRUE(op$preserves_groups)) {
    add("Cut-point operational proposed", op$display,
        "Must preserve exactly the same subjects as the statistical cut-point",
        status_symbol_text("green", "Equivalent"))
  }

  la <- cv$lahti %||% NULL
  if (!is.null(la) && nrow(la)) {
    for (g in unique(as.character(la$group))) {
      gp <- la[as.character(la$group) == g, , drop = FALSE]
      lo <- gp[gp$side == "lower", , drop = FALSE]
      up <- gp[gp$side == "upper", , drop = FALSE]
      one <- function(z) if (nrow(z)) paste0(z$outside_n[1], "/", z$n[1], " (", format_percent(z$proportion[1], 1), ")") else "—"
      gst <- worst_status(gp$status %||% character(0))
      tail_chunks <- character(0)
      if (isTRUE(design$active[["lower"]])) tail_chunks <- c(tail_chunks, paste0("tail lower ", one(lo)))
      if (isTRUE(design$active[["upper"]])) tail_chunks <- c(tail_chunks, paste0("tail upper ", one(up)))
      expected <- 100 * if (identical(design$tail, "two_sided")) (1-design$coverage)/2 else (1-design$coverage)
      add(paste0("Lahti · ", age_group_operational_label(result, g)),
          paste(tail_chunks, collapse="; "),
          paste0("Active tail(s) according to the design; nominal proportion outside the limit = ", formatC(expected, format="fg", digits=4, decimal.mark=","), " %. Lahti criterion adapted in RIveR."),
          if (identical(gst, "red"))
            paste0(status_symbol_text("red", "Common RI not acceptable"), " → ", status_symbol_text("green", "Partition supported"))
          else if (identical(gst, "green"))
            status_symbol_text("green", "Common RI acceptable; no support for partitioning")
          else if (identical(gst, "yellow"))
            status_symbol_text("yellow", "Intermediate zone; inconclusive")
          else status_symbol_text("grey", "Not evaluable"))
    }
  }

  hb <- cv$harris_boyd %||% NULL
  if (!is.null(hb)) {
    add("Harris-Boyd",
        paste0("Z=", fmt2(hb$z), "; Z*=", fmt2(hb$z_critical), "; SD ratio=", fmt2(hb$sd_ratio)),
        "Supports partitioning if Z>Z* or SD ratio>1.5",
        if (isTRUE(hb$supports_partition))
          status_symbol_text("green", "Partition supported")
        else if (isFALSE(hb$supports_partition))
          status_symbol_text("green", "No support for partitioning")
        else status_symbol_text("grey", "Not evaluable"))
  }

  st <- cv$segment_table %||% NULL
  if (!is.null(st) && nrow(st)) {
    for (ii in seq_len(nrow(st))) {
      checks <- logical(0)
      if (isTRUE(design$active[["lower"]])) checks <- c(checks, is.finite(st[["Precision LRL"]][ii]) && st[["Precision LRL"]][ii] < .20)
      if (isTRUE(design$active[["upper"]])) checks <- c(checks, is.finite(st[["Precision URL"]][ii]) && st[["Precision URL"]][ii] < .20)
      ok <- length(checks) > 0 && all(checks)
      precision_chunks <- character(0)
      if (isTRUE(design$active[["lower"]])) precision_chunks <- c(precision_chunks, paste0(reference_percentile_label(design$pair_percentiles[["lower"]]), " ", format_percent(st[["Precision LRL"]][ii], 1)))
      if (isTRUE(design$active[["upper"]])) precision_chunks <- c(precision_chunks, paste0(reference_percentile_label(design$pair_percentiles[["upper"]]), " ", format_percent(st[["Precision URL"]][ii], 1)))
      add(paste0("Precision · ", age_group_operational_label(result, st$Group[ii])),
          paste(precision_chunks, collapse="; "),
          "90% CI width of each active limit <20% of the computational RI width",
          status_symbol_text(if (ok) "green" else "red", if (ok) "Meets" else "Does not meet"))
    }
  }
  nb <- if (is.data.frame(cv$boundary_outlier_candidates)) nrow(cv$boundary_outlier_candidates) else 0L
  add("Protected boundary observations", as.character(nb), "Any pending review must be resolved before validating the partition",
      status_symbol_text(if (nb == 0L) "green" else "yellow", if (nb == 0L) "None" else "Review cut-point"))
  dec_txt <- switch(cv$decision %||% "indeterminate",
                    partition = "Partition recommended", common = "common RI",
                    indeterminate = "Inconclusive", "Inconclusive")
  add("Integrated decision", dec_txt,
      "Lahti + Harris-Boyd + n≥120 + precision + review of aberrant",
      status_symbol_text(cv$status %||% "yellow", dec_txt))
  do.call(rbind, rows)
}

age_boundary_outlier_display_table <- function(result) {
  x <- result$cut_validation$boundary_outlier_candidates %||% NULL
  if (is.null(x) || !is.data.frame(x) || !nrow(x)) return(NULL)
  data.frame(
    `ID/row` = ifelse(nzchar(as.character(x$patient_id %||% "")),
                       paste0(as.character(x$patient_id), " / ", as.character(x$row_id)),
                       as.character(x$row_id)),
    Age = x$age,
    Value = x$value,
    `Distance to the cut-point` = x$distance_to_cut,
    Interpretation = "Compatible with the neighboring group; review first the cut-point",
    stringsAsFactors = FALSE, check.names = FALSE
  )
}

age_curve_display_table <- function(result, ages = NULL) {
  c <- result$continuous$curves %||% NULL
  if (is.null(c) || !nrow(c)) return(NULL)
  design <- normalize_reference_design(result$reference_design %||% NULL)
  if (is.null(ages)) ages <- unique(round(stats::quantile(c$age, c(0,.25,.5,.75,1), names=FALSE), 0))
  rows <- lapply(ages, function(a) {
    i <- which.min(abs(c$age - a))
    vals <- list(Age = round(c$age[i],1))
    if (isTRUE(design$active[["lower"]])) vals[[reference_percentile_label(design$pair_percentiles[["lower"]])]] <- signif(c$p025[i], 6)
    vals[["P50"]] <- signif(c$p50[i], 6)
    if (isTRUE(design$active[["upper"]])) vals[[reference_percentile_label(design$pair_percentiles[["upper"]])]] <- signif(c$p975[i], 6)
    as.data.frame(vals, check.names=FALSE)
  })
  do.call(rbind, rows)
}

# v0.16.0 · general layer for quantitative variable -------------------------
# The historical age engine is retained unchanged to preserve D4 regression behavior.
# For another quantitative variable, RIveR normalizes only the X-axis to
# an equivalent internal range (18–80) and reuses exactly the same
# relative/AIC/residual criteria. Axis points are transformed back to
# the original scale before returning the result.
quantitative_is_age_like <- function(label) {
  z <- tolower(trimws(as.character(label %||% "")))
  z <- gsub("[^[:alpha:]]", "", z)
  z %in% c("age")
}

quantitative_relabel_text <- function(x, label) {
  if (!is.character(x) || !length(x)) return(x)
  lab <- as.character(label %||% "quantitative variable")
  x <- gsub("the age", lab, x, fixed=TRUE)
  x <- gsub("The age", lab, x, fixed=TRUE)
  x <- gsub("of age", paste0("of ", lab), x, fixed=TRUE)
  x <- gsub("Of age", paste0("Of ", lab), x, fixed=TRUE)
  x <- gsub("for age", paste0("for ", lab), x, fixed=TRUE)
  x <- gsub("with age", paste0("with ", lab), x, fixed=TRUE)
  x <- gsub("age", lab, x, fixed=TRUE)
  x <- gsub("Age", lab, x, fixed=TRUE)
  x <- gsub(" years", "", x, fixed=TRUE)
  x
}

quantitative_backtransform_result <- function(res, from_internal, label) {
  tr <- function(v) { z <- suppressWarnings(as.numeric(v)); ifelse(is.finite(z), from_internal(z), z) }
  if (!is.null(res$bins) && is.data.frame(res$bins)) {
    for (nm in intersect(c("age_min","age_max","age_mid","age"), names(res$bins))) res$bins[[nm]] <- tr(res$bins[[nm]])
  }
  if (!is.null(res$candidate_splits)) res$candidate_splits <- tr(res$candidate_splits)
  if (!is.null(res$continuous$curves) && is.data.frame(res$continuous$curves) && "age" %in% names(res$continuous$curves)) res$continuous$curves$age <- tr(res$continuous$curves$age)
  if (!is.null(res$model$curves) && is.data.frame(res$model$curves) && "age" %in% names(res$model$curves)) res$model$curves$age <- tr(res$model$curves$age)
  if (!is.null(res$model$data) && is.data.frame(res$model$data) && "age" %in% names(res$model$data)) res$model$data$age <- tr(res$model$data$age)
  for (nm in c("rpart_cut","target_step_age","selected_cut")) if (!is.null(res[[nm]])) res[[nm]] <- tr(res[[nm]])
  if (!is.null(res$cut_refinement)) {
    for (nm in c("selected_cut","target_age","search_min","search_max")) if (!is.null(res$cut_refinement[[nm]])) res$cut_refinement[[nm]] <- tr(res$cut_refinement[[nm]])
    if (!is.null(res$cut_refinement$table) && is.data.frame(res$cut_refinement$table)) {
      for (nm in names(res$cut_refinement$table)) if (grepl("Cut-point|age|Age", nm)) res$cut_refinement$table[[nm]] <- tr(res$cut_refinement$table[[nm]])
    }
  }
  if (!is.null(res$operational_cut_info) && is.finite(res$selected_cut %||% NA_real_)) {
    cut <- res$selected_cut
    res$operational_cut_info$statistical_cut <- cut
    res$operational_cut_info$operational_cut <- cut
    fmt <- formatC(cut, format="fg", digits=6, decimal.mark=",")
    res$operational_cut_info$lower_label <- paste0("< ", fmt)
    res$operational_cut_info$upper_label <- paste0("≥ ", fmt)
    res$operational_cut_info$display <- paste0("< ", fmt, " / ≥ ", fmt)
  }
  if (!is.null(res$cut_validation$segment_table) && is.data.frame(res$cut_validation$segment_table)) {
    for (nm in intersect(c("Age minimum","Age maximum"), names(res$cut_validation$segment_table))) res$cut_validation$segment_table[[nm]] <- tr(res$cut_validation$segment_table[[nm]])
  }
  if (!is.null(res$auto_partition$table) && is.data.frame(res$auto_partition$table)) {
    for (nm in intersect(c("Age minimum","Age maximum"), names(res$auto_partition$table))) res$auto_partition$table[[nm]] <- tr(res$auto_partition$table[[nm]])
  }
  if (!is.null(res$cut_validation$boundary_outlier_candidates) && is.data.frame(res$cut_validation$boundary_outlier_candidates) && "age" %in% names(res$cut_validation$boundary_outlier_candidates)) {
    res$cut_validation$boundary_outlier_candidates$age <- tr(res$cut_validation$boundary_outlier_candidates$age)
  }
  if (!is.null(res$sil_adaptation$annual_table) && is.data.frame(res$sil_adaptation$annual_table) && "Age" %in% names(res$sil_adaptation$annual_table)) res$sil_adaptation$annual_table$Age <- tr(res$sil_adaptation$annual_table$Age)
  if (!is.null(res$sil_adaptation$table) && is.data.frame(res$sil_adaptation$table)) {
    # The band table is already operational; for a generic variable, the suffix "years" is removed.
    if ("Band" %in% names(res$sil_adaptation$table)) res$sil_adaptation$table$Band <- gsub(" years", "", res$sil_adaptation$table$Band, fixed=TRUE)
  }
  res$recommendation <- quantitative_relabel_text(res$recommendation %||% "", label)
  if (!is.null(res$note)) res$note <- quantitative_relabel_text(res$note, label)
  if (!is.null(res$sil_adaptation$recommendation)) res$sil_adaptation$recommendation <- quantitative_relabel_text(res$sil_adaptation$recommendation, label)
  res$covariate_label <- label
  res$covariate_is_age <- FALSE
  res
}

assess_quantitative_pattern <- function(data, route = c("direct", "indirect"), n_bootstrap = 200, seed = 1201,
                                        reviewed_extreme_values = numeric(0), reference_design = NULL,
                                        covariate_label = "Quantitative variable") {
  route <- match.arg(route)
  if (!"quantitative" %in% names(data)) {
    if ("age" %in% names(data)) data$quantitative <- data$age else
      return(list(status="grey", decision="not_evaluable", recommendation="There is no quantitative variable available.", covariate_label=covariate_label))
  }
  q <- suppressWarnings(as.numeric(data$quantitative))
  ok <- is.finite(q) & is.finite(data$value)
  if (sum(ok) < 120L || length(unique(q[ok])) < 10L || !is.finite(diff(range(q[ok]))) || diff(range(q[ok])) <= 0) {
    return(list(status="grey", decision="not_evaluable", recommendation="There are insufficient data, distinct values, or range of the quantitative variable to assess continuous dependence.", covariate_label=covariate_label))
  }
  if (quantitative_is_age_like(covariate_label)) {
    data$age <- q
    res <- assess_age_pattern(data, route=route, n_bootstrap=n_bootstrap, seed=seed,
                              reviewed_extreme_values=reviewed_extreme_values, reference_design=reference_design)
    res$covariate_label <- covariate_label
    res$covariate_is_age <- TRUE
    return(res)
  }
  qr <- range(q[ok], na.rm=TRUE); span <- diff(qr)
  to_internal <- function(x) 18 + 62 * (x - qr[1]) / span
  from_internal <- function(x) qr[1] + (x - 18) * span / 62
  work <- data
  work$age <- to_internal(q)
  res <- assess_age_pattern(work, route=route, n_bootstrap=n_bootstrap, seed=seed,
                            reviewed_extreme_values=reviewed_extreme_values, reference_design=reference_design)
  quantitative_backtransform_result(res, from_internal, covariate_label)
}
