bootstrap_limits <- function(x, stat_fun, R = 1000, conf = 0.90, seed = 1234) {
  x <- x[is.finite(x)]
  if (length(x) < 3) stop("Insufficient sample size for bootstrap.")
  set.seed(seed)
  b <- replicate(R, stat_fun(sample(x, length(x), replace = TRUE)))
  if (is.vector(b)) b <- matrix(b, nrow = 2)
  alpha <- (1 - conf) / 2
  ci_low <- stats::quantile(b[1, ], c(alpha, 1 - alpha), na.rm = TRUE, names = FALSE, type = 6)
  ci_up  <- stats::quantile(b[2, ], c(alpha, 1 - alpha), na.rm = TRUE, names = FALSE, type = 6)
  list(lower = ci_low, upper = ci_up)
}

nonparametric_ri <- function(x, coverage = 0.95, reference_design = NULL) {
  d <- normalize_reference_design(reference_design, coverage = coverage)
  as.numeric(stats::quantile(x, d$pair_percentiles, na.rm = TRUE, type = 6, names = FALSE))
}

parametric_ri <- function(x, coverage = 0.95, reference_design = NULL) {
  d <- normalize_reference_design(reference_design, coverage = coverage)
  mu <- mean(x, na.rm = TRUE); sig <- stats::sd(x, na.rm = TRUE)
  as.numeric(stats::qnorm(d$pair_percentiles, mean = mu, sd = sig))
}

precision_ratio <- function(ri, ci_lower, ci_upper) {
  width <- ri[2] - ri[1]
  if (!is.finite(width) || width <= 0) return(c(lower = NA_real_, upper = NA_real_))
  c(lower = (ci_lower[2] - ci_lower[1]) / width,
    upper = (ci_upper[2] - ci_upper[1]) / width)
}

# Detection of extreme observations for direct studies ------------------------
# RIveR v0.6.0 separates "detection" from "exclusion". No observation is
# removed automatically. Dixon/Reed (one-third rule) is used as a conservative
# extreme-value signal; Tukey is presented using the practical terminology of
# RefValAdvisor: suspicious (1.5–3 IQR) and extreme (>3 IQR).

dixon_reed_one_third <- function(x) {
  z <- sort(x[is.finite(x)])
  n <- length(z)
  if (n < 3 || !is.finite(diff(range(z))) || diff(range(z)) <= 0) {
    return(list(
      range = NA_real_, lower_ratio = NA_real_, upper_ratio = NA_real_,
      lower_flag = FALSE, upper_flag = FALSE,
      lower_value = NA_real_, upper_value = NA_real_
    ))
  }
  rr <- z[n] - z[1]
  d_low <- z[2] - z[1]
  d_up <- z[n] - z[n - 1]
  r_low <- d_low / rr
  r_up <- d_up / rr
  list(
    range = rr,
    lower_ratio = r_low,
    upper_ratio = r_up,
    lower_flag = is.finite(r_low) && r_low >= (1/3),
    upper_flag = is.finite(r_up) && r_up >= (1/3),
    lower_value = z[1],
    upper_value = z[n]
  )
}

tukey_outlier_assessment <- function(x) {
  z <- x[is.finite(x)]
  if (length(z) < 4) {
    return(list(q1 = NA_real_, q3 = NA_real_, iqr = NA_real_,
                lower_inner = NA_real_, upper_inner = NA_real_,
                lower_outer = NA_real_, upper_outer = NA_real_,
                suspect_low = numeric(0), suspect_high = numeric(0),
                extreme_low = numeric(0), extreme_high = numeric(0)))
  }
  q <- stats::quantile(z, c(.25, .75), na.rm = TRUE, type = 7, names = FALSE)
  iq <- q[2] - q[1]
  if (!is.finite(iq) || iq <= 0) {
    return(list(q1 = q[1], q3 = q[2], iqr = iq,
                lower_inner = NA_real_, upper_inner = NA_real_,
                lower_outer = NA_real_, upper_outer = NA_real_,
                suspect_low = numeric(0), suspect_high = numeric(0),
                extreme_low = numeric(0), extreme_high = numeric(0)))
  }
  li <- q[1] - 1.5 * iq
  ui <- q[2] + 1.5 * iq
  lo <- q[1] - 3 * iq
  uo <- q[2] + 3 * iq
  tol <- max(1e-12, .Machine$double.eps * max(abs(z), 1, na.rm = TRUE) * 100)
  list(
    q1 = q[1], q3 = q[2], iqr = iq,
    lower_inner = li, upper_inner = ui,
    lower_outer = lo, upper_outer = uo,
    suspect_low = sort(z[z < (li - tol) & z >= (lo - tol)]),
    suspect_high = sort(z[z > (ui + tol) & z <= (uo + tol)]),
    extreme_low = sort(z[z < (lo - tol)]),
    extreme_high = sort(z[z > (uo + tol)])
  )
}

detect_direct_outliers <- function(x) {
  dr <- dixon_reed_one_third(x)
  tk <- tukey_outlier_assessment(x)
  extreme_values <- unique(c(
    if (isTRUE(dr$lower_flag)) dr$lower_value else numeric(0),
    if (isTRUE(dr$upper_flag)) dr$upper_value else numeric(0),
    tk$extreme_low, tk$extreme_high
  ))
  # suspect_n/extreme_n count observations, not unique values. This prevents
  # discrepancies when several subjects share the same result.
  suspect_observations <- c(tk$suspect_low, tk$suspect_high)
  z <- x[is.finite(x)]
  tol <- max(1e-12, .Machine$double.eps * max(abs(z), 1, na.rm = TRUE) * 100)
  is_extreme_obs <- if (length(extreme_values)) {
    vapply(z, function(v) any(abs(v - extreme_values) <= tol), logical(1))
  } else rep(FALSE, length(z))
  list(
    dixon_reed = dr,
    tukey = tk,
    extreme_values = sort(extreme_values),
    suspect_values = sort(suspect_observations),
    extreme_n = sum(is_extreme_obs),
    suspect_n = length(suspect_observations),
    review_required = length(extreme_values) > 0
  )
}

values_not_reviewed <- function(values, reviewed, tol = NULL) {
  values <- values[is.finite(values)]
  reviewed <- reviewed[is.finite(reviewed)]
  if (!length(values)) return(numeric(0))
  if (!length(reviewed)) return(values)
  if (is.null(tol)) tol <- max(1e-10, .Machine$double.eps * max(abs(c(values, reviewed)), 1) * 100)
  values[!vapply(values, function(v) any(abs(v - reviewed) <= tol), logical(1))]
}

outlier_candidate_rows <- function(data, result) {
  if (is.null(data) || is.null(result) || !identical(result$type, "direct_establishment") ||
      is.null(result$outlier_assessment)) return(NULL)
  vals <- if (!is.null(result$unreviewed_extreme_values)) result$unreviewed_extreme_values else result$outlier_assessment$extreme_values
  if (is.null(vals)) vals <- numeric(0)
  vals <- vals[is.finite(vals)]
  if (!length(vals) || !"value" %in% names(data)) return(NULL)
  z <- data$value
  tol <- max(1e-10, .Machine$double.eps * max(abs(c(z[is.finite(z)], vals)), 1, na.rm = TRUE) * 100)
  keep <- is.finite(z) & vapply(z, function(v) any(abs(v - vals) <= tol), logical(1))
  if (!any(keep)) return(NULL)
  out <- data[keep, intersect(c(".ri_row_id", "patient_id", "value"), names(data)), drop = FALSE]
  if (!".ri_row_id" %in% names(out)) out$.ri_row_id <- which(keep)
  if (!"patient_id" %in% names(out)) out$patient_id <- ""
  out <- out[, c(".ri_row_id", "patient_id", "value"), drop = FALSE]
  names(out) <- c("row_id", "patient_id", "value")
  out
}

# Distribution assessment and method selection -------------------------------
# Reference Value Advisor compares behavior in original and
# transformed data (normality, symmetry, outliers) before interpreting
# parametric/robust methods. RIveR follows that logic while retaining CLSI:
# with n>=120, the default final method remains non-parametric.

anderson_darling_normality <- function(x) {
  z <- x[is.finite(x)]
  n <- length(z)
  if (n < 4 || !is.finite(stats::sd(z)) || stats::sd(z) <= 0) {
    return(list(statistic = NA_real_, p.value = NA_real_))
  }
  z <- sort((z - mean(z)) / stats::sd(z))
  Fz <- stats::pnorm(z)
  eps <- 1e-12
  Fz <- pmin(pmax(Fz, eps), 1 - eps)
  i <- seq_len(n)
  a2 <- -n - mean((2 * i - 1) * (log(Fz) + log(1 - rev(Fz))))
  a2s <- a2 * (1 + 0.75 / n + 2.25 / n^2)
  if (!is.finite(a2s)) return(list(statistic = a2s, p.value = NA_real_))
  p <- if (isTRUE(a2s < 0.2)) {
    1 - exp(-13.436 + 101.14 * a2s - 223.73 * a2s^2)
  } else if (isTRUE(a2s < 0.34)) {
    1 - exp(-8.318 + 42.796 * a2s - 59.938 * a2s^2)
  } else if (isTRUE(a2s < 0.6)) {
    exp(0.9177 - 4.279 * a2s - 1.38 * a2s^2)
  } else {
    exp(1.2937 - 5.709 * a2s + 0.0186 * a2s^2)
  }
  list(statistic = a2s, p.value = max(0, min(1, p)))
}

# Runs-based symmetry test following the McWilliams rationale:
# absolute deviations from the median are ordered and the test evaluates whether
# signs change less often than expected under symmetry. In the presence of
# ties exact in the median is removed those observations for the test.
symmetry_runs_test <- function(x) {
  z <- x[is.finite(x)]
  if (length(z) < 8) return(list(statistic = NA_real_, p.value = NA_real_, n = length(z)))
  med <- stats::median(z)
  d <- z - med
  d <- d[abs(d) > .Machine$double.eps * max(1, abs(med)) * 100]
  if (length(d) < 8) return(list(statistic = NA_real_, p.value = NA_real_, n = length(d)))
  ord <- order(abs(d), seq_along(d))
  sg <- d[ord] > 0
  transitions <- sum(sg[-1] != sg[-length(sg)])
  m <- length(sg) - 1L
  p <- stats::pbinom(transitions, size = m, prob = 0.5)
  list(statistic = transitions, p.value = max(0, min(1, p)), n = length(sg))
}

boxcox_transform <- function(x, lambda, shift = 0) {
  z <- x + shift
  if (any(!is.finite(z)) || any(z <= 0)) return(rep(NA_real_, length(x)))
  if (abs(lambda) < 1e-8) log(z) else (z^lambda - 1) / lambda
}

boxcox_inverse <- function(y, lambda, shift = 0) {
  if (abs(lambda) < 1e-8) return(exp(y) - shift)
  base <- lambda * y + 1
  out <- rep(NA_real_, length(y))
  ok <- is.finite(base) & base > 0
  out[ok] <- base[ok]^(1 / lambda) - shift
  out
}

fit_boxcox <- function(x) {
  z0 <- x[is.finite(x)]
  n <- length(z0)
  if (n < 4 || !is.finite(stats::sd(z0)) || stats::sd(z0) <= 0) {
    return(list(ok = FALSE, lambda = NA_real_, shift = NA_real_, transformed = numeric(0), reason = "Insufficient variability."))
  }
  # Standard Box-Cox requires positive values. When zeros/negative values occur,
  # the minimum required shift is used; this is reported explicitly because it is
  # not identical to the two-parameter generalized Box-Cox approach in RefValAdv.
  scale <- max(stats::sd(z0), diff(range(z0)), abs(stats::median(z0)), 1, na.rm = TRUE)
  shift <- if (min(z0) <= 0) -min(z0) + max(1e-8, 1e-6 * scale) else 0
  z <- z0 + shift
  logz <- log(z)
  prof_ll <- function(lambda) {
    y <- boxcox_transform(z0, lambda, shift)
    if (any(!is.finite(y))) return(-Inf)
    v <- mean((y - mean(y))^2)
    if (!is.finite(v) || v <= 0) return(-Inf)
    -n / 2 * log(v) + (lambda - 1) * sum(logz)
  }
  opt <- tryCatch(stats::optimize(function(l) -prof_ll(l), interval = c(-3, 3)), error = function(e) NULL)
  if (is.null(opt) || !is.finite(opt$minimum)) {
    return(list(ok = FALSE, lambda = NA_real_, shift = shift, transformed = numeric(0), reason = "Box-Cox optimization failed."))
  }
  lambda <- opt$minimum
  y <- boxcox_transform(z0, lambda, shift)
  if (any(!is.finite(y))) return(list(ok = FALSE, lambda = lambda, shift = shift, transformed = y, reason = "Invalid transformation."))
  list(ok = TRUE, lambda = lambda, shift = shift, transformed = y, reason = NULL)
}

robust_point_ri <- function(x, coverage = 0.95) {
  if (!pkg_available("referenceIntervals")) return(c(NA_real_, NA_real_))
  rr <- tryCatch(referenceIntervals::robust(x, refConf = coverage), error = function(e) NULL)
  if (is.null(rr) || length(rr) < 2) c(NA_real_, NA_real_) else as.numeric(rr[1:2])
}

direct_distribution_diagnostics <- function(x, coverage = 0.95) {
  z <- x[is.finite(x)]
  ad0 <- anderson_darling_normality(z)
  sy0 <- symmetry_runs_test(z)
  bc <- fit_boxcox(z)
  adbc <- if (isTRUE(bc$ok)) anderson_darling_normality(bc$transformed) else list(statistic=NA_real_, p.value=NA_real_)
  sybc_raw <- if (isTRUE(bc$ok)) symmetry_runs_test(bc$transformed) else list(statistic=NA_real_, p.value=NA_real_)
  # With rounded data, a near-identity transformation can break exact ties in
  # deviations from the median and make the runs test change artificially.
  # If Box-Cox is minimal (lambda≈1 and no shift), this numerical jump is not
  # interpreted as a true change in symmetry; the original diagnostic is retained
  # and explicitly traced.
  symmetry_inherited <- isTRUE(bc$ok) && is.finite(bc$lambda) && abs(bc$lambda - 1) < 0.02 &&
    is.finite(bc$shift) && abs(bc$shift) < 1e-12
  sybc <- if (isTRUE(symmetry_inherited)) {
    c(sy0, list(note = "Minimal Box-Cox transformation: the symmetry diagnostic from the original data is retained."))
  } else sybc_raw
  ri_p <- parametric_ri(z, coverage)
  ri_r <- robust_point_ri(z, coverage)
  ri_pbc <- c(NA_real_, NA_real_)
  ri_rbc <- c(NA_real_, NA_real_)
  if (isTRUE(bc$ok)) {
    ri_pbc <- boxcox_inverse(parametric_ri(bc$transformed, coverage), bc$lambda, bc$shift)
    rrbc <- robust_point_ri(bc$transformed, coverage)
    if (all(is.finite(rrbc))) ri_rbc <- boxcox_inverse(rrbc, bc$lambda, bc$shift)
  }
  list(
    original = list(ad = ad0, symmetry = sy0),
    boxcox = list(ok = bc$ok, lambda = bc$lambda, shift = bc$shift, ad = adbc, symmetry = sybc,
                  symmetry_raw = sybc_raw, symmetry_inherited = symmetry_inherited,
                  transformed = bc$transformed, reason = bc$reason),
    candidate_ri = list(parametric = ri_p, robust = ri_r, parametric_bc = ri_pbc, robust_bc = ri_rbc, nonparametric = nonparametric_ri(z, coverage))
  )
}

choose_small_sample_method <- function(x, coverage = 0.95) {
  dd <- direct_distribution_diagnostics(x, coverage = coverage)
  pnorm <- dd$original$ad$p.value
  psym <- dd$original$symmetry$p.value
  pnorm_bc <- dd$boxcox$ad$p.value
  psym_bc <- dd$boxcox$symmetry$p.value

  if (isTRUE(is.finite(pnorm) && pnorm >= 0.05)) {
    return(list(method = "parametric", reason = "Anderson-Darling provides insufficient evidence to question normality of the original data; the standard parametric model is selected.", diagnostics = dd))
  }
  if (isTRUE(isTRUE(dd$boxcox$ok) && is.finite(pnorm_bc) && pnorm_bc >= 0.05)) {
    return(list(method = "parametric_bc", reason = "Normality of the original data is questioned, but after Box-Cox, Anderson-Darling no longer provides sufficient evidence to question normality; the Box-Cox parametric model is selected.", diagnostics = dd))
  }
  if (isTRUE(is.finite(psym) && psym >= 0.05) && pkg_available("referenceIntervals")) {
    return(list(method = "robust", reason = "Normality is questioned and the symmetry test detects no evidence of asymmetry; the robust method is selected as a defensible alternative.", diagnostics = dd))
  }
  if (isTRUE(isTRUE(dd$boxcox$ok) && is.finite(psym_bc) && psym_bc >= 0.05) && pkg_available("referenceIntervals")) {
    return(list(method = "robust_bc", reason = "After Box-Cox, normality remains questionable, but the symmetry test detects no evidence of asymmetry; the robust method is selected on the transformed scale.", diagnostics = dd))
  }
  reason <- "No sufficiently defensible parametric/robust model is identified with the current data. The non-parametric RI may be shown for exploratory purposes, but with n<120 it is not automatically adopted as the final RI."
  list(method = "exploratory", reason = reason, diagnostics = dd)
}

estimate_parametric_bc <- function(x, coverage = 0.95, ci_level = 0.90, bootstrap_R = 1000, seed = 1201) {
  fit <- fit_boxcox(x)
  if (!isTRUE(fit$ok)) stop("Box-Cox could not be fitted.")
  point_fun <- function(z) {
    f <- fit_boxcox(z)
    if (!isTRUE(f$ok)) return(c(NA_real_, NA_real_))
    boxcox_inverse(parametric_ri(f$transformed, coverage), f$lambda, f$shift)
  }
  ri <- point_fun(x)
  ci <- bootstrap_limits(x, point_fun, R = bootstrap_R, conf = ci_level, seed = seed)
  list(ri = ri, ci_lower = ci$lower, ci_upper = ci$upper, fit = fit)
}

estimate_robust_bc <- function(x, coverage = 0.95, ci_level = 0.90, bootstrap_R = 1000, seed = 1201) {
  if (!pkg_available("referenceIntervals")) stop("The robust method requires 'referenceIntervals'.")
  fit <- fit_boxcox(x)
  if (!isTRUE(fit$ok)) stop("Box-Cox could not be fitted.")
  point_fun <- function(z) {
    f <- fit_boxcox(z)
    if (!isTRUE(f$ok)) return(c(NA_real_, NA_real_))
    r <- robust_point_ri(f$transformed, coverage)
    if (!all(is.finite(r))) return(c(NA_real_, NA_real_))
    boxcox_inverse(r, f$lambda, f$shift)
  }
  ri <- point_fun(x)
  ci <- bootstrap_limits(x, point_fun, R = bootstrap_R, conf = ci_level, seed = seed)
  list(ri = ri, ci_lower = ci$lower, ci_upper = ci$upper, fit = fit)
}

run_direct_establishment <- function(x, coverage = 0.95, ci_level = 0.90,
                                     bootstrap_R = 1000, seed = 1201,
                                     reviewed_extreme_values = numeric(0),
                                     reference_design = NULL) {
  design <- normalize_reference_design(reference_design, coverage = coverage)
  coverage <- design$pair_coverage
  active_limits <- design$active
  x <- x[is.finite(x)]
  n <- length(x)
  display_digits <- infer_decimal_places(x)
  if (n < 3) stop("There are not enough valid results.")

  outlier_assessment <- tryCatch(
    detect_direct_outliers(x),
    error = function(e) stop("D3 · extreme-value detection: ", conditionMessage(e), call. = FALSE)
  )
  distribution <- tryCatch(
    direct_distribution_diagnostics(x, coverage = coverage),
    error = function(e) stop("D3 · distribution diagnostics: ", conditionMessage(e), call. = FALSE)
  )

  suspicious_horn <- numeric(0)
  if (pkg_available("referenceIntervals")) {
    suspicious_horn <- tryCatch(referenceIntervals::horn.outliers(x)$outliers,
                                error = function(e) numeric(0))
  }

  ci_source <- NA_character_
  diagnostic <- NULL
  if (n >= 120) {
    # CLSI: with n>=120, the non-parametric method is the standard pathway, so
    # normality/Box-Cox are reported diagnostically and do not alter the final method.
    method <- "Non-parametric (CLSI, ≥120)"
    rr <- NULL
    if (pkg_available("referenceIntervals")) {
      rr <- tryCatch(referenceIntervals::refLimit(x, out.method = "horn", out.rm = FALSE,
                                                  IR = "n", CI = "n", refConf = coverage,
                                                  limitConf = ci_level), error = function(e) NULL)
    }
    rr_ok <- !is.null(rr) && length(rr$Ref_Int) >= 2 && length(rr$Conf_Int) >= 4 &&
      all(is.finite(as.numeric(rr$Ref_Int[1:2]))) && all(is.finite(as.numeric(rr$Conf_Int[1:4])))
    if (isTRUE(rr_ok)) {
      ri <- as.numeric(rr$Ref_Int[1:2])
      ci_lower <- as.numeric(rr$Conf_Int[1:2])
      ci_upper <- as.numeric(rr$Conf_Int[3:4])
      ci_source <- "Non-parametric by ranks (referenceIntervals)"
    } else {
      ri <- if (!is.null(rr) && length(rr$Ref_Int) >= 2 && all(is.finite(as.numeric(rr$Ref_Int[1:2]))))
        as.numeric(rr$Ref_Int[1:2]) else nonparametric_ri(x, coverage)
      ci <- bootstrap_limits(x, function(z) nonparametric_ri(z, coverage), R = bootstrap_R, conf = ci_level, seed = seed)
      ci_lower <- ci$lower; ci_upper <- ci$upper
      ci_source <- paste0("Non-parametric bootstrap (", bootstrap_R, " replicates)")
    }
    route <- "standard"
    diagnostic <- list(method = "nonparametric", reason = "n≥120: standard CLSI non-parametric method; distribution assessment is informative.", diagnostics = distribution)
  } else {
    diagnostic <- tryCatch(
      choose_small_sample_method(x, coverage = coverage),
      error = function(e) stop("D3 · method selection with n<120: ", conditionMessage(e), call. = FALSE)
    )
    distribution <- diagnostic$diagnostics
    route <- "small_sample"
    if (identical(diagnostic$method, "parametric")) {
      method <- "Standard parametric (n<120; normality not questioned)"
      ri <- parametric_ri(x, coverage)
      ci <- bootstrap_limits(x, function(z) parametric_ri(z, coverage), R = bootstrap_R, conf = ci_level, seed = seed)
      ci_lower <- ci$lower; ci_upper <- ci$upper
      ci_source <- paste0("Non-parametric bootstrap of the parametric estimator (", bootstrap_R, " replicates)")
    } else if (identical(diagnostic$method, "parametric_bc")) {
      method <- "Parametric with Box-Cox (n<120)"
      est <- estimate_parametric_bc(x, coverage, ci_level, bootstrap_R, seed)
      ri <- est$ri; ci_lower <- est$ci_lower; ci_upper <- est$ci_upper
      ci_source <- paste0("Bootstrap with Box-Cox refitting (", bootstrap_R, " replicates)")
    } else if (identical(diagnostic$method, "robust") && pkg_available("referenceIntervals")) {
      method <- "Robust (Horn-Pesce; n<120; no evidence of asymmetry)"
      rr <- referenceIntervals::refLimit(x, out.method = "horn", out.rm = FALSE,
                                         IR = "r", CI = "boot", refConf = coverage,
                                         limitConf = ci_level, bootStat = "perc")
      ri <- as.numeric(rr$Ref_Int); ci_lower <- as.numeric(rr$Conf_Int[1:2]); ci_upper <- as.numeric(rr$Conf_Int[3:4])
      ci_source <- "Bootstrap of the robust method (referenceIntervals)"
    } else if (identical(diagnostic$method, "robust_bc") && pkg_available("referenceIntervals")) {
      method <- "Robust after Box-Cox (n<120)"
      est <- estimate_robust_bc(x, coverage, ci_level, min(bootstrap_R, 1000), seed)
      ri <- est$ri; ci_lower <- est$ci_lower; ci_upper <- est$ci_upper
      ci_source <- paste0("Bootstrap with Box-Cox refitting + robust method (", min(bootstrap_R,1000), " replicates)")
    } else {
      method <- "Exploratory non-parametric (n<120; not suitable for automatic closure)"
      ri <- nonparametric_ri(x, coverage)
      ci <- bootstrap_limits(x, function(z) nonparametric_ri(z, coverage), R = bootstrap_R, conf = ci_level, seed = seed)
      ci_lower <- ci$lower; ci_upper <- ci$upper
      ci_source <- paste0("Exploratory non-parametric bootstrap (", bootstrap_R, " replicates)")
    }
  }

  pr <- precision_ratio(ri, ci_lower, ci_upper)
  precision_ok <- all(is.finite(pr[active_limits])) && all(pr[active_limits] < 0.20)

  if (isTRUE(n >= 120 && precision_ok)) {
    status <- "green"
    recommendation <- "The sample size supports the standard non-parametric procedure and the precision of the active limit(s) is adequate. Normality is not required for this pathway."
  } else if (isTRUE(n >= 120 && !precision_ok)) {
    status <- "yellow"
    recommendation <- "Although n≥120, at least one active limit has a 90% CI that is too wide relative to the RI width. Increasing the sample size is recommended before finalizing the interval."
  } else if (isTRUE(diagnostic$method %in% c("parametric", "parametric_bc", "robust", "robust_bc")) && isTRUE(precision_ok)) {
    status <- "yellow"
    recommendation <- paste0("With n<120, RIveR selected ", method, " because its assumptions are defensible and the precision of the active limit(s) meets the criterion. Adoption is conditional and requires specialist justification.")
  } else {
    status <- "red"
    recommendation <- paste0("Establishing a final RI with the current data is not recommended. ", diagnostic$reason %||% "Increase the sample size or use an appropriately validated alternative strategy.")
  }

  base_status <- status
  base_recommendation <- recommendation
  unreviewed_extremes <- values_not_reviewed(outlier_assessment$extreme_values, reviewed_extreme_values)
  if (length(unreviewed_extremes) > 0) {
    if (!identical(status, "red")) status <- "yellow"
    recommendation <- paste0(base_recommendation, " Detected ", outlier_assessment$extreme_n,
                             " extreme observation(s) identified by Dixon/Reed and/or Tukey. Review the cause and document whether they should be retained or excluded before closing the study.")
  } else if (length(outlier_assessment$extreme_values) > 0) {
    recommendation <- paste0(base_recommendation, " The detected extreme values have been reviewed and their retention in the reference dataset has been documented.")
  }

  list(
    type = "direct_establishment", n = n, method = method, route = route,
    reference_design = design,
    ri = setNames(ri, c("lower", "upper")),
    ci90_lower = setNames(ci_lower, c("low", "high")),
    ci90_upper = setNames(ci_upper, c("low", "high")),
    precision_ratio = pr, precision_ok = precision_ok,
    normality_p = distribution$original$ad$p.value,
    symmetry_p = distribution$original$symmetry$p.value,
    boxcox_normality_p = distribution$boxcox$ad$p.value,
    boxcox_symmetry_p = distribution$boxcox$symmetry$p.value,
    boxcox_lambda = distribution$boxcox$lambda,
    boxcox_shift = distribution$boxcox$shift,
    bowley = bowley_skewness(x), distribution = distribution,
    method_selection_reason = diagnostic$reason,
    outlier_assessment = outlier_assessment,
    base_status = base_status, base_recommendation = base_recommendation,
    reviewed_extreme_values = reviewed_extreme_values,
    unreviewed_extreme_values = unreviewed_extremes,
    outlier_review_required = length(unreviewed_extremes) > 0,
    suspicious_outliers = suspicious_horn, suspicious_outlier_n = length(suspicious_horn),
    display_digits = display_digits, precision_threshold = 0.20,
    ci_source = ci_source, status = status, recommendation = recommendation
  )
}

direct_distribution_display_table <- function(result) {
  if (is.null(result) || !identical(result$type, "direct_establishment") || is.null(result$distribution)) return(NULL)
  d <- result$distribution
  ptxt <- function(p) format_p_value(p)
  interpret_norm <- function(p) if (!is.finite(p)) status_symbol_text("grey", "Not evaluable") else if (p < .05) status_symbol_text("red", "Normality not defensible for the parametric method") else status_symbol_text("green", "Normality is not questioned")
  interpret_sym <- function(p) if (!is.finite(p)) status_symbol_text("grey", "Not evaluable") else if (p < .05) status_symbol_text("red", "Symmetry not defensible for the robust method") else status_symbol_text("green", "No evidence of asymmetry detected")
  interpret_sym_bc <- function(p) {
    if (isTRUE(d$boxcox$symmetry_inherited)) {
      base <- if (!is.finite(p)) status_symbol_text("grey", "Not evaluable") else if (p < .05) status_symbol_text("red", "Symmetry not defensible") else status_symbol_text("green", "No evidence of asymmetry")
      return(paste0(base, " · minimal transformation; original diagnostic retained"))
    }
    interpret_sym(p)
  }
  data.frame(
    Assessment = c("Anderson-Darling", "Symmetry (McWilliams-type runs test)", "Anderson-Darling after Box-Cox", "Symmetry after Box-Cox", "Lambda Box-Cox", "Box-Cox shift"),
    Result = c(ptxt(d$original$ad$p.value), ptxt(d$original$symmetry$p.value), ptxt(d$boxcox$ad$p.value), ptxt(d$boxcox$symmetry$p.value),
                  if (is.finite(d$boxcox$lambda)) formatC(d$boxcox$lambda, format="f", digits=3, decimal.mark=",") else "—",
                  if (is.finite(d$boxcox$shift)) format_lab_number(d$boxcox$shift, result$display_digits %||% 2) else "—"),
    Interpretation = c(interpret_norm(d$original$ad$p.value), interpret_sym(d$original$symmetry$p.value), interpret_norm(d$boxcox$ad$p.value), interpret_sym_bc(d$boxcox$symmetry$p.value),
                       if (!isTRUE(d$boxcox$ok)) "Box-Cox unavailable" else if (is.finite(d$boxcox$ad$p.value) && d$boxcox$ad$p.value < .05 && is.finite(d$boxcox$symmetry$p.value) && d$boxcox$symmetry$p.value < .05) status_symbol_text("red", "Does not achieve an adequate model") else if (is.finite(d$boxcox$lambda) && abs(d$boxcox$lambda - 1) < .10) "Minimal transformation" else if (is.finite(d$boxcox$lambda) && abs(d$boxcox$lambda) < .10) "Close to a log transformation" else status_symbol_text("green", "Improves model adequacy"),
                       if (isTRUE(d$boxcox$shift > 0)) "Technical shift applied" else "Not required"),
    check.names=FALSE, stringsAsFactors=FALSE
  )
}

direct_method_candidates_display_table <- function(result) {
  if (is.null(result) || !identical(result$type, "direct_establishment") || is.null(result$distribution)) return(NULL)
  d <- result$distribution; digs <- result$display_digits %||% 2
  nr <- result$n >= 120
  norm0 <- is.finite(d$original$ad$p.value) && d$original$ad$p.value >= .05
  sym0 <- is.finite(d$original$symmetry$p.value) && d$original$symmetry$p.value >= .05
  normbc <- isTRUE(d$boxcox$ok) && is.finite(d$boxcox$ad$p.value) && d$boxcox$ad$p.value >= .05
  symbc <- isTRUE(d$boxcox$ok) && is.finite(d$boxcox$symmetry$p.value) && d$boxcox$symmetry$p.value >= .05

  selected_param <- identical(result$method,"Standard parametric (n<120; normality not questioned)")
  selected_robust <- grepl("^Robust \\(Horn-Pesce", result$method)
  selected_pbc <- grepl("^Parametric with Box-Cox", result$method)
  selected_rbc <- grepl("^Robust after Box-Cox", result$method)

  role <- function(selected, assumption_ok, secondary = FALSE, n_ge_120 = FALSE) {
    if (isTRUE(selected)) return("SELECTED")
    if (!isTRUE(assumption_ok)) return("Not recommended")
    if (isTRUE(n_ge_120)) return("Not required with n≥120")
    if (isTRUE(secondary)) return("Secondary alternative")
    "Candidate"
  }

  param_role <- role(selected_param, norm0, secondary = FALSE, n_ge_120 = nr)
  # If Box-Cox restores normality, the original robust method remains a secondary alternative,
  # even if McWilliams does not detect asymmetry.
  robust_role <- role(selected_robust, sym0, secondary = (!nr && !norm0 && normbc), n_ge_120 = nr)
  pbc_role <- role(selected_pbc, normbc, secondary = FALSE, n_ge_120 = nr)
  robust_bc_role <- role(selected_rbc, symbc, secondary = (!nr && normbc), n_ge_120 = nr)

  rows <- data.frame(
    Method = c("Standard parametric", "Robust", "Parametric + Box-Cox", "Robust + Box-Cox", "Non-parametric"),
    IR = c(format_lab_interval(d$candidate_ri$parametric,digs), format_lab_interval(d$candidate_ri$robust,digs), format_lab_interval(d$candidate_ri$parametric_bc,digs), format_lab_interval(d$candidate_ri$robust_bc,digs), format_lab_interval(d$candidate_ri$nonparametric,digs)),
    `Main assumption` = c(if (norm0) status_symbol_text("green","Normality not questioned") else status_symbol_text("red","Normality not defensible"),
                            if (sym0) status_symbol_text("green","No evidence of asymmetry") else status_symbol_text("red","Symmetry not defensible"),
                            if (normbc) status_symbol_text("green","Normality not questioned after Box-Cox") else status_symbol_text("red","Normality not defensible after Box-Cox"),
                            if (symbc) status_symbol_text("green","No evidence of asymmetry after Box-Cox") else status_symbol_text("red","Symmetry not defensible after Box-Cox"),
                            if (nr) status_symbol_text("green","Standard pathway with n≥120") else status_symbol_text("yellow","Exploratory with n<120")),
    Paper = c(param_role,
              robust_role,
              pbc_role,
              robust_bc_role,
              if (nr) "SELECTED" else if (grepl("Exploratory non-parametric", result$method)) "SHOWN FOR EXPLORATORY PURPOSES ONLY" else "Exploratory reference"),
    check.names=FALSE, stringsAsFactors=FALSE
  )
  rows$IR[5] <- format_lab_interval(if (nr) result$ri else d$candidate_ri$nonparametric %||% c(NA,NA), digs)
  rows
}

run_direct_verification <- function(x, target_lower = NA_real_, target_upper = NA_real_, verification_round = NULL, allow_embedded_second = FALSE, reference_design = NULL) {
  keep <- is.finite(x)
  x <- x[keep]
  if (!is.null(verification_round) && length(verification_round) == length(keep)) verification_round <- verification_round[keep]
  design <- normalize_reference_design(reference_design)
  active <- design$active
  target <- c(lower = suppressWarnings(as.numeric(target_lower))[1], upper = suppressWarnings(as.numeric(target_upper))[1])
  if (isTRUE(active[["lower"]]) && !is.finite(target[["lower"]])) stop("Enter a valid candidate lower limit.")
  if (isTRUE(active[["upper"]]) && !is.finite(target[["upper"]])) stop("Enter a valid candidate upper limit.")
  if (all(active) && target[["lower"]] >= target[["upper"]]) stop("Enter a valid candidate RI with LRL < URL.")

  n <- length(x)
  if (!isTRUE(allow_embedded_second) && n > 20L) {
    return(list(type = "direct_verification", n = n, reference_design = design, status = "grey", decision = "NOT EVALUABLE",
                outside_first20 = NA_integer_, outside_second20 = NA_integer_,
                first_n = n, second_n = 0L, cohort_assignment = "Sequential workflow required",
                traceability_warning = FALSE,
                target = target,
                recommendation = paste0("The first direct-verification upload must correspond to a single cohort of 20 individuals. The upload contains ", n, ". Analyze cohort 1 first and, only if the result is inconclusive (3–4/20 outside), subsequently add a second independent file containing 20 individuals.")))
  }
  if (n < 20) {
    return(list(type = "direct_verification", n = n, reference_design = design, status = "grey", decision = "NOT EVALUABLE",
                outside_first20 = NA_integer_, outside_second20 = NA_integer_,
                first_n = n, second_n = 0L, cohort_assignment = "Not applicable",
                traceability_warning = FALSE,
                target = target,
                recommendation = "Twenty valid reference individuals are required for direct verification. If any result is excluded for a documented reason, it must be replaced until 20 evaluable results are again available."))
  }

  outside <- function(z) {
    flag <- rep(FALSE, length(z))
    if (isTRUE(active[["lower"]])) flag <- flag | z < target[["lower"]]
    if (isTRUE(active[["upper"]])) flag <- flag | z > target[["upper"]]
    sum(flag, na.rm = TRUE)
  }

  round_supplied <- !is.null(verification_round) && length(verification_round) == length(x)
  vr <- if (round_supplied) suppressWarnings(as.integer(verification_round)) else integer(0)
  if (round_supplied && any(!is.finite(vr) | !vr %in% c(1L, 2L))) {
    return(list(type = "direct_verification", n = n, reference_design = design, status = "grey", decision = "NOT EVALUABLE",
                outside_first20 = NA_integer_, outside_second20 = NA_integer_,
                first_n = sum(vr == 1L, na.rm=TRUE), second_n = sum(vr == 2L, na.rm=TRUE),
                cohort_assignment = "Invalid cohort column", traceability_warning = TRUE,
                target = target,
                recommendation = "The cohort column must contain only values 1 or 2 for all evaluable results."))
  }
  round_mode <- isTRUE(round_supplied)
  traceability_warning <- FALSE

  if (round_mode) {
    first <- x[vr == 1L]
    second <- x[vr == 2L]
    if (length(first) != 20L) {
      return(list(type = "direct_verification", n = n, reference_design = design, status = "grey", decision = "NOT EVALUABLE",
                  outside_first20 = NA_integer_, outside_second20 = NA_integer_,
                  first_n = length(first), second_n = length(second),
                  cohort_assignment = "Explicit cohort column",
                  traceability_warning = FALSE,
                  target = target,
                  recommendation = paste0("The first verification cohort must contain exactly 20 valid reference individuals; ", length(first), ".")))
    }
    if (length(second) > 0L && length(second) != 20L) {
      return(list(type = "direct_verification", n = n, reference_design = design, status = "grey", decision = "NOT EVALUABLE",
                  outside_first20 = outside(first), outside_second20 = NA_integer_,
                  first_n = length(first), second_n = length(second),
                  cohort_assignment = "Explicit cohort column",
                  traceability_warning = FALSE,
                  target = target,
                  recommendation = paste0("When provided, the second cohort must contain exactly 20 valid reference individuals; ", length(second), ".")))
    }
    cohort_assignment <- "Explicit cohort column (1/2)"
  } else {
    if (!n %in% c(20L, 40L)) {
      return(list(type = "direct_verification", n = n, reference_design = design, status = "grey", decision = "NOT EVALUABLE",
                  outside_first20 = NA_integer_, outside_second20 = NA_integer_,
                  first_n = min(n, 20L), second_n = max(0L, n - 20L),
                  cohort_assignment = "Not defined", traceability_warning = TRUE,
                  target = target,
                  recommendation = paste0("Without an explicit cohort column, the upload must contain exactly 20 individuals (first cohort) or 40 individuals (two cohorts). The upload contains ", n, ". RIveR does not silently use only the first 20 results.")))
    }
    first <- x[seq_len(20)]
    second <- if (n == 40L) x[21:40] else numeric(0)
    cohort_assignment <- if (n == 40L) "Row order (1–20 / 21–40)" else "Single cohort of 20"
    traceability_warning <- isTRUE(n == 40L)
  }

  out1 <- outside(first)
  out2 <- NA_integer_
  ext1 <- tryCatch(detect_direct_outliers(first), error = function(e) NULL)
  ext2 <- NULL
  extreme_first_n <- if (!is.null(ext1)) as.integer(ext1$extreme_n %||% 0L) else 0L

  if (out1 <= 2) {
    status <- "green"
    decision <- "VERIFIED"
    rec <- paste0(out1, "/20 results outside the RI. The candidate interval meets the direct verification criterion.")
  } else if (out1 %in% 3:4) {
    if (length(second) < 20L) {
      status <- "yellow"
      decision <- "INCONCLUSIVE"
      rec <- paste0(out1, "/20 results outside. A second independent cohort of 20 reference individuals is required to complete the procedure.")
    } else {
      out2 <- outside(second)
      ext2 <- tryCatch(detect_direct_outliers(second), error = function(e) NULL)
      if (out2 <= 2) {
        status <- "green"
        decision <- "VERIFIED AFTER THE SECOND SAMPLE"
        rec <- paste0("First cohort: ", out1, "/20 outside; second cohort: ", out2, "/20 outside. The candidate RI is verified after repetition.")
      } else {
        status <- "red"
        decision <- "NOT VERIFIED"
        rec <- paste0("First cohort: ", out1, "/20 outside; second cohort: ", out2, "/20 outside. The candidate RI is not verified.")
      }
    }
  } else {
    status <- "red"
    decision <- "NOT VERIFIED"
    rec <- paste0(out1, "/20 results outside the RI. The candidate interval does not meet the direct verification criterion.")
  }

  if (isTRUE(traceability_warning) && out1 %in% 3:4 && length(second) >= 20L) {
    rec <- paste0(rec, " Traceability warning: cohorts were inferred from row order; for a final study, explicitly assign cohort 1/2.")
    if (identical(status, "green")) status <- "yellow"
  }

  extreme_second_n <- if (!is.null(ext2)) as.integer(ext2$extreme_n %||% 0L) else 0L
  extreme_policy <- "In direct verification, no result is excluded solely because it is statistically extreme. All valid reference individuals count against the candidate RI; exclusion requires a documented reason independent of the value."
  if (extreme_first_n > 0L || extreme_second_n > 0L) {
    rec <- paste0(rec, " Statistical extreme-value signals were detected (cohort 1: ", extreme_first_n,
                  if (length(second)) paste0("; cohort 2: ", extreme_second_n) else "",
                  "), but they were retained in the count in the absence of a documented reason for exclusion.")
  }

  list(type = "direct_verification", n = n, status = status, decision = decision,
       reference_design = design,
       outside_first20 = out1, outside_second20 = out2,
       first_n = length(first), second_n = length(second),
       cohort_assignment = cohort_assignment,
       traceability_warning = traceability_warning,
       extreme_first_n = extreme_first_n, extreme_second_n = extreme_second_n,
       extreme_policy = extreme_policy,
       target = target, recommendation = rec)
}

direct_verification_display_table <- function(result) {
  if (is.null(result) || !identical(result$type, "direct_verification")) return(NULL)
  out2 <- if (is.finite(result$outside_second20 %||% NA_real_)) paste0(result$outside_second20, "/20") else "—"
  steps <- c("Candidate RI", "Cohort assignment", "First cohort", "Second cohort")
  vals <- c(
    reference_result_text(result$target, result$reference_design %||% make_reference_design("two_sided", 0.95), result$display_digits %||% 2),
    result$cohort_assignment %||% "—",
    if (is.finite(result$outside_first20 %||% NA_real_)) paste0(result$outside_first20, "/20 outside") else paste0(result$first_n %||% result$n, " individuals"),
    if ((result$second_n %||% 0L) > 0L) paste0(out2, " outside") else "Not provided / not required"
  )
  src <- result$cohort_sources %||% NULL
  if (!is.null(src) && length(src)) {
    steps <- c(steps, "Cohort 1 file", "Cohort 2 file")
    vals <- c(vals, src[[1]] %||% "—", if (length(src) >= 2 && nzchar(src[[2]] %||% "")) src[[2]] else "—")
  }
  if (!is.null(result$extreme_policy)) {
    steps <- c(steps, "Statistical extreme-value signals", "Exclusion policy")
    ext_txt <- paste0("Cohort 1: ", result$extreme_first_n %||% 0L)
    if ((result$second_n %||% 0L) > 0L) ext_txt <- paste0(ext_txt, " · Cohort 2: ", result$extreme_second_n %||% 0L)
    vals <- c(vals, ext_txt, result$extreme_policy)
  }
  if (!is.null(result$cohort_exclusions) && is.data.frame(result$cohort_exclusions) && nrow(result$cohort_exclusions)) {
    steps <- c(steps, "Justified cohort exclusions")
    vals <- c(vals, paste0(nrow(result$cohort_exclusions), " individual(s); see review traceability"))
  }
  if (length(result$replacement_sources %||% character(0))) {
    steps <- c(steps, "Replacement/completion file(s)")
    vals <- c(vals, paste(unique(result$replacement_sources), collapse = " · "))
  }
  data.frame(
    Step = c(steps, "Decision"),
    Result = c(vals, result$decision %||% "—"),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}


verification_partition_display_label <- function(x) {
  z <- trimws(as.character(x %||% ""))
  if (!nzchar(z)) "Partition" else z
}

assign_direct_verification_partitions <- function(data, definitions, reference_design = NULL) {
  if (is.null(data) || !is.data.frame(data) || !"value" %in% names(data)) {
    return(list(ok = FALSE, message = "No prepared data are available for partitioned verification."))
  }
  if (is.null(definitions) || !is.data.frame(definitions) || !nrow(definitions)) {
    return(list(ok = FALSE, message = "The partitions and corresponding candidate RIs have not been defined."))
  }
  req_cols <- c("key","label","type","lower","upper")
  if (!all(req_cols %in% names(definitions))) {
    return(list(ok = FALSE, message = "The partition definition is incomplete."))
  }
  design <- normalize_reference_design(reference_design)
  active <- design$active
  bad <- rep(FALSE, nrow(definitions))
  if (isTRUE(active[["lower"]])) bad <- bad | !is.finite(definitions$lower)
  if (isTRUE(active[["upper"]])) bad <- bad | !is.finite(definitions$upper)
  if (isTRUE(active[["lower"]]) && isTRUE(active[["upper"]])) bad <- bad | definitions$lower >= definitions$upper
  if (any(bad)) {
    return(list(ok = FALSE, message = "Each partition must have valid active reference limit(s) for the selected design."))
  }
  if ("age_min_valid" %in% names(definitions) && any(!definitions$age_min_valid)) {
    return(list(ok = FALSE, message = "There is a non-numeric minimum age in the range definition."))
  }
  if ("age_max_valid" %in% names(definitions) && any(!definitions$age_max_valid)) {
    return(list(ok = FALSE, message = "There is a non-numeric maximum age in the range definition."))
  }

  n <- nrow(data)
  hit <- matrix(FALSE, nrow = n, ncol = nrow(definitions))
  for (i in seq_len(nrow(definitions))) {
    tp <- definitions$type[i]
    if (identical(tp, "sex")) {
      if (!"sex" %in% names(data)) {
        return(list(ok = FALSE, message = "To verify RIs partitioned by sex, the sex/group column must be assigned."))
      }
      val <- tolower(trimws(as.character(definitions$sex_value[i] %||% "")))
      sx <- tolower(trimws(as.character(data$sex)))
      hit[, i] <- !is.na(sx) & nzchar(sx) & sx == val
    } else if (identical(tp, "age")) {
      if (!"age" %in% names(data)) {
        return(list(ok = FALSE, message = "To verify RIs partitioned by age, the age column must be assigned."))
      }
      amin <- definitions$age_min[i]
      amax <- definitions$age_max[i]
      amin <- if (is.finite(amin)) amin else -Inf
      amax <- if (is.finite(amax)) amax else Inf
      hit[, i] <- is.finite(data$age) & data$age >= amin & data$age < amax
    } else {
      return(list(ok = FALSE, message = paste0("Unrecognized partition type: ", tp, ".")))
    }
  }

  nhit <- rowSums(hit)
  if (any(nhit == 0L)) {
    return(list(ok = FALSE, message = paste0("There are ", sum(nhit == 0L), " result(s) that cannot be assigned to any defined partition.")))
  }
  if (any(nhit > 1L)) {
    return(list(ok = FALSE, message = paste0("There are ", sum(nhit > 1L), " result(s) assignable to more than one partition. Review the range limits.")))
  }
  idx <- max.col(hit, ties.method = "first")
  list(ok = TRUE, index = idx, key = definitions$key[idx], label = definitions$label[idx])
}

run_direct_verification_partitioned <- function(data, definitions,
                                                second_cohorts = list(),
                                                first_source = NULL,
                                                second_sources = list(),
                                                reference_design = NULL) {
  design <- normalize_reference_design(reference_design)
  active <- design$active
  ass <- assign_direct_verification_partitions(data, definitions, reference_design = design)
  if (!isTRUE(ass$ok)) {
    return(list(type = "direct_verification", partitioned = TRUE, reference_design = design,
                partition_type = if (!is.null(definitions) && nrow(definitions)) definitions$type[1] else "unknown",
                n = if (is.data.frame(data)) nrow(data) else 0L,
                status = "grey", decision = "NOT EVALUABLE",
                partition_results = list(), pending_partitions = character(0),
                definitions = definitions,
                recommendation = ass$message %||% "Partitioned verification cannot be run."))
  }

  results <- list()
  total_n <- 0L
  for (i in seq_len(nrow(definitions))) {
    key <- as.character(definitions$key[i])
    lab <- verification_partition_display_label(definitions$label[i])
    d1 <- data[ass$index == i, , drop = FALSE]
    x1 <- d1$value[is.finite(d1$value)]
    total_n <- total_n + length(x1)
    sec <- second_cohorts[[key]] %||% NULL
    x2 <- if (is.null(sec)) numeric(0) else suppressWarnings(as.numeric(sec$value %||% numeric(0)))
    x2 <- x2[is.finite(x2)]
    total_n <- total_n + length(x2)

    if (length(x1) != 20L) {
      rr <- list(type = "direct_verification", n = length(x1), status = "grey",
                 decision = "NOT EVALUABLE", outside_first20 = NA_integer_, outside_second20 = NA_integer_,
                 first_n = length(x1), second_n = length(x2), cohort_assignment = "Predefined partition",
                 traceability_warning = FALSE,
                 target = c(lower = definitions$lower[i], upper = definitions$upper[i]),
                 recommendation = paste0("The partition '", lab, "' must contain exactly 20 reference individuals in the first cohort; n=", length(x1), "."))
    } else if (length(x2) > 0L && length(x2) != 20L) {
      rr <- list(type = "direct_verification", n = length(x1) + length(x2), status = "grey",
                 decision = "NOT EVALUABLE", outside_first20 = sum((if (active[["lower"]]) x1 < definitions$lower[i] else FALSE) |
                                       (if (active[["upper"]]) x1 > definitions$upper[i] else FALSE)),
                 outside_second20 = NA_integer_, first_n = 20L, second_n = length(x2),
                 cohort_assignment = "Predefined partition",
                 traceability_warning = FALSE,
                 target = c(lower = definitions$lower[i], upper = definitions$upper[i]),
                 recommendation = paste0("The second cohort for partition '", lab, "' must contain exactly 20 individuals; n=", length(x2), "."))
    } else {
      xx <- if (length(x2)) c(x1, x2) else x1
      vr <- if (length(x2)) c(rep(1L, 20L), rep(2L, 20L)) else NULL
      rr <- run_direct_verification(xx, definitions$lower[i], definitions$upper[i],
                                    verification_round = vr,
                                    allow_embedded_second = length(x2) == 20L,
                                    reference_design = design)
      rr$cohort_assignment <- paste0("Predefined partition · ", lab)
    }
    rr$partition_key <- key
    rr$partition_label <- lab
    rr$cohort_sources <- c(first_source %||% "—", second_sources[[key]] %||% "")
    results[[key]] <- rr
  }

  decisions <- vapply(results, function(z) z$decision %||% "NOT EVALUABLE", character(1))
  pending <- names(results)[decisions == "INCONCLUSIVE"]
  failed <- names(results)[decisions == "NOT VERIFIED"]
  not_eval <- names(results)[decisions == "NOT EVALUABLE"]

  if (length(not_eval)) {
    status <- "grey"; decision <- "NOT EVALUABLE"
  } else if (length(failed)) {
    status <- "red"; decision <- "NOT VERIFIED"
  } else if (length(pending)) {
    status <- "yellow"; decision <- "INCONCLUSIVE"
  } else {
    status <- "green"; decision <- "VERIFIED"
  }

  lab_for <- function(keys) {
    if (!length(keys)) return(character(0))
    vapply(keys, function(k) results[[k]]$partition_label %||% k, character(1))
  }
  rec <- if (identical(decision, "VERIFIED")) {
    paste0("All partitions passed independent direct verification. ",
           paste(vapply(results, function(z) paste0(z$partition_label, ": ", z$decision), character(1)), collapse = " · "), ".")
  } else if (identical(decision, "INCONCLUSIVE")) {
    paste0("Verification is inconclusive in ", paste(lab_for(pending), collapse = ", "),
           ". Add a second independent cohort of 20 individuals only for each pending partition. Partitions already verified should not be repeated.")
  } else if (identical(decision, "NOT VERIFIED")) {
    txt <- paste0("The complete partitioned-RI scheme is not verified because the following fail: ", paste(lab_for(failed), collapse = ", "), ".")
    if (length(pending)) txt <- paste0(txt, " Partitions still pending a second cohort: ", paste(lab_for(pending), collapse = ", "), ".")
    txt
  } else {
    paste0("Partitioned verification is not evaluable in: ", paste(lab_for(not_eval), collapse = ", "), ". Review sample size, group assignment, and candidate RIs.")
  }

  list(type = "direct_verification", partitioned = TRUE, reference_design = design,
       partition_type = definitions$type[1],
       n = total_n, status = status, decision = decision,
       partition_results = results, pending_partitions = pending,
       definitions = definitions, recommendation = rec)
}

direct_partitioned_verification_display_table <- function(result) {
  if (is.null(result) || !isTRUE(result$partitioned) || !length(result$partition_results %||% list())) return(NULL)
  rows <- lapply(result$partition_results, function(z) {
    out1 <- if (is.finite(z$outside_first20 %||% NA_real_)) paste0(z$outside_first20, "/20 outside") else paste0(z$first_n %||% 0L, " individuals")
    out2 <- if (is.finite(z$outside_second20 %||% NA_real_)) paste0(z$outside_second20, "/20 outside") else if ((z$second_n %||% 0L) > 0L) paste0(z$second_n, " individuals") else "—"
    src <- z$cohort_sources %||% c("—","")
    data.frame(
      Partition = z$partition_label %||% z$partition_key %||% "—",
      `Candidate limit / RI` = reference_result_text(z$target, z$reference_design %||% result$reference_design, z$display_digits %||% 2),
      `First cohort` = out1,
      `Second cohort` = out2,
      `File 1` = src[[1]] %||% "—",
      `File 2` = if (length(src) >= 2 && nzchar(src[[2]] %||% "")) src[[2]] else "—",
      Decision = z$decision %||% "—",
      check.names = FALSE, stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

direct_outlier_display_table <- function(result) {
  if (is.null(result) || result$type != "direct_establishment" || is.null(result$outlier_assessment)) return(NULL)
  oa <- result$outlier_assessment
  dr <- oa$dixon_reed
  tk <- oa$tukey
  digits <- result$display_digits %||% 2
  fmt_vals <- function(v) {
    v <- v[is.finite(v)]
    if (!length(v)) return("—")
    txt <- vapply(v, format_lab_number, character(1), digits = digits)
    if (length(txt) > 8) paste0(paste(txt[1:8], collapse = ", "), " … (+", length(txt)-8, ")") else paste(txt, collapse = ", ")
  }
  flag_label <- function(flag) if (isTRUE(flag)) status_symbol_text("red", "Extreme-value signal") else status_symbol_text("green", "Not detected")
  data.frame(
    Method = c("Dixon/Reed", "Dixon/Reed", "Tukey", "Tukey"),
    Assessment = c("Lower extreme", "Upper extreme", "Suspected values (1.5–3 IQR)", "Extreme values (>3 IQR)"),
    `Statistic / criterion` = c(
      if (is.finite(dr$lower_ratio)) paste0("D/R = ", formatC(dr$lower_ratio, format="f", digits=3, decimal.mark=","), "; cut-off ≥0.333") else "—",
      if (is.finite(dr$upper_ratio)) paste0("D/R = ", formatC(dr$upper_ratio, format="f", digits=3, decimal.mark=","), "; cut-off ≥0.333") else "—",
      "Between the 1.5-IQR and 3-IQR limits",
      "Beyond 3 IQR"
    ),
    Result = c(
      flag_label(dr$lower_flag),
      flag_label(dr$upper_flag),
      if (oa$suspect_n > 0) status_symbol_text("yellow", paste0(oa$suspect_n, " moderately distant")) else status_symbol_text("green", "None detected"),
      if (length(c(tk$extreme_low, tk$extreme_high)) > 0) status_symbol_text("red", paste0(length(c(tk$extreme_low, tk$extreme_high)), " extreme")) else status_symbol_text("green", "None detected")
    ),
    Values = c(
      if (isTRUE(dr$lower_flag)) fmt_vals(dr$lower_value) else "—",
      if (isTRUE(dr$upper_flag)) fmt_vals(dr$upper_value) else "—",
      fmt_vals(c(tk$suspect_low, tk$suspect_high)),
      fmt_vals(c(tk$extreme_low, tk$extreme_high))
    ),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}
