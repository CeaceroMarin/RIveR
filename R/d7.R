# D7 · Indirect establishment of a reference interval ---------------------
# v0.15.0
#
# Principles:
# - refineR is the primary estimator.
# - reflimR provides an independent estimate and 95% CI when applicable.
# - Agreement between methods is evidence of robustness, not a vote.
# - RIbench provides methodological performance context for the algorithms; it does not validate
# the specific RI obtained from real data and cannot reveal the "true" RI.
# - Values are not removed arbitrarily simply because they are extreme.
# - A categorical partition candidate requires explicit criteria and then the
# complete D7 engine within each group.
# - A continuous age effect is not converted into arbitrary bands; it is modeled
# using local indirect estimates and a candidate continuous curve.
# - No result is implemented automatically: the output is a candidate for
# specialist approval, or a blocker/need for review.

D7_REFERENCES <- c(
  "Jones GRD et al. Indirect methods for reference interval determination – review and recommendations. Clin Chem Lab Med. 2019;57:20-29. doi:10.1515/cclm-2018-0073",
  "Ammer T et al. Estimation of Reference Intervals from Routine Data Using the refineR Algorithm—A Practical Guide. J Appl Lab Med. 2023;8:84-91. doi:10.1093/jalm/jfac101",
  "Ammer T et al. RIbench: A Proposed Benchmark for the Standardized Evaluation of Indirect Methods for Reference Interval Estimation. Clin Chem. 2022;68:1410-1424. doi:10.1093/clinchem/hvac142",
  "Lahti A et al. Objective criteria for partitioning Gaussian-distributed reference values into subgroups. Clin Chem. 2002;48:338-352. PMID:11805016",
  "Harris EK, Boyd JC. On dividing reference data into subgroups to produce separate reference ranges. Clin Chem. 1990;36:265-270. PMID:2302771",
  "Yildiz R et al. Indirect estimation of serum enzymes reference intervals in adults using the reflimR and refineR algorithms. Biochem Med (Zagreb). 2026;36:010706. doi:10.11613/BM.2026.010706"
)

d7_sample_context <- function(n) {
  n <- as.integer(n %||% 0L)
  if (n < 200L) {
    return(list(level = "not_evaluable", status = "grey", label = "Not evaluable",
                text = paste0("n=", n, ": below the internal operational minimum for automated indirect establishment in RIveR. No RI will be estimated, but the study can still be documented and exported.")))
  }
  if (n < 1000L) {
    return(list(level = "limited", status = "yellow", label = "Limited sample context",
                text = paste0("n=", n, ": estimation is possible, but robustness is limited. This range is an internal RIveR caution rule, not a universal minimum.")))
  }
  if (n < 5000L) {
    return(list(level = "moderate", status = "yellow", label = "Intermediate sample context",
                text = paste0("n=", n, ": the dataset is adequate for estimating a candidate RI, with additional caution because of sample size. RIbench is used only as methodological performance context for the algorithms, not as validation of this specific RI.")))
  }
  list(level = "favorable", status = "green", label = "Favorable sample context",
       text = paste0("n=", n, ": sample size favorable. In RIbench, many modern indirect methods performed well across numerous scenarios with large samples and limited pathological contamination; this does not constitute a universal threshold or validate the specific RI obtained here."))
}

d7_np_fraction <- function(refine, reflim = NULL) {
  if (!is.null(refine$fit)) {
    fit <- refine$fit
    pmed <- suppressWarnings(as.numeric(fit$PMed %||% NA_real_))[1]
    pfull <- suppressWarnings(as.numeric(fit$P %||% NA_real_))[1]
    use_bs <- identical(refine$point_method %||% "", "medianBS") && is.finite(pmed)
    p <- if (isTRUE(use_bs)) pmed else pfull
    if (length(p) && is.finite(p[1]) && p[1] >= 0 && p[1] <= 1) return(p[1])
  }
  rp <- suppressWarnings(as.numeric(reflim$perc_norm %||% NA_real_))
  if (length(rp) && is.finite(rp[1]) && rp[1] >= 0 && rp[1] <= 100) return(rp[1] / 100)
  NA_real_
}

d7_pathology_context <- function(refine, reflim = NULL) {
  np <- d7_np_fraction(refine, reflim)
  if (!is.finite(np)) {
    return(list(level = "unknown", status = "grey", np_fraction = NA_real_,
                text = "The non-pathological fraction could not be estimated in an interpretable way; population composition should be reviewed using metadata and selection criteria."))
  }
  pct <- round(100 * np, 1)
  if (np >= 0.80) {
    return(list(level = "favorable", status = "green", np_fraction = np,
                text = paste0("Estimated non-pathological fraction ≈", pct, "%. This is a favorable context for indirect estimation; the value is a model estimate, not an individual clinical classification.")))
  }
  if (np >= 0.70) {
    return(list(level = "intermediate", status = "yellow", np_fraction = np,
                text = paste0("Estimated non-pathological fraction ≈", pct, "% (pathological fraction ≈", round(100*(1-np),1), "%). refineR may still be useful, but population review and external validation should be strengthened.")))
  }
  if (np >= 0.50) {
    return(list(level = "high_pathology", status = "yellow", np_fraction = np,
                text = paste0("Estimated non-pathological fraction ≈", pct, "%. Estimated pathological contamination is high; RIveR will not consider a new RI robust without reviewing/filtering the population and repeating the estimation.")))
  }
  list(level = "assumption_compromised", status = "red", np_fraction = np,
       text = paste0("Estimated non-pathological fraction ≈", pct, "%. Most observations do not appear to correspond to the non-pathological distribution, compromising a basic assumption of indirect methods; an RI should not be established from this dataset."))
}

d7_refine_ci <- function(refine) {
  tab <- refine$table %||% NULL
  out <- c(lower_low = NA_real_, lower_high = NA_real_, upper_low = NA_real_, upper_high = NA_real_)
  if (is.null(tab) || !is.data.frame(tab) || !all(c("CILow", "CIHigh") %in% names(tab))) return(out)
  design <- normalize_reference_design(refine$reference_design)
  p_lo <- design$pair_percentiles[["lower"]]
  p_hi <- design$pair_percentiles[["upper"]]

  # v0.15.3: same approach as for PointEst. If the percentiles are known
  # When the requested order and the number of files match, use the returned position.
  req <- suppressWarnings(as.numeric(refine$requested_percentiles %||% numeric(0)))
  req <- sort(unique(req[is.finite(req)]))
  if (length(req) && nrow(tab) == length(req)) {
    ilo <- which.min(abs(req - p_lo))
    ihi <- which.min(abs(req - p_hi))
    out[] <- suppressWarnings(as.numeric(c(tab$CILow[ilo], tab$CIHigh[ilo],
                                            tab$CILow[ihi], tab$CIHigh[ihi])))
    return(out)
  }

  # Fallback using the Percentile label.
  if ("Percentile" %in% names(tab)) {
    lo <- if (exists("rilctms_percentile_index", mode="function"))
      rilctms_percentile_index(tab$Percentile, p_lo) else which.min(abs(as.numeric(tab$Percentile) - p_lo))
    hi <- if (exists("rilctms_percentile_index", mode="function"))
      rilctms_percentile_index(tab$Percentile, p_hi) else which.min(abs(as.numeric(tab$Percentile) - p_hi))
    if (length(lo) && length(hi) && is.finite(lo) && is.finite(hi)) {
      out[] <- suppressWarnings(as.numeric(c(tab$CILow[lo], tab$CIHigh[lo],
                                              tab$CILow[hi], tab$CIHigh[hi])))
    }
  }
  out
}

d7_reflim_ci <- function(reflim) {
  out <- c(lower_low = NA_real_, lower_high = NA_real_, upper_low = NA_real_, upper_high = NA_real_)
  ci <- reflim$confidence.int %||% NULL
  if (is.null(ci)) return(out)
  nm <- names(ci) %||% character(0)
  if (length(nm)) {
    getv <- function(pattern) {
      ii <- grep(pattern, nm, ignore.case = TRUE)
      if (length(ii)) suppressWarnings(as.numeric(ci[ii[1]])) else NA_real_
    }
    out[] <- c(getv("lower.*low"), getv("lower.*upp|lower.*high"),
               getv("upper.*low"), getv("upper.*upp|upper.*high"))
    if (all(is.finite(out))) return(out)
  }
  z <- suppressWarnings(as.numeric(unlist(ci, use.names = FALSE)))
  if (length(z) >= 4L) out[] <- z[1:4]
  out
}

d7_compare_refine_reflim <- function(refine_ri, reflim_ri, reference_design = NULL) {
  design <- normalize_reference_design(reference_design)
  active <- design$active
  out <- list(status = "grey", limit_status = c(lower = "grey", upper = "grey"),
              message = "Agreement between refineR and reflimR could not be quantified.", interpretation = NULL)
  if (!identical(design$tail, "two_sided")) {
    out$message <- "In one-sided mode, reflimR 1.1.0 estimates P2.5/P97.5 and is not directly comparable with the active P5/P95; it is used only as descriptive support."
    return(out)
  }
  if (length(refine_ri) != 2L || length(reflim_ri) != 2L ||
      any(!is.finite(c(refine_ri, reflim_ri))) || any(c(refine_ri, reflim_ri) <= 0)) return(out)
  ip <- tryCatch(reflimR::interpretation(as.numeric(refine_ri), as.numeric(reflim_ri)), error = function(e) e)
  if (inherits(ip, "error")) {
    out$message <- paste("refineR and reflimR could not be compared:", conditionMessage(ip))
    return(out)
  }
  ls <- reflim_limit_statuses(ip)
  ls[!active] <- "grey"
  st <- reflim_overall_status(ls, active)
  out$status <- st
  out$limit_status <- ls
  out$interpretation <- ip
  out$message <- switch(st,
    green = "The refineR and reflimR estimates agree within the equivalence limits (EL).",
    yellow = "The refineR and reflimR estimates show an intermediate discrepancy in at least one limit.",
    red = "The refineR and reflimR estimates show a marked discrepancy in at least one limit.",
    "The agreement between methods is not evaluable."
  )
  out
}

d7_reflim_result <- function(x, reference_design = NULL) {
  design <- normalize_reference_design(reference_design)
  refl_raw <- tryCatch(run_reflim(x, reference_design = design), error = function(e) e)
  if (inherits(refl_raw, "error")) {
    return(list(available = FALSE, comparable = FALSE,
                message = paste("reflimR unavailable/uninterpretable:", conditionMessage(refl_raw))))
  }
  rri <- extract_reflim_ri(refl_raw)
  ok <- length(rri) == 2L && all(is.finite(rri))
  comparable <- ok && identical(design$tail, "two_sided")
  list(available = ok, comparable = comparable, fit = refl_raw, ri = rri, reference_design = design,
       perc_norm = suppressWarnings(as.numeric(refl_raw$perc.norm %||% NA_real_)),
       message = if (!ok) "reflimR did not return finite limits" else if (comparable)
         "Independent P2.5/P97.5 estimate available"
       else "Descriptive P2.5/P97.5 support available; not directly comparable with the active P5/P95")
}

d7_candidate_text <- function(refine) {
  design <- normalize_reference_design(refine$reference_design)
  digs <- 5
  reference_result_text(refine$ri, design, digs)
}

d7_core_decision <- function(sc, refine, reflim, agreement, pathology_context) {
  if ((pathology_context$level %||% "unknown") %in% c("high_pathology", "assumption_compromised")) {
    return(list(status = "yellow", decision = "INCONCLUSIVE",
                recommendation = paste0("The estimated dataset composition is not sufficiently favorable to establish a robust RI. ", pathology_context$text),
                action = "Review extraction criteria and metadata to reduce pathological contamination; repeat D7 before proposing an RI for approval."))
  }
  if (!isTRUE(reflim$available)) {
    return(list(status = "yellow", decision = "INCONCLUSIVE",
                recommendation = paste0("refineR estimates ", d7_candidate_text(refine), ", but an interpretable independent reflimR estimate is unavailable."),
                action = "Do not adopt the RI automatically. Review why reflimR is not applicable and provide independent confirmation or external evidence before approval."))
  }
  if (identical(agreement$status, "red")) {
    return(list(status = "yellow", decision = "INCONCLUSIVE",
                recommendation = "refineR and reflimR show a marked discrepancy. This suggests model sensitivity, heterogeneity, or pathological contamination.",
                action = "Do not implement either RI by default. Review data selection, subpopulations, and mclust/rpart exploration; repeat D7 after resolving the likely cause."))
  }
  if (identical(sc$level, "favorable") && agreement$status %in% c("green", "yellow") && identical(pathology_context$level %||% "unknown", "favorable")) {
    st <- if (identical(agreement$status, "green")) "green" else "yellow"
    return(list(status = st,
                decision = if (st == "green") "CANDIDATE — ROBUST" else "CANDIDATE — MINOR DISCORDANCE",
                recommendation = paste0("refineR estimates ", d7_candidate_text(refine), ". ", agreement$message),
                action = if (st == "green")
                  "The RI can proceed to review and specialist approval after confirming clinical coherence, applicability to the target population, preanalytical/analytical stability, and external evidence. Do not implement it automatically."
                else "Specifically review the discordant limit and its clinical plausibility before approving the RI."))
  }
  if (identical(sc$level, "favorable") && agreement$status %in% c("green", "yellow") && identical(pathology_context$level %||% "unknown", "intermediate")) {
    return(list(status = "yellow", decision = "CANDIDATE — CONDITIONAL",
                recommendation = paste0("refineR estimates ", d7_candidate_text(refine), ". ", agreement$message, " ", pathology_context$text),
                action = "Do not implement it as final yet. Strengthen population filtering/selection and external validation, and document the estimated non-pathological fraction."))
  }
  if (identical(sc$level, "moderate") && agreement$status %in% c("green", "yellow")) {
    return(list(status = "yellow", decision = "CANDIDATE — CONDITIONAL",
                recommendation = paste0("refineR estimates ", d7_candidate_text(refine), ". ", agreement$message),
                action = "Do not implement it as final yet. Complete the covariate review, clinical-coherence assessment, and external-evidence review; document the intermediate sample-size context according to the internal RIveR criterion."))
  }
  list(status = "yellow", decision = "CANDIDATE — EXPLORATORY",
       recommendation = paste0("refineR provisionally estimates ", d7_candidate_text(refine), ". ", agreement$message),
       action = "Use it only as an exploratory result. Expand the dataset and repeat D7 before considering specialist approval.")
}

d7_core_decision_unilateral <- function(sc, refine, reflim, pathology_context) {
  if ((pathology_context$level %||% "unknown") %in% c("high_pathology", "assumption_compromised")) {
    return(list(status = "yellow", decision = "INCONCLUSIVE",
                recommendation = paste0("The estimated dataset composition is not sufficiently favorable to establish a robust one-sided limit. ", pathology_context$text),
                action = "Review extraction criteria and metadata to reduce pathological contamination; repeat D7 before proposing the limit for approval."))
  }
  if (identical(sc$level, "favorable") && (pathology_context$level %||% "unknown") %in% c("favorable", "intermediate")) {
    return(list(status = "yellow", decision = "CANDIDATE — CONDITIONAL",
                recommendation = paste0("refineR estimates ", d7_candidate_text(refine), ". In one-sided mode, reflimR provides descriptive P2.5/P97.5 context but not an independent estimate of the same P5/P95 limit."),
                action = "Review clinical coherence, applicability to the target population, preanalytical/analytical stability, and external evidence or direct validation before approval. RIveR does not classify this one-sided limit as a robust candidate based on sample size alone."))
  }
  if (identical(sc$level, "moderate")) {
    return(list(status = "yellow", decision = "CANDIDATE — CONDITIONAL",
                recommendation = paste0("refineR estimates ", d7_candidate_text(refine), ". The sample size is intermediate and there is no second reflimR estimate of the same P5/P95 limit."),
                action = "Do not implement it as final yet. Complete the covariate review, clinical-coherence assessment, and external-evidence review or direct validation."))
  }
  list(status = "yellow", decision = "CANDIDATE — EXPLORATORY",
       recommendation = paste0("refineR provisionally estimates ", d7_candidate_text(refine), ". One-sided mode requires additional caution because reflimR does not confirm the same active percentile."),
       action = "Use it only as an exploratory result. Expand the dataset and provide external/direct validation before considering specialist approval.")
}

d7_core_analysis <- function(x, n_bootstrap = 200, seed = 2201, progress = NULL, reference_design = NULL) {
  design <- normalize_reference_design(reference_design)
  x <- x[is.finite(x)]
  n <- length(x)
  sc <- d7_sample_context(n)
  if (n < 200L) {
    return(list(type = "indirect_establishment", module = "D7", n = n, reference_design = design, status = "grey",
                decision = "NOT EVALUABLE", confidence = sc$label, sample_context = sc, ri = NULL,
                recommendation = "An indirect RI cannot be established with the current dataset.",
                action = "Increase the dataset and confirm preanalytical/analytical stability before repeating D7.",
                refine = NULL, reflim = NULL, agreement = NULL, pathology_context = NULL,
                technical_error = FALSE, references = D7_REFERENCES))
  }

  if (is.function(progress)) progress(0.15, paste0("D7 · refineR + ", n_bootstrap, " bootstrap"))
  refine <- tryCatch(run_refineR(x, n_bootstrap = n_bootstrap, seed = seed, reference_design = design), error = function(e) e)
  if (inherits(refine, "error")) {
    msg <- paste("refineR could not be run:", conditionMessage(refine))
    return(list(type = "indirect_establishment", module = "D7", n = n, reference_design = design, status = "red", decision = "TECHNICAL ERROR",
                technical_error = TRUE, technical_message = msg, sample_context = sc, confidence = sc$label, ri = NULL,
                recommendation = msg, action = "Correct the technical error before interpreting D7.", references = D7_REFERENCES))
  }

  if (is.function(progress)) progress(0.48, "D7 · independent estimation with reflimR")
  reflim <- d7_reflim_result(x, reference_design = design)
  agreement <- if (isTRUE(reflim$available)) d7_compare_refine_reflim(refine$ri, reflim$ri, reference_design = design) else
    list(status = "grey", limit_status = c(lower = "grey", upper = "grey"), message = "There is no second interpretable estimate from reflimR.")
  pathology_context <- d7_pathology_context(refine, reflim)
  dec <- if (identical(design$tail, "two_sided"))
    d7_core_decision(sc, refine, reflim, agreement, pathology_context)
  else d7_core_decision_unilateral(sc, refine, reflim, pathology_context)

  c(list(type = "indirect_establishment", module = "D7", n = n, reference_design = design, confidence = sc$label,
         sample_context = sc, technical_error = FALSE, refine = refine, reflim = reflim,
         agreement = agreement, pathology_context = pathology_context, ri = refine$ri,
         references = D7_REFERENCES), dec)
}

d7_age_screen <- function(data) {
  if (is.null(data) || !is.data.frame(data) || !all(c("value", "age") %in% names(data))) {
    return(list(available = FALSE, status = "grey", decision = "not_evaluable",
                message = "There is no age covariate available."))
  }
  d <- data[is.finite(data$value) & is.finite(data$age), c("value", "age"), drop = FALSE]
  if (nrow(d) < 500L || length(unique(d$age)) < 10L || diff(range(d$age)) < 5) {
    return(list(available = TRUE, status = "grey", decision = "not_evaluable",
                message = "Age is available, but the data have insufficient range or density for robust screening."))
  }
  probs <- seq(0, 1, length.out = min(13L, max(6L, floor(nrow(d) / 200L) + 1L)))
  br <- unique(as.numeric(stats::quantile(d$age, probs = probs, na.rm = TRUE, type = 7)))
  if (length(br) < 5L) {
    return(list(available = TRUE, status = "grey", decision = "not_evaluable",
                message = "Could not construct informative age bands."))
  }
  bin <- cut(d$age, breaks = br, include.lowest = TRUE, labels = FALSE)
  spl <- split(seq_len(nrow(d)), bin)
  rows <- lapply(spl, function(ix) {
    if (length(ix) < 80L) return(NULL)
    data.frame(age = stats::median(d$age[ix]), n = length(ix), median = stats::median(d$value[ix]), stringsAsFactors = FALSE)
  })
  bins <- do.call(rbind, Filter(Negate(is.null), rows))
  if (is.null(bins) || nrow(bins) < 4L) {
    return(list(available = TRUE, status = "grey", decision = "not_evaluable",
                message = "Not enough informative age bands were obtained."))
  }
  rho <- suppressWarnings(stats::cor(bins$age, bins$median, method = "spearman", use = "complete.obs"))
  span <- diff(range(bins$median, na.rm = TRUE))
  iqr <- stats::IQR(d$value, na.rm = TRUE)
  effect <- if (is.finite(iqr) && iqr > 0) span / iqr else NA_real_
  if (is.finite(effect) && effect < 0.25 && (!is.finite(rho) || abs(rho) < 0.30)) {
    return(list(available = TRUE, status = "green", decision = "common", rho = rho, normalized_change = effect,
                bins = bins, message = "No sufficiently important age effect was detected to block an overall RI in D7 screening."))
  }
  list(available = TRUE, status = "yellow", decision = "age_effect", rho = rho, normalized_change = effect,
       bins = bins,
       message = "A relevant age-related pattern is observed. D7 will not convert this signal into arbitrary bands: the overall RI remains blocked and indirect continuous age modeling is activated.")
}

d7_lahti_prop_status <- function(p) {
  if (!is.finite(p)) return("grey")
  if (p < 0.009 || p > 0.041) return("red")
  if (p >= 0.018 && p <= 0.032) return("green")
  "yellow"
}

d7_lahti_distance_status <- function(z) {
  if (!is.finite(z)) return("grey")
  if (z >= 0.75) return("red")
  if (z < 0.25) return("green")
  "yellow"
}

d7_lahti_modelled <- function(pooled_ri, group_results, groups, reference_design = NULL) {
  design <- normalize_reference_design(reference_design)
  active <- design$active
  if (length(pooled_ri) != 2L || any(!is.finite(pooled_ri)) || length(group_results) != 2L) {
    return(list(decision = "not_evaluable", status = "grey", table = NULL, distance_table = NULL,
                message = "Lahti is not evaluable."))
  }
  rows <- list(); sdv <- numeric(2); limits <- matrix(NA_real_, 2, 2)
  for (i in 1:2) {
    ri <- group_results[[i]]$ri %||% c(NA_real_, NA_real_)
    if (length(ri) != 2L || any(!is.finite(ri)) || ri[2] <= ri[1]) next
    mu <- mean(ri)
    zspan <- stats::qnorm(design$pair_percentiles[["upper"]]) - stats::qnorm(design$pair_percentiles[["lower"]])
    s <- diff(ri) / zspan
    sdv[i] <- s; limits[i, ] <- ri
    p_lo <- stats::pnorm(pooled_ri[1], mean = mu, sd = s)
    p_hi <- 1 - stats::pnorm(pooled_ri[2], mean = mu, sd = s)
    if (isTRUE(active[["lower"]])) rows[[length(rows)+1L]] <- data.frame(
      Group = groups[i], Tail = "LRL", `Modeled proportion outside overall RI` = p_lo,
      Status = if (identical(design$tail, "two_sided")) d7_lahti_prop_status(p_lo) else "grey", stringsAsFactors = FALSE, check.names = FALSE)
    if (isTRUE(active[["upper"]])) rows[[length(rows)+1L]] <- data.frame(
      Group = groups[i], Tail = "URL", `Modeled proportion outside overall RI` = p_hi,
      Status = if (identical(design$tail, "two_sided")) d7_lahti_prop_status(p_hi) else "grey", stringsAsFactors = FALSE, check.names = FALSE)
  }
  tab <- if (length(rows)) do.call(rbind, rows) else NULL
  smin <- suppressWarnings(min(sdv[is.finite(sdv) & sdv > 0], na.rm = TRUE))
  dlow <- if (is.finite(smin)) abs(limits[1,1] - limits[2,1]) / smin else NA_real_
  dupp <- if (is.finite(smin)) abs(limits[1,2] - limits[2,2]) / smin else NA_real_
  lim_names <- c("LRL","URL")[active]
  lim_dist <- c(dlow, dupp)[active]
  dtab <- data.frame(Limit = lim_names, `Distance / smaller SD` = lim_dist,
                     Status = vapply(lim_dist, d7_lahti_distance_status, character(1)),
                     stringsAsFactors = FALSE, check.names = FALSE)

  if (!identical(design$tail, "two_sided")) {
    return(list(decision = "not_validated", status = "yellow", table = tab, distance_table = dtab,
                message = "The Lahti proportion thresholds operationalized in RIveR correspond to nominal 2.5% tails and are not automatically extrapolated to P5/P95. The distance between limits is shown only as descriptive support.",
                note = "One-sided mode: Lahti is not an automatic decision criterion until specific validation is available for the one-sided percentile."))
  }

  statuses <- c(if (!is.null(tab)) tab$Status else "grey", dtab$Status)
  if (any(statuses == "red")) {
    dec <- "partition"; st <- "red"
    msg <- "Lahti supports partitioning: at least one modeled tail/proportion or distance between limits exceeds the separation criterion."
  } else if (length(statuses) && all(statuses == "green")) {
    dec <- "common"; st <- "green"
    msg <- "Lahti is compatible with retaining a common RI."
  } else {
    dec <- "marginal"; st <- "yellow"
    msg <- "Lahti is marginal/inconclusive and requires integration with other criteria and biological plausibility."
  }
  list(decision = dec, status = st, table = tab, distance_table = dtab, message = msg,
       note = "D7 adaptation: proportions are calculated from Gaussian approximations defined by the refineR RI of each group to avoid applying Lahti directly to the contaminated routine-data mixture. This is supporting evidence, not an automatic rule.")
}

d7_nonpath_proxy <- function(x, core) {
  z <- x[is.finite(x)]
  ri <- core$ri %||% NULL
  if (is.null(ri) || length(ri) != 2L || any(!is.finite(ri))) return(z)
  y <- z[z >= ri[1] & z <= ri[2]]
  if (length(y) >= 50L) y else z
}

d7_sdr_two_group <- function(x1, x2) {
  x1 <- x1[is.finite(x1)]; x2 <- x2[is.finite(x2)]
  n1 <- length(x1); n2 <- length(x2); N <- n1 + n2
  if (n1 < 3L || n2 < 3L) return(list(sdr = NA_real_, supports_partition = NA, message = "SDR not evaluable."))
  m1 <- mean(x1); m2 <- mean(x2); v1 <- stats::var(x1); v2 <- stats::var(x2)
  grand <- (n1*m1 + n2*m2) / N
  msw <- ((n1-1)*v1 + (n2-1)*v2) / (N-2)
  msb <- (n1*(m1-grand)^2 + n2*(m2-grand)^2)
  n0 <- N - (n1^2 + n2^2)/N
  vb <- if (is.finite(msw) && is.finite(n0) && n0 > 0) max((msb - msw) / n0, 0) else NA_real_
  sdr <- if (is.finite(vb) && is.finite(msw) && msw > 0) sqrt(vb) / sqrt(msw) else NA_real_
  sup <- is.finite(sdr) && sdr >= 0.30
  list(sdr = sdr, supports_partition = sup,
       message = if (!is.finite(sdr)) "SDR not evaluable." else paste0("SDR=", round(sdr, 3), if (sup) " (≥0.30: supports consideration of partitioning)." else " (<0.30: provides no additional support)."),
       note = "D7 adaptation on central subsets compatible with the refineR RI of each group. The 0.30 threshold is a consideration guide, not a universal standard.")
}

d7_partition_core_table <- function(substudies, groups) {
  rows <- lapply(seq_along(groups), function(i) {
    z <- substudies[[i]]
    d <- normalize_reference_design(z$reference_design)
    agr <- z$agreement$status %||% "grey"
    np <- z$pathology_context$np_fraction %||% NA_real_
    refl_txt <- if (isTRUE(z$reflim$available)) {
      if (identical(d$tail, "two_sided")) reference_result_text(z$reflim$ri, d, 6)
      else paste0("P2.5=", signif(z$reflim$ri[1],6), " · P97.5=", signif(z$reflim$ri[2],6), " (descriptive)")
    } else "—"
    data.frame(
      Group = groups[i], n = z$n %||% NA_integer_,
      `refineR` = if (!is.null(z$ri)) reference_result_text(z$ri, d, 6) else "—",
      `reflimR` = refl_txt,
      Agreement = if (!identical(d$tail, "two_sided")) "Not applicable to P5/P95" else switch(agr, green="Favorable", yellow="Intermediate", red="Unfavorable", "Not evaluable"),
      `Non-pathological fraction` = if (is.finite(np)) paste0(round(100*np,1), " %") else "—",
      `D7 group decision` = z$decision %||% "—",
      stringsAsFactors = FALSE, check.names = FALSE)
  })
  do.call(rbind, rows)
}

d7_partition_methods_table <- function(partition) {
  subs <- partition$substudies %||% NULL
  groups <- partition$groups %||% names(subs)
  if (is.null(subs) || !length(subs)) return(NULL)
  rows <- list()
  for (i in seq_along(subs)) {
    tb <- d7_methods_table(subs[[i]])
    if (is.null(tb) || !nrow(tb)) next
    tb <- data.frame(Group = groups[i], tb, check.names = FALSE, stringsAsFactors = FALSE)
    tb$`D7 group decision` <- subs[[i]]$decision %||% "—"
    rows[[length(rows)+1L]] <- tb
  }
  if (!length(rows)) return(NULL)
  do.call(rbind, rows)
}

d7_partition_sex <- function(data, global_core, n_bootstrap = 200, seed = 2301,
                             progress = NULL, reference_design = NULL) {
  design <- normalize_reference_design(reference_design %||% global_core$reference_design)
  if (is.null(data) || !is.data.frame(data) || !all(c("value", "sex") %in% names(data))) {
    return(list(available = FALSE, status = "grey", decision = "not_evaluable", message = "No categorical covariate is available."))
  }
  ok <- is.finite(data$value) & !is.na(data$sex) & nzchar(trimws(as.character(data$sex)))
  d <- data[ok, c("value", "sex"), drop = FALSE]
  groups <- names(sort(table(as.character(d$sex)), decreasing = TRUE))
  if (length(groups) < 2L) return(list(available = TRUE, status = "grey", decision = "not_evaluable", message = "There are fewer than two evaluable groups."))
  note_more <- if (length(groups) > 2L) "There are more than two categories; this version automatically compares the two most frequent categories." else NULL
  groups <- groups[1:2]
  raw <- lapply(groups, function(g) d$value[as.character(d$sex) == g])
  subs <- lapply(seq_along(groups), function(i) {
    if (is.function(progress)) progress(0.50 + 0.10*(i/length(groups)), paste0("D7 · partition · group ", i, "/", length(groups)))
    d7_core_analysis(raw[[i]], n_bootstrap = n_bootstrap, seed = seed + i, reference_design = design)
  })
  names(subs) <- groups
  tab <- d7_partition_core_table(subs, groups)

  if (any(vapply(subs, function(z) identical(z$decision %||% "", "NOT EVALUABLE") || isTRUE(z$technical_error), logical(1)))) {
    return(list(available = TRUE, status = "yellow", decision = "indeterminate", groups = groups,
                substudies = subs, table = tab, message = "At least one group does not allow the complete D7 engine to run; the partition cannot be resolved.", note = note_more))
  }

  lahti <- d7_lahti_modelled(global_core$ri, subs, groups, reference_design = design)
  proxy <- lapply(seq_along(groups), function(i) d7_nonpath_proxy(raw[[i]], subs[[i]]))
  hb <- harris_boyd_support(proxy[[1]], proxy[[2]])
  sdr <- d7_sdr_two_group(proxy[[1]], proxy[[2]])
  bad_comp <- any(vapply(subs, function(z) (z$pathology_context$level %||% "unknown") %in% c("high_pathology", "assumption_compromised"), logical(1)))
  groups_evaluable <- all(vapply(subs, function(z) grepl("^CANDIDATE", z$decision %||% ""), logical(1)))

  hb_txt <- if (!is.finite(hb$z %||% NA_real_)) "Harris–Boyd not evaluable." else
    paste0("Z=", round(hb$z,2), "; Z*=", round(hb$z_critical,2), "; SD ratio=", round(hb$sd_ratio,2),
           if (isTRUE(hb$supports_partition)) ": supports partitioning." else ": does not support partitioning.")
  unilateral <- !identical(design$tail, "two_sided")
  criteria <- data.frame(
    Criterion = c("Lahti · proportions/distances", "Harris–Boyd", "SDR", "D7 engine by group", "Biological plausibility"),
    Result = c(lahti$message, hb_txt, sdr$message,
                 if (groups_evaluable) "Both groups generate an interpretable candidate." else "At least one group does not generate an interpretable candidate.",
                 "Requires specialist review; RIveR cannot resolve it automatically."),
    `Role in the decision` = c(if (unilateral) "Not a decision criterion for P5/P95; one-sided proportion thresholds are not validated in RIveR." else "Primary criterion for impact on the limits; modeled adaptation for indirect data.",
                              "Support based on central tendency/variability differences; applied to the central subset compatible with refineR.",
                              "Supporting evidence for effect magnitude; internal consideration threshold SDR≥0.30.",
                              "Required before proposing separate results.",
                              "Required before final approval."),
    stringsAsFactors = FALSE, check.names = FALSE)

  if (bad_comp) {
    st <- "yellow"; dec <- "indeterminate"
    msg <- "The composition of at least one group is not sufficiently favorable; the observed difference must not be converted into a definitive partition."
  } else if (unilateral) {
    st <- "yellow"; dec <- "indeterminate"
    msg <- "In one-sided P5/P95 mode, RIveR does not extrapolate Lahti 2.5% tail thresholds. Harris–Boyd, SDR, and the distance between active limits are shown as supporting evidence, but partitioning is not resolved automatically; specialist review, biological plausibility, and external evidence are required."
  } else if (identical(lahti$decision, "partition") && (isTRUE(hb$supports_partition) || isTRUE(sdr$supports_partition)) && groups_evaluable) {
    st <- "yellow"; dec <- "partition"
    msg <- "Lahti and at least one additional criterion (Harris–Boyd/SDR) support separation, and the D7 engine is interpretable in both groups. RIveR proposes candidate partitioned RIs, pending biological-plausibility review and specialist approval."
  } else if (identical(lahti$decision, "common") && !isTRUE(hb$supports_partition) && !isTRUE(sdr$supports_partition)) {
    st <- "green"; dec <- "common"
    msg <- "Lahti, Harris–Boyd and SDR do not provide sufficient evidence to separate the groups; the overall RI assessment is retained."
  } else {
    st <- "yellow"; dec <- "indeterminate"
    msg <- "Partitioning criteria are discordant or marginal. Do not adopt either the overall RI or separate RIs until the discrepancy is resolved using biological plausibility, clinical impact, and external evidence."
  }
  list(available = TRUE, status = st, decision = dec, groups = groups, substudies = subs, table = tab, reference_design = design,
       methods_table = d7_partition_methods_table(list(substudies=subs, groups=groups)),
       lahti = lahti, harris_boyd = hb, sdr = sdr, criteria_table = criteria,
       message = msg, note = note_more,
       biological_review_required = identical(dec, "partition"))
}


# v0.16.0 · qualitative variable general -----------------------------------
d7_partition_qualitative <- function(data, global_core, n_bootstrap = 200, seed = 2301,
                                     progress = NULL, reference_design = NULL,
                                     group_col = "qualitative") {
  design <- normalize_reference_design(reference_design %||% global_core$reference_design)
  if (is.null(data) || !is.data.frame(data)) {
    return(list(available=FALSE,status="grey",decision="not_evaluable",message="There is no qualitative variable available."))
  }
  if (!group_col %in% names(data)) {
    if ("sex" %in% names(data)) group_col <- "sex" else
      return(list(available=FALSE,status="grey",decision="not_evaluable",message="There is no qualitative variable available."))
  }
  ok <- is.finite(data$value) & !is.na(data[[group_col]]) & nzchar(trimws(as.character(data[[group_col]])))
  d <- data[ok, c("value", group_col), drop=FALSE]
  names(d)[2] <- "qualitative"
  groups <- names(sort(table(as.character(d$qualitative)), decreasing=TRUE))
  if (length(groups) < 2L) return(list(available=TRUE,status="grey",decision="not_evaluable",message="There are fewer than two evaluable categories."))
  if (length(groups) == 2L) {
    dd <- data.frame(value=d$value, sex=d$qualitative, stringsAsFactors=FALSE)
    return(d7_partition_sex(dd, global_core=global_core, n_bootstrap=n_bootstrap, seed=seed,
                            progress=progress, reference_design=design))
  }

  raw <- lapply(groups, function(g) d$value[as.character(d$qualitative) == g]); names(raw) <- groups
  subs <- vector("list", length(groups)); names(subs) <- groups
  for (i in seq_along(groups)) {
    if (is.function(progress)) progress(0.50 + 0.10*(i/length(groups)), paste0("D7 · partition · group ", i, "/", length(groups)))
    subs[[i]] <- d7_core_analysis(raw[[i]], n_bootstrap=n_bootstrap, seed=seed+i, reference_design=design)
  }
  tab <- d7_partition_core_table(subs, groups)
  methods <- d7_partition_methods_table(list(substudies=subs, groups=groups))
  if (any(vapply(subs, function(z) identical(z$decision %||% "", "NOT EVALUABLE") || isTRUE(z$technical_error), logical(1)))) {
    return(list(available=TRUE,status="yellow",decision="indeterminate",groups=groups,substudies=subs,
                table=tab,methods_table=methods,message="At least one category does not allow the complete D7 engine to run; the multigroup partition cannot be resolved."))
  }

  cmb <- utils::combn(seq_along(groups), 2, simplify=FALSE)
  pair_rows <- list(); pair_details <- list()
  unilateral <- !identical(design$tail, "two_sided")
  for (j in seq_along(cmb)) {
    ij <- cmb[[j]]; gp <- groups[ij]; pair_subs <- subs[ij]
    lahti <- d7_lahti_modelled(global_core$ri, pair_subs, gp, reference_design=design)
    proxy <- lapply(ij, function(i) d7_nonpath_proxy(raw[[i]], subs[[i]]))
    hb <- harris_boyd_support(proxy[[1]], proxy[[2]])
    sdr <- d7_sdr_two_group(proxy[[1]], proxy[[2]])
    bad_comp <- any(vapply(pair_subs, function(z) (z$pathology_context$level %||% "unknown") %in% c("high_pathology","assumption_compromised"), logical(1)))
    groups_evaluable <- all(vapply(pair_subs, function(z) grepl("^CANDIDATE", z$decision %||% ""), logical(1)))
    if (bad_comp) { dec <- "indeterminate"; st <- "yellow"; reason <- "The estimated composition is not sufficiently favorable in at least one category." }
    else if (unilateral) { dec <- "indeterminate"; st <- "yellow"; reason <- "One-sided mode: Lahti is not an automatic decision criterion." }
    else if (identical(lahti$decision,"partition") && (isTRUE(hb$supports_partition) || isTRUE(sdr$supports_partition)) && groups_evaluable) {
      dec <- "partition"; st <- "yellow"; reason <- "Lahti + Harris–Boyd/SDR support separation."
    } else if (identical(lahti$decision,"common") && !isTRUE(hb$supports_partition) && !isTRUE(sdr$supports_partition)) {
      dec <- "common"; st <- "green"; reason <- "Lahti, Harris–Boyd and SDR compatible with common RI."
    } else { dec <- "indeterminate"; st <- "yellow"; reason <- "Criteria discordant or marginal." }
    pair_rows[[j]] <- data.frame(`Category A`=gp[1],`Category B`=gp[2],Status=st,Decision=dec,
      `Lahti`=lahti$status %||% "grey",`Harris–Boyd`=if (isTRUE(hb$supports_partition)) "Support" else "Without support",
      SDR=if (is.finite(sdr$sdr %||% NA_real_)) round(sdr$sdr,3) else NA_real_,Detail=reason,
      check.names=FALSE,stringsAsFactors=FALSE)
    pair_details[[j]] <- list(groups=gp,lahti=lahti,harris_boyd=hb,sdr=sdr,decision=dec,status=st)
  }
  pair_table <- do.call(rbind,pair_rows)
  decs <- pair_table$Decision
  if (any(decs == "partition")) {
    st <- "yellow"; dec <- "partition"
    msg <- paste0("At least one of the ", nrow(pair_table), " comparisons between categories supports separation. The limit/overall RI is not defensible. RIveR retains the candidates for each category and does not merge categories automatically.")
  } else if (all(decs == "common")) {
    st <- "green"; dec <- "common"
    msg <- paste0("The ", nrow(pair_table), " comparisons between categories are compatible with retaining a common limit/overall RI.")
  } else {
    st <- "yellow"; dec <- "indeterminate"
    msg <- "Comparisons between categories are discordant or marginal. Do not automatically adopt either the overall result or separate results."
  }
  criteria <- pair_table
  list(available=TRUE,status=st,decision=dec,groups=groups,substudies=subs,table=tab,
       methods_table=methods,pairwise_table=pair_table,pairwise=pair_details,criteria_table=criteria,
       message=msg,note=">2 categories: pairwise application of the same D7 criteria; categories are not automatically merged.",
       biological_review_required=identical(dec,"partition"))
}

d7_refine_p50 <- function(refine) {
  if (is.null(refine)) return(NA_real_)

  # v0.15.4: P50 comes from the refineR MODEL, not from the getRI() limits table.
  # This decouples the median from VeRUS margins and avoids incorrectly interpreting
  # P2.5/P97.5 as P50 when refineR formats the percentiles in a
  # different scale or returns only the RI limits.
  ri <- suppressWarnings(as.numeric(refine$ri %||% c(NA_real_, NA_real_)))
  valid_geometry <- function(v) {
    v <- suppressWarnings(as.numeric(v))[1]
    if (!is.finite(v)) return(FALSE)
    if (length(ri) >= 2L && all(is.finite(ri[1:2]))) return(ri[1] < v && v < ri[2])
    TRUE
  }

  med <- suppressWarnings(as.numeric(refine$median %||% NA_real_))[1]
  if (valid_geometry(med)) return(med)

  if (!is.null(refine$fit) && exists("rilctms_refine_model_quantile", mode = "function")) {
    med <- rilctms_refine_model_quantile(
      refine$fit, p = 0.5,
      point_method = refine$point_method %||% "fullDataEst"
    )
    if (valid_geometry(med)) return(med)
  }

  NA_real_
}

d7_local_window_indices <- function(
    x,
    center,
    target_n,
    min_distinct = 5L
) {
  
  valid <- is.finite(x)
  
  original_idx <- which(valid)
  x <- x[valid]
  
  if (!length(x)) {
    return(integer(0))
  }
  
  distance <- abs(x - center)
  
  # Increase the radius progressively.
  # The window must contain:
  #   1) at least target_n observations
  #   2) at least min_distinct covariate values
  #
  # All observations at the boundary are retained so tied
  # covariate values are not split arbitrarily.
  
  radii <- sort(unique(distance))
  
  for (radius in radii) {
    
    ix <- which(distance <= radius)
    
    enough_n <-
      length(ix) >= target_n
    
    enough_distinct <-
      length(unique(x[ix])) >= min_distinct
    
    if (enough_n && enough_distinct) {
      return(original_idx[ix])
    }
  }
  
  # Fallback: use all available observations
  original_idx
}

d7_age_model_indirect <- function(data, n_bootstrap = 200, seed = 2601, progress = NULL, reference_design = NULL) {
  design <- normalize_reference_design(reference_design)
  if (is.null(data) || !is.data.frame(data) || !all(c("value", "age") %in% names(data))) {
    return(list(available = FALSE, status = "grey", decision = "not_evaluable", message = "There are no age data available for modeling."))
  }
  d <- data[is.finite(data$value) & is.finite(data$age), c("value", "age"), drop = FALSE]
  n <- nrow(d)
  if (n < 1000L || length(unique(d$age)) < 10L || diff(range(d$age)) < 5) {
    return(list(available = FALSE, status = "yellow", decision = "not_evaluable",
                message = "An age effect was detected, but density or range is insufficient to construct a defensible continuous indirect curve."))
  }

  n_centers <- if (n >= 5000L) 9L else if (n >= 2500L) 7L else 5L
  centers <- unique(as.numeric(stats::quantile(d$age, probs = seq(0.05, 0.95, length.out = n_centers), na.rm = TRUE, type = 7)))
  if (length(centers) < 5L) return(list(available = FALSE, status = "yellow", decision = "not_evaluable", message = "Not enough independent age points were obtained for modeling."))
  window_n <- min(n, max(500L, min(1500L, as.integer(ceiling(n * 0.20)))))

  windows <- vector("list", length(centers)); rows <- vector("list", length(centers))
  for (i in seq_along(centers)) {
    if (is.function(progress)) progress(0.62 + 0.18*(i/length(centers)), paste0("D7 · age model · window ", i, "/", length(centers)))
    ix <- d7_local_window_indices(
      x = d$age,
      center = centers[i],
      target_n = window_n,
      min_distinct = 5L)
    dd <- d[ix, , drop = FALSE]
    core <- d7_core_analysis(dd$value, n_bootstrap = n_bootstrap, seed = seed + i, reference_design = design)
    if (is.function(progress)) progress(0.62 + 0.18*(i/length(centers)), paste0("D7 · age model · window ", i, "/", length(centers), " completed"))
    windows[[i]] <- core
    np <- core$pathology_context$np_fraction %||% NA_real_
    rows[[i]] <- data.frame(
      `Target age` = centers[i],
      `Median age in window` = stats::median(dd$age),
      `Minimum age in window` = min(dd$age),
      `Maximum age in window` = max(dd$age),
      n = nrow(dd),
      
      LRL = if (!is.null(core$ri)) core$ri[1] else NA_real_,
      P50 = d7_refine_p50(core$refine),
      URL = if (!is.null(core$ri)) core$ri[2] else NA_real_,
      
      `LRL reflimR` = if (isTRUE(core$reflim$available)) {
        core$reflim$ri[1]
      } else {
        NA_real_
      },
      
      `URL reflimR` = if (isTRUE(core$reflim$available)) {
        core$reflim$ri[2]
      } else {
        NA_real_
      },
      
      Agreement = core$agreement$status %||% "grey",
      
      `Non-pathological fraction` = if (is.finite(np)) {
        np
      } else {
        NA_real_
      },
      
      Decision = core$decision %||% "—",
      
      `Technical message` = if (isTRUE(core$technical_error)) {
        core$technical_message %||% "Unknown technical error"
      } else {
        ""
      },
      
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  }
  win <- do.call(rbind, rows)
  finite_lrl <- is.finite(win$LRL)
  finite_p50 <- is.finite(win$P50)
  finite_url <- is.finite(win$URL)
  ordered <- finite_lrl & finite_p50 & finite_url & win$LRL < win$P50 & win$P50 < win$URL
  good <- ordered
  win[["Window interpretable"]] <- ifelse(good, "Yes", "No")
  diag <- list(
    total = nrow(win), good = sum(good),
    finite_lrl = sum(finite_lrl), finite_p50 = sum(finite_p50), finite_url = sum(finite_url),
    geometrically_ordered = sum(ordered)
  )
  if (sum(good) < 5L) {
    msg <- paste0(
      "There are insufficient local windows with interpretable refineR results to construct a continuous curve (",
      sum(good), "/", nrow(win),
      " meet LRL < P50 < URL; finite LRL ",
      sum(finite_lrl), "/", nrow(win),
      "; finite P50 ",
      sum(finite_p50), "/", nrow(win),
      "; finite URL ",
      sum(finite_url), "/", nrow(win),
      ")."
    )
    return(list(available = TRUE, status = "yellow", decision = "review", windows = win,
                window_diagnostics = diag, reference_design = design, message = msg))
  }
  z <- win[good, , drop = FALSE]
  ord <- order(z[["Target age"]])
  z <- z[ord, , drop = FALSE]
  xage <- z[["Target age"]]; ww <- z$n
  fit_one <- function(y) tryCatch(stats::smooth.spline(x = xage, y = y, w = ww), error = function(e) NULL)
  fl <- fit_one(z$LRL); fm <- fit_one(z$P50); fu <- fit_one(z$URL)
  if (is.null(fl) || is.null(fm) || is.null(fu)) {
    return(list(available = TRUE, status = "yellow", decision = "review", windows = win,
                message = "The continuous curve of the indirect limits could not be fitted stably."))
  }
  grid <- seq(min(xage), max(xage), length.out = 101L)
  curve <- data.frame(Age = grid,
                      LRL = stats::predict(fl, grid)$y,
                      P50 = stats::predict(fm, grid)$y,
                      URL = stats::predict(fu, grid)$y,
                      stringsAsFactors = FALSE)
  if (any(!is.finite(as.matrix(curve[,c("LRL","P50","URL")]))) || any(curve$LRL >= curve$P50) || any(curve$P50 >= curve$URL)) {
    return(list(available = TRUE, status = "yellow", decision = "review", windows = win, curve = curve,
                message = "The continuous curve has geometric inconsistencies (LRL < P50 < URL is required) and should not be proposed as an age-dependent RI."))
  }
  rep_age <- seq(min(grid), max(grid), length.out = 9L)
  rep_tab <- data.frame(Age = rep_age,
                        LRL = stats::predict(fl, rep_age)$y,
                        P50 = stats::predict(fm, rep_age)$y,
                        URL = stats::predict(fu, rep_age)$y,
                        stringsAsFactors = FALSE)
  agr <- z$Agreement
  np <- z[["Non-pathological fraction"]]
  red_n <- sum(agr == "red", na.rm = TRUE)
  bad_np <- sum(is.finite(np) & np < 0.70)
  if (red_n == 0L && bad_np == 0L) {
    st <- "yellow"; dec <- "continuous_candidate"
    msg <- if (identical(design$tail, "two_sided"))
      paste0(
        "A candidate continuous RI was constructed between approximately ",
        round(min(grid),1),
        " and ",
        round(max(grid),1),
        " years from ",
        nrow(z),
        " local estimation windows. No arbitrary cut-points were created."
      )
    else paste0(
      "A candidate continuous one-sided limit (",
      reference_design_percentile_text(design),
      ") was constructed between approximately ",
      round(min(grid),1),
      " and ",
      round(max(grid),1),
      " years from ",
      nrow(z),
      " local estimation windows. No arbitrary cut-points were created."
    )
  } else {
    st <- "yellow"; dec <- "review"
    msg <- paste0(
      "The continuous model generated a curve, but ",
      red_n,
      " local window(s) showed refineR–reflimR discordance and ",
      bad_np,
      " window(s) had an estimated non-pathological fraction <70%. ",
      "Review these findings before proposing an age-dependent RI."
    )
  }
  list(available = TRUE, status = st, decision = dec, reference_design = design, curve = curve, table = rep_tab, windows = win,
       window_diagnostics = diag, supported_age = range(grid), window_n = window_n, n_windows = nrow(z), bootstrap = n_bootstrap,
       message = msg,
       note = if (identical(design$tail, "two_sided"))
         "Internal RIveR model for indirect data: refineR + reflimR in local age windows with continuous smoothing of the limits. This is a candidate modeling approach under validation, not a universal standard, and it requires specialist review and external validation before clinical use."
       else "RIveR internal one-sided model: refineR estimates P5/P95 in local age windows and the active percentile is modeled continuously; reflimR is used only as descriptive P2.5/P97.5 support. Specialist review and external validation are required before clinical use.")
}


# v0.16.0 · quantitative variable general -----------------------------------
d7_quantitative_screen <- function(data, covariate_label = "Quantitative variable") {
  if (is.null(data) || !is.data.frame(data) || !all(c("value","quantitative") %in% names(data))) {
    return(list(available=FALSE,status="grey",decision="not_evaluable",message="There is no quantitative variable available.", covariate_label=covariate_label))
  }
  d <- data[is.finite(data$value) & is.finite(data$quantitative), c("value","quantitative"), drop=FALSE]
  if (nrow(d) < 500L || length(unique(d$quantitative)) < 10L || diff(range(d$quantitative)) <= 0) {
    return(list(available=TRUE,status="grey",decision="not_evaluable",message="The quantitative variable is available, but the data have insufficient density or range for robust screening.", covariate_label=covariate_label))
  }
  probs <- seq(0,1,length.out=min(13L,max(6L,floor(nrow(d)/200L)+1L)))
  br <- unique(as.numeric(stats::quantile(d$quantitative, probs=probs, na.rm=TRUE, type=7)))
  if (length(br) < 5L) return(list(available=TRUE,status="grey",decision="not_evaluable",message="Informative bands for the quantitative variable could not be constructed.", covariate_label=covariate_label))
  bin <- cut(d$quantitative, breaks=br, include.lowest=TRUE, labels=FALSE)
  spl <- split(seq_len(nrow(d)), bin)
  rows <- lapply(spl, function(ix) {
    if (length(ix) < 80L) return(NULL)
    data.frame(quantitative=stats::median(d$quantitative[ix]), n=length(ix), median=stats::median(d$value[ix]), stringsAsFactors=FALSE)
  })
  bins <- do.call(rbind, Filter(Negate(is.null), rows))
  if (is.null(bins) || nrow(bins) < 4L) return(list(available=TRUE,status="grey",decision="not_evaluable",message="Not enough informative bands were obtained.", covariate_label=covariate_label))
  names(bins)[1] <- covariate_label
  rho <- suppressWarnings(stats::cor(bins[[1]], bins$median, method="spearman", use="complete.obs"))
  span <- diff(range(bins$median, na.rm=TRUE)); iqr <- stats::IQR(d$value, na.rm=TRUE)
  effect <- if (is.finite(iqr) && iqr > 0) span/iqr else NA_real_
  if (is.finite(effect) && effect < 0.25 && (!is.finite(rho) || abs(rho) < 0.30)) {
    return(list(available=TRUE,status="green",decision="common",rho=rho,normalized_change=effect,bins=bins,covariate_label=covariate_label,
                message=paste0("No sufficiently important effect of ", covariate_label, " was detected to block an overall RI in D7 screening.")))
  }
  list(available=TRUE,status="yellow",decision="age_effect",rho=rho,normalized_change=effect,bins=bins,covariate_label=covariate_label,
       message=paste0("A relevant dependence pattern is observed for ", covariate_label, ". D7 will not create arbitrary bands: the overall RI remains blocked and indirect continuous modeling is activated."))
}

d7_quantitative_model_indirect <- function(data, n_bootstrap=200, seed=2601, progress=NULL, reference_design=NULL,
                                           covariate_label="Quantitative variable") {
  design <- normalize_reference_design(reference_design)
  if (is.null(data) || !is.data.frame(data) || !all(c("value","quantitative") %in% names(data))) {
    return(list(available=FALSE,status="grey",decision="not_evaluable",message="There is no quantitative variable available for modeling.",covariate_label=covariate_label))
  }
  d <- data[is.finite(data$value) & is.finite(data$quantitative), c("value","quantitative"), drop=FALSE]
  n <- nrow(d)
  if (n < 1000L || length(unique(d$quantitative)) < 10L || diff(range(d$quantitative)) <= 0) {
    return(list(available=FALSE,status="yellow",decision="not_evaluable",message="A quantitative effect was detected, but density or range is insufficient to construct a defensible continuous indirect curve.",covariate_label=covariate_label))
  }
  n_centers <- if (n >= 5000L) 9L else if (n >= 2500L) 7L else 5L
  centers <- unique(as.numeric(stats::quantile(d$quantitative, probs=seq(0.05,0.95,length.out=n_centers), na.rm=TRUE, type=7)))
  if (length(centers) < 5L) return(list(available=FALSE,status="yellow",decision="not_evaluable",message="Not enough independent points were obtained for modeling.",covariate_label=covariate_label))
  window_n <- min(n,max(500L,min(1500L,as.integer(ceiling(n*0.20)))))
  windows <- vector("list",length(centers)); rows <- vector("list",length(centers))
  for (i in seq_along(centers)) {
    if (is.function(progress)) progress(0.62+0.18*(i/length(centers)), paste0("D7 · quantitative model · window ",i,"/",length(centers)))
    ix <- d7_local_window_indices(
      x = d$quantitative,
      center = centers[i],
      target_n = window_n,
      min_distinct = 5L)
    dd <- d[ix, , drop = FALSE]
    core <- d7_core_analysis(dd$value,n_bootstrap=n_bootstrap,seed=seed+i,reference_design=design)
    if (is.function(progress)) progress(0.62+0.18*(i/length(centers)), paste0("D7 · quantitative model · window ",i,"/",length(centers)," completed"))
    windows[[i]] <- core; np <- core$pathology_context$np_fraction %||% NA_real_
    row <- data.frame(xcenter=stats::median(dd$quantitative), xmin=min(dd$quantitative), xmax=max(dd$quantitative), n=nrow(dd),
      LRL=if(!is.null(core$ri)) core$ri[1] else NA_real_, P50=d7_refine_p50(core$refine), URL=if(!is.null(core$ri)) core$ri[2] else NA_real_,
      `LRL reflimR`=if(isTRUE(core$reflim$available)) core$reflim$ri[1] else NA_real_,
      `URL reflimR`=if(isTRUE(core$reflim$available)) core$reflim$ri[2] else NA_real_, Agreement=core$agreement$status %||% "grey",
      `Non-pathological fraction`=if(is.finite(np)) np else NA_real_, Decision=core$decision %||% "—", stringsAsFactors=FALSE, check.names=FALSE)
    names(row)[1:3] <- c(paste0(covariate_label," central"), paste0(covariate_label," minimum window"), paste0(covariate_label," maximum window"))
    rows[[i]] <- row
  }
  win <- do.call(rbind,rows); xcol <- names(win)[1]
  finite_lrl <- is.finite(win$LRL); finite_p50 <- is.finite(win$P50); finite_url <- is.finite(win$URL)
  good <- finite_lrl & finite_p50 & finite_url & win$LRL < win$P50 & win$P50 < win$URL
  win[["Window interpretable"]] <- ifelse(good,"Yes","No")
  diag <- list(total=nrow(win),good=sum(good),finite_lrl=sum(finite_lrl),finite_p50=sum(finite_p50),finite_url=sum(finite_url),geometrically_ordered=sum(good))
  if (sum(good)<5L) return(list(available=TRUE,status="yellow",decision="review",windows=win,window_diagnostics=diag,reference_design=design,covariate_label=covariate_label,
    message=paste0("There are insufficient local windows with interpretable refineR results to construct a continuous curve (",sum(good),"/",nrow(win)," meet LRL < P50 < URL).")))
  z <- win[good,,drop=FALSE]; z <- z[order(z[[xcol]]),,drop=FALSE]; xx <- z[[xcol]]; ww <- z$n
  fit_one <- function(y) tryCatch(stats::smooth.spline(x=xx,y=y,w=ww),error=function(e)NULL)
  fl<-fit_one(z$LRL); fm<-fit_one(z$P50); fu<-fit_one(z$URL)
  if (is.null(fl)||is.null(fm)||is.null(fu)) return(list(available=TRUE,status="yellow",decision="review",windows=win,covariate_label=covariate_label,message="The continuous curve of the indirect limits could not be fitted stably."))
  grid <- seq(min(xx),max(xx),length.out=101L)
  curve <- data.frame(x=grid,LRL=stats::predict(fl,grid)$y,P50=stats::predict(fm,grid)$y,URL=stats::predict(fu,grid)$y,stringsAsFactors=FALSE); names(curve)[1] <- covariate_label
  if (any(!is.finite(as.matrix(curve[,c("LRL","P50","URL")]))) || any(curve$LRL>=curve$P50) || any(curve$P50>=curve$URL)) return(list(available=TRUE,status="yellow",decision="review",windows=win,curve=curve,covariate_label=covariate_label,message="The continuous curve has geometric inconsistencies (LRL < P50 < URL is required)."))
  repx <- seq(min(grid),max(grid),length.out=9L); tab <- data.frame(x=repx,LRL=stats::predict(fl,repx)$y,P50=stats::predict(fm,repx)$y,URL=stats::predict(fu,repx)$y,stringsAsFactors=FALSE); names(tab)[1] <- covariate_label
  agr <- z$Agreement; np <- z[["Non-pathological fraction"]]; red_n <- sum(agr=="red",na.rm=TRUE); bad_np <- sum(is.finite(np)&np<0.70)
  if (red_n==0L && bad_np==0L) { st<-"yellow"; dec<-"continuous_candidate"; msg<-paste0("A candidate continuous model for ",covariate_label," was constructed from ",nrow(z)," overlapping local windows. No arbitrary cut-points were created.") }
  else { st<-"yellow"; dec<-"review"; msg<-paste0("The continuous modeling has generated a curve, but there has ",red_n," window/is with discordance refineR–reflimR and ",bad_np," with non-pathological fraction <70 %. Review-the.") }
  list(available=TRUE,status=st,decision=dec,reference_design=design,curve=curve,table=tab,windows=win,window_diagnostics=diag,
       supported_quantitative=range(grid),window_n=window_n,n_windows=nrow(z),bootstrap=n_bootstrap,covariate_label=covariate_label,message=msg,
       note="RIveR indirect quantitative model: overlapping refineR/reflimR windows with continuous smoothing of the limits; a candidate under validation, not a universal standard.")
}

d7_methods_table <- function(result) {
  if (is.null(result) || !identical(result$type %||% "", "indirect_establishment")) return(NULL)
  design <- normalize_reference_design(result$reference_design)
  active <- design$active
  one_sided <- !identical(design$tail, "two_sided")
  rows <- list()
  make_row <- function(method, role, available, ri, ci, paper) {
    out <- data.frame(Method=method, Paper=role, Available=available,
                      LRL=as.numeric(ri[1]), `95% CI LRL`=paste(signif(ci[1],6),"–",signif(ci[2],6)),
                      URL=as.numeric(ri[2]), `95% CI URL`=paste(signif(ci[3],6),"–",signif(ci[4],6)),
                      `Paper in D7`=paper, check.names=FALSE, stringsAsFactors=FALSE)
    if (!isTRUE(active[["lower"]])) { out$LRL <- NA_real_; out[["95% CI LRL"]] <- "Not applicable" }
    if (!isTRUE(active[["upper"]])) { out$URL <- NA_real_; out[["95% CI URL"]] <- "Not applicable" }
    out
  }
  if (!is.null(result$refine)) {
    ci <- d7_refine_ci(result$refine)
    rows[[length(rows)+1L]] <- make_row(
      "refineR","Primary","Yes",result$refine$ri,ci,
      paste0("Primary estimate; bootstrap=", result$refine$n_bootstrap %||% NA_integer_,
             "; pointEst=", result$refine$point_method %||% "—",
             "; estimated non-pathological fraction=",
             if (is.finite(d7_np_fraction(result$refine,result$reflim))) paste0(round(100*d7_np_fraction(result$refine,result$reflim),1),"%") else "—")
    )
  }
  r <- result$reflim
  if (!is.null(r)) {
    if (isTRUE(r$available) && one_sided) {
      rr <- data.frame(Method="reflimR", Paper="Descriptive support", Available="Yes",
                       LRL=NA_real_, `95% CI LRL`="Not comparable",
                       URL=NA_real_, `95% CI URL`="Not comparable",
                       `Paper in D7`=paste0("reflimR 1.1.0 estimates P2.5=", signif(r$ri[1],6), " and P97.5=", signif(r$ri[2],6),
                         "; used for distribution/non-pathological-fraction context, not as confirmation of the ", reference_design_percentile_text(design), "."),
                       check.names=FALSE, stringsAsFactors=FALSE)
      rows[[length(rows)+1L]] <- rr
    } else if (isTRUE(r$available)) {
      ci <- d7_reflim_ci(r$fit)
      rows[[length(rows)+1L]] <- make_row(
        "reflimR","Independent estimate","Yes",r$ri,ci,
        paste0("Independent support; estimated non-pathological %=",
               if (is.finite(r$perc_norm %||% NA_real_)) paste0(signif(r$perc_norm,4),"%") else "—")
      )
    } else {
      rr <- make_row("reflimR",if (one_sided) "Descriptive support" else "Independent estimate","No",c(NA,NA),rep(NA,4),r$message %||% "Not available")
      rr[["95% CI LRL"]] <- if (one_sided) "Not comparable" else if (active[["lower"]]) "—" else "Not applicable"
      rr[["95% CI URL"]] <- if (one_sided) "Not comparable" else if (active[["upper"]]) "—" else "Not applicable"
      rows[[length(rows)+1L]] <- rr
    }
  }
  if (!length(rows)) return(NULL)
  do.call(rbind, rows)
}

d7_summary_table <- function(result) {
  
  if (
    is.null(result) ||
    !identical(result$type %||% "", "indirect_establishment")
  ) {
    return(NULL)
  }
  
  design <- normalize_reference_design(result$reference_design)
  active <- design$active
  one_sided <- !identical(design$tail, "two_sided")
  
  comp <- result$agreement %||% list(
    status = "grey",
    limit_status = c(
      lower = "grey",
      upper = "grey"
    )
  )
  
  # Robust extraction of limit-specific agreement status.
  # Supports named and unnamed vectors, including results
  # recovered from previous persistent analyses.
  get_limit_status <- function(comp, side) {
    
    ls <- comp$limit_status %||% NULL
    
    if (is.null(ls) || length(ls) == 0L) {
      return("grey")
    }
    
    # Preferred case: named vector.
    if (!is.null(names(ls)) && side %in% names(ls)) {
      
      out <- as.character(ls[[side]])[1]
      
      if (!is.na(out) && nzchar(out)) {
        return(out)
      }
    }
    
    # Fallback for old/persistent results stored
    # as an unnamed vector.
    idx <- match(
      side,
      c("lower", "upper")
    )
    
    if (!is.na(idx) && length(ls) >= idx) {
      
      out <- as.character(ls[[idx]])[1]
      
      if (!is.na(out) && nzchar(out)) {
        return(out)
      }
    }
    
    return("grey")
  }
  
  sl <- function(x) {
    
    status_symbol_text(
      x %||% "grey",
      switch(
        x %||% "grey",
        green = "Favorable",
        yellow = "Intermediate",
        red = "Unfavorable",
        "Not evaluable"
      )
    )
  }
  
  ri_label <- if (is.null(result$ri)) {
    
    reference_limit_name(design)
    
  } else if (
    identical(
      result$partition$decision %||% "",
      "partition"
    ) ||
    identical(
      result$age$decision %||% "",
      "age_effect"
    )
  ) {
    
    paste0(
      reference_limit_name(design),
      " overall (not adopted)"
    )
    
  } else {
    
    paste0(
      reference_limit_name(design),
      " candidate"
    )
  }
  
  elems <- c(
    "D7 decision",
    ri_label,
    "Reference design",
    "Active percentile(s)",
    "Context of sample size",
    "Estimated population composition"
  )
  
  vals <- c(
    result$decision %||% "—",
    
    if (!is.null(result$ri)) {
      reference_result_text(
        result$ri,
        design,
        6
      )
    } else {
      "Not estimated"
    },
    
    reference_design_label(design),
    
    reference_design_percentile_text(design),
    
    result$sample_context$text %||% "—",
    
    result$pathology_context$text %||% "Not evaluable"
  )
  
  if (one_sided) {
    
    elems <- c(
      elems,
      "Support reflimR"
    )
    
    vals <- c(
      vals,
      paste0(
        "Descriptive P2.5–P97.5; ",
        "not directly comparable with the active P5/P95."
      )
    )
    
  } else {
    
    if (isTRUE(active[["lower"]])) {
      
      elems <- c(
        elems,
        "Agreement refineR–reflimR · LRL"
      )
      
      vals <- c(
        vals,
        sl(
          get_limit_status(
            comp,
            "lower"
          )
        )
      )
    }
    
    if (isTRUE(active[["upper"]])) {
      
      elems <- c(
        elems,
        "Agreement refineR–reflimR · URL"
      )
      
      vals <- c(
        vals,
        sl(
          get_limit_status(
            comp,
            "upper"
          )
        )
      )
    }
  }
  
  elems <- c(
    elems,
    "Qualitative variable / partition",
    "Quantitative variable / continuous model",
    "Exploration of complexity"
  )
  
  vals <- c(
    vals,
    
    result$partition$message %||% "Not assessed",
    
    paste(
      c(
        result$age$message %||% "Not assessed",
        result$age$model$message %||% NULL
      ),
      collapse = " "
    ),
    
    result$exploration$message %||% "Not required"
  )
  
  data.frame(
    Element = elems,
    Result = vals,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

d7_environment_table <- function(result) {
  if (is.null(result) || !identical(result$type %||% "", "indirect_establishment")) return(NULL)
  pv <- function(pkg) if (requireNamespace(pkg, quietly = TRUE)) as.character(utils::packageVersion(pkg)) else "not installed"
  boot <- if (!is.null(result$refine)) as.character(result$refine$n_bootstrap %||% NA_integer_) else "Not run"
  sd0 <- if (!is.null(result$refine)) as.character(result$refine$seed %||% NA_integer_) else "Not run"
  ptxt <- if (identical(result$partition$decision %||% "", "partition")) "Yes · complete D7 engine by group" else if (isTRUE(result$partition$available)) "Assessed" else "No"
  atxt <- if (identical(result$age$model$decision %||% "", "continuous_candidate")) paste0("Yes · ", result$age$model$n_windows %||% NA_integer_, " windows") else if (identical(result$age$decision %||% "", "age_effect")) "Attempted / under review" else "No"
  data.frame(Element = c("R", "refineR", "reflimR", "mclust", "Bootstrap refineR overall", "Seed refineR overall", "Partitions D7 complete", "Continuous model for quantitative variable"),
             Version = c(R.version.string, pv("refineR"), pv("reflimR"), pv("mclust"), boot, sd0, ptxt, atxt),
             check.names = FALSE, stringsAsFactors = FALSE)
}

run_indirect_establishment_d7 <- function(x, data = NULL, n_bootstrap = 200, seed = 2201,
                                          check_partition = TRUE, check_age = TRUE,
                                          explore_complexity = TRUE, progress = NULL,
                                          reference_design = NULL,
                                          qualitative_label = "Qualitative variable",
                                          quantitative_label = "Quantitative variable") {
  design <- normalize_reference_design(reference_design)
  x <- x[is.finite(x)]
  n <- length(x)
  core <- d7_core_analysis(x, n_bootstrap = n_bootstrap, seed = seed, progress = progress, reference_design = design)
  if (n < 200L || isTRUE(core$technical_error)) {
    core$partition <- list(available = FALSE, status = "grey", decision = "not_checked", message = "Not assessed because D7 cannot be executed with the current dataset.")
    core$age <- list(available = FALSE, status = "grey", decision = "not_checked", message = "Not assessed because D7 cannot be executed with the current dataset.")
    core$exploration <- list(available = FALSE, message = "Not required: the dataset is not evaluable before running the indirect methods.")
    return(core)
  }

  if (is.function(progress)) progress(0.56, "D7 · covariates and partition")
  part <- if (isTRUE(check_partition)) d7_partition_qualitative(data, global_core = core, n_bootstrap = n_bootstrap, seed = seed + 100L, progress = progress, reference_design = design) else
    list(available = FALSE, status = "grey", decision = "not_checked", message = "Qualitative variable unavailable or not evaluable.")
  quantitative_is_age <- exists("quantitative_is_age_like", mode="function") && quantitative_is_age_like(quantitative_label)
  age <- if (isTRUE(check_age)) {
    if (quantitative_is_age) d7_age_screen(data) else d7_quantitative_screen(data, covariate_label=quantitative_label)
  } else list(available = FALSE, status = "grey", decision = "not_checked", message = "Quantitative variable unavailable or not evaluable.")
  if (identical(age$decision %||% "", "age_effect")) {
    age$model <- if (quantitative_is_age) d7_age_model_indirect(data, n_bootstrap = n_bootstrap, seed = seed + 400L, progress = progress, reference_design = design)
      else d7_quantitative_model_indirect(data, n_bootstrap=n_bootstrap, seed=seed+400L, progress=progress, reference_design=design, covariate_label=quantitative_label)
  }

  need_explore <- isTRUE(explore_complexity) && ((core$agreement$status %||% "grey") %in% c("yellow", "red", "grey") ||
                  (core$pathology_context$level %||% "unknown") %in% c("intermediate", "high_pathology", "assumption_compromised") ||
                  identical(part$decision %||% "", "partition") || identical(part$decision %||% "", "indeterminate") ||
                  identical(age$decision %||% "", "age_effect"))
  exploration <- if (need_explore && !is.null(data)) run_indirect_complexity_exploration(data) else
    list(available = FALSE, message = if (need_explore) "No structured data are available for exploration." else "Not required by the D7 workflow.")

  if (is.function(progress)) progress(0.88, "D7 · methodological integration")
  unilateral <- !identical(design$tail, "two_sided")
  global_noun <- if (unilateral) "overall limit" else "overall RI"
  plural_noun <- if (unilateral) "limits" else "RI"
  part_partition <- identical(part$decision %||% "", "partition")
  part_indet <- identical(part$decision %||% "", "indeterminate")
  age_effect <- identical(age$decision %||% "", "age_effect")
  age_candidate <- identical(age$model$decision %||% "", "continuous_candidate")

  if (part_partition && age_effect) {
    status <- "yellow"; decision <- "INCONCLUSIVE — QUALITATIVE × QUANTITATIVE"
    rec <- paste0("There is simultaneous evidence of dependence on ", qualitative_label, " and ", quantitative_label, ". A ", global_noun, " is not defensible, and the two dimensions should not be resolved independently without reviewing their interaction.")
    action <- paste0("Do not implement the ", global_noun, " or the simple candidates for either covariate. Complete a joint modeling strategy and document biological plausibility.")
  } else if (part_partition) {
    status <- "yellow"; decision <- if (unilateral) "CANDIDATE PARTITIONED LIMITS" else "CANDIDATE PARTITIONED RIs"
    rec <- paste0("The qualitative-variable partition is supported by explicit criteria and each group has completed the D7 engine. The ", global_noun, " should not be adopted.")
    action <- paste0("Review the justification for partitioning, the complete D7 results for each group, and biological plausibility. If these are consistent, the ", plural_noun, " by group can be proposed for specialist approval; do not implement them automatically.")
  } else if (part_indet) {
    status <- "yellow"; decision <- "INCONCLUSIVE"
    rec <- "The need for qualitative-variable partitioning is inconclusive according to the integrated criteria."
    action <- if (unilateral) "Do not implement the overall limit or separate limits yet. Lahti 2.5% tail thresholds are not extrapolated to P5/P95; review Harris–Boyd, SDR, each group’s composition, biological plausibility, and external evidence before closing D7." else "Do not implement the overall RI or separate RIs yet. Review Lahti, Harris–Boyd, SDR, each group’s composition, and biological plausibility before closing D7."
  } else if (age_effect && age_candidate) {
    q_decision_label <- if (quantitative_is_age) "AGE" else toupper(quantitative_label)
    status <- "yellow"; decision <- if (unilateral) paste0("CONTINUOUS LIMIT BY ", q_decision_label, " — CANDIDATE") else paste0("CONTINUOUS RI BY ", q_decision_label, " — CANDIDATE")
    rec <- paste0("A continuous effect of ", quantitative_label, " was detected and a candidate indirect curve was constructed; the ", global_noun, " should not be adopted.")
    action <- if (unilateral) "Review the continuous curve of the active percentile, the local refineR windows, and biological coherence. reflimR is descriptive only in P5/P95. Externally validate the model before proposing it for specialist approval; RIveR will not create arbitrary bands." else "Review the continuous curve, the local refineR/reflimR windows, and biological coherence. Externally validate the model before proposing it for specialist approval; RIveR will not create arbitrary bands."
  } else if (age_effect) {
    status <- "yellow"; decision <- if (unilateral) "QUANTITATIVE EFFECT — NO OVERALL LIMIT" else "QUANTITATIVE EFFECT — NO OVERALL RI"
    rec <- paste0("Relevant dependence was detected for ", quantitative_label, "; the continuous model requires review, and a single ", global_noun, " could conceal this variation.")
    action <- paste0("Do not implement a single ", global_noun, ". Review continuous modeling of ", quantitative_label, " and increase/clean the data if needed; do not create arbitrary bands from screening.")
  } else {
    status <- core$status; decision <- core$decision; rec <- core$recommendation; action <- core$action
  }

  core$status <- status; core$decision <- decision; core$recommendation <- rec; core$action <- action
  part$covariate_label <- qualitative_label
  age$covariate_label <- quantitative_label
  core$partition <- part; core$age <- age; core$exploration <- exploration
  core$qualitative_label <- qualitative_label; core$quantitative_label <- quantitative_label
  core$references <- D7_REFERENCES
  core
}
