run_reflim <- function(x, target = NULL, reference_design = NULL) {
  if (!pkg_available("reflimR")) stop("The 'reflimR' package is required.")
  if (!pkg_version_at_least("reflimR", "1.1.0")) {
    stop(paste0("The installed version of reflimR is ", as.character(utils::packageVersion("reflimR")),
                ". RIveR D6 requires reflimR >= 1.1.0 to ensure the validated engine. ",
                "Run source(\"install_packages.R\") and restart the application."))
  }
  x <- x[is.finite(x)]
  design <- normalize_reference_design(reference_design)

  # v0.15.3 · preserved two-sided compatibility.
  # For two-sided 95%, reproduce the call validated in v0.14.5 exactly.
  # This prevents the two-sided/one-sided infrastructure from introducing accidental
  # numerical changes in previously frozen cases (D6/D7, including D7E).
  bilateral95 <- identical(design$tail, "two_sided") &&
    isTRUE(all.equal(as.numeric(design$coverage), 0.95, tolerance = 1e-12))
  if (bilateral95) {
    args <- list(x = x, plot.it = FALSE, plot.all = FALSE, print.n = FALSE)
    if (!is.null(target)) args$targets <- as.numeric(target)
    res <- do.call(reflimR::reflim, args)
  } else {
    # In one-sided designs, reflimR 1.1.0 still returns P2.5/P97.5;
    # it is retained only as descriptive support and not as a P5/P95 estimator.
    args <- list(x = x, plot.it = FALSE, plot.all = FALSE, print.n = FALSE,
                 perc.trunc = 2.5)
    res <- do.call(reflimR::reflim, args)
  }
  attr(res, "rilctms_reference_design") <- design
  attr(res, "rilctms_comparable_to_active_percentiles") <- bilateral95
  res
}

extract_reflim_ri <- function(res) {
  c(lower = as.numeric(res$limits[["lower.lim"]]), upper = as.numeric(res$limits[["upper.lim"]]))
}

# Returns the row closest to percentile p and allows a version/return object to
# represent the Percentile column on either a 0–1 or 0–100 scale.
rilctms_percentile_index <- function(percentiles, p) {
  z <- suppressWarnings(as.numeric(percentiles))
  p <- suppressWarnings(as.numeric(p))[1]
  if (!length(z) || !is.finite(p) || !any(is.finite(z))) return(NA_integer_)
  finite <- z[is.finite(z)]
  target <- if (max(abs(finite), na.rm = TRUE) > 1.5) 100 * p else p
  ii <- which.min(abs(z - target))
  if (!length(ii) || !is.finite(z[ii])) return(NA_integer_)
  as.integer(ii[1])
}

# v0.15.4 · quantile directly from the refineR model -------------------------
# In refineR 2.0.0, getRI() also includes VeRUS uncertainty margins.
# These margins are defined for RI limits and are not used to
# retrieve P50. To avoid dependence on the shape of the table
# returned by getRI(), P50 is calculated directly from the parameters of the
# same RWDRI model using cdfTruncatedBoxCox().
rilctms_refine_model_quantile <- function(fit, p = 0.5, point_method = "fullDataEst") {
  if (is.null(fit) || !pkg_available("refineR")) return(NA_real_)
  p <- suppressWarnings(as.numeric(p))[1]
  if (!is.finite(p) || p < 0 || p > 1) return(NA_real_)

  # Directly reproduce the transformation that getRI() applies to the RWDRI model.
  # This avoids dependence on the structure of the returned table and on
  # internal/non-exported package functions. With medianBS, the parameters
  # of the median-bootstrap model are used, as in getRI(pointEst="medianBS").
  use_bs <- identical(point_method %||% "fullDataEst", "medianBS")
  get1 <- function(nm) {
    v <- suppressWarnings(as.numeric(fit[[nm]] %||% NA_real_))
    if (length(v)) v[1] else NA_real_
  }
  bs_ok <- length(fit$MuBS %||% numeric(0)) > 0L &&
    length(fit$SigmaBS %||% numeric(0)) > 0L &&
    length(fit$LambdaBS %||% numeric(0)) > 0L &&
    length(fit$ShiftBS %||% numeric(0)) > 0L

  if (use_bs && bs_ok) {
    mu <- get1("MuMed"); sigma <- get1("SigmaMed")
    lambda <- get1("LambdaMed"); shift <- get1("ShiftMed")
  } else {
    mu <- get1("Mu"); sigma <- get1("Sigma")
    lambda <- get1("Lambda"); shift <- get1("Shift")
  }
  if (!all(is.finite(c(mu, sigma, lambda, shift))) || sigma <= 0) return(NA_real_)

  # The transformed normal distribution is truncated by the Box-Cox domain.
  # For lambda≈0, the lower bound is -Inf and the truncation correction is 0.
  if (abs(lambda) < 1e-12) {
    z <- stats::qnorm(p, mean = mu, sd = sigma)
  } else {
    lower_prob <- stats::pnorm(-1/lambda, mean = mu, sd = sigma)
    prob <- lower_prob + p * (1 - lower_prob)
    prob <- min(max(prob, .Machine$double.eps), 1 - .Machine$double.eps)
    z <- stats::qnorm(prob, mean = mu, sd = sigma)
  }
  q <- tryCatch(refineR::invBoxCox(z, lambda = lambda) + shift,
                error = function(e) NA_real_)
  q <- suppressWarnings(as.numeric(q))[1]
  if (is.finite(q)) return(max(0, q))

  # Documented final fallback: getRI() can explicitly calculate P50. This pathway
  # is not reused as the primary source, keeping D7E decoupled from
  # changes in table format.
  tab <- tryCatch(
    refineR::getRI(fit, RIperc = p, CIprop = 0.95,
                   pointEst = point_method %||% "fullDataEst"),
    error = function(e) NULL
  )
  if (is.data.frame(tab) && nrow(tab) && "PointEst" %in% names(tab)) {
    q <- suppressWarnings(as.numeric(tab$PointEst[1]))
    if (length(q) && is.finite(q[1])) return(q[1])
  }
  NA_real_
}

# CPU budget for refineR.
# v0.16.4: statistical computation is already executed in an independent R process
# (callr::r_bg). To ensure that the Shiny session remains available during
# long-running studies, a second layer of local workers is NOT opened inside
# refineR. findRI() explicitly receives NCores = 1.
#
# Important: this does not make computation synchronous. It only runs the bootstrap
# sequentially inside the external worker; the Shiny process remains separate.
rilctms_refine_ncores <- function() {
  1L
}

run_refineR <- function(x, n_bootstrap = 200, seed = 1201, reference_design = NULL) {
  if (!pkg_available("refineR")) {
    stop("The 'refineR' package version 2.0.0 or later is required. Run source(\"install_packages.R\").")
  }
  if (!pkg_version_at_least("refineR", "2.0.0")) {
    stop(paste0(
      "The installed version of refineR is ", as.character(utils::packageVersion("refineR")),
      ". RIveR requires refineR >= 2.0.0 to calculate VeRUS uncertainty margins. ",
      "Run source(\"install_packages.R\") and restart the application."
    ))
  }
  design <- normalize_reference_design(reference_design)
  x <- x[is.finite(x)]
  n_cores <- rilctms_refine_ncores()
  fit <- refineR::findRI(Data = x, NBootstrap = n_bootstrap, seed = seed, NCores = n_cores)
  point <- if (n_bootstrap > 0) "medianBS" else "fullDataEst"

  p_lower <- design$pair_percentiles[["lower"]]
  p_upper <- design$pair_percentiles[["upper"]]
  pp_limits <- c(p_lower, p_upper)

  # v0.15.4: getRI() is reserved for active limits and their uncertainty.
  # P50 is not an RI limit and is calculated directly from the same RWDRI model.
  tab <- refineR::getRI(fit, IRperc = pp_limits, CIprop = 0.95,
                        pointEst = point, UMprop = 0.90)

  lo <- rilctms_percentile_index(tab$Percentile, p_lower)
  hi <- rilctms_percentile_index(tab$Percentile, p_upper)
  ri <- c(
    lower = if (is.finite(lo)) suppressWarnings(as.numeric(tab$PointEst[lo])) else NA_real_,
    upper = if (is.finite(hi)) suppressWarnings(as.numeric(tab$PointEst[hi])) else NA_real_
  )
  median <- rilctms_refine_model_quantile(fit, p = 0.5, point_method = point)

  list(fit = fit, table = tab, ri = ri, median = median, point_method = point,
       n_bootstrap = n_bootstrap, seed = seed, n_cores = n_cores, reference_design = design,
       requested_percentiles = pp_limits,
       percentile_extraction_method = "getRI_limits_by_label",
       median_extraction_method = "cdfTruncatedBoxCox_model")
}

infer_kosmic_decimals <- function(x, max_decimals = 4L) {
  x <- x[is.finite(x)]
  if (!length(x)) return(1L)

  # Prefer the rounding base estimated by refineR, because it is designed for
  # routinely reported laboratory results. Convert e.g. 1 -> 0 decimals,
  # 0.1 -> 1, 0.01 -> 2. Any the value to avoid unnecessarily slow kosmic fits.
  rb <- tryCatch({
    if (pkg_available("refineR")) refineR::findRoundingBase(x) else NA_real_
  }, error = function(e) NA_real_)

  if (length(rb) && is.finite(rb[1]) && rb[1] > 0) {
    rb <- rb[1]
    d <- if (rb >= 1) 0L else as.integer(ceiling(-log10(rb) - 1e-10))
    return(as.integer(max(0L, min(max_decimals, d))))
  }

  # Fallback: find the smallest number of decimal places that reproduces
  # essentially all values within floating-point tolerance.
  scale <- max(1, stats::IQR(x, na.rm = TRUE), max(abs(x), na.rm = TRUE))
  tol <- max(1e-8, scale * 1e-10)
  for (d in 0:max_decimals) {
    err <- abs(x - round(x, d))
    if (stats::quantile(err, 0.99, na.rm = TRUE, names = FALSE) <= tol) return(as.integer(d))
  }
  2L
}

run_kosmic <- function(x, decimals = NULL) {
  if (!pkg_available("tidykosmic")) {
    return(list(available = FALSE, message = "tidykosmic is not installed; reflimR will be used as the secondary method."))
  }
  x <- x[is.finite(x)]
  if (is.null(decimals)) decimals <- infer_kosmic_decimals(x)
  decimals <- as.integer(decimals[1])

  # tidykosmic::kosmic() requires the 'decimals' argument; it has no default.
  # It represents the number of digits of numerical precision used by kosmic.
  fit <- tryCatch(
    tidykosmic::kosmic(x, decimals = decimals),
    error = function(e) e
  )
  if (inherits(fit, "error")) {
    return(list(available = FALSE, decimals = decimals,
                message = paste("kosmic could not be executed:", conditionMessage(fit))))
  }

  q <- tryCatch(stats::quantile(fit, probs = c(0.025, 0.5, 0.975)), error = function(e) NULL)
  if (is.null(q)) {
    sm <- tryCatch(summary(fit), error = function(e) NULL)
    # Known versions print a named vector of quantiles. Try several common structures.
    if (is.numeric(sm) && length(sm) >= 3) q <- sm[seq_len(3)]
    if (is.list(sm)) {
      cand <- unlist(sm, recursive = TRUE, use.names = TRUE)
      cand_num <- suppressWarnings(as.numeric(cand))
      cand_num <- cand_num[is.finite(cand_num)]
      if (length(cand_num) >= 3) q <- cand_num[seq_len(3)]
    }
  }
  if (is.null(q) || length(q) < 3) {
    return(list(available = FALSE, fit = fit, decimals = decimals,
                message = "kosmic was executed, but this version did not allow the percentiles to be extracted automatically. Review expert mode."))
  }
  q <- as.numeric(q)
  list(available = TRUE, fit = fit, decimals = decimals,
       ri = c(lower = q[1], upper = q[3]), median = q[2],
       message = paste0("OK (decimals = ", decimals, ")"))
}

compare_two_ri <- function(ri1, ri2, labels = c("Method 1", "Method 2")) {
  ri1 <- as.numeric(ri1); ri2 <- as.numeric(ri2)
  if (pkg_available("refineR")) {
    sim <- tryCatch(refineR::getRISimilarity(ri1, ri2, UMprop = 0.90,
                                              Overlap = "OverlapMargins",
                                              printResults = FALSE, verbose = FALSE),
                    error = function(e) NULL)
    if (!is.null(sim)) {
      txt <- paste(capture.output(print(sim)), collapse = "\n")
      lowtxt <- tolower(txt)
      if (grepl("nooverlap|no overlap", lowtxt)) status <- "red"
      else if (grepl("marsoverlap|margin|overlap", lowtxt)) status <- "green"
      else status <- "yellow"
      return(list(status = status, similarity = sim, method = "VeRUS/uncertainty margins",
                  labels = labels))
    }
  }
  # Fallback: standardized Bias Ratio using IR1 as reference.
  sdri <- (ri1[2] - ri1[1]) / 3.92
  br <- if (is.finite(sdri) && sdri > 0) (ri2 - ri1) / sdri else c(NA_real_, NA_real_)
  status <- if (all(is.finite(br)) && all(abs(br) < 0.375)) "green" else "yellow"
  list(status = status, similarity = data.frame(limit = c("lower", "upper"), BR = br),
       method = "Bias Ratio (fallback; |BR|<0.375)", labels = labels)
}

run_indirect_establishment <- function(x, n_bootstrap = 200, seed = 1201, progress = NULL) {
  x <- x[is.finite(x)]
  n <- length(x)
  if (n < 200) {
    return(list(type = "indirect_establishment", n = n, status = "red",
                recommendation = "The dataset contains fewer than 200 results. Automated indirect estimation is not recommended.",
                refine = NULL, kosmic = NULL, reflim = NULL, comparison = NULL,
                action = "Do not implement an RI. Increase the number of results or use a direct strategy."))
  }

  if (is.function(progress)) progress(0.15, paste0("refineR: primary fit + ", n_bootstrap, " bootstrap"))
  refine <- tryCatch(run_refineR(x, n_bootstrap, seed, reference_design = design), error = function(e) e)
  if (inherits(refine, "error")) {
    msg <- paste("refineR could not be executed:", conditionMessage(refine))
    return(list(type = "indirect_establishment", n = n, status = "red",
                recommendation = msg, technical_error = TRUE, technical_message = msg,
                refine = NULL, kosmic = NULL, reflim = NULL, comparison = NULL,
                action = "Correct the technical error before interpreting the study."))
  }

  if (is.function(progress)) progress(0.55, "refineR completed. Running kosmic")
  kosmic <- run_kosmic(x)
  if (isTRUE(kosmic$available)) kosmic$method <- "kosmic"

  if (is.function(progress)) progress(0.66, "Running reflimR as a supporting method")
  refl_raw <- tryCatch(run_reflim(x), error = function(e) e)
  if (inherits(refl_raw, "error")) {
    reflim <- list(available = FALSE, message = paste("reflimR could not be executed:", conditionMessage(refl_raw)))
  } else {
    reflim <- list(available = TRUE, method = "reflimR", fit = refl_raw,
                   ri = extract_reflim_ri(refl_raw), median = NA_real_,
                   message = "Supporting/screening method")
  }

  comparison_kosmic <- NULL
  comparison_reflim <- NULL
  if (isTRUE(kosmic$available)) {
    if (is.function(progress)) progress(0.73, "Comparing refineR with kosmic")
    comparison_kosmic <- compare_two_ri(refine$ri, kosmic$ri, c("refineR", "kosmic"))
  }
  if (isTRUE(reflim$available)) {
    comparison_reflim <- compare_two_ri(refine$ri, reflim$ri, c("refineR", "reflimR"))
  }

  # The primary decision is based on refineR + kosmic when kosmic is available.
  # reflimR is shown as supporting evidence and acts as a substitute comparison only
  # when kosmic is unavailable.
  primary_cmp <- if (!is.null(comparison_kosmic)) comparison_kosmic else comparison_reflim
  confirmatory_name <- if (!is.null(comparison_kosmic)) "kosmic" else if (!is.null(comparison_reflim)) "reflimR" else NA_character_

  if (!is.null(primary_cmp) && primary_cmp$status == "red") {
    status <- "red"
    rec <- paste0("Adoption of the RI is not yet recommended ",
                  signif(refine$ri[1], 5), "–", signif(refine$ri[2], 5),
                  ": the primary and confirmatory methods do not converge within acceptable margins.")
    action <- "Do not implement the RI. Review filters, heterogeneity, pathological contamination, and partitions; repeat the analysis after correcting the cause of disagreement."
    confidence <- "low"
  } else if (!is.null(primary_cmp) && primary_cmp$status == "green" && n >= 5000) {
    status <- "green"
    rec <- paste0("Proposed local RI: ", signif(refine$ri[1], 5), "–", signif(refine$ri[2], 5),
                  ". refineR and ", confirmatory_name, " are compatible and the dataset size is favorable.")
    action <- "You may continue with the automated partition assessment. If partitioning is not indicated and the clinical review is consistent, the RI may proceed to specialist approval."
    confidence <- "high"
  } else if (!is.null(primary_cmp) && primary_cmp$status == "green" && n >= 1000) {
    status <- "yellow"
    rec <- paste0("Proposed candidate RI: ", signif(refine$ri[1], 5), "–", signif(refine$ri[2], 5),
                  ". The methods converge, but the dataset size requires enhanced assessment.")
    action <- "Do not implement it as final yet: complete the partitioning and clinical-coherence assessments. If these assessments are favorable, it may be approved as a local RI with the sample-size limitation documented."
    confidence <- "moderate"
  } else if (is.null(primary_cmp)) {
    status <- "yellow"
    rec <- paste0("refineR estimates an RI of ", signif(refine$ri[1], 5), "–", signif(refine$ri[2], 5),
                  ", but an interpretable confirmatory method could not be obtained.")
    action <- "Do not adopt automatically. Review why kosmic/reflimR are unavailable and repeat confirmation before finalizing the RI."
    confidence <- "moderate-low"
  } else {
    status <- "yellow"
    rec <- paste0("A candidate RI of ", signif(refine$ri[1], 5), "–", signif(refine$ri[2], 5),
                  " was obtained, but agreement between methods does not support a definitive recommendation.")
    action <- "Review the results of each method in the agreement table and resolve the discrepancy before implementing the RI."
    confidence <- "moderate-low"
  }

  list(type = "indirect_establishment", n = n, status = status,
       recommendation = rec, action = action, confidence = confidence,
       technical_error = FALSE,
       refine = refine, kosmic = kosmic, reflim = reflim,
       secondary = if (isTRUE(kosmic$available)) kosmic else reflim,
       comparison = primary_cmp,
       comparisons = list(refine_vs_kosmic = comparison_kosmic,
                          refine_vs_reflimR = comparison_reflim),
       ri = refine$ri)
}

indirect_methods_table <- function(result) {
  if (is.null(result) || result$type != "indirect_establishment") return(NULL)
  rows <- list()
  if (!is.null(result$refine)) {
    rows[[length(rows)+1]] <- data.frame(
      Method = "refineR", Role = "Primary", Available = "Yes",
      LRL = as.numeric(result$refine$ri[1]), Median = as.numeric(result$refine$median),
      URL = as.numeric(result$refine$ri[2]),
      Agreement = "—",
      Detail = paste0("Bootstrap = ", result$refine$n_bootstrap %||% NA_integer_,
                       if (!is.null(result$refine$point_method)) paste0("; pointEst = ", result$refine$point_method) else ""),
      check.names = FALSE)
  }
  k <- result$kosmic
  if (!is.null(k)) {
    if (isTRUE(k$available)) {
      cmp <- result$comparisons$refine_vs_kosmic
      rows[[length(rows)+1]] <- data.frame(
        Method = "kosmic", Role = "Confirmatory", Available = "Yes",
        LRL = as.numeric(k$ri[1]), Median = as.numeric(k$median), URL = as.numeric(k$ri[2]),
        Agreement = if (is.null(cmp)) "Not assessed" else status_label(cmp$status),
        Detail = k$message %||% "", check.names = FALSE)
    } else {
      rows[[length(rows)+1]] <- data.frame(
        Method = "kosmic", Role = "Confirmatory", Available = "No",
        LRL = NA_real_, Median = NA_real_, URL = NA_real_, Agreement = "—",
        Detail = k$message %||% "Unavailable", check.names = FALSE)
    }
  }
  r <- result$reflim
  if (!is.null(r)) {
    if (isTRUE(r$available)) {
      cmp <- result$comparisons$refine_vs_reflimR
      rows[[length(rows)+1]] <- data.frame(
        Method = "reflimR", Role = "Support/screening", Available = "Yes",
        LRL = as.numeric(r$ri[1]), Median = NA_real_, URL = as.numeric(r$ri[2]),
        Agreement = if (is.null(cmp)) "Not assessed" else status_label(cmp$status),
        Detail = r$message %||% "", check.names = FALSE)
    } else {
      rows[[length(rows)+1]] <- data.frame(
        Method = "reflimR", Role = "Support/screening", Available = "No",
        LRL = NA_real_, Median = NA_real_, URL = NA_real_, Agreement = "—",
        Detail = r$message %||% "Unavailable", check.names = FALSE)
    }
  }
  if (!length(rows)) return(NULL)
  do.call(rbind, rows)
}

# D6 · Indirect verification -------------------------------------------------
# Specification based on the staged workflow described in the 2024–2026 literature:
# reflimR/EL -> refineR/VeRUS -> mclust/rpart exploration if disagreement is present.
# Disagreement between criteria is not resolved automatically.

traffic_light_from_text <- function(x) {
  z <- tolower(trimws(as.character(x %||% "")))
  if (!nzchar(z) || is.na(z)) return("grey")
  if (grepl("within tolerance|green|overlap with pe", z)) return("green")
  if (grepl("slightly|yellow|overlap of margins", z)) return("yellow")
  if (grepl("markedly|red|no overlap", z)) return("red")
  "yellow"
}

reflim_limit_statuses <- function(interpretation) {
  out <- c(lower = "grey", upper = "grey")
  if (is.null(interpretation) || !length(interpretation)) return(out)
  # reflimR currently returns an interpretation list containing $dev.lim;
  # Previous versions/structures may expose the text vector directly.
  if (is.list(interpretation) && !is.null(interpretation$dev.lim)) interpretation <- interpretation$dev.lim
  z <- as.character(interpretation)
  nm <- tolower(names(interpretation) %||% rep("", length(z)))
  li <- which(grepl("lower", nm))[1]
  ui <- which(grepl("upper", nm))[1]
  if (!is.finite(li)) li <- 1L
  if (!is.finite(ui)) ui <- min(2L, length(z))
  out["lower"] <- traffic_light_from_text(z[li])
  out["upper"] <- traffic_light_from_text(z[ui])
  out
}

reflim_overall_status <- function(statuses, active = c(lower=TRUE, upper=TRUE)) {
  z <- as.character(statuses[active])
  if (!length(z) || any(z == "grey")) return("grey")
  if (all(z == "green")) return("green")
  if (any(z == "red")) return("red")
  "yellow"
}

verus_limit_statuses <- function(ver) {
  out <- c(lower = "grey", upper = "grey")
  tab <- ver$RIVerificationTab %||% NULL
  if (is.null(tab) || !is.data.frame(tab) || nrow(tab) < 1L) return(out)
  ord <- order(tab$Percentile %||% seq_len(nrow(tab)))
  tab <- tab[ord, , drop = FALSE]
  classify <- function(i) {
    pe <- if ("OverlapPointEst" %in% names(tab)) isTRUE(tab$OverlapPointEst[i]) else FALSE
    ma <- if ("OverlapMargins" %in% names(tab)) isTRUE(tab$OverlapMargins[i]) else FALSE
    if (pe) "green" else if (ma) "yellow" else "red"
  }
  out["lower"] <- classify(1L)
  out["upper"] <- classify(nrow(tab))
  out
}

verus_overall_status <- function(statuses, active = c(lower=TRUE, upper=TRUE)) {
  z <- as.character(statuses[active])
  if (!length(z) || any(z == "grey")) return("grey")
  if (all(z == "green")) return("green")
  if (any(z == "red")) return("red")
  "yellow"
}

run_verus_confirmation <- function(refine, target, reference_design = NULL) {
  design <- normalize_reference_design(reference_design %||% refine$reference_design)
  active <- design$active
  if (!pkg_available("refineR") || is.null(refine$fit)) {
    return(list(available = FALSE, status = "grey", limit_status = c(lower="grey", upper="grey"),
                message = "refineR/VeRUS is unavailable."))
  }
  target <- setNames(as.numeric(target)[1:2], c("lower","upper"))
  # VeRUS works with a pair of limits. In one-sided mode, the inactive limit
  # is fixed to the local refineR estimate and ignored in the decision.
  cand <- setNames(as.numeric(refine$ri), c("lower","upper"))
  cand[active] <- target[active]
  ver <- tryCatch(
    refineR::verifyRI(
      RIdata = refine$fit,
      RIcand = as.numeric(cand),
      RIperc = as.numeric(design$pair_percentiles),
      UMprop = 0.90,
      pointEst = refine$point_method,
      printResults = FALSE,
      generatePlot = FALSE,
      verbose = FALSE
    ),
    error = function(e) e
  )
  if (inherits(ver, "error")) {
    return(list(available = FALSE, status = "grey", limit_status = c(lower="grey", upper="grey"),
                message = paste("VeRUS could not be executed:", conditionMessage(ver))))
  }
  lim <- verus_limit_statuses(ver)
  lim[!active] <- "grey"
  sim <- tryCatch(
    refineR::getRISimilarity(
      RIdata = refine$fit, RIcand = as.numeric(cand),
      RIperc = as.numeric(design$pair_percentiles), UMprop = 0.90,
      pointEst = refine$point_method, Overlap = "OverlapMargins",
      printResults = FALSE, verbose = FALSE
    ), error = function(e) NULL
  )
  list(available = TRUE, raw = ver, table = ver$RIVerificationTab %||% NULL,
       limit_status = lim, status = verus_overall_status(lim, active), similarity = sim,
       message = "VeRUS with 90% uncertainty margins (UM90; reference n=120).",
       reference_design = design)
}

integrate_indirect_limits <- function(reflim_status, verus_status = NULL, confirmation_run = FALSE) {
  rl <- as.character(reflim_status)
  names(rl) <- c("lower", "upper")
  vu <- if (is.null(verus_status)) c(lower="grey", upper="grey") else as.character(verus_status)
  names(vu) <- c("lower", "upper")
  out <- c(lower = "grey", upper = "grey")
  reason <- c(lower = "", upper = "")
  for (nm in c("lower","upper")) {
    r <- rl[nm]; v <- vu[nm]
    if (!isTRUE(confirmation_run)) {
      out[nm] <- r
      reason[nm] <- if (r == "green") "Favorable reflimR/EL screening." else "Pending confirmation."
    } else if (r == "green" && v == "green") {
      out[nm] <- "green"; reason[nm] <- "Favorable agreement between reflimR/EL and VeRUS/UM."
    } else if (r == "red" && v == "red") {
      out[nm] <- "red"; reason[nm] <- "Unfavorable agreement between reflimR/EL and VeRUS/UM."
    } else if (r == "grey" || v == "grey") {
      out[nm] <- "grey"; reason[nm] <- "Could not obtain both criteria."
    } else {
      out[nm] <- "yellow"; reason[nm] <- "Disagreement or intermediate zone between EL and UM."
    }
  }
  list(status = out, reason = reason)
}

indirect_global_status <- function(limit_status, active = c(lower=TRUE, upper=TRUE)) {
  z <- as.character(limit_status[active])
  if (any(z == "red")) return("red")
  if (any(z == "yellow")) return("yellow")
  if (any(z == "grey")) return("grey")
  if (length(z) && all(z == "green")) return("green")
  "grey"
}

# Descriptive sample-size context for D6. It is not an additional clinical threshold
# and does not modify the EL/UM decision; it contextualizes the robustness of
# indirect estimation, especially when refineR is run.
indirect_sample_size_context <- function(n) {
  n <- as.integer(n %||% 0L)
  if (n < 200L) {
    return(list(level = "not_evaluable", label = "Not evaluable",
                message = paste0("n=", n, ": below the approximate operational minimum of 200 results for partition for reflimR screening.")))
  }
  if (n < 1000L) {
    return(list(level = "limited", label = "Limited robustness for refineR",
                message = paste0("n=", n, ": sufficient size for reflimR screening, but refineR confirmation should be interpreted with caution. This note does not by itself modify the EL/UM decision.")))
  }
  if (n < 5000L) {
    return(list(level = "moderate", label = "Intermediate robustness",
                message = paste0("n=", n, ": adequate size to run the workflow; refineR tends to be more stable with larger datasets. This note does not by itself modify the EL/UM decision.")))
  }
  list(level = "favorable", label = "Favorable sample size",
       message = paste0("n=", n, ": favorable sample size for indirect estimation. Data quality and heterogeneity remain important determinants."))
}

run_indirect_complexity_exploration <- function(dat) {
  if (is.null(dat) || !is.data.frame(dat) || !"value" %in% names(dat)) {
    return(list(available = FALSE, message = "No prepared data are available for exploration."))
  }
  x <- dat$value[is.finite(dat$value)]
  mcl <- list(available = FALSE, message = "mclust is not installed.")
  if (length(x) >= 200L && pkg_available("mclust")) {
    # Calculate the BIC explicitly and reuse it in Mclust. This avoids
    # function-lookup dependencies within the namespace and follows the
    # documented of mclust: mclustBIC(data) -> Mclust(data, x = BIC).
    # Mclust() internally reconstructs the call to `mclustBIC` and evaluates it in the
    # parent.frame() workflow. The local alias is also exposed without attaching the package.
    mclustBIC <- mclust::mclustBIC
    bic <- tryCatch(mclustBIC(x, G = 1:4, verbose = FALSE), error = function(e) e)
    mf <- if (!inherits(bic, "error")) {
      tryCatch(mclust::Mclust(x, x = bic, verbose = FALSE), error = function(e) e)
    } else bic
    if (!inherits(mf, "error")) {
      G <- as.integer(mf$G %||% 1L)
      pro <- as.numeric(mf$parameters$pro %||% rep(NA_real_, G))
      mu <- as.numeric(mf$parameters$mean %||% rep(NA_real_, G))
      vv <- mf$parameters$variance %||% list()
      sig2 <- suppressWarnings(as.numeric(vv$sigmasq %||% NA_real_))
      if (length(sig2) == 1L && G > 1L) sig2 <- rep(sig2, G)
      if (length(sig2) < G) sig2 <- c(sig2, rep(NA_real_, G - length(sig2)))
      sdv <- sqrt(pmax(sig2[seq_len(G)], 0))
      if (length(pro) == 1L && G > 1L) pro <- rep(pro, G)
      if (length(pro) < G) pro <- c(pro, rep(NA_real_, G - length(pro)))
      if (length(mu) < G) mu <- c(mu, rep(NA_real_, G - length(mu)))
      ord <- order(mu[seq_len(G)], na.last = TRUE)
      comp <- data.frame(
        Component = seq_len(G),
        Proportion = pro[seq_len(G)][ord],
        Mean = mu[seq_len(G)][ord],
        SD = sdv[ord],
        stringsAsFactors = FALSE, check.names = FALSE
      )
      mcl <- list(available = TRUE, fit = mf, components = G,
                  proportions = comp$Proportion, means = comp$Mean, sds = comp$SD,
                  component_table = comp,
                  message = if (G == 1L) "mclust suggests 1 statistical component (exploratory)." else paste0("mclust suggests ", G, " statistical components (exploratory)."))
    } else {
      mcl <- list(available = FALSE, message = paste("mclust could not be executed:", conditionMessage(mf)))
    }
  }

  rp <- list(available = FALSE, message = "No suitable covariates are available for rpart.")
  dd <- data.frame(value = dat$value)
  vars <- character(0)
  if ("age" %in% names(dat) && any(is.finite(dat$age))) { dd$age <- dat$age; vars <- c(vars, "age") }
  if ("sex" %in% names(dat) && any(!is.na(dat$sex) & nzchar(dat$sex))) { dd$sex <- factor(dat$sex); vars <- c(vars, "sex") }
  if ("date" %in% names(dat) && any(!is.na(dat$date))) {
    dd$hour <- as.numeric(format(dat$date, "%H")) + as.numeric(format(dat$date, "%M"))/60
    vars <- c(vars, "hour")
  }
  if (length(vars) && pkg_available("rpart")) {
    keep <- is.finite(dd$value)
    for (v in vars) keep <- keep & !is.na(dd[[v]])
    dx <- dd[keep, c("value", vars), drop = FALSE]
    if (nrow(dx) >= 200L) {
      fm <- stats::as.formula(paste("value ~", paste(vars, collapse = " + ")))
      minbucket <- max(30L, as.integer(floor(nrow(dx) * 0.05)))
      rf <- tryCatch(rpart::rpart(fm, data = dx, method = "anova",
                                  control = rpart::rpart.control(cp = 0.01, minbucket = minbucket)),
                     error = function(e) e)
      if (!inherits(rf, "error")) {
        split_vars <- if (!is.null(rf$splits) && nrow(rf$splits)) unique(rownames(rf$splits)) else character(0)
        rp <- list(available = TRUE, fit = rf, variables = split_vars,
                   nsplit = length(unique(as.integer(rf$where))) - 1L,
                   message = if (length(split_vars)) paste0("rpart identifies structure associated with: ", paste(split_vars, collapse = ", "), ".") else "rpart does not identify stable partitions with the exploratory parameters.")
      } else {
        rp <- list(available = FALSE, message = paste("rpart could not be executed:", conditionMessage(rf)))
      }
    }
  }
  list(available = isTRUE(mcl$available) || isTRUE(rp$available), mclust = mcl, rpart = rp,
       message = "Hypothesis-generating exploration; it does not automatically modify the candidate RI.")
}

run_indirect_verification <- function(x, target_lower = NA_real_, target_upper = NA_real_,
                                      n_bootstrap = 200, seed = 1201, progress = NULL,
                                      data = NULL, force_refine = FALSE, explore_complexity = TRUE,
                                      reference_design = NULL) {
  design <- normalize_reference_design(reference_design)
  active <- design$active
  one_sided <- !identical(design$tail, "two_sided")
  x <- x[is.finite(x)]
  n <- length(x)
  target <- c(lower = suppressWarnings(as.numeric(target_lower))[1],
              upper = suppressWarnings(as.numeric(target_upper))[1])

  if (n < 200L) {
    return(list(type = "indirect_verification", n = n, target = target, reference_design = design,
                status = "grey", decision = "NOT EVALUABLE", stage = "adequacy",
                recommendation = paste0("Approximately 200 routine results for partition are required to apply the indirect verification workflow. n = ", n, "."),
                action = "Expand the dataset before interpreting indirect verification.",
                reflim_status = "grey", reflim_limit_status = c(lower="grey", upper="grey"),
                verus_status = "grey", verus_limit_status = c(lower="grey", upper="grey"),
                confirmation_run = FALSE, exploration = NULL,
                reflim_comparable = !one_sided,
                sample_size_context = indirect_sample_size_context(n)))
  }

  if (isTRUE(active[["lower"]]) && !is.finite(target[["lower"]])) stop("A numeric candidate lower reference limit is required.")
  if (isTRUE(active[["upper"]]) && !is.finite(target[["upper"]])) stop("A numeric candidate upper reference limit is required.")
  if (all(active) && target[["lower"]] >= target[["upper"]]) stop("The candidate RI must satisfy LRL < URL.")

  if (is.function(progress)) {
    progress(0.20, if (one_sided) "Step 1 · descriptive support with reflimR" else "Step 1 · rapid screening with reflimR/EL")
  }
  refl <- tryCatch(run_reflim(x, reference_design = design), error = function(e) e)
  refl_error <- inherits(refl, "error")

  # reflimR 1.1.0 returns P2.5/P97.5 limits. These are comparable with the
  # two-sided 95% design, but NOT with a one-sided P5/P95 limit. In the latter
  # case, reflimR is retained only as descriptive support and does not enter
  # the EL decision for the active percentile.
  if (refl_error || one_sided) {
    refl_lim <- c(lower="grey", upper="grey")
  } else {
    est_pair <- extract_reflim_ri(refl)
    targets_pair <- est_pair
    targets_pair[active] <- target[active]
    ip <- tryCatch(reflimR::interpretation(est_pair, targets_pair), error=function(e) NULL)
    refl$interpretation <- ip
    refl_lim <- if (is.null(ip)) c(lower="grey", upper="grey") else reflim_limit_statuses(ip)
    refl_lim[!active] <- "grey"
  }
  refl_status <- reflim_overall_status(refl_lim, active)

  # In two-sided mode, the frozen D6 workflow is retained exactly. In one-sided mode,
  # refineR/VeRUS confirmation is mandatory because there is no reflimR EL
  # equivalent for P5/P95.
  need_confirm <- if (one_sided) TRUE else isTRUE(force_refine) || !all(refl_lim[active] == "green") || refl_error
  refine <- NULL
  verus <- list(available = FALSE, status = "grey", limit_status = c(lower="grey", upper="grey"), message = "Not required.")

  if (need_confirm) {
    if (is.function(progress)) progress(0.42, paste0("Step 2 · refineR + VeRUS confirmation (", n_bootstrap, " bootstrap)"))
    refine <- tryCatch(run_refineR(x, n_bootstrap, seed, reference_design = design), error = function(e) e)
    if (!inherits(refine, "error")) {
      verus <- run_verus_confirmation(refine, target, reference_design = design)
    } else {
      verus <- list(available = FALSE, status = "grey", limit_status = c(lower="grey", upper="grey"),
                    message = paste("refineR could not be executed:", conditionMessage(refine)))
      refine <- NULL
    }
  }

  if (one_sided) {
    integrated <- list(status = verus$limit_status %||% c(lower="grey", upper="grey"),
                       reason = c(lower="", upper=""))
    integrated$status[!active] <- "grey"
    for (nm in names(active)[active]) {
      integrated$reason[[nm]] <- switch(integrated$status[[nm]] %||% "grey",
        green = "VeRUS/UM favorable for the active one-sided limit.",
        yellow = "VeRUS/UM in the intermediate zone for the active one-sided limit.",
        red = "VeRUS/UM unfavorable for the active one-sided limit.",
        "VeRUS/UM not evaluable for the active one-sided limit.")
    }
  } else {
    integrated <- integrate_indirect_limits(refl_lim, verus$limit_status, confirmation_run = need_confirm)
  }
  global <- indirect_global_status(integrated$status, active)
  exploration <- NULL
  if (isTRUE(explore_complexity) && global == "yellow") {
    if (is.function(progress)) progress(0.75, "Step 3 · complexity exploration with mclust/rpart")
    exploration <- run_indirect_complexity_exploration(data %||% data.frame(value = x))
  }

  if (one_sided) {
    if (global == "green") {
      decision <- "VERIFIED"
      rec <- paste0("The ", tolower(reference_limit_name(design)), " candidate is favorable in refineR/VeRUS for the active percentile ", reference_design_percentile_text(design), ". reflimR is retained only as descriptive support for P2.5–P97.5.")
      action <- "You may propose maintaining/applying the candidate limit after specialist review of transferability and dataset quality."
    } else if (global == "red") {
      decision <- "NOT VERIFIED"
      rec <- paste0("The ", tolower(reference_limit_name(design)), " candidate is not verified by refineR/VeRUS at the active percentile ", reference_design_percentile_text(design), ".")
      action <- "Do not automatically adapt the estimated limit in this workflow. If a new local limit is to be proposed, continue to the indirect-establishment workflow."
    } else if (global == "yellow") {
      decision <- "INCONCLUSIVE"
      rec <- paste0("VeRUS/UM places the ", tolower(reference_limit_name(design)), " candidate in an intermediate zone. The uncertainty should not be resolved automatically.")
      action <- "Review dataset quality, possible subpopulations, and covariates. mclust/rpart outputs, when available, are exploratory and require specialist interpretation."
    } else {
      decision <- "NOT EVALUABLE"
      rec <- paste("One-sided refineR/VeRUS confirmation could not be completed.", verus$message %||% "")
      action <- "Correct the technical or methodological limitation before issuing a decision on the reference limit."
    }
  } else if (global == "green") {
    decision <- "VERIFIED"
    if (!need_confirm) {
      rec <- "The active limit(s) are green in reflimR/EL screening. The candidate RI is verified by indirect screening; refineR/VeRUS is not required under the staged workflow."
    } else {
      rec <- "reflimR/EL and refineR/VeRUS are concordant and favorable for the evaluated active limit(s). The candidate RI is indirectly verified."
    }
    action <- "You may propose maintaining/applying the candidate RI after specialist review of transferability and dataset quality."
  } else if (global == "red") {
    decision <- "NOT VERIFIED"
    rec <- "At least one active limit is concordantly rejected by reflimR/EL and VeRUS/UM. The candidate RI is not verified with the local data."
    action <- "Do not automatically adapt the estimated limits in this workflow. If a new local RI is to be proposed, continue to the indirect-establishment workflow."
  } else if (global == "yellow") {
    decision <- "INCONCLUSIVE"
    rec <- "There is an intermediate zone or disagreement between reflimR/EL and VeRUS/UM. The divergence is informative and should not be resolved automatically."
    action <- "Review dataset quality, possible subpopulations, and covariates. mclust/rpart outputs, when available, are exploratory and require specialist interpretation."
  } else {
    decision <- "NOT EVALUABLE"
    rec <- paste("Not all criteria in the indirect workflow could be completed.",
                 if (refl_error) paste("reflimR:", conditionMessage(refl)) else "",
                 verus$message %||% "")
    action <- "Correct the technical or methodological limitation before issuing a decision on the RI."
  }

  list(type = "indirect_verification", n = n, target = target, reference_design = design,
       stage = if (one_sided) "confirmation" else if (!need_confirm) "screening" else if (global == "yellow" && !is.null(exploration)) "exploration" else "confirmation",
       reflim = if (refl_error) NULL else refl,
       reflim_status = refl_status, reflim_limit_status = refl_lim,
       reflim_comparable = !one_sided,
       refine = refine,
       verus = if (isTRUE(verus$available)) verus$raw else NULL,
       verus_table = verus$table %||% NULL,
       verus_status = verus$status %||% "grey", verus_limit_status = verus$limit_status %||% c(lower="grey", upper="grey"),
       confirmation_run = need_confirm,
       n_bootstrap = if (!is.null(refine)) as.integer(refine$n_bootstrap %||% n_bootstrap) else 0L,
       refine_seed = if (!is.null(refine)) as.integer(refine$seed %||% seed) else NA_integer_,
       bootstrap_mode = if (!is.null(refine) && (refine$n_bootstrap %||% 0L) < 200L) "test" else if (!is.null(refine)) "final analysis" else "not required",
       refine_point_method = if (!is.null(refine)) refine$point_method %||% "—" else "—",
       integrated_limit_status = integrated$status,
       integrated_limit_reason = integrated$reason,
       exploration = exploration,
       status = global, decision = decision,
       recommendation = rec, action = action,
       method = if (one_sided) "refineR/VeRUS · one-sided verification" else if (!need_confirm) "reflimR/EL · screening" else "reflimR/EL + refineR/VeRUS",
       evidence_note = if (one_sided)
         "In one-sided mode, reflimR 1.1.0 estimates P2.5/P97.5 and is used only as descriptive support; the decision for P5/P95 is based on refineR/VeRUS. A reference limit is not necessarily a clinical decision limit."
       else "EL and UM are different criteria. Their disagreement is not interpreted as a vote and is not resolved automatically.",
       sample_size_context = indirect_sample_size_context(n),
       references = c(
         "Hoffmann G et al. Indirect methods for the verification reference intervals in laboratory medicine. Crit Rev Clin Lab Sci. 2026. doi:10.1080/10408363.2026.2687513",
         "Beck M et al. VeRUS: verification reference intervals based on the uncertainty of sampling. Clin Chem Lab Med. 2026;64:688-697. doi:10.1515/cclm-2025-0728",
         "Hoffmann G et al. A Novel Tool for the Rapid and Transparent Verification of Reference Intervals in Clinical Laboratories. J Clin Med. 2024;13:4397. doi:10.3390/jcm13154397"
       ))
}

subset_indirect_partition <- function(dat, def) {
  if (identical(def$type, "sex")) {
    if (!"sex" %in% names(dat)) return(dat[0, , drop=FALSE])
    return(dat[trimws(as.character(dat$sex)) == as.character(def$sex_value), , drop=FALSE])
  }
  if (!"age" %in% names(dat)) return(dat[0, , drop=FALSE])
  keep <- is.finite(dat$age)
  if (is.finite(def$age_min)) keep <- keep & dat$age >= def$age_min
  if (is.finite(def$age_max)) keep <- keep & dat$age < def$age_max
  dat[keep, , drop=FALSE]
}

run_indirect_verification_partitioned <- function(dat, definitions,
                                                  n_bootstrap = 200, seed = 1201,
                                                  force_refine = FALSE, explore_complexity = TRUE,
                                                  progress = NULL, reference_design = NULL) {
  design <- normalize_reference_design(reference_design)
  if (is.null(definitions) || !nrow(definitions)) stop("No valid partition definitions are available.")
  results <- vector("list", nrow(definitions))
  for (i in seq_len(nrow(definitions))) {
    d <- definitions[i, , drop=FALSE]
    sub <- subset_indirect_partition(dat, d)
    if (is.function(progress)) progress(0.12 + 0.75 * (i-1)/max(1,nrow(definitions)), paste0("Partition ", d$label, " · n=", nrow(sub)))
    z <- run_indirect_verification(sub$value, d$lower, d$upper,
                                   n_bootstrap = n_bootstrap, seed = seed + i,
                                   data = sub, force_refine = force_refine,
                                   explore_complexity = explore_complexity,
                                   reference_design = design)
    z$partition_key <- d$key
    z$partition_label <- d$label
    z$partition_type <- d$type
    results[[i]] <- z
  }
  st <- vapply(results, function(z) z$status %||% "grey", character(1))
  global <- if (any(st == "red")) "red" else if (any(st == "yellow")) "yellow" else if (all(st == "green")) "green" else "grey"
  decision <- switch(global, green="VERIFIED", red="NOT VERIFIED", yellow="INCONCLUSIVE", "NOT EVALUABLE")
  labels <- vapply(results, function(z) paste0(z$partition_label, ": ", z$decision), character(1))
  rec <- paste(labels, collapse = " · ")
  list(type = "indirect_verification", partitioned = TRUE, reference_design = design,
       partition_type = definitions$type[1], partition_results = results,
       n = sum(vapply(results, function(z) z$n %||% 0L, numeric(1))),
       status = global, decision = decision,
       recommendation = paste0("Independent indirect verification by partition. ", rec),
       action = if (global == "green") "All partitions passed the indirect workflow." else if (global == "red") "At least one partition is not verified; do not implement the set of candidate RIs without review." else "Some partitions are inconclusive or not evaluable; review them individually before deciding.",
       method = if (identical(design$tail, "two_sided"))
         "Indirect verification by partition · reflimR/EL → refineR/VeRUS"
       else "One-sided indirect verification by partition · refineR/VeRUS",
       references = unique(unlist(lapply(results, function(z) z$references %||% character(0)))))
}

indirect_status_label <- function(x) {
  z <- as.character(x %||% "grey")
  switch(z,
         green = "🟢 Favorable",
         yellow = "🟡 Intermediate / discordant",
         red = "🔴 Unfavorable",
         grey = "⚪ Not evaluable",
         `not required` = "— Not required",
         z)
}

indirect_verification_display_table <- function(res) {
  design0 <- normalize_reference_design(res$reference_design %||% NULL)
  one_sided0 <- !identical(design0$tail, "two_sided")
  status_cols <- function(z, confirmation = FALSE, integrated = FALSE, screening = FALSE) {
    d <- normalize_reference_design(z$reference_design %||% design0)
    one_sided <- !identical(d$tail, "two_sided")
    out <- list()
    if (isTRUE(d$active[["lower"]])) {
      if (screening && one_sided) st <- "not_applicable" else
        st <- if (integrated) z$integrated_limit_status[["lower"]] %||% "grey" else if (confirmation) z$verus_limit_status[["lower"]] %||% "grey" else z$reflim_limit_status[["lower"]] %||% "grey"
      out[[reference_percentile_label(d$pair_percentiles[["lower"]])]] <- if (identical(st, "not_applicable")) "— Not applicable" else indirect_status_label(st)
    }
    if (isTRUE(d$active[["upper"]])) {
      if (screening && one_sided) st <- "not_applicable" else
        st <- if (integrated) z$integrated_limit_status[["upper"]] %||% "grey" else if (confirmation) z$verus_limit_status[["upper"]] %||% "grey" else z$reflim_limit_status[["upper"]] %||% "grey"
      out[[reference_percentile_label(d$pair_percentiles[["upper"]])]] <- if (identical(st, "not_applicable")) "— Not applicable" else indirect_status_label(st)
    }
    out
  }
  reflim_text <- function(z, d) {
    if (is.null(z$reflim)) return("—")
    rr <- extract_reflim_ri(z$reflim)
    if (!all(is.finite(rr))) return("—")
    if (identical(d$tail, "two_sided")) return(reference_result_text(rr, d, 6))
    paste0("P2.5=", signif(rr[1],6), " · P97.5=", signif(rr[2],6), " (descriptive)")
  }
  if (isTRUE(res$partitioned)) {
    rows <- lapply(res$partition_results %||% list(), function(z) {
      d <- normalize_reference_design(z$reference_design %||% design0)
      base <- data.frame(
        Partition = z$partition_label %||% "—",
        n = z$n %||% NA_integer_,
        `Candidate limit / RI` = reference_result_text(z$target, d, 6),
        `Local estimate reflimR` = reflim_text(z, d),
        `Local estimate refineR` = if (!is.null(z$refine)) reference_result_text(z$refine$ri, d, 6) else "—",
        check.names = FALSE, stringsAsFactors = FALSE
      )
      scr <- status_cols(z, screening=TRUE)
      ver <- if (isTRUE(z$confirmation_run)) status_cols(z, confirmation=TRUE) else setNames(as.list(rep("Not required", length(scr))), names(scr))
      for (nm in names(scr)) base[[paste0("reflimR/EL · ", nm)]] <- scr[[nm]]
      for (nm in names(ver)) base[[paste0("VeRUS/UM · ", nm)]] <- ver[[nm]]
      base$Decision <- z$decision %||% "—"
      base
    })
    if (!length(rows)) return(data.frame(Message="No results"))
    return(do.call(rbind, rows))
  }
  d <- design0
  ri_refl <- reflim_text(res, d)
  ri_refine <- if (!is.null(res$refine)) reference_result_text(res$refine$ri, d, 6) else "—"
  if (one_sided0) {
    stages <- data.frame(
      Stage = c("Descriptive reflimR support (P2.5–P97.5)", "refineR/VeRUS confirmation", "Integration"),
      `Local estimate` = c(ri_refl, ri_refine, "—"),
      Result = c("— Not applicable to active P5/P95", indirect_status_label(res$verus_status %||% "grey"), res$decision %||% "—"),
      check.names = FALSE, stringsAsFactors = FALSE
    )
  } else {
    stages <- data.frame(
      Stage = c("reflimR/EL screening", "refineR/VeRUS confirmation", "Integration"),
      `Local estimate` = c(ri_refl, ri_refine, "—"),
      Result = c(indirect_status_label(res$reflim_status %||% "grey"), indirect_status_label(if (isTRUE(res$confirmation_run)) res$verus_status %||% "grey" else "not required"), res$decision %||% "—"),
      check.names = FALSE, stringsAsFactors = FALSE
    )
  }
  scr <- status_cols(res, screening=TRUE)
  ver <- if (isTRUE(res$confirmation_run)) status_cols(res, confirmation=TRUE) else setNames(as.list(rep("Not required", length(scr))), names(scr))
  integ <- status_cols(res, integrated=TRUE)
  for (nm in names(scr)) stages[[nm]] <- c(scr[[nm]], ver[[nm]], integ[[nm]])
  stages[, c("Stage", names(scr), "Local estimate", "Result"), drop=FALSE]
}

