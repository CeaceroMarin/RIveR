lahti_status <- function(p) {
  if (is.na(p)) return("grey")
  if (p < 0.009 || p > 0.041) return("red")
  if (p >= 0.018 && p <= 0.032) return("green")
  "yellow"
}

harris_boyd_support <- function(x1, x2) {
  x1 <- x1[is.finite(x1)]; x2 <- x2[is.finite(x2)]
  n1 <- length(x1); n2 <- length(x2)
  if (n1 < 3 || n2 < 3) {
    return(list(z = NA_real_, z_critical = NA_real_, sd_ratio = NA_real_,
                z_support = NA, sd_support = NA, supports_partition = NA, n1 = n1, n2 = n2))
  }
  s1 <- stats::sd(x1); s2 <- stats::sd(x2)
  z <- abs(mean(x1) - mean(x2)) / sqrt((s1^2 / n1) + (s2^2 / n2))
  # EP28-A3c / Harris-Boyd: z* = 3 * sqrt((n1+n2)/240).
  z_critical <- 3 * sqrt((n1 + n2) / 240)
  ratio <- if (min(s1, s2) > 0) max(s1, s2) / min(s1, s2) else Inf
  z_support <- is.finite(z) && is.finite(z_critical) && z > z_critical
  sd_support <- is.finite(ratio) && ratio > 1.5
  list(z = z, z_critical = z_critical, sd_ratio = ratio,
       z_support = z_support, sd_support = sd_support,
       supports_partition = isTRUE(z_support) || isTRUE(sd_support),
       n1 = n1, n2 = n2)
}

partition_shape_diagnostic <- function(x) {
  z <- x[is.finite(x)]
  n <- length(z)
  if (n < 4) return(list(n=n, bowley=NA_real_, excess_kurtosis=NA_real_, normality_p=NA_real_, caution=TRUE))
  b <- bowley_skewness(z)
  m <- mean(z); sdv <- stats::sd(z)
  ek <- if (is.finite(sdv) && sdv > 0) mean((z-m)^4)/(sdv^4) - 3 else NA_real_
  pn <- normality_p(z)
  # Anderson-Darling is used as a normality diagnostic, but not in isolation to decide partitioning.
  # Robust skewness and tail behavior (kurtosis) are also considered.
  caution <- (is.finite(b) && abs(b) > 0.15) || (is.finite(ek) && abs(ek) > 1)
  list(n=n, bowley=b, excess_kurtosis=ek, normality_p=pn, caution=caution)
}

partition_discordance_guidance <- function(overall_lahti, hb, g1, g2, subgroup_precision_ok, subgroup_standard) {
  d1 <- partition_shape_diagnostic(g1)
  d2 <- partition_shape_diagnostic(g2)
  shape_caution <- isTRUE(d1$caution) || isTRUE(d2$caution)
  if (identical(overall_lahti, "red") && !isTRUE(hb$supports_partition) &&
      isTRUE(subgroup_precision_ok) && isTRUE(subgroup_standard)) {
    favored <- if (shape_caution) "partition" else "review"
    text <- if (shape_caution) {
      paste(
        "RIveR conditionally favors partitioning because the tail impact according to Lahti is clear,",
        "the specific RIs can be estimated with sufficient precision, and the shape diagnostic suggests that Harris-Boyd, which is based on means and standard deviations, should be interpreted with caution.",
        "The final decision remains a specialist decision and must be justified."
      )
    } else {
      paste(
        "Lahti supports partitioning whereas Harris-Boyd does not. The specific RIs are precise, but no sufficiently compelling distributional reason is identified to automatically prioritize either criterion.",
        "RIveR recommends a documented specialist decision based on biological plausibility, clinical impact, and external evidence."
      )
    }
  } else if (identical(overall_lahti, "green") && isTRUE(hb$supports_partition)) {
    favored <- "review"
    text <- paste(
      "Harris-Boyd supports partitioning, but the Lahti classification impact remains acceptable.",
      "RIveR does not automatically prioritize separation; review biological/clinical relevance and external evidence before deciding."
    )
  } else {
    favored <- "review"
    text <- "The criteria are discordant and RIveR does not have sufficient evidence to automatically favor either alternative."
  }
  list(favored = favored, text = text, shape_caution = shape_caution, group1 = d1, group2 = d2)
}

assess_categorical_partition <- function(data, group_col = "sex", route = c("direct", "indirect"),
                                         n_bootstrap = 20, seed = 1201,
                                         reviewed_extreme_values = numeric(0),
                                         reference_design = NULL) {
  route <- match.arg(route)
  design <- normalize_reference_design(reference_design)
  active <- design$active
  if (!group_col %in% names(data)) stop("No categorical covariate has been defined.")
  g <- as.character(data[[group_col]])
  ok <- !is.na(g) & nzchar(g) & is.finite(data$value)
  dat <- data[ok, , drop = FALSE]
  groups <- names(sort(table(dat[[group_col]]), decreasing = TRUE))
  if (length(groups) < 2) {
    return(list(status = "grey", decision = "not_evaluable", recommendation = "There are not at least two evaluable groups.", details = NULL))
  }
  # v0.3 automatically compares the two most represented groups; additional levels require a specific extension.
  groups2 <- groups[1:2]
  d1 <- dat[as.character(dat[[group_col]]) == groups2[1], "value"]
  d2 <- dat[as.character(dat[[group_col]]) == groups2[2], "value"]
  hb <- harris_boyd_support(d1, d2)

  if (route == "direct") {
    pooled <- run_direct_establishment(c(d1, d2), bootstrap_R = 1000, seed = seed,
                                      reviewed_extreme_values = reviewed_extreme_values,
                                      reference_design = design)
    ri <- pooled$ri

    make_props <- function(z, group) {
      n <- length(z)
      counts <- c(lower = sum(z < ri[1]), upper = sum(z > ri[2]))
      props <- counts / n
      keep <- active
      data.frame(
        group = group,
        side = names(props)[keep],
        n = n,
        outside_n = as.numeric(counts[keep]),
        proportion = as.numeric(props[keep]),
        status = if (identical(design$tail, "two_sided")) vapply(props[keep], lahti_status, character(1)) else rep("grey", sum(keep)),
        stringsAsFactors = FALSE
      )
    }
    props <- rbind(make_props(d1, groups2[1]), make_props(d2, groups2[2]))
    overall <- worst_status(props$status)

    # Always calculate the RIs of both subgroups so that, if partitioning becomes
    # necessary or uncertain, the specialist can see whether those RIs are estimable and with what precision.
    g1 <- tryCatch(run_direct_establishment(d1, bootstrap_R = 1000, seed = seed + 11,
                                             reviewed_extreme_values = reviewed_extreme_values,
                                             reference_design = design), error = function(e) e)
    g2 <- tryCatch(run_direct_establishment(d2, bootstrap_R = 1000, seed = seed + 12,
                                             reviewed_extreme_values = reviewed_extreme_values,
                                             reference_design = design), error = function(e) e)
    subgroup_ok <- !inherits(g1, "error") && !inherits(g2, "error")
    subgroup_precision_ok <- subgroup_ok && isTRUE(g1$precision_ok) && isTRUE(g2$precision_ok)
    subgroup_standard <- subgroup_ok && g1$n >= 120 && g2$n >= 120

    make_subgroup_candidates <- function(group_name, rr) {
      if (inherits(rr, "error") || is.null(rr)) return(NULL)
      dd <- dat[as.character(dat[[group_col]]) == group_name, , drop = FALSE]
      cc <- outlier_candidate_rows(dd, rr)
      if (is.null(cc) || !nrow(cc)) return(NULL)
      cc$group <- group_name
      cc$scope <- if (identical(group_col, "sex")) paste0("Sex subgroup: ", group_name) else paste0("Subgroup ", group_col, ": ", group_name)
      cc
    }
    subgroup_candidates <- do.call(rbind, Filter(Negate(is.null), list(
      make_subgroup_candidates(groups2[1], g1),
      make_subgroup_candidates(groups2[2], g2)
    )))
    if (is.null(subgroup_candidates)) subgroup_candidates <- data.frame()
    subgroup_outlier_pending <- nrow(subgroup_candidates) > 0
    subgroup_outlier_summary <- do.call(rbind, lapply(seq_along(groups2), function(i) {
      rr <- if (i == 1) g1 else g2
      if (inherits(rr, "error") || is.null(rr) || is.null(rr$outlier_assessment)) return(NULL)
      oa <- rr$outlier_assessment
      data.frame(
        Group = groups2[i], n = rr$n,
        `Extreme values detected` = oa$extreme_n %||% 0,
        `Tukey suspected values` = oa$suspect_n %||% 0,
        `Review pending` = if (isTRUE(rr$outlier_review_required)) "Yes" else "No",
        Values = if (length(rr$unreviewed_extreme_values %||% numeric(0))) paste(format(rr$unreviewed_extreme_values, trim=TRUE), collapse=", ") else "—",
        stringsAsFactors = FALSE, check.names = FALSE
      )
    }))

    # Deliberately conservative integration: Lahti does not decide on its own.
    # To recommend partitioning, concordance is required between classification impact
    # (Lahti red zone) and Harris-Boyd support.
    cov_label <- if (identical(group_col, "sex")) "sex" else group_col
    if (!identical(design$tail, "two_sided")) {
      status <- "yellow"
      decision <- "indeterminate"
      rec <- paste0(
        "In one-sided mode ", reference_design_percentile_text(design), ", RIveR does not extrapolate Lahti thresholds defined for 2.5% tails to a 5% tail. ",
        "Harris-Boyd and direct subgroup limits are shown as supporting evidence, but partitioning requires specialist review and biological plausibility; it is not resolved automatically."
      )
      guidance <- NULL
    } else if (overall == "green" && !isTRUE(hb$supports_partition)) {
      status <- "green"
      decision <- "common"
      rec <- paste0(
        "Partitioning is not justified by ", cov_label, ". The common RI maintains adequate classification ",
        "in both groups and Harris-Boyd does not support separation (Z=", round(hb$z, 2),
        "; Z*=", round(hb$z_critical, 2), "; SD ratio=", round(hb$sd_ratio, 2), ")."
      )
    } else if (overall == "red" && isTRUE(hb$supports_partition)) {
      decision <- "partition"
      if (subgroup_precision_ok && subgroup_standard) {
        status <- "green"
        rec <- paste0(
          "Classification with a common RI is inadequate and Harris-Boyd supports separation. ",
          "Both subgroups have n≥120 and acceptable precision; RIveR recommends separate RIs."
        )
      } else if (subgroup_precision_ok) {
        status <- "yellow"
        rec <- paste0(
          "Classification with a common RI and Harris-Boyd support separation, but at least one subgroup has n<120. ",
          "The group-specific RIs meet the defined precision criterion, so partitioning may be considered conditionally with appropriate justification."
        )
      } else {
        status <- "yellow"
        decision <- "indeterminate"
        rec <- paste0(
          "Classification with a common RI and Harris-Boyd suggest partitioning, but subgroup RIs do not achieve sufficient precision ",
          "or could not be estimated. Do not implement separate RIs yet; increase subgroup sample sizes and repeat the assessment."
        )
      }
    } else {
      status <- "yellow"
      decision <- "indeterminate"
      guidance <- partition_discordance_guidance(overall, hb, d1, d2, subgroup_precision_ok, subgroup_standard)
      rec <- paste0(
        "Evidence for partitioning is discordant: Lahti and Harris-Boyd do not lead to the same conclusion ",
        "(Z=", round(hb$z, 2), "; Z*=", round(hb$z_critical, 2),
        "; SD ratio=", round(hb$sd_ratio, 2), "). ", guidance$text
      )
    }
    if (!exists("guidance", inherits = FALSE)) guidance <- NULL

    preliminary_status <- status
    preliminary_decision <- decision
    preliminary_recommendation <- rec
    if (isTRUE(subgroup_outlier_pending)) {
      status <- "yellow"
      decision <- "indeterminate"
      rec <- paste0(
        "Extreme values were detected in at least one subgroup. The partitioning conclusion is provisional until these values are reviewed by the laboratory specialist. ",
        "If any are excluded for a demonstrated cause, RIveR will recalculate subgroup RIs, Lahti, Harris-Boyd, and precision from scratch."
      )
    }

    return(list(
      status = status, decision = decision, recommendation = rec, reference_design = design, groups = groups2, group_col = group_col,
      preliminary_status = preliminary_status, preliminary_decision = preliminary_decision, preliminary_recommendation = preliminary_recommendation,
      subgroup_outlier_pending = subgroup_outlier_pending, subgroup_outlier_summary = subgroup_outlier_summary,
      subgroup_outlier_candidates = subgroup_candidates,
      pooled_ri = ri, lahti = props, harris_boyd = hb, discordance_guidance = guidance,
      group1 = if (inherits(g1, "error")) NULL else g1,
      group2 = if (inherits(g2, "error")) NULL else g2,
      subgroup_precision_ok = subgroup_precision_ok,
      subgroup_standard = subgroup_standard,
      note = paste(c(if (!identical(design$tail, "two_sided")) "One-sided mode: Lahti 2.5% tail thresholds are not used as decision criteria for P5/P95." else NULL,
                     if (length(groups) > 2) "There are more than two categories; this version automatically compares the two most frequent." else NULL), collapse = " ")
    ))
  }

  min_indirect <- 200
  if (length(d1) < min_indirect || length(d2) < min_indirect) {
    return(list(status = "yellow", decision = "indeterminate", recommendation = "At least one group has fewer than 200 results; RIveR cannot robustly recommend or rule out partitioning.",
                groups = groups2, harris_boyd = hb))
  }

  f1 <- tryCatch(run_refineR(d1, n_bootstrap = n_bootstrap, seed = seed, reference_design = design), error = function(e) e)
  f2 <- tryCatch(run_refineR(d2, n_bootstrap = n_bootstrap, seed = seed + 1, reference_design = design), error = function(e) e)
  if (inherits(f1, "error") || inherits(f2, "error")) {
    return(list(status = "yellow", decision = "indeterminate", recommendation = "Indirect modeling of both groups could not be completed; do not implement a partition until the comparison is complete.",
                groups = groups2, harris_boyd = hb))
  }
  if (!identical(design$tail, "two_sided")) {
    return(list(status = "yellow", decision = "indeterminate", reference_design = design, groups = groups2,
                ri_group1 = f1$ri, ri_group2 = f2$ri, comparison = NULL, harris_boyd = hb,
                recommendation = paste0("In one-sided mode ", reference_design_percentile_text(design), ", the refineR candidates for both groups are documented, but RIveR does not automatically resolve partitioning using criteria derived from the two-sided design. Review biological plausibility, clinical impact, and external evidence."),
                note = "Conservative one-sided partitioning: no automatic decision until the P5/P95 criteria have been specifically validated."))
  }
  cmp <- compare_two_ri(f1$ri, f2$ri, groups2)
  if (cmp$status == "red") {
    status <- "green"
    decision <- "partition"
    rec <- "The estimated group RIs are not compatible within the uncertainty margins. RIveR recommends separate RIs for the evaluated groups, subject to biological plausibility."
  } else if (cmp$status == "green" && !isTRUE(hb$supports_partition)) {
    status <- "green"
    decision <- "common"
    rec <- "The group RIs are compatible and Harris-Boyd provides no additional evidence for separation. RIveR recommends a common RI."
  } else {
    status <- "yellow"
    decision <- "indeterminate"
    rec <- "Partitioning criteria are discordant. Do not implement a partition for this covariate until the methodological disagreement has been resolved."
  }
  list(status = status, decision = decision, recommendation = rec, reference_design = design, groups = groups2,
       ri_group1 = f1$ri, ri_group2 = f2$ri, comparison = cmp,
       harris_boyd = hb,
       note = "With indirect data, Lahti applied to raw results is not used as a decision criterion because the LIS mixture contains pathological results.")
}


# v0.16.0 · general qualitative variable (2 or more categories) --------------
# The binary scientific criteria do not change. With >2 categories, they are applied
# to all pairs and the result is integrated conservatively, without
# automatically grouping categories.
assess_qualitative_partition <- function(data, group_col = "qualitative", route = c("direct", "indirect"),
                                         n_bootstrap = 20, seed = 1201,
                                         reviewed_extreme_values = numeric(0),
                                         reference_design = NULL) {
  route <- match.arg(route)
  if (!group_col %in% names(data)) {
    return(list(status="grey", decision="not_evaluable", recommendation="No qualitative variable is available.", groups=character(0)))
  }
  g <- trimws(as.character(data[[group_col]]))
  ok <- is.finite(data$value) & !is.na(g) & nzchar(g)
  dat <- data[ok, , drop=FALSE]
  groups <- names(sort(table(trimws(as.character(dat[[group_col]]))), decreasing=TRUE))
  if (length(groups) < 2L) {
    return(list(status="grey", decision="not_evaluable", recommendation="There are not at least two evaluable categories.", groups=groups))
  }
  if (length(groups) == 2L) {
    return(assess_categorical_partition(dat, group_col=group_col, route=route,
      n_bootstrap=n_bootstrap, seed=seed, reviewed_extreme_values=reviewed_extreme_values,
      reference_design=reference_design))
  }

  cmb <- utils::combn(groups, 2, simplify=FALSE)
  pair_results <- lapply(seq_along(cmb), function(i) {
    pair <- cmb[[i]]
    dd <- dat[trimws(as.character(dat[[group_col]])) %in% pair, , drop=FALSE]
    rr <- tryCatch(assess_categorical_partition(dd, group_col=group_col, route=route,
      n_bootstrap=n_bootstrap, seed=seed + i*17L,
      reviewed_extreme_values=reviewed_extreme_values, reference_design=reference_design), error=function(e) e)
    list(pair=pair, result=rr)
  })

  pair_table <- do.call(rbind, lapply(pair_results, function(z) {
    rr <- z$result
    if (inherits(rr, "error")) {
      return(data.frame(`Category A`=z$pair[1], `Category B`=z$pair[2], Status="grey",
        Decision="not_evaluable", `Lahti`="—", `Harris-Boyd`="—", Detail=conditionMessage(rr), check.names=FALSE))
    }
    lahti_txt <- if (!is.null(rr$lahti) && nrow(rr$lahti)) {
      paste(unique(rr$lahti$status %||% "—"), collapse="/")
    } else if (!is.null(rr$comparison$status)) rr$comparison$status else "—"
    hb <- rr$harris_boyd %||% list()
    hb_txt <- if (is.finite(hb$z %||% NA_real_)) paste0("Z=", round(hb$z,2), "; Z*=", round(hb$z_critical,2), "; SD=", round(hb$sd_ratio,2)) else "—"
    data.frame(`Category A`=z$pair[1], `Category B`=z$pair[2], Status=rr$status %||% "grey",
      Decision=rr$decision %||% "not_evaluable", Lahti=lahti_txt, `Harris-Boyd`=hb_txt,
      Detail=rr$recommendation %||% "—", check.names=FALSE, stringsAsFactors=FALSE)
  }))

  decisions <- vapply(pair_results, function(z) if (inherits(z$result,"error")) "not_evaluable" else z$result$decision %||% "not_evaluable", character(1))
  any_partition <- any(decisions == "partition")
  all_common <- length(decisions) > 0L && all(decisions == "common")
  any_indeterminate <- any(decisions %in% c("indeterminate", "not_evaluable"))

  group_results <- list()
  subgroup_candidates <- list(); subgroup_summary <- list()
  for (i in seq_along(groups)) {
    gg <- groups[i]
    vals <- dat$value[trimws(as.character(dat[[group_col]])) == gg]
    rr <- tryCatch({
      if (route == "direct") run_direct_establishment(vals, bootstrap_R=1000, seed=seed+500L+i,
        reviewed_extreme_values=reviewed_extreme_values, reference_design=reference_design)
      else run_refineR(vals, n_bootstrap=n_bootstrap, seed=seed+500L+i, reference_design=reference_design)
    }, error=function(e) e)
    group_results[[gg]] <- if (inherits(rr,"error")) NULL else rr
    if (route == "direct" && !inherits(rr,"error") && !is.null(rr)) {
      dd <- dat[trimws(as.character(dat[[group_col]])) == gg, , drop=FALSE]
      cc <- outlier_candidate_rows(dd, rr)
      if (!is.null(cc) && nrow(cc)) {
        cc$group <- gg; cc$scope <- paste0("Subgroup ", group_col, ": ", gg)
        subgroup_candidates[[length(subgroup_candidates)+1L]] <- cc
      }
      oa <- rr$outlier_assessment %||% NULL
      if (!is.null(oa)) subgroup_summary[[length(subgroup_summary)+1L]] <- data.frame(
        Group=gg, n=rr$n, `Extreme values detected`=oa$extreme_n %||% 0,
        `Tukey suspected values`=oa$suspect_n %||% 0,
        `Review pending`=if (isTRUE(rr$outlier_review_required)) "Yes" else "No",
        Values=if (length(rr$unreviewed_extreme_values %||% numeric(0))) paste(format(rr$unreviewed_extreme_values, trim=TRUE), collapse=", ") else "—",
        stringsAsFactors=FALSE, check.names=FALSE)
    }
  }
  subgroup_candidates <- if (length(subgroup_candidates)) do.call(rbind, subgroup_candidates) else data.frame()
  subgroup_summary <- if (length(subgroup_summary)) do.call(rbind, subgroup_summary) else data.frame()
  valid_group_results <- Filter(Negate(is.null), group_results)
  subgroup_precision_ok <- length(valid_group_results) == length(groups) && all(vapply(valid_group_results, function(z) isTRUE(z$precision_ok %||% TRUE), logical(1)))
  subgroup_standard <- length(valid_group_results) == length(groups) && all(vapply(valid_group_results, function(z) (z$n %||% 0L) >= if (route == "direct") 120L else 200L, logical(1)))
  pending <- nrow(subgroup_candidates) > 0L

  if (any_partition) {
    preliminary_decision <- "partition"
    preliminary_status <- "yellow"
    preliminary_rec <- paste0("At least one of the ", length(cmb), " pairwise category comparisons supports partitioning using the same binary RIveR criteria. The overall RI should not be adopted. Categories are not automatically merged: the full pattern and biological plausibility must be reviewed.")
  } else if (all_common) {
    preliminary_decision <- "common"
    preliminary_status <- "green"
    preliminary_rec <- paste0("The ", length(cmb), " pairwise category comparisons are compatible with a common RI according to the current RIveR criteria.")
  } else {
    preliminary_decision <- "indeterminate"
    preliminary_status <- "yellow"
    preliminary_rec <- "Category comparisons are discordant or not evaluable. Neither a common RI nor a partition can be adopted automatically."
  }

  decision <- preliminary_decision; status <- preliminary_status; rec <- preliminary_rec
  if (pending) {
    decision <- "indeterminate"; status <- "yellow"
    rec <- paste0("Extreme values were detected in at least one category. The multigroup conclusion is provisional until they are reviewed. ", preliminary_rec)
  }

  list(status=status, decision=decision, recommendation=rec, groups=groups, group_col=group_col,
       pairwise=pair_results, pairwise_table=pair_table, group_results=group_results,
       preliminary_status=preliminary_status, preliminary_decision=preliminary_decision,
       preliminary_recommendation=preliminary_rec,
       subgroup_outlier_pending=pending, subgroup_outlier_summary=subgroup_summary,
       subgroup_outlier_candidates=subgroup_candidates, subgroup_precision_ok=subgroup_precision_ok,
       subgroup_standard=subgroup_standard,
       group1=group_results[[groups[1]]] %||% NULL, group2=group_results[[groups[2]]] %||% NULL,
       note=">2 categories: exhaustive pairwise application of the same partitioning criteria; RIveR does not automatically merge categories.")
}

make_age_bins <- function(age, value, max_bins = 30, min_per_bin = 50, reference_design = NULL) {
  design <- normalize_reference_design(reference_design)
  ok <- is.finite(age) & is.finite(value)
  age <- age[ok]; value <- value[ok]
  if (length(age) < min_per_bin * 3) return(NULL)
  probs <- seq(0, 1, length.out = min(max_bins + 1, floor(length(age) / min_per_bin) + 1))
  br <- unique(as.numeric(stats::quantile(age, probs, na.rm = TRUE, type = 7)))
  if (length(br) < 4) return(NULL)
  bin <- cut(age, breaks = br, include.lowest = TRUE, labels = FALSE)
  spl <- split(seq_along(age), bin)
  rows <- lapply(spl, function(idx) {
    if (length(idx) < min_per_bin) return(NULL)
    data.frame(age_mid = stats::median(age[idx]), n = length(idx),
               median = stats::median(value[idx]),
               p025 = stats::quantile(value[idx], design$pair_percentiles[["lower"]], names = FALSE, type = 6),
               p975 = stats::quantile(value[idx], design$pair_percentiles[["upper"]], names = FALSE, type = 6))
  })
  out <- do.call(rbind, rows)
  attr(out, "reference_design") <- design
  out
}

age_candidate_splits <- function(bins) {
  if (is.null(bins) || nrow(bins) < 5 || !pkg_available("rpart")) return(numeric(0))
  fit <- tryCatch(rpart::rpart(median ~ age_mid, data = bins, weights = n,
                               control = rpart::rpart.control(cp = 0.01, minsplit = 3, maxdepth = 4)),
                  error = function(e) NULL)
  if (is.null(fit) || is.null(fit$splits)) return(numeric(0))
  vals <- suppressWarnings(as.numeric(fit$splits[, "index"]))
  sort(unique(vals[is.finite(vals)]))
}


age_segment_table <- function(data, cuts) {
  d <- data[is.finite(data$age) & is.finite(data$value), c("age", "value"), drop = FALSE]
  if (!nrow(d)) return(NULL)
  cuts <- sort(unique(cuts[is.finite(cuts)]))
  lo <- floor(min(d$age, na.rm = TRUE)); hi <- ceiling(max(d$age, na.rm = TRUE))
  cuts <- cuts[cuts > lo & cuts < hi]
  br <- c(-Inf, cuts, Inf)
  d$segment <- cut(d$age, breaks = br, include.lowest = TRUE, right = TRUE)
  lev <- levels(d$segment)
  rows <- lapply(seq_along(lev), function(i) {
    z <- d[d$segment == lev[i], , drop = FALSE]
    if (!nrow(z)) return(NULL)
    data.frame(segment = i, label = lev[i], n = nrow(z),
               age_min = min(z$age), age_max = max(z$age),
               median = stats::median(z$value), stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}

merge_small_age_segments <- function(data, cuts, min_n = 200) {
  cuts <- sort(unique(cuts[is.finite(cuts)]))
  repeat {
    tab <- age_segment_table(data, cuts)
    if (is.null(tab) || nrow(tab) <= 1 || all(tab$n >= min_n)) break
    i <- which.min(tab$n)
    if (i == 1) remove_idx <- 1
    else if (i == nrow(tab)) remove_idx <- length(cuts)
    else {
      dl <- abs(tab$median[i] - tab$median[i-1])
      dr <- abs(tab$median[i] - tab$median[i+1])
      remove_idx <- if (dl <= dr) i-1 else i
    }
    cuts <- cuts[-remove_idx]
  }
  cuts
}

fit_age_segments_refineR <- function(data, cuts, n_bootstrap = 0, seed = 1201, reference_design = NULL) {
  design <- normalize_reference_design(reference_design)
  d <- data[is.finite(data$age) & is.finite(data$value), c("age", "value"), drop = FALSE]
  cuts <- sort(unique(cuts[is.finite(cuts)]))
  br <- c(-Inf, cuts, Inf)
  seg <- cut(d$age, breaks = br, include.lowest = TRUE, right = TRUE)
  lev <- levels(seg)
  fits <- vector("list", length(lev))
  rows <- vector("list", length(lev))
  for (i in seq_along(lev)) {
    z <- d$value[seg == lev[i]]
    if (length(z) < 200) return(NULL)
    f <- tryCatch(run_refineR(z, n_bootstrap = n_bootstrap, seed = seed + i, reference_design = design), error = function(e) e)
    if (inherits(f, "error")) return(NULL)
    fits[[i]] <- f
    ages <- d$age[seg == lev[i]]
    rows[[i]] <- data.frame(segment = i, label = lev[i], n = length(z),
                            age_min = min(ages), age_max = max(ages),
                            LRL = as.numeric(f$ri[1]), URL = as.numeric(f$ri[2]),
                            stringsAsFactors = FALSE)
  }
  list(cuts = cuts, fits = fits, table = do.call(rbind, rows), reference_design = design)
}

auto_age_partition_indirect <- function(data, candidate_splits, n_bootstrap = 200,
                                        seed = 1201, min_screen_n = 200, reference_design = NULL) {
  design <- normalize_reference_design(reference_design)
  if (!identical(design$tail, "two_sided")) {
    return(list(status = "yellow", decision = "indeterminate", reference_design = design,
                recommendation = paste0("In one-sided mode ", reference_design_percentile_text(design), ", RIveR does not automatically derive discrete age partitions using the two-sided segment-merging algorithm. Prioritize continuous modeling of the active limit and specialist review."),
                table = NULL, cuts = numeric(0),
                note = "Automatic discrete age partitioning has not been specifically validated for P5/P95."))
  }
  if (!length(candidate_splits)) {
    return(list(status = "green", decision = "common",
                recommendation = "No age effect cut-points were identified that justify automatic partitioning.",
                table = NULL, cuts = numeric(0)))
  }
  cuts <- merge_small_age_segments(data, candidate_splits, min_n = min_screen_n)
  if (!length(cuts)) {
    return(list(status = "green", decision = "common",
                recommendation = "After applying the minimum segment-size requirement, no age partition remains.",
                table = NULL, cuts = numeric(0)))
  }

  # First rapid phase without bootstrap: merge adjacent segments whose RIs are compatible.
  repeat {
    fit0 <- fit_age_segments_refineR(data, cuts, n_bootstrap = 0, seed = seed, reference_design = design)
    if (is.null(fit0)) {
      return(list(status = "yellow", decision = "indeterminate",
                  recommendation = "RIveR could not stably estimate all candidate age segments.",
                  table = NULL, cuts = cuts))
    }
    if (nrow(fit0$table) <= 1) break
    cmps <- lapply(seq_len(nrow(fit0$table)-1), function(i) compare_two_ri(fit0$fits[[i]]$ri, fit0$fits[[i+1]]$ri,
                                                                          c(fit0$table$label[i], fit0$table$label[i+1])))
    greens <- which(vapply(cmps, function(z) identical(z$status, "green"), logical(1)))
    if (!length(greens)) break
    # Merge the leftmost compatible pair first; then repeat and reassess.
    cuts <- cuts[-greens[1]]
    if (!length(cuts)) break
  }

  final <- fit_age_segments_refineR(data, cuts, n_bootstrap = n_bootstrap, seed = seed + 100, reference_design = design)
  if (is.null(final)) {
    return(list(status = "yellow", decision = "indeterminate",
                recommendation = "Candidate cut-points were identified, but final bootstrap estimation could not be completed in all partitions.",
                table = NULL, cuts = cuts))
  }
  if (nrow(final$table) <= 1) {
    return(list(status = "green", decision = "common",
                recommendation = "After comparing and merging compatible segments, RIveR recommends a single RI for the studied age range.",
                table = final$table, cuts = numeric(0), fits = final$fits))
  }

  weak <- any(final$table$n < 1000)
  status <- if (weak) "yellow" else "green"
  rec <- paste0("RIveR proposes ", nrow(final$table), " age partitions: ",
                paste(paste0(final$table$age_min, "–", final$table$age_max, " years: ",
                             signif(final$table$LRL, 4), "–", signif(final$table$URL, 4)), collapse = "; "),
                ". Compatible adjacent segments have already been merged automatically.",
                if (weak) " At least one partition has <1000 results and requires enhanced assessment." else "")
  list(status = status, decision = "partition", recommendation = rec,
       table = final$table, cuts = cuts, fits = final$fits,
       note = "Operational algorithm v0.3: rpart proposes cut-points; refineR estimates each segment; VeRUS/UM merges compatible adjacent segments.")
}

assess_age_pattern <- function(data, route = c("direct", "indirect"), n_bootstrap = 200, seed = 1201,
                               reviewed_extreme_values = numeric(0), reference_design = NULL) {
  route <- match.arg(route)
  design <- normalize_reference_design(reference_design)
  if (!"age" %in% names(data)) return(list(status = "grey", recommendation = "Age is not available.", reference_design = design))
  bins <- make_age_bins(data$age, data$value, reference_design = design)
  if (is.null(bins) || nrow(bins) < 4) return(list(status = "grey", recommendation = "There are insufficient age-distributed data to assess the trend."))

  rho <- suppressWarnings(stats::cor(bins$age_mid, bins$median, method = "spearman", use = "complete.obs"))
  med_range <- diff(range(bins$median, na.rm = TRUE))
  overall_iqr <- stats::IQR(data$value, na.rm = TRUE)
  effect <- if (is.finite(overall_iqr) && overall_iqr > 0) med_range / overall_iqr else NA_real_
  splits <- age_candidate_splits(bins)

  if (is.finite(effect) && effect < 0.25 && (is.na(rho) || abs(rho) < 0.30)) {
    status <- "green"
    rec <- "No age effect dependence of sufficient magnitude is detected to recommend automatic partitioning."
  } else if (length(splits) > 0) {
    status <- "yellow"
    rec <- paste0("An age-related pattern is detected. Candidate cut-points: ",
                  paste(round(splits, 1), collapse = ", "),
                  ". They must be validated against a continuous model and equivalence criteria before implementation.")
  } else {
    status <- "yellow"
    rec <- "Possible age dependence is observed, but robust cut-points are not obtained. Continuous modeling is recommended."
  }

  continuous <- if (route == "direct") fit_direct_continuous_age(data, reference_design = design) else NULL
  rel_curve_change <- NA_real_
  auto_partition <- NULL
  decision <- if (status == "green") "common" else "indeterminate"

  if (!is.null(continuous)) {
    curve_width <- stats::median(continuous$curves$p975 - continuous$curves$p025, na.rm = TRUE)
    curve_change <- max(diff(range(continuous$curves$p025, na.rm = TRUE)), diff(range(continuous$curves$p975, na.rm = TRUE)))
    rel_curve_change <- if (is.finite(curve_width) && curve_width > 0) curve_change / curve_width else NA_real_
    if (is.finite(rel_curve_change) && rel_curve_change >= 0.10) {
      status <- "yellow"
      decision <- "continuous"
      rec <- paste0("The continuous GAMLSS model detects a relevant age-related variation in the limits (relative change ≈ ", round(rel_curve_change, 2), "). Priority should be given to ", if (identical(design$tail, "two_sided")) "a continuous RI" else "a continuous reference limit", "; if the LIS does not support it, discrete partitions will need to be derived.")
    }
  }

  if (route == "indirect" && status != "green" && length(splits)) {
    auto_partition <- auto_age_partition_indirect(data, splits, n_bootstrap = n_bootstrap, seed = seed, reference_design = design)
    status <- auto_partition$status
    decision <- auto_partition$decision
    rec <- auto_partition$recommendation
  }

  list(status = status, decision = decision, recommendation = rec, bins = bins, spearman_rho = rho,
       normalized_median_change = effect, candidate_splits = splits,
       continuous = continuous, relative_curve_change = rel_curve_change,
       auto_partition = auto_partition,
       route = route, reference_design = design,
       note = if (route == "indirect") "Automatic partitioning v0.3: candidate cut-points are validated and merged using refineR RIs and VeRUS/UM compatibility. Formal validation is required before production use." else NULL)
}

fit_direct_continuous_age <- function(data, reference_design = NULL) {
  design <- normalize_reference_design(reference_design)
  if (!pkg_available("gamlss") || !pkg_available("gamlss.dist")) return(NULL)
  d <- data.frame(y = data$value, age = data$age)
  d <- d[is.finite(d$y) & is.finite(d$age), , drop = FALSE]
  if (nrow(d) < 120 || length(unique(d$age)) < 10) return(NULL)

  fam <- if (all(d$y > 0)) "BCCG" else "NO"
  fit <- tryCatch({
    if (fam == "BCCG") {
      suppressWarnings(suppressMessages(gamlss::gamlss(
        y ~ gamlss::pb(age), sigma.formula = ~ gamlss::pb(age),
        nu.formula = ~ gamlss::pb(age), family = gamlss.dist::BCCG,
        data = d, trace = FALSE)))
    } else {
      suppressWarnings(suppressMessages(gamlss::gamlss(
        y ~ gamlss::pb(age), sigma.formula = ~ gamlss::pb(age),
        family = gamlss.dist::NO, data = d, trace = FALSE)))
    }
  }, error = function(e) NULL)
  if (is.null(fit)) return(NULL)

  grid <- seq(floor(min(d$age)), ceiling(max(d$age)), length.out = 100)
  cent <- tryCatch(gamlss::centiles.pred(fit, xname = "age", xvalues = grid,
                                         cent = 100 * c(design$pair_percentiles[["lower"]], 0.50, design$pair_percentiles[["upper"]]), plot = FALSE),
                   error = function(e) NULL)
  if (is.null(cent)) return(NULL)
  cent <- as.data.frame(cent)
  if (ncol(cent) < 4) return(NULL)
  names(cent)[1:4] <- c("age", "p025", "p50", "p975")
  list(fit = fit, family = fam, curves = cent[, 1:4, drop = FALSE], reference_design = design)
}

lahti_label <- function(status) {
  switch(status,
         green = status_symbol_text("green", "Does not support partitioning"),
         yellow = status_symbol_text("yellow", "Inconclusive"),
         red = status_symbol_text("red", "Supports partitioning"),
         grey = status_symbol_text("grey", "Not evaluable"),
         status_symbol_text("grey", "Not evaluable"))
}

harris_boyd_label <- function(hb) {
  if (is.null(hb) || is.na(hb$supports_partition %||% NA)) {
    return(status_symbol_text("grey", "Not evaluable"))
  }
  if (isTRUE(hb$supports_partition)) {
    status_symbol_text("red", "Supports partitioning")
  } else {
    status_symbol_text("green", "Does not support partitioning")
  }
}

friendly_group_label <- function(x, group_col = "sex") {
  z <- trimws(as.character(x))
  if (identical(group_col, "sex")) {
    zl <- tolower(z)
    if (zl %in% c("f", "female", "woman", "women")) return("Women")
    if (zl %in% c("m", "male", "man", "men")) return("Men")
  }
  z
}

direct_partition_display_table <- function(partition_result) {
  if (is.null(partition_result) || is.null(partition_result$lahti) || is.null(partition_result$groups)) return(NULL)
  props <- partition_result$lahti
  groups <- partition_result$groups
  fits <- list(partition_result$group1, partition_result$group2)
  compact <- identical(partition_result$decision, "common")
  design <- normalize_reference_design(partition_result$reference_design)
  active <- design$active

  rows <- lapply(seq_along(groups), function(i) {
    g <- groups[i]
    gp <- props[props$group == g, , drop = FALSE]
    lo <- gp[gp$side == "lower", , drop = FALSE]
    up <- gp[gp$side == "upper", , drop = FALSE]
    fit <- fits[[i]]
    digits <- if (!is.null(fit)) fit$display_digits %||% 2 else 2
    ri_txt <- if (!is.null(fit)) reference_result_text(fit$ri, fit$reference_design %||% design, digits) else "Not estimated"
    label <- friendly_group_label(g, "sex")

    if (compact) {
      return(data.frame(
        Group = label,
        n = if (nrow(gp)) gp$n[1] else NA_integer_,
        `Exploratory group RI` = ri_txt,
        `Group RI precision` = if (!is.null(fit)) "Not required (common RI recommended)" else "Not evaluable",
        `Below common LRL` = if (nrow(lo)) paste0(lo$outside_n, "/", lo$n, " (", format_percent(lo$proportion, 1), ")") else "—",
        `Lower criterion` = if (nrow(lo)) lahti_label(lo$status) else "—",
        `Above common URL` = if (nrow(up)) paste0(up$outside_n, "/", up$n, " (", format_percent(up$proportion, 1), ")") else "—",
        `Upper criterion` = if (nrow(up)) lahti_label(up$status) else "—",
        check.names = FALSE, stringsAsFactors = FALSE
      ))
    }

    prec_txt <- if (!is.null(fit) && isTRUE(fit$precision_ok)) "Adequate" else if (!is.null(fit)) "Insufficient" else "Not evaluable"
    data.frame(
      Group = label,
      n = if (nrow(gp)) gp$n[1] else NA_integer_,
      `Group RI` = ri_txt,
      `90% CI LRL` = if (!is.null(fit)) format_lab_interval(fit$ci90_lower, digits) else "—",
      `LRL precision ratio` = if (!is.null(fit)) format_percent(fit$precision_ratio[1], 1) else "—",
      `90% CI URL` = if (!is.null(fit)) format_lab_interval(fit$ci90_upper, digits) else "—",
      `URL precision ratio` = if (!is.null(fit)) format_percent(fit$precision_ratio[2], 1) else "—",
      `RI precision` = prec_txt,
      `Below common LRL` = if (nrow(lo)) paste0(lo$outside_n, "/", lo$n, " (", format_percent(lo$proportion, 1), ")") else "—",
      `Lower criterion` = if (nrow(lo)) lahti_label(lo$status) else "—",
      `Above common URL` = if (nrow(up)) paste0(up$outside_n, "/", up$n, " (", format_percent(up$proportion, 1), ")") else "—",
      `Upper criterion` = if (nrow(up)) lahti_label(up$status) else "—",
      check.names = FALSE, stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  if (!isTRUE(active[["lower"]])) out <- out[, !grepl("LRL|lower|Below", names(out), ignore.case=TRUE), drop=FALSE]
  if (!isTRUE(active[["upper"]])) out <- out[, !grepl("URL|upper|Above", names(out), ignore.case=TRUE), drop=FALSE]
  out
}
