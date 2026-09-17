route_recommendation <- function(study_type, data_origin, n,
                                 population_defined = TRUE,
                                 preanalytic_ok = TRUE,
                                 analytical_stable = TRUE,
                                 qc_ok = TRUE,
                                 target_present = FALSE) {
  blockers <- character(0)
  warnings <- character(0)

  if (!isTRUE(analytical_stable)) blockers <- c(blockers, "Analytical stability has not been confirmed.")
  if (!isTRUE(qc_ok)) blockers <- c(blockers, "Acceptable quality control has not been confirmed.")

  if (study_type == "establish" && data_origin == "direct") {
    if (!isTRUE(population_defined)) blockers <- c(blockers, "The reference population is not defined.")
    if (!isTRUE(preanalytic_ok)) blockers <- c(blockers, "Preanalytical conditions are not sufficiently controlled/documented.")
    if (n < 3) blockers <- c(blockers, "At least 3 valid results are required for an exploratory estimate.")
    else if (n < 120) warnings <- c(warnings, "n<120: a small-sample pathway will be applied. Acceptability depends on model assumptions and CI precision, not on an arbitrary universal minimum.")
    route <- "direct"
    msg <- "Direct establishment"
  } else if (study_type == "establish" && data_origin == "indirect") {
    if (n < 200) blockers <- c(blockers, "Fewer than 200 results are available for the automated indirect pathway.")
    route <- "indirect"
    msg <- "Indirect establishment"
  } else if (study_type %in% c("verify", "review") && data_origin == "direct") {
    if (!target_present) blockers <- c(blockers, "The candidate/current RI must be entered.")
    if (!isTRUE(population_defined)) blockers <- c(blockers, "The target population and selection criteria for reference individuals are not sufficiently defined.")
    if (!isTRUE(preanalytic_ok)) blockers <- c(blockers, "Preanalytical conditions for verification are not sufficiently controlled/documented.")
    if (n < 20) warnings <- c(warnings, "n<20: direct verification can run only to document a NOT EVALUABLE result; the cohort must be completed to 20 valid individuals.")
    route <- "direct"
    msg <- "Direct verification"
  } else if (study_type %in% c("verify", "review") && data_origin == "indirect") {
    if (!target_present) blockers <- c(blockers, "The candidate/current RI must be entered.")
    if (n < 200) warnings <- c(warnings, "n<200: the D6 workflow will run only to document a NOT EVALUABLE result; the dataset must be expanded.")
    route <- "indirect"
    msg <- if (study_type == "review") "Indirect review of the current RI" else "Indirect verification"
  } else {
    route <- data_origin
    msg <- "Study pathway"
  }

  status <- if (length(blockers)) "red" else if (length(warnings)) "yellow" else "green"
  list(status = status, route = route, title = msg, blockers = blockers, warnings = warnings,
       can_run = length(blockers) == 0)
}


# v0.17.7 · recommended numerical result on the final screen -------------------
# This layer DOES NOT recalculate any statistic. It only summarizes the configuration
# already favored by the engine (common RI, qualitative partition, quantitative partition/
# model, or verified candidate RI). When two dimensions require
# a joint strategy that is not available, it does not fabricate combinations.
rilctms_recommended_result_payload <- function(main_result, partition_result = NULL,
                                               age_result = NULL, final = NULL) {
  empty <- function(title = "No recommended numerical result yet",
                    note = "The methodological decision remains open; an RI should not be selected from partial results.",
                    status = "yellow") {
    list(show = TRUE, has_values = FALSE, status = status, title = title,
         table = NULL, note = note, kind = "none")
  }
  if (is.null(main_result)) return(list(show = FALSE, has_values = FALSE, table = NULL))

  design <- normalize_reference_design(main_result$reference_design %||% NULL)
  digits <- main_result$display_digits %||% 2
  p <- partition_result %||% main_result$partition %||% NULL
  q <- age_result %||% main_result$age %||% NULL
  pres <- final$partition_resolution$decision %||% NULL
  pdec <- if (!is.null(pres) && pres %in% c("partition", "common")) pres else p$decision %||% NULL
  qdec <- q$decision %||% NULL
  qlabel <- q$covariate_label %||% main_result$quantitative_label %||% "quantitative variable"
  plabel <- p$covariate_label %||% main_result$qualitative_label %||% "qualitative variable"
  result_name <- if (identical(design$tail, "two_sided")) "RI recommended" else paste0(reference_limit_name(design), " recommended")

  common_payload <- function(ri, title = "Recommended result · common RI", note = NULL, status = "green") {
    if (is.null(ri) || length(ri) < 2L) return(empty("Common result not available", "There is no complete numerical result to display.", "grey"))
    tab <- data.frame(Application = "Common", stringsAsFactors = FALSE, check.names = FALSE)
    tab[[result_name]] <- reference_result_text(ri, design, digits)
    list(show = TRUE, has_values = TRUE, status = status, title = title, table = tab,
         note = note %||% "This is the single numerical configuration that RIveR proposes for specialist approval.", kind = "common")
  }

  qualitative_payload <- function(pp, status = "yellow") {
    if (is.null(pp)) return(empty())
    # D7: refineR is the primary estimator; reflimR is not included in this summary.
    # v0.17.7: whenever possible, the final table is constructed from
    # the numerical objects of the substudy analyses and uses the same decimal places as
    # the main result. This avoids exposing the 6 internal decimal places used by D7.
    subs <- pp$substudies %||% pp$group_results %||% NULL
    if (!is.null(subs) && length(subs)) {
      labs <- pp$groups %||% names(subs)
      if (is.null(labs) || length(labs) != length(subs)) labs <- names(subs)
      if (is.null(labs) || !length(labs)) labs <- as.character(seq_along(subs))
      rows <- lapply(seq_along(subs), function(i) {
        z <- subs[[i]]
        if (is.null(z$ri)) return(NULL)
        d <- normalize_reference_design(z$reference_design %||% design)
        data.frame(.group = labs[i],
                   .ri = reference_result_text(z$ri, d, digits),
                   .status = z$decision %||% "CANDIDATE",
                   stringsAsFactors = FALSE, check.names = FALSE)
      })
      rows <- Filter(Negate(is.null), rows)
      if (length(rows)) {
        tab <- do.call(rbind, rows)
        names(tab) <- c(plabel, result_name, "Status")
        return(list(show = TRUE, has_values = TRUE, status = status,
                    title = paste0("Recommended result · specific RIs according to ", plabel), table = tab,
                    note = "Only the primary refineR candidates for each category are shown, using the same rounding as the main result. Supporting methods and discarded alternatives are not repeated on this screen.",
                    kind = "qualitative_partition"))
      }
    }
    # Compatibility with legacy objects that retain only the summary table.
    if (!is.null(pp$table) && is.data.frame(pp$table) && nrow(pp$table) && all(c("Group", "refineR") %in% names(pp$table))) {
      keep <- c("Group", "refineR")
      if ("D7 group decision" %in% names(pp$table)) keep <- c(keep, "D7 group decision")
      tab <- pp$table[, keep, drop = FALSE]
      names(tab)[1:2] <- c(plabel, result_name)
      if (ncol(tab) == 3L) names(tab)[3] <- "Status"
      return(list(show = TRUE, has_values = TRUE, status = status,
                  title = paste0("Recommended result · specific RIs according to ", plabel), table = tab,
                  note = "Only the primary refineR candidates for each category are shown; supporting methods and discarded alternatives are not repeated on this screen.",
                  kind = "qualitative_partition"))
    }
    if (!is.null(pp$group1$ri) && !is.null(pp$group2$ri) && length(pp$groups %||% character(0)) >= 2L) {
      labs <- pp$groups[1:2]
      tab <- data.frame(.group = labs,
                        .ri = c(reference_result_text(pp$group1$ri, pp$group1$reference_design %||% design, digits),
                                reference_result_text(pp$group2$ri, pp$group2$reference_design %||% design, digits)),
                        .status = c(pp$group1$decision %||% "CANDIDATE", pp$group2$decision %||% "CANDIDATE"),
                        stringsAsFactors = FALSE, check.names = FALSE)
      names(tab) <- c(plabel, result_name, "Status")
      return(list(show = TRUE, has_values = TRUE, status = status,
                  title = paste0("Recommended result · specific RIs according to ", plabel), table = tab,
                  note = "The final screen shows only the RIs included in the recommended partition, using the same rounding as the main result.", kind = "qualitative_partition"))
    }
    empty("Recommended partition without an available numerical table", "Review the result of the partition before approval.", status)
  }

  quantitative_partition_payload <- function(qq, status = "yellow") {
    tab <- tryCatch(age_partition_display_table(qq), error = function(e) NULL)
    if (is.null(tab) || !is.data.frame(tab) || !nrow(tab)) return(empty("Quantitative partition recommended without table available", "The final numerical summary could not be constructed.", status))
    if (!isTRUE(qq$covariate_is_age) && exists("quantitative_relabel_text", mode = "function")) {
      names(tab) <- vapply(names(tab), function(nm) quantitative_relabel_text(nm, qlabel), character(1))
      for (nm in names(tab)) if (is.character(tab[[nm]])) tab[[nm]] <- quantitative_relabel_text(tab[[nm]], qlabel)
    }
    list(show = TRUE, has_values = TRUE, status = status,
         title = paste0("Recommended result · RI for intervals of ", qlabel), table = tab,
         note = "Only quantitative-variable intervals that passed the partition-validation workflow are shown.", kind = "quantitative_partition")
  }

  quantitative_continuous_payload <- function(qq, d7 = FALSE, status = "yellow") {
    continuous_term <- if (identical(design$tail, "two_sided")) "continuous RI" else paste0("continuous ", reference_limit_name(design))
    if (isTRUE(d7)) {
      m <- qq$model %||% NULL
      if (is.null(m) || !identical(m$decision %||% "", "continuous_candidate")) {
        return(empty("Continuous model not ready for adoption", paste0("The model for ", qlabel, " did not reach candidate status; no application RI is shown."), "yellow"))
      }
      tab <- tryCatch(d7_continuous_application_table(m, age_like = isTRUE(qq$covariate_is_age) ||
        (exists("quantitative_is_age_like", mode="function") && quantitative_is_age_like(qlabel))), error=function(e) NULL)
      if (is.null(tab) || !nrow(tab)) return(empty("Continuous model candidate without table of application", "Review the curve before approval.", status))
      return(list(show = TRUE, has_values = TRUE, status = status,
                  title = paste0("Recommended result · ", continuous_term, " according to ", qlabel), table = tab,
                  note = paste0("Each row is a prediction of the continuous curve according to ", qlabel, "; it does not represent a band. The continuous curve is the primary scientific result."),
                  kind = "quantitative_continuous"))
    }
    annual_tab <- qq$sil_adaptation$annual_table %||% NULL
    if (is.null(annual_tab) || !is.data.frame(annual_tab) || !nrow(annual_tab)) annual_tab <- tryCatch(age_curve_display_table(qq), error=function(e) NULL)
    if (is.null(annual_tab) || !is.data.frame(annual_tab) || !nrow(annual_tab)) return(empty(paste0(continuous_term, " recommended without table available"), "The curve remains the main result; review the quantitative-variable panel.", status))
    if (!isTRUE(qq$covariate_is_age)) {
      names(annual_tab)[1] <- qlabel
      if (exists("quantitative_relabel_text", mode="function")) {
        names(annual_tab) <- vapply(names(annual_tab), function(nm) quantitative_relabel_text(nm, qlabel), character(1))
      }
    }

    # v1.0.1 · implementation hierarchy on the final screen.
    # If a validated LIS discretization exists, it is shown FIRST because it is
    # the operational response to be copied into the LIS when the LIS cannot apply
    # the GAMLSS. The continuous model remains available and is shown next as
    # the preferred scientific result and as an implementation option when the LIS
    # supports a formula or a table parameterized.
    a <- qq$sil_adaptation %||% NULL
    discrete_tab <- NULL
    if (!is.null(a) && isTRUE(a$ok) && isTRUE(a$acceptable)) {
      full_discrete <- tryCatch(age_sil_display_table(qq), error=function(e) NULL)
      if (!is.null(full_discrete) && is.data.frame(full_discrete) && nrow(full_discrete)) {
        active_cols <- c(
          if (isTRUE(design$active[["lower"]])) reference_percentile_label(design$pair_percentiles[["lower"]]) else character(0),
          if (isTRUE(design$active[["upper"]])) reference_percentile_label(design$pair_percentiles[["upper"]]) else character(0),
          if (identical(design$tail, "two_sided")) c("LRL", "URL") else character(0)
        )
        keep <- intersect(c("Band", active_cols), names(full_discrete))
        if (length(keep) >= 2L) discrete_tab <- full_discrete[, keep, drop=FALSE]
        if (!is.null(discrete_tab) && !isTRUE(qq$covariate_is_age)) {
          names(discrete_tab)[1] <- paste0("Interval of ", qlabel)
          if (exists("quantitative_relabel_text", mode="function")) {
            names(discrete_tab) <- vapply(names(discrete_tab), function(nm) quantitative_relabel_text(nm, qlabel), character(1))
            for (nm in names(discrete_tab)) if (is.character(discrete_tab[[nm]])) discrete_tab[[nm]] <- quantitative_relabel_text(discrete_tab[[nm]], qlabel)
          }
        }
      }
    }

    if (!is.null(discrete_tab) && nrow(discrete_tab)) {
      return(list(
        show = TRUE, has_values = TRUE, status = status,
        title = "If the LIS cannot apply GAMLSS · validated discrete bands",
        table = discrete_tab,
        note = paste0("This is the first operational option when the LIS does not support the continuous model. It is a validated approximation derived from GAMLSS; the bands are not independently re-established RIs."),
        secondary_title = paste0("Preferred scientific model · ", continuous_term, " according to ", qlabel),
        secondary_table = annual_tab,
        secondary_note = paste0("If the LIS supports a formula or parameterized table, use this continuous model. The preceding discretization is only an implementation alternative for a limited LIS."),
        kind = "quantitative_continuous_with_sil_fallback"
      ))
    }

    list(show = TRUE, has_values = TRUE, status = status,
         title = paste0("Recommended result · ", continuous_term, " according to ", qlabel), table = annual_tab,
         note = paste0("The continuous function is the recommended result. No validated discrete adaptation is available to replace it in a LIS that does not support the continuous model."),
         kind = "quantitative_continuous")
  }

  # Verification/review: the candidate RI is shown only when the workflow retains it.
  if ((main_result$type %||% "") %in% c("direct_verification", "indirect_verification")) {
    verified <- grepl("VERIFIED", main_result$decision %||% "", fixed = TRUE) && !grepl("NOT VERIFIED", main_result$decision %||% "", fixed = TRUE)
    vstatus <- main_result$status %||% "yellow"
    if (!verified || identical(vstatus, "red") || identical(vstatus, "grey")) {
      return(empty("No RI is recommended for retention yet", main_result$recommendation %||% "Verification has not concluded favorably.", vstatus))
    }
    if (isTRUE(main_result$partitioned) && length(main_result$partition_results %||% list())) {
      rows <- lapply(main_result$partition_results, function(z) {
        if (is.null(z$target)) return(NULL)
        data.frame(Partition = z$partition_label %||% "—",
                   `Verified candidate RI` = reference_result_text(z$target, z$reference_design %||% design, z$display_digits %||% digits),
                   stringsAsFactors = FALSE, check.names = FALSE)
      })
      rows <- Filter(Negate(is.null), rows)
      if (length(rows)) return(list(show=TRUE,has_values=TRUE,status=vstatus,title="Recommended result · verified candidate RIs by partition",table=do.call(rbind,rows),note=if (identical(vstatus, "green")) "Only candidate RIs included in the verified configuration are shown." else "The candidate RIs passed the verification criterion, but a traceability or documentation warning remains and must be resolved before closing the study.",kind="verification_partitioned"))
    }
    return(common_payload(main_result$target, "Recommended result · verified candidate RI", if (identical(vstatus, "green")) "The candidate RI passed the verification workflow and is the result RIveR proposes retaining/applying after specialist approval." else "The candidate RI passed the verification criterion, but a traceability or documentation warning remains and must be resolved before closing the study.", vstatus))
  }

  is_d7 <- identical(main_result$module %||% "", "D7") && identical(main_result$type %||% "", "indirect_establishment")
  if (is_d7) {
    mdec <- toupper(main_result$decision %||% "")
    if (grepl("^INCONCLUSIVE", mdec) || grepl("^QUANTITATIVE EFFECT", mdec)) {
      return(empty("No implementable RI recommended", main_result$recommendation %||% "The combined evidence does not allow selection of an RI configuration.", "yellow"))
    }
    if (identical(pdec, "partition") && grepl("PARTITION", mdec)) return(qualitative_payload(p, main_result$status %||% "yellow"))
    if (!is.null(q$model) && identical(q$model$decision %||% "", "continuous_candidate") && grepl("CONTINUOUS", mdec)) {
      return(quantitative_continuous_payload(q, d7=TRUE, status=main_result$status %||% "yellow"))
    }
    if (!is.null(main_result$ri) && !identical(main_result$status %||% "", "red")) {
      return(common_payload(main_result$ri, "Recommended result · overall RI candidate", "No covariate has been identified that justifies replacing this result with a specific configuration.", main_result$status %||% "yellow"))
    }
    return(empty("No recommended numerical result", main_result$recommendation %||% "The study has not produced an implementable candidate.", main_result$status %||% "yellow"))
  }

  # Direct establishment: the final configuration depends on covariates that have already
  # been assessed/resolved. If both require a change, a joint strategy is needed.
  p_effect <- identical(pdec, "partition")
  q_effect <- qdec %in% c("partition", "continuous")
  if (p_effect && q_effect) {
    return(empty("A joint covariate strategy is required",
                 paste0("Both ", plabel, " and ", qlabel, " modify the recommended configuration. RIveR does not combine independent RIs in a crossed table without a validated joint model."), "yellow"))
  }
  if (identical(pdec, "indeterminate") && is.null(pres)) return(empty("Qualitative partition pending", p$recommendation %||% "Resolve the partition before selecting an RI."))
  if (identical(qdec, "indeterminate")) return(empty("Quantitative variable pending", q$recommendation %||% "The quantitative assessment must be completed before selecting an RI."))
  if (p_effect) return(qualitative_payload(p, p$status %||% "yellow"))
  if (identical(qdec, "partition")) return(quantitative_partition_payload(q, q$status %||% "yellow"))
  if (identical(qdec, "continuous")) return(quantitative_continuous_payload(q, d7=FALSE, status=q$status %||% "yellow"))
  if (!is.null(main_result$ri) && !identical(main_result$status %||% "", "red")) {
    return(common_payload(main_result$ri, "Recommended result · common RI", "The assessed covariates do not require replacing the overall RI with a specific configuration.", main_result$status %||% "green"))
  }
  empty("No RI is recommended to implement", main_result$recommendation %||% "Resolve the methodological blockers before selecting a result.", main_result$status %||% "yellow")
}

compose_final_recommendation <- function(main_result, partition_result = NULL, age_result = NULL,
                                         partition_resolution = NULL, small_sample_resolution = NULL) {
  if (is.null(main_result)) {
    return(list(status = "grey", headline = "No analysis is available",
                text = "Run the analysis before issuing a recommendation.",
                action = "Run the analysis.", notes = character(0), is_final = FALSE))
  }

  st <- main_result$status %||% "grey"
  text <- main_result$recommendation %||% "No automatic recommendation."
  action <- main_result$action %||% text
  notes <- character(0)
  is_final <- FALSE
  final_decision <- NULL

  pdec <- partition_result$decision %||% NULL
  adec <- age_result$decision %||% NULL
  pres <- partition_resolution$decision %||% NULL
  sres <- small_sample_resolution$decision %||% NULL
  favored <- partition_result$discordance_guidance$favored %||% "review"
  digits <- main_result$display_digits %||% 2
  qualitative_label <- partition_result$covariate_label %||% main_result$qualitative_label %||% "qualitative variable"
  quantitative_label <- age_result$covariate_label %||% main_result$quantitative_label %||% "quantitative variable"

  group_ri_text <- function() {
    main_design <- normalize_reference_design(main_result$reference_design %||% NULL)
    if (!is.null(partition_result$group_results) && length(partition_result$group_results)) {
      bits <- vapply(names(partition_result$group_results), function(g) {
        z <- partition_result$group_results[[g]]
        if (is.null(z) || is.null(z$ri)) return(paste0(g, " not estimated"))
        dz <- normalize_reference_design(z$reference_design %||% main_design)
        paste0(g, " ", reference_result_text(z$ri, dz, z$display_digits %||% digits))
      }, character(1))
      return(paste(bits, collapse="; "))
    }
    if (is.null(partition_result$group1) || is.null(partition_result$group2) || is.null(partition_result$groups)) return(NULL)
    lab1 <- friendly_group_label(partition_result$groups[1], "sex")
    lab2 <- friendly_group_label(partition_result$groups[2], "sex")
    d1 <- normalize_reference_design(partition_result$group1$reference_design %||% main_design)
    d2 <- normalize_reference_design(partition_result$group2$reference_design %||% main_design)
    ri1 <- reference_result_text(partition_result$group1$ri, d1, partition_result$group1$display_digits %||% digits)
    ri2 <- reference_result_text(partition_result$group2$ri, d2, partition_result$group2$display_digits %||% digits)
    paste0(lab1, " ", ri1, "; ", lab2, " ", ri2)
  }

  if (identical(main_result$type, "direct_establishment") && !is.null(main_result$ri)) {
    dmain <- normalize_reference_design(main_result$reference_design %||% NULL)
    ri_txt <- reference_result_text(main_result$ri, dmain, digits)
    result_term <- if (identical(dmain$tail, "two_sided")) paste0("an RI of ", ri_txt) else paste0("the ", reference_limit_name(dmain), " ", ri_txt)
    if (identical(main_result$status, "green")) {
      action <- paste0("You may propose for specialist approval ", result_term,
                       ". Before implementation, confirm that the partitioning assessments do not indicate a need for specific results.")
    } else if (identical(main_result$status, "yellow")) {
      action <- paste0("Do not implement ", result_term, " provisional. ", main_result$recommendation)
    } else if (identical(main_result$status, "red")) {
      action <- paste0("Do not implement ", result_term, " estimated. ", main_result$recommendation)
    }
  }

  if (!is.null(partition_result)) {
    notes <- c(notes, paste("Qualitative variable:", partition_result$recommendation %||% "No conclusion available."))
    if (identical(pdec, "indeterminate") && is.null(pres)) st <- if (identical(st, "red")) "red" else "yellow"
    if (identical(pdec, "partition") && identical(partition_result$status %||% NULL, "yellow") && !identical(st, "red")) st <- "yellow"
  }
  if (!is.null(age_result)) {
    notes <- c(notes, paste("Quantitative variable:", age_result$recommendation %||% "No conclusion available."))
    if (identical(adec, "indeterminate") || identical(age_result$status, "yellow")) {
      if (!identical(st, "red")) st <- "yellow"
    }
  }

  # Explicitly resolve partitioning discordance. This decision does not
  # alter the calculations; it documents which RI is adopted after reviewing the
  # evidence and allows a traceable final report to be issued.
  if (!is.null(pres) && pres %in% c("partition", "common")) {
    pending_outliers <- isTRUE(main_result$outlier_review_required)
    age_block <- identical(adec, "indeterminate") || identical(adec, "partition") || identical(adec, "continuous")
    if (identical(pres, "partition")) {
      group_txt <- group_ri_text()
      group_ok <- isTRUE(partition_result$subgroup_precision_ok) && isTRUE(partition_result$subgroup_standard)
      if (!isTRUE(group_ok)) {
        st <- "yellow"
        action <- "The specialist decision favors partitioning, but one or more specific RIs still do not meet the sample-size/precision requirements. Do not close the study until this is resolved."
      } else if (pending_outliers || age_block) {
        st <- "yellow"
        action <- paste0("The decision to partition has been documented (", group_txt,
                         "), but other methodological blockers remain before a final report can be issued.")
      } else {
        st <- if (identical(favored, "partition") || identical(pdec, "partition")) "green" else "yellow"
        final_decision <- paste0("Partition for qualitative variable approved: ", group_txt, ".")
        action <- paste0(final_decision, " Specialist decision documented. ",
                         if (identical(st, "green")) "It agrees with the alternative favored by RIveR." else "It does not fully agree with the alternative favored by RIveR; retain the justification in the report.")
        text <- final_decision
        is_final <- TRUE
      }
    } else if (identical(pres, "common")) {
      if (!isTRUE(main_result$precision_ok)) {
        st <- "yellow"
        action <- "The specialist decision is to retain a common RI, but overall RI precision does not meet the established criterion. Do not close the study until the precision issue is resolved."
      } else if (pending_outliers || age_block) {
        st <- "yellow"
        action <- "The decision to retain a common RI has been documented, but other methodological blockers remain before a final report can be issued."
      } else {
        ri_txt <- format_lab_interval(main_result$ri, digits)
        st <- if (identical(favored, "common") || identical(pdec, "common")) "green" else "yellow"
        final_decision <- paste0("Common RI approved: ", ri_txt, ".")
        action <- paste0(final_decision, " Specialist decision documented. ",
                         if (identical(st, "green")) "It agrees with the alternative favored by RIveR." else "Partitioning evidence was discordant; retain the specialist justification in the final report.")
        text <- final_decision
        is_final <- TRUE
      }
    }
  }

  # Specialist closure specific to direct establishment with n<120.
  # The program selects the model and assesses precision, but final adoption
  # is documented explicitly because this is a small-sample pathway.
  if (identical(main_result$type, "direct_establishment") && isTRUE(main_result$n < 120) && is.null(pres) && !is_final) {
    no_model_choices <- c("investigate","review_population","increase_no_model","no_ri","other")
    if (!is.null(sres) && sres %in% no_model_choices && grepl("exploratory", tolower(main_result$method %||% ""))) {
      action_label <- switch(sres,
        investigate = "Investigate possible subpopulations and repeat the analysis.",
        review_population = "Review the selection/inclusion criteria for the reference population and repeat the analysis.",
        increase_no_model = "Increase the reference population and reassess after confirming that it represents a single population.",
        no_ri = "Close the study without establishing a reference interval.",
        other = "Follow the alternative action documented by the specialist.",
        "Review the study before continuing.")
      st <- "green"
      final_decision <- "RI NOT ESTABLISHED."
      text <- paste0("No reference interval is established by this study. None of the assessed parametric/robust models has sufficiently defensible assumptions, and Box-Cox transformation does not resolve the distributional structure.")
      action <- paste0(text, " Agreed specialist action: ", action_label)
      is_final <- TRUE
    }

    small_model_ok <- !grepl("exploratory", tolower(main_result$method %||% "")) &&
      isTRUE(main_result$precision_ok)
    pending_outliers <- isTRUE(main_result$outlier_review_required)
    partition_block <- identical(pdec, "partition") || identical(pdec, "indeterminate")
    age_block <- identical(adec, "partition") || identical(adec, "continuous") || identical(adec, "indeterminate")
    ri_txt <- format_lab_interval(main_result$ri, digits)

    if (!is_final && identical(sres, "adopt")) {
      if (!small_model_ok) {
        st <- "red"
        action <- "The intention to adopt the RI has been recorded, but the model or precision does not meet the defined requirements. A final adoption report cannot be issued."
      } else if (pending_outliers || partition_block || age_block) {
        st <- "yellow"
        action <- "The decision to adopt the RI with n<120 is documented, but other methodological blockers remain before a final report can be issued."
      } else {
        st <- "green"
        final_decision <- paste0("RI approved with n<120: ", ri_txt, " using ", main_result$method, ".")
        text <- final_decision
        action <- paste0(final_decision, " Method selection, precision of both limits, and specialist justification are documented.")
        is_final <- TRUE
      }
    } else if (!is_final && identical(sres, "increase")) {
      st <- "yellow"
      text <- paste0("Provisional RI not adopted: ", ri_txt, ".")
      action <- "The specialist has decided not to adopt the RI yet and to increase the sample size. Keep the study open and repeat the analysis when additional reference individuals are available."
    } else if (!is_final && small_model_ok && !pending_outliers && !partition_block && !age_block) {
      st <- "yellow"
      action <- paste0("RIveR conditionally proposes the RI ", ri_txt, " using ", main_result$method,
                       ". With n<120, model assumptions and precision are adequate. Record in the closure panel whether to adopt the RI, increase the sample size, or leave the decision pending; RIveR will then generate the corresponding report.")
    }
  }

  # An automatically resolved partition modifies the action; it is not treated as an error.
  if (is.null(pres) && !is_final && !identical(st, "red")) {
    if (identical(pdec, "partition") && adec %in% c("partition", "continuous")) {
      st <- "yellow"
      action <- paste(
        paste0("Do not implement an overall RI. Effects have been identified for both ", qualitative_label, " and ", quantitative_label, "."),
        "Before implementation, RIveR must assess a joint covariate strategy; the effects should not be resolved independently or combined into simple crossed RIs without a validated joint model."
      )
    } else if (identical(pdec, "partition")) {
      group_txt <- group_ri_text()
      if (!is.null(group_txt)) {
        if (identical(partition_result$status %||% NULL, "green")) {
          action <- paste0("Do not implement the overall RI. RIveR recommends partitioning by ", qualitative_label, ": ", group_txt,
                           ". All the specific RIs have n≥120 and precision adequate. These can be proposed for specialist approval.")
        } else {
          action <- paste0("Do not implement the overall RI. The evidence supports partitioning for ", qualitative_label, ": ", group_txt,
                           ". Review the section of partition before the specialist approval.")
        }
      }
    } else if (identical(adec, "partition")) {
      action <- paste0("Do not implement a single RI for all the values of ", quantitative_label, ". Use the partitions proposed by RIveR after specialist approval.")
    } else if (identical(adec, "continuous")) {
      sa <- age_result$sil_adaptation %||% NULL
      dref <- normalize_reference_design(age_result$reference_design %||% main_result$reference_design %||% NULL)
      continuous_term <- if (identical(dref$tail, "two_sided")) "continuous RI" else paste0("continuous ", reference_limit_name(dref), " (", reference_design_percentile_text(dref), ")")
      if (!is.null(sa) && isTRUE(sa$ok) && isTRUE(sa$acceptable)) {
        action <- paste0(
          "Prioritize the ", continuous_term, " by ", quantitative_label, ". If the LIS supports a parameterized table, use the RIveR export. ",
          "If the LIS only allows bands, the minimum operational proposal consists of ", sa$n_groups,
          " bands, with a maximum discretization error of approximately ", round(100 * sa$overall_error, 1),
          " % of the typical computational width, with coverage of all bands compatible with the ", round(100 * (sa$expected_coverage %||% dref$coverage), 1), " % expected according to the binomial test. ",
          "Review and approve this adaptation before configuring the LIS."
        )
      } else {
        action <- paste0("Prioritize the ", continuous_term, " dependent on ", quantitative_label, ". If the LIS supports neither a formula nor a parameterized table, review the proposed discrete adaptation; do not define arbitrary bands.")
      }
    } else if (identical(pdec, "indeterminate")) {
      guide <- partition_result$discordance_guidance %||% NULL
      proposal <- switch(guide$favored %||% "review",
                         partition = "RIveR conditionally favors partitioning.",
                         common = "RIveR conditionally favors a common RI.",
                         "RIveR does not automatically prioritize either alternative.")
      action <- paste(
        "The partitioning assessment is inconclusive because the criteria are discordant.", proposal,
        "Review the impact of retaining the common RI versus using specific RIs and record a decision in the 'Resolve partitioning discordance' panel.",
        "After the decision, RIveR will generate the corresponding final report."
      )
      st <- "yellow"
    } else if (identical(adec, "indeterminate")) {
      action <- paste0("Do not implement the RI yet. Complete the assessment of ", quantitative_label, " before closing the study.")
      st <- "yellow"
    }
  }

  if (is.null(pres) && !is_final && identical(st, "green") && identical(main_result$type, "direct_establishment") &&
      !is.null(main_result$ri) && (is.null(pdec) || identical(pdec, "common")) &&
      (is.null(adec) || identical(adec, "common") || identical(age_result$status %||% NULL, "grey"))) {
    action <- paste0("You may propose a single RI for specialist approval: ",
                     format_lab_interval(main_result$ri, digits),
                     ". No demonstrated need for partitioning has been identified for the assessed covariates.")
  }

  # Blockers are presented in the order in which they should be resolved. A pending extreme value
  # precedes assessment of precision/partitioning because an exclusion requires
  # recalculation of the entire analysis. Tukey suspected outliers at 1.5–3 IQR are not blockers by themselves.
  if (identical(main_result$type, "direct_establishment") && !is_final) {
    pending_outliers <- isTRUE(main_result$outlier_review_required)
    precision_bad <- !isTRUE(main_result$precision_ok)
    p_indeterminate <- identical(pdec, "indeterminate") && is.null(pres)
    a_indeterminate <- identical(adec, "indeterminate")

    if (pending_outliers) {
      if (!identical(st, "red")) st <- "yellow"
      action <- paste(
        "1. Review the flagged extreme values and document whether they should be retained or excluded.",
        "2. If any value is excluded, RIveR will recalculate the RI, CIs, precision, and partitions from scratch.",
        "3. Interpret the remaining results as provisional until this recalculation is complete."
      )
    } else if (!p_indeterminate) {
      issues <- character(0)
      no_model <- isTRUE(main_result$n < 120) && grepl("exploratory", tolower(main_result$method %||% ""))
      if (no_model) {
        issues <- c(issues,
          "No parametric/robust model is sufficiently defensible, and Box-Cox transformation does not resolve the distributional structure. First investigate heterogeneity, subpopulations, and subject selection.",
          "The non-parametric RI is shown only for exploratory purposes and should not be interpreted as a primary or adoptable RI.")
        if (precision_bad) issues <- c(issues, "In addition, at least one limit of the exploratory RI has insufficient precision.")
      }
      # If partitioning has been selected, the relevant precision is that of the group-specific RIs.
      if (precision_bad && !identical(pres, "partition") && !no_model) {
        issues <- c(issues, "The precision of at least one overall RI limit does not meet the <20% criterion; increase the sample size or complete the planned strategy before closing a common RI.")
      }
      if (a_indeterminate) issues <- c(issues, paste0("The assessment of ", quantitative_label, " is inconclusive: ", age_result$recommendation %||% "complete the assessment."))
      if (length(issues)) {
        if (!identical(st, "red")) st <- "yellow"
        action <- paste(paste0(seq_along(issues), ". ", issues), collapse = " ")
      }
    }
  }

  pending_boundary_outliers <- isTRUE(age_result$boundary_outlier_pending)
  pending_subgroup_outliers <- isTRUE(partition_result$subgroup_outlier_pending) || isTRUE(age_result$subgroup_outlier_pending)
  if (!is_final && pending_boundary_outliers) {
    if (!identical(st, "red")) st <- "yellow"
    action <- paste(
      paste0("Review first the location of the cut-point of ", quantitative_label, "."),
      "There is an extreme observation near the boundary that is compatible with the neighboring group distribution; RIveR protects it from exclusion at this step.",
      "Only after confirming or readjusting the cut-point should segment-level aberrant values be reviewed and Lahti, Harris-Boyd, and precision be reassessed."
    )
  } else if (!is_final && pending_subgroup_outliers) {
    if (!identical(st, "red")) st <- "yellow"
    action <- paste(
      "First review the extreme values detected within the subgroups of the candidate partition.",
      "The partitioning conclusion and specific RIs remain provisional until this review is documented.",
      paste0("If an observation is excluded for a demonstrated reason, RIveR will recalculate the group RI, Lahti, Harris-Boyd, precision, and the assessment of ", quantitative_label, ".")
    )
  }

  headline <- if (is_final) {
    if (identical(final_decision, "RI NOT ESTABLISHED.")) "Final report — RI not established" else if (identical(st, "green")) "Final specialist decision documented" else "Final report — specialist decision with documented discordance"
  } else switch(st,
                green = "Operational recommendation",
                yellow = "Provisional recommendation — one step remains before implementation",
                red = "Do not implement the RI with the current evidence",
                "Result not evaluable")

  list(status = st, headline = headline, text = text, action = action, notes = notes,
       is_final = is_final, final_decision = final_decision,
       partition_resolution = partition_resolution, small_sample_resolution = small_sample_resolution)
}
