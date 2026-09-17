# RIveR v0.17.7 · cross-cutting persistent engine -------------------------
# All statistical analyses and recalculations run in a separate R process
# (Rscript/processx) to avoid blocking the Shiny session. Module methodology is
# unchanged: this file only coordinates execution, progress, ETA, and return values.

rilctms_atomic_save_rds <- function(object, path) {
  tmp <- paste0(path, ".tmp")
  saveRDS(object, tmp)
  if (file.exists(path)) unlink(path)
  ok <- file.rename(tmp, path)
  if (!isTRUE(ok)) {
    file.copy(tmp, path, overwrite = TRUE)
    unlink(tmp)
  }
  invisible(TRUE)
}

rilctms_async_worker <- function(job_file, app_dir, status_file, result_file, error_file, manifest_file = NULL) {
  old <- getwd(); on.exit(setwd(old), add = TRUE)
  setwd(app_dir)
  source("R/helpers.R", local = TRUE)
  source("R/job_manager.R", local = TRUE)
  source("R/direct.R", local = TRUE)
  source("R/indirect.R", local = TRUE)
  source("R/d7.R", local = TRUE)
  source("R/partition.R", local = TRUE)
  source("R/age.R", local = TRUE)
  source("R/decision.R", local = TRUE)

  spec <- readRDS(job_file)
  job_id <- as.character(spec$job_id %||% "")
  source_label <- as.character(spec$source_label %||% "Analysis")
  started <- Sys.time()
  last_progress <- 0
  current_stage <- "Worker preparation"

  # The worker owns its process identity. It records it in the
  # persistent manifest so a new Shiny session can verify that it is still
  # alive without retaining any processx object from the session that started it.
  if (!is.null(manifest_file)) {
    job_dir <- dirname(manifest_file)
    pct <- tryCatch({
      if (requireNamespace("ps", quietly = TRUE)) ps::ps_create_time(ps::ps_handle(Sys.getpid())) else NULL
    }, error = function(e) NULL)
    try(rilctms_update_job_manifest(job_dir, state = "running", pid = Sys.getpid(),
                                    process_create_time = pct, worker_started_at = started), silent = TRUE)
  }

  # Temporary ETA state based on actual work units. D7E explicitly reports
  # the start and end of each window; after two completed windows,
  # ETA uses the speed observed on this computer.
  age_window_started <- list()
  age_window_durations <- numeric(0)
  age_window_total <- NA_integer_
  age_window_completed <- 0L

  estimate_eta <- function(value, detail, now = Sys.time()) {
    elapsed <- as.numeric(difftime(now, started, units = "secs"))
    if (!is.finite(elapsed) || elapsed < 8 || value >= 0.995) {
      return(list(low=NA_real_, high=NA_real_, method="preparing"))
    }

    in_age_window <- grepl("D7 · age model · window", detail, fixed=TRUE) || grepl("D7 · quantitative model · window", detail, fixed=TRUE)
    if (in_age_window && length(age_window_durations) >= 2L && is.finite(age_window_total) && age_window_total > 0L) {
      recent <- tail(age_window_durations[is.finite(age_window_durations) & age_window_durations > 0], 4L)
      if (length(recent) >= 2L) {
        unit_time <- stats::median(recent)
        remaining_windows <- max(0L, age_window_total - age_window_completed)
        central <- unit_time * remaining_windows
        # Small margin for integration/serialization after the last window.
        overhead <- if (remaining_windows > 0L) max(10, 0.04 * elapsed) else max(5, 0.02 * elapsed)
        return(list(low=max(0, 0.80*central), high=max(0, 1.30*central + overhead), method="completed_windows"))
      }
    }
    if (in_age_window && length(age_window_durations) < 2L) {
      return(list(low=NA_real_, high=NA_real_, method="learning_windows"))
    }

    # General cross-cutting estimate based on current progress reported by the
    # module. It is enabled only once there is enough progress to avoid inventing an ETA.
    if (value >= 0.15 && value < 0.96 && elapsed >= 15) {
      central <- elapsed * (1-value) / max(value, 0.05)
      return(list(low=max(0, 0.75*central), high=max(0, 1.35*central), method="progress_rate"))
    }
    list(low=NA_real_, high=NA_real_, method="preparing")
  }

  write_status <- function(value, detail, phase = NULL) {
    value <- suppressWarnings(as.numeric(value))
    value <- if (length(value)) value[1] else NA_real_
    if (!isTRUE(is.finite(value))) value <- last_progress
    value <- max(0, min(1, value))
    # Internal callbacks may reuse local scales. Do not allow an
    # internal phase to move the overall progress bar backward.
    value <- max(last_progress, value)
    last_progress <<- value
    detail <- as.character(detail %||% "")
    now <- Sys.time()
    eta <- estimate_eta(value, detail, now)
    rilctms_atomic_save_rds(list(
      state = "running", progress = value, detail = detail,
      phase = as.character(phase %||% detail %||% source_label), source_label = source_label,
      eta_low_seconds = eta$low, eta_high_seconds = eta$high, eta_method = eta$method,
      job_id = job_id, started_at = started, updated_at = now
    ), status_file)
  }

  progress_cb <- function(value, detail) {
    d <- as.character(detail %||% "")
    p <- suppressWarnings(as.numeric(value))[1]

    if (identical(spec$study_type, "establish") && identical(spec$data_origin, "indirect")) {
      has_age <- isTRUE(spec$check_age)
      has_part <- isTRUE(spec$check_partition)

      # Record current durations for D7E windows.
      if (grepl("D7 · age model · window", d, fixed=TRUE) || grepl("D7 · quantitative model · window", d, fixed=TRUE)) {
        mm <- regmatches(d, regexec("window ([0-9]+)/([0-9]+)", d))[[1]]
        if (length(mm) >= 3L) {
          i <- as.integer(mm[2]); n <- as.integer(mm[3])
          age_window_total <<- n
          completed <- grepl("completed", d, fixed=TRUE)
          key <- as.character(i)
          if (!completed) {
            age_window_started[[key]] <<- Sys.time()
          } else {
            t0 <- age_window_started[[key]] %||% NULL
            if (!is.null(t0)) {
              dt <- as.numeric(difftime(Sys.time(), t0, units="secs"))
              if (is.finite(dt) && dt > 0) age_window_durations <<- c(age_window_durations, dt)
            }
            age_window_completed <<- max(age_window_completed, i)
          }
        }
      }

      # Temporal reweighting of D7. Internal module percentages are
      # methodological; this layer converts them into monotonic overall progress.
      if (grepl("D7 · refineR", d, fixed=TRUE)) p <- 0.05
      else if (grepl("independent estimate with reflimR", d, fixed=TRUE)) {
        p <- if (has_age) 0.10 else if (has_part) 0.28 else 0.82
      } else if (grepl("D7 · partition · group", d, fixed=TRUE)) {
        mm <- regmatches(d, regexec("group ([0-9]+)/([0-9]+)", d))[[1]]
        if (length(mm) >= 3L) {
          i <- as.numeric(mm[2]); n <- as.numeric(mm[3])
          base <- if (has_age) 0.10 else 0.28
          span <- if (has_age) 0.16 else 0.58
          p <- base + span * (i-1)/max(1,n)
        }
      } else if (grepl("D7 · covariates", d, fixed=TRUE)) {
        p <- if (has_age) 0.26 else if (has_part) 0.86 else 0.84
      } else if (grepl("D7 · age model · window", d, fixed=TRUE) || grepl("D7 · quantitative model · window", d, fixed=TRUE)) {
        mm <- regmatches(d, regexec("window ([0-9]+)/([0-9]+)", d))[[1]]
        if (length(mm) >= 3L) {
          i <- as.numeric(mm[2]); n <- as.numeric(mm[3])
          base <- if (has_part) 0.28 else 0.12
          span <- if (has_part) 0.62 else 0.78
          completed <- grepl("completed", d, fixed=TRUE)
          done_fraction <- if (completed) i/max(1,n) else (i-1)/max(1,n)
          p <- base + span * done_fraction
        }
      } else if (grepl("methodological integration", d, fixed=TRUE)) p <- 0.94
    }
    write_status(p, d)
  }

  set_stage <- function(stage, value = NULL, detail = NULL) {
    current_stage <<- as.character(stage %||% "Unspecified stage")[1]
    if (!is.null(value)) write_status(value, detail %||% current_stage, phase = current_stage)
    invisible(current_stage)
  }

  set_stage("Calculation preparation", 0.02, paste0(source_label, " · preparing calculation"))

  ans <- tryCatch({
    dat <- spec$dat
    x <- dat$value
    design <- normalize_reference_design(spec$reference_design)
    main <- NULL; partition <- NULL; age <- NULL

    if (identical(spec$study_type, "establish") && identical(spec$data_origin, "direct")) {
      set_stage("D3 · direct establishment", 0.15, "Calculating the limit/interval using the direct method")
      main <- run_direct_establishment(
        x, reviewed_extreme_values = spec$retained_extreme_values %||% numeric(0),
        reference_design = design
      )
      pending <- isTRUE(main$outlier_review_required)
      if (!pending) {
        if (isTRUE(spec$check_partition) && "qualitative" %in% names(dat)) {
          write_status(0.55, paste0("Assessing the qualitative variable · ", spec$qualitative_label %||% ""))
          partition <- tryCatch(
            assess_qualitative_partition(
              dat, "qualitative", route = "direct", n_bootstrap = min(spec$n_bootstrap %||% 200L, 200L),
              reviewed_extreme_values = spec$retained_extreme_values %||% numeric(0),
              reference_design = design
            ), error=function(e) list(status="grey",decision="not_evaluable",recommendation=conditionMessage(e))
          )
        }
        if (isTRUE(spec$check_age) && "quantitative" %in% names(dat)) {
          write_status(0.72, paste0("Assessing the quantitative variable · ", spec$quantitative_label %||% ""))
          age <- tryCatch(
            assess_quantitative_pattern(
              dat, route="direct", n_bootstrap=spec$n_bootstrap %||% 200L, seed=1201,
              reviewed_extreme_values = spec$retained_extreme_values %||% numeric(0),
              reference_design = design, covariate_label=spec$quantitative_label %||% "Quantitative variable"
            ), error=function(e) list(status="grey",decision="not_evaluable",recommendation=conditionMessage(e))
          )
        }
      }
    } else if (identical(spec$study_type, "establish") && identical(spec$data_origin, "indirect")) {
      set_stage("D7 · indirect establishment")
      main <- run_indirect_establishment_d7(
        x, data=dat, n_bootstrap=spec$n_bootstrap, seed=2201,
        check_partition=isTRUE(spec$check_partition), check_age=isTRUE(spec$check_age),
        explore_complexity=isTRUE(spec$explore_complexity), progress=progress_cb,
        reference_design=design, qualitative_label=spec$qualitative_label %||% "Qualitative variable",
        quantitative_label=spec$quantitative_label %||% "Quantitative variable"
      )
    } else if (spec$study_type %in% c("verify","review") && identical(spec$data_origin, "direct")) {
      set_stage("D5 · direct verification", 0.20, "Applying direct verification")
      if (identical(spec$study_type, "verify") && !identical(spec$verification_mode, "none")) {
        main <- run_direct_verification_partitioned(
          dat, spec$verification_definitions,
          second_cohorts=spec$partition_second_data %||% list(),
          first_source=spec$raw_name %||% "—",
          second_sources=spec$partition_second_sources %||% list(),
          reference_design=design
        )
      } else {
        main <- run_direct_verification(
          x, spec$target_lower, spec$target_upper,
          verification_round=dat$verification_round %||% NULL,
          allow_embedded_second=isTRUE(spec$allow_embedded_second),
          reference_design=design
        )
      }
    } else {
      set_stage("D6 · indirect verification", 0.12, "Starting the indirect-verification workflow")
      if (identical(spec$study_type, "verify") && !identical(spec$verification_mode, "none")) {
        main <- run_indirect_verification_partitioned(
          dat, spec$verification_definitions,
          n_bootstrap=spec$n_bootstrap, seed=1201,
          force_refine=isTRUE(spec$force_refine), explore_complexity=isTRUE(spec$explore_complexity),
          progress=progress_cb, reference_design=design
        )
      } else {
        main <- run_indirect_verification(
          x, spec$target_lower, spec$target_upper,
          n_bootstrap=spec$n_bootstrap, seed=1201, progress=progress_cb,
          data=dat, force_refine=isTRUE(spec$force_refine),
          explore_complexity=isTRUE(spec$explore_complexity), reference_design=design
        )
      }
    }

    set_stage("Methodological integration")
    if (!is.null(partition)) partition$covariate_label <- spec$qualitative_label %||% "Qualitative variable"
    if (!is.null(age)) age$covariate_label <- spec$quantitative_label %||% "Quantitative variable"
    if (is.null(main)) stop("The analysis returned no result.")
    if (isTRUE(main$technical_error)) stop(main$technical_message %||% main$recommendation %||% "Technical error during analysis.")
    write_status(0.94, "Integrating the recommendation")
    final <- compose_final_recommendation(main, partition, age, NULL, NULL)
    list(job_id=job_id, main=main, partition=partition, age=age, final=final)
  }, error=function(e) e)

  if (inherits(ans, "error")) {
    msg <- conditionMessage(ans)
    call_txt <- tryCatch({
      cc <- conditionCall(ans)
      if (is.null(cc)) NULL else paste(deparse(cc), collapse = " ")
    }, error = function(e) NULL)
    rilctms_atomic_save_rds(list(message=msg, stage=current_stage, call=call_txt,
                                 job_id=job_id, pid=Sys.getpid(), time=Sys.time()), error_file)
    rilctms_atomic_save_rds(list(state="error", progress=1, detail=paste0(current_stage, " · ", msg),
                                 phase=current_stage,
                                 job_id=job_id, started_at=started, updated_at=Sys.time()), status_file)
    if (!is.null(manifest_file)) try(rilctms_update_job_manifest(dirname(manifest_file), state = "error", finished_at = Sys.time()), silent = TRUE)
    return(invisible(FALSE))
  }
  rilctms_atomic_save_rds(ans, result_file)
  rilctms_atomic_save_rds(list(state="complete", progress=1, detail="Analysis completed", phase=source_label,
                               eta_low_seconds=0, eta_high_seconds=0, eta_method="complete",
                               job_id=job_id, started_at=started, updated_at=Sys.time()), status_file)
  if (!is.null(manifest_file)) try(rilctms_update_job_manifest(dirname(manifest_file), state = "complete", finished_at = Sys.time()), silent = TRUE)
  invisible(TRUE)
}
