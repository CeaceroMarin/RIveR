# RIveR v0.17.2 · persistent job manager --------------------------------
# The Shiny session only creates/queries jobs. The statistical process is external,
# unsupervised, and is not destroyed when the web session closes or is lost.

rilctms_job_atomic_save_rds <- function(object, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tmp <- paste0(path, ".tmp_", Sys.getpid(), "_", sample.int(999999L, 1L))
  saveRDS(object, tmp)
  if (file.exists(path)) unlink(path, force = TRUE)
  ok <- file.rename(tmp, path)
  if (!isTRUE(ok)) {
    file.copy(tmp, path, overwrite = TRUE)
    unlink(tmp, force = TRUE)
  }
  invisible(TRUE)
}

rilctms_jobs_root <- function() {
  base <- tryCatch(tools::R_user_dir("RIveR", which = "data"), error = function(e) NA_character_)
  if (!is.character(base) || !length(base) || is.na(base[1]) || !nzchar(base[1])) {
    base <- file.path(path.expand("~"), ".ribell")
  }
  root <- file.path(base[1], "jobs")
  ok <- dir.exists(root) || isTRUE(dir.create(root, recursive = TRUE, showWarnings = FALSE))
  if (!ok) {
    root <- file.path(path.expand("~"), "RIveR_jobs")
    ok <- dir.exists(root) || isTRUE(dir.create(root, recursive = TRUE, showWarnings = FALSE))
  }
  if (!ok) stop("The persistent RIveR job directory could not be created.")
  normalizePath(root, winslash = "/", mustWork = TRUE)
}

rilctms_job_paths <- function(job_dir) {
  list(
    job_dir = job_dir,
    job = file.path(job_dir, "job.rds"),
    manifest = file.path(job_dir, "manifest.rds"),
    snapshot = file.path(job_dir, "snapshot.rds"),
    status = file.path(job_dir, "status.rds"),
    result = file.path(job_dir, "result.rds"),
    error = file.path(job_dir, "error.rds"),
    stdout = file.path(job_dir, "worker_stdout.log"),
    stderr = file.path(job_dir, "worker_stderr.log"),
    consumed = file.path(job_dir, "consumed.rds"),
    dismissed = file.path(job_dir, "dismissed.rds")
  )
}

rilctms_safe_read_rds <- function(path, default = NULL) {
  if (!file.exists(path)) return(default)
  tryCatch(readRDS(path), error = function(e) default)
}

rilctms_job_manifest <- function(job_dir) {
  p <- rilctms_job_paths(job_dir)
  rilctms_safe_read_rds(p$manifest, list())
}

rilctms_update_job_manifest <- function(job_dir, ...) {
  p <- rilctms_job_paths(job_dir)
  man <- rilctms_safe_read_rds(p$manifest, list())
  updates <- list(...)
  for (nm in names(updates)) man[[nm]] <- updates[[nm]]
  man$manifest_updated_at <- Sys.time()
  rilctms_job_atomic_save_rds(man, p$manifest)
  invisible(man)
}

rilctms_job_process_handle <- function(manifest) {
  if (!requireNamespace("ps", quietly = TRUE)) return(NULL)
  pid <- suppressWarnings(as.integer(manifest$pid %||% NA_integer_))
  if (!is.finite(pid) || pid <= 0L) return(NULL)
  ct <- manifest$process_create_time %||% NULL
  tryCatch({
    if (!is.null(ct) && length(ct) && !all(is.na(ct))) return(ps::ps_handle(pid, time = ct))
    # Fallback only for incomplete manifests: check that the current process
    # time is compatible with launch time before accepting the PID.
    h <- ps::ps_handle(pid)
    actual <- ps::ps_create_time(h)
    expected <- manifest$worker_started_at %||% manifest$launched_at %||% manifest$created_at %||% NULL
    if (!is.null(expected) && length(expected) && !all(is.na(expected))) {
      dt <- abs(as.numeric(difftime(actual, expected, units = "secs")))
      if (is.finite(dt) && dt > 120) return(NULL)
    }
    h
  }, error = function(e) NULL)
}

rilctms_job_process_alive <- function(manifest) {
  if (!requireNamespace("ps", quietly = TRUE)) return(NA)
  pid <- suppressWarnings(as.integer(manifest$pid %||% NA_integer_))
  if (!is.finite(pid) || pid <= 0L) return(NA)
  h <- rilctms_job_process_handle(manifest)
  if (is.null(h)) return(FALSE)
  tryCatch(isTRUE(ps::ps_is_running(h)), error = function(e) FALSE)
}

rilctms_job_state <- function(job_dir) {
  p <- rilctms_job_paths(job_dir)
  man <- rilctms_safe_read_rds(p$manifest, list())
  st <- rilctms_safe_read_rds(p$status, list(state = "unknown", progress = 0, detail = "Status unavailable"))
  state <- as.character(st$state %||% man$state %||% "unknown")
  alive <- rilctms_job_process_alive(man)

  # A result/error written by the worker takes precedence over a partial status.rds.
  if (file.exists(p$result)) state <- "complete"
  if (file.exists(p$error) && !file.exists(p$result)) state <- "error"

  # If the worker no longer exists and has left neither a result nor an error, the job
  # is marked as interrupted; it is not shown indefinitely as "running".
  if (identical(state, "running") && identical(alive, FALSE) && !file.exists(p$result) && !file.exists(p$error)) {
    state <- "interrupted"
  }

  consumed_info <- rilctms_safe_read_rds(p$consumed, NULL)
  list(job_dir = job_dir, paths = p, manifest = man, status = st, state = state, alive = alive,
       consumed = !is.null(consumed_info), consumed_info = consumed_info, dismissed = file.exists(p$dismissed))
}

rilctms_list_jobs <- function(include_consumed = FALSE, include_dismissed = FALSE) {
  root <- rilctms_jobs_root()
  dirs <- list.dirs(root, full.names = TRUE, recursive = FALSE)
  if (!length(dirs)) return(list())
  out <- lapply(dirs, rilctms_job_state)
  out <- Filter(function(z) {
    (isTRUE(include_consumed) || !isTRUE(z$consumed)) &&
      (isTRUE(include_dismissed) || !isTRUE(z$dismissed))
  }, out)
  ord <- order(vapply(out, function(z) {
    tt <- z$manifest$created_at %||% z$status$started_at %||% as.POSIXct("1970-01-01", tz = "UTC")
    as.numeric(tt)[1]
  }, numeric(1)), decreasing = TRUE)
  out[ord]
}

rilctms_latest_recoverable_job <- function() {
  # v0.17.1: completed results are not automatically consumed.
  # Compatibility with v0.17.0: that version marked the result as consumed
  # as soon as it was applied to the active session. Allow ONE MORE recovery
  # of these results when the consumption reason is exactly the former auto-application marker.
  jobs <- rilctms_list_jobs(include_consumed = TRUE)
  if (!length(jobs)) return(NULL)
  legacy_auto_consumed <- function(z) {
    if (!isTRUE(z$consumed) || !identical(z$state, "complete")) return(FALSE)
    if (!identical(as.character(z$manifest$app_version %||% ""), "0.17.0")) return(FALSE)
    note <- if (is.list(z$consumed_info)) as.character(z$consumed_info$note %||% "") else ""
    note %in% c("Result applied", "Recovered result applied")
  }
  jobs <- Filter(function(z) !isTRUE(z$dismissed) && (!isTRUE(z$consumed) || legacy_auto_consumed(z)), jobs)
  if (!length(jobs)) return(NULL)
  # Priority: running job first; then the most recent recoverable terminal job.
  idx <- which(vapply(jobs, function(z) identical(z$state, "running"), logical(1)))
  if (length(idx)) return(jobs[[idx[1]]])
  jobs[[1]]
}

rilctms_any_running_job <- function(except_job_id = NULL) {
  jobs <- rilctms_list_jobs(include_consumed = TRUE, include_dismissed = TRUE)
  for (z in jobs) {
    jid <- as.character(z$manifest$job_id %||% z$status$job_id %||% "")
    if (!is.null(except_job_id) && identical(jid, as.character(except_job_id))) next
    if (identical(z$state, "running") && !identical(z$alive, FALSE)) return(z)
  }
  NULL
}

rilctms_mark_job_consumed <- function(job_dir, note = "Result applied to the session") {
  p <- rilctms_job_paths(job_dir)
  rilctms_job_atomic_save_rds(list(time = Sys.time(), note = note), p$consumed)
  invisible(TRUE)
}

rilctms_dismiss_job <- function(job_dir) {
  p <- rilctms_job_paths(job_dir)
  rilctms_job_atomic_save_rds(list(time = Sys.time()), p$dismissed)
  invisible(TRUE)
}

rilctms_cancel_job <- function(job_dir) {
  z <- rilctms_job_state(job_dir)
  if (!identical(z$state, "running")) return(invisible(TRUE))
  man <- z$manifest
  ok <- FALSE
  h <- rilctms_job_process_handle(man)
  if (!is.null(h)) {
    ok <- tryCatch({ ps::ps_kill(h); TRUE }, error = function(e) FALSE)
  } else if (!requireNamespace("ps", quietly = TRUE)) {
    pid <- suppressWarnings(as.integer(man$pid %||% NA_integer_))
    if (is.finite(pid) && pid > 0L) ok <- tryCatch(isTRUE(tools::pskill(pid, tools::SIGTERM)), error = function(e) FALSE)
  }
  if (!isTRUE(ok)) return(invisible(FALSE))
  st <- z$status %||% list()
  st$state <- "cancelled"
  st$progress <- suppressWarnings(as.numeric(st$progress %||% 0))
  st$detail <- "Analysis cancelled by the user"
  st$updated_at <- Sys.time()
  rilctms_job_atomic_save_rds(st, z$paths$status)
  rilctms_update_job_manifest(job_dir, state = "cancelled", cancelled_at = Sys.time())
  invisible(TRUE)
}

rilctms_prune_old_jobs <- function(days = 30) {
  jobs <- rilctms_list_jobs(include_consumed = TRUE, include_dismissed = TRUE)
  cutoff <- Sys.time() - as.difftime(days, units = "days")
  for (z in jobs) {
    tt <- z$manifest$created_at %||% z$status$started_at %||% Sys.time()
    if (isTRUE(z$consumed || z$dismissed) && is.finite(as.numeric(tt)) && tt < cutoff && !identical(z$state, "running")) {
      unlink(z$job_dir, recursive = TRUE, force = TRUE)
    }
  }
  invisible(TRUE)
}
