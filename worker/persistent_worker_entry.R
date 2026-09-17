# RIveR v1.0.1 · persistent worker entry point -------------------------------
# Executed via Rscript/processx. It does not depend on the temporary files
# that callr uses to transfer functions/arguments to subprocesses.

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 6L) stop("RIveR worker: incomplete input arguments.")

job_file      <- args[[1]]
app_dir       <- args[[2]]
status_file   <- args[[3]]
result_file   <- args[[4]]
error_file    <- args[[5]]
manifest_file <- args[[6]]

# Explicitly limit the number of threads within the worker, including when
# external environments override variables during Rscript startup.
Sys.setenv(
  OMP_NUM_THREADS = "1", OPENBLAS_NUM_THREADS = "1", MKL_NUM_THREADS = "1",
  VECLIB_MAXIMUM_THREADS = "1", NUMEXPR_NUM_THREADS = "1"
)

atomic_error <- function(message, stage = "worker startup", call = NULL) {
  dir.create(dirname(error_file), recursive = TRUE, showWarnings = FALSE)
  tmp <- paste0(error_file, ".startup_tmp_", Sys.getpid())
  obj <- list(
    message = as.character(message)[1], stage = stage,
    call = if (is.null(call)) NULL else paste(deparse(call), collapse = " "),
    pid = Sys.getpid(), time = Sys.time()
  )
  try(saveRDS(obj, tmp), silent = TRUE)
  if (file.exists(tmp)) {
    if (file.exists(error_file)) unlink(error_file, force = TRUE)
    ok <- file.rename(tmp, error_file)
    if (!isTRUE(ok)) {
      file.copy(tmp, error_file, overwrite = TRUE)
      unlink(tmp, force = TRUE)
    }
  }
  invisible(FALSE)
}

tryCatch({
  app_dir <- normalizePath(app_dir, winslash = "/", mustWork = TRUE)
  setwd(app_dir)
  sys.source(file.path(app_dir, "R", "async_worker.R"), envir = .GlobalEnv)
  
  if (!exists("rilctms_async_worker", mode = "function", inherits = TRUE)) {
    stop("Could not load rilctms_async_worker().")
  }
  
  rilctms_async_worker(
    job_file,
    app_dir,
    status_file,
    result_file,
    error_file,
    manifest_file
  )
  
}, error = function(e) {
  
  # If async_worker has already written error.rds, preserve it because it
  # contains the precise methodological stage. Only startup/loading errors
  # are written here.
  if (!file.exists(error_file)) {
    atomic_error(
      conditionMessage(e),
      call = conditionCall(e)
    )
  }
  
  message("RIveR worker error: ", conditionMessage(e))
  quit(save = "no", status = 1L, runLast = FALSE)
})
