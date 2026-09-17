
# RIveR - dependency installation
# RIveR v1.0.0 · dependency installer
# refineR and its dependencies are NOT forced to compile from source.

cat("RIveR v1.0.0 · dependency check\n")
cat("R: ", R.version.string, "\n", sep = "")
cat("Platform: ", R.version$platform, "\n\n", sep = "")

core <- c(
  "shiny",
  "readxl",
  "referenceIntervals",
  "reflimR",
  "rpart",
  "gamlss",
  "gamlss.dist",
  "htmltools",
  "processx",
  "ps",
  "future",
  "promises"
)

install_pkgs <- function(pkgs) {
  pkgs <- unique(pkgs)
  if (!length(pkgs)) return(invisible(TRUE))
  if (.Platform$OS.type == "windows") {
    utils::install.packages(pkgs, dependencies = NA, type = "binary")
  } else {
    utils::install.packages(pkgs, dependencies = NA)
  }
  invisible(TRUE)
}

missing <- core[!vapply(core, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))]
if (length(missing)) {
  cat("Missing required packages:\n  ", paste(missing, collapse = ", "), "\n\n", sep = "")
  install_pkgs(missing)
}

# Shiny >= 1.8.1 is required for ExtendedTask (non-blocking recovery).
shiny_ok <- requireNamespace("shiny", quietly = TRUE) &&
  tryCatch(utils::packageVersion("shiny") >= package_version("1.8.1"), error = function(e) FALSE)

if (!shiny_ok) {
  old <- if (requireNamespace("shiny", quietly = TRUE)) {
    as.character(utils::packageVersion("shiny"))
  } else {
    "not installed"
  }
  
  cat("\nInstalled Shiny version: ", old, "\n", sep = "")
  cat("RIveR v1.0.0 requires Shiny >= 1.8.1. Installing/updating...\n")
  try(install_pkgs("shiny"), silent = TRUE)
}

# reflimR >= 1.1.0 is required for the validated D6 engine.
reflim_ok <- requireNamespace("reflimR", quietly = TRUE) &&
  tryCatch(utils::packageVersion("reflimR") >= package_version("1.1.0"), error = function(e) FALSE)

if (!reflim_ok) {
  old <- if (requireNamespace("reflimR", quietly = TRUE)) {
    as.character(utils::packageVersion("reflimR"))
  } else {
    "not installed"
  }
  
  cat("\nInstalled reflimR version: ", old, "\n", sep = "")
  cat("RIveR v1.0.0 requires reflimR >= 1.1.0. Installing/updating...\n")
  try(install_pkgs("reflimR"), silent = TRUE)
}

# refineR >= 2.0.0 is required for VeRUS/uncertainty margins.
refine_ok <- requireNamespace("refineR", quietly = TRUE) &&
  tryCatch(utils::packageVersion("refineR") >= package_version("2.0.0"), error = function(e) FALSE)

if (!refine_ok) {
  old <- if (requireNamespace("refineR", quietly = TRUE)) {
    as.character(utils::packageVersion("refineR"))
  } else {
    "not installed"
  }
  
  cat("\nInstalled refineR version: ", old, "\n", sep = "")
  cat("RIveR v1.0.0 requires refineR >= 2.0.0 (VeRUS and uncertainty margins).\n")
  
  if (.Platform$OS.type == "windows") {
    
    # Explicitly installing dependencies as binaries prevents parallelly
    # from being compiled from source (which requires Rtools on Windows).
    deps_refiner <- c("parallelly", "ash", "future", "future.apply")
    
    deps_missing <- deps_refiner[
      !vapply(
        deps_refiner,
        requireNamespace,
        quietly = TRUE,
        FUN.VALUE = logical(1)
      )
    ]
    
    if (length(deps_missing)) {
      cat(
        "Installing binary refineR dependencies: ",
        paste(deps_missing, collapse = ", "),
        "\n",
        sep = ""
      )
      try(install_pkgs(deps_missing), silent = TRUE)
    }
    
    cat("Installing/updating refineR from the CRAN binary...\n\n")
    try(
      utils::install.packages(
        "refineR",
        dependencies = NA,
        type = "binary"
      ),
      silent = TRUE
    )
    
  } else {
    
    cat("Installing/updating refineR from CRAN...\n\n")
    try(
      utils::install.packages(
        "refineR",
        dependencies = NA
      ),
      silent = TRUE
    )
  }
}

# ----------------------------------------------------------------------------
# OPTIONAL PACKAGES
# ----------------------------------------------------------------------------

# mclust: optional package for exploratory D6 analyses.
if (!requireNamespace("mclust", quietly = TRUE)) {
  cat("\nInstalling optional package 'mclust' for D6 exploration...\n")
  try(install_pkgs("mclust"), silent = TRUE)
}

# tidykosmic: optional package for KOSMIC indirect RI estimation.
# If unavailable, RIveR uses reflimR as the secondary indirect method.
if (!requireNamespace("tidykosmic", quietly = TRUE)) {
  cat("\nOptional package 'tidykosmic' is not installed.\n")
  cat("KOSMIC indirect RI estimation will not be available.\n")
  cat("RIveR will use reflimR as the secondary indirect method.\n")
}

# Final check of all required packages.
required <- c(core, "refineR")

still_missing <- required[
  !vapply(
    required,
    requireNamespace,
    quietly = TRUE,
    FUN.VALUE = logical(1)
  )
]

refine_version_ok <- requireNamespace("refineR", quietly = TRUE) &&
  tryCatch(
    utils::packageVersion("refineR") >= package_version("2.0.0"),
    error = function(e) FALSE
  )

reflim_version_ok <- requireNamespace("reflimR", quietly = TRUE) &&
  tryCatch(
    utils::packageVersion("reflimR") >= package_version("1.1.0"),
    error = function(e) FALSE
  )

shiny_version_ok <- requireNamespace("shiny", quietly = TRUE) &&
  tryCatch(
    utils::packageVersion("shiny") >= package_version("1.8.1"),
    error = function(e) FALSE
  )

if (
  length(still_missing) ||
  !refine_version_ok ||
  !reflim_version_ok ||
  !shiny_version_ok
) {
  
  refine_status <- if (requireNamespace("refineR", quietly = TRUE)) {
    paste0(
      "version ",
      as.character(utils::packageVersion("refineR"))
    )
  } else {
    "not installed"
  }
  
  extra <- if (.Platform$OS.type == "windows") {
    paste0(
      "\n\nOn Windows, RIveR attempts to use binary packages and does not ",
      "require Rtools in a standard installation. ",
      "If your R version is old and CRAN no longer provides compatible ",
      "binaries, update R to a current version and run ",
      "source(\"install_packages.R\") again. ",
      "Detected R version: ", R.version.string, "."
    )
  } else {
    ""
  }
  
  stop(
    "Not all required packages could be installed. ",
    if (length(still_missing)) {
      paste0(
        "Missing: ",
        paste(still_missing, collapse = ", "),
        ". "
      )
    } else {
      ""
    },
    "refineR: ", refine_status, ". ",
    "reflimR: ",
    if (requireNamespace("reflimR", quietly = TRUE)) {
      paste0(
        "version ",
        as.character(utils::packageVersion("reflimR"))
      )
    } else {
      "not installed"
    },
    ". ",
    "Shiny: ",
    if (requireNamespace("shiny", quietly = TRUE)) {
      paste0(
        "version ",
        as.character(utils::packageVersion("shiny"))
      )
    } else {
      "not installed"
    },
    ".",
    extra,
    "\nCopy this message and send it for troubleshooting."
  )
}


cat("\nInstallation/dependency check completed successfully.\n")
cat(
  "reflimR: ",
  as.character(utils::packageVersion("reflimR")),
  "\n",
  sep = ""
)
cat(
  "refineR: ",
  as.character(utils::packageVersion("refineR")),
  "\n",
  sep = ""
)

if (requireNamespace("mclust", quietly = TRUE)) {
  cat(
    "mclust: ",
    as.character(utils::packageVersion("mclust")),
    "\n",
    sep = ""
  )
}

if (requireNamespace("tidykosmic", quietly = TRUE)) {
  cat(
    "tidykosmic: ",
    as.character(utils::packageVersion("tidykosmic")),
    " (optional)\n",
    sep = ""
  )
} else {
  cat("tidykosmic: not installed (optional; reflimR fallback available)\n")
}

cat("Run: source(\"run_app_RIveR.R\")\n")

