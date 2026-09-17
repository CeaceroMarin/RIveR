# ============================================================================
# RIveR v1.0.0 - Application launcher
# ============================================================================

# ----------------------------------------------------------------------------
# 1. SET THE RIveR APPLICATION FOLDER
# ----------------------------------------------------------------------------
# IMPORTANT:
# Replace the path below with the folder where you extracted RIveR.
# This folder must contain "app.R".
#
# Example:
# app_dir <- "C:/Users/your_username/Downloads/RIveR_v1.0.0"

app_dir <- "C:/Users/your_username/Downloads/RIveR_v1.0.0"

# ----------------------------------------------------------------------------
# 2. CHECK APPLICATION DIRECTORY
# ----------------------------------------------------------------------------

if (!dir.exists(app_dir)) {
  stop(
    "The RIveR application folder does not exist:\n",
    app_dir,
    "\n\nPlease edit 'app_dir' in this script and provide the correct folder."
  )
}

if (!file.exists(file.path(app_dir, "app.R"))) {
  stop(
    "The selected folder does not contain 'app.R':\n",
    app_dir,
    "\n\nPlease select the main RIveR application folder."
  )
}


# ----------------------------------------------------------------------------
# 3. CHECK REQUIRED R PACKAGES
# ----------------------------------------------------------------------------

required <- c(
  "shiny",
  "readxl",
  "referenceIntervals",
  "reflimR",
  "rpart",
  "gamlss",
  "gamlss.dist",
  "htmltools",
  "refineR",
  "processx",
  "ps",
  "future",
  "promises"
)

missing <- required[
  !vapply(
    required,
    requireNamespace,
    quietly = TRUE,
    FUN.VALUE = logical(1)
  )
]

if (length(missing)) {
  stop(
    "Missing required packages: ",
    paste(missing, collapse = ", "),
    ".\n\nPlease run source(\"install_packages.R\") first."
  )
}


# ----------------------------------------------------------------------------
# 4. CHECK REQUIRED PACKAGE VERSIONS
# ----------------------------------------------------------------------------

if (utils::packageVersion("shiny") < package_version("1.8.1")) {
  stop(
    "RIveR v1.0.0 requires Shiny >= 1.8.1.\n",
    "Please run source(\"install_packages.R\") to update the required packages."
  )
}

if (utils::packageVersion("refineR") < package_version("2.0.0")) {
  stop(
    "RIveR v1.0.0 requires refineR >= 2.0.0.\n",
    "Please run source(\"install_packages.R\") to update the required packages."
  )
}

if (utils::packageVersion("reflimR") < package_version("1.1.0")) {
  stop(
    "RIveR v1.0.0 requires reflimR >= 1.1.0.\n",
    "Please run source(\"install_packages.R\") to update the required packages."
  )
}


# ----------------------------------------------------------------------------
# 5. OPTIONAL PACKAGE CHECK
# ----------------------------------------------------------------------------

if (!requireNamespace("mclust", quietly = TRUE)) {
  message(
    "RIveR note: the 'mclust' package is not installed. ",
    "Core RIveR workflows will still work, ",
    "but GMM exploration will not be available. ",
    "Run source(\"install_packages.R\") if you want to install it."
  )
}


# ----------------------------------------------------------------------------
# 6. LAUNCH RIveR
# ----------------------------------------------------------------------------

message("Launching RIveR from:")
message(normalizePath(app_dir, winslash = "/", mustWork = TRUE))

shiny::runApp(
  appDir = app_dir,
  launch.browser = TRUE
)
