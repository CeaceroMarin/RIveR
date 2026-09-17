# RIveR -----------------------------------------------------------------
# RIveR v1.0.0 · stable version
# Guided assistant for RI establishment, verification, and review.

options(shiny.maxRequestSize = 100 * 1024^2)

if (!requireNamespace("shiny", quietly = TRUE)) {
  stop("Install the 'shiny' package before starting RIveR. Run install_packages.R")
}

library(shiny)

# v0.17.7: recovery of a persistent result is also executed outside the
# reactive graph. ExtendedTask is available in Shiny >= 1.8.1. The main
# statistical calculation continues to use a decoupled persistent Rscript worker.
if (!("ExtendedTask" %in% getNamespaceExports("shiny"))) {
  stop("RIveR v1.0.0 requires Shiny >= 1.8.1 to recover results without blocking the session. Run source(\"install_packages.R\").")
}
if (!requireNamespace("future", quietly = TRUE) || !requireNamespace("promises", quietly = TRUE)) {
  stop("RIveR v1.0.0 requires the 'future' and 'promises' packages. Run source(\"install_packages.R\").")
}
# This plan belongs only to the web process and is used for
# presentation/recovery operations. Statistical studies continue in a persistent Rscript process.
future::plan(future::multisession, workers = 1L)

source("R/helpers.R", local = TRUE)
source("R/job_manager.R", local = TRUE)
source("R/direct.R", local = TRUE)
source("R/indirect.R", local = TRUE)
source("R/d7.R", local = TRUE)
source("R/partition.R", local = TRUE)
source("R/age.R", local = TRUE)
source("R/decision.R", local = TRUE)
source("R/reporting.R", local = TRUE)

APP_VERSION <- "1.0.0"

# Visible branding. Internal code, function names, and historical identifiers
# remain rilctms_*/RIveR; only user-visible text is transformed.
brand_visible_text <- function(x) {
  if (is.null(x)) return(x)
  gsub("RIveR", "RIveR", as.character(x), fixed = TRUE)
}
brand_visible_object <- function(x) {
  if (is.null(x)) return(x)
  if (is.character(x)) return(brand_visible_text(x))
# Do not enter statistical S3/S4 objects (RWDRI, Mclust, rpart, etc.).
# Branding is presentation-only and must not modify any fit or internal structure.
  if (is.object(x) && !is.data.frame(x)) return(x)
  if (is.data.frame(x)) {
    for (nm in names(x)) if (is.character(x[[nm]])) x[[nm]] <- brand_visible_text(x[[nm]])
    return(x)
  }
  if (is.list(x)) {
    for (i in seq_along(x)) x[i] <- list(brand_visible_object(x[[i]]))
    return(x)
  }
  x
}

# status_html is a presentation function; the wrapper avoids modifying helpers.R.
.status_html_internal <- status_html
status_html <- function(status, title = NULL, text = NULL) {
  .status_html_internal(status, brand_visible_text(title), brand_visible_text(text))
}

css <- "
:root{--blue:#175a8b;--blue2:#0f4268;--bg:#f5f7fa;--line:#dbe2ea;--text:#1f2933;--muted:#66717d}
body{background:var(--bg);color:var(--text)}
.container-fluid{max-width:1180px;margin:auto}
.hero{background:linear-gradient(135deg,#174e75,#2777a8);color:white;border-radius:18px;padding:28px 30px;margin:18px 0 22px;box-shadow:0 8px 24px rgba(18,65,98,.16)}
.hero h1{font-weight:700;margin:0 0 3px}.hero p{margin:0;opacity:.9;font-size:16px}.river-expansion{margin:0 0 8px!important;font-size:14px!important;opacity:.95!important;letter-spacing:.1px}
.stepbar{display:flex;gap:8px;margin:10px 0 22px;flex-wrap:wrap}.stepchip{padding:8px 13px;border-radius:20px;background:#e7edf3;color:#5b6671;font-size:13px;font-weight:600}.stepchip.active{background:#175a8b;color:white}.stepchip.done{background:#d9efe4;color:#236746}
.panel-card{background:white;border:1px solid var(--line);border-radius:14px;padding:22px;margin-bottom:18px;box-shadow:0 2px 9px rgba(15,30,45,.04)}
.panel-card h3{margin-top:0}.muted{color:var(--muted)}
.status-card{background:white;border-radius:12px;padding:16px 18px;margin:12px 0;border:1px solid var(--line);border-left-width:6px}.status-green{border-left-color:#2f855a}.status-yellow{border-left-color:#b7791f}.status-red{border-left-color:#c53030}.status-grey{border-left-color:#718096}.status-title{font-weight:700;font-size:16px}.status-green .status-title{color:#276749}.status-yellow .status-title{color:#975a16}.status-red .status-title{color:#9b2c2c}.status-grey .status-title{color:#4a5568}.status-text{margin-top:5px;color:#4a5568}
.btn-primary{background:#175a8b;border-color:#175a8b}.btn-primary:hover{background:#0f4268;border-color:#0f4268}.btn-lg{border-radius:10px}\n/* Long calculations run outside the web process. Outputs are not dimmed during short polling flushes. */\n.recalculating,.shiny-html-output.recalculating{opacity:1!important}\nhtml.shiny-busy,html.shiny-busy body,html.shiny-busy .container-fluid{opacity:1!important;filter:none!important}\n.recovery-loading{border:1px solid #b9d3e5;border-left:6px solid #175a8b;border-radius:12px;padding:14px 16px;background:#f7fbfe;margin:14px 0}\n.analysis-progress{border:1px solid #dbe2ea;border-radius:12px;padding:14px 16px;background:#fbfcfd;margin-top:14px}.analysis-bar{height:12px;background:#e6ebf0;border-radius:8px;overflow:hidden;margin:8px 0}.analysis-fill{height:100%;background:#175a8b;transition:width .4s ease}.analysis-detail{font-size:13px;color:#4a5568}.analysis-eta{font-weight:600;margin-top:5px}
.choice-block .radio{padding:9px 11px;border:1px solid #dfe5eb;border-radius:9px;margin:7px 0;background:#fbfcfd}
.navrow{display:flex;justify-content:space-between;margin:18px 0}.small-note{font-size:12px;color:#68717c}.metric{display:inline-block;background:#f0f4f8;border-radius:10px;padding:10px 14px;margin:4px 5px 4px 0}.metric b{display:block;font-size:18px}
table{background:white}.footer-note{font-size:12px;color:#707b86;margin:24px 0 12px;border-top:1px solid #dce3e9;padding-top:12px}
"

busy_ui <- if ("useBusyIndicators" %in% getNamespaceExports("shiny")) {
  shiny::useBusyIndicators(spinners = FALSE, pulse = FALSE, fade = FALSE)
} else NULL
busy_opts <- if ("busyIndicatorOptions" %in% getNamespaceExports("shiny")) {
  shiny::busyIndicatorOptions(fade_opacity = 1, fade_selector = "html", spinner_delay = "999999s")
} else NULL

ui <- fluidPage(
  busy_ui,
  busy_opts,
  tags$head(tags$style(HTML(css))),
  div(class = "hero",
      h1("RIveR"),
      p(class = "river-expansion",
        tags$b("R"), "eference ",
        tags$b("I"), "nterval ",
        tags$b("v"), "erification and ",
        tags$b("e"), "stablishment in ",
        tags$b("R")
      ),
      p("Assistant for reference interval establishment, verification, review, and transferability"),
      div(style="margin-top:8px;font-size:12px;opacity:.8", paste("Version", APP_VERSION))
  ),
  uiOutput("job_recovery_ui"),
  uiOutput("stepbar"),
  tabsetPanel(id = "steps", type = "hidden",
    tabPanel("Start",
      div(class = "panel-card",
        h3("What do you need to do?"),
        p(class="muted", "RIveR will guide you step by step and propose the most appropriate methodological procedure."),
        div(class="choice-block",
          radioButtons("study_type", NULL,
            choices = c(
              "Establish a new reference interval" = "establish",
              "Verification, review, or transferability of a reference interval" = "verify"
            ), selected = "establish")
        ),
        actionButton("start_btn", "Start", class = "btn-primary btn-lg")
      ),
      div(class="panel-card",
          h4("How it works"),
          p("The application separates methodological decision-making from statistical calculation. It first assesses whether the data are suitable, selects or proposes the method, and finally generates a reasoned recommendation."),
          p(class="small-note", "The application does not replace the decision of the responsible laboratory specialist. All decisions must be reviewed and documented before clinical implementation."))
    ),

    tabPanel("Information",
      div(class="panel-card",
        h3("1 · Study information"),
        fluidRow(
          column(4, textInput("study_name", "Study name", placeholder = "E.g., ALT adults 18–65 years")),
          column(8, textInput("analyte", "Measurand", placeholder = "E.g., alanine aminotransferase"))
        ),
        fluidRow(
          column(4, textInput("unit", "Unit", placeholder = "U/L")),
          column(8, textInput("system", "Measurement system / procedure", placeholder = "Analyzer, reagent, method"))
        ),
        textAreaInput("population", "Target population", rows = 2, placeholder = "E.g., adult outpatient population, 18–65 years"),
        fluidRow(
          column(4,
            radioButtons("reference_tail", "Reference type",
              choices = c(
                "Two-sided" = "two_sided",
                "Lower one-sided" = "lower",
                "Upper one-sided" = "upper"
              ), selected = "two_sided", inline = TRUE)
          ),
          column(8, selectInput("reference_coverage", "Coverage", choices = c("95 %" = "95"), selected = "95"))
        ),
        uiOutput("reference_design_explanation"),
        uiOutput("data_origin_selector"),
        conditionalPanel("input.study_type != 'establish'",
          div(class="panel-card", style="background:#fafcfe",
            h4("Candidate or current interval"),
            conditionalPanel("input.study_type == 'verify'",
              radioButtons("verify_partition_mode", "Candidate RI structure",
                choices = c(
                  "Single RI, without partitions" = "none",
                  "RI partitioned by a qualitative variable" = "qualitative",
                  "RI partitioned by a quantitative variable (predefined ranges)" = "quantitative"
                ), selected = "none"),
              p(class="small-note", "A pre-existing partition is not reassessed in this workflow: each candidate RI is verified independently using the selected direct or indirect method.")
            ),
            fluidRow(column(6, selectInput("target_source", "Source of the candidate RI(s)", c("Manufacturer", "Literature", "Another laboratory", "Previous local study", "Other")))),
            conditionalPanel("input.study_type != 'verify' || input.verify_partition_mode == 'none'",
              uiOutput("target_limits_ui")
            ),
            conditionalPanel("input.study_type == 'verify' && input.verify_partition_mode != 'none'",
              p(class="small-note", "The limits for each partition will be entered after data preparation, once RIveR has identified the groups or ranges.")
            ),
            checkboxInput("transferability_ok", "Transferability (measurand, matrix, procedure, population, and preanalytical conditions) has been assessed as plausible", FALSE)
          )
        )
      ),
      div(class="panel-card",
        h4("Prerequisites"),
        fluidRow(
          column(6,
            checkboxInput("analytical_stable", "Measurement procedure stable throughout the study", TRUE),
            checkboxInput("qc_ok", "Internal/external quality control acceptable", TRUE),
            checkboxInput("preanalytic_ok", "Preanalytical conditions controlled/documented", TRUE)
          ),
          column(6,
            checkboxInput("population_defined", "Reference/target population defined", TRUE),
            checkboxInput("inclusion_defined", "Inclusion/exclusion criteria defined (if direct method)", TRUE),
            checkboxInput("outpatient_selectable", "Outpatient origin can be identified/selected (if indirect method)", FALSE)
          )
        )
      ),
      div(class="navrow", actionButton("back_start", "← Back"), actionButton("to_data", "Continue →", class="btn-primary"))
    ),

    tabPanel("Data",
      div(class="panel-card",
        h3("2 · Data upload and preparation"),
        p(class="muted", "Upload a CSV/TXT or Excel file. Data must be anonymized."),
        fluidRow(
          column(12, fileInput("data_file", "File", accept = c(".csv", ".txt", ".xlsx", ".xls")))
        ),
        uiOutput("raw_info"),
        uiOutput("column_mapping")
      ),
      div(class="panel-card",
        checkboxInput("one_per_patient", "Use one result for patient when an identifier is available", TRUE),
        actionButton("prepare_btn", "Prepare dataset", class="btn-primary"),
        br(), br(),
        uiOutput("prepared_summary"),
        tableOutput("audit_table")
      ),
      div(class="navrow", actionButton("back_info", "← Back"), actionButton("to_analysis", "Continue →", class="btn-primary"))
    ),

    tabPanel("Analysis",
      div(class="panel-card",
        h3("3 · Methodological assessment"),
        uiOutput("route_status"),
        h4("Dataset quality"),
        tableOutput("quality_table")
      ),
      div(class="panel-card",
        h4("Additional analyses"),
        conditionalPanel("input.study_type == 'establish'",
          uiOutput("covariate_analysis_status"),
          p(class="small-note", "RIveR automatically evaluates the covariates assigned during data preparation: the qualitative variable using the current partitioning criteria and the quantitative variable using the continuous-dependence workflow. No checkbox needs to be activated.")
        ),
        conditionalPanel("input.study_type == 'verify'",
          uiOutput("verification_partition_config")
        ),
        conditionalPanel("input.data_origin == 'indirect' && input.study_type == 'establish'",
          numericInput("bootstrap_n_est", "Bootstrap refineR", value = 200, min = 30, max = 1000, step = 10),
          p(class="small-note", "D7 uses refineR as the primary estimator and reflimR as an independent estimate. RIbench provides methodological context and does not validate the specific RI. Exploration with mclust/rpart is activated automatically when there is discordance, relevant heterogeneity, or a covariate signal; it is not a manual decision. If a qualitative variable requires partitioning, the D7 engine is run within the evaluable groups. If a quantitative variable acts continuously, continuous limits are modeled without creating arbitrary ranges. For functional testing, 30 bootstrap replicates may be used; for a final analysis, ≥200 are recommended.")
        ),
        conditionalPanel("input.data_origin == 'indirect' && input.study_type == 'verify'",
          numericInput("bootstrap_n", "Bootstrap refineR", value = 200, min = 30, max = 1000, step = 10),
          checkboxInput("d6_confirm_green", "Expert mode: run refineR/VeRUS even when reflimR is green/green", FALSE),
          checkboxInput("d6_explore_complexity", "Explore with mclust/rpart if the result is inconclusive", TRUE),
          p(class="small-note", "D6 follows the current workflow: reflimR/EL as screening; refineR/VeRUS if either limit is not green; mclust/rpart only as exploratory support when discordance is present. Discordance is not resolved automatically."),
          p(class="small-note", "For functional testing, 30 replicates may be used to reduce computation time. For a final analysis, refineR recommends ≥200 bootstrap replicates. The number used is recorded in the report.")
        ),
        uiOutput("run_analysis_ui"),
        uiOutput("analysis_progress_ui")
      ),
      div(class="navrow", actionButton("back_data", "← Back"), actionButton("to_results", "View results →", class="btn-primary"))
    ),

    tabPanel("Results",
      div(class="panel-card",
        h3("4 · Results"),
        uiOutput("next_action")
      ),
      uiOutput("verification_cohort_review_panel"),
      uiOutput("replacement_panel"),
      uiOutput("second_cohort_panel"),
      conditionalPanel("input.data_origin == 'direct' && input.study_type == 'establish'",
        div(class="panel-card",
          h4("1. Review of extreme / aberrant values"),
          p(class="small-note", "Aberrant-value review is performed before interpreting the RI, distribution, or partitions, because any justified exclusion requires the entire study to be recalculated. Detection does not imply exclusion."),
          h5("Complete reference population"),
          tableOutput("outlier_table"),
          uiOutput("outlier_boxplot_ui"),
          uiOutput("outlier_decision_ui"),
          uiOutput("outlier_impact_ui"),
          uiOutput("subgroup_outlier_ui"),
          tableOutput("subgroup_outlier_table"),
          uiOutput("subgroup_boxplot_ui"),
          uiOutput("subgroup_outlier_decision_ui"),
          tableOutput("outlier_decisions_table")
        )
      ),
      div(class="panel-card",
        h4("Main result"),
        uiOutput("main_status"),
        uiOutput("main_metrics"),
        tableOutput("main_table"),
        plotOutput("main_plot", height = 360)
      ),
      conditionalPanel("input.data_origin == 'direct' && input.study_type == 'establish'",
        div(class="panel-card",
          h4("Distribution assessment and method selection"),
          p(class="small-note", "With n≥120, CLSI retains the non-parametric method even if the distribution is normal. With n<120, RIveR uses normality, symmetry, and Box-Cox to select a defensible parametric/robust approach."),
          tableOutput("distribution_table"),
          tableOutput("method_candidates_table"),
          uiOutput("method_selection_note"),
          hr(),
          h5("Graphical inspection of the distribution"),
          p(class="small-note", "The plots complement formal tests by allowing assessment of shape, tails, and the current effect of Box-Cox before accepting a model."),
          fluidRow(
            column(6, h5("Original data"), plotOutput("distribution_original_hist", height = 250), plotOutput("distribution_original_qq", height = 250)),
            column(6, h5("After Box-Cox"), plotOutput("distribution_boxcox_hist", height = 250), plotOutput("distribution_boxcox_qq", height = 250))
          ),
          uiOutput("small_sample_resolution_ui")
        )
      ),
      conditionalPanel("input.data_origin == 'indirect' && input.study_type == 'establish'",
        div(class="panel-card",
          h4("D7 · Indirect establishment"),
          p(class="small-note", "refineR is the primary estimator; reflimR provides an independent estimate. Concordance adds robustness but is not a vote. RIbench only contextualizes the methodological performance of the algorithms."),
          tableOutput("d7_summary_table"),
          h5("Overall estimates by method"),
          tableOutput("indirect_methods_table"),
          h5("Overall refineR graphical diagnostic"),
          p(class="small-note", "Visual inspection of fit is part of the D7 review. If a covariate blocks the overall RI, this plot is retained for traceability rather than as a result to implement."),
          uiOutput("d7_recovered_diag_control"),
          plotOutput("d7_refine_plot", height = 380),
          uiOutput("d7_partition_title"),
          tableOutput("d7_partition_criteria_table"),
          tableOutput("d7_lahti_table"),
          tableOutput("d7_lahti_distance_table"),
          h5("Complete D7 results by group"),
          tableOutput("d7_partition_table"),
          tableOutput("d7_partition_methods_table"),
          uiOutput("d7_partition_plots_title"),
          fluidRow(
            column(6, plotOutput("d7_partition_refine_plot1", height = 300)),
            column(6, plotOutput("d7_partition_refine_plot2", height = 300))
          ),
          uiOutput("d7_age_title"),
          tableOutput("d7_age_table"),
          uiOutput("d7_age_model_title"),
          plotOutput("d7_age_curve_plot", height = 360),
          h5("Representative points of the continuous curve"),
          tableOutput("d7_age_model_table"),
          uiOutput("d7_application_title"),
          tableOutput("d7_application_table"),
          uiOutput("d7_sil_title"),
          tableOutput("d7_sil_table"),
          tableOutput("d7_sil_tradeoff_table"),
          uiOutput("d7_window_title"),
          tableOutput("d7_age_window_table"),
          uiOutput("d7_mclust_title"),
          tableOutput("d7_mclust_table"),
          h5("Computational environment and reproducibility"),
          tableOutput("d7_environment_table")
        )
      ),
      div(class="panel-card",
        h4("Qualitative variable · partitioning assessment"),
        uiOutput("partition_status"),
        uiOutput("partition_criteria_summary"),
        tableOutput("partition_pairwise_table"),
        tableOutput("partition_table"),
        uiOutput("partition_resolution_ui")
      ),
      div(class="panel-card",
        h4("Quantitative variable · continuous dependence"),
        p(class="small-note", "RIveR evaluates the percentiles defined by the reference design and P50 across the quantitative variable using a continuous model. rpart cut-points are candidate values only and are not recommended without subsequent validation."),
        uiOutput("age_status"),
        tableOutput("age_diagnostics_table"),
        uiOutput("age_cut_summary"),
        tableOutput("age_cut_validation_table"),
        tableOutput("age_boundary_table"),
        tableOutput("age_partition_table"),
        uiOutput("age_sil_status"),
        tableOutput("age_sil_table"),
        uiOutput("age_sil_tradeoff_title"),
        tableOutput("age_sil_tradeoff_table"),
        uiOutput("age_sil_download_ui"),
        h5("Reference curves by quantitative variable"),
        plotOutput("age_plot", height = 360),
        conditionalPanel("output.age_has_model == true",
          h5("Residual diagnostics of the continuous model"),
          fluidRow(
            column(6, plotOutput("age_residual_age_plot", height = 260)),
            column(6, plotOutput("age_residual_fitted_plot", height = 260))
          )
        )
      ),
      div(class="navrow", actionButton("back_analysis", "← Back"), actionButton("to_recommendation", "Recommendation →", class="btn-primary"))
    ),

    tabPanel("Recommendation",
      div(class="panel-card",
        h3("5 · RIveR recommendation"),
        uiOutput("final_status"),
        uiOutput("final_recommended_result"),
        tableOutput("final_recommended_table"),
        uiOutput("final_recommended_secondary"),
        tableOutput("final_recommended_secondary_table"),
        uiOutput("final_notes")
      ),
      div(class="panel-card",
        h4("Specialist closure / report"),
        p(class="small-note", "When RIveR requires a specific decision (e.g., resolving partitioning discordance), that decision is recorded and determines the content of the final report."),
        radioButtons("faculty_decision", NULL,
                     choices = c("I accept the proposed recommendation" = "accept",
                                 "I do not accept it / I want to modify it" = "override",
                                 "Decision pending" = "pending"), selected = "pending"),
        conditionalPanel("input.faculty_decision == 'override'",
                         textAreaInput("faculty_reason", "Justification", rows = 3)),
        downloadButton("download_report", "Generate HTML report"),
        downloadButton("download_pdf", "Generate visual PDF report"),
        downloadButton("download_clean", "Export prepared dataset")
      ),
      div(class="navrow", actionButton("back_results", "← Back"), actionButton("new_study", "New study"))
    )
  ),
  div(class="footer-note", "RIveR · methodological tool. Do not enter patient-identifiable data. The automated recommendation requires review by a laboratory specialist before clinical use.")
)

server <- function(input, output, session) {
  rv <- reactiveValues(
    step = "Start", study_type = "establish", raw = NULL, raw_name = NULL,
    prepared = NULL, quality = NULL, route = NULL,
    main = NULL, partition = NULL, age = NULL, final = NULL,
    metadata = list(),
    retained_extreme_values = numeric(0),
    outlier_baseline_data = NULL,
    outlier_decisions = data.frame(),
    outlier_impacts = data.frame(),
    partition_resolution = NULL,
    partition_decisions = data.frame(),
    small_sample_resolution = NULL,
    small_sample_decisions = data.frame(),
    analysis_iteration = 0L,
    second_raw = NULL, second_raw_name = NULL,
    partition_second_data = list(), partition_second_sources = list(),
    verification_exclusions = data.frame(), verification_exclusion_baseline = NULL,
    replacement_raw = NULL, replacement_raw_name = NULL, replacement_sources = character(0), replacement_n_total = 0L,
    analysis_running = FALSE, analysis_process = NULL, analysis_job_dir = NULL,
    analysis_job_id = NULL, analysis_signature = NULL, analysis_completion = NULL,
    analysis_status = NULL, analysis_started = NULL, analysis_status_mtime = as.POSIXct(NA),
    analysis_recovered = FALSE, recoverable_job = NULL, analysis_last_liveness_check = as.POSIXct(NA), analysis_dead_seen_at = as.POSIXct(NA),
    recovery_inputs = list(), analysis_rehydrating = FALSE, session_handled_job_ids = character(0),
    recovery_loading = FALSE, recovery_job_dir = NULL, recovery_job_id = NULL, recovery_expected_state = NULL,
    recovery_error = NULL, recovery_applied_key = NULL, defer_heavy_diagnostics = FALSE
  )

  try(rilctms_prune_old_jobs(30), silent = TRUE)
  rv$recoverable_job <- tryCatch(rilctms_latest_recoverable_job(), error = function(e) NULL)

# Non-blocking loader for persistent snapshots/results. It only performs I/O and
# deserialization outside the web process; it does not execute statistical calculations.
  recovery_loader <- shiny::ExtendedTask$new(function(jobdir, expected_job_id) {
    promises::future_promise({
      brand_text_bg <- function(x) {
        if (is.null(x)) return(x)
        gsub("RIveR", "RIveR", as.character(x), fixed = TRUE)
      }
      brand_object_bg <- function(x) {
        if (is.null(x)) return(x)
        if (is.character(x)) return(brand_text_bg(x))
        if (is.object(x) && !is.data.frame(x)) return(x)
        if (is.data.frame(x)) {
          for (nm in names(x)) if (is.character(x[[nm]])) x[[nm]] <- brand_text_bg(x[[nm]])
          return(x)
        }
        if (is.list(x)) {
          for (i in seq_along(x)) x[i] <- list(brand_object_bg(x[[i]]))
          return(x)
        }
        x
      }
      paths <- list(
        snapshot = file.path(jobdir, "snapshot.rds"),
        result = file.path(jobdir, "result.rds"),
        manifest = file.path(jobdir, "manifest.rds"),
        status = file.path(jobdir, "status.rds"),
        error = file.path(jobdir, "error.rds")
      )
      if (!file.exists(paths$snapshot)) stop("The persistent study snapshot was not found.")
      snap <- readRDS(paths$snapshot)
      man <- if (file.exists(paths$manifest)) readRDS(paths$manifest) else list()
      st <- if (file.exists(paths$status)) readRDS(paths$status) else list()
      ans <- if (file.exists(paths$result)) readRDS(paths$result) else NULL
      err <- if (file.exists(paths$error) && is.null(ans)) readRDS(paths$error) else NULL
      jid <- if (!is.null(ans) && !is.null(ans$job_id)) as.character(ans$job_id)[1] else
        if (!is.null(man$job_id)) as.character(man$job_id)[1] else
          if (!is.null(st$job_id)) as.character(st$job_id)[1] else ""
      if (nzchar(expected_job_id) && nzchar(jid) && !identical(as.character(expected_job_id), jid)) {
        stop("The persistent result does not correspond to the selected job.")
      }
      presentation_fields <- c("quality", "route", "partition_resolution", "partition_decisions", "small_sample_resolution", "small_sample_decisions")
      for (nm in intersect(names(if (is.null(snap$rv_state)) list() else snap$rv_state), presentation_fields)) snap$rv_state[[nm]] <- brand_object_bg(snap$rv_state[[nm]])
      if (!is.null(ans)) {
        for (nm in intersect(names(ans), c("main","partition","age","final"))) ans[[nm]] <- brand_object_bg(ans[[nm]])
      }
      list(snapshot = snap, result = ans, manifest = man, status = st, error = err, job_id = jid, prebranded = TRUE)
    }, seed = TRUE)
  })

  session_job_handled <- function(job_id) {
    jid <- as.character(job_id %||% "")[1]
    isTRUE(nzchar(jid)) && jid %in% (rv$session_handled_job_ids %||% character(0))
  }

  mark_job_handled_in_session <- function(job_id) {
    jid <- as.character(job_id %||% "")[1]
    if (nzchar(jid)) rv$session_handled_job_ids <- unique(c(rv$session_handled_job_ids %||% character(0), jid))
    invisible(TRUE)
  }

  input_or_snapshot <- function(name, default = NULL) {
    snap <- (rv$recovery_inputs %||% list())[[name]]
    prefer_snapshot <- isTRUE(rv$analysis_rehydrating) || isTRUE(rv$analysis_recovered)
    val <- if (prefer_snapshot && !is.null(snap) && length(snap) > 0L) snap else input[[name]]
    if (is.null(val) || length(val) == 0L) val <- snap
    if (is.null(val) || length(val) == 0L) default else val
  }

  steps_order <- c("Start", "Information", "Data", "Analysis", "Results", "Recommendation")
  goto <- function(step) {
    rv$step <- step
    updateTabsetPanel(session, "steps", selected = step)
  }

  reset_outlier_review <- function() {
    rv$retained_extreme_values <- numeric(0)
    rv$outlier_baseline_data <- NULL
    rv$outlier_decisions <- data.frame()
    rv$outlier_impacts <- data.frame()
    rv$partition_resolution <- NULL
    rv$partition_decisions <- data.frame()
    rv$small_sample_resolution <- NULL
    rv$small_sample_decisions <- data.frame()
    rv$analysis_iteration <- 0L
  }

  reset_verification_followup <- function() {
    rv$second_raw <- NULL
    rv$second_raw_name <- NULL
    rv$partition_second_data <- list()
    rv$partition_second_sources <- list()
    invisible(NULL)
  }

  reset_verification_cohort_review <- function() {
    rv$verification_exclusions <- data.frame()
    rv$verification_exclusion_baseline <- NULL
    rv$replacement_raw <- NULL
    rv$replacement_raw_name <- NULL
    rv$replacement_sources <- character(0)
    rv$replacement_n_total <- 0L
    invisible(NULL)
  }

  capture_analysis_snapshot <- function(spec, signature, completion) {
    rv_fields <- c(
      "study_type", "raw", "raw_name", "prepared", "quality", "route", "metadata",
      "retained_extreme_values", "outlier_baseline_data", "outlier_decisions", "outlier_impacts",
      "partition_resolution", "partition_decisions", "small_sample_resolution", "small_sample_decisions",
      "analysis_iteration", "second_raw", "second_raw_name", "partition_second_data", "partition_second_sources",
      "verification_exclusions", "verification_exclusion_baseline", "replacement_raw", "replacement_raw_name",
      "replacement_sources", "replacement_n_total"
    )
    rv_state <- setNames(lapply(rv_fields, function(nm) isolate(rv[[nm]])), rv_fields)
    input_names <- c(
      "study_type", "study_name", "analyte", "unit", "system", "population",
      "reference_tail", "reference_coverage", "data_origin", "target_source",
      "verify_partition_mode", "target_lower", "target_upper", "bootstrap_n_est", "bootstrap_n",
      "d6_confirm_green", "d6_explore_complexity", "transferability_ok", "analytical_stable",
      "qc_ok", "preanalytic_ok", "population_defined", "inclusion_defined", "outpatient_selectable",
      "one_per_patient"
    )
    inputs <- setNames(lapply(input_names, function(nm) isolate(input[[nm]])), input_names)
    list(
      app_version = APP_VERSION, created_at = Sys.time(), signature = signature,
      completion = completion, rv_state = rv_state, inputs = inputs
    )
  }

  restore_snapshot_inputs <- function(vals) {
    vals <- vals %||% list()
    rv$recovery_inputs <- vals
    rv$analysis_rehydrating <- TRUE
    later <- function() {
      if (!is.null(vals$study_type)) updateRadioButtons(session, "study_type", selected = vals$study_type)
      if (!is.null(vals$study_name)) updateTextInput(session, "study_name", value = vals$study_name)
      if (!is.null(vals$analyte)) updateTextInput(session, "analyte", value = vals$analyte)
      if (!is.null(vals$unit)) updateTextInput(session, "unit", value = vals$unit)
      if (!is.null(vals$system)) updateTextInput(session, "system", value = vals$system)
      if (!is.null(vals$population)) updateTextAreaInput(session, "population", value = vals$population)
      if (!is.null(vals$reference_tail)) updateRadioButtons(session, "reference_tail", selected = vals$reference_tail)
      if (!is.null(vals$reference_coverage)) updateSelectInput(session, "reference_coverage", selected = vals$reference_coverage)
      if (!is.null(vals$data_origin)) updateRadioButtons(session, "data_origin", selected = vals$data_origin)
      if (!is.null(vals$target_source)) updateSelectInput(session, "target_source", selected = vals$target_source)
      if (!is.null(vals$verify_partition_mode)) updateRadioButtons(session, "verify_partition_mode", selected = vals$verify_partition_mode)
      if (!is.null(vals$target_lower)) updateNumericInput(session, "target_lower", value = vals$target_lower)
      if (!is.null(vals$target_upper)) updateNumericInput(session, "target_upper", value = vals$target_upper)
      if (!is.null(vals$bootstrap_n_est)) updateNumericInput(session, "bootstrap_n_est", value = vals$bootstrap_n_est)
      if (!is.null(vals$bootstrap_n)) updateNumericInput(session, "bootstrap_n", value = vals$bootstrap_n)
      if (!is.null(vals$d6_confirm_green)) updateCheckboxInput(session, "d6_confirm_green", value = isTRUE(vals$d6_confirm_green))
      if (!is.null(vals$d6_explore_complexity)) updateCheckboxInput(session, "d6_explore_complexity", value = isTRUE(vals$d6_explore_complexity))
      if (!is.null(vals$transferability_ok)) updateCheckboxInput(session, "transferability_ok", value = isTRUE(vals$transferability_ok))
      if (!is.null(vals$analytical_stable)) updateCheckboxInput(session, "analytical_stable", value = isTRUE(vals$analytical_stable))
      if (!is.null(vals$qc_ok)) updateCheckboxInput(session, "qc_ok", value = isTRUE(vals$qc_ok))
      if (!is.null(vals$preanalytic_ok)) updateCheckboxInput(session, "preanalytic_ok", value = isTRUE(vals$preanalytic_ok))
      if (!is.null(vals$population_defined)) updateCheckboxInput(session, "population_defined", value = isTRUE(vals$population_defined))
      if (!is.null(vals$inclusion_defined)) updateCheckboxInput(session, "inclusion_defined", value = isTRUE(vals$inclusion_defined))
      if (!is.null(vals$outpatient_selectable)) updateCheckboxInput(session, "outpatient_selectable", value = isTRUE(vals$outpatient_selectable))
      if (!is.null(vals$one_per_patient)) updateCheckboxInput(session, "one_per_patient", value = isTRUE(vals$one_per_patient))
      session$onFlushed(function() { rv$analysis_rehydrating <- FALSE }, once = TRUE)
    }
    session$onFlushed(later, once = TRUE)
    invisible(TRUE)
  }

  restore_analysis_snapshot <- function(job_dir, snap = NULL, prebranded = FALSE) {
    paths <- rilctms_job_paths(job_dir)
    if (is.null(snap)) snap <- rilctms_safe_read_rds(paths$snapshot, NULL)
    if (is.null(snap)) return(FALSE)
    presentation_fields <- c("quality", "route", "partition_resolution", "partition_decisions", "small_sample_resolution", "small_sample_decisions")
    for (nm in names(snap$rv_state %||% list())) {
      val <- snap$rv_state[[nm]]
      if (!isTRUE(prebranded) && nm %in% presentation_fields) val <- brand_visible_object(val)
      rv[[nm]] <- val
    }
    rv$analysis_signature <- snap$signature %||% ""
    rv$analysis_completion <- snap$completion %||% list(action = "standard", go_results = TRUE, source_label = "Analysis")
    rv$analysis_recovered <- TRUE
    restore_snapshot_inputs(snap$inputs)
    TRUE
  }

  refresh_verification_audit <- function() {
    if (is.null(rv$prepared) || is.null(rv$prepared$audit)) return(invisible(NULL))
    aud <- rv$prepared$audit
    aud <- aud[!aud$step %in% c("Justified cohort exclusions", "Replacement/completion subjects added", "final n analyzed"), , drop = FALSE]
    nex <- if (is.data.frame(rv$verification_exclusions)) nrow(rv$verification_exclusions) else 0L
    if (nex > 0L) aud <- rbind(aud, data.frame(step = "Justified cohort exclusions", n = nex, stringsAsFactors = FALSE))
    nrep_obs <- as.integer(rv$replacement_n_total %||% 0L)
    if (nrep_obs > 0L) {
      aud <- rbind(aud, data.frame(step = "Replacement/completion subjects added", n = nrep_obs, stringsAsFactors = FALSE))
    }
    aud <- rbind(aud, data.frame(step = "final n analyzed", n = nrow(rv$prepared$data), stringsAsFactors = FALSE))
    rv$prepared$audit <- aud
    invisible(NULL)
  }

  refresh_outlier_audit <- function() {
    if (is.null(rv$prepared) || is.null(rv$prepared$audit)) return(invisible(NULL))
    aud <- rv$prepared$audit
    n_before_review <- if (any(aud$step == "n before specialist review")) {
      aud$n[which(aud$step == "n before specialist review")[1]]
    } else if (any(aud$step == "final n analyzed")) {
      aud$n[which(aud$step == "final n analyzed")[1]]
    } else nrow(rv$prepared$data)
    extra_names <- c("Specialist exclusions of extreme values", "Final n after specialist review",
                     "final n analyzed", "n before specialist review")
    aud <- aud[!aud$step %in% extra_names, , drop = FALSE]
    n_excl <- if (!is.null(rv$outlier_decisions) && nrow(rv$outlier_decisions)) {
      sum(rv$outlier_decisions$decision == "Exclude", na.rm = TRUE)
    } else 0L
    if (n_excl > 0) {
      aud <- rbind(aud,
                   data.frame(step = "n before specialist review", n = n_before_review, stringsAsFactors = FALSE),
                   data.frame(step = "Specialist exclusions of extreme values", n = n_excl, stringsAsFactors = FALSE),
                   data.frame(step = "final n analyzed", n = nrow(rv$prepared$data), stringsAsFactors = FALSE))
    } else {
      aud <- rbind(aud, data.frame(step = "final n analyzed", n = nrow(rv$prepared$data), stringsAsFactors = FALSE))
    }
    rv$prepared$audit <- aud
    invisible(NULL)
  }

  output$stepbar <- renderUI({
    idx <- match(rv$step, steps_order)
    div(class="stepbar",
        lapply(seq_along(steps_order), function(i) {
          cls <- if (i < idx) "stepchip done" else if (i == idx) "stepchip active" else "stepchip"
          div(class=cls, if (i == 1) paste0("0 · ", steps_order[i]) else paste0(i-1, " · ", steps_order[i]))
        }))
  })

  reference_design_reactive <- reactive({
    make_reference_design(
      input_or_snapshot("reference_tail", "two_sided"),
      (suppressWarnings(as.numeric(input_or_snapshot("reference_coverage", 95))) / 100)
    )
  })

  output$reference_design_explanation <- renderUI({
    d <- reference_design_reactive()
    txt <- if (identical(d$tail, "two_sided")) {
      paste0("Result: ", reference_percentile_label(d$pair_percentiles[["lower"]]), " – ",
             reference_percentile_label(d$pair_percentiles[["upper"]]),
             " (central interval ", formatC(100*d$coverage, format="fg", digits=4), " %).")
    } else if (identical(d$tail, "lower")) {
      paste0("Main result: lower limit ", reference_percentile_label(d$pair_percentiles[["lower"]]),
             ". The computational upper limit is used only for internal diagnostics and is not part of the clinical result.")
    } else {
      paste0("Main result: upper limit ", reference_percentile_label(d$pair_percentiles[["upper"]]),
             ". The computational lower limit is used only for internal diagnostics and is not part of the clinical result.")
    }
    p(class="small-note", txt,
      " A reference limit is not necessarily a clinical decision limit. ",
      "In this version, the operational coverage is 95%; other coverages are reserved for subsequent specific validation.")
  })

  quantitative_label <- function(result = NULL) {
    lab <- result$covariate_label %||% rv$main$quantitative_label %||% rv$prepared$covariates$quantitative$source %||% "Quantitative variable"
    as.character(lab)[1]
  }

  qualitative_label <- function(result = NULL) {
    lab <- result$covariate_label %||% rv$main$qualitative_label %||% rv$prepared$covariates$qualitative$source %||% "Qualitative variable"
    as.character(lab)[1]
  }

  quantitative_age_like <- function(result = NULL) {
    if (!is.null(result$covariate_is_age)) return(isTRUE(result$covariate_is_age))
    exists("quantitative_is_age_like", mode = "function") && quantitative_is_age_like(quantitative_label(result))
  }

  quantitative_axis_label <- function(result = NULL) {
    lab <- quantitative_label(result)
    if (quantitative_age_like(result)) paste0(lab, " (years)") else lab
  }

  genericize_quantitative_table <- function(tab, result = NULL) {
    if (is.null(tab) || !is.data.frame(tab) || quantitative_age_like(result)) return(tab)
    lab <- quantitative_label(result)
    names(tab) <- vapply(names(tab), function(nm) quantitative_relabel_text(nm, lab), character(1))
    for (nm in names(tab)) if (is.character(tab[[nm]])) tab[[nm]] <- quantitative_relabel_text(tab[[nm]], lab)
    tab
  }

  output$target_limits_ui <- renderUI({
    d <- reference_design_reactive()
    if (identical(d$tail, "two_sided")) {
      fluidRow(
        column(6, numericInput("target_lower", paste0("Lower limit · ", reference_percentile_label(d$pair_percentiles[["lower"]])), value = NA, step = 0.1)),
        column(6, numericInput("target_upper", paste0("Upper limit · ", reference_percentile_label(d$pair_percentiles[["upper"]])), value = NA, step = 0.1))
      )
    } else if (identical(d$tail, "lower")) {
      numericInput("target_lower", paste0("Candidate lower limit · ", reference_percentile_label(d$pair_percentiles[["lower"]])), value = NA, step = 0.1)
    } else {
      numericInput("target_upper", paste0("Candidate upper limit · ", reference_percentile_label(d$pair_percentiles[["upper"]])), value = NA, step = 0.1)
    }
  })

  output$data_origin_selector <- renderUI({
    st <- input$study_type %||% rv$study_type %||% "establish"
    if (st %in% c("verify", "review")) {
      radioButtons(
        "data_origin",
        "How do you want to verify/review the interval?",
        choices = c(
          "Direct verification · selected reference individuals" = "direct",
          "Indirect verification · routine LIS results" = "indirect"
        ),
        selected = input$data_origin %||% "direct"
      )
    } else {
      radioButtons(
        "data_origin",
        "What type of data will you use?",
        choices = c(
          "Selected reference individuals (direct method)" = "direct",
          "Routine LIS results (indirect method)" = "indirect"
        ),
        selected = input$data_origin %||% "direct"
      )
    }
  })

  observeEvent(input$verify_partition_mode, {
    reset_verification_followup()
    reset_verification_cohort_review()
    rv$main <- NULL; rv$partition <- NULL; rv$age <- NULL; rv$final <- NULL
  }, ignoreInit = TRUE)

  observeEvent(input$start_btn, {
    if (input$study_type == "guide") {
      showModal(modalDialog(
        title = "RIveR guides you",
        radioButtons("guide_state", "Current situation",
          choices = c("There is no suitable candidate RI" = "establish",
                      "There is an external RI that I want to apply" = "verify",
                      "We already use an RI and I want to review whether it remains suitable" = "review"),
          selected = "verify"),
        footer = tagList(modalButton("Cancel"), actionButton("guide_ok", "Continue", class="btn-primary"))
      ))
    } else {
      rv$study_type <- input$study_type
      goto("Information")
    }
  })
  observeEvent(input$guide_ok, {
    rv$study_type <- input$guide_state
    updateRadioButtons(session, "study_type", selected = rv$study_type)
    removeModal(); goto("Information")
  })

  observeEvent(input$back_start, goto("Start"))
  observeEvent(input$back_info, goto("Information"))
  observeEvent(input$back_data, goto("Data"))
  observeEvent(input$back_analysis, goto("Analysis"))
  observeEvent(input$back_results, goto("Results"))

  observeEvent(input$to_data, {
    rv$study_type <- input$study_type
    if (identical(rv$study_type, "establish") && identical(input$data_origin, "direct") &&
        !nzchar(trimws(input$population %||% ""))) {
      showNotification("Define the reference/target population before continuing.", type="warning", duration=8)
      return()
    }
    rv$metadata <- list(
      study_name = input$study_name, analyte = input$analyte,
      unit = input$unit, system = input$system, population = input$population,
      study_type = rv$study_type, data_origin = input$data_origin,
      target_source = input$target_source,
      reference_design = reference_design_reactive(),
      reference_design_label = reference_design_label(reference_design_reactive())
    )
    goto("Data")
  })

  observeEvent(input$data_file, {
    req(input$data_file)
    dat <- tryCatch(read_lab_file(input$data_file$datapath, input$data_file$name), error = function(e) e)
    if (inherits(dat, "error")) {
      showNotification(conditionMessage(dat), type="error", duration=8)
    } else {
      rv$raw <- dat; rv$raw_name <- input$data_file$name
      reset_verification_followup()
      reset_verification_cohort_review()
      rv$prepared <- NULL; rv$main <- NULL; rv$partition <- NULL; rv$age <- NULL; rv$final <- NULL
      reset_outlier_review()
    }
  })

  output$raw_info <- renderUI({
    if (is.null(rv$raw)) return(status_html("grey", "No file uploaded", "Upload a file to continue."))
    tagList(
      div(class="metric", tags$b(fmt_integer(nrow(rv$raw))), "rows"),
      div(class="metric", tags$b(ncol(rv$raw)), "columns"),
      div(class="small-note", paste("File:", rv$raw_name))
    )
  })

  guess_qualitative_column <- function(dat, value_col = NULL) {
    nms <- names(dat)
    if (!length(nms)) return("")
    low <- tolower(nms)
    preferred <- c("sex", "sex", "gender", "group", "group", "category", "category")
    for (nm in preferred) {
      hit <- which(low == nm)
      if (length(hit)) return(nms[hit[1]])
    }
    candidates <- nms[vapply(nms, function(nm) {
      if (!is.null(value_col) && identical(nm, value_col)) return(FALSE)
      z <- dat[[nm]]
      zz <- trimws(as.character(z)); zz <- zz[!is.na(zz) & nzchar(zz)]
      k <- length(unique(zz))
      (is.character(z) || is.factor(z) || is.logical(z)) && k >= 2L && k <= 20L
    }, logical(1))]
    if (length(candidates)) candidates[1] else ""
  }

  guess_quantitative_column <- function(dat, value_col = NULL) {
    nms <- names(dat)
    if (!length(nms)) return("")
    low <- tolower(nms)
    preferred <- c("age", "age", "age")
    for (nm in preferred) {
      hit <- which(low == nm)
      if (length(hit)) return(nms[hit[1]])
    }
    candidates <- nms[vapply(nms, function(nm) {
      if (!is.null(value_col) && identical(nm, value_col)) return(FALSE)
      z <- safe_num(dat[[nm]])
      sum(is.finite(z)) >= 20L && length(unique(z[is.finite(z)])) >= 10L
    }, logical(1))]
    if (length(candidates)) candidates[1] else ""
  }

  output$column_mapping <- renderUI({
    req(rv$raw)
    nms <- names(rv$raw)
    choices_opt <- c("(none)" = "", nms)
    value_default <- if ("Value" %in% nms) "Value" else if ("value" %in% nms) "value" else nms[1]
    qualitative_default <- guess_qualitative_column(rv$raw, value_default)
    quantitative_default <- guess_quantitative_column(rv$raw, value_default)
    tagList(
      h4("Column assignment"),
      fluidRow(
        column(4, selectInput("value_col", "Result *", choices = nms, selected = value_default)),
        column(4, selectInput("id_col", "Anonymized identifier", choices = choices_opt, selected = if ("ID_ref" %in% nms) "ID_ref" else if ("ID" %in% nms) "ID" else "")),
        column(4, selectInput("date_col", "Date/time", choices = choices_opt, selected = if ("Date" %in% nms) "Date" else ""))
      ),
      fluidRow(
        column(4, selectInput("qualitative_col", "Qualitative variable", choices = choices_opt, selected = qualitative_default)),
        column(4, selectInput("quantitative_col", "Quantitative variable", choices = choices_opt, selected = quantitative_default)),
        column(4, selectInput("origin_col", "Origin", choices = choices_opt, selected = if ("Origin" %in% nms) "Origin" else ""))
      ),
      p(class="small-note", "RIveR automatically proposes the variable type based on file contents, but you can correct the assignment before preparing the data. A qualitative variable may have 2 or more categories; a quantitative variable is evaluated as a continuous covariate."),
      conditionalPanel("input.study_type == 'verify' && input.data_origin == 'direct'",
        p(class="small-note", "For direct verification, the first upload corresponds to the first cohort. For a single RI, this consists of 20 individuals; if the RI is partitioned, 20 individuals are required for partition. Only inconclusive partitions will subsequently require a second independent cohort of 20 individuals.")
      )
    )
  })

  observeEvent(input$prepare_btn, {
    req(rv$raw, input$value_col)
    pr <- tryCatch(prepare_analysis_data(rv$raw, input$value_col,
                                         id_col = input$id_col, quantitative_col = input$quantitative_col,
                                         qualitative_col = input$qualitative_col, date_col = input$date_col,
                                         origin_col = input$origin_col,
                                         verification_round_col = if (identical(rv$study_type, "verify") && identical(input$data_origin, "direct")) "" else input$verification_round_col %||% "",
                                         one_per_patient = input$one_per_patient),
                   error = function(e) e)
    if (inherits(pr, "error")) {
      showNotification(conditionMessage(pr), type="error", duration=8)
    } else {
      if (identical(rv$study_type, "verify") && identical(input$data_origin, "direct")) {
        reset_verification_followup()
        reset_verification_cohort_review()
      }
      rv$prepared <- pr
      rv$main <- NULL; rv$partition <- NULL; rv$age <- NULL; rv$final <- NULL
      reset_outlier_review()
      rv$outlier_baseline_data <- pr$data
      showNotification(paste("Prepared dataset:", nrow(pr$data), "results"), type="message")
    }
  })

  output$prepared_summary <- renderUI({
    if (is.null(rv$prepared)) return(status_html("grey", "Dataset pending", "Click 'Prepare dataset' to validate and apply the basic rules."))
    dat <- rv$prepared$data
    tagList(
      status_html("green", "Prepared dataset", paste(nrow(dat), "evaluable numeric results")),
      div(class="metric", tags$b(fmt_integer(nrow(dat))), "final n"),
      div(class="metric", tags$b(rv$prepared$removed_non_numeric), "non-numeric/NA values removed"),
      div(class="metric", tags$b(rv$prepared$removed_duplicates), "repeated results removed"),
      if ("qualitative" %in% names(dat)) div(class="metric", tags$b(length(unique(dat$qualitative[!is.na(dat$qualitative) & nzchar(dat$qualitative)]))), paste0("categories · ", rv$prepared$covariates$qualitative$source %||% "qualitative")),
      if ("quantitative" %in% names(dat)) div(class="metric", tags$b(length(unique(dat$quantitative[is.finite(dat$quantitative)]))), paste0("values · ", rv$prepared$covariates$quantitative$source %||% "quantitative"))
    )
  })
  output$audit_table <- renderTable({ req(rv$prepared); rv$prepared$audit }, striped=TRUE, bordered=FALSE, spacing="s")

  observeEvent(input$to_analysis, {
    if (is.null(rv$prepared)) {
      showNotification("Prepare the dataset first.", type="warning"); return()
    }
    goto("Analysis")
  })

  verification_partition_mode <- reactive({
    if (!identical(rv$study_type %||% input_or_snapshot("study_type", "establish"), "verify")) return("none")
    input_or_snapshot("verify_partition_mode", "none")
  })

  qualitative_verification_groups <- reactive({
    req(rv$prepared)
    if (!"qualitative" %in% names(rv$prepared$data)) return(character(0))
    z <- trimws(as.character(rv$prepared$data$qualitative))
    sort(unique(z[!is.na(z) & nzchar(z)]))
  })

  parse_optional_number <- function(x) {
    z <- trimws(as.character(x %||% ""))
    if (!nzchar(z)) return(NA_real_)
    suppressWarnings(as.numeric(gsub(",", ".", z, fixed = TRUE)))
  }

  output$verification_partition_config <- renderUI({
    mode <- verification_partition_mode()
    design <- reference_design_reactive()
    active <- design$active
    limit_inputs <- function(i, lrl0 = NA, url0 = NA, lower_width = 4, upper_width = 4) {
      items <- list()
      if (isTRUE(active[["lower"]])) {
        items[[length(items)+1L]] <- column(lower_width,
          numericInput(paste0("verify_part_lrl_", i),
                       paste0("Lower limit · ", reference_percentile_label(design$pair_percentiles[["lower"]])),
                       value = lrl0, step = 0.1))
      }
      if (isTRUE(active[["upper"]])) {
        items[[length(items)+1L]] <- column(upper_width,
          numericInput(paste0("verify_part_url_", i),
                       paste0("Upper limit · ", reference_percentile_label(design$pair_percentiles[["upper"]])),
                       value = url0, step = 0.1))
      }
      items
    }
    if (identical(mode, "none")) {
      return(p(class="small-note", if (identical(input$data_origin, "direct")) "Single limit/RI: the D5 workflow is applied with an initial cohort of 20 individuals." else "Single limit/RI: the D6 workflow is applied to routine results; reflimR requires approximately n≥200 for partition."))
    }
    if (identical(mode, "qualitative")) {
      groups <- qualitative_verification_groups()
      if (!length(groups)) {
        return(status_html("grey", "Qualitative variable missing",
                           "Assign and prepare a qualitative variable before entering candidate limits/RIs."))
      }
      rows <- lapply(seq_along(groups), function(i) {
        g <- groups[i]
        lrl0 <- NA_real_
        url0 <- NA_real_
        cols <- c(list(column(if (identical(design$tail, "two_sided")) 4 else 6,
                              tags$b(paste0("Partition: ", g)),
                              div(class="small-note", if (identical(input$data_origin, "direct")) "20 individuals are required in the first cohort." else "≥200 routine results are recommended for this partition."))),
                  limit_inputs(i, lrl0, url0, if (identical(design$tail, "two_sided")) 4 else 6,
                               if (identical(design$tail, "two_sided")) 4 else 6))
        do.call(fluidRow, cols)
      })
      return(tagList(
        h5("Candidate limits / RIs by qualitative variable"),
        p(class="small-note", "RIveR does not assess whether partitioning is required: it assumes that these limits/RIs have already been predefined and verifies each partition independently."),
        rows
      ))
    }

    nband <- input$verify_age_band_n %||% 2L
    nband <- max(2L, min(6L, as.integer(nband)))
    rows <- lapply(seq_len(nband), function(i) {
      lab0 <- paste0("Range ", i)
      min0 <- ""
      max0 <- ""
      lrl0 <- NA_real_
      url0 <- NA_real_
      base_cols <- list(
        column(3, textInput(paste0("verify_age_label_", i), "Label", value = lab0)),
        column(2, textInput(paste0("verify_age_min_", i), "Minimum value (included)", value = min0, placeholder = "no limit")),
        column(2, textInput(paste0("verify_age_max_", i), "Maximum value (excluded)", value = max0, placeholder = "no limit"))
      )
      lw <- if (identical(design$tail, "two_sided")) 2 else 4
      cols <- c(base_cols, limit_inputs(i, lrl0, url0, lw, lw))
      div(style="border-top:1px solid #e7ebef;padding-top:8px;margin-top:8px", do.call(fluidRow, cols))
    })
    tagList(
      h5("Candidate limits / RIs by quantitative-variable ranges"),
      numericInput("verify_age_band_n", "Number of ranges", value = nband, min = 2, max = 6, step = 1),
      p(class="small-note", "Convention: minimum value included and maximum value excluded. Leave an endpoint blank if the first or last range has no bound."),
      rows
    )
  })

  verification_partition_definitions <- reactive({
    mode <- verification_partition_mode()
    if (identical(mode, "none")) return(NULL)
    if (identical(mode, "qualitative")) {
      groups <- qualitative_verification_groups()
      if (!length(groups)) return(NULL)
      rows <- lapply(seq_along(groups), function(i) {
        data.frame(
          key = paste0("qualitative_", i), label = groups[i], type = "sex",
          lower = if (isTRUE(reference_design_reactive()$active[["lower"]])) suppressWarnings(as.numeric(input[[paste0("verify_part_lrl_", i)]] %||% NA_real_)) else NA_real_,
          upper = if (isTRUE(reference_design_reactive()$active[["upper"]])) suppressWarnings(as.numeric(input[[paste0("verify_part_url_", i)]] %||% NA_real_)) else NA_real_,
          sex_value = groups[i], age_min = NA_real_, age_max = NA_real_,
          stringsAsFactors = FALSE
        )
      })
      return(do.call(rbind, rows))
    }
    nband <- input$verify_age_band_n %||% 2L
    nband <- max(2L, min(6L, as.integer(nband)))
    rows <- lapply(seq_len(nband), function(i) {
      min_raw <- trimws(as.character(input[[paste0("verify_age_min_", i)]] %||% ""))
      max_raw <- trimws(as.character(input[[paste0("verify_age_max_", i)]] %||% ""))
      amin <- parse_optional_number(min_raw)
      amax <- parse_optional_number(max_raw)
      data.frame(
        key = paste0("quantitative_", i),
        label = trimws(as.character(input[[paste0("verify_age_label_", i)]] %||% paste0("Range ", i))),
        type = "age",
        lower = if (isTRUE(reference_design_reactive()$active[["lower"]])) suppressWarnings(as.numeric(input[[paste0("verify_part_lrl_", i)]] %||% NA_real_)) else NA_real_,
        upper = if (isTRUE(reference_design_reactive()$active[["upper"]])) suppressWarnings(as.numeric(input[[paste0("verify_part_url_", i)]] %||% NA_real_)) else NA_real_,
        sex_value = NA_character_,
        age_min = amin,
        age_max = amax,
        age_min_valid = !nzchar(min_raw) || is.finite(amin),
        age_max_valid = !nzchar(max_raw) || is.finite(amax),
        stringsAsFactors = FALSE
      )
    })
    do.call(rbind, rows)
  })

  route_reactive <- reactive({
    req(rv$prepared)
    dat <- rv$prepared$data
    data_origin <- input_or_snapshot("data_origin", "direct")
    population_defined <- isTRUE(input_or_snapshot("population_defined", FALSE))
    population_text <- as.character(input_or_snapshot("population", ""))[1]
    inclusion_defined <- isTRUE(input_or_snapshot("inclusion_defined", FALSE))
    preanalytic_ok <- isTRUE(input_or_snapshot("preanalytic_ok", FALSE))
    analytical_stable <- isTRUE(input_or_snapshot("analytical_stable", FALSE))
    qc_ok <- isTRUE(input_or_snapshot("qc_ok", FALSE))
    transferability_ok <- isTRUE(input_or_snapshot("transferability_ok", FALSE))
    vmode <- verification_partition_mode()
    defs <- verification_partition_definitions()
    design <- reference_design_reactive()
    active <- design$active
    valid_targets <- function(lower, upper) {
      ok <- TRUE
      if (isTRUE(active[["lower"]])) ok <- ok && length(lower) > 0L && all(is.finite(lower))
      if (isTRUE(active[["upper"]])) ok <- ok && length(upper) > 0L && all(is.finite(upper))
      if (isTRUE(active[["lower"]]) && isTRUE(active[["upper"]])) ok <- ok && all(lower < upper)
      ok
    }
    target_present <- if (identical(rv$study_type, "verify") && !identical(vmode, "none")) {
      !is.null(defs) && nrow(defs) >= 2L && valid_targets(defs$lower, defs$upper) &&
        (!"age_min_valid" %in% names(defs) || all(defs$age_min_valid)) &&
        (!"age_max_valid" %in% names(defs) || all(defs$age_max_valid))
    } else {
      valid_targets(input_or_snapshot("target_lower", NA_real_), input_or_snapshot("target_upper", NA_real_))
    }
    population_ok <- population_defined && nzchar(trimws(population_text)) &&
      (if (identical(data_origin, "direct")) inclusion_defined else TRUE)
    rr <- route_recommendation(rv$study_type, data_origin, nrow(dat),
                               population_defined = population_ok,
                               preanalytic_ok = preanalytic_ok,
                               analytical_stable = analytical_stable,
                               qc_ok = qc_ok,
                               target_present = target_present)
# D7: n<200 is an auditable conclusion (NOT EVALUABLE), not a technical error.
# route_recommendation() remains unchanged so the frozen D4–D6 engines are not altered.
    if (identical(rv$study_type, "establish") && identical(data_origin, "indirect") && nrow(dat) < 200L) {
      rr$blockers <- rr$blockers[!grepl("Fewer than 200 results", rr$blockers, fixed = TRUE)]
      rr$warnings <- unique(c(rr$warnings, "n<200: D7 will not run refineR/reflimR, but it will generate a NOT EVALUABLE result and an auditable report."))
      rr$can_run <- length(rr$blockers) == 0L
      rr$status <- if (length(rr$blockers)) "red" else "yellow"
    }
    if (identical(rv$study_type, "verify") && !identical(vmode, "none")) {
      if (identical(vmode, "qualitative") && !"qualitative" %in% names(dat)) {
        rr$status <- "red"; rr$can_run <- FALSE
        rr$blockers <- c(rr$blockers, "A qualitative variable must be assigned to verify partitioned RIs.")
      }
      if (identical(vmode, "quantitative") && !"quantitative" %in% names(dat)) {
        rr$status <- "red"; rr$can_run <- FALSE
        rr$blockers <- c(rr$blockers, "A quantitative variable must be assigned to verify RIs partitioned by ranges.")
      }
    }
    if (rv$study_type %in% c("verify", "review") && !transferability_ok) {
      rr$status <- "red"; rr$can_run <- FALSE
      rr$blockers <- c(rr$blockers, "Transferability of the candidate/current RI has not been confirmed.")
    }
    rr
  })

  quality_reactive <- reactive({
    req(rv$prepared)
    dat <- rv$prepared$data
    data_origin <- input_or_snapshot("data_origin", "direct")
    population_ok <- isTRUE(input_or_snapshot("population_defined", FALSE)) &&
      nzchar(trimws(as.character(input_or_snapshot("population", ""))[1])) &&
      (if (identical(data_origin, "direct")) isTRUE(input_or_snapshot("inclusion_defined", FALSE)) else TRUE)
    make_quality_assessment(data_origin, nrow(dat),
                            isTRUE(input_or_snapshot("analytical_stable", FALSE)),
                            isTRUE(input_or_snapshot("qc_ok", FALSE)),
                            population_defined = population_ok,
                            preanalytic_ok = isTRUE(input_or_snapshot("preanalytic_ok", FALSE)),
                            has_patient_id = "patient_id" %in% names(dat), has_quantitative = "quantitative" %in% names(dat),
                            has_qualitative = "qualitative" %in% names(dat),
                            outpatient_selectable = isTRUE(input_or_snapshot("outpatient_selectable", FALSE)),
                            study_type = rv$study_type %||% input_or_snapshot("study_type", "establish"))
  })

  output$covariate_analysis_status <- renderUI({
    req(rv$prepared)
    dat <- rv$prepared$data
    st <- rv$study_type %||% input$study_type %||% "establish"
    vmode <- verification_partition_mode()
    items <- list()
    if ("qualitative" %in% names(dat)) {
      lv <- sort(unique(trimws(as.character(dat$qualitative[!is.na(dat$qualitative) & nzchar(trimws(as.character(dat$qualitative)))]))))
      src <- rv$prepared$covariates$qualitative$source %||% "Qualitative variable"
      txt <- if (identical(st, "establish")) {
        if (length(lv) >= 2L) paste0(length(lv), " categories detected · RIveR will automatically assess whether the RI should be partitioned.") else "There are fewer than two evaluable categories."
      } else if (identical(st, "verify") && identical(vmode, "qualitative")) {
        paste0(length(lv), " categories detected · they will be used to independently verify the predefined candidate RIs; RIveR will not reassess the need for partitioning.")
      } else {
        paste0(length(lv), " categories detected · available for traceability; this workflow does not redefine the partition.")
      }
      items[[length(items)+1L]] <- status_html(if (length(lv) >= 2L) "green" else "yellow", paste0("Qualitative variable · ", src), txt)
    } else {
      items[[length(items)+1L]] <- status_html("grey", "Qualitative variable", "Not assigned.")
    }
    if ("quantitative" %in% names(dat)) {
      z <- dat$quantitative[is.finite(dat$quantitative)]
      src <- rv$prepared$covariates$quantitative$source %||% "Quantitative variable"
      txt <- if (identical(st, "establish")) {
        if (length(unique(z)) >= 10L) paste0("Range ", signif(min(z),5), "–", signif(max(z),5), " · RIveR will automatically assess continuous dependence.") else "There are not enough distinct values to assess continuous dependence."
      } else if (identical(st, "verify") && identical(vmode, "quantitative")) {
        paste0("Range ", signif(min(z),5), "–", signif(max(z),5), " · will be used to assign each result to the predefined ranges that will be verified independently.")
      } else {
        paste0("Range ", signif(min(z),5), "–", signif(max(z),5), " · available for traceability; this workflow does not create new ranges.")
      }
      items[[length(items)+1L]] <- status_html(if (length(unique(z)) >= 10L || !identical(st,"establish")) "green" else "yellow", paste0("Quantitative variable · ", src), txt)
    } else {
      items[[length(items)+1L]] <- status_html("grey", "Quantitative variable", "Not assigned.")
    }
    do.call(tagList, items)
  })

  output$route_status <- renderUI({
    if (isTRUE(rv$analysis_rehydrating)) return(status_html("grey", "Recovering study state…", "RIveR is restoring the methodological configuration from the previous session."))
    rr <- route_reactive()
    details <- c(rr$blockers, rr$warnings)
    status_html(rr$status, rr$title, if (length(details)) paste(details, collapse=" ") else "No prior methodological blockers have been identified.")
  })
  output$quality_table <- renderTable({
    if (isTRUE(rv$analysis_rehydrating)) return(data.frame(Element="Recovering study state…", Detail="Please wait a few moments while RIveR restores the session.", stringsAsFactors=FALSE))
    q <- quality_reactive()
    if ("status" %in% names(q)) {
      q$status <- vapply(q$status, function(st) switch(st,
        green = status_symbol_text("green", "Correct"),
        yellow = status_symbol_text("yellow", "Review"),
        red = status_symbol_text("red", "Blocked"),
        grey = status_symbol_text("grey", "Not evaluable"),
        status_symbol_text("grey", "Not evaluable")), character(1))
      names(q)[names(q) == "status"] <- "Status"
    }
    names(q)[names(q) == "item"] <- "Element"
    names(q)[names(q) == "detail"] <- "Detail"
    q
  }, striped=TRUE, spacing="s")

  output$job_recovery_ui <- renderUI({
    if (isTRUE(rv$recovery_loading)) {
      return(div(class = "recovery-loading",
        h4("Recovering results…"),
        p("RIveR is loading the persistent job outside the web session."),
        p(class = "small-note", "You can continue using the interface while recovery is prepared. No statistical result is recalculated.")
      ))
    }
    z <- rv$recoverable_job
    if (is.null(z)) return(NULL)
    jid <- as.character(z$manifest$job_id %||% z$status$job_id %||% "")
    if (session_job_handled(jid)) return(NULL)
    if (isTRUE(rv$analysis_running) && identical(jid, as.character(rv$analysis_job_id %||% ""))) return(NULL)
    state <- z$state %||% "unknown"
    label <- z$manifest$source_label %||% "Analysis"
    study <- z$manifest$study_name %||% "Unnamed study"
    p <- max(0, min(1, suppressWarnings(as.numeric(z$status$progress %||% 0))))
    started <- z$status$started_at %||% z$manifest$created_at %||% NULL
    when <- if (!is.null(started)) format(started, "%d/%m/%Y %H:%M") else "—"
    title <- switch(state,
      running = "There is an ongoing analysis that can be recovered",
      complete = "There is a completed analysis pending recovery",
      error = "There is an analysis that ended with an error",
      interrupted = "There is an interrupted analysis",
      cancelled = "Analysis cancelled",
      "There is a previous job pending review"
    )
    action_label <- if (identical(state, "running")) "Resume monitoring" else if (identical(state, "complete")) "Recover results" else "Review job"
    div(class = "panel-card",
        h4(title),
        p(tags$b(study), " · ", label, " · start: ", when),
        p(class = "small-note", paste0("Status: ", state, if (identical(state, "running")) paste0(" · ", round(100*p), " %") else "")),
        actionButton("recover_job", action_label, class = "btn-primary"),
        if (!identical(state, "running")) actionButton("dismiss_recovery", "Dismiss notice")
    )
  })

  observe({
    if (isTRUE(rv$analysis_running) || isTRUE(rv$recovery_loading)) return()
    invalidateLater(5000, session)
    z <- tryCatch(rilctms_latest_recoverable_job(), error = function(e) NULL)
    old <- isolate(rv$recoverable_job)
    old_id <- if (is.null(old)) "" else as.character(old$manifest$job_id %||% old$status$job_id %||% "")
    new_id <- if (is.null(z)) "" else as.character(z$manifest$job_id %||% z$status$job_id %||% "")
    old_state <- if (is.null(old)) "" else as.character(old$state %||% "")
    new_state <- if (is.null(z)) "" else as.character(z$state %||% "")
    if (!identical(old_id, new_id) || !identical(old_state, new_state)) rv$recoverable_job <- z
  })

  output$run_analysis_ui <- renderUI({
    if (isTRUE(rv$analysis_running)) {
      tags$button(type="button", class="btn btn-primary btn-lg", disabled="disabled",
                  icon("hourglass-half"), " Analysis in progress…")
    } else {
      actionButton("run_analysis", "Run analysis", class="btn-primary btn-lg")
    }
  })

  format_elapsed <- function(seconds) {
    seconds <- max(0, as.numeric(seconds %||% 0))
    if (seconds < 60) return(paste0(round(seconds), " s"))
    h <- floor(seconds / 3600); m <- floor((seconds %% 3600) / 60)
    if (h > 0) paste0(h, " h ", m, " min") else paste0(m, " min")
  }

  output$analysis_elapsed_text <- renderText({
    if (isTRUE(rv$analysis_running)) invalidateLater(1000, session)
    elapsed <- if (!is.null(rv$analysis_started)) as.numeric(difftime(Sys.time(), rv$analysis_started, units="secs")) else 0
    format_elapsed(elapsed)
  })

  output$analysis_progress_ui <- renderUI({
    if (!isTRUE(rv$analysis_running) && is.null(rv$analysis_status)) return(NULL)
    st <- rv$analysis_status %||% list(state="running", progress=0.01, detail="Preparing calculation")
    p <- max(0, min(1, suppressWarnings(as.numeric(st$progress %||% 0))))
    elapsed <- if (!is.null(rv$analysis_started)) as.numeric(difftime(Sys.time(), rv$analysis_started, units="secs")) else 0
    eta_txt <- NULL
    eta_lo <- suppressWarnings(as.numeric(st$eta_low_seconds %||% NA_real_))
    eta_hi <- suppressWarnings(as.numeric(st$eta_high_seconds %||% NA_real_))
    if (identical(st$state %||% "", "running") && is.finite(eta_lo) && is.finite(eta_hi) && eta_hi >= 0) {
      eta_lo <- max(0, eta_lo); eta_hi <- max(eta_lo, eta_hi)
      finish_lo <- format(Sys.time() + eta_lo, "%H:%M")
      finish_hi <- format(Sys.time() + eta_hi, "%H:%M")
      eta_txt <- paste0("Estimated time remaining: ", format_elapsed(eta_lo), " – ", format_elapsed(eta_hi),
                        " (approx.) · estimated completion: ", finish_lo, " – ", finish_hi)
    } else if (identical(st$state %||% "", "running")) {
      eta_txt <- "Estimating remaining time…"
    } else if (identical(st$state %||% "", "complete")) {
      eta_txt <- paste0("Total time: ", format_elapsed(elapsed))
    }
    phase_txt <- st$phase %||% st$detail %||% "Calculating…"
    state_title <- switch(as.character(st$state %||% "running"),
      complete = "Analysis completed", error = "Analysis error", interrupted = "Analysis interrupted",
      cancelled = "Analysis cancelled", discarded = "Result discarded", "Analysis in progress")
    div(class="analysis-progress",
        tags$b(state_title),
        div(class="analysis-bar", div(class="analysis-fill", style=paste0("width:", round(100*p), "%"))),
        div(class="analysis-detail", paste0(round(100*p), " % · ", phase_txt)),
        if (!identical(phase_txt, st$detail %||% "")) div(class="analysis-detail", st$detail %||% ""),
        div(class="analysis-detail", "Elapsed time: ", textOutput("analysis_elapsed_text", inline=TRUE)),
        div(class="analysis-eta", eta_txt),
        if (isTRUE(rv$analysis_running)) tagList(
          p(class="small-note", "You can continue using RIveR while the calculation runs. If the web session closes or disconnects, the job will continue in the background and can be recovered when the application is reopened."),
          actionButton("cancel_analysis", "Cancel analysis", class="btn-default")
        )
    )
  })

  analysis_idle_or_notify <- function() {
    if (isTRUE(rv$analysis_running)) {
      showNotification("A calculation is already in progress. Wait for it to finish or explicitly cancel it before starting another one.", type="warning", duration=8)
      return(FALSE)
    }
    z <- tryCatch(rilctms_any_running_job(), error = function(e) NULL)
    if (!is.null(z)) {
      rv$recoverable_job <- z
      showNotification("A persistent analysis is already in progress. Resume monitoring it before starting another calculation.", type="warning", duration=10)
      return(FALSE)
    }
    TRUE
  }

  build_analysis_spec <- function() {
    req(rv$prepared)
    design <- reference_design_reactive()
    vmode <- verification_partition_mode()
    defs <- verification_partition_definitions()
    nboot <- if (identical(rv$study_type, "establish")) as.integer(input$bootstrap_n_est %||% 200L) else as.integer(input$bootstrap_n %||% 200L)
    nboot <- max(30L, min(1000L, nboot))
    list(
      dat=rv$prepared$data, study_type=rv$study_type, data_origin=input$data_origin,
      reference_design=design, n_bootstrap=nboot,
      check_partition=identical(rv$study_type, "establish") && "qualitative" %in% names(rv$prepared$data) && length(unique(trimws(as.character(rv$prepared$data$qualitative[!is.na(rv$prepared$data$qualitative) & nzchar(trimws(as.character(rv$prepared$data$qualitative)))])))) >= 2L,
      check_age=identical(rv$study_type, "establish") && "quantitative" %in% names(rv$prepared$data) && length(unique(rv$prepared$data$quantitative[is.finite(rv$prepared$data$quantitative)])) >= 10L,
      qualitative_label=rv$prepared$covariates$qualitative$source %||% "Qualitative variable",
      quantitative_label=rv$prepared$covariates$quantitative$source %||% "Quantitative variable",
      explore_complexity=if (identical(rv$study_type, "establish")) TRUE else isTRUE(input$d6_explore_complexity),
      force_refine=isTRUE(input$d6_confirm_green),
      retained_extreme_values=rv$retained_extreme_values %||% numeric(0),
      verification_mode=vmode, verification_definitions=defs,
      partition_second_data=rv$partition_second_data %||% list(),
      partition_second_sources=rv$partition_second_sources %||% list(),
      raw_name=rv$raw_name %||% "—",
      target_lower=input$target_lower %||% NA_real_, target_upper=input$target_upper %||% NA_real_,
      allow_embedded_second=nzchar(rv$second_raw_name %||% "")
    )
  }

  analysis_spec_signature <- function(spec) {
    f <- tempfile("rilctms_signature_", fileext=".rds")
    on.exit(unlink(f, force=TRUE), add=TRUE)
    saveRDS(spec, f, version=2)
    unname(as.character(tools::md5sum(f)[1]))
  }

  apply_async_completion <- function(completion) {
    completion <- completion %||% list(action="standard", context=list(), go_results=TRUE, source_label="Analysis")
    action <- completion$action %||% "standard"
    ctx <- completion$context %||% list()

    if (identical(action, "outlier_global")) {
      before_main <- ctx$before_main %||% NULL
      outlier_action <- ctx$outlier_action %||% "retain"
      if (identical(outlier_action, "exclude") && !is.null(before_main$ri) && !is.null(rv$main$ri)) {
        impact <- data.frame(
          timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
          n_before = before_main$n, n_after = rv$main$n,
          ri_before = format_lab_interval(before_main$ri, before_main$display_digits %||% 2),
          ri_after = format_lab_interval(rv$main$ri, rv$main$display_digits %||% 2),
          pr_lower_before = before_main$precision_ratio[1], pr_lower_after = rv$main$precision_ratio[1],
          pr_upper_before = before_main$precision_ratio[2], pr_upper_after = rv$main$precision_ratio[2],
          stringsAsFactors = FALSE
        )
        if (is.null(rv$outlier_impacts) || !nrow(rv$outlier_impacts)) rv$outlier_impacts <- impact else rv$outlier_impacts <- rbind(rv$outlier_impacts, impact)
      }
      showNotification(if (identical(outlier_action, "exclude"))
        "Value(s) excluded with full traceability. The RI, CIs, precision, and partitions have been recalculated from scratch."
        else "Value(s) retained with full traceability. The study has been recalculated and the review no longer blocks the recommendation.",
        type="message", duration=12)
    } else if (identical(action, "subgroup_outlier")) {
      showNotification("Decision recorded. The overall RI, subgroup RIs, Lahti, Harris-Boyd, precision, and age assessment have been recalculated.", type="message", duration=12)
    } else if (identical(action, "verification_exclusion")) {
      showNotification("Exclusion documented. The cohort remains incomplete until the necessary replacement subject(s) are added.", type="message", duration=10)
    } else if (identical(action, "replacement")) {
      showNotification("Cohort completed to n=20 and verification recalculated.", type="message", duration=8)
    } else if (identical(action, "partition_second")) {
      showNotification(paste0("Second cohort added for ", ctx$label %||% "the partition", " and verification recalculated."), type="message", duration=8)
    } else if (identical(action, "second_cohort")) {
      showNotification("Second cohort added and verification recalculated.", type="message", duration=8)
    } else {
      showNotification("Analysis completed.", type="message", duration=6)
    }

    if (isTRUE(completion$go_results %||% TRUE)) goto("Results")
    invisible(TRUE)
  }

  launch_async_analysis <- function(go_results = TRUE, source_label = "Analysis",
                                    completion_action = "standard", completion_context = list()) {
    req(rv$prepared)
    if (!analysis_idle_or_notify()) return(invisible(FALSE))
# If this same session had already applied a terminal job and now
# launches a new calculation, the previous job is no longer the active recovery point.
    previous_job <- rv$analysis_job_dir
    if (!is.null(previous_job) && dir.exists(previous_job)) {
      zprev <- tryCatch(rilctms_job_state(previous_job), error=function(e) NULL)
      if (!is.null(zprev) && !identical(zprev$state, "running"))
        try(rilctms_mark_job_consumed(previous_job, "New calculation started from the same session"), silent=TRUE)
    }
    rr <- route_reactive(); rv$route <- rr; rv$quality <- quality_reactive()
    if (!isTRUE(rr$can_run)) {
      showNotification(paste("Cannot run:", paste(rr$blockers, collapse=" ")), type="error", duration=10)
      return(invisible(FALSE))
    }
    if (!requireNamespace("processx", quietly=TRUE) || !requireNamespace("ps", quietly=TRUE)) {
      showNotification("The 'processx' and/or 'ps' packages are missing. Run source(\"install_packages.R\") and restart RIveR.", type="error", duration=12)
      return(invisible(FALSE))
    }

    spec <- build_analysis_spec()
    signature <- analysis_spec_signature(spec)
    job_id <- paste0(format(Sys.time(), "%Y%m%d%H%M%OS3"), "_", sprintf("%06d", sample.int(999999L, 1L)))
    spec$job_id <- job_id
    spec$source_label <- source_label
    completion <- list(action=completion_action, context=completion_context,
                       go_results=isTRUE(go_results), source_label=source_label)

    design <- spec$reference_design
    rv$metadata <- list(
      study_name=input$study_name, analyte=input$analyte, unit=input$unit,
      system=input$system, population=input$population, study_type=rv$study_type,
      data_origin=input$data_origin, target_source=input$target_source,
      reference_design=design, reference_design_label=reference_design_label(design)
    )

# v0.17.0: the job lives in a persistent user directory, not in
# tempdir(). This allows it to be recovered after losing/closing the web session.
    jobs_root <- tryCatch(rilctms_jobs_root(), error=function(e) e)
    if (inherits(jobs_root, "error")) {
      showNotification(paste("The persistent job store could not be prepared:", conditionMessage(jobs_root)), type="error", duration=15)
      return(invisible(FALSE))
    }
    jobdir <- file.path(jobs_root, paste0("job_", job_id))
    dir.create(jobdir, recursive=TRUE, showWarnings=FALSE)
    paths <- rilctms_job_paths(jobdir)
    app_dir <- normalizePath(getwd(), winslash="/", mustWork=TRUE)
    created <- Sys.time()

    manifest <- list(
      job_id=job_id, app_version=APP_VERSION, state="starting", created_at=created,
      source_label=source_label, signature=signature, app_dir=app_dir,
      study_name=input$study_name %||% "", analyte=input$analyte %||% "",
      study_type=rv$study_type, data_origin=input$data_origin %||% "",
      n_bootstrap=spec$n_bootstrap %||% NA_integer_, pid=NA_integer_, process_create_time=NULL
    )
    snapshot <- capture_analysis_snapshot(spec, signature, completion)
    snapshot$rv_state$analysis_iteration <- as.integer(rv$analysis_iteration %||% 0L) + 1L
    saveRDS(spec, paths$job)
    rilctms_job_atomic_save_rds(manifest, paths$manifest)
    rilctms_job_atomic_save_rds(snapshot, paths$snapshot)
    rilctms_job_atomic_save_rds(
      list(state="running", progress=0.01, detail=paste0(source_label, " · starting calculation process"),
           phase=source_label, job_id=job_id, started_at=created, updated_at=created),
      paths$status
    )

    worker_script <- normalizePath(file.path(app_dir, "worker", "persistent_worker_entry.R"),
                                   winslash="/", mustWork=TRUE)
    rscript_bin <- file.path(R.home("bin"),
                             if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")
    if (!file.exists(rscript_bin)) rscript_bin <- file.path(R.home("bin"), "Rscript")
    proc <- tryCatch(processx::process$new(
      command = rscript_bin,
      args = c("--vanilla", worker_script, paths$job, app_dir, paths$status,
               paths$result, paths$error, paths$manifest),
      # v0.17.7: function/argument objects are not serialized to temporary callr files. The worker
# starts from a physical script in the application and receives only persistent paths.
# cleanup=FALSE + supervise=FALSE allow the process to survive GC,
# browser-tab closure, and loss of the Shiny session.
      supervise = FALSE, cleanup = FALSE, cleanup_tree = FALSE,
# On Windows, processx uses !cleanup by default; we set it explicitly so the
# worker is not tied to the Shiny process that created it.
      windows_detached_process = TRUE,
      stdout = paths$stdout, stderr = paths$stderr, wd = app_dir,
      env = c("current",
              OMP_NUM_THREADS = "1", OPENBLAS_NUM_THREADS = "1", MKL_NUM_THREADS = "1",
              VECLIB_MAXIMUM_THREADS = "1", NUMEXPR_NUM_THREADS = "1"),
      windows_hide_window = TRUE
    ), error=function(e) e)
    if (inherits(proc, "error")) {
      unlink(jobdir, recursive=TRUE, force=TRUE)
      showNotification(paste("The calculation process could not be started:", conditionMessage(proc)), type="error", duration=15)
      return(invisible(FALSE))
    }

    pid <- tryCatch(proc$get_pid(), error=function(e) NA_integer_)
    pct <- tryCatch(ps::ps_create_time(ps::ps_handle(pid)), error=function(e) NULL)
    try(rilctms_update_job_manifest(jobdir, state="running", pid=pid, process_create_time=pct,
                                    launched_at=Sys.time()), silent=TRUE)
# We do not store the processx object inside the session. With cleanup=FALSE the worker
# is decoupled from the Shiny lifecycle and is recovered pathway the manifest/PID.
    proc <- NULL

    rv$main <- NULL; rv$partition <- NULL; rv$age <- NULL; rv$final <- NULL
    rv$defer_heavy_diagnostics <- FALSE
    rv$partition_resolution <- NULL; rv$small_sample_resolution <- NULL
    rv$analysis_iteration <- as.integer(rv$analysis_iteration %||% 0L) + 1L
    rv$analysis_running <- TRUE
    rv$analysis_process <- NULL
    rv$analysis_job_dir <- jobdir
    rv$analysis_job_id <- job_id
    rv$analysis_signature <- signature
    rv$analysis_completion <- completion
    rv$analysis_status <- rilctms_safe_read_rds(paths$status, list(state="running", progress=.01, job_id=job_id))
    rv$analysis_status_mtime <- tryCatch(file.info(paths$status)$mtime[1], error=function(e) as.POSIXct(NA))
    rv$analysis_started <- rv$analysis_status$started_at %||% created
    rv$analysis_recovered <- FALSE
    rv$analysis_dead_seen_at <- as.POSIXct(NA)
    rv$recoverable_job <- NULL
    goto("Analysis")
    showNotification(paste0(source_label, " started as a persistent job. If the web session closes, the calculation will continue and can be recovered."), type="message", duration=8)
    invisible(TRUE)
  }

  observeEvent(input$run_analysis, launch_async_analysis())

  observeEvent(input$recover_job, {
    if (isTRUE(rv$recovery_loading)) return()
    z <- rv$recoverable_job %||% tryCatch(rilctms_latest_recoverable_job(), error=function(e) NULL)
    if (is.null(z) || is.null(z$job_dir) || !dir.exists(z$job_dir)) {
      showNotification("No recoverable job was found.", type="warning")
      rv$recoverable_job <- NULL
      return()
    }
    jid <- as.character(z$manifest$job_id %||% z$status$job_id %||% "")
    rv$recovery_loading <- TRUE
    rv$recovery_job_dir <- z$job_dir
    rv$recovery_job_id <- jid
    rv$recovery_expected_state <- z$state %||% "unknown"
    rv$recovery_error <- NULL
    rv$recoverable_job <- NULL
    recovery_loader$invoke(z$job_dir, jid)
    showNotification("Recovery started in the background. The web session remains available.", type="message", duration=6)
  })

  observe({
    if (!isTRUE(rv$recovery_loading)) return()
    st_task <- recovery_loader$status()
    if (identical(st_task, "initial") || identical(st_task, "running")) return()

    if (identical(st_task, "error")) {
      er <- tryCatch(recovery_loader$result(), error=function(e) e)
      rv$recovery_loading <- FALSE
      rv$recovery_error <- if (inherits(er, "error")) conditionMessage(er) else "Unknown error during recovery."
      rv$recoverable_job <- tryCatch(rilctms_latest_recoverable_job(), error=function(e) NULL)
      showNotification(paste("The job could not be recovered:", rv$recovery_error), type="error", duration=15)
      return()
    }

    bundle <- tryCatch(recovery_loader$result(), error=function(e) e)
    if (inherits(bundle, "error") || is.null(bundle$snapshot)) {
      rv$recovery_loading <- FALSE
      rv$recoverable_job <- tryCatch(rilctms_latest_recoverable_job(), error=function(e) NULL)
      showNotification("The job exists, but the study snapshot could not be recovered.", type="error", duration=12)
      return()
    }

    jobdir <- rv$recovery_job_dir
    jid <- as.character(rv$recovery_job_id %||% bundle$job_id %||% "")
    expected_state <- as.character(rv$recovery_expected_state %||% "unknown")
    apply_key <- paste0(jid, "::", expected_state)
    if (identical(rv$recovery_applied_key %||% "", apply_key)) {
      rv$recovery_loading <- FALSE
      return()
    }

# The entire bundle is applied within the same flush; Shiny does not render
# intermediate states. Deserialization and branding transformation have already been
# completed inside ExtendedTask.
    if (!isTRUE(restore_analysis_snapshot(jobdir, snap=bundle$snapshot, prebranded=isTRUE(bundle$prebranded)))) {
      rv$recovery_loading <- FALSE
      rv$recoverable_job <- tryCatch(rilctms_latest_recoverable_job(), error=function(e) NULL)
      showNotification("The recovered snapshot could not be applied.", type="error", duration=12)
      return()
    }

    rv$analysis_job_dir <- jobdir
    rv$analysis_job_id <- jid
    rv$analysis_status <- bundle$status %||% list()
    rv$analysis_status_mtime <- tryCatch(file.info(file.path(jobdir,"status.rds"))$mtime[1], error=function(e) as.POSIXct(NA))
    rv$analysis_started <- bundle$status$started_at %||% bundle$manifest$created_at %||% Sys.time()
    rv$analysis_process <- NULL
    rv$analysis_last_liveness_check <- as.POSIXct(NA)
    rv$analysis_dead_seen_at <- as.POSIXct(NA)
    rv$recovery_applied_key <- apply_key

    has_result <- !is.null(bundle$result)
    if (has_result) {
      rv$analysis_running <- FALSE
      rv$defer_heavy_diagnostics <- TRUE
      ok <- isTRUE(apply_persistent_job_result(jobdir, recovered=TRUE, ans=bundle$result, defer_completion=TRUE, prebranded=isTRUE(bundle$prebranded)))
      rv$recovery_loading <- FALSE
      if (!ok) {
        rv$recoverable_job <- tryCatch(rilctms_latest_recoverable_job(), error=function(e) NULL)
        showNotification("The result was found but could not be applied.", type="error", duration=12)
      } else {
        showNotification("Result recovered. Tables and the decision have been restored; resource-intensive plots are loaded only if requested.", type="message", duration=10)
      }
    } else if (identical(expected_state, "running")) {
      rv$analysis_running <- TRUE
      rv$recovery_loading <- FALSE
      goto("Analysis")
      showNotification("Monitoring recovered. The calculation was already in progress and has not been restarted.", type="message", duration=8)
    } else {
      rv$analysis_running <- FALSE
      rv$recovery_loading <- FALSE
      goto("Analysis")
      msg <- bundle$error$message %||% bundle$status$detail %||% "The persistent job contains no recoverable result."
      rv$analysis_status <- modifyList(bundle$status %||% list(), list(state=expected_state, detail=msg))
      rv$recoverable_job <- tryCatch(rilctms_latest_recoverable_job(), error=function(e) NULL)
      showNotification(msg, type=if (expected_state %in% c("error","interrupted")) "error" else "message", duration=12)
    }
  })

  observeEvent(input$dismiss_recovery, {
    z <- rv$recoverable_job
    if (!is.null(z$job_dir)) try(rilctms_dismiss_job(z$job_dir), silent=TRUE)
    rv$recoverable_job <- NULL
  })

  observeEvent(input$cancel_analysis, {
    if (!isTRUE(rv$analysis_running) || is.null(rv$analysis_job_dir)) return()
    showModal(modalDialog(
      title = "Cancel the analysis?",
      "This action will explicitly stop the persistent calculation process. It cannot be resumed.",
      footer = tagList(modalButton("Continue calculating"), actionButton("confirm_cancel_analysis", "Yes, cancel", class="btn-danger")),
      easyClose = FALSE
    ))
  })

  observeEvent(input$confirm_cancel_analysis, {
    removeModal()
    jobdir <- rv$analysis_job_dir
    if (is.null(jobdir)) return()
    ok <- tryCatch(rilctms_cancel_job(jobdir), error=function(e) FALSE)
    if (isTRUE(ok)) {
      rilctms_mark_job_consumed(jobdir, "Analysis explicitly cancelled")
      rv$analysis_running <- FALSE
      rv$analysis_process <- NULL
      rv$analysis_status <- modifyList(rv$analysis_status %||% list(), list(state="cancelled", detail="Analysis cancelled by the user"))
      rv$analysis_status_mtime <- as.POSIXct(NA)
      rv$analysis_recovered <- FALSE
      rv$analysis_dead_seen_at <- as.POSIXct(NA)
      rv$recoverable_job <- tryCatch(rilctms_latest_recoverable_job(), error=function(e) NULL)
      showNotification("Analysis cancelled.", type="message", duration=6)
    } else {
      showNotification("Process termination could not be confirmed. The job remains under monitoring.", type="error", duration=10)
    }
  })

  apply_persistent_job_result <- function(jobdir, recovered = isTRUE(rv$analysis_recovered), ans = NULL, defer_completion = FALSE, prebranded = FALSE) {
    paths <- rilctms_job_paths(jobdir)
    job_id <- rv$analysis_job_id %||% rilctms_job_manifest(jobdir)$job_id %||% ""
    if (is.null(ans)) ans <- tryCatch(readRDS(paths$result), error=function(e) e)
    if (inherits(ans, "error")) return(FALSE)
    if (!identical(as.character(ans$job_id %||% ""), as.character(job_id))) return(FALSE)

# In an uninterrupted session, retain the historical protection
# against stale results if the user has changed the data/configuration.
# During recovery, the original snapshot is authoritative and is restored before
# applying the result, so it is not mixed with a new study.
    if (!isTRUE(recovered)) {
      current_signature <- tryCatch(analysis_spec_signature(build_analysis_spec()), error=function(e) NA_character_)
      if (!identical(current_signature, rv$analysis_signature %||% "")) {
        rv$analysis_running <- FALSE; rv$analysis_process <- NULL
        rv$analysis_status_mtime <- as.POSIXct(NA)
        rv$analysis_status <- list(state="discarded", progress=1, detail="Result discarded because the study changed during calculation.", job_id=job_id)
        rilctms_mark_job_consumed(jobdir, "Result discarded due to context change")
        showNotification("The result was not applied because the data or study configuration changed during calculation. Run the analysis again using the current state.", type="warning", duration=15)
        return(FALSE)
      }
    }

    if (isTRUE(prebranded)) {
      rv$main <- ans$main; rv$partition <- ans$partition; rv$age <- ans$age; rv$final <- ans$final
    } else {
      rv$main <- brand_visible_object(ans$main); rv$partition <- brand_visible_object(ans$partition); rv$age <- brand_visible_object(ans$age); rv$final <- brand_visible_object(ans$final)
    }
    if (identical(rv$main$type %||% "", "direct_verification")) {
      rv$main$cohort_exclusions <- rv$verification_exclusions %||% data.frame()
      rv$main$replacement_sources <- rv$replacement_sources %||% character(0)
      rv$main$cohort_review_policy <- "An observation may be excluded from direct verification only for a documented reason independent of its value. A statistically extreme value alone is retained and counted against the candidate limit/RI."
      if (!isTRUE(rv$main$partitioned)) {
        second_src <- rv$second_raw_name %||% ""
        if (!nzchar(second_src) && (rv$main$second_n %||% 0L) > 0L) second_src <- rv$raw_name %||% ""
        rv$main$cohort_sources <- c(rv$raw_name %||% "—", second_src)
      }
    }
    completion <- rv$analysis_completion %||% list(action="standard", go_results=TRUE)
    rv$analysis_running <- FALSE
    rv$analysis_status_mtime <- as.POSIXct(NA)
    rv$analysis_status <- list(state="complete", progress=1, detail="Analysis completed", phase=completion$source_label %||% "Analysis", job_id=job_id)
    rv$analysis_process <- NULL
    rv$analysis_recovered <- FALSE
    rv$analysis_last_liveness_check <- as.POSIXct(NA)
    rv$analysis_dead_seen_at <- as.POSIXct(NA)
    mark_job_handled_in_session(job_id)
    try(rilctms_update_job_manifest(jobdir, state = "complete", last_applied_at = Sys.time(),
                                    last_applied_version = APP_VERSION, last_applied_recovered = isTRUE(recovered)), silent = TRUE)
    rv$recoverable_job <- tryCatch(rilctms_latest_recoverable_job(), error=function(e) NULL)
    if (isTRUE(defer_completion)) {
# First allow Shiny to publish the recovered state and rehydrate inputs.
# Navigation to Results occurs two flushes later, preventing recovery and
# rendering of the entire screen from competing at the same time.
      session$onFlushed(function() {
        session$onFlushed(function() apply_async_completion(completion), once=TRUE)
      }, once=TRUE)
    } else {
      apply_async_completion(completion)
    }
    TRUE
  }

  observe({
    if (!isTRUE(rv$analysis_running)) return()
    invalidateLater(2500, session)
    jobdir <- rv$analysis_job_dir
    job_id <- rv$analysis_job_id %||% ""
    if (is.null(jobdir) || !dir.exists(jobdir)) return()
    paths <- rilctms_job_paths(jobdir)

    if (file.exists(paths$status)) {
      mt <- tryCatch(file.info(paths$status)$mtime[1], error=function(e) as.POSIXct(NA))
      last_mt <- isolate(rv$analysis_status_mtime)
      changed <- is.na(last_mt) || (!is.na(mt) && !identical(as.numeric(mt), as.numeric(last_mt)))
      if (isTRUE(changed)) {
        st <- tryCatch(readRDS(paths$status), error=function(e) NULL)
        if (!is.null(st) && identical(as.character(st$job_id %||% job_id), as.character(job_id))) {
          current_status <- isolate(rv$analysis_status)
          rv$analysis_status_mtime <- mt
          if (!identical(st, current_status)) rv$analysis_status <- st
        }
      }
    }

    if (file.exists(paths$result)) {
      apply_persistent_job_result(jobdir, recovered=isTRUE(rv$analysis_recovered))
      return()
    }

    if (file.exists(paths$error)) {
      er <- tryCatch(readRDS(paths$error), error=function(e) NULL)
      msg <- er$message %||% "The calculation process ended with an error."
      stage <- er$stage %||% NULL
      if (!is.null(stage) && nzchar(as.character(stage)[1]) &&
          !grepl(as.character(stage)[1], msg, fixed=TRUE)) {
        msg <- paste0(as.character(stage)[1], ": ", msg)
      }
      rv$analysis_running <- FALSE; rv$analysis_process <- NULL
      rv$analysis_status_mtime <- as.POSIXct(NA)
      rv$analysis_status <- list(state="error", progress=1, detail=msg, job_id=job_id)
      mark_job_handled_in_session(job_id)
      try(rilctms_update_job_manifest(jobdir, last_reviewed_at = Sys.time(), last_reviewed_version = APP_VERSION), silent = TRUE)
      rv$recoverable_job <- tryCatch(rilctms_latest_recoverable_job(), error=function(e) NULL)
      showNotification(paste("The analysis could not be completed:", msg), type="error", duration=15)
      return()
    }

# Process liveness is checked only every 30 s. We do not depend on a processx object from
# the session: PID + process creation time in the manifest identify the worker.
    now <- Sys.time()
    last_live <- isolate(rv$analysis_last_liveness_check)
    due <- is.na(last_live) || as.numeric(difftime(now, last_live, units="secs")) >= 30
    if (isTRUE(due)) {
      rv$analysis_last_liveness_check <- now
      z <- tryCatch(rilctms_job_state(jobdir), error=function(e) NULL)
      if (!is.null(z) && identical(z$state, "interrupted")) {
# Avoid a race at worker termination: the process may disappear a few
# moments before result.rds/error.rds becomes visible. Require two
# separate dead-process checks before declaring interruption.
        seen <- isolate(rv$analysis_dead_seen_at)
        if (is.na(seen)) {
          rv$analysis_dead_seen_at <- now
          return()
        }
        msg <- "The calculation process was interrupted without generating a result."
        if (file.exists(paths$stderr)) {
          zz <- tail(readLines(paths$stderr, warn=FALSE), 6)
          if (length(zz)) msg <- paste(c(msg, zz), collapse=" ")
        }
        rv$analysis_running <- FALSE; rv$analysis_process <- NULL
        rv$analysis_status_mtime <- as.POSIXct(NA)
        rv$analysis_status <- list(state="interrupted", progress=z$status$progress %||% 1, detail=msg, job_id=job_id)
        mark_job_handled_in_session(job_id)
        try(rilctms_update_job_manifest(jobdir, last_reviewed_at = Sys.time(), last_reviewed_version = APP_VERSION), silent = TRUE)
        rv$recoverable_job <- tryCatch(rilctms_latest_recoverable_job(), error=function(e) NULL)
        showNotification(msg, type="error", duration=15)
      } else if (!is.null(z) && identical(z$state, "cancelled")) {
        rv$analysis_running <- FALSE; rv$analysis_process <- NULL
        rv$analysis_status <- list(state="cancelled", progress=z$status$progress %||% 1, detail="Analysis cancelled by the user.", job_id=job_id)
      } else if (!is.null(z) && identical(z$state, "running")) {
        rv$analysis_dead_seen_at <- as.POSIXct(NA)
      }
    }
  })

  observeEvent(input$to_results, {
    if (isTRUE(rv$analysis_running)) showNotification("The analysis is still in progress. You can follow its progress on this screen.", type="message")
    else if (is.null(rv$main)) showNotification("Run the analysis first.", type="warning") else goto("Results")
  })

  observeEvent(input$to_recommendation, {
    if (is.null(rv$main)) {
      showNotification("Run the analysis first.", type="warning")
      return()
    }
    rv$final <- brand_visible_object(compose_final_recommendation(rv$main, rv$partition, rv$age, rv$partition_resolution, rv$small_sample_resolution))
    goto("Recommendation")
  })

  output$main_status <- renderUI({
    if (is.null(rv$main)) return(status_html("grey", "No analysis", "Run the analysis in the previous step."))
    status_html(rv$main$status %||% "grey", status_label(rv$main$status %||% "grey"), rv$main$recommendation)
  })

  output$next_action <- renderUI({
    req(rv$main)
    action <- rv$final$action %||% rv$main$action %||% rv$main$recommendation %||% "Review the results before continuing."
    st <- rv$final$status %||% rv$main$status %||% "grey"
    title <- if (st == "green") "What to do now" else if (st == "yellow") "Recommended next step" else "Required action"
    status_html(st, title, action)
  })

  output$indirect_methods_table <- renderTable({
    req(rv$main)
    tab <- if (identical(rv$main$module %||% "", "D7")) d7_methods_table(rv$main) else indirect_methods_table(rv$main)
    if (is.null(tab)) return(data.frame(Message = "No indirect-method table is available."))
    num <- intersect(c("LRL", "Median", "URL"), names(tab))
    for (nm in num) tab[[nm]] <- ifelse(is.finite(tab[[nm]]), signif(tab[[nm]], 6), NA)
    tab
  }, striped = TRUE, spacing = "s")

  output$d7_summary_table <- renderTable({
    req(rv$main)
    d7_summary_table(rv$main)
  }, striped = TRUE, spacing = "s")

  output$d7_recovered_diag_control <- renderUI({
    req(rv$main)
    if (!isTRUE(rv$defer_heavy_diagnostics) || !identical(rv$main$module %||% "", "D7")) return(NULL)
    div(class="recovery-loading",
        tags$b("refineR diagnostic plot not yet loaded"),
        p(class="small-note", "To keep recovery immediate, RIveR does not automatically regenerate resource-intensive model plots. The tables, limits, and decision have already been recovered."),
        actionButton("load_recovered_diagnostics", "Load diagnostic plots"))
  })

  observeEvent(input$load_recovered_diagnostics, {
    rv$defer_heavy_diagnostics <- FALSE
    showNotification("Loading diagnostic plots for the recovered result.", type="message", duration=5)
  })

  output$d7_refine_plot <- renderPlot({
    req(rv$main)
    if (isTRUE(rv$defer_heavy_diagnostics)) {
      diagnostic_empty_plot("Recovered result · diagnostic plot pending loading")
      return(invisible(NULL))
    }
    if (!identical(rv$main$module %||% "", "D7") || is.null(rv$main$refine$fit)) return(invisible(NULL))
    pm <- rv$main$refine$point_method %||% "fullDataEst"
    design <- normalize_reference_design(rv$main$reference_design %||% rv$metadata$reference_design)
    tryCatch(
      plot(rv$main$refine$fit, RIperc = as.numeric(design$pair_percentiles), showCI = TRUE,
           showPathol = TRUE, showBSModels = FALSE, pointEst = pm,
           xlab = paste(rv$metadata$analyte %||% "Result", rv$metadata$unit %||% ""),
           title = "D7 · refineR fit"),
      error = function(e) diagnostic_empty_plot(paste("refineR could not be plotted:", conditionMessage(e)))
    )
  })

  output$d7_partition_title <- renderUI({
    req(rv$main)
    if (!identical(rv$main$module %||% "", "D7") || is.null(rv$main$partition$table)) return(NULL)
    h5("Qualitative variable · estimates by category")
  })
  output$d7_partition_table <- renderTable({
    req(rv$main)
    z <- rv$main$partition$table %||% NULL
    if (is.null(z)) return(NULL)
    z
  }, striped = TRUE, spacing = "s")

  output$d7_partition_criteria_table <- renderTable({
    req(rv$main)
    rv$main$partition$criteria_table %||% NULL
  }, striped = TRUE, spacing = "s")

  output$d7_partition_methods_table <- renderTable({
    req(rv$main)
    z <- rv$main$partition$methods_table %||% NULL
    if (is.null(z)) return(NULL)
    for (nm in intersect(c("LRL","URL"), names(z))) z[[nm]] <- ifelse(is.finite(z[[nm]]), signif(z[[nm]], 6), NA)
    z
  }, striped = TRUE, spacing = "s")

  output$d7_lahti_table <- renderTable({
    req(rv$main)
    z <- rv$main$partition$lahti$table %||% NULL
    if (is.null(z)) return(NULL)
    if ("Modeled proportion outside overall RI" %in% names(z)) z[["Modeled proportion outside overall RI"]] <- paste0(round(100*z[["Modeled proportion outside overall RI"]], 2), " %")
    z$Status <- vapply(z$Status, function(st) switch(st, green="Compatible with common RI", yellow="Marginal", red="Supports partitioning", "Not evaluable"), character(1))
    z
  }, striped = TRUE, spacing = "s")

  output$d7_lahti_distance_table <- renderTable({
    req(rv$main)
    z <- rv$main$partition$lahti$distance_table %||% NULL
    if (is.null(z)) return(NULL)
    z$Status <- vapply(z$Status, function(st) switch(st, green="Compatible with common RI", yellow="Marginal", red="Supports partitioning", "Not evaluable"), character(1))
    z
  }, striped = TRUE, spacing = "s")

  output$d7_partition_plots_title <- renderUI({
    req(rv$main)
    p <- rv$main$partition
    if (!identical(p$decision %||% "", "partition") || length(p$substudies %||% list()) < 2L) return(NULL)
    h5("refineR diagnostic by candidate partition")
  })

  d7_partition_refine_plot <- function(i) {
    renderPlot({
      req(rv$main)
      if (isTRUE(rv$defer_heavy_diagnostics)) {
        diagnostic_empty_plot("Recovered result · diagnostic plot pending loading")
        return(invisible(NULL))
      }
      p <- rv$main$partition
      subs <- p$substudies %||% list()
      if (length(subs) < i) return(invisible(NULL))
      z <- subs[[i]]
      if (is.null(z) || is.null(z$refine$fit)) return(invisible(NULL))
      pm <- z$refine$point_method %||% "fullDataEst"
      lab <- (p$groups %||% c("Group 1","Group 2"))[i]
      design <- normalize_reference_design(z$reference_design %||% rv$main$reference_design)
      tryCatch(plot(z$refine$fit, RIperc=as.numeric(design$pair_percentiles), showCI=TRUE, showPathol=TRUE,
                    showBSModels=FALSE, pointEst=pm,
                    xlab=paste(rv$metadata$analyte %||% "Result", rv$metadata$unit %||% ""),
                    title=paste0("D7 · ", lab, " · refineR")),
               error=function(e) diagnostic_empty_plot(conditionMessage(e)))
    })
  }
  output$d7_partition_refine_plot1 <- d7_partition_refine_plot(1)
  output$d7_partition_refine_plot2 <- d7_partition_refine_plot(2)

  output$d7_age_title <- renderUI({
    req(rv$main)
    if (!identical(rv$main$module %||% "", "D7") || is.null(rv$main$age$bins)) return(NULL)
    h5(paste0("Dependence screening · ", quantitative_label(rv$main$age)))
  })
  output$d7_age_table <- renderTable({
    req(rv$main)
    z <- rv$main$age$bins %||% NULL
    if (is.null(z)) return(NULL)
    qlab <- quantitative_label(rv$main$age)
    if (ncol(z) >= 3L) names(z)[1:3] <- c(paste0("Representative ", qlab), "n", "Median")
    z
  }, striped = TRUE, spacing = "s")

  output$d7_age_model_title <- renderUI({
    req(rv$main)
    m <- rv$main$age$model %||% NULL
    if (is.null(m) || !isTRUE(m$available)) return(NULL)
    design <- normalize_reference_design(m$reference_design %||% rv$main$reference_design)
    qlab <- quantitative_label(rv$main$age)
    candidate <- d7_continuous_model_is_candidate(m)
    ttl <- if (candidate) {
      if (identical(design$tail, "two_sided")) paste0("D7 · Candidate continuous RI by ", qlab) else paste0("D7 · ", reference_limit_name(design), " candidate continuous by ", qlab)
    } else paste0("D7 · Continuous model under review by ", qlab)
    note <- if (candidate) m$note %||% "" else m$message %||% "The curve was calculated for diagnostic purposes, but it did not reach candidate status and does not generate an application table or an LIS implementation proposal."
    tagList(h5(ttl), p(class="small-note", note))
  })
  output$d7_age_model_table <- renderTable({
    req(rv$main)
    m <- rv$main$age$model %||% NULL
    if (!d7_continuous_model_is_candidate(m)) return(NULL)
    z <- m$table %||% NULL
    if (is.null(z)) return(NULL)
    design <- normalize_reference_design(rv$main$age$model$reference_design %||% rv$main$reference_design)
    if (!isTRUE(design$active[["lower"]]) && "LRL" %in% names(z)) z$LRL <- NULL
    if (!isTRUE(design$active[["upper"]]) && "URL" %in% names(z)) z$URL <- NULL
    xcols <- setdiff(names(z), c("LRL", "P50", "URL"))
    for (nm in intersect(c(xcols[1], "LRL", "P50", "URL"), names(z))) z[[nm]] <- signif(z[[nm]], 6)
    z
  }, striped = TRUE, spacing = "s")


  d7_application_reactive <- reactive({
    req(rv$main)
    m <- rv$main$age$model %||% NULL
    if (!d7_continuous_model_is_candidate(m)) return(NULL)
    z <- d7_continuous_application_table(m, age_like = quantitative_age_like(rv$main$age))
    if (is.null(z)) return(NULL)
    design <- normalize_reference_design(m$reference_design %||% rv$main$reference_design)
    if (!isTRUE(design$active[["lower"]]) && "LRL" %in% names(z)) z$LRL <- NULL
    if (!isTRUE(design$active[["upper"]]) && "URL" %in% names(z)) z$URL <- NULL
    for (nm in names(z)) if (is.numeric(z[[nm]])) z[[nm]] <- signif(z[[nm]], 6)
    z
  })

  d7_sil_reactive <- reactive({
    req(rv$main)
    m <- rv$main$age$model %||% NULL
    if (!d7_continuous_model_is_candidate(m)) return(NULL)
    d7_continuous_operational_discretization(m, age_like = quantitative_age_like(rv$main$age), tolerance = 0.10, max_groups = 10L)
  })

  output$d7_application_title <- renderUI({
    z <- d7_application_reactive()
    if (is.null(z) || !nrow(z)) return(NULL)
    qlab <- quantitative_label(rv$main$age)
    tagList(
      h5(paste0("Continuous model values by ", qlab, " · application table")),
      p(class="small-note", if (quantitative_age_like(rv$main$age))
        "Each row is the value predicted by the continuous curve at that age; it is NOT an age range. For intermediate ages, the continuous curve takes precedence."
        else "Each row is a point on the continuous curve; it does NOT define categories or ranges. For intermediate values, the continuous curve takes precedence.")
    )
  })
  output$d7_application_table <- renderTable({
    d7_application_reactive()
  }, striped = TRUE, spacing = "s")

  output$d7_sil_title <- renderUI({
    a <- d7_sil_reactive()
    if (is.null(a) || !isTRUE(a$ok)) return(NULL)
    qlab <- quantitative_label(rv$main$age)
    tagList(
      h5(paste0("Non-overlapping operational proposal for the LIS · ", qlab)),
      p(class="small-note", a$recommendation %||% ""),
      p(class="small-note", tags$strong("Important: "),
        "these ranges are an operational approximation of the continuous curve; they are not RIs established independently within each range.")
    )
  })
  output$d7_sil_table <- renderTable({
    a <- d7_sil_reactive()
    if (is.null(a) || !isTRUE(a$ok)) return(NULL)
    d7_continuous_operational_display_table(a)
  }, striped = TRUE, spacing = "s")
  output$d7_sil_tradeoff_table <- renderTable({
    a <- d7_sil_reactive()
    if (is.null(a) || !isTRUE(a$ok)) return(NULL)
    d7_continuous_tradeoff_display_table(a)
  }, striped = TRUE, spacing = "s")

  output$d7_window_title <- renderUI({
    req(rv$main)
    z <- rv$main$age$model$windows %||% NULL
    if (is.null(z) || !is.data.frame(z) || !nrow(z)) return(NULL)
    tagList(
      h5("Local calculation windows — NOT application intervals"),
      p(class="small-note", "The windows intentionally overlap because they are used to estimate and locally validate the continuous refineR/reflimR curve. They must not be used as age ranges or clinical intervals.")
    )
  })
  output$d7_age_window_table <- renderTable({
    req(rv$main)
    z <- rv$main$age$model$windows %||% NULL
    if (is.null(z)) return(NULL)
    design <- normalize_reference_design(rv$main$age$model$reference_design %||% rv$main$reference_design)
    if (!identical(design$tail, "two_sided")) {
      if ("LRL reflimR" %in% names(z)) names(z)[names(z)=="LRL reflimR"] <- "P2.5 reflimR (descriptive)"
      if ("URL reflimR" %in% names(z)) names(z)[names(z)=="URL reflimR"] <- "P97.5 reflimR (descriptive)"
      if ("Agreement" %in% names(z)) z$Agreement <- "Not applicable to P5/P95"
      if (!isTRUE(design$active[["lower"]]) && "LRL" %in% names(z)) z$LRL <- NULL
      if (!isTRUE(design$active[["upper"]]) && "URL" %in% names(z)) z$URL <- NULL
    } else if ("Agreement" %in% names(z)) {
      z$Agreement <- vapply(z$Agreement, function(st) switch(st, green="Favorable", yellow="Intermediate", red="Unfavorable", "Not evaluable"), character(1))
    }
    if ("Non-pathological fraction" %in% names(z)) z[["Non-pathological fraction"]] <- ifelse(is.finite(z[["Non-pathological fraction"]]), paste0(round(100*z[["Non-pathological fraction"]],1), " %"), "—")
    z
  }, striped = TRUE, spacing = "s")
  output$d7_age_curve_plot <- renderPlot({
    req(rv$main)
    z <- rv$main$age$model$curve %||% NULL
    if (is.null(z) || !nrow(z)) return(invisible(NULL))
    design <- normalize_reference_design(rv$main$age$model$reference_design %||% rv$main$reference_design)
    ylab <- paste(rv$metadata$analyte %||% "Result", rv$metadata$unit %||% "")
    qlab <- quantitative_label(rv$main$age)
    xcol <- setdiff(names(z), c("LRL", "P50", "URL"))[1]
    if (is.na(xcol) || !nzchar(xcol)) return(invisible(NULL))
    xx <- z[[xcol]]
    candidate <- d7_continuous_model_is_candidate(rv$main$age$model %||% NULL)
    if (identical(design$tail, "two_sided")) {
      yr <- range(c(z$LRL,z$P50,z$URL), finite=TRUE)
      plot(xx, z$P50, type="l", ylim=yr, xlab=qlab, ylab=ylab, main=paste0("D7 · ", if (candidate) "Candidate continuous RI" else "continuous curve under review", " by ", qlab))
      lines(xx, z$LRL, lty=2); lines(xx, z$URL, lty=2)
      legend("topleft", legend=c("P50","LRL","URL"), lty=c(1,2,2), bty="n")
    } else {
      side <- if (isTRUE(design$active[["lower"]])) "LRL" else "URL"
      vals <- z[[side]]
      plot(xx, vals, type="l", xlab=qlab, ylab=ylab,
           main=paste0("D7 · ", reference_limit_name(design), " according to ", qlab))
      legend("topleft", legend=reference_percentile_label(design$active_percentiles[[1]]), lty=1, bty="n")
    }
  })

  output$d7_mclust_title <- renderUI({
    req(rv$main)
    if (!identical(rv$main$module %||% "", "D7") || is.null(rv$main$exploration$mclust$component_table)) return(NULL)
    h5("mclust exploration · statistical components")
  })
  output$d7_mclust_table <- renderTable({
    req(rv$main)
    z <- rv$main$exploration$mclust$component_table %||% NULL
    if (is.null(z)) return(NULL)
    if ("Proportion" %in% names(z)) z$Proportion <- ifelse(is.finite(z$Proportion), paste0(round(100*z$Proportion, 1), " %"), "—")
    z
  }, striped = TRUE, spacing = "s")

  output$d7_environment_table <- renderTable({
    req(rv$main)
    d7_environment_table(rv$main)
  }, striped = TRUE, spacing = "s")

  output$verification_exclusion_table <- renderTable({
    z <- rv$verification_exclusions
    if (!is.data.frame(z) || !nrow(z)) return(NULL)
    out <- z
    keep <- intersect(c("patient_id", "value", "reason"), names(out))
    out <- out[, keep, drop = FALSE]
    names(out) <- c("Individual", "Result", "Justification")[seq_along(keep)]
    out
  }, striped = TRUE, bordered = FALSE, spacing = "s")

  output$verification_cohort_review_panel <- renderUI({
    req(rv$main, rv$prepared)
    if (!identical(rv$study_type, "verify") || !identical(input$data_origin, "direct") ||
        !identical(rv$main$type %||% "", "direct_verification") || isTRUE(rv$main$partitioned)) return(NULL)
    if ((rv$main$second_n %||% 0L) > 0L || nrow(rv$prepared$data) > 20L) return(NULL)

    dat <- rv$prepared$data
    if (!nrow(dat)) return(NULL)
    rid <- as.character(dat$.ri_row_id %||% seq_len(nrow(dat)))
    pid <- if ("patient_id" %in% names(dat)) trimws(as.character(dat$patient_id)) else rep("", nrow(dat))
    lab_id <- ifelse(nzchar(pid), pid, paste0("row ", rid))
    labs <- paste0(lab_id, " · result ", format_lab_number(dat$value, infer_decimal_places(dat$value)))
    choices <- rid; names(choices) <- labs

    div(class = "panel-card",
      h4("Verification-cohort suitability review"),
      p("This review is not an aberrant-value removal procedure. An individual should only be excluded when there is a documented reason independent on the analytical result."),
      p(class = "small-note", "Examples: failure to meet a predefined selection criterion, documented preanalytical incident, incorrect identification, or another documented cause. A statistically extreme value alone must be retained and counted against the candidate RI."),
      if (is.data.frame(rv$verification_exclusions) && nrow(rv$verification_exclusions)) tagList(
        h5("Documented exclusions"), tableOutput("verification_exclusion_table")
      ),
      selectInput("verify_exclude_row", "Individual to exclude for a documented reason", choices = choices),
      textAreaInput("verify_exclusion_reason", "Justification for exclusion", rows = 2,
                    placeholder = "Describe the reason independent on the analytical value"),
      checkboxInput("verify_exclusion_confirm",
                    "I confirm that the exclusion is based on a documented reason and not solely on the result being extreme or outside the RI", FALSE),
      actionButton("apply_verification_exclusion", "Apply justified exclusion and recalculate", class = "btn-primary")
    )
  })

  observeEvent(input$apply_verification_exclusion, {
    req(rv$main, rv$prepared, input$verify_exclude_row)
    if (!analysis_idle_or_notify()) return()
    if (!identical(rv$study_type, "verify") || !identical(input$data_origin, "direct") ||
        isTRUE(rv$main$partitioned) || (rv$main$second_n %||% 0L) > 0L) {
      showNotification("In this version, the suitability review applies to the initial cohort of a single RI before a second cohort is added.", type = "warning", duration = 10)
      return()
    }
    reason <- trimws(as.character(input$verify_exclusion_reason %||% ""))
    if (!isTRUE(input$verify_exclusion_confirm) || nchar(reason) < 5L) {
      showNotification("The reason must be documented and it must be confirmed that this is not merely an extreme value or a value outside the RI.", type = "warning", duration = 10)
      return()
    }
    dat <- rv$prepared$data
    rid <- as.character(dat$.ri_row_id %||% seq_len(nrow(dat)))
    idx <- which(rid == as.character(input$verify_exclude_row))[1]
    if (!length(idx) || is.na(idx)) {
      showNotification("The selected individual could not be identified.", type = "error")
      return()
    }
    if (is.null(rv$verification_exclusion_baseline)) rv$verification_exclusion_baseline <- dat
    row <- dat[idx, , drop = FALSE]
    rec <- data.frame(
      row_id = as.character(row$.ri_row_id %||% idx),
      patient_id = if ("patient_id" %in% names(row)) as.character(row$patient_id[1] %||% "") else paste0("row ", row$.ri_row_id[1] %||% idx),
      value = as.numeric(row$value[1]),
      reason = reason,
      stringsAsFactors = FALSE
    )
    rv$verification_exclusions <- rbind(rv$verification_exclusions, rec)
    rv$prepared$data <- dat[-idx, , drop = FALSE]
    refresh_verification_audit()
    rv$main <- NULL; rv$partition <- NULL; rv$age <- NULL; rv$final <- NULL
    updateCheckboxInput(session, "verify_exclusion_confirm", value = FALSE)
    updateTextAreaInput(session, "verify_exclusion_reason", value = "")
    launch_async_analysis(go_results = TRUE, source_label = "Cohort suitability review",
                          completion_action = "verification_exclusion")
  })

  output$replacement_panel <- renderUI({
    req(rv$main, rv$prepared)
    if (!identical(rv$study_type, "verify") || !identical(input$data_origin, "direct") ||
        !identical(rv$main$type %||% "", "direct_verification") || isTRUE(rv$main$partitioned)) return(NULL)
    ncur <- nrow(rv$prepared$data)
    if (ncur >= 20L || ncur < 1L || !identical(rv$main$decision %||% "", "NOT EVALUABLE")) return(NULL)
    missing <- 20L - ncur
    has_excl <- is.data.frame(rv$verification_exclusions) && nrow(rv$verification_exclusions) > 0L
    title <- if (has_excl) "Add replacement subject(s)" else "Complete initial cohort"
    intro <- if (has_excl) {
      paste0("After the documented exclusion, the cohort has n=", ncur, ". Exactly ", missing, " new subject(s) must be added to restore n=20 before applying the verification criterion.")
    } else {
      paste0("The initial cohort has n=", ncur, ". Exactly ", missing, " new subject(s) must be added to complete the 20 reference individuals.")
    }
    nms <- if (!is.null(rv$replacement_raw)) names(rv$replacement_raw) else character(0)
    choices_opt <- c("(none)" = "", nms)
    div(class = "panel-card",
      h4(title), p(intro),
      fileInput("replacement_file", "File containing replacement/completion subjects", accept = c(".csv", ".txt", ".xlsx", ".xls")),
      if (!is.null(rv$replacement_raw)) tagList(
        div(class = "small-note", paste0("Selected file: ", rv$replacement_raw_name, " · ", nrow(rv$replacement_raw), " rows")),
        fluidRow(
          column(6, selectInput("replacement_value_col", "Result *", choices = nms, selected = if ("Value" %in% nms) "Value" else nms[1])),
          column(6, selectInput("replacement_id_col", "Anonymized identifier", choices = choices_opt, selected = if ("ID_ref" %in% nms) "ID_ref" else if ("ID" %in% nms) "ID" else ""))
        ),
        actionButton("append_replacement_subjects", paste0("Add ", missing, " subject(s) and reanalyze"), class = "btn-primary")
      ),
      p(class = "small-note", "This upload completes the first cohort; it is not a second verification cohort. RIveR will require exactly the number of missing subjects and, if identifiers are available, will check that individuals have not been reused.")
    )
  })

  observeEvent(input$replacement_file, {
    req(input$replacement_file)
    dat2 <- tryCatch(read_lab_file(input$replacement_file$datapath, input$replacement_file$name), error = function(e) e)
    if (inherits(dat2, "error")) {
      showNotification(conditionMessage(dat2), type = "error", duration = 8)
      rv$replacement_raw <- NULL; rv$replacement_raw_name <- NULL
    } else {
      rv$replacement_raw <- dat2
      rv$replacement_raw_name <- input$replacement_file$name
    }
  })

  observeEvent(input$append_replacement_subjects, {
    req(rv$main, rv$prepared, rv$replacement_raw, input$replacement_value_col)
    if (!analysis_idle_or_notify()) return()
    if (isTRUE(rv$main$partitioned) || !identical(rv$main$decision %||% "", "NOT EVALUABLE")) {
      showNotification("Replacement/completion is only applicable when the initial cohort of a single RI is not evaluable because n<20.", type = "warning", duration = 8)
      return()
    }
    ncur <- nrow(rv$prepared$data)
    missing <- 20L - ncur
    if (missing <= 0L) return()
    pr2 <- tryCatch(prepare_analysis_data(rv$replacement_raw, input$replacement_value_col,
                                         id_col = input$replacement_id_col %||% "",
                                         one_per_patient = input$one_per_patient),
                    error = function(e) e)
    if (inherits(pr2, "error")) {
      showNotification(conditionMessage(pr2), type = "error", duration = 8)
      return()
    }
    if (nrow(pr2$data) != missing) {
      showNotification(paste0("Exactly ", missing, " evaluable result(s) must be added; the file provides ", nrow(pr2$data), "."), type = "error", duration = 10)
      return()
    }
    first <- rv$prepared$data
    if ("patient_id" %in% names(pr2$data)) {
      new_ids <- trimws(as.character(pr2$data$patient_id)); new_ids <- new_ids[nzchar(new_ids)]
      used_ids <- character(0)
      if ("patient_id" %in% names(first)) used_ids <- c(used_ids, trimws(as.character(first$patient_id)))
      if (is.data.frame(rv$verification_exclusions) && "patient_id" %in% names(rv$verification_exclusions)) used_ids <- c(used_ids, trimws(as.character(rv$verification_exclusions$patient_id)))
      used_ids <- used_ids[nzchar(used_ids)]
      dup_cross <- intersect(new_ids, used_ids)
      if (length(dup_cross)) {
        showNotification(paste0("Detected ", length(dup_cross), " identifier(s) already used or excluded. Replacement subjects must be new."), type = "error", duration = 12)
        return()
      }
    } else {
      showNotification("The replacement file does not contain identifiers; RIveR cannot automatically verify that the individuals are new.", type = "warning", duration = 8)
    }
    second <- pr2$data
    old_ids <- suppressWarnings(as.numeric(first$.ri_row_id %||% seq_len(nrow(first))))
    excl_ids <- suppressWarnings(as.numeric(rv$verification_exclusions$row_id %||% numeric(0)))
    mx <- max(c(old_ids[is.finite(old_ids)], excl_ids[is.finite(excl_ids)], 0), na.rm = TRUE)
    second$.ri_row_id <- mx + seq_len(nrow(second))
    all_names <- union(names(first), names(second))
    for (nm in setdiff(all_names, names(first))) first[[nm]] <- NA
    for (nm in setdiff(all_names, names(second))) second[[nm]] <- NA
    rv$prepared$data <- rbind(first[, all_names, drop = FALSE], second[, all_names, drop = FALSE])
    src <- rv$replacement_raw_name %||% "—"
    rv$replacement_sources <- c(rv$replacement_sources %||% character(0), src)
    rv$replacement_n_total <- as.integer(rv$replacement_n_total %||% 0L) + nrow(second)
    rv$replacement_raw <- NULL; rv$replacement_raw_name <- NULL
    refresh_verification_audit()
    rv$main <- NULL; rv$partition <- NULL; rv$age <- NULL; rv$final <- NULL
    launch_async_analysis(go_results = TRUE, source_label = "Cohort replacement/completion",
                          completion_action = "replacement")
  })

  output$second_cohort_panel <- renderUI({
    req(rv$main)
    if (!identical(rv$study_type, "verify") || !identical(input$data_origin, "direct") ||
        !identical(rv$main$type %||% "", "direct_verification")) return(NULL)

    pending <- if (isTRUE(rv$main$partitioned)) rv$main$pending_partitions %||% character(0) else if (identical(rv$main$decision %||% "", "INCONCLUSIVE")) "single" else character(0)
    if (!length(pending)) return(NULL)

    nms <- if (!is.null(rv$second_raw)) names(rv$second_raw) else character(0)
    choices_opt <- c("(none)" = "", nms)

    if (isTRUE(rv$main$partitioned)) {
      labs <- vapply(pending, function(k) rv$main$partition_results[[k]]$partition_label %||% k, character(1))
      names(pending) <- labs
      intro <- "Each partition is verified independently. Add a second cohort only for the selected pending partition; partitions already verified are not repeated."
      selector <- selectInput("pending_partition", "Pending partition", choices = pending,
                              selected = unname(pending[1]))
    } else {
      intro <- "The first cohort was inconclusive (3–4/20 outside). You may add a new independent cohort without replacing or re-uploading the first."
      selector <- NULL
    }

    div(class="panel-card",
      h4("Add second verification cohort"),
      p(intro),
      selector,
      if (isTRUE(rv$main$partitioned)) checkboxInput("confirm_second_partition",
        "I confirm that the 20 individuals in the file belong to the selected partition", FALSE),
      fileInput("second_cohort_file", "Second-cohort file", accept = c(".csv", ".txt", ".xlsx", ".xls")),
      if (!is.null(rv$second_raw)) tagList(
        div(class="small-note", paste0("Selected file: ", rv$second_raw_name, " · ", nrow(rv$second_raw), " rows")),
        fluidRow(
          column(6, selectInput("second_value_col", "Result *", choices = nms, selected = if ("Value" %in% nms) "Value" else nms[1])),
          column(6, selectInput("second_id_col", "Anonymized identifier", choices = choices_opt, selected = if ("ID_ref" %in% nms) "ID_ref" else if ("ID" %in% nms) "ID" else ""))
        ),
        actionButton("append_second_cohort", "Add 20 individuals and reanalyze", class="btn-primary")
      ),
      p(class="small-note", "RIveR will require exactly 20 valid results. In partitioned verification, all individuals in the second file are assigned to the selected partition; therefore, you must confirm that the file has been filtered correctly. If identifiers are available, reuse of any individual is also checked.")
    )
  })

  observeEvent(input$second_cohort_file, {
    req(input$second_cohort_file)
    dat2 <- tryCatch(read_lab_file(input$second_cohort_file$datapath, input$second_cohort_file$name), error = function(e) e)
    if (inherits(dat2, "error")) {
      showNotification(conditionMessage(dat2), type="error", duration=8)
      rv$second_raw <- NULL; rv$second_raw_name <- NULL
    } else {
      rv$second_raw <- dat2
      rv$second_raw_name <- input$second_cohort_file$name
    }
  })

  observeEvent(input$append_second_cohort, {
    req(rv$main, rv$prepared, rv$second_raw, input$second_value_col)
    if (!analysis_idle_or_notify()) return()

    pr2 <- tryCatch(prepare_analysis_data(rv$second_raw, input$second_value_col,
                                         id_col = input$second_id_col %||% "",
                                         one_per_patient = input$one_per_patient),
                    error = function(e) e)
    if (inherits(pr2, "error")) {
      showNotification(conditionMessage(pr2), type="error", duration=8)
      return()
    }
    if (nrow(pr2$data) != 20L) {
      showNotification(paste0("The second cohort must contain exactly 20 evaluable results; after preparation n=", nrow(pr2$data), "."),
                       type="error", duration=12)
      return()
    }

    if (isTRUE(rv$main$partitioned)) {
      pending <- rv$main$pending_partitions %||% character(0)
      key <- input$pending_partition %||% ""
      if (!isTRUE(input$confirm_second_partition)) {
        showNotification("Confirm that the file corresponds exclusively to the selected partition.", type="warning", duration=8)
        return()
      }
      if (!nzchar(key) || !key %in% pending) {
        showNotification("Select a partition that still requires a second cohort.", type="warning", duration=8)
        return()
      }

      first <- rv$prepared$data
      if ("patient_id" %in% names(first) && "patient_id" %in% names(pr2$data)) {
        id1 <- trimws(as.character(first$patient_id)); id2 <- trimws(as.character(pr2$data$patient_id))
        prev_ids <- unlist(lapply(rv$partition_second_data %||% list(), function(d) if ("patient_id" %in% names(d)) as.character(d$patient_id) else character(0)), use.names = FALSE)
        dup_cross <- intersect(id2[nzchar(id2)], c(id1[nzchar(id1)], prev_ids[nzchar(prev_ids)]))
        if (length(dup_cross)) {
          showNotification(paste0("The second cohort is not independent: ", length(dup_cross), " identifiers already used in the study."),
                           type="error", duration=12)
          return()
        }
      } else {
        showNotification("Identifiers are not available in both uploads; RIveR cannot automatically verify independence of the individuals.",
                         type="warning", duration=10)
      }

      rv$partition_second_data[[key]] <- pr2$data
      rv$partition_second_sources[[key]] <- rv$second_raw_name %||% "—"
      lab <- rv$main$partition_results[[key]]$partition_label %||% key

      aud <- rv$prepared$audit
      step_lab <- paste0("Second cohort · ", lab)
      if (!is.null(aud) && nrow(aud)) aud <- aud[!aud$step %in% c(step_lab, "final n analyzed"), , drop = FALSE]
      total_eval <- nrow(rv$prepared$data) + sum(vapply(rv$partition_second_data %||% list(), nrow, integer(1)))
      rv$prepared$audit <- rbind(
        aud,
        data.frame(step = step_lab, n = 20L, stringsAsFactors = FALSE),
        data.frame(step = "final n analyzed", n = total_eval, stringsAsFactors = FALSE)
      )

      rv$second_raw <- NULL
      rv$second_raw_name <- NULL
      launch_async_analysis(go_results = TRUE, source_label = paste0("Second cohort · ", lab),
                            completion_action = "partition_second", completion_context = list(label=lab))
      return()
    }

    if (!identical(rv$main$decision %||% "", "INCONCLUSIVE")) {
      showNotification("A second cohort can only be added when the first verification is inconclusive.", type="warning", duration=8)
      return()
    }

    first <- rv$prepared$data
    if (nrow(first) != 20L) {
      showNotification("To add a new second cohort, the prepared first cohort must contain exactly 20 individuals.", type="error", duration=10)
      return()
    }

    if ("patient_id" %in% names(first) && "patient_id" %in% names(pr2$data)) {
      id1 <- trimws(as.character(first$patient_id)); id2 <- trimws(as.character(pr2$data$patient_id))
      dup_cross <- intersect(id1[nzchar(id1)], id2[nzchar(id2)])
      if (length(dup_cross)) {
        showNotification(paste0("The second cohort is not independent: ", length(dup_cross), " identifiers already present in the first cohort."),
                         type="error", duration=12)
        return()
      }
    } else {
      showNotification("Identifiers are not available in both cohorts; RIveR cannot automatically verify that the 20 individuals are independent.",
                       type="warning", duration=10)
    }

    first$verification_round <- 1L
    second <- pr2$data
    if (".ri_row_id" %in% names(second)) second$.ri_row_id <- max(first$.ri_row_id %||% seq_len(nrow(first)), na.rm=TRUE) + seq_len(nrow(second))
    second$verification_round <- 2L
    all_names <- union(names(first), names(second))
    for (nm in setdiff(all_names, names(first))) first[[nm]] <- NA
    for (nm in setdiff(all_names, names(second))) second[[nm]] <- NA
    first <- first[, all_names, drop=FALSE]
    second <- second[, all_names, drop=FALSE]
    rv$prepared$data <- rbind(first, second)
    aud <- rv$prepared$audit
    if (!is.null(aud) && nrow(aud)) aud <- aud[aud$step != "final n analyzed", , drop=FALSE]
    rv$prepared$audit <- rbind(aud,
      data.frame(step="New second cohort imported", n=20L, stringsAsFactors=FALSE),
      data.frame(step="final n analyzed", n=40L, stringsAsFactors=FALSE))

    launch_async_analysis(go_results = TRUE, source_label = "Second cohort",
                          completion_action = "second_cohort")
  })

  output$main_metrics <- renderUI({
    req(rv$main)
    m <- rv$main
    pieces <- list(div(class="metric", tags$b(fmt_integer(m$n %||% 0)), "n"))
    d7_global_blocked <- identical(m$module %||% "", "D7") &&
      (identical(m$partition$decision %||% "", "partition") || identical(m$age$decision %||% "", "age_effect"))
    if (!is.null(m$ri) && !d7_global_blocked) {
      digits <- m$display_digits %||% infer_decimal_places(rv$prepared$data$value)
      design <- normalize_reference_design(m$reference_design %||% rv$metadata$reference_design)
      if (isTRUE(design$active[["lower"]])) pieces <- c(pieces, list(div(class="metric", tags$b(format_lab_number(m$ri[[1]], digits)), paste0("Lower limit · ", reference_percentile_label(design$pair_percentiles[["lower"]])))))
      if (isTRUE(design$active[["upper"]])) pieces <- c(pieces, list(div(class="metric", tags$b(format_lab_number(m$ri[[2]], digits)), paste0("Upper limit · ", reference_percentile_label(design$pair_percentiles[["upper"]])))))
    }
    if (!is.null(m$decision)) pieces <- c(pieces, list(div(class="metric", tags$b(m$decision), "Decision")))
    do.call(tagList, pieces)
  })

  output$main_table <- renderTable({
    req(rv$main)
    m <- rv$main
    if (m$type == "direct_establishment") {
      digits <- m$display_digits %||% infer_decimal_places(rv$prepared$data$value)
      design <- normalize_reference_design(m$reference_design %||% rv$metadata$reference_design)
      partitioned <- identical(rv$partition$decision %||% "", "partition") || identical(rv$age$decision %||% "", "partition") || identical(rv$age$decision %||% "", "continuous")
      res_label <- if (identical(design$tail, "two_sided")) {
        if (grepl("exploratory", tolower(m$method %||% ""))) "Exploratory non-parametric RI (not adopted)" else if (partitioned) "Overall RI assessed (not adopted)" else "Estimated RI"
      } else {
        paste0(reference_limit_name(design), if (partitioned) " overall assessed (not adopted)" else " estimated")
      }
      pars <- c("Method", "Reference design", res_label)
      vals <- c(m$method, paste0(reference_design_label(design), " · ", reference_design_percentile_text(design)), reference_result_text(m$ri, design, digits))
      if (isTRUE(design$active[["lower"]])) {
        pars <- c(pars, paste0("90% CI · ", reference_percentile_label(design$pair_percentiles[["lower"]])), "Lower-limit precision")
        vals <- c(vals, format_lab_interval(m$ci90_lower, digits), format_percent(m$precision_ratio[1], 1))
      }
      if (isTRUE(design$active[["upper"]])) {
        pars <- c(pars, paste0("90% CI · ", reference_percentile_label(design$pair_percentiles[["upper"]])), "Upper-limit precision")
        vals <- c(vals, format_lab_interval(m$ci90_upper, digits), format_percent(m$precision_ratio[2], 1))
      }
      pars <- c(pars, "Precision criterion", "Extremes requiring review", "Tukey suspected outliers", "Skewness of Bowley", "Anderson-Darling p", "Symmetry p", "Box-Cox λ", "AD p after Box-Cox")
      vals <- c(vals,
                paste0("< ", format_percent(m$precision_threshold %||% 0.20, 0), " of the computational width for each active limit"),
                as.character(m$outlier_assessment$extreme_n %||% 0), as.character(m$outlier_assessment$suspect_n %||% 0),
                ifelse(is.finite(m$bowley), formatC(m$bowley, format="f", digits=3, decimal.mark=","), "—"),
                format_p_value(m$normality_p), format_p_value(m$symmetry_p),
                ifelse(is.finite(m$boxcox_lambda), formatC(m$boxcox_lambda, format="f", digits=3, decimal.mark=","), "—"),
                format_p_value(m$boxcox_normality_p))
      data.frame(Parameter=pars, Result=vals, check.names=FALSE)
    } else if (m$type == "direct_verification") {
      if (isTRUE(m$partitioned)) {
        direct_partitioned_verification_display_table(m)
      } else {
        tab <- direct_verification_display_table(m)
        names(tab) <- c("Parameter", "Result")
        tab
      }
    } else if (m$type == "indirect_establishment" && identical(m$module %||% "", "D7")) {
      if (identical(m$partition$decision %||% "", "partition") && !is.null(m$partition$table)) {
        m$partition$table
      } else if (identical(m$age$model$decision %||% "", "continuous_candidate") && !is.null(m$age$model$table)) {
        m$age$model$table
      } else if (!is.null(m$refine)) {
        tab <- m$refine$table
        names(tab) <- gsub("PointEst", "Estimate", names(tab))
        tab
      } else {
        data.frame(Parameter=c("D7 decision","n analyzed","Reason"),
                   Result=c(m$decision %||% "NOT EVALUABLE", m$n %||% 0L, m$sample_context$text %||% m$recommendation %||% "—"),
                   check.names=FALSE)
      }
    } else if (m$type == "indirect_verification") {
      indirect_verification_display_table(m)
    } else data.frame(Message="Result available in the recommendation.")
  }, striped=TRUE, spacing="s")

  output$main_plot <- renderPlot({
    req(rv$prepared, rv$main)
    ri <- rv$main$ri %||% rv$main$target %||% NULL
    if (identical(rv$main$module %||% "", "D7") &&
        (identical(rv$main$partition$decision %||% "", "partition") || identical(rv$main$age$decision %||% "", "age_effect"))) ri <- NULL
    simple_hist(rv$prepared$data$value,
                main = rv$metadata$analyte %||% "Result distribution",
                xlab = paste(rv$metadata$analyte %||% "Result", rv$metadata$unit %||% ""), ri = ri)
  })

  output$distribution_original_hist <- renderPlot({
    req(rv$prepared, rv$main)
    if (!identical(rv$main$type, "direct_establishment")) return(invisible(NULL))
    diagnostic_hist_density(rv$prepared$data$value, main = "Histogram + density",
                            xlab = paste(rv$metadata$analyte %||% "Result", rv$metadata$unit %||% ""))
  })

  output$distribution_original_qq <- renderPlot({
    req(rv$prepared, rv$main)
    if (!identical(rv$main$type, "direct_establishment")) return(invisible(NULL))
    diagnostic_qq_plot(rv$prepared$data$value, main = "Q-Q plot · original data")
  })

  output$distribution_boxcox_hist <- renderPlot({
    req(rv$main)
    if (!identical(rv$main$type, "direct_establishment")) return(invisible(NULL))
    z <- rv$main$distribution$boxcox$transformed %||% numeric(0)
    if (!length(z) || !any(is.finite(z))) return(diagnostic_empty_plot("Box-Cox not available"))
    diagnostic_hist_density(z, main = "Histogram + density", xlab = "Box-Cox scale")
  })

  output$distribution_boxcox_qq <- renderPlot({
    req(rv$main)
    if (!identical(rv$main$type, "direct_establishment")) return(invisible(NULL))
    z <- rv$main$distribution$boxcox$transformed %||% numeric(0)
    if (!length(z) || !any(is.finite(z))) return(diagnostic_empty_plot("Box-Cox not available"))
    diagnostic_qq_plot(z, main = "Q-Q plot · after Box-Cox")
  })

  output$distribution_table <- renderTable({
    req(rv$main)
    tab <- direct_distribution_display_table(rv$main)
    if (is.null(tab)) return(data.frame(Message = "No distribution diagnostic is available."))
    tab
  }, striped = TRUE, spacing = "s")

  output$method_candidates_table <- renderTable({
    req(rv$main)
    tab <- direct_method_candidates_display_table(rv$main)
    if (is.null(tab)) return(data.frame(Message = "No method comparison is available."))
    tab
  }, striped = TRUE, spacing = "s")

  output$method_selection_note <- renderUI({
    req(rv$main)
    if (!identical(rv$main$type, "direct_establishment")) return(NULL)
    txt <- rv$main$method_selection_reason %||% ""
    if (rv$main$n >= 120) txt <- paste0("Final method: ", rv$main$method, ". The normality/Box-Cox assessment is shown for traceability but does not replace the standard non-parametric approach with n≥120.")
    status_html(if (rv$main$n >=120) "green" else if (grepl("exploratory", tolower(rv$main$method))) "red" else "yellow", "Method selection", txt)
  })

  output$small_sample_resolution_ui <- renderUI({
    req(rv$main)
    if (!identical(rv$main$type, "direct_establishment") || !isTRUE(rv$main$n < 120)) return(NULL)

    model_ok <- !grepl("exploratory", tolower(rv$main$method %||% ""))
    precision_ok <- isTRUE(rv$main$precision_ok)
    if (!model_ok) {
      current <- rv$small_sample_resolution$decision %||% "pending"
      current_reason <- rv$small_sample_resolution$reason %||% ""
      return(tagList(
        hr(), h4("Specialist closure when an RI cannot be established"),
        status_html("red", "No defensible model identified",
                    "The data do not support an adequate parametric/robust model, and Box-Cox does not resolve the distributional structure. The non-parametric RI is shown for exploratory purposes only. Do not automatically increase the sample size without first investigating possible population heterogeneity."),
        div(class="panel-card", style="background:#fafcfe",
            tags$b("Recommended next action"),
            tags$ol(
              tags$li("Investigate possible subpopulations or mixtures (sex, age, origin, clinical/preanalytical conditions, or other available covariates)."),
              tags$li("Review the reference-population selection criteria and the possible inclusion of individuals who do not belong to the target population."),
              tags$li("Only if a single population is confirmed, increase the sample size and reassess; with n≥120, the standard non-parametric approach can be applied.")
            )),
        radioButtons("small_sample_resolution_choice", "Specialist decision",
                     choices = c("Investigate possible subpopulations and repeat the analysis"="investigate",
                                 "Review subject selection/inclusion"="review_population",
                                 "Increase the reference population and reassess"="increase_no_model",
                                 "Close the study without establishing an RI"="no_ri",
                                 "Another action"="other",
                                 "Leave the decision pending"="pending"),
                     selected = current),
        textAreaInput("small_sample_resolution_reason", "Justification / action plan", value=current_reason, rows=3,
                      placeholder="Document the interpretation of the issue and the agreed action."),
        p(class="small-note", "Any decision other than 'pending' requires a justification of at least 20 characters. RIveR can formally close the study as FINAL REPORT — RI NOT ESTABLISHED."),
        actionButton("apply_small_sample_resolution", "Record the decision and prepare the report", class="btn-primary")
      ))
    }
    if (!precision_ok) {
      return(status_html("red", "Insufficient precision to close with n<120",
                         "A method could be selected, but one or both 90% CIs are too wide. The option to adopt the RI is not offered; increase the sample size and repeat the analysis."))
    }
    if (isTRUE(rv$main$outlier_review_required)) {
      return(status_html("yellow", "Resolve extreme values first",
                         "Before deciding whether to adopt the RI with n<120, review of the detected extreme values must be completed."))
    }
    pdec <- rv$partition$decision %||% NULL
    adec <- rv$age$decision %||% NULL
    if (identical(pdec, "partition") || identical(pdec, "indeterminate") || identical(adec, "partition") || identical(adec, "continuous") || identical(adec, "indeterminate")) {
      return(status_html("yellow", "Closure with n<120 pending covariates",
                         "The model and precision are adequate, but there is a partitioning/age issue that must be resolved before approving a final RI."))
    }

    current <- rv$small_sample_resolution$decision %||% "pending"
    current_cat <- rv$small_sample_resolution$category %||% ""
    current_reason <- rv$small_sample_resolution$reason %||% ""
    digs <- rv$main$display_digits %||% 2
    tagList(
      hr(), h4("Specialist closure with n<120"),
      status_html("yellow", "Specialist decision required",
                  paste0("RIveR conditionally proposes ", format_lab_interval(rv$main$ri, digs),
                         " using ", rv$main$method, ". The selected assumptions and the precision of both limits are adequate; document whether you adopt the RI or prefer to increase the sample size.")),
      radioButtons("small_sample_resolution_choice", "Specialist decision",
                   choices = c("Adopt the proposed RI"="adopt",
                               "Do not adopt it yet and increase the sample size"="increase",
                               "Leave the decision pending"="pending"),
                   selected = current),
      selectInput("small_sample_reason_category", "Primary rationale",
                  choices = c("Select…"="", "Model/distribution assumptions"="model",
                              "Limit precision"="precision", "Biological / clinical plausibility"="biological",
                              "Literature / external consensus"="literature", "Other"="other"),
                  selected = current_cat),
      textAreaInput("small_sample_resolution_reason", "Specialist justification", value=current_reason, rows=3,
                    placeholder="Document why you adopt the RI with n<120 or why you decide to increase the sample size."),
      p(class="small-note", "To adopt the RI or decide to increase the sample size, a rationale and justification of at least 20 characters are required. The decision and precision values will be traceable in the report."),
      actionButton("apply_small_sample_resolution", "Record the decision and prepare the report", class="btn-primary")
    )
  })

  observeEvent(input$apply_small_sample_resolution, {
    req(rv$main)
    if (!identical(rv$main$type, "direct_establishment") || !isTRUE(rv$main$n < 120)) return()
    choice <- input$small_sample_resolution_choice %||% "pending"
    if (identical(choice, "pending")) {
      rv$small_sample_resolution <- NULL
      rv$final <- compose_final_recommendation(rv$main, rv$partition, rv$age, rv$partition_resolution, NULL)
      updateRadioButtons(session, "faculty_decision", selected="pending")
      showNotification("The decision remains pending; the report will remain provisional.", type="warning", duration=9)
      return()
    }
    no_model_choices <- c("investigate","review_population","increase_no_model","no_ri","other")
    no_model_mode <- grepl("exploratory", tolower(rv$main$method %||% ""))
    reason <- trimws(input$small_sample_resolution_reason %||% "")
    category <- if (no_model_mode && choice %in% no_model_choices) "no_model" else (input$small_sample_reason_category %||% "")
    if (nchar(reason) < 20 || (!no_model_mode && !nzchar(category))) {
      showNotification(if (no_model_mode) "Document a justification or action plan of at least 20 characters." else "Select the rationale and document a justification of at least 20 characters.", type="warning", duration=10)
      return()
    }
    if (identical(choice, "adopt") && (!isTRUE(rv$main$precision_ok) || grepl("exploratory", tolower(rv$main$method %||% "")))) {
      showNotification("The RI does not meet the requirements for adoption through the n<120 pathway.", type="error", duration=10)
      return()
    }
    resolution <- list(
      decision=choice, mode=if (no_model_mode) "no_model" else "small_sample",
      timestamp=format(Sys.time(), "%Y-%m-%d %H:%M:%S"), category=category, reason=reason,
      method=rv$main$method, n=rv$main$n, ri=rv$main$ri,
      precision_lower=rv$main$precision_ratio[1], precision_upper=rv$main$precision_ratio[2],
      normality_p=rv$main$normality_p, symmetry_p=rv$main$symmetry_p,
      boxcox_lambda=rv$main$boxcox_lambda, boxcox_normality_p=rv$main$boxcox_normality_p
    )
    rv$small_sample_resolution <- resolution
    logrow <- data.frame(
      timestamp=resolution$timestamp, decision=switch(choice,
        adopt="Adopt RI", increase="Increase sample size", investigate="Investigate subpopulations",
        review_population="Review reference population", increase_no_model="Increase and reassess",
        no_ri="Close without establishing an RI", other="Another action", choice),
      category=category, reason=reason, method=rv$main$method, n=rv$main$n,
      ri=format_lab_interval(rv$main$ri, rv$main$display_digits %||% 2),
      precision_lower=rv$main$precision_ratio[1], precision_upper=rv$main$precision_ratio[2],
      stringsAsFactors=FALSE
    )
    if (is.null(rv$small_sample_decisions) || !nrow(rv$small_sample_decisions)) rv$small_sample_decisions <- logrow else rv$small_sample_decisions <- rbind(rv$small_sample_decisions, logrow)
    rv$final <- compose_final_recommendation(rv$main, rv$partition, rv$age, rv$partition_resolution, rv$small_sample_resolution)
    updateRadioButtons(session, "faculty_decision", selected=if (isTRUE(rv$final$is_final)) "accept" else "pending")
    goto("Recommendation")
    showNotification(if (isTRUE(rv$final$is_final)) "Decision recorded. The report generated now will be marked as FINAL." else "Decision recorded. The study remains open/provisional.",
                     type=if (isTRUE(rv$final$is_final)) "message" else "warning", duration=12)
  })

  output$outlier_table <- renderTable({
    req(rv$main)
    tab <- direct_outlier_display_table(rv$main)
    if (is.null(tab)) return(data.frame(Message = "No extreme-value assessment is available."))
    tab
  }, striped = TRUE, spacing = "s")

  current_outlier_candidates <- reactive({
    req(rv$main, rv$prepared)
    outlier_candidate_rows(rv$prepared$data, rv$main)
  })


  excluded_outlier_row_ids <- reactive({
    d <- rv$outlier_decisions %||% data.frame()
    if (!is.data.frame(d) || !nrow(d) || !all(c("row_id","decision") %in% names(d))) return(integer(0))
    unique(d$row_id[d$decision == "Exclude"])
  })

  output$outlier_boxplot_ui <- renderUI({
    req(rv$prepared, rv$main)
    if (!identical(rv$main$type, "direct_establishment")) return(NULL)
    excl <- excluded_outlier_row_ids()
    if (length(excl) && !is.null(rv$outlier_baseline_data)) {
      return(tagList(
        h5("Visualization · box plot"),
        p(class="small-note", "The box plot is a visual aid. Detection criteria remain Dixon/Reed and Tukey; no point is automatically excluded based on its position in the plot."),
        fluidRow(
          column(6, h6("Before specialist review"), plotOutput("outlier_global_boxplot_before", height=310)),
          column(6, h6("After recalculation"), plotOutput("outlier_global_boxplot_current", height=310))
        )
      ))
    }
    tagList(
      h5("Visualization · box plot"),
      p(class="small-note", "The box plot is a visual aid. Detection criteria remain Dixon/Reed and Tukey; no point is automatically excluded based on its position in the plot."),
      plotOutput("outlier_global_boxplot_current", height=310)
    )
  })

  output$outlier_global_boxplot_before <- renderPlot({
    req(rv$outlier_baseline_data)
    plot_outlier_boxplot(rv$outlier_baseline_data, main="Complete population · before review",
                         ylab=paste(rv$metadata$analyte %||% "Result", rv$metadata$unit %||% ""),
                         marked_row_ids=excluded_outlier_row_ids())
  })

  output$outlier_global_boxplot_current <- renderPlot({
    req(rv$prepared$data)
    plot_outlier_boxplot(rv$prepared$data, main=if (length(excluded_outlier_row_ids())) "Complete population · after recalculation" else "Complete population",
                         ylab=paste(rv$metadata$analyte %||% "Result", rv$metadata$unit %||% ""))
  })

  output$outlier_decision_ui <- renderUI({
    req(rv$main, rv$prepared)
    if (!identical(rv$main$type, "direct_establishment")) return(NULL)
    cand <- current_outlier_candidates()
    if (is.null(cand) || !nrow(cand)) {
      if (!is.null(rv$outlier_decisions) && nrow(rv$outlier_decisions)) {
        return(status_html("green", "Extreme-value review completed",
                           "Specialist decisions are documented. The analysis shown corresponds to the dataset after applying these decisions."))
      }
      return(NULL)
    }
    digits <- rv$main$display_digits %||% infer_decimal_places(rv$prepared$data$value)
    labels <- vapply(seq_len(nrow(cand)), function(i) {
      idtxt <- if (nzchar(cand$patient_id[i] %||% "")) paste0(" · ID ", cand$patient_id[i]) else ""
      paste0(format_lab_number(cand$value[i], digits), " ", rv$metadata$unit %||% "", idtxt,
             " · row ", cand$row_id[i])
    }, character(1))
    # checkboxGroupInput uses names(choices) as the visible label and the
    # vector values as the value returned to the input. In v0.6.0 they were
# Previously inverted: the row (e.g., 301) was displayed, but Shiny returned the
# full descriptive label, which could not then be converted to an integer.
# Explicitly construct label -> row_id.
    choice_values <- setNames(as.character(cand$row_id), labels)
    tagList(
      status_html("yellow", "Specialist decision required",
                  "Statistical detection alone does not demonstrate that the datum is aberrant. Review the cause and document the decision."),
      radioButtons("outlier_action_choice", "What do you want to do with the detected extreme values?",
                   choices = c("Retain them after review" = "retain",
                               "Exclude the selected values and recalculate" = "exclude",
                               "Leave the decision pending" = "pending"),
                   selected = isolate(input$outlier_action_choice) %||% "pending"),
      conditionalPanel("input.outlier_action_choice == 'exclude'",
                       checkboxGroupInput("outlier_selected_ids", "Values to exclude",
                                          choices = choice_values, selected = as.character(cand$row_id))),
      selectInput("outlier_reason_category", "Primary reason",
                  choices = c("Select…"="", "Confirmed preanalytical error"="preanalytical",
                              "Analytical error / invalid result"="analytical",
                              "Failure to meet selection criteria"="selection",
                              "Legitimate population datum: retain"="legitimate", "Other"="other"),
                  selected = isolate(input$outlier_reason_category) %||% ""),
      textAreaInput("outlier_review_reason", "Justification / review outcome",
                    value = isolate(input$outlier_review_reason) %||% "", rows = 3,
                    placeholder = "Briefly describe the evidence supporting retention or exclusion of the value."),
      p(class="small-note", "Retention or exclusion requires a reason and a justification of at least 15 characters. If a value is excluded, RIveR will automatically recalculate the entire study; if new extremes appear, a new review will be requested."),
      actionButton("apply_outlier_decision", "Apply decision and recalculate", class="btn-primary")
    )
  })

  output$outlier_decisions_table <- renderTable({
    if (is.null(rv$outlier_decisions) || !nrow(rv$outlier_decisions)) return(NULL)
    d <- rv$outlier_decisions
    cat_label <- function(x) switch(as.character(x),
      preanalytical="Confirmed preanalytical error", analytical="Analytical error / invalid result",
      selection="Failure to meet selection criteria", legitimate="Legitimate population datum",
      other="Other", as.character(x))
    data.frame(
      Data = d$timestamp,
      `Iteration` = d$iteration,
      `ID/row` = ifelse(nzchar(d$patient_id), paste0(d$patient_id, " / ", d$row_id), d$row_id),
      Value = d$value,
      Scope = if ("scope" %in% names(d)) d$scope else "Complete population",
      Decision = d$decision,
      Reason = vapply(d$category, cat_label, character(1)),
      Justification = d$reason,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  }, striped = TRUE, spacing = "s")

  observeEvent(input$apply_outlier_decision, {
    req(rv$main, rv$prepared)
    if (!analysis_idle_or_notify()) return()
    cand <- current_outlier_candidates()
    if (is.null(cand) || !nrow(cand)) {
      showNotification("There are no extreme values pending a decision.", type="message")
      return()
    }
    action <- input$outlier_action_choice %||% "pending"
    if (identical(action, "pending")) {
      showNotification("The decision remains pending; the dataset has not been modified.", type="warning", duration=8)
      return()
    }
    reason <- trimws(input$outlier_review_reason %||% "")
    category <- input$outlier_reason_category %||% ""
    if (!nzchar(category) || nchar(reason) < 15) {
      showNotification("Select a reason and document a justification of at least 15 characters.", type="warning", duration=10)
      return()
    }
    before_main <- rv$main

    if (identical(action, "exclude")) {
      # Row identifiers are handled as text to avoid coercion
      # so that it works even if .ri_row_id is not strictly
      # to integer.
      ids <- as.character(input$outlier_selected_ids %||% character(0))
      ids <- ids[nzchar(ids)]
      if (!length(ids)) {
        showNotification("Select at least one value to exclude.", type="warning", duration=8)
        return()
      }
      chosen <- cand[as.character(cand$row_id) %in% ids, , drop = FALSE]
      if (!nrow(chosen)) {
        showNotification("The selected values are no longer available in the current dataset.", type="warning")
        return()
      }
      decision_label <- "Exclude"
      rv$prepared$data <- rv$prepared$data[!rv$prepared$data$.ri_row_id %in% chosen$row_id, , drop = FALSE]
    } else {
      chosen <- cand
      decision_label <- "Retain"
      rv$retained_extreme_values <- unique(c(rv$retained_extreme_values, chosen$value))
    }

    chosen$patient_id <- as.character(chosen$patient_id)
    chosen$patient_id[is.na(chosen$patient_id)] <- ""
    log_rows <- data.frame(
      timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      iteration = rv$analysis_iteration %||% 0L,
      row_id = chosen$row_id,
      patient_id = chosen$patient_id,
      value = chosen$value,
      scope = "Complete population",
      decision = decision_label,
      category = category,
      reason = reason,
      stringsAsFactors = FALSE
    )
    if (is.null(rv$outlier_decisions) || !nrow(rv$outlier_decisions)) rv$outlier_decisions <- log_rows
    else rv$outlier_decisions <- rbind(rv$outlier_decisions, log_rows)

    refresh_outlier_audit()
# Each new batch of extreme values requires a separate explicit decision.
    updateRadioButtons(session, "outlier_action_choice", selected = "pending")
    updateSelectInput(session, "outlier_reason_category", selected = "")
    updateTextAreaInput(session, "outlier_review_reason", value = "")
    updateRadioButtons(session, "faculty_decision", selected = "pending")
    rv$partition_resolution <- NULL
    rv$small_sample_resolution <- NULL
    launch_async_analysis(go_results = TRUE,
                          source_label = if (identical(action, "exclude")) "Recalculating after a justified exclusion" else "Recalculating after extreme-value review",
                          completion_action = "outlier_global",
                          completion_context = list(before_main=before_main, outlier_action=action))
  })

  output$outlier_impact_ui <- renderUI({
    if (is.null(rv$outlier_impacts) || !nrow(rv$outlier_impacts)) return(NULL)
    d <- rv$outlier_impacts[nrow(rv$outlier_impacts), , drop=FALSE]
    status_html("green", "Impact of recalculation after exclusion",
                paste0("Before: n=", d$n_before, ", RI ", d$ri_before,
                       ", URL precision ", format_percent(d$pr_upper_before, 1),
                       ". After: n=", d$n_after, ", RI ", d$ri_after,
                       ", URL precision ", format_percent(d$pr_upper_after, 1), "."))
  })

  current_subgroup_outlier_candidates <- reactive({
    rows <- list()
    if (!is.null(rv$partition$subgroup_outlier_candidates) && nrow(rv$partition$subgroup_outlier_candidates)) {
      x <- rv$partition$subgroup_outlier_candidates
      x$source <- "partitioning by qualitative variable"
      rows[[length(rows)+1]] <- x
    }
    av <- rv$age$cut_validation %||% NULL
    if (!is.null(av$subgroup_outlier_candidates) && nrow(av$subgroup_outlier_candidates)) {
      x <- av$subgroup_outlier_candidates
      x$source <- "partitioning by quantitative variable"
      rows[[length(rows)+1]] <- x
    }
    if (!length(rows)) return(NULL)
    out <- do.call(rbind, rows)
    ids <- unique(as.character(out$row_id))
    merged <- lapply(ids, function(id) {
      z <- out[as.character(out$row_id) == id, , drop=FALSE]
      r <- z[1, , drop=FALSE]
      r$scope <- paste(unique(z$scope), collapse=" + ")
      r$source <- paste(unique(z$source), collapse=" + ")
      r
    })
    do.call(rbind, merged)
  })

  output$subgroup_outlier_ui <- renderUI({
    req(rv$main)
    if (!identical(rv$main$type, "direct_establishment")) return(NULL)
    if (isTRUE(rv$main$outlier_review_required)) {
      return(status_html("grey", "Aberrant values within subgroups: not yet assessed",
                         "First complete the review of extreme values in the full population. RIveR will then recalculate and, if there is a candidate partition by a qualitative or quantitative variable, repeat detection within each subgroup."))
    }
    av <- rv$age$cut_validation %||% NULL
    if (!is.null(av) && isTRUE(av$boundary_outlier_pending)) {
      return(status_html("yellow", "Possible misclassified boundary observation",
                         "RIveR detected an extreme value near the cut-point that is compatible with the neighboring group's distribution. It is not presented as an exclusion candidate: review the cut-point location first."))
    }
    cand <- current_subgroup_outlier_candidates()
    if (!is.null(cand) && nrow(cand)) {
      return(status_html("yellow", "Aberrant-value review within subgroups pending",
                         "A value may not be extreme in the overall population but may be extreme within its subgroup. The partitioning conclusion remains provisional until these values are reviewed."))
    }
    has_partition_eval <- !is.null(rv$partition) || !is.null(rv$age$cut_validation)
    if (has_partition_eval) {
      return(status_html("green", "Aberrant-value review within subgroups completed",
                         "No extreme values remain pending within the qualitative- or quantitative-variable partitions that have been assessed."))
    }
    if (!is.null(rv$age) && identical(rv$age$decision %||% "", "continuous")) {
      qlab <- quantitative_label(rv$age)
      return(status_html("grey", paste0("Continuous quantitative variable · ", qlab),
                         paste0("When ", qlab, " is modeled continuously, applying Dixon/Reed or Tukey within arbitrary ranges is not appropriate. Detection of problematic observations should be based on residuals and influence from the continuous model.")))
    }
    NULL
  })

  output$subgroup_outlier_table <- renderTable({
    tabs <- list()
    if (!is.null(rv$partition$subgroup_outlier_summary) && nrow(rv$partition$subgroup_outlier_summary)) {
      x <- rv$partition$subgroup_outlier_summary
      x$Scope <- "Qualitative variable"
      tabs[[length(tabs)+1]] <- x
    }
    av <- rv$age$cut_validation %||% NULL
    if (!is.null(av$subgroup_outlier_summary) && nrow(av$subgroup_outlier_summary)) {
      x <- av$subgroup_outlier_summary
      if ("Group" %in% names(x)) x$Group <- vapply(x$Group, function(g) age_group_operational_label(rv$age, g), character(1))
      x$Scope <- "Quantitative variable"
      tabs[[length(tabs)+1]] <- x
    }
    if (!length(tabs)) return(NULL)
    out <- do.call(rbind, tabs)
    out[, c("Scope", setdiff(names(out), "Scope")), drop=FALSE]
  }, striped=TRUE, spacing="s")


  subgroup_plot_specs <- reactive({
    req(rv$prepared)
    specs <- list()
    if (!is.null(rv$partition) && "qualitative" %in% names(rv$prepared$data)) {
      qlab <- qualitative_label(rv$partition)
      specs$sex <- list(label=paste0("Qualitative variable · ", qlab), current=as.character(rv$prepared$data$qualitative), covariate_label=qlab)
    }
    cut <- rv$age$selected_cut %||% NA_real_
    cv <- rv$age$cut_validation %||% NULL
    if (!is.null(cv) && is.finite(cut) && "quantitative" %in% names(rv$prepared$data)) {
      qlab <- quantitative_label(rv$age)
      suffix <- if (quantitative_age_like(rv$age)) " years" else ""
      op <- rv$age$operational_cut_info %||% NULL
      lower_lab <- if (!is.null(op) && isTRUE(op$preserves_groups)) op$lower_label else paste0("≤ ", formatC(cut, format="f", digits=2, decimal.mark=","), suffix)
      upper_lab <- if (!is.null(op) && isTRUE(op$preserves_groups)) op$upper_label else paste0("> ", formatC(cut, format="f", digits=2, decimal.mark=","), suffix)
      lab <- if (!is.null(op) && isTRUE(op$preserves_groups)) paste0("Quantitative variable · ", qlab, " · operational cut-point ", op$display) else paste0("Quantitative variable · ", qlab, " · statistical cut-point ", formatC(cut, format="f", digits=2, decimal.mark=","), suffix)
      specs$age <- list(label=lab, current=ifelse(rv$prepared$data$quantitative <= cut, lower_lab, upper_lab), cut=cut, covariate_label=qlab)
    }
    specs
  })

  output$subgroup_boxplot_ui <- renderUI({
    sp <- subgroup_plot_specs()
    if (!length(sp)) return(NULL)
    excl <- excluded_outlier_row_ids()
    has_before <- length(excl) && !is.null(rv$outlier_baseline_data)
    items <- list(h5("Subgroup visualization · box plot"),
                  p(class="small-note", "For a discrete partition, visualization is repeated within groups. This allows identification of observations that may not stand out globally but do so within their subgroup context."))
    if (!is.null(sp$sex)) {
      items <- c(items, list(h6(sp$sex$label)))
      if (has_before) items <- c(items, list(fluidRow(column(6, h6("Before review"), plotOutput("outlier_sex_boxplot_before", height=320)), column(6, h6("After recalculation"), plotOutput("outlier_sex_boxplot_current", height=320)))))
      else items <- c(items, list(plotOutput("outlier_sex_boxplot_current", height=320)))
    }
    if (!is.null(sp$age)) {
      items <- c(items, list(h6(sp$age$label)))
      if (has_before) items <- c(items, list(fluidRow(column(6, h6("Before review"), plotOutput("outlier_age_boxplot_before", height=320)), column(6, h6("After recalculation"), plotOutput("outlier_age_boxplot_current", height=320)))))
      else items <- c(items, list(plotOutput("outlier_age_boxplot_current", height=320)))
    }
    do.call(tagList, items)
  })

  output$outlier_sex_boxplot_before <- renderPlot({
    req(rv$outlier_baseline_data, "sex" %in% names(rv$outlier_baseline_data))
    plot_outlier_boxplot(rv$outlier_baseline_data, group=vapply(rv$outlier_baseline_data$sex, friendly_group_label, character(1), group_col="sex"),
                         main=paste0(qualitative_label(rv$partition), " · before review"), ylab=paste(rv$metadata$analyte %||% "Result", rv$metadata$unit %||% ""),
                         marked_row_ids=excluded_outlier_row_ids())
  })
  output$outlier_sex_boxplot_current <- renderPlot({
    req(rv$prepared$data, "sex" %in% names(rv$prepared$data))
    plot_outlier_boxplot(rv$prepared$data, group=vapply(rv$prepared$data$sex, friendly_group_label, character(1), group_col="sex"),
                         main=if (length(excluded_outlier_row_ids())) paste0(qualitative_label(rv$partition), " · after recalculation") else qualitative_label(rv$partition),
                         ylab=paste(rv$metadata$analyte %||% "Result", rv$metadata$unit %||% ""))
  })
  output$outlier_age_boxplot_before <- renderPlot({
    req(rv$outlier_baseline_data, "age" %in% names(rv$outlier_baseline_data))
    cut <- rv$age$selected_cut %||% NA_real_; req(is.finite(cut))
    op <- rv$age$operational_cut_info %||% NULL
    lower_lab <- if (!is.null(op) && isTRUE(op$preserves_groups)) op$lower_label else paste0("≤ ", formatC(cut, format="f", digits=2, decimal.mark=","), " years")
    upper_lab <- if (!is.null(op) && isTRUE(op$preserves_groups)) op$upper_label else paste0("> ", formatC(cut, format="f", digits=2, decimal.mark=","), " years")
    grp <- ifelse(rv$outlier_baseline_data$age <= cut, lower_lab, upper_lab)
    plot_outlier_boxplot(rv$outlier_baseline_data, group=grp, main=paste0(quantitative_label(rv$age), " · before review"),
                         ylab=paste(rv$metadata$analyte %||% "Result", rv$metadata$unit %||% ""), marked_row_ids=excluded_outlier_row_ids())
  })
  output$outlier_age_boxplot_current <- renderPlot({
    req(rv$prepared$data, "age" %in% names(rv$prepared$data))
    cut <- rv$age$selected_cut %||% NA_real_; req(is.finite(cut))
    op <- rv$age$operational_cut_info %||% NULL
    lower_lab <- if (!is.null(op) && isTRUE(op$preserves_groups)) op$lower_label else paste0("≤ ", formatC(cut, format="f", digits=2, decimal.mark=","), " years")
    upper_lab <- if (!is.null(op) && isTRUE(op$preserves_groups)) op$upper_label else paste0("> ", formatC(cut, format="f", digits=2, decimal.mark=","), " years")
    grp <- ifelse(rv$prepared$data$age <= cut, lower_lab, upper_lab)
    plot_outlier_boxplot(rv$prepared$data, group=grp, main=if (length(excluded_outlier_row_ids())) paste0(quantitative_label(rv$age), " · after recalculation") else quantitative_label(rv$age),
                         ylab=paste(rv$metadata$analyte %||% "Result", rv$metadata$unit %||% ""))
  })

  output$subgroup_outlier_decision_ui <- renderUI({
    cand <- current_subgroup_outlier_candidates()
    if (is.null(cand) || !nrow(cand)) return(NULL)
    digits <- rv$main$display_digits %||% infer_decimal_places(rv$prepared$data$value)
    labels <- vapply(seq_len(nrow(cand)), function(i) {
      idtxt <- if (nzchar(cand$patient_id[i] %||% "")) paste0(" · ID ", cand$patient_id[i]) else ""
      paste0(cand$scope[i], " · ", format_lab_number(cand$value[i], digits), " ", rv$metadata$unit %||% "", idtxt, " · row ", cand$row_id[i])
    }, character(1))
    choices <- setNames(as.character(cand$row_id), labels)
    tagList(
      radioButtons("subgroup_outlier_action_choice", "What do you want to do with the extreme values detected within subgroups?",
                   choices=c("Retain them after review"="retain",
                             "Exclude the selected values and recalculate"="exclude",
                             "Leave the decision pending"="pending"), selected="pending"),
      conditionalPanel("input.subgroup_outlier_action_choice == 'exclude'",
                       checkboxGroupInput("subgroup_outlier_selected_ids", "Values to exclude", choices=choices, selected=as.character(cand$row_id))),
      selectInput("subgroup_outlier_reason_category", "Primary reason",
                  choices=c("Select…"="", "Confirmed preanalytical error"="preanalytical",
                            "Analytical error / invalid result"="analytical",
                            "Failure to meet selection criteria"="selection",
                            "Legitimate subgroup datum: retain"="legitimate", "Other"="other")),
      textAreaInput("subgroup_outlier_review_reason", "Justification / review outcome", rows=3,
                    placeholder="Document why the value is retained or excluded within the subgroup context."),
      p(class="small-note", "Detection within a subgroup does not by itself justify exclusion. Any exclusion must be based on a demonstrated cause or failure to meet selection criteria; RIs, Lahti, Harris-Boyd, and precision are then recalculated."),
      actionButton("apply_subgroup_outlier_decision", "Apply decision and recalculate", class="btn-primary")
    )
  })

  observeEvent(input$apply_subgroup_outlier_decision, {
    req(rv$prepared, rv$main)
    if (!analysis_idle_or_notify()) return()
    cand <- current_subgroup_outlier_candidates()
    if (is.null(cand) || !nrow(cand)) {
      showNotification("There are no subgroup extreme values pending.", type="message")
      return()
    }
    action <- input$subgroup_outlier_action_choice %||% "pending"
    if (identical(action, "pending")) {
      showNotification("The decision remains pending; the partitioning conclusion remains provisional.", type="warning", duration=8)
      return()
    }
    reason <- trimws(input$subgroup_outlier_review_reason %||% "")
    category <- input$subgroup_outlier_reason_category %||% ""
    if (!nzchar(category) || nchar(reason) < 15) {
      showNotification("Select a reason and document a justification of at least 15 characters.", type="warning", duration=10)
      return()
    }
    if (identical(action, "exclude")) {
      ids <- as.character(input$subgroup_outlier_selected_ids %||% character(0))
      ids <- ids[nzchar(ids)]
      if (!length(ids)) {
        showNotification("Select at least one value to exclude.", type="warning")
        return()
      }
      chosen <- cand[as.character(cand$row_id) %in% ids, , drop=FALSE]
      rv$prepared$data <- rv$prepared$data[!rv$prepared$data$.ri_row_id %in% chosen$row_id, , drop=FALSE]
      decision_label <- "Exclude"
    } else {
      chosen <- cand
      rv$retained_extreme_values <- unique(c(rv$retained_extreme_values, chosen$value))
      decision_label <- "Retain"
    }
    chosen$patient_id <- as.character(chosen$patient_id); chosen$patient_id[is.na(chosen$patient_id)] <- ""
    log_rows <- data.frame(
      timestamp=format(Sys.time(), "%Y-%m-%d %H:%M:%S"), iteration=rv$analysis_iteration %||% 0L,
      row_id=chosen$row_id, patient_id=chosen$patient_id, value=chosen$value,
      scope=chosen$scope, decision=decision_label, category=category, reason=reason, stringsAsFactors=FALSE
    )
    if (is.null(rv$outlier_decisions) || !nrow(rv$outlier_decisions)) rv$outlier_decisions <- log_rows else rv$outlier_decisions <- rbind(rv$outlier_decisions, log_rows)
    refresh_outlier_audit()
    rv$partition_resolution <- NULL
    rv$small_sample_resolution <- NULL
    updateRadioButtons(session, "faculty_decision", selected="pending")
    launch_async_analysis(go_results=TRUE, source_label="Recalculating after subgroup aberrant-value review",
                          completion_action="subgroup_outlier")
  })

  output$partition_status <- renderUI({
    if (is.null(rv$partition)) return(status_html("grey", "Not assessed", "A qualitative variable has not been assessed or is not available."))
    if (!is.null(rv$partition_resolution) && (rv$partition_resolution$decision %||% "") %in% c("partition","common")) {
      dec_txt <- if (identical(rv$partition_resolution$decision, "partition")) "Separate RIs by group" else "Common RI"
      st <- if (isTRUE(rv$final$is_final)) rv$final$status %||% "green" else "yellow"
      return(status_html(st, "Discordance resolved by specialist decision",
                         paste0("Decision recorded: ", dec_txt, ". The automated recommendation and justification are traced separately.")))
    }
    dec <- rv$partition$decision %||% "indeterminate"
    title <- switch(dec,
                    partition = "RIveR recommends partitioning",
                    common = "RIveR recommends a common RI",
                    indeterminate = "A decision cannot yet be made",
                    "Partition not evaluable")
    status_html(rv$partition$status, title, rv$partition$recommendation)
  })
  output$partition_criteria_summary <- renderUI({
    req(rv$partition)
    hb <- rv$partition$harris_boyd %||% NULL
    lahti <- rv$partition$lahti %||% NULL
    if (is.null(hb) && is.null(lahti)) return(NULL)
    blocks <- list()
    if (!is.null(lahti)) {
      ls <- worst_status(lahti$status)
      lstatus <- if (ls == "red") "red" else if (ls == "green") "green" else "yellow"
      ltxt <- if (lstatus == "red") "Supports partitioning" else if (lstatus == "green") "Does not support partitioning" else "Inconclusive"
      blocks <- c(blocks, list(status_html(lstatus, "Lahti criterion", ltxt)))
    }
    if (!is.null(hb)) {
      hstatus <- if (isTRUE(hb$supports_partition)) "red" else "green"
      htxt <- paste0(if (hstatus == "red") "Supports partitioning" else "Does not support partitioning",
                     " · Z=", formatC(hb$z, format="f", digits=2, decimal.mark=","),
                     " · Z*=", formatC(hb$z_critical, format="f", digits=2, decimal.mark=","),
                     " · SD ratio=", formatC(hb$sd_ratio, format="f", digits=2, decimal.mark=","))
      blocks <- c(blocks, list(status_html(hstatus, "Harris–Boyd criterion", htxt)))
    }
    dg <- rv$partition$discordance_guidance %||% NULL
    if (!is.null(dg)) {
      dstatus <- if (isTRUE(dg$shape_caution)) "yellow" else "green"
      fmt_diag <- function(di, lab) paste0(lab, ": Bowley=", ifelse(is.finite(di$bowley), formatC(di$bowley, format="f", digits=2, decimal.mark=","), "—"),
                                              ", excess kurtosis=", ifelse(is.finite(di$excess_kurtosis), formatC(di$excess_kurtosis, format="f", digits=2, decimal.mark=","), "—"),
                                              ", AD p=", ifelse(is.finite(di$normality_p), formatC(di$normality_p, format="g", digits=3, decimal.mark=","), "—"))
      diag_txt <- paste(fmt_diag(dg$group1, friendly_group_label(rv$partition$groups[1], "sex")),
                        fmt_diag(dg$group2, friendly_group_label(rv$partition$groups[2], "sex")), sep=" · ")
      dtxt <- if (isTRUE(dg$shape_caution))
        paste("The shape/tails of at least one group suggest that Harris-Boyd should be interpreted cautiously; it is not considered invalid.", diag_txt) else
        paste("The RIveR operational diagnostic does not identify marked distortion of shape/tails.", diag_txt)
      blocks <- c(blocks, list(status_html(dstatus, "Distributional applicability of Harris–Boyd", dtxt)))
    }
    do.call(tagList, blocks)
  })

  output$partition_pairwise_table <- renderTable({
    req(rv$partition)
    z <- rv$partition$pairwise_table %||% NULL
    if (is.null(z) || !nrow(z)) return(NULL)
    z
  }, striped=TRUE, spacing="s")

  output$partition_table <- renderTable({
    req(rv$partition)
    if (!is.null(rv$partition$group_results) && length(rv$partition$group_results)) {
      d <- rv$main$display_digits %||% 2
      rows <- lapply(names(rv$partition$group_results), function(g) {
        fit <- rv$partition$group_results[[g]]
        data.frame(Category=g, n=fit$n %||% NA_integer_,
                   `Group limit / RI`=if (!is.null(fit$ri)) reference_result_text(fit$ri, fit$reference_design %||% rv$metadata$reference_design, fit$display_digits %||% d) else "Not estimated",
                   `Precision`=if (isTRUE(fit$precision_ok)) "Adequate" else "Review",
                   check.names=FALSE, stringsAsFactors=FALSE)
      })
      return(do.call(rbind, rows))
    }
    if (!is.null(rv$partition$lahti)) {
      friendly <- direct_partition_display_table(rv$partition)
      if (!is.null(friendly)) return(friendly)
    }
    if (!is.null(rv$partition$ri_group1)) {
      return(data.frame(Group=rv$partition$groups,
                        LRL=c(rv$partition$ri_group1[1], rv$partition$ri_group2[1]),
                        URL=c(rv$partition$ri_group1[2], rv$partition$ri_group2[2])))
    }
    data.frame(Detail=rv$partition$note %||% "No table available")
  }, striped=TRUE, spacing="s")

  output$partition_resolution_ui <- renderUI({
    req(rv$partition)
    if (isTRUE(rv$partition$subgroup_outlier_pending)) {
      return(status_html("yellow", "Resolve subgroup aberrant values first",
                         "Discordance or the partitioning proposal should not be resolved by specialist decision until the extreme values detected within subgroups have been reviewed and the calculations repeated."))
    }
    pdec <- rv$partition$decision %||% "not_evaluable"
    if (!pdec %in% c("partition", "common", "indeterminate") || !identical(rv$main$type %||% "", "direct_establishment")) return(NULL)
    dg <- rv$partition$discordance_guidance %||% list(favored="review", text="")
    favored <- if (pdec %in% c("partition","common")) pdec else dg$favored %||% "review"
    favored_txt <- switch(favored,
                          partition = if (pdec == "partition") "RIveR recommends group-specific partitioning." else "RIveR conditionally favors group-specific partitioning.",
                          common = if (pdec == "common") "RIveR recommends retaining a common RI." else "RIveR conditionally favors retaining a common RI.",
                          "RIveR does not automatically prioritize either alternative.")
    current <- rv$partition_resolution$decision %||% "pending"
    current_reason <- rv$partition_resolution$reason %||% ""
    current_cat <- rv$partition_resolution$category %||% ""
    d <- rv$main$display_digits %||% 2
    common_txt <- if (!is.null(rv$main$ri)) paste0("Retain common RI: ", format_lab_interval(rv$main$ri, d), ". Preserves the classification impact observed in the Lahti table.") else "Retain common RI."
    split_txt <- if (!is.null(rv$partition$group_results) && length(rv$partition$group_results)) {
      bits <- vapply(names(rv$partition$group_results), function(g) {
        fit <- rv$partition$group_results[[g]]
        if (is.null(fit) || is.null(fit$ri)) return(paste0(g, ": not estimated"))
        paste0(g, " ", format_lab_interval(fit$ri, fit$display_digits %||% d))
      }, character(1))
      paste0("Adopt category-specific RIs: ", paste(bits, collapse="; "), ".")
    } else if (!is.null(rv$partition$group1) && !is.null(rv$partition$group2)) {
      paste0("Adopt separate RIs: ", friendly_group_label(rv$partition$groups[1], "sex"), " ", format_lab_interval(rv$partition$group1$ri, rv$partition$group1$display_digits %||% d),
             "; ", friendly_group_label(rv$partition$groups[2], "sex"), " ", format_lab_interval(rv$partition$group2$ri, rv$partition$group2$display_digits %||% d), ".")
    } else "Adopt separate RIs by group."
    tagList(
      hr(), h4(if (pdec == "indeterminate") "Resolve partitioning discordance" else "Specialist closure of partitioning"),
      status_html(if (pdec == "indeterminate") "yellow" else "green", "What should I do?", paste(favored_txt, if (pdec == "indeterminate") dg$text %||% "" else rv$partition$recommendation %||% "")),
      fluidRow(column(6, status_html("grey", "Alternative A · common RI", common_txt)),
               column(6, status_html("grey", "Alternative B · separate RIs", split_txt))),
      p(class="small-note", "The decision changes the final report, not the data or statistical calculations. It must be justified by biological/clinical plausibility, classification impact, and/or external evidence."),
      radioButtons("partition_resolution_choice", "Specialist decision",
                   choices = c("Adopt separate RIs by group"="partition",
                               "Retain a common RI"="common",
                               "Do not close the study yet"="pending"), selected=current),
      selectInput("partition_reason_category", "Primary rationale",
                  choices=c("Select…"="", "Biological / physiological plausibility"="biological",
                            "Classification / clinical impact"="clinical", "Distribution shape / tails"="distribution",
                            "Literature / external consensus"="literature", "Operational / LIS limitation"="operational", "Other"="other"),
                  selected=current_cat),
      textAreaInput("partition_resolution_reason", "Specialist justification", value=current_reason, rows=3,
                    placeholder="Document why you adopt separate RIs, retain the common RI, or leave the study pending."),
      actionButton("apply_partition_resolution", "Record the decision and prepare the final report", class="btn-primary")
    )
  })

  observeEvent(input$apply_partition_resolution, {
    req(rv$partition, rv$main)
    if (isTRUE(rv$partition$subgroup_outlier_pending)) {
      showNotification("Review the extreme values detected within subgroups first.", type="warning", duration=10)
      return()
    }
    pdec <- rv$partition$decision %||% "not_evaluable"
    if (!pdec %in% c("partition", "common", "indeterminate")) return()
    choice <- input$partition_resolution_choice %||% "pending"
    if (identical(choice, "pending")) {
      rv$partition_resolution <- NULL
      rv$final <- compose_final_recommendation(rv$main, rv$partition, rv$age, NULL, rv$small_sample_resolution)
      showNotification("The study remains provisional. No final partitioning decision has been issued.", type="warning", duration=10)
      return()
    }
    category <- input$partition_reason_category %||% ""
    reason <- trimws(input$partition_resolution_reason %||% "")
    if (!nzchar(category) || nchar(reason) < 20) {
      showNotification("Select the rationale and document a justification of at least 20 characters.", type="warning", duration=10)
      return()
    }
    favored <- if (pdec %in% c("partition","common")) pdec else rv$partition$discordance_guidance$favored %||% "review"
    system_text <- if (pdec == "indeterminate") rv$partition$discordance_guidance$text %||% "" else rv$partition$recommendation %||% ""
    resolution <- list(decision=choice, timestamp=format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                       category=category, reason=reason, system_favored=favored,
                       system_text=system_text)
    rv$partition_resolution <- resolution
    logrow <- data.frame(timestamp=resolution$timestamp,
                         decision=if (choice=="partition") "Separate RIs" else "Common RI",
                         category=category, reason=reason,
                         system_favored=favored,
                         concordant=if (favored %in% c("partition","common")) choice==favored else NA,
                         stringsAsFactors=FALSE)
    if (is.null(rv$partition_decisions) || !nrow(rv$partition_decisions)) rv$partition_decisions <- logrow else rv$partition_decisions <- rbind(rv$partition_decisions, logrow)
    rv$final <- compose_final_recommendation(rv$main, rv$partition, rv$age, rv$partition_resolution, rv$small_sample_resolution)
    updateRadioButtons(session, "faculty_decision", selected=if (isTRUE(logrow$concordant)) "accept" else "override")
    if (!isTRUE(logrow$concordant)) updateTextAreaInput(session, "faculty_reason", value=reason)
    goto("Recommendation")
    showNotification(if (isTRUE(rv$final$is_final)) "Decision recorded. The report generated now will be marked as FINAL." else "Decision recorded, but blockers remain before the study can be closed.",
                     type=if (isTRUE(rv$final$is_final)) "message" else "warning", duration=12)
  })

  output$age_status <- renderUI({
    if (is.null(rv$age)) return(status_html("grey", "Quantitative variable · not assessed", "No quantitative variable has been assessed or is available."))
    dec <- rv$age$decision %||% "indeterminate"
    qlab <- quantitative_label(rv$age)
    design <- normalize_reference_design(rv$age$reference_design %||% rv$main$reference_design %||% rv$metadata$reference_design)
    base_term <- if (identical(design$tail, "two_sided")) "RI" else reference_limit_name(design)
    title <- switch(dec,
                    partition = paste0("Partitioning by quantitative variable validated · ", qlab),
                    common = paste0(base_term, " common with respect to ", qlab),
                    continuous = paste0("Continuous ", tolower(base_term), " by ", qlab),
                    indeterminate = paste0("Dependence on ", qlab, " unresolved"),
                    not_evaluable = paste0(qlab, " · not evaluable"),
                    paste0(qlab, " · not evaluable"))
    status_html(rv$age$status, title, rv$age$recommendation)
  })

  output$age_has_model <- reactive({
    !is.null(rv$age$model) && isTRUE(rv$age$model$ok)
  })
  outputOptions(output, "age_has_model", suspendWhenHidden = FALSE)

  output$age_diagnostics_table <- renderTable({
    req(rv$age)
    genericize_quantitative_table(age_diagnostics_display_table(rv$age), rv$age)
  }, striped=TRUE, bordered=FALSE, spacing="s")

  output$age_cut_summary <- renderUI({
    req(rv$age)
    if (!isTRUE(rv$age$step_like) || !is.finite(rv$age$selected_cut %||% NA_real_)) return(NULL)
    cv <- rv$age$cut_validation %||% NULL
    rp <- rv$age$rpart_cut %||% NA_real_
    rf <- rv$age$cut_refinement %||% NULL
    qlab <- quantitative_label(rv$age)
    suffix <- if (quantitative_age_like(rv$age)) " years" else ""
    cut_txt <- paste0(formatC(rv$age$selected_cut, format="f", digits=2, decimal.mark=","), suffix)
    rpart_txt <- if (is.finite(rp)) paste0(formatC(rp, format="f", digits=2, decimal.mark=","), suffix) else "—"
    op <- rv$age$operational_cut_info %||% NULL
    op_txt <- if (!is.null(op) && isTRUE(op$preserves_groups)) paste0(" Equivalent operational cut-point: ", op$display, ".") else ""
    refine_txt <- if (!is.null(rf) && isTRUE(rf$ok))
      paste0("rpart delimited the region around ", rpart_txt,
             "; robust search over observed boundaries of ", qlab, " refined the statistical cut-point to ", cut_txt, ".", op_txt)
    else paste0("The candidate cut-point of ", qlab, " is ", cut_txt, ".", op_txt)
    if (!is.null(cv) && isTRUE(cv$boundary_outlier_pending)) {
      return(status_html("yellow", paste0("Review cut-point location: ", cut_txt, " years"),
                         paste0(refine_txt, " Possible observation misclassified because of cut-point location. Review the cut-point first before considering exclusion.")))
    }
    if (!is.null(cv) && identical(cv$decision, "partition")) {
      return(status_html("green", paste0("Refined and validated statistical cut-point: ", cut_txt, " years"),
                         paste0(refine_txt, " It was subsequently assessed against direct RIs by segment, Lahti, Harris-Boyd, and precision.")))
    }
    status_html("yellow", paste0("Refined statistical cut-point: ", cut_txt, " years"),
                paste0(refine_txt, " The pattern appears concentrated, but the cut-point did not meet all requirements for recommending automatic partitioning."))
  })

  output$age_cut_validation_table <- renderTable({
    req(rv$age)
    if (!isTRUE(rv$age$step_like) || is.null(rv$age$cut_validation)) return(NULL)
    genericize_quantitative_table(age_cut_validation_display_table(rv$age), rv$age)
  }, striped=TRUE, bordered=FALSE, spacing="s")

  output$age_boundary_table <- renderTable({
    req(rv$age)
    genericize_quantitative_table(age_boundary_outlier_display_table(rv$age), rv$age)
  }, striped=TRUE, bordered=FALSE, spacing="s")

  output$age_partition_table <- renderTable({
    req(rv$age)
    genericize_quantitative_table(age_partition_display_table(rv$age), rv$age)
  }, striped=TRUE, bordered=FALSE, spacing="s")

  output$age_sil_status <- renderUI({
    req(rv$age)
    if (!identical(rv$age$decision %||% "", "continuous")) return(NULL)
    a <- rv$age$sil_adaptation %||% NULL
    if (is.null(a) || !isTRUE(a$ok)) {
      return(status_html("yellow", "LIS adaptation pending",
                         a$reason %||% "A discrete approximation of the continuous model could not be derived."))
    }
    title <- if (isTRUE(a$acceptable)) "LIS adaptation available" else "Discrete adaptation with limitations"
    qlab <- quantitative_label(rv$age)
    design <- normalize_reference_design(rv$age$reference_design %||% rv$main$reference_design %||% rv$metadata$reference_design)
    model_term <- if (identical(design$tail, "two_sided")) "continuous RI" else paste0("continuous ", reference_limit_name(design), " (", reference_design_percentile_text(design), ")")
    txt <- paste0(
      "Preferred model: ", model_term, " dependent on ", qlab, ". If the LIS supports a table parameterized by this variable, use only the active limit(s) derived from the GAMLSS model. ",
      "If only ranges are supported, RIveR proposes the minimum discretization shown below. ",
      a$recommendation %||% ""
    )
    status_html(a$status %||% "yellow", title, txt)
  })

  output$age_sil_table <- renderTable({
    req(rv$age)
    if (!identical(rv$age$decision %||% "", "continuous")) return(NULL)
    genericize_quantitative_table(age_sil_display_table(rv$age), rv$age)
  }, striped=TRUE, bordered=FALSE, spacing="s")

  output$age_sil_tradeoff_title <- renderUI({
    req(rv$age)
    tab <- age_sil_tradeoff_display_table(rv$age)
    if (is.null(tab) || !nrow(tab)) return(NULL)
    tagList(
      h5("Trade-off between number of segments, error, and coverage"),
      p(class="small-note", paste0("RIveR displays all evaluable solutions. The internal criterion simultaneously requires current maximum error ≤10% for the active limit(s) and overall coverage compatible with the expected ", round(100 * (rv$age$sil_adaptation$expected_coverage %||% 0.95), 1), "% using two-sided exact binomial tests with Holm adjustment. Individual 95% CIs are retained as range-level diagnostics; if the current error slightly exceeds 10% but rounding to one decimal displays 10.0%, the solution is flagged as borderline."))
    )
  })

  output$age_sil_tradeoff_table <- renderTable({
    req(rv$age)
    if (!identical(rv$age$decision %||% "", "continuous")) return(NULL)
    genericize_quantitative_table(age_sil_tradeoff_display_table(rv$age), rv$age)
  }, striped=TRUE, bordered=FALSE, spacing="s")

  output$age_sil_download_ui <- renderUI({
    req(rv$age)
    a <- rv$age$sil_adaptation %||% NULL
    if (!identical(rv$age$decision %||% "", "continuous") || is.null(a$annual_table)) return(NULL)
    tagList(
      p(class="small-note", paste0(
        "The parameterized table by ", quantitative_label(rv$age), " preserves the continuous model and avoids creating artificial steps. Discrete ranges are an operational approximation; they do not imply biological subpopulations.")),
      downloadButton("download_age_sil_table", if (quantitative_age_like(rv$age)) "Download annual table for the LIS (CSV)" else "Download table for the LIS (CSV)")
    )
  })

  output$age_plot <- renderPlot({
    req(rv$age$bins)
    design <- normalize_reference_design(rv$age$reference_design %||% rv$main$reference_design %||% rv$metadata$reference_design)
    qlab <- quantitative_label(rv$age)
    xlab <- quantitative_axis_label(rv$age)
    if (!is.null(rv$age$continuous) && !is.null(rv$age$continuous$curves)) {
      c <- rv$age$continuous$curves
      if (identical(design$tail, "two_sided")) {
        yr <- range(c$p025, c$p975, rv$age$bins$p025, rv$age$bins$p975, na.rm=TRUE)
        plot(c$age, c$p50, type="l", lwd=2, xlab=xlab, ylab="Result", ylim=yr,
             main=paste0(reference_percentile_label(design$pair_percentiles[["lower"]]), " / P50 / ", reference_percentile_label(design$pair_percentiles[["upper"]]), " according to ", qlab))
        lines(c$age, c$p025, lty=2, lwd=2); lines(c$age, c$p975, lty=2, lwd=2)
        points(rv$age$bins$age_mid, rv$age$bins$p50, pch=19, cex=.65)
        points(rv$age$bins$age_mid, rv$age$bins$p025, pch=1, cex=.6); points(rv$age$bins$age_mid, rv$age$bins$p975, pch=1, cex=.6)
        if (is.finite(rv$age$selected_cut %||% NA_real_)) abline(v=rv$age$selected_cut, lty=3, lwd=2)
        legend("topleft", legend=c("P50 model", paste0(reference_design_percentile_text(design), " model"), "Percentiles by ranges", "Candidate cut-point"), lty=c(1,2,NA,3), pch=c(NA,NA,19,NA), bty="n", cex=.8)
      } else {
        y <- if (isTRUE(design$active[["lower"]])) c$p025 else c$p975
        yp <- if (isTRUE(design$active[["lower"]])) rv$age$bins$p025 else rv$age$bins$p975
        plot(c$age, y, type="l", lwd=2, xlab=xlab, ylab="Result", main=paste0(reference_limit_name(design), " according to ", qlab))
        points(rv$age$bins$age_mid, yp, pch=19, cex=.65)
        if (is.finite(rv$age$selected_cut %||% NA_real_)) abline(v=rv$age$selected_cut, lty=3, lwd=2)
      }
    } else {
      b <- rv$age$bins
      if (identical(design$tail, "two_sided")) {
        yr <- range(b$p025, b$p975, na.rm=TRUE)
        plot(b$age_mid, b$p50, type="b", pch=19, xlab=xlab, ylab="Result", ylim=yr, main=paste0("Percentiles by ", qlab, " ranges"))
        lines(b$age_mid, b$p025, type="b", pch=1, lty=2); lines(b$age_mid, b$p975, type="b", pch=1, lty=2)
      } else {
        y <- if (isTRUE(design$active[["lower"]])) b$p025 else b$p975
        plot(b$age_mid, y, type="b", pch=19, xlab=xlab, ylab="Result", main=paste0(reference_limit_name(design), " by ", qlab, " ranges"))
      }
    }
  })

  output$age_residual_age_plot <- renderPlot({
    req(rv$age$model, isTRUE(rv$age$model$ok))
    m <- rv$age$model
    qlab <- quantitative_label(rv$age)
    plot(m$data$age, m$residuals, pch=16, cex=.55, xlab=quantitative_axis_label(rv$age), ylab="Normalized residual",
         main=paste0("Residuals vs ", qlab))
    abline(h=0, lty=2)
    ok <- is.finite(m$data$age) & is.finite(m$residuals)
    if (sum(ok) >= 20) lines(stats::lowess(m$data$age[ok], m$residuals[ok], f=.45), lwd=2)
  })

  output$age_residual_fitted_plot <- renderPlot({
    req(rv$age$model, isTRUE(rv$age$model$ok))
    m <- rv$age$model
    plot(m$fitted, m$residuals, pch=16, cex=.55, xlab="Fitted value", ylab="Normalized residual",
         main="Residuals vs fitted values")
    abline(h=0, lty=2)
    ok <- is.finite(m$fitted) & is.finite(m$residuals)
    if (sum(ok) >= 20) lines(stats::lowess(m$fitted[ok], m$residuals[ok], f=.45), lwd=2)
  })


  final_recommended_payload <- reactive({
    req(rv$main)
    f <- rv$final %||% compose_final_recommendation(rv$main, rv$partition, rv$age, rv$partition_resolution, rv$small_sample_resolution)
    brand_visible_object(rilctms_recommended_result_payload(rv$main, rv$partition, rv$age, f))
  })

  output$final_status <- renderUI({
    req(rv$main)
    f <- rv$final %||% compose_final_recommendation(rv$main, rv$partition, rv$age, rv$partition_resolution, rv$small_sample_resolution)
    status_html(f$status %||% "grey", f$headline %||% status_label(f$status %||% "grey"), f$action %||% f$text %||% "")
  })

  output$final_recommended_result <- renderUI({
    z <- final_recommended_payload()
    if (is.null(z) || !isTRUE(z$show)) return(NULL)
    if (!isTRUE(z$has_values)) {
      return(status_html(z$status %||% "yellow", z$title %||% "No recommended numeric result", z$note %||% ""))
    }
    tagList(
      h4("Result that RIveR proposes for approval"),
      h5(z$title %||% "Recommended result"),
      p(class="small-note", z$note %||% "")
    )
  })

  output$final_recommended_table <- renderTable({
    z <- final_recommended_payload()
    if (is.null(z) || !isTRUE(z$show) || !isTRUE(z$has_values) || is.null(z$table)) return(NULL)
    z$table
  }, striped = TRUE, spacing = "s")

  output$final_recommended_secondary <- renderUI({
    z <- final_recommended_payload()
    if (is.null(z) || is.null(z$secondary_table) || !is.data.frame(z$secondary_table) || !nrow(z$secondary_table)) return(NULL)
    tagList(
      h5(z$secondary_title %||% "Continuous model GAMLSS"),
      p(class="small-note", z$secondary_note %||% "")
    )
  })

  output$final_recommended_secondary_table <- renderTable({
    z <- final_recommended_payload()
    if (is.null(z) || is.null(z$secondary_table) || !is.data.frame(z$secondary_table) || !nrow(z$secondary_table)) return(NULL)
    z$secondary_table
  }, striped = TRUE, spacing = "s")

  output$final_notes <- renderUI({
    req(rv$main)
    f <- rv$final %||% compose_final_recommendation(rv$main, rv$partition, rv$age, rv$partition_resolution, rv$small_sample_resolution)
    notes <- f$notes %||% character(0)
    if (!length(notes)) return(NULL)
    tags$ul(lapply(notes, tags$li))
  })

  output$download_report <- downloadHandler(
    filename = function() paste0("RIveR_", gsub("[^A-Za-z0-9_-]", "_", rv$metadata$analyte %||% "study"), "_", Sys.Date(), ".html"),
    content = function(file) {
      state <- list(metadata = rv$metadata, faculty_decision = input$faculty_decision,
                    faculty_reason = input$faculty_reason, app_version = APP_VERSION,
                    audit = rv$prepared$audit %||% NULL,
                    prepared_data = rv$prepared$data %||% NULL,
                    outlier_baseline_data = rv$outlier_baseline_data %||% NULL,
                    outlier_decisions = rv$outlier_decisions %||% data.frame(),
                    outlier_impacts = rv$outlier_impacts %||% data.frame(),
                    partition_resolution = rv$partition_resolution,
                    partition_decisions = rv$partition_decisions %||% data.frame(),
                    small_sample_resolution = rv$small_sample_resolution,
                    small_sample_decisions = rv$small_sample_decisions %||% data.frame(),
                    analysis_iteration = rv$analysis_iteration %||% 0L)
      write_html_report(file, state, rv$main, rv$partition, rv$age, rv$final)
    }
  )
  output$download_pdf <- downloadHandler(
    filename = function() paste0("RIveR_", gsub("[^A-Za-z0-9_-]", "_", rv$metadata$analyte %||% "study"), "_", Sys.Date(), ".pdf"),
    content = function(file) {
      state <- list(metadata = rv$metadata, faculty_decision = input$faculty_decision,
                    faculty_reason = input$faculty_reason, app_version = APP_VERSION,
                    audit = rv$prepared$audit %||% NULL,
                    prepared_data = rv$prepared$data %||% NULL,
                    outlier_baseline_data = rv$outlier_baseline_data %||% NULL,
                    outlier_decisions = rv$outlier_decisions %||% data.frame(),
                    outlier_impacts = rv$outlier_impacts %||% data.frame(),
                    partition_resolution = rv$partition_resolution,
                    partition_decisions = rv$partition_decisions %||% data.frame(),
                    small_sample_resolution = rv$small_sample_resolution,
                    small_sample_decisions = rv$small_sample_decisions %||% data.frame(),
                    analysis_iteration = rv$analysis_iteration %||% 0L)
      write_visual_pdf_report(file, state, rv$main, rv$partition, rv$age, rv$final)
    }
  )

  output$download_age_sil_table <- downloadHandler(
    filename = function() paste0("RIveR_RI_quantitative_variable_LIS_", Sys.Date(), ".csv"),
    content = function(file) {
      req(rv$age)
      tab <- rv$age$sil_adaptation$annual_table %||% NULL
      if (is.null(tab) || !nrow(tab)) stop("No annual table is available.")
      out <- tab
      digits <- rv$main$display_digits %||% 2
# v1.0.0: the one-sided table contains only P5 or P95; LRL+URL are not forced.
      for (nm in names(out)) if (is.numeric(out[[nm]]) && !identical(nm, names(out)[1])) out[[nm]] <- round(out[[nm]], digits)
      utils::write.csv(out, file, row.names = FALSE, na = "")
    }
  )

  output$download_clean <- downloadHandler(
    filename = function() paste0("RIveR_prepared_dataset_", Sys.Date(), ".csv"),
    content = function(file) {
      d <- rv$prepared$data
      if (identical(rv$main$type %||% "", "direct_verification") && isTRUE(rv$main$partitioned)) {
        ass <- tryCatch(assign_direct_verification_partitions(d, rv$main$definitions), error = function(e) NULL)
        if (!is.null(ass) && isTRUE(ass$ok)) {
          d$verification_partition <- ass$label
          d$verification_round <- 1L
          extra <- list(d)
          for (key in names(rv$partition_second_data %||% list())) {
            d2 <- rv$partition_second_data[[key]]
            lab <- rv$main$partition_results[[key]]$partition_label %||% key
            d2$verification_partition <- lab
            d2$verification_round <- 2L
            extra[[length(extra) + 1L]] <- d2
          }
          all_names <- unique(unlist(lapply(extra, names), use.names = FALSE))
          extra <- lapply(extra, function(z) {
            for (nm in setdiff(all_names, names(z))) z[[nm]] <- NA
            z[, all_names, drop = FALSE]
          })
          d <- do.call(rbind, extra)
        }
      }
      if (".ri_row_id" %in% names(d)) d$.ri_row_id <- NULL
      utils::write.csv(d, file, row.names = FALSE, na = "")
    }
  )

  observeEvent(input$new_study, {
    if (isTRUE(rv$analysis_running)) {
      showNotification("An analysis is in progress. Wait for it to finish before starting a new study.", type="warning", duration=8)
      return()
    }
    old_job <- rv$analysis_job_dir
    if (!is.null(old_job) && dir.exists(old_job)) {
      zold <- tryCatch(rilctms_job_state(old_job), error=function(e) NULL)
      if (!is.null(zold) && !identical(zold$state, "running")) try(rilctms_mark_job_consumed(old_job, "Study closed when starting a new study"), silent=TRUE)
    }
    rv$raw <- NULL; rv$raw_name <- NULL; rv$prepared <- NULL; rv$quality <- NULL; rv$route <- NULL
    rv$main <- NULL; rv$partition <- NULL; rv$age <- NULL; rv$final <- NULL
    reset_verification_followup()
    reset_outlier_review()
    rv$analysis_status <- NULL; rv$analysis_started <- NULL; rv$analysis_status_mtime <- as.POSIXct(NA); rv$analysis_job_dir <- NULL
    rv$analysis_job_id <- NULL; rv$analysis_signature <- NULL; rv$analysis_completion <- NULL
    rv$analysis_recovered <- FALSE; rv$analysis_last_liveness_check <- as.POSIXct(NA); rv$analysis_dead_seen_at <- as.POSIXct(NA)
    rv$recovery_inputs <- list(); rv$analysis_rehydrating <- FALSE; rv$session_handled_job_ids <- character(0)
    rv$recovery_loading <- FALSE; rv$recovery_job_dir <- NULL; rv$recovery_job_id <- NULL; rv$recovery_expected_state <- NULL
    rv$recovery_error <- NULL; rv$recovery_applied_key <- NULL; rv$defer_heavy_diagnostics <- FALSE
    rv$recoverable_job <- tryCatch(rilctms_latest_recoverable_job(), error=function(e) NULL)
    goto("Start")
  })

  session$onSessionEnded(function() {
# v0.17.1: a web disconnection does NOT cancel the job. Only a trace is left
# in the manifest; the external process continues until completion or until
# the user explicitly presses “Cancel analysis”.
    jobdir <- isolate(rv$analysis_job_dir)
    if (isTRUE(isolate(rv$analysis_running)) && !is.null(jobdir) && dir.exists(jobdir)) {
      try(rilctms_update_job_manifest(jobdir, last_ui_disconnect_at = Sys.time()), silent=TRUE)
    }
  })
}

shinyApp(ui, server)
