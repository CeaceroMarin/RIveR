`%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || (length(x) == 1 && is.na(x))) y else x

pkg_available <- function(pkg) {
  requireNamespace(pkg, quietly = TRUE)
}

pkg_version_at_least <- function(pkg, version) {
  if (!pkg_available(pkg)) return(FALSE)
  tryCatch(utils::packageVersion(pkg) >= package_version(version), error = function(e) FALSE)
}

fmt_integer <- function(x) {
  x <- suppressWarnings(as.integer(round(as.numeric(x))))
  if (!length(x) || is.na(x[1])) return("")
  format(x[1], big.mark = ".", decimal.mark = ",", scientific = FALSE, trim = TRUE)
}


infer_decimal_places <- function(x, max_decimals = 4) {
  x <- x[is.finite(x)]
  if (!length(x)) return(2L)
  for (d in 0:max_decimals) {
    tol <- max(1e-10, 10^(-(d + 7)))
    if (all(abs(x - round(x, d)) <= tol)) return(as.integer(d))
  }
  as.integer(max_decimals)
}

format_lab_number <- function(x, digits = 2, decimal_mark = ",") {
  ifelse(is.finite(as.numeric(x)),
         formatC(as.numeric(x), format = "f", digits = digits, decimal.mark = decimal_mark),
         "—")
}

format_lab_interval <- function(ri, digits = 2, decimal_mark = ",") {
  if (is.null(ri) || length(ri) < 2 || any(!is.finite(as.numeric(ri[1:2])))) return("—")
  paste(format_lab_number(as.numeric(ri[1]), digits, decimal_mark),
        format_lab_number(as.numeric(ri[2]), digits, decimal_mark), sep = " – ")
}

format_percent <- function(x, digits = 1) {
  ifelse(is.finite(as.numeric(x)),
         paste0(formatC(100 * as.numeric(x), format = "f", digits = digits, decimal.mark = ","), " %"),
         "—")
}

format_p_value <- function(p, digits = 3, decimal_mark = ",") {
  p <- suppressWarnings(as.numeric(p))
  if (!length(p) || !is.finite(p[1])) return("—")
  p <- p[1]
  if (p > 0.999) return(">0,999")
  if (p < 0.001) return("<0,001")
  formatC(p, format = "f", digits = digits, decimal.mark = decimal_mark)
}

safe_num <- function(x) {
  suppressWarnings(as.numeric(gsub(",", ".", trimws(as.character(x)), fixed = TRUE)))
}

read_lab_file <- function(path, name) {
  ext <- tolower(tools::file_ext(name))
  if (ext %in% c("csv", "txt")) {
    # Try semicolon first (common in Spanish Excel exports), then comma.
    dat <- tryCatch(utils::read.csv2(path, stringsAsFactors = FALSE, check.names = FALSE), error = function(e) NULL)
    if (is.null(dat) || ncol(dat) <= 1) {
      dat <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
    }
    return(dat)
  }
  if (ext %in% c("xlsx", "xls")) {
    if (!pkg_available("readxl")) stop("The 'readxl' package is required to read Excel files.")
    return(as.data.frame(readxl::read_excel(path), check.names = FALSE))
  }
  stop("Unsupported format. Use CSV, TXT, XLSX, or XLS.")
}

status_rank <- function(status) {
  match(status, c("green", "yellow", "red", "grey")) %||% 4L
}

worst_status <- function(x) {
  if (length(x) == 0) return("grey")
  if (any(x == "red", na.rm = TRUE)) return("red")
  if (any(x == "yellow", na.rm = TRUE)) return("yellow")
  if (all(x == "green", na.rm = TRUE)) return("green")
  "grey"
}

status_label <- function(status) {
  switch(status,
         green = "RECOMMENDED / ACCEPTABLE",
         yellow = "CONDITIONAL / REQUIRES REVIEW",
         red = "NOT RECOMMENDED",
         grey = "NOT EVALUABLE",
         "NOT EVALUABLE")
}

status_symbol_text <- function(status, text = NULL) {
  symbol <- switch(status,
                   green = "🟢",
                   yellow = "🟡",
                   red = "🔴",
                   grey = "⚪",
                   "⚪")
  if (is.null(text) || !nzchar(as.character(text))) symbol else paste(symbol, text)
}

status_html <- function(status, title = NULL, text = NULL) {
  # The dot is colored pathway CSS, avoiding dependence on emoji fonts in the UI.
  icon <- "●"
  cls <- paste0("status-card status-", status)
  shiny::div(class = cls,
             shiny::div(class = "status-title", paste(icon, title %||% status_label(status))),
             if (!is.null(text)) shiny::div(class = "status-text", text))
}

bowley_skewness <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) < 4) return(NA_real_)
  q <- stats::quantile(x, c(.25, .50, .75), na.rm = TRUE, names = FALSE, type = 7)
  den <- q[3] - q[1]
  if (!is.finite(den) || den == 0) return(0)
  (q[3] + q[1] - 2 * q[2]) / den
}

normality_p <- function(x) {
  x <- x[is.finite(x)]
  n <- length(x)
  if (n < 4 || !is.finite(stats::sd(x)) || stats::sd(x) <= 0) return(NA_real_)
  z <- sort((x - mean(x)) / stats::sd(x))
  Fz <- pmin(pmax(stats::pnorm(z), 1e-12), 1 - 1e-12)
  i <- seq_len(n)
  a2 <- -n - mean((2 * i - 1) * (log(Fz) + log(1 - rev(Fz))))
  a <- a2 * (1 + 0.75/n + 2.25/n^2)
  p <- if (a < .2) 1-exp(-13.436+101.14*a-223.73*a^2) else if (a < .34) 1-exp(-8.318+42.796*a-59.938*a^2) else if (a < .6) exp(.9177-4.279*a-1.38*a^2) else exp(1.2937-5.709*a+.0186*a^2)
  max(0, min(1, p))
}




# Extreme-value visualization -----------------------------------------------
# The box plot is a visual aid. Formal criteria remain
# Dixon/Reed + Tukey, and the exclusion decision remains subject to specialist review.
outlier_plot_flags <- function(x) {
  z <- suppressWarnings(as.numeric(x))
  n <- length(z)
  out <- data.frame(suspect = rep(FALSE, n), extreme = rep(FALSE, n), dixon = rep(FALSE, n))
  ok <- is.finite(z)
  if (sum(ok) < 3) return(out)
  zz <- z[ok]
  tk <- tukey_outlier_assessment(zz)
  dr <- dixon_reed_one_third(zz)
  tol <- max(1e-10, .Machine$double.eps * max(abs(zz), 1, na.rm = TRUE) * 100)

  if (is.finite(tk$lower_inner) && is.finite(tk$upper_inner) &&
      is.finite(tk$lower_outer) && is.finite(tk$upper_outer)) {
    out$suspect[ok] <- (zz < tk$lower_inner - tol & zz >= tk$lower_outer - tol) |
                       (zz > tk$upper_inner + tol & zz <= tk$upper_outer + tol)
    out$extreme[ok] <- zz < tk$lower_outer - tol | zz > tk$upper_outer + tol
  }
  if (isTRUE(dr$lower_flag)) out$dixon[ok] <- out$dixon[ok] | abs(zz - dr$lower_value) <= tol
  if (isTRUE(dr$upper_flag)) out$dixon[ok] <- out$dixon[ok] | abs(zz - dr$upper_value) <= tol
  out
}

plot_outlier_boxplot <- function(data, group = NULL, main = "Extreme-value box plot",
                                 ylab = "Result", marked_row_ids = integer(0),
                                 digits = NULL) {
  if (is.null(data) || !is.data.frame(data) || !"value" %in% names(data)) {
    return(diagnostic_empty_plot("No data are available for the box plot"))
  }
  d <- data[is.finite(data$value), , drop = FALSE]
  if (!nrow(d)) return(diagnostic_empty_plot("No data are available for the box plot"))
  if (is.null(digits)) digits <- infer_decimal_places(d$value)
  if (is.null(group)) group <- rep("Complete population", nrow(d))
  group <- as.character(group)
  group[is.na(group) | !nzchar(trimws(group))] <- "Not reported"
  f <- factor(group, levels = unique(group))

  old <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(old), add = TRUE)
  graphics::par(mar = c(5.2, 4.5, 3.6, 1.2))
  graphics::boxplot(d$value ~ f, outline = FALSE, xlab = "", ylab = ylab,
                    main = main, las = 1)

  any_sus <- any_ext <- any_dix <- FALSE
  marked <- rep(FALSE, nrow(d))
  if (".ri_row_id" %in% names(d) && length(marked_row_ids)) {
    marked <- as.character(d$.ri_row_id) %in% as.character(marked_row_ids)
  }

  lev <- levels(f)
  for (i in seq_along(lev)) {
    idx <- which(f == lev[i])
    fl <- outlier_plot_flags(d$value[idx])
    if (any(fl$suspect)) {
      any_sus <- TRUE
      graphics::points(rep(i, sum(fl$suspect)), d$value[idx][fl$suspect], pch = 1, cex = 1.05)
    }
    if (any(fl$extreme)) {
      any_ext <- TRUE
      graphics::points(rep(i, sum(fl$extreme)), d$value[idx][fl$extreme], pch = 4, cex = 1.15, lwd = 1.5)
    }
    if (any(fl$dixon)) {
      any_dix <- TRUE
      graphics::points(rep(i, sum(fl$dixon)), d$value[idx][fl$dixon], pch = 8, cex = 1.15)
    }
    mk <- marked[idx]
    if (any(mk)) {
      graphics::points(rep(i, sum(mk)), d$value[idx][mk], pch = 17, cex = 1.15)
    }
    lab <- fl$extreme | fl$dixon | mk
    if (any(lab)) {
      vals <- d$value[idx][lab]
      graphics::text(rep(i, length(vals)), vals, labels = format_lab_number(vals, digits),
                     pos = 4, cex = .68, offset = .45, xpd = NA)
    }
  }

  labs <- pchs <- character(0)
  if (any_sus) { labs <- c(labs, "Tukey 1,5–3 IQR"); pchs <- c(pchs, 1) }
  if (any_ext) { labs <- c(labs, "Tukey >3 IQR"); pchs <- c(pchs, 4) }
  if (any_dix) { labs <- c(labs, "Dixon/Reed"); pchs <- c(pchs, 8) }
  if (any(marked)) { labs <- c(labs, "Reviewed/excluded value"); pchs <- c(pchs, 17) }
  if (length(labs)) graphics::legend("topleft", legend = labs, pch = as.numeric(pchs), bty = "n", cex = .78)
  invisible(NULL)
}

prepare_analysis_data <- function(dat, value_col, id_col = NULL, quantitative_col = NULL,
                                  qualitative_col = NULL, date_col = NULL,
                                  origin_col = NULL, verification_round_col = NULL,
                                  one_per_patient = TRUE, age_col = NULL, sex_col = NULL) {
  if (is.null(dat) || !nrow(dat)) stop("No data have been loaded.")
  if (!value_col %in% names(dat)) stop("Select the results column.")

  # Stable internal identifier to preserve traceability even after
  # sorting, deduplication, or exclusion of observations during specialist review.
  out <- data.frame(
    .ri_row_id = seq_len(nrow(dat)),
    value = safe_num(dat[[value_col]]),
    stringsAsFactors = FALSE
  )
  if (!is.null(id_col) && nzchar(id_col) && id_col %in% names(dat)) out$patient_id <- as.character(dat[[id_col]])

  # v0.16.0: general covariate layer. The interface refers only to
  # qualitative and quantitative variables. For compatibility with the
  # already validated scientific engines, the internal sex/age aliases are also created.
  # This generalizes the data layer without rewriting the criteria.
  if ((is.null(quantitative_col) || !nzchar(quantitative_col)) && !is.null(age_col)) quantitative_col <- age_col
  if ((is.null(qualitative_col) || !nzchar(qualitative_col)) && !is.null(sex_col)) qualitative_col <- sex_col
  if (!is.null(quantitative_col) && nzchar(quantitative_col) && quantitative_col %in% names(dat)) {
    out$quantitative <- safe_num(dat[[quantitative_col]])
    out$age <- out$quantitative
  }
  if (!is.null(qualitative_col) && nzchar(qualitative_col) && qualitative_col %in% names(dat)) {
    out$qualitative <- trimws(as.character(dat[[qualitative_col]]))
    out$sex <- out$qualitative
  }
  if (!is.null(date_col) && nzchar(date_col) && date_col %in% names(dat)) out$date <- suppressWarnings(as.POSIXct(dat[[date_col]], tz = "UTC"))
  if (!is.null(origin_col) && nzchar(origin_col) && origin_col %in% names(dat)) out$origin <- trimws(as.character(dat[[origin_col]]))
  if (!is.null(verification_round_col) && nzchar(verification_round_col) && verification_round_col %in% names(dat)) {
    vr <- trimws(as.character(dat[[verification_round_col]]))
    vr_norm <- ifelse(tolower(vr) %in% c("1","first","first","first","a"), 1L,
                      ifelse(tolower(vr) %in% c("2","second","second","second","b"), 2L, NA_integer_))
    out$verification_round <- vr_norm
  }

  n_initial <- nrow(out)
  out <- out[is.finite(out$value), , drop = FALSE]
  n_numeric <- nrow(out)

  # One result for patient: first chronologically if dates exist, otherwise first row.
  n_before_dedupe <- nrow(out)
  if (isTRUE(one_per_patient) && "patient_id" %in% names(out)) {
    valid_id <- !is.na(out$patient_id) & nzchar(out$patient_id)
    if (any(valid_id)) {
      if ("date" %in% names(out) && any(!is.na(out$date))) {
        ord <- order(out$patient_id, out$date, na.last = TRUE)
        out <- out[ord, , drop = FALSE]
      }
      keep <- !duplicated(out$patient_id) | !valid_id
      out <- out[keep, , drop = FALSE]
    }
  }

  removed_duplicates <- n_before_dedupe - nrow(out)
  unique_patients <- if ("patient_id" %in% names(out)) {
    ids <- out$patient_id[!is.na(out$patient_id) & nzchar(out$patient_id)]
    if (length(ids)) length(unique(ids)) else NA_integer_
  } else NA_integer_
  excluded_preparation <- (n_initial - n_numeric) + removed_duplicates

  audit_steps <- c("Imported rows", "Valid numeric results")
  audit_n <- c(n_initial, n_numeric)
  if (is.finite(unique_patients)) {
    audit_steps <- c(audit_steps, "Unique patients identified")
    audit_n <- c(audit_n, unique_patients)
  }
  audit_steps <- c(audit_steps, "Duplicates removed", "Results excluded during preparation", "final n analyzed")
  audit_n <- c(audit_n, removed_duplicates, excluded_preparation, nrow(out))

  qual_levels <- if ("qualitative" %in% names(out)) {
    z <- trimws(as.character(out$qualitative)); sort(unique(z[!is.na(z) & nzchar(z)]))
  } else character(0)
  quant_range <- if ("quantitative" %in% names(out) && any(is.finite(out$quantitative))) range(out$quantitative, na.rm = TRUE) else c(NA_real_, NA_real_)

  list(
    data = out,
    audit = data.frame(step = audit_steps, n = audit_n, stringsAsFactors = FALSE),
    removed_non_numeric = n_initial - n_numeric,
    removed_duplicates = removed_duplicates,
    covariates = list(
      qualitative = list(source = qualitative_col %||% "", levels = qual_levels, n_levels = length(qual_levels)),
      quantitative = list(source = quantitative_col %||% "", range = quant_range, n_unique = if ("quantitative" %in% names(out)) length(unique(out$quantitative[is.finite(out$quantitative)])) else 0L)
    )
  )
}

make_quality_assessment <- function(study_route, n, analytical_stable, qc_ok,
                                    population_defined = TRUE, preanalytic_ok = TRUE,
                                    has_patient_id = FALSE, has_quantitative = FALSE, has_qualitative = FALSE,
                                    outpatient_selectable = FALSE,
                                    study_type = "establish") {
  rows <- list()
  add <- function(item, status, detail) {
    rows[[length(rows) + 1L]] <<- data.frame(item = item, status = status, detail = detail, stringsAsFactors = FALSE)
  }

  add("Analytical stability", if (isTRUE(analytical_stable)) "green" else "red",
      if (isTRUE(analytical_stable)) "Declared stable" else "Not demonstrated")
  add("Quality control", if (isTRUE(qc_ok)) "green" else "red",
      if (isTRUE(qc_ok)) "QC acceptable" else "QC not confirmed")

  if (study_route == "direct") {
    add("Reference population", if (isTRUE(population_defined)) "green" else "red",
        if (isTRUE(population_defined)) "Defined" else "Must be defined before the study")
    add("Preanalytical conditions", if (isTRUE(preanalytic_ok)) "green" else "red",
        if (isTRUE(preanalytic_ok)) "Documented/controlled" else "Insufficiently controlled")
    if (study_type %in% c("verify", "review")) {
      add("Verification sample size", if (n >= 20) "green" else "red",
          paste0("n = ", n, if (n >= 20) "; initial cohort of 20 available" else "; 20 valid reference individuals are required"))
    } else {
      add("Sample size", if (n >= 120) "green" else if (n >= 3) "yellow" else "red",
          paste0("n = ", n, if (n >= 120) "; standard non-parametric method" else if (n >= 3) "; small-sample pathway: decide based on model + CI precision" else "; insufficient for estimation"))
    }
  } else {
    add("Data volume", if (n >= 5000) "green" else if (n >= 1000) "yellow" else if (n >= 200) "yellow" else "red",
        paste0("n = ", n, if (n >= 5000) "; favorable" else if (n >= 1000) "; usable with enhanced assessment" else if (n >= 200) "; exploratory/screening" else "; insufficient"))
    add("Patient identifier", if (has_patient_id) "green" else "yellow",
        if (has_patient_id) "Available" else "Unavailable: one result for patient cannot be guaranteed")
    add("Quantitative variable", if (has_quantitative) "green" else "yellow", if (has_quantitative) "Available" else "Unavailable")
    add("Qualitative variable", if (has_qualitative) "green" else "yellow", if (has_qualitative) "Available" else "Unavailable")
    add("Origin", if (outpatient_selectable) "green" else "yellow",
        if (outpatient_selectable) "Available for filtering" else "Unavailable or not selected")
  }
  do.call(rbind, rows)
}

simple_hist <- function(x, main = "Distribution of results", xlab = "Result", ri = NULL) {
  graphics::hist(x, breaks = "FD", main = main, xlab = xlab, border = "white")
  if (!is.null(ri) && length(ri) == 2 && all(is.finite(ri))) {
    graphics::abline(v = ri, lty = 2, lwd = 2)
  }
}

# Graphical distribution diagnostics -----------------------------------------
# Complements formal tests. It does not determine the method by itself.
diagnostic_hist_density <- function(x, main = "Histogram + density", xlab = "Result") {
  z <- x[is.finite(x)]
  if (length(z) < 3 || length(unique(z)) < 2) return(diagnostic_empty_plot("Insufficient data"))
  graphics::hist(z, breaks = "FD", probability = TRUE, main = main, xlab = xlab)
  den <- tryCatch(stats::density(z, na.rm = TRUE), error = function(e) NULL)
  if (!is.null(den) && all(is.finite(den$x)) && all(is.finite(den$y))) graphics::lines(den, lwd = 2)
  graphics::rug(z)
}

diagnostic_qq_plot <- function(x, main = "QQ-plot") {
  z <- x[is.finite(x)]
  if (length(z) < 3 || !is.finite(stats::sd(z)) || stats::sd(z) <= 0) return(diagnostic_empty_plot("Insufficient data"))
  stats::qqnorm(z, main = main, xlab = "Theoretical quantiles", ylab = "Observed quantiles")
  stats::qqline(z, lwd = 2)
}

diagnostic_empty_plot <- function(message = "Not evaluable") {
  graphics::plot.new()
  graphics::text(0.5, 0.5, message)
  invisible(NULL)
}

flatten_named <- function(x, prefix = NULL) {
  out <- list()
  rec <- function(obj, nm) {
    if (is.data.frame(obj)) {
      for (j in seq_along(obj)) rec(obj[[j]], paste0(nm, if (nzchar(nm)) "." else "", names(obj)[j]))
    } else if (is.list(obj) && !is.atomic(obj)) {
      nms <- names(obj)
      if (is.null(nms)) nms <- seq_along(obj)
      for (i in seq_along(obj)) rec(obj[[i]], paste0(nm, if (nzchar(nm)) "." else "", nms[i]))
    } else if (length(obj) <= 10) {
      out[[nm]] <<- paste(obj, collapse = "; ")
    }
  }
  rec(x, prefix %||% "")
  out
}

html_escape <- function(x) {
  htmltools::htmlEscape(as.character(x))
}

# Reference design: two-sided or one-sided ----------------------------------
# v0.15.0. The engine internally retains a pair of percentiles so existing
# estimators/diagnostics can be reused, but only the limits
# marked as active are part of the clinical result.
make_reference_design <- function(tail = c("two_sided", "lower", "upper"), coverage = 0.95) {
  tail <- match.arg(tail)
  coverage <- suppressWarnings(as.numeric(coverage))[1]
  if (!is.finite(coverage) || coverage <= 0.50 || coverage >= 1) {
    stop("Reference coverage must be between 50% and 100%.")
  }
  if (identical(tail, "two_sided")) {
    a <- (1 - coverage) / 2
    pair <- c(lower = a, upper = 1 - a)
    active <- c(lower = TRUE, upper = TRUE)
  } else {
    a <- 1 - coverage
    # Symmetric computational pair P(alpha)-P(1-alpha). In a one-sided
    # design, only one of the two limits is clinically active.
    pair <- c(lower = a, upper = 1 - a)
    active <- c(lower = identical(tail, "lower"), upper = identical(tail, "upper"))
  }
  list(
    tail = tail,
    coverage = coverage,
    pair_percentiles = pair,
    active = active,
    pair_coverage = unname(diff(pair)),
    # reflimR 1.1.0 uses perc.trunc for truncation robustness, but the
    # final limits remain P2.5/P97.5. This argument must not be used
    # to redefine P5/P95 in one-sided studies.
    reflim_perc_trunc = 2.5,
    active_percentiles = pair[active]
  )
}

normalize_reference_design <- function(reference_design = NULL, tail = "two_sided", coverage = 0.95) {
  if (is.null(reference_design)) return(make_reference_design(tail, coverage))
  if (is.character(reference_design) && length(reference_design) == 1L) {
    return(make_reference_design(reference_design, coverage))
  }
  if (!is.list(reference_design)) stop("Invalid reference design.")
  make_reference_design(reference_design$tail %||% tail, reference_design$coverage %||% coverage)
}

reference_design_label <- function(reference_design) {
  d <- normalize_reference_design(reference_design)
  cov <- formatC(100*d$coverage, format="fg", digits=4)
  switch(d$tail,
    two_sided = paste0("Two-sided · central coverage ", cov, " %"),
    lower = paste0("Lower one-sided · coverage ", cov, " %"),
    upper = paste0("Upper one-sided · coverage ", cov, " %")
  )
}

reference_percentile_label <- function(p) {
  p <- 100 * suppressWarnings(as.numeric(p))[1]
  if (!is.finite(p)) return("—")
  # formatC(format = "fg") may add alignment spaces (e.g., " 95").
  # Percentile labels are part of the interface and exported column
  # names, so they must be canonical: P95, P5, P2.5...
  z <- trimws(formatC(p, format="fg", digits=5, decimal.mark="."))
  paste0("P", z)
}

reference_design_percentile_text <- function(reference_design) {
  d <- normalize_reference_design(reference_design)
  pp <- d$active_percentiles
  if (length(pp) == 2L) paste(vapply(pp, reference_percentile_label, character(1)), collapse=" – ")
  else reference_percentile_label(pp)
}

reference_active_limits <- function(x, reference_design) {
  d <- normalize_reference_design(reference_design)
  x <- setNames(as.numeric(x)[1:2], c("lower", "upper"))
  x[d$active]
}

reference_result_text <- function(x, reference_design, digits = 2) {
  d <- normalize_reference_design(reference_design)
  x <- setNames(as.numeric(x)[1:2], c("lower", "upper"))
  if (identical(d$tail, "two_sided")) return(format_lab_interval(x, digits))
  nm <- if (identical(d$tail, "lower")) "lower" else "upper"
  paste0(reference_percentile_label(d$pair_percentiles[[nm]]), " = ", format_lab_number(x[[nm]], digits))
}

reference_limit_name <- function(reference_design, side = NULL, short = FALSE) {
  d <- normalize_reference_design(reference_design)
  if (is.null(side)) {
    if (identical(d$tail, "two_sided")) return(if (short) "RI" else "Reference interval")
    side <- if (identical(d$tail, "lower")) "lower" else "upper"
  }
  if (identical(side, "lower")) return(if (short) "LRL" else "Lower reference limit")
  if (short) "URL" else "Upper reference limit"
}
