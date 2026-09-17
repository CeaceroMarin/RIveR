# RIveR v1.0.1 · reporting stable -----------------
# These functions DO NOT re-estimate the RI or modify refineR/reflimR. They only
# transform the already calculated continuous curve into two clinical presentations:
# (1) a parameterized table of model values and (2) an approximation
# of non-overlapping discrete bands for LIS systems that do not support a continuous RI.

d7_continuous_model_is_candidate <- function(model) {
  !is.null(model) && identical(model$decision %||% "", "continuous_candidate") &&
    isTRUE(model$available %||% TRUE) && !is.null(model$curve)
}

d7_continuous_application_table <- function(model, age_like = FALSE, generic_points = 51L) {
  if (!d7_continuous_model_is_candidate(model)) return(NULL)
  curve <- model$curve %||% NULL
  if (is.null(curve) || !is.data.frame(curve) || !nrow(curve)) return(NULL)
  xcol <- setdiff(names(curve), c("LRL", "P50", "URL"))[1]
  if (is.na(xcol) || !nzchar(xcol) || !all(c("LRL", "URL") %in% names(curve))) return(NULL)
  x <- suppressWarnings(as.numeric(curve[[xcol]]))
  ok <- is.finite(x) & is.finite(curve$LRL) & is.finite(curve$URL)
  if (sum(ok) < 3L) return(NULL)
  curve <- curve[ok, , drop = FALSE]
  x <- suppressWarnings(as.numeric(curve[[xcol]]))
  ord <- order(x); curve <- curve[ord, , drop = FALSE]; x <- x[ord]
  xmin <- min(x); xmax <- max(x)
  if (!is.finite(xmin) || !is.finite(xmax) || xmax <= xmin) return(NULL)

  if (isTRUE(age_like)) {
    annual <- seq(ceiling(xmin), floor(xmax), by = 1)
    xp <- sort(unique(c(xmin, annual, xmax)))
  } else {
    generic_points <- max(9L, min(101L, as.integer(generic_points)))
    xp <- seq(xmin, xmax, length.out = generic_points)
  }
  out <- data.frame(.x = xp, stringsAsFactors = FALSE)
  names(out)[1] <- xcol
  if ("LRL" %in% names(curve)) out$LRL <- stats::approx(x, curve$LRL, xout = xp, rule = 2)$y
  if ("P50" %in% names(curve)) out$P50 <- stats::approx(x, curve$P50, xout = xp, rule = 2)$y
  if ("URL" %in% names(curve)) out$URL <- stats::approx(x, curve$URL, xout = xp, rule = 2)$y
  out
}

d7_continuous_operational_discretization <- function(model, age_like = FALSE, tolerance = 0.10, max_groups = 10L) {
  if (!d7_continuous_model_is_candidate(model)) {
    return(list(ok = FALSE, status = "yellow", acceptable = FALSE,
                reason = "The quantitative model has not reached candidate status; no operational proposal for the LIS is generated."))
  }
  curve <- model$curve %||% NULL
  design <- normalize_reference_design(model$reference_design %||% NULL)
  if (!identical(design$tail, "two_sided")) {
    return(list(ok = FALSE, status = "grey", acceptable = FALSE,
                reason = "RIveR D7 operational discretization is defined only for two-sided RIs."))
  }
  if (is.null(curve) || !is.data.frame(curve) || !nrow(curve)) {
    return(list(ok = FALSE, status = "grey", acceptable = FALSE,
                reason = "There is no complete continuous curve from which to derive operational bands."))
  }
  xcol <- setdiff(names(curve), c("LRL", "P50", "URL"))[1]
  if (is.na(xcol) || !nzchar(xcol) || !all(c("LRL", "URL") %in% names(curve))) {
    return(list(ok = FALSE, status = "grey", acceptable = FALSE,
                reason = "The continuous curve does not contain the required limits."))
  }
  x <- suppressWarnings(as.numeric(curve[[xcol]]))
  lo <- suppressWarnings(as.numeric(curve$LRL)); up <- suppressWarnings(as.numeric(curve$URL))
  ok <- is.finite(x) & is.finite(lo) & is.finite(up) & lo < up
  if (sum(ok) < 10L) {
    return(list(ok = FALSE, status = "grey", acceptable = FALSE,
                reason = "There are insufficient valid curve points for discretization."))
  }
  x <- x[ok]; lo <- lo[ok]; up <- up[ok]
  ord <- order(x); x <- x[ord]; lo <- lo[ord]; up <- up[ord]
  m <- length(x)
  typical_width <- stats::median(up - lo, na.rm = TRUE)
  if (!is.finite(typical_width) || typical_width <= 0) {
    return(list(ok = FALSE, status = "grey", acceptable = FALSE,
                reason = "A typical width of the continuous RI could not be defined."))
  }
  tolerance <- suppressWarnings(as.numeric(tolerance))[1]
  if (!is.finite(tolerance) || tolerance <= 0) tolerance <- 0.10
  max_groups <- max(2L, min(as.integer(max_groups), m))

  cost <- matrix(Inf, nrow = m, ncol = m)
  cache <- vector("list", m * m)
  key <- function(i, j) (i - 1L) * m + j
  for (i in seq_len(m)) {
    for (j in i:m) {
      cl <- (min(lo[i:j]) + max(lo[i:j])) / 2
      cu <- (min(up[i:j]) + max(up[i:j])) / 2
      err <- max(abs(lo[i:j] - cl), abs(up[i:j] - cu), na.rm = TRUE) / typical_width
      if (is.finite(err)) {
        cost[i, j] <- err
        cache[[key(i, j)]] <- list(i = i, j = j, lrl = cl, url = cu, rel_error = err)
      }
    }
  }

  dp <- matrix(Inf, nrow = max_groups, ncol = m)
  prev <- matrix(NA_integer_, nrow = max_groups, ncol = m)
  for (j in seq_len(m)) dp[1, j] <- cost[1, j]
  if (max_groups >= 2L) {
    for (k in 2:max_groups) {
      for (j in seq_len(m)) {
        if (j < k) next
        best <- Inf; best_i <- NA_integer_
        for (i in k:j) {
          left <- dp[k - 1L, i - 1L]; right <- cost[i, j]
          if (!is.finite(left) || !is.finite(right)) next
          val <- max(left, right)
          if (val < best) { best <- val; best_i <- i }
        }
        dp[k, j] <- best; prev[k, j] <- best_i
      }
    }
  }

  reconstruct <- function(k) {
    if (!is.finite(dp[k, m])) return(NULL)
    idx <- vector("list", k); j <- m
    for (kk in k:1) {
      i <- if (kk == 1L) 1L else prev[kk, j]
      if (!is.finite(i)) return(NULL)
      idx[[kk]] <- c(i, j); j <- i - 1L
    }
    segs <- lapply(idx, function(z) cache[[key(z[1], z[2])]])
    if (any(vapply(segs, is.null, logical(1)))) return(NULL)
    segs
  }

  solutions <- lapply(2:max_groups, function(k) {
    segs <- reconstruct(k)
    if (is.null(segs)) return(NULL)
    err <- max(vapply(segs, `[[`, numeric(1), "rel_error"), na.rm = TRUE)
    list(k = k, segments = segs, overall_error = err, acceptable = is.finite(err) && err <= tolerance)
  })
  names(solutions) <- as.character(2:max_groups)
  valid <- Filter(Negate(is.null), solutions)
  if (!length(valid)) {
    return(list(ok = FALSE, status = "yellow", acceptable = FALSE,
                reason = "Could not reconstruct any operational discretization."))
  }
  acceptable_k <- as.integer(names(valid)[vapply(valid, function(z) isTRUE(z$acceptable), logical(1))])
  chosen_k <- if (length(acceptable_k)) min(acceptable_k) else {
    errs <- vapply(valid, `[[`, numeric(1), "overall_error")
    as.integer(names(valid)[which.min(errs)])
  }
  chosen <- valid[[as.character(chosen_k)]]

  fmt <- function(v, digits = if (isTRUE(age_like)) 1L else 4L) {
    formatC(v, format = "fg", digits = digits, decimal.mark = ",")
  }
  seg_rows <- lapply(seq_along(chosen$segments), function(ii) {
    sg <- chosen$segments[[ii]]; i <- sg$i; j <- sg$j
    lower_boundary <- if (i == 1L) x[1] else mean(x[c(i - 1L, i)])
    upper_boundary <- if (j == m) x[m] else mean(x[c(j, j + 1L)])
    label <- if (ii < length(chosen$segments)) {
      paste0(fmt(lower_boundary), " ≤ ", xcol, " < ", fmt(upper_boundary), if (isTRUE(age_like)) " years" else "")
    } else {
      paste0(fmt(lower_boundary), " ≤ ", xcol, " ≤ ", fmt(upper_boundary), if (isTRUE(age_like)) " years" else "")
    }
    data.frame(
      `Interval of application` = label,
      LRL = sg$lrl,
      URL = sg$url,
      `Error maximum vs model` = sg$rel_error,
      stringsAsFactors = FALSE, check.names = FALSE
    )
  })
  tab <- do.call(rbind, seg_rows)
  tradeoff <- do.call(rbind, lapply(valid, function(z) data.frame(
    Bands = z$k,
    `Error maximum vs model` = z$overall_error,
    `Meets geometric criterion` = isTRUE(z$acceptable),
    stringsAsFactors = FALSE, check.names = FALSE
  )))
  tradeoff <- tradeoff[order(tradeoff$Bands), , drop = FALSE]
  acceptable <- isTRUE(chosen$acceptable)
  rec <- if (acceptable) {
    paste0(
      "If the LIS does not support a continuous RI, the simplest discrete representation that meets the internal geometric criterion uses ",
      chosen_k, " non-overlapping bands (maximum error ", round(100 * chosen$overall_error, 1),
      " % of the typical RI width; limit ", round(100 * tolerance, 0), " %). ",
      "The limits of each band approximate the continuous curve and are NOT independently re-established RIs."
    )
  } else {
    paste0(
      "With a maximum of ", max_groups, " bands, the internal geometric criterion ≤",
      round(100 * tolerance, 0), "%. The displayed discretization is only the best available approximation and should not be implemented without an explicit specialist decision."
    )
  }
  rec <- paste0(rec,
    " In indirect establishment, raw coverage of routine data is not used to validate these bands because the dataset may contain pathological results; external validation is required before clinical use.")

  list(ok = TRUE, status = if (acceptable) "green" else "yellow", acceptable = acceptable,
       tolerance = tolerance, n_groups = chosen_k, overall_error = chosen$overall_error,
       table = tab, tradeoff_table = tradeoff, recommendation = rec,
       note = "Operational discretization derived exclusively from the D7 continuous curve. It reuses the RIveR minimax geometric criterion, but not the D4 coverage criterion, which cannot be transferred directly to routine indirect data with possible pathological contamination.")
}

d7_continuous_operational_display_table <- function(adaptation) {
  tab <- adaptation$table %||% NULL
  if (is.null(tab) || !is.data.frame(tab) || !nrow(tab)) return(NULL)
  out <- tab
  for (nm in intersect(c("LRL", "URL"), names(out))) out[[nm]] <- signif(out[[nm]], 6)
  if ("Error maximum vs model" %in% names(out)) out[["Error maximum vs model"]] <- ifelse(
    is.finite(out[["Error maximum vs model"]]),
    paste0(formatC(100*out[["Error maximum vs model"]], format="f", digits=1, decimal.mark=","), " %"), "—")
  out
}

d7_continuous_tradeoff_display_table <- function(adaptation) {
  tab <- adaptation$tradeoff_table %||% NULL
  if (is.null(tab) || !is.data.frame(tab) || !nrow(tab)) return(NULL)
  out <- tab
  if ("Error maximum vs model" %in% names(out)) out[["Error maximum vs model"]] <- ifelse(
    is.finite(out[["Error maximum vs model"]]),
    paste0(formatC(100*out[["Error maximum vs model"]], format="f", digits=1, decimal.mark=","), " %"), "—")
  if ("Meets geometric criterion" %in% names(out)) out[["Meets geometric criterion"]] <- ifelse(out[["Meets geometric criterion"]], "Yes", "No")
  out
}

write_html_report <- function(file, state, main_result, partition_result = NULL, age_result = NULL, final = NULL) {
  esc <- htmltools::htmlEscape
  val <- function(x) if (is.null(x) || length(x) == 0 || all(is.na(x))) "—" else paste(x, collapse = ", ")
  reason_label <- function(x) switch(as.character(x %||% ""),
    preanalytical="Confirmed preanalytical error", analytical="Analytical error / invalid result",
    selection="Failure to meet the selection criteria", legitimate="Legitimate population observation",
    biological="Biological / physiological plausibility", clinical="Classification / clinical impact",
    distribution="Distribution shape / tails", literature="Literature / external consensus",
    operational="Operational limitation / LIS", model="Assumptions of the model / distribution",
    precision="Precision of the limits", other="Other", as.character(x %||% "—"))
  df_html <- function(df) {
    if (is.null(df) || !nrow(df)) return("")
    hdr <- paste0("<tr>", paste0("<th>", esc(names(df)), "</th>", collapse = ""), "</tr>")
    rows <- apply(df, 1, function(r) paste0("<tr>", paste0("<td>", esc(ifelse(is.na(r), "—", as.character(r))), "</td>", collapse = ""), "</tr>"))
    paste0("<table class='wide'>", hdr, paste(rows, collapse = ""), "</table>")
  }
  qualitative_covariate_label <- partition_result$covariate_label %||% main_result$qualitative_label %||% "Qualitative variable"
  quantitative_covariate_label <- age_result$covariate_label %||% main_result$quantitative_label %||% "Quantitative variable"
  quantitative_age_like_report <- if (!is.null(age_result$covariate_is_age)) isTRUE(age_result$covariate_is_age) else
    (exists("quantitative_is_age_like", mode="function") && quantitative_is_age_like(quantitative_covariate_label))
  quantitative_axis_report <- if (quantitative_age_like_report) paste0(quantitative_covariate_label, " (years)") else quantitative_covariate_label
  genericize_quantitative_report_table <- function(tab) {
    if (is.null(tab) || !is.data.frame(tab) || quantitative_age_like_report) return(tab)
    if (exists("quantitative_relabel_text", mode="function")) {
      names(tab) <- vapply(names(tab), function(nm) quantitative_relabel_text(nm, quantitative_covariate_label), character(1))
      for (nm in names(tab)) if (is.character(tab[[nm]])) tab[[nm]] <- quantitative_relabel_text(tab[[nm]], quantitative_covariate_label)
    }
    tab
  }

  svg_plot_fragment <- function(plot_fun, width = 5.2, height = 3.1) {
    tf <- tempfile(fileext = ".svg")
    opened <- FALSE
    ok <- tryCatch({
      grDevices::svg(tf, width = width, height = height, onefile = TRUE, bg = "white")
      opened <- TRUE
      plot_fun()
      grDevices::dev.off(); opened <- FALSE
      TRUE
    }, error = function(e) FALSE, finally = {
      if (opened) try(grDevices::dev.off(), silent = TRUE)
    })
    if (!isTRUE(ok) || !file.exists(tf)) return("")
    txt <- tryCatch(readLines(tf, warn = FALSE, encoding = "UTF-8"), error = function(e) character(0))
    unlink(tf)
    if (!length(txt)) return("")
    st <- grep("<svg", txt, fixed = TRUE)[1]
    if (!is.finite(st)) return("")
    svg_txt <- paste(txt[st:length(txt)], collapse = "\n")
    paste0("<img class='diagimg' alt='Diagnostic plot' src='data:image/svg+xml;charset=utf-8,", utils::URLencode(svg_txt, reserved = TRUE), "'/>")
  }
  distribution_graph_html <- function() {
    if (!identical(main_result$type, "direct_establishment")) return("")
    pd <- state$prepared_data %||% NULL
    x <- if (!is.null(pd) && is.data.frame(pd) && "value" %in% names(pd)) pd$value else numeric(0)
    y <- main_result$distribution$boxcox$transformed %||% numeric(0)
    if (!length(x) || !any(is.finite(x))) return("")
    g1 <- svg_plot_fragment(function() diagnostic_hist_density(x, "Histogram + density", meta$analyte %||% "Result"))
    g2 <- svg_plot_fragment(function() diagnostic_qq_plot(x, "Q-Q plot · original data"))
    if (length(y) && any(is.finite(y))) {
      g3 <- svg_plot_fragment(function() diagnostic_hist_density(y, "Histogram + density", "Box-Cox scale"))
      g4 <- svg_plot_fragment(function() diagnostic_qq_plot(y, "QQ-plot · after Box-Cox"))
    } else { g3 <- g4 <- "" }
    if (!nzchar(g1) && !nzchar(g2) && !nzchar(g3) && !nzchar(g4)) return("")
    paste0("<div class='card'><h2>Inspection graphical of the distribution</h2>",
           "<p class='small'>Graphical inspection complements normality and symmetry assessments; it does not replace model evaluation.</p>",
           "<div class='plotgrid'>",
           "<div class='plotgroup'><h3>Original data</h3><div class='plotbox'>", g1, "</div><div class='plotbox'>", g2, "</div></div>",
           "<div class='plotgroup'><h3>After of Box-Cox</h3><div class='plotbox'>", g3, "</div><div class='plotbox'>", g4, "</div></div>",
           "</div></div>")
  }
  age_graph_html <- function() {
    if (is.null(age_result) || is.null(age_result$bins)) return("")
    b <- age_result$bins
    c <- age_result$continuous$curves %||% NULL
    design_age <- normalize_reference_design(age_result$reference_design %||% main_result$reference_design %||% (state$metadata %||% list())$reference_design)
    qlab <- quantitative_covariate_label
    xlab <- quantitative_axis_report
    g1 <- svg_plot_fragment(function() {
      if (!is.null(c) && nrow(c)) {
        if (identical(design_age$tail, "two_sided")) {
          yr <- range(c$p025,c$p975,b$p025,b$p975,na.rm=TRUE)
          plot(c$age,c$p50,type="l",lwd=2,xlab=xlab,ylab="Result",ylim=yr,
               main=paste0(reference_percentile_label(design_age$pair_percentiles[["lower"]])," / P50 / ",reference_percentile_label(design_age$pair_percentiles[["upper"]])," according to ",qlab))
          lines(c$age,c$p025,lty=2,lwd=2); lines(c$age,c$p975,lty=2,lwd=2)
          points(b$age_mid,b$p50,pch=19,cex=.6); points(b$age_mid,b$p025,pch=1,cex=.55); points(b$age_mid,b$p975,pch=1,cex=.55)
        } else {
          y <- if (isTRUE(design_age$active[["lower"]])) c$p025 else c$p975
          yp <- if (isTRUE(design_age$active[["lower"]])) b$p025 else b$p975
          plot(c$age,y,type="l",lwd=2,xlab=xlab,ylab="Result",main=paste0(reference_limit_name(design_age)," according to ",qlab))
          points(b$age_mid,yp,pch=19,cex=.6)
        }
        if (is.finite(age_result$selected_cut %||% NA_real_)) abline(v=age_result$selected_cut,lty=3,lwd=2)
      } else {
        if (identical(design_age$tail, "two_sided")) {
          yr <- range(b$p025,b$p975,na.rm=TRUE)
          plot(b$age_mid,b$p50,type="b",pch=19,xlab=xlab,ylab="Result",ylim=yr,main=paste0("Percentiles by ",qlab," bands"))
          lines(b$age_mid,b$p025,type="b",pch=1,lty=2); lines(b$age_mid,b$p975,type="b",pch=1,lty=2)
        } else {
          y <- if (isTRUE(design_age$active[["lower"]])) b$p025 else b$p975
          plot(b$age_mid,y,type="b",pch=19,xlab=xlab,ylab="Result",main=paste0(reference_limit_name(design_age)," for bands of ",qlab))
        }
      }
    }, width=7.2, height=4.1)
    m <- age_result$model %||% NULL
    g2 <- g3 <- ""
    if (!is.null(m) && isTRUE(m$ok)) {
      g2 <- svg_plot_fragment(function() {
        plot(m$data$age,m$residuals,pch=16,cex=.5,xlab=xlab,ylab="Normalized residual",main=paste0("Residuals vs ",qlab))
        abline(h=0,lty=2); ok <- is.finite(m$data$age)&is.finite(m$residuals)
        if (sum(ok)>=20) lines(stats::lowess(m$data$age[ok],m$residuals[ok],f=.45),lwd=2)
      })
      g3 <- svg_plot_fragment(function() {
        plot(m$fitted,m$residuals,pch=16,cex=.5,xlab="Fitted value",ylab="Normalized residual",main="Residuals vs fitted values")
        abline(h=0,lty=2); ok <- is.finite(m$fitted)&is.finite(m$residuals)
        if (sum(ok)>=20) lines(stats::lowess(m$fitted[ok],m$residuals[ok],f=.45),lwd=2)
      })
    }
    paste0("<div class='card'><h2>Graphical diagnostic · quantitative variable: ",esc(qlab),"</h2><div class='plotbox'>",g1,"</div>",
           if (nzchar(g2)||nzchar(g3)) paste0("<div class='plotgrid'><div class='plotbox'>",g2,"</div><div class='plotbox'>",g3,"</div></div>") else "",
           "</div>")
  }


  outlier_boxplot_html <- function(scope = c("overall", "subgroups")) {
    scope <- match.arg(scope)
    if (!identical(main_result$type, "direct_establishment")) return("")
    current <- state$prepared_data %||% NULL
    baseline <- state$outlier_baseline_data %||% NULL
    decisions <- state$outlier_decisions %||% data.frame()
    excluded_ids <- if (is.data.frame(decisions) && nrow(decisions) && all(c("row_id","decision") %in% names(decisions))) unique(decisions$row_id[decisions$decision == "Exclude"]) else integer(0)
    has_before <- length(excluded_ids) && is.data.frame(baseline) && nrow(baseline)
    ylab <- paste(meta$analyte %||% "Result", meta$unit %||% "")
    make_pair <- function(before_fun = NULL, current_fun, before_title = "Before review", current_title = "After recalculation") {
      gc <- svg_plot_fragment(current_fun, width=6.2, height=3.8)
      if (has_before && !is.null(before_fun)) {
        gb <- svg_plot_fragment(before_fun, width=6.2, height=3.8)
        return(paste0("<div class='plotgrid'><div class='plotgroup'><h3>",before_title,"</h3><div class='plotbox'>",gb,"</div></div><div class='plotgroup'><h3>",current_title,"</h3><div class='plotbox'>",gc,"</div></div></div>"))
      }
      paste0("<div class='plotbox'>",gc,"</div>")
    }
    if (scope == "overall") {
      if (!is.data.frame(current) || !nrow(current)) return("")
      pair <- make_pair(
        before_fun = if (has_before) function() plot_outlier_boxplot(baseline, main="Complete population · before review", ylab=ylab, marked_row_ids=excluded_ids) else NULL,
        current_fun = function() plot_outlier_boxplot(current, main=if (has_before) "Complete population · after recalculation" else "Complete population", ylab=ylab)
      )
      return(paste0("<div class='card'><h2>Box plot · complete population</h2>",
                    "<p class='small'>Visual aid: symbols highlight values identified by Tukey and/or Dixon/Reed. The plot alone is not an exclusion criterion.</p>", pair, "</div>"))
    }

    if (!is.data.frame(current) || !nrow(current)) return("")
    sections <- character(0)
    qg_col <- if ("qualitative" %in% names(current)) "qualitative" else if ("sex" %in% names(current)) "sex" else NA_character_
    if (!is.null(partition_result) && is.character(qg_col) && !is.na(qg_col)) {
      before_fun <- NULL
      if (has_before && qg_col %in% names(baseline)) {
        before_fun <- local({bd <- baseline; gc <- qg_col; ids <- excluded_ids; yl <- ylab; lab <- qualitative_covariate_label; function() plot_outlier_boxplot(bd, group=as.character(bd[[gc]]), main=paste0(lab," · before the review"), ylab=yl, marked_row_ids=ids)})
      }
      current_fun <- local({cd <- current; gc <- qg_col; yl <- ylab; hb <- has_before; lab <- qualitative_covariate_label; function() plot_outlier_boxplot(cd, group=as.character(cd[[gc]]), main=if (hb) paste0(lab," · after recalculation") else lab, ylab=yl)})
      sec <- make_pair(before_fun=before_fun, current_fun=current_fun)
      sections <- c(sections, paste0("<h3>Qualitative variable · ",esc(qualitative_covariate_label),"</h3>", sec))
    }
    cut <- age_result$selected_cut %||% NA_real_
    cv <- age_result$cut_validation %||% NULL
    qx_col <- if ("quantitative" %in% names(current)) "quantitative" else if ("age" %in% names(current)) "age" else NA_character_
    if (!is.null(cv) && is.finite(cut) && is.character(qx_col) && !is.na(qx_col)) {
      suffix <- if (quantitative_age_like_report) " years" else ""
      op <- age_result$operational_cut_info %||% NULL
      lower_lab <- if (!is.null(op) && isTRUE(op$preserves_groups)) op$lower_label else paste0("≤ ", formatC(cut, format="f", digits=2, decimal.mark=","), suffix)
      upper_lab <- if (!is.null(op) && isTRUE(op$preserves_groups)) op$upper_label else paste0("> ", formatC(cut, format="f", digits=2, decimal.mark=","), suffix)
      title_lab <- if (!is.null(op) && isTRUE(op$preserves_groups)) paste0("operational cut-point ", op$display) else paste0("statistical cut-point ", formatC(cut, format="f", digits=2, decimal.mark=","), suffix)
      curgrp <- ifelse(current[[qx_col]] <= cut, lower_lab, upper_lab)
      before_fun <- NULL
      if (has_before && qx_col %in% names(baseline)) {
        basegrp <- ifelse(baseline[[qx_col]] <= cut, lower_lab, upper_lab)
        before_fun <- local({bd <- baseline; bg <- basegrp; ids <- excluded_ids; yl <- ylab; lab <- quantitative_covariate_label; function() plot_outlier_boxplot(bd, group=bg, main=paste0(lab," · before the review"), ylab=yl, marked_row_ids=ids)})
      }
      current_fun <- local({cd <- current; cg <- curgrp; yl <- ylab; hb <- has_before; lab <- quantitative_covariate_label; function() plot_outlier_boxplot(cd, group=cg, main=if (hb) paste0(lab," · after recalculation") else lab, ylab=yl)})
      sec <- make_pair(before_fun=before_fun, current_fun=current_fun)
      sections <- c(sections, paste0("<h3>Quantitative variable · ",esc(quantitative_covariate_label)," · ",esc(title_lab),"</h3>",sec))
    }
    if (!length(sections)) return("")
    paste0("<div class='card'><h2>Box plot · subgroups</h2>",
           "<p class='small'>Subgroup visualization complements formal detection and may reveal observations that are not conspicuous in the overall population. Continuous modeling of the quantitative variable does not create artificial bands merely to display a box plot.</p>",
           paste(sections, collapse=""), "</div>")
  }

  app_version <- state$app_version %||% "1.0.1"
  pres_early <- state$partition_resolution %||% NULL
  sres_early <- state$small_sample_resolution %||% NULL
  decision_label <- function(x) switch(x %||% "",
    common = "common RI", partition = "Partition recommended",
    indeterminate = "Inconclusive", continuous = "Preferred continuous RI",
    not_evaluable = "Not evaluable", "—")

  result_rows <- character(0)
  digits <- main_result$display_digits %||% 2
  design <- normalize_reference_design(main_result$reference_design %||% (state$metadata %||% list())$reference_design)
  fmt_reference <- function(x, d = design) reference_result_text(x, d, digits)
  if (identical(main_result$module %||% "", "D7") && identical(main_result$partition$decision %||% "", "partition")) {
    if (!is.null(main_result$ri)) result_rows <- c(result_rows, paste0("<tr><th>", esc(if (identical(design$tail, "two_sided")) "overall refineR RI assessed (DO NOT ADOPT)" else paste0(reference_limit_name(design), " overall refineR assessed (DO NOT ADOPT)")), "</th><td>", esc(fmt_reference(main_result$ri)), "</td></tr>"))
    subs <- main_result$partition$substudies %||% list(); gr <- main_result$partition$groups %||% names(subs)
    for (ii in seq_along(subs)) {
      z <- subs[[ii]]
      result_rows <- c(result_rows,
        paste0("<tr><th>", esc(if (identical(design$tail, "two_sided")) "candidate RI" else reference_limit_name(design)), " · ", esc(gr[ii] %||% paste0("Group ", ii)), "</th><td><b>", esc(if (!is.null(z$ri)) reference_result_text(z$ri, z$reference_design %||% design, digits) else "—"), "</b></td></tr>"),
        paste0("<tr><th>D7 decision · ", esc(gr[ii] %||% paste0("Group ", ii)), "</th><td>", esc(z$decision %||% "—"), "</td></tr>"))
    }
    result_rows <- c(result_rows, paste0("<tr><th>Partition status</th><td><b>", if (identical(design$tail, "two_sided")) "CANDIDATE PARTITIONED RIs" else "CANDIDATE PARTITIONED LIMITS", " · pending biological-plausibility review and specialist approval</b></td></tr>"))
  } else if (identical(main_result$module %||% "", "D7") && identical(main_result$age$model$decision %||% "", "continuous_candidate")) {
    if (!is.null(main_result$ri)) result_rows <- c(result_rows, paste0("<tr><th>", esc(if (identical(design$tail, "two_sided")) "overall refineR RI assessed (DO NOT ADOPT)" else paste0(reference_limit_name(design), " overall refineR assessed (DO NOT ADOPT)")), "</th><td>", esc(fmt_reference(main_result$ri)), "</td></tr>"))
    ar <- main_result$age$model$supported_quantitative %||% main_result$age$model$supported_age %||% c(NA_real_, NA_real_)
    suffix <- if (quantitative_age_like_report) " years" else ""
    result_rows <- c(result_rows,
      paste0("<tr><th>Primary result</th><td><b>", esc(main_result$decision %||% if (identical(design$tail, "two_sided")) "CONTINUOUS RI — CANDIDATE" else paste0(toupper(reference_limit_name(design)), " CONTINUOUS — CANDIDATE")), "</b></td></tr>"),
      paste0("<tr><th>Modeled interval · ", esc(quantitative_covariate_label), "</th><td>", esc(if (all(is.finite(ar))) paste0(signif(ar[1],4), "–", signif(ar[2],4), suffix) else "—"), "</td></tr>"),
      paste0("<tr><th>Interpretation</th><td>", esc(if (identical(design$tail, "two_sided")) paste0("The continuous curve is the primary scientific result. See the application table for LRL/P50/URL according to ", quantitative_covariate_label, "; if the LIS does not support a continuous model, use the operational proposal of non-overlapping bands derived from the curve.") else paste0("The continuous curve is the primary scientific result. See the continuous ", reference_limit_name(design), " according to ", quantitative_covariate_label, "; no arbitrary bands were created.")), "</td></tr>"))
  } else if (identical(main_result$module %||% "", "D7") && is.null(main_result$ri)) {
    result_rows <- c(result_rows, paste0("<tr><th>D7 decision</th><td><b>", esc(main_result$decision %||% "NOT EVALUABLE"), "</b></td></tr>"),
                                  paste0("<tr><th>Reason</th><td>", esc(main_result$sample_context$text %||% main_result$recommendation %||% "—"), "</td></tr>"))
  } else if (!is.null(main_result$ri)) {
    if (!is.null(pres_early) && identical(pres_early$decision, "partition") && length(partition_result$group_results %||% list())) {
      result_rows <- c(result_rows, paste0("<tr><th>Common assessed RI (not adopted)</th><td>", esc(fmt_reference(main_result$ri)), "</td></tr>"))
      grs <- partition_result$group_results %||% list()
      for (gn in names(grs)) {
        gz <- grs[[gn]]
        if (!is.null(gz$ri)) result_rows <- c(result_rows, paste0("<tr><th>RI final · ", esc(gn), "</th><td><b>", esc(reference_result_text(gz$ri, gz$reference_design %||% design, gz$display_digits %||% digits)), "</b></td></tr>"))
      }
    } else if (!is.null(pres_early) && identical(pres_early$decision, "partition") && !is.null(partition_result$group1) && !is.null(partition_result$group2)) {
      result_rows <- c(result_rows,
        paste0("<tr><th>Common assessed RI (not adopted)</th><td>", esc(fmt_reference(main_result$ri)), "</td></tr>"),
        paste0("<tr><th>RI final · ", esc(pretty_group_label(partition_result$groups[1])), "</th><td><b>", esc(reference_result_text(partition_result$group1$ri, partition_result$group1$reference_design %||% design, digits)), "</b></td></tr>"),
        paste0("<tr><th>RI final · ", esc(pretty_group_label(partition_result$groups[2])), "</th><td><b>", esc(reference_result_text(partition_result$group2$ri, partition_result$group2$reference_design %||% design, digits)), "</b></td></tr>"))
    } else {
      no_model <- grepl("exploratory", tolower(main_result$method %||% ""))
      age_dep <- (age_result$decision %||% "") %in% c("continuous","partition")
      partition_dep <- identical(partition_result$decision %||% "", "partition")
      base_term <- if (identical(design$tail, "two_sided")) "RI" else reference_limit_name(design)
      lab <- if (identical(main_result$module %||% "", "D7")) paste0(base_term, " refineR candidate (do not implement automatically)") else if (no_model) paste0(base_term, " exploratory non-parametric (not adopted)") else if (age_dep || partition_dep) paste0(base_term, " overall assessed (not adopted)") else if ((!is.null(pres_early) && identical(pres_early$decision, "common")) || (!is.null(sres_early) && identical(sres_early$decision, "adopt"))) paste0(base_term, " final adopted") else paste0(base_term, " primary")
      result_rows <- c(result_rows, paste0("<tr><th>", lab, "</th><td><b>", esc(fmt_reference(main_result$ri)), "</b></td></tr>"))
      if (identical(age_result$decision %||% "", "continuous")) {
        result_rows <- c(result_rows, paste0("<tr><th>Strategy dependent on the quantitative variable</th><td><b>", esc(if (identical(design$tail, "two_sided")) paste0("Continuous RI · see the limits and P50 according to ", quantitative_covariate_label) else paste0(reference_limit_name(design), " continuous · ", reference_design_percentile_text(design), " according to ", quantitative_covariate_label)), "</b></td></tr>"))
        if (!is.null(age_result$sil_adaptation) && isTRUE(age_result$sil_adaptation$ok)) {
          result_rows <- c(result_rows, "<tr><th>Adaptation in the LIS</th><td>Exportable annual table and proposal of discrete bands derived from the model</td></tr>")
        }
      } else if (identical(age_result$decision %||% "", "partition")) {
        seg <- age_result$cut_validation$segment_table %||% NULL
        if (!is.null(seg) && nrow(seg)) {
          for (ii in seq_len(nrow(seg))) {
            glab <- age_group_operational_label(age_result, seg$Group[ii])
            seg_ri <- c(seg$LRL[ii], seg$URL[ii])
            result_rows <- c(result_rows, paste0("<tr><th>", esc(if (identical(design$tail, "two_sided")) "RI for quantitative variable" else reference_limit_name(design)), " · ", esc(glab), "</th><td><b>", esc(reference_result_text(seg_ri, design, digits)), "</b></td></tr>"))
          }
        }
      }
    }
  }
  if (!is.null(main_result$target)) {
    result_rows <- c(result_rows,
                     paste0("<tr><th>candidate RI/current</th><td>", esc(reference_result_text(main_result$target, design, digits)), "</td></tr>"))
  }
  if (identical(main_result$type %||% "", "direct_verification")) {
    if (isTRUE(main_result$partitioned)) {
      result_rows <- c(result_rows,
        paste0("<tr><th>Direct verification</th><td>Partitioned RIs by ", esc(if (identical(main_result$partition_type, "age")) "quantitative variable" else "qualitative variable"), "</td></tr>"),
        paste0("<tr><th>Overall verification decision</th><td><b>", esc(main_result$decision %||% "—"), "</b></td></tr>")
      )
      for (z in main_result$partition_results %||% list()) {
        src <- z$cohort_sources %||% c("—","")
        result_rows <- c(result_rows,
          paste0("<tr><th>Partition</th><td><b>", esc(z$partition_label %||% z$partition_key %||% "—"), "</b></td></tr>"),
          paste0("<tr><th>candidate RI · ", esc(z$partition_label %||% "partition"), "</th><td>", esc(reference_result_text(z$target, z$reference_design %||% design, digits)), "</td></tr>"),
          paste0("<tr><th>First cohort · ", esc(z$partition_label %||% "partition"), "</th><td>", esc(if (is.finite(z$outside_first20 %||% NA_real_)) paste0(z$outside_first20, "/20 outside") else paste0(z$first_n %||% 0L, " individuals")), "</td></tr>"),
          paste0("<tr><th>Second cohort · ", esc(z$partition_label %||% "partition"), "</th><td>", esc(if (is.finite(z$outside_second20 %||% NA_real_)) paste0(z$outside_second20, "/20 outside") else "—"), "</td></tr>"),
          paste0("<tr><th>Files · ", esc(z$partition_label %||% "partition"), "</th><td>", esc(src[[1]] %||% "—"), if (length(src) >= 2 && nzchar(src[[2]] %||% "")) paste0(" → ", esc(src[[2]])) else "", "</td></tr>"),
          paste0("<tr><th>Decision · ", esc(z$partition_label %||% "partition"), "</th><td><b>", esc(z$decision %||% "—"), "</b></td></tr>")
        )
      }
    } else {
      src_rows <- character(0)
      src <- main_result$cohort_sources %||% NULL
      if (!is.null(src) && length(src)) {
        src_rows <- c(
          paste0("<tr><th>File cohort 1</th><td>", esc(src[[1]] %||% "—"), "</td></tr>"),
          paste0("<tr><th>File cohort 2</th><td>", esc(if (length(src) >= 2 && nzchar(src[[2]] %||% "")) src[[2]] else "—"), "</td></tr>")
        )
      }
      review_rows <- character(0)
      ex <- main_result$cohort_exclusions %||% data.frame()
      if (is.data.frame(ex) && nrow(ex)) {
        review_rows <- c(review_rows, paste0("<tr><th>Documented cohort exclusions</th><td>", nrow(ex), "</td></tr>"))
        for (ii in seq_len(nrow(ex))) {
          who <- ex$patient_id[ii] %||% paste0("row ", ex$row_id[ii] %||% ii)
          review_rows <- c(review_rows,
            paste0("<tr><th>Exclusion · ", esc(who), "</th><td>Result ", esc(format_lab_number(ex$value[ii], digits)), " · ", esc(ex$reason[ii] %||% "—"), "</td></tr>"))
        }
      }
      reps <- main_result$replacement_sources %||% character(0)
      if (length(reps)) {
        review_rows <- c(review_rows, paste0("<tr><th>File(s) of substitution/completion</th><td>", esc(paste(unique(reps), collapse = " · ")), "</td></tr>"))
      }
      if (!is.null(main_result$cohort_review_policy)) {
        review_rows <- c(review_rows, paste0("<tr><th>Policy of exclusion</th><td>", esc(main_result$cohort_review_policy), "</td></tr>"))
      }
      if (!is.null(main_result$extreme_policy)) {
        ext_txt <- paste0("Cohort 1: ", main_result$extreme_first_n %||% 0L)
        if ((main_result$second_n %||% 0L) > 0L) ext_txt <- paste0(ext_txt, " · Cohort 2: ", main_result$extreme_second_n %||% 0L)
        review_rows <- c(review_rows,
          paste0("<tr><th>Statistical extreme-value signals</th><td>", esc(ext_txt), "</td></tr>"))
      }
      result_rows <- c(result_rows,
        paste0("<tr><th>Assignment of cohorts</th><td>", esc(main_result$cohort_assignment %||% "—"), "</td></tr>"),
        paste0("<tr><th>First cohort</th><td>", esc(if (is.finite(main_result$outside_first20 %||% NA_real_)) paste0(main_result$outside_first20, "/20 outside") else paste0(main_result$first_n %||% main_result$n, " individuals")), "</td></tr>"),
        paste0("<tr><th>Second cohort</th><td>", esc(if (is.finite(main_result$outside_second20 %||% NA_real_)) paste0(main_result$outside_second20, "/20 outside") else "—"), "</td></tr>"),
        src_rows,
        review_rows,
        paste0("<tr><th>Verification decision</th><td><b>", esc(main_result$decision %||% "—"), "</b></td></tr>")
      )
    }
  }
  if (identical(main_result$type %||% "", "indirect_verification")) {
    lab_status <- function(st) switch(st %||% "grey", green="🟢 Green", yellow="🟡 Yellow", red="🔴 Red", grey="⚪ Not evaluable", st %||% "—")
    refine_ci_text <- function(z) {
      tab <- z$refine$table %||% NULL
      if (is.null(tab) || !is.data.frame(tab) || !all(c("Percentile","CILow","CIHigh") %in% names(tab))) return("—")
      dz <- normalize_reference_design(z$reference_design %||% design)
      chunks <- character(0)
      if (isTRUE(dz$active[["lower"]])) {
        lo <- which.min(abs(tab$Percentile - dz$pair_percentiles[["lower"]]))
        if (all(is.finite(c(tab$CILow[lo], tab$CIHigh[lo])))) chunks <- c(chunks, paste0(reference_percentile_label(dz$pair_percentiles[["lower"]]), ": ", format_lab_interval(c(tab$CILow[lo], tab$CIHigh[lo]), digits)))
      }
      if (isTRUE(dz$active[["upper"]])) {
        hi <- which.min(abs(tab$Percentile - dz$pair_percentiles[["upper"]]))
        if (all(is.finite(c(tab$CILow[hi], tab$CIHigh[hi])))) chunks <- c(chunks, paste0(reference_percentile_label(dz$pair_percentiles[["upper"]]), ": ", format_lab_interval(c(tab$CILow[hi], tab$CIHigh[hi]), digits)))
      }
      if (length(chunks)) paste(chunks, collapse=" · ") else "—"
    }
    local_reflim_text <- function(z) {
      if (is.null(z$reflim)) return("—")
      dz <- normalize_reference_design(z$reference_design %||% design)
      rr <- extract_reflim_ri(z$reflim)
      if (!all(is.finite(rr))) return("—")
      if (identical(dz$tail, "two_sided")) reference_result_text(rr, dz, digits)
      else paste0("P2,5=", format_lab_number(rr[1], digits), " · P97,5=", format_lab_number(rr[2], digits), " (descriptive)")
    }
    local_refine_text <- function(z) if (!is.null(z$refine)) reference_result_text(z$refine$ri, z$reference_design %||% design, digits) else "—"
    status_rows_html <- function(z, prefix, confirmation = FALSE) {
      dz <- normalize_reference_design(z$reference_design %||% design)
      out <- character(0)
      if (isTRUE(dz$active[["lower"]])) {
        st <- if (!confirmation && !identical(dz$tail, "two_sided")) "not_applicable" else if (confirmation) z$verus_limit_status[["lower"]] else z$reflim_limit_status[["lower"]]
        lab <- if (identical(st, "not_applicable")) "— Not applicable: reflimR estimates P2.5/P97.5" else lab_status(st)
        out <- c(out, paste0("<tr><th>", prefix, " · ", reference_percentile_label(dz$pair_percentiles[["lower"]]), "</th><td>", esc(lab), "</td></tr>"))
      }
      if (isTRUE(dz$active[["upper"]])) {
        st <- if (!confirmation && !identical(dz$tail, "two_sided")) "not_applicable" else if (confirmation) z$verus_limit_status[["upper"]] else z$reflim_limit_status[["upper"]]
        lab <- if (identical(st, "not_applicable")) "— Not applicable: reflimR estimates P2.5/P97.5" else lab_status(st)
        out <- c(out, paste0("<tr><th>", prefix, " · ", reference_percentile_label(dz$pair_percentiles[["upper"]]), "</th><td>", esc(lab), "</td></tr>"))
      }
      out
    }
    sample_context_text <- function(z) z$sample_size_context$message %||% "—"
    if (isTRUE(main_result$partitioned)) {
      result_rows <- c(result_rows,
        paste0("<tr><th>Indirect verification</th><td>Partitioned RIs by ", esc(if (identical(main_result$partition_type, "age")) "quantitative variable" else "qualitative variable"), "</td></tr>"),
        paste0("<tr><th>Overall decision</th><td><b>", esc(main_result$decision %||% "—"), "</b></td></tr>")
      )
      for (z in main_result$partition_results %||% list()) {
        result_rows <- c(result_rows,
          paste0("<tr><th>Partition</th><td><b>", esc(z$partition_label %||% "—"), "</b></td></tr>"),
          paste0("<tr><th>candidate RI · ", esc(z$partition_label %||% "partition"), "</th><td>", esc(reference_result_text(z$target, z$reference_design %||% design, digits)), "</td></tr>"),
          paste0("<tr><th>RI local reflimR · ", esc(z$partition_label %||% "partition"), "</th><td>", esc(local_reflim_text(z)), "</td></tr>"),
          if (isTRUE(z$confirmation_run)) paste0("<tr><th>RI local refineR · ", esc(z$partition_label %||% "partition"), "</th><td>", esc(local_refine_text(z)), "</td></tr>") else "",
          paste0("<tr><th>Context of sample size · ", esc(z$partition_label %||% "partition"), "</th><td>", esc(sample_context_text(z)), "</td></tr>"),
          status_rows_html(z, "reflimR/EL", confirmation = FALSE),
          if (isTRUE(z$confirmation_run)) status_rows_html(z, "refineR/VeRUS", confirmation = TRUE) else "<tr><th>Confirmation refineR/VeRUS</th><td>Not required</td></tr>",
          if (isTRUE(z$confirmation_run)) paste0("<tr><th>Bootstrap refineR · ", esc(z$partition_label %||% "partition"), "</th><td>", esc(paste0(z$n_bootstrap %||% 0L, " replicates · ", if (identical(z$bootstrap_mode %||% "", "test")) "test mode" else "final analysis")), "</td></tr>") else "",
          if (isTRUE(z$confirmation_run)) paste0("<tr><th>95% CI bootstrap refineR · ", esc(z$partition_label %||% "partition"), "</th><td>", esc(refine_ci_text(z)), "</td></tr>") else "",
          paste0("<tr><th>Decision · ", esc(z$partition_label %||% "partition"), "</th><td><b>", esc(z$decision %||% "—"), "</b></td></tr>")
        )
      }
    } else {
      result_rows <- c(result_rows,
        paste0("<tr><th>Stage reached</th><td>", esc(c(adequacy="suitability", screening="screening", confirmation="confirmation", exploration="exploration")[[main_result$stage %||% ""]] %||% main_result$stage %||% "—"), "</td></tr>"),
        status_rows_html(main_result, "reflimR/EL", confirmation = FALSE),
        paste0("<tr><th>Confirmation refineR/VeRUS</th><td>", esc(if (isTRUE(main_result$confirmation_run)) "Executed" else "Not required"), "</td></tr>"),
        if (isTRUE(main_result$confirmation_run)) paste0("<tr><th>Bootstrap refineR</th><td>", esc(paste0(main_result$n_bootstrap %||% 0L, " replicates · ", if (identical(main_result$bootstrap_mode %||% "", "test")) "test mode" else "final analysis")), "</td></tr>") else "",
        if (isTRUE(main_result$confirmation_run)) paste0("<tr><th>refineR point estimate</th><td>", esc(if (identical(main_result$refine_point_method %||% "", "medianBS")) "Median of bootstrap models (medianBS)" else if (identical(main_result$refine_point_method %||% "", "fullDataEst")) "Estimate from the complete dataset (fullDataEst)" else main_result$refine_point_method %||% "—"), "</td></tr>") else "",
        if (isTRUE(main_result$confirmation_run)) paste0("<tr><th>95% CI bootstrap refineR</th><td>", esc(refine_ci_text(main_result)), "</td></tr>") else "",
        if (isTRUE(main_result$confirmation_run) && identical(main_result$bootstrap_mode %||% "", "test")) "<tr><th>Warning</th><td>Reduced number of bootstrap replicates for functional validation. Do not interpret the bootstrap CI as a final estimate.</td></tr>" else "",
        if (isTRUE(main_result$confirmation_run)) status_rows_html(main_result, "VeRUS/UM", confirmation = TRUE) else "",
        paste0("<tr><th>Verification decision</th><td><b>", esc(main_result$decision %||% "—"), "</b></td></tr>"),
        paste0("<tr><th>Integration principle</th><td>", esc(main_result$evidence_note %||% "—"), "</td></tr>"),
        paste0("<tr><th>Context of sample size</th><td>", esc(main_result$sample_size_context$message %||% "—"), "</td></tr>")
      )
      ex <- main_result$exploration %||% NULL
      if (!is.null(ex)) {
        result_rows <- c(result_rows,
          paste0("<tr><th>Exploration mclust</th><td>", esc(ex$mclust$message %||% "Not available"), "</td></tr>"),
          paste0("<tr><th>Exploration rpart</th><td>", esc(ex$rpart$message %||% "Not available"), "</td></tr>"),
          "<tr><th>Use of exploration</th><td>Hypothesis generation only; it does not automatically modify the candidate RI.</td></tr>"
        )
      }
    }
  }
  if (!is.null(main_result$confidence)) {
    conf_lab <- if (identical(main_result$module %||% "", "D7")) "Context of sample size" else "Confidence methodological"
    result_rows <- c(result_rows, paste0("<tr><th>", conf_lab, "</th><td>", esc(main_result$confidence), "</td></tr>"))
  }
  if (!is.null(main_result$method)) {
    result_rows <- c(result_rows, paste0("<tr><th>Method</th><td>", esc(main_result$method), "</td></tr>"))
  }
  result_rows <- c(result_rows, paste0("<tr><th>n analyzed</th><td>", esc(fmt_integer(main_result$n %||% NA)), "</td></tr>"))

  direct_precision <- NULL
  if (identical(main_result$type, "direct_establishment")) {
    sides <- names(design$active)[design$active]
    direct_precision <- do.call(rbind, lapply(sides, function(side) {
      ii <- if (identical(side, "lower")) 1L else 2L
      ci <- if (identical(side, "lower")) main_result$ci90_lower else main_result$ci90_upper
      data.frame(
        Limit = reference_percentile_label(design$pair_percentiles[[side]]),
        Estimate = format_lab_number(main_result$ri[ii], digits),
        `90% CI` = format_lab_interval(ci, digits),
        `CI width / computational RI width` = format_percent(main_result$precision_ratio[ii], 1),
        Criterion = if (isTRUE(main_result$precision_ratio[ii] < (main_result$precision_threshold %||% .20))) status_symbol_text("green", "Meets") else status_symbol_text("red", "Does not meet"),
        check.names = FALSE, stringsAsFactors = FALSE
      )
    }))
  }
  outlier_tab <- tryCatch(direct_outlier_display_table(main_result), error = function(e) NULL)
  subgroup_outlier_tabs <- list()
  if (!is.null(partition_result$subgroup_outlier_summary) && nrow(partition_result$subgroup_outlier_summary)) {
    z <- partition_result$subgroup_outlier_summary; z$Scope <- "Qualitative variable"; subgroup_outlier_tabs[[length(subgroup_outlier_tabs)+1]] <- z
  }
  av <- age_result$cut_validation %||% NULL
  if (!is.null(av$subgroup_outlier_summary) && nrow(av$subgroup_outlier_summary)) {
    z <- av$subgroup_outlier_summary
    if ("Group" %in% names(z)) z$Group <- vapply(z$Group, function(g) age_group_operational_label(age_result, g), character(1))
    z$Scope <- "Quantitative variable"; subgroup_outlier_tabs[[length(subgroup_outlier_tabs)+1]] <- z
  }
  subgroup_outlier_tab <- if (length(subgroup_outlier_tabs)) {
    z <- do.call(rbind, subgroup_outlier_tabs); z[, c("Scope", setdiff(names(z), "Scope")), drop=FALSE]
  } else NULL
  distribution_tab <- tryCatch(direct_distribution_display_table(main_result), error = function(e) NULL)
  method_candidates_tab <- tryCatch(direct_method_candidates_display_table(main_result), error = function(e) NULL)

  is_d7 <- identical(main_result$module %||% "", "D7") && identical(main_result$type %||% "", "indirect_establishment")
  methods <- tryCatch(if (is_d7) d7_methods_table(main_result) else indirect_methods_table(main_result), error = function(e) NULL)
  d7_summary <- if (is_d7) tryCatch(d7_summary_table(main_result), error = function(e) NULL) else NULL
  d7_environment <- if (is_d7) tryCatch(d7_environment_table(main_result), error = function(e) NULL) else NULL
  d7_partition <- if (is_d7) main_result$partition$table %||% NULL else NULL
  d7_partition_criteria <- if (is_d7) main_result$partition$criteria_table %||% NULL else NULL
  d7_partition_methods <- if (is_d7) main_result$partition$methods_table %||% NULL else NULL
  d7_lahti <- if (is_d7) main_result$partition$lahti$table %||% NULL else NULL
  d7_lahti_distance <- if (is_d7) main_result$partition$lahti$distance_table %||% NULL else NULL
  if (!is.null(d7_lahti) && is.data.frame(d7_lahti) && nrow(d7_lahti)) {
    if ("Modeled proportion outside overall RI" %in% names(d7_lahti)) d7_lahti[["Modeled proportion outside overall RI"]] <- ifelse(is.finite(d7_lahti[["Modeled proportion outside overall RI"]]), paste0(formatC(100*d7_lahti[["Modeled proportion outside overall RI"]], format="f", digits=2, decimal.mark=","), " %"), "—")
    if ("Status" %in% names(d7_lahti)) d7_lahti$Status <- vapply(d7_lahti$Status, function(st) switch(st, green="Compatible with common RI", yellow="Marginal", red="Supports partitioning", "Not evaluable"), character(1))
  }
  if (!is.null(d7_lahti_distance) && is.data.frame(d7_lahti_distance) && nrow(d7_lahti_distance) && "Status" %in% names(d7_lahti_distance)) {
    d7_lahti_distance$Status <- vapply(d7_lahti_distance$Status, function(st) switch(st, green="Compatible with common RI", yellow="Marginal", red="Supports partitioning", "Not evaluable"), character(1))
  }
  if (!is.null(d7_partition_methods) && is.data.frame(d7_partition_methods)) {
    for (nm in intersect(c("LRL","URL"), names(d7_partition_methods))) d7_partition_methods[[nm]] <- ifelse(is.finite(d7_partition_methods[[nm]]), signif(d7_partition_methods[[nm]], 6), NA)
  }
  if (!isTRUE(design$active[["lower"]]) && !is.null(d7_partition_methods)) d7_partition_methods <- d7_partition_methods[, !grepl("LRL|lower", names(d7_partition_methods), ignore.case=TRUE), drop=FALSE]
  if (!isTRUE(design$active[["upper"]]) && !is.null(d7_partition_methods)) d7_partition_methods <- d7_partition_methods[, !grepl("URL|upper", names(d7_partition_methods), ignore.case=TRUE), drop=FALSE]
  d7_age <- if (is_d7) main_result$age$bins %||% NULL else NULL
  d7_age_model_candidate <- is_d7 && d7_continuous_model_is_candidate(main_result$age$model %||% NULL)
  d7_age_model <- if (d7_age_model_candidate) main_result$age$model$table %||% NULL else NULL
  d7_age_windows <- if (is_d7) main_result$age$model$windows %||% NULL else NULL
  d7_age_curve <- if (d7_age_model_candidate) main_result$age$model$curve %||% NULL else NULL
  d7_age_application <- if (d7_age_model_candidate) tryCatch(
    d7_continuous_application_table(main_result$age$model, age_like = quantitative_age_like_report),
    error = function(e) NULL) else NULL
  d7_age_sil <- if (d7_age_model_candidate) tryCatch(
    d7_continuous_operational_discretization(main_result$age$model, age_like = quantitative_age_like_report, tolerance = 0.10, max_groups = 10L),
    error = function(e) NULL) else NULL
  d7_age_sil_tab <- if (!is.null(d7_age_sil) && isTRUE(d7_age_sil$ok)) d7_continuous_operational_display_table(d7_age_sil) else NULL
  d7_age_sil_tradeoff <- if (!is.null(d7_age_sil) && isTRUE(d7_age_sil$ok)) d7_continuous_tradeoff_display_table(d7_age_sil) else NULL
  if (!is.null(d7_age) && is.data.frame(d7_age) && nrow(d7_age)) {
    if (ncol(d7_age) >= 3L) names(d7_age)[1:3] <- c(paste0("Representative ", quantitative_covariate_label), "n", "Median")
    d7_age[[1]] <- signif(d7_age[[1]], 6)
    if ("Median" %in% names(d7_age)) d7_age[["Median"]] <- signif(d7_age[["Median"]], 6)
  }
  if (!is.null(d7_age_model) && is.data.frame(d7_age_model) && nrow(d7_age_model)) {
    if (!isTRUE(design$active[["lower"]]) && "LRL" %in% names(d7_age_model)) d7_age_model$LRL <- NULL
    if (!isTRUE(design$active[["upper"]]) && "URL" %in% names(d7_age_model)) d7_age_model$URL <- NULL
    xcols <- setdiff(names(d7_age_model), c("LRL","P50","URL"))
    for (nm in intersect(c(xcols[1],"LRL","P50","URL"), names(d7_age_model))) d7_age_model[[nm]] <- signif(d7_age_model[[nm]], 6)
  }
  if (!is.null(d7_age_application) && is.data.frame(d7_age_application) && nrow(d7_age_application)) {
    if (!isTRUE(design$active[["lower"]]) && "LRL" %in% names(d7_age_application)) d7_age_application$LRL <- NULL
    if (!isTRUE(design$active[["upper"]]) && "URL" %in% names(d7_age_application)) d7_age_application$URL <- NULL
    for (nm in names(d7_age_application)) if (is.numeric(d7_age_application[[nm]])) d7_age_application[[nm]] <- signif(d7_age_application[[nm]], 6)
  }
  if (!is.null(d7_age_windows) && is.data.frame(d7_age_windows) && nrow(d7_age_windows)) {
    if (!identical(design$tail, "two_sided")) {
      if (!isTRUE(design$active[["lower"]]) && "LRL" %in% names(d7_age_windows)) d7_age_windows$LRL <- NULL
      if (!isTRUE(design$active[["upper"]]) && "URL" %in% names(d7_age_windows)) d7_age_windows$URL <- NULL
      if ("LRL reflimR" %in% names(d7_age_windows)) names(d7_age_windows)[names(d7_age_windows)=="LRL reflimR"] <- "P2,5 reflimR (descriptive)"
      if ("URL reflimR" %in% names(d7_age_windows)) names(d7_age_windows)[names(d7_age_windows)=="URL reflimR"] <- "P97,5 reflimR (descriptive)"
      if ("Agreement" %in% names(d7_age_windows)) d7_age_windows$Agreement <- "Not applicable to P5/P95"
    }
    if ("Non-pathological fraction" %in% names(d7_age_windows)) d7_age_windows[["Non-pathological fraction"]] <- ifelse(is.finite(d7_age_windows[["Non-pathological fraction"]]), paste0(formatC(100*d7_age_windows[["Non-pathological fraction"]], format="f", digits=1, decimal.mark=","), " %"), "—")
    if ("Agreement" %in% names(d7_age_windows) && identical(design$tail, "two_sided")) d7_age_windows$Agreement <- vapply(d7_age_windows$Agreement, function(st) switch(st, green="Favorable", yellow="Intermediate", red="Unfavorable", "Not evaluable"), character(1))
  }
  d7_mclust_components <- NULL
  if (is_d7) {
    tb <- main_result$exploration$mclust$component_table %||% NULL
    if (!is.null(tb) && is.data.frame(tb) && nrow(tb)) {
      d7_mclust_components <- tb
      if ("Proportion" %in% names(d7_mclust_components)) d7_mclust_components$Proportion <- ifelse(is.finite(d7_mclust_components$Proportion), paste0(formatC(100*d7_mclust_components$Proportion, format="f", digits=1, decimal.mark=","), " %"), "—")
      if ("Mean" %in% names(d7_mclust_components)) d7_mclust_components$Mean <- ifelse(is.finite(d7_mclust_components$Mean), formatC(d7_mclust_components$Mean, format="f", digits=digits, decimal.mark=","), "—")
      if ("DE" %in% names(d7_mclust_components)) d7_mclust_components$DE <- ifelse(is.finite(d7_mclust_components$DE), formatC(d7_mclust_components$DE, format="f", digits=digits, decimal.mark=","), "—")
    }
  }
  d7_references <- if (is_d7) main_result$references %||% character(0) else character(0)
  d7_refine_graph <- ""
  if (is_d7 && !is.null(main_result$refine$fit)) {
    pm <- main_result$refine$point_method %||% "fullDataEst"
    d7_refine_graph <- svg_plot_fragment(function() {
      plot(main_result$refine$fit, RIperc = as.numeric(design$pair_percentiles), showCI = TRUE,
           showPathol = TRUE, showBSModels = FALSE, pointEst = pm,
           xlab = paste((state$metadata %||% list())$analyte %||% "Result", (state$metadata %||% list())$unit %||% ""),
           title = "D7 · refineR fit")
    }, width = 7.0, height = 4.2)
  }
  d7_partition_graphs <- character(0)
  if (is_d7 && identical(main_result$partition$decision %||% "", "partition")) {
    subs <- main_result$partition$substudies %||% list(); gr <- main_result$partition$groups %||% names(subs)
    for (ii in seq_along(subs)) {
      z <- subs[[ii]]
      if (is.null(z$refine$fit)) next
      pm <- z$refine$point_method %||% "fullDataEst"
      gg <- svg_plot_fragment(function() {
        plot(z$refine$fit, RIperc=as.numeric(normalize_reference_design(z$reference_design %||% design)$pair_percentiles), showCI=TRUE, showPathol=TRUE, showBSModels=FALSE, pointEst=pm,
             xlab=paste((state$metadata %||% list())$analyte %||% "Result", (state$metadata %||% list())$unit %||% ""),
             title=paste0("D7 · ", gr[ii] %||% paste0("Group ",ii), " · refineR"))
      }, width=6.4, height=3.8)
      d7_partition_graphs <- c(d7_partition_graphs, paste0("<h3>", esc(gr[ii] %||% paste0("Group ",ii)), "</h3><div class='plotbox'>", gg, "</div>"))
    }
  }
  d7_age_graph <- ""
  if (is_d7 && !is.null(d7_age_curve) && is.data.frame(d7_age_curve) && nrow(d7_age_curve)) {
    d7_age_graph <- svg_plot_fragment(function() {
      z <- d7_age_curve
      yl <- paste((state$metadata %||% list())$analyte %||% "Result", (state$metadata %||% list())$unit %||% "")
      xcol <- setdiff(names(z), c("LRL","P50","URL"))[1]
      if (is.na(xcol) || !nzchar(xcol)) return(invisible(NULL))
      xx <- z[[xcol]]
      if (identical(design$tail, "two_sided")) {
        yr <- range(c(z$LRL,z$P50,z$URL), finite=TRUE)
        plot(xx,z$P50,type="l",ylim=yr,xlab=quantitative_covariate_label,ylab=yl,main=paste0("D7 · Candidate continuous RI by ", quantitative_covariate_label))
        lines(xx,z$LRL,lty=2); lines(xx,z$URL,lty=2)
        legend("topleft",legend=c("P50","LRL","URL"),lty=c(1,2,2),bty="n")
      } else {
        side <- if (isTRUE(design$active[["lower"]])) "LRL" else "URL"
        plot(xx,z[[side]],type="l",xlab=quantitative_covariate_label,ylab=yl,main=paste0("D7 · ",reference_limit_name(design)," according to ",quantitative_covariate_label))
        legend("topleft",legend=reference_design_percentile_text(design),lty=1,bty="n")
      }
    }, width=7.0, height=4.2)
  }

  d6_methods <- if (identical(main_result$type %||% "", "indirect_verification")) tryCatch(indirect_verification_display_table(main_result), error=function(e) NULL) else NULL
  d6_mclust_components <- NULL
  if (identical(main_result$type %||% "", "indirect_verification")) {
    collect_mclust <- function(z, label = NULL) {
      tb <- z$exploration$mclust$component_table %||% NULL
      if (is.null(tb) || !is.data.frame(tb) || !nrow(tb)) return(NULL)
      out <- tb
      if (!is.null(label)) out <- cbind(Partition = label, out, stringsAsFactors = FALSE)
      if ("Proportion" %in% names(out)) out$Proportion <- ifelse(is.finite(out$Proportion), paste0(formatC(100*out$Proportion, format="f", digits=1, decimal.mark=","), " %"), "—")
      if ("Mean" %in% names(out)) out$Mean <- ifelse(is.finite(out$Mean), formatC(out$Mean, format="f", digits=digits, decimal.mark=","), "—")
      if ("DE" %in% names(out)) out$DE <- ifelse(is.finite(out$DE), formatC(out$DE, format="f", digits=digits, decimal.mark=","), "—")
      out
    }
    if (isTRUE(main_result$partitioned)) {
      tabs <- lapply(main_result$partition_results %||% list(), function(z) collect_mclust(z, z$partition_label %||% "Partition"))
      tabs <- Filter(Negate(is.null), tabs)
      if (length(tabs)) d6_mclust_components <- do.call(rbind, tabs)
    } else {
      d6_mclust_components <- collect_mclust(main_result)
    }
  }
  d6_references <- if (identical(main_result$type %||% "", "indirect_verification")) main_result$references %||% character(0) else character(0)
  d6_environment <- NULL
  if (identical(main_result$type %||% "", "indirect_verification")) {
    pv <- function(pkg) if (requireNamespace(pkg, quietly = TRUE)) as.character(utils::packageVersion(pkg)) else "not installed"
    seeds <- if (isTRUE(main_result$partitioned)) {
      zz <- main_result$partition_results %||% list()
      paste(vapply(zz, function(z) paste0(z$partition_label %||% "Partition", ": ", if (is.finite(z$refine_seed %||% NA_real_)) z$refine_seed else "—"), character(1)), collapse = " · ")
    } else if (is.finite(main_result$refine_seed %||% NA_real_)) as.character(main_result$refine_seed) else "—"
    d6_environment <- data.frame(
      Element = c("R", "reflimR", "refineR", "mclust", "Seed refineR"),
      Version = c(R.version.string, pv("reflimR"), pv("refineR"), pv("mclust"), seeds),
      check.names = FALSE, stringsAsFactors = FALSE
    )
  }
  if (!is.null(methods)) {
    num <- intersect(c("LRL", "Median", "URL"), names(methods))
    for (nm in num) methods[[nm]] <- ifelse(is.finite(methods[[nm]]), signif(methods[[nm]], 6), NA)
  }

  ptab <- NULL
  partition_pairwise_tab <- NULL
  hb_text <- NULL
  if (!is.null(partition_result)) {
    if (!is.null(partition_result$pairwise_table) && is.data.frame(partition_result$pairwise_table) && nrow(partition_result$pairwise_table)) {
      partition_pairwise_tab <- partition_result$pairwise_table
    }
    if (length(partition_result$group_results %||% list()) > 2L) {
      grs <- partition_result$group_results
      ptab <- do.call(rbind, lapply(names(grs), function(g) {
        z <- grs[[g]]
        data.frame(Category=g, n=z$n %||% NA_integer_,
                   LRL=if (!is.null(z$ri)) signif(z$ri[1],6) else NA_real_,
                   URL=if (!is.null(z$ri)) signif(z$ri[2],6) else NA_real_,
                   stringsAsFactors=FALSE, check.names=FALSE)
      }))
    } else if (!is.null(partition_result$lahti)) {
      ptab <- direct_partition_display_table(partition_result)
      hb <- partition_result$harris_boyd %||% NULL
      if (!is.null(hb)) {
        hb_text <- paste0(
          "Harris-Boyd: ", harris_boyd_label(hb),
          " · Z = ", ifelse(is.finite(hb$z), formatC(hb$z, format="f", digits=2, decimal.mark=","), "—"),
          " · Z* = ", ifelse(is.finite(hb$z_critical), formatC(hb$z_critical, format="f", digits=2, decimal.mark=","), "—"),
          " · relationship of DE = ", ifelse(is.finite(hb$sd_ratio), formatC(hb$sd_ratio, format="f", digits=2, decimal.mark=","), "—")
        )
      }
    } else if (!is.null(partition_result$ri_group1)) {
      ptab <- data.frame(Group = partition_result$groups,
                         LRL = signif(c(partition_result$ri_group1[1], partition_result$ri_group2[1]), 6),
                         URL = signif(c(partition_result$ri_group1[2], partition_result$ri_group2[2]), 6),
                         check.names = FALSE)
    }
  }

  shape_tab <- NULL
  if (!is.null(partition_result$discordance_guidance) && !is.null(partition_result$discordance_guidance$group1)) {
    dg <- partition_result$discordance_guidance
    d1 <- dg$group1; d2 <- dg$group2
    fmtp <- function(x) format_p_value(x)
    shape_tab <- data.frame(
      Group = vapply(partition_result$groups[1:2], pretty_group_label, character(1)),
      `Bowley skewness` = c(ifelse(is.finite(d1$bowley), formatC(d1$bowley,format="f",digits=3,decimal.mark=","),"—"), ifelse(is.finite(d2$bowley), formatC(d2$bowley,format="f",digits=3,decimal.mark=","),"—")),
      `Excess kurtosis` = c(ifelse(is.finite(d1$excess_kurtosis), formatC(d1$excess_kurtosis,format="f",digits=2,decimal.mark=","),"—"), ifelse(is.finite(d2$excess_kurtosis), formatC(d2$excess_kurtosis,format="f",digits=2,decimal.mark=","),"—")),
      `AD p` = c(fmtp(d1$normality_p), fmtp(d2$normality_p)),
      Interpretation = c(if (isTRUE(d1$caution)) "Distribution shape/tails require caution" else "No important shape/tail signal", if (isTRUE(d2$caution)) "Distribution shape/tails require caution" else "No important shape/tail signal"),
      check.names=FALSE, stringsAsFactors=FALSE)
  }

  atab <- tryCatch(age_partition_display_table(age_result), error=function(e) NULL)
  age_diag_tab <- tryCatch(age_diagnostics_display_table(age_result), error=function(e) NULL)
  age_curve_tab <- tryCatch(age_curve_display_table(age_result), error=function(e) NULL)
  age_sil_tab <- tryCatch(age_sil_display_table(age_result), error=function(e) NULL)
  age_sil_tradeoff_tab <- tryCatch(age_sil_tradeoff_display_table(age_result), error=function(e) NULL)
  age_boundary_tab <- tryCatch(age_boundary_outlier_display_table(age_result), error=function(e) NULL)
  age_cut_validation_tab <- tryCatch(age_cut_validation_display_table(age_result), error=function(e) NULL)
  atab <- genericize_quantitative_report_table(atab)
  age_diag_tab <- genericize_quantitative_report_table(age_diag_tab)
  age_curve_tab <- genericize_quantitative_report_table(age_curve_tab)
  age_sil_tab <- genericize_quantitative_report_table(age_sil_tab)
  age_sil_tradeoff_tab <- genericize_quantitative_report_table(age_sil_tradeoff_tab)
  age_boundary_tab <- genericize_quantitative_report_table(age_boundary_tab)
  age_cut_validation_tab <- genericize_quantitative_report_table(age_cut_validation_tab)

  audit <- state$audit %||% NULL
  if (!is.null(audit) && nrow(audit)) {
    names(audit) <- c("Stage", "n")
    audit$n <- vapply(audit$n, fmt_integer, character(1))
  }

  outlier_decisions <- state$outlier_decisions %||% NULL
  app_version <- state$app_version %||% "1.0.1"
  report_is_final <- isTRUE(final$is_final)
  report_state_label <- if (report_is_final) "FINAL REPORT" else "PROVISIONAL REPORT"
  pres <- state$partition_resolution %||% NULL
  outlier_decision_tab <- NULL
  if (!is.null(outlier_decisions) && is.data.frame(outlier_decisions) && nrow(outlier_decisions)) {
    outlier_decision_tab <- data.frame(
      Data = outlier_decisions$timestamp,
      Iteration = outlier_decisions$iteration,
      `ID/row` = ifelse(nzchar(outlier_decisions$patient_id),
                          paste0(outlier_decisions$patient_id, " / ", outlier_decisions$row_id),
                          as.character(outlier_decisions$row_id)),
      Value = outlier_decisions$value,
      Scope = if ("scope" %in% names(outlier_decisions)) outlier_decisions$scope else "Complete population",
      Decision = outlier_decisions$decision,
      Reason = vapply(outlier_decisions$category %||% rep("", nrow(outlier_decisions)), reason_label, character(1)),
      Justification = outlier_decisions$reason,
      check.names = FALSE, stringsAsFactors = FALSE
    )
  }

  outlier_impacts <- state$outlier_impacts %||% NULL
  outlier_impact_tab <- NULL
  if (!is.null(outlier_impacts) && is.data.frame(outlier_impacts) && nrow(outlier_impacts)) {
    oi <- outlier_impacts[nrow(outlier_impacts), , drop=FALSE]
    outlier_impact_tab <- data.frame(
      Moment = c("Before exclusion", "After recalculation"),
      n = c(fmt_integer(oi$n_before), fmt_integer(oi$n_after)),
      IR = c(oi$ri_before, oi$ri_after),
      `Precision LRL` = c(format_percent(oi$pr_lower_before,1), format_percent(oi$pr_lower_after,1)),
      `Precision URL` = c(format_percent(oi$pr_upper_before,1), format_percent(oi$pr_upper_after,1)),
      check.names=FALSE, stringsAsFactors=FALSE
    )
  }

  pres <- state$partition_resolution %||% NULL
  pdecision_tab <- NULL
  if (!is.null(pres) && (pres$decision %||% "") %in% c("partition","common")) {
    pdecision_tab <- data.frame(
      Data = pres$timestamp %||% "—",
      `RIveR proposal` = switch(pres$system_favored %||% "review", partition="Favors the partition", common="Favors the common RI", "No automatic preference"),
      `Specialist decision` = if (identical(pres$decision,"partition")) "Adopt separate RIs" else "Retain common RI",
      Basis = reason_label(pres$category),
      Justification = pres$reason %||% "—",
      check.names=FALSE, stringsAsFactors=FALSE
    )
  }

  sres <- state$small_sample_resolution %||% NULL
  sdecision_tab <- NULL
  if (!is.null(sres) && (sres$decision %||% "") != "pending") {
    no_model_dec <- (sres$decision %||% "") %in% c("investigate","review_population","increase_no_model","no_ri","other")
    dec_txt <- switch(sres$decision %||% "",
      adopt="Adopt the proposed RI", increase="Do not adopt yet / increase sample size",
      investigate="Investigate possible subpopulations and repeat the analysis",
      review_population="Review the selection/inclusion of subjects",
      increase_no_model="Increase the population and reassess",
      no_ri="Close the study without establishing an RI",
      other="Another documented action", sres$decision %||% "—")
    if (no_model_dec) {
      sdecision_tab <- data.frame(
        Data = sres$timestamp %||% "—",
        `Specialist decision` = dec_txt,
        `Result of the study` = "RI not established",
        `Exploratory RI` = format_lab_interval(sres$ri %||% main_result$ri, digits),
        Justification = sres$reason %||% "—",
        check.names=FALSE, stringsAsFactors=FALSE
      )
    } else {
      sdecision_tab <- data.frame(
        Data = sres$timestamp %||% "—",
        `Specialist decision` = dec_txt,
        Method = sres$method %||% main_result$method %||% "—",
        n = fmt_integer(sres$n %||% main_result$n %||% NA),
        IR = format_lab_interval(sres$ri %||% main_result$ri, digits),
        `Precision LRL` = format_percent(sres$precision_lower %||% main_result$precision_ratio[1],1),
        `Precision URL` = format_percent(sres$precision_upper %||% main_result$precision_ratio[2],1),
        Basis = reason_label(sres$category),
        Justification = sres$reason %||% "—",
        check.names=FALSE, stringsAsFactors=FALSE
      )
    }
  }

  meta <- state$metadata %||% list()
  design <- normalize_reference_design(main_result$reference_design %||% meta$reference_design)
  final_status <- final$status %||% main_result$status %||% "grey"
  final_head <- final$headline %||% status_label(final_status)
  final_text <- final$text %||% main_result$recommendation %||% ""
  final_action <- final$action %||% main_result$action %||% final_text
  recommended_payload <- tryCatch(rilctms_recommended_result_payload(main_result, partition_result, age_result, final), error=function(e) NULL)
  recommended_html <- if (!is.null(recommended_payload) && isTRUE(recommended_payload$show)) {
    if (isTRUE(recommended_payload$has_values) && !is.null(recommended_payload$table)) {
      paste0("<div class='card ", esc(recommended_payload$status %||% "green"), "'><h2>Result RIveR proposes for approval</h2><h3>",
             esc(recommended_payload$title %||% "Recommended result"), "</h3>", df_html(recommended_payload$table),
             "<p class='small'>", esc(recommended_payload$note %||% ""), "</p>",
             if (!is.null(recommended_payload$secondary_table) && is.data.frame(recommended_payload$secondary_table) && nrow(recommended_payload$secondary_table))
               paste0("<h3>", esc(recommended_payload$secondary_title %||% "Continuous model GAMLSS"), "</h3>",
                      df_html(recommended_payload$secondary_table),
                      "<p class='small'>", esc(recommended_payload$secondary_note %||% ""), "</p>") else "",
             "</div>")
    } else {
      paste0("<div class='card ", esc(recommended_payload$status %||% "yellow"), "'><h2>",
             esc(recommended_payload$title %||% "No recommended numerical result"), "</h2><p>",
             esc(recommended_payload$note %||% ""), "</p></div>")
    }
  } else ""
  report_is_final <- isTRUE(final$is_final)
  report_state_label <- if (report_is_final) "FINAL REPORT" else "PROVISIONAL REPORT"
  graph_html <- distribution_graph_html()
  age_graph <- age_graph_html()
  outlier_global_graph <- outlier_boxplot_html("overall")
  outlier_subgroup_graph <- outlier_boxplot_html("subgroups")

  html <- paste0(
    "<!doctype html><html><head><meta charset='utf-8'><title>RIveR - Report</title>",
    "<style>body{font-family:Arial,sans-serif;margin:38px;color:#20252b}h1{margin-bottom:0}.sub{color:#68717c;margin-top:4px}.card{border:1px solid #d9dee5;border-radius:10px;padding:16px;margin:18px 0}table{border-collapse:collapse;width:100%;font-size:13px}th,td{text-align:left;border-bottom:1px solid #e8ebef;padding:8px;vertical-align:top}.card>table th{width:34%}.wide th{width:auto;background:#f5f7fa}.green{border-left:6px solid #2f855a}.yellow{border-left:6px solid #b7791f}.red{border-left:6px solid #c53030}.grey{border-left:6px solid #718096}.action{font-size:17px;font-weight:600}.small{font-size:12px;color:#666}.plotgrid{display:grid;grid-template-columns:1fr 1fr;gap:16px}.plotgroup{min-width:0}.plotbox{border:1px solid #edf0f3;border-radius:8px;padding:6px;margin:8px 0;overflow:hidden}.plotbox svg,.plotbox .diagimg{width:100%;height:auto;display:block}@media(max-width:850px){.plotgrid{grid-template-columns:1fr}}</style></head><body>",
    "<h1>RIveR</h1><div class='sub'>Report of study reference intervals · RIveR v", app_version, " · <b>", report_state_label, "</b></div>",
    "<div class='card'><h2>Identification</h2><table>",
    "<tr><th>Study</th><td>", esc(val(meta$study_name)), "</td></tr>",
    "<tr><th>Measurand</th><td>", esc(val(meta$analyte)), "</td></tr>",
    "<tr><th>Unit</th><td>", esc(val(meta$unit)), "</td></tr>",
    "<tr><th>Procedure/system</th><td>", esc(val(meta$system)), "</td></tr>",
    "<tr><th>Population</th><td>", esc(val(meta$population)), "</td></tr>",
    "<tr><th>Reference design</th><td>", esc(paste0(reference_design_label(design), " · ", reference_design_percentile_text(design))), "</td></tr>",
    "</table></div>",
    "<div class='card ", esc(final_status), "'><h2>", if (report_is_final && identical(final$final_decision %||% NULL, "RI NOT ESTABLISHED.")) "Final documented decision — RI not established" else if (report_is_final) "Final decision approved" else "Required specialist action", "</h2><p class='action'>", esc(final_action), "</p></div>",
    if (!is.null(audit)) paste0("<div class='card'><h2>Data traceability</h2>", df_html(audit), "</div>") else "",
    if (!is.null(outlier_tab)) paste0("<div class='card'><h2>Extreme / aberrant values</h2>", df_html(outlier_tab),
                                       "<p class='small'>Detection ≠ exclusion. Dixon/Reed uses D/R ≥1/3. Values between 1.5 and 3 IQR are shown as moderately distant and do not require individual review or automatic exclusion; they may reflect distribution shape. Values &gt;3 IQR or flagged by Dixon/Reed require specialist review.</p></div>") else "",
    outlier_global_graph,
    if (!is.null(subgroup_outlier_tab)) paste0("<div class='card'><h2>Extreme values within subgroups</h2>", if (!is.null(outlier_decision_tab)) "<h3>Status after recalculation</h3>" else "", df_html(subgroup_outlier_tab),
      "<p class='small'>Detection is repeated within discrete partitions because a value may be non-extreme overall but extreme within its subgroup. When a prior decision exists, this table shows the post-recalculation status; the original decision is retained for traceability.</p></div>") else "",
    outlier_subgroup_graph,
    if (!is.null(outlier_impact_tab)) paste0("<div class='card green'><h2>Impact of exclusion and recalculation</h2>", df_html(outlier_impact_tab), "</div>") else "",
    if (!is.null(outlier_decision_tab)) paste0("<div class='card'><h2>Specialist decisions on extreme values</h2>", df_html(outlier_decision_tab),
                                                "<p class='small'>The table retains the decision, rationale, and iteration in which it was made. Justified exclusions are also reflected in sample-size traceability.</p></div>") else "",
    "<div class='card'><h2>Primary result</h2><table>", paste(result_rows, collapse = ""), "</table><p>", esc(final_text), "</p></div>",
    if (!is.null(distribution_tab)) paste0("<div class='card'><h2>Assessment of the distribution</h2>", df_html(distribution_tab),
      "<p class='small'>Anderson-Darling assesses normality. Symmetry is assessed using a McWilliams-type runs test. Box-Cox is used to determine whether transformation permits a defensible parametric/robust model. With n≥120, these diagnostics do not replace the standard CLSI non-parametric pathway.</p></div>") else "",
    graph_html,
    if (!is.null(method_candidates_tab)) paste0("<div class='card'><h2>Method comparison and selection</h2>", df_html(method_candidates_tab),
      "<p><b>Method selected:</b> ", esc(main_result$method %||% "—"), "</p><p>", esc(main_result$method_selection_reason %||% ""), "</p></div>") else "",
    if (!is.null(direct_precision)) paste0("<div class='card'><h2>Precision of the limits</h2>", df_html(direct_precision),
                                            "<p class='small'>Predefined RIveR internal precision criterion: 90% CI width of each limit &lt;20% of the RI width. CI source: ", esc(main_result$ci_source %||% "—"), ". This is a precision criterion, not a criterion of clinical relevance.</p></div>") else "",
    if (!is.null(d7_summary)) paste0("<div class='card'><h2>D7 · Evidence for the indirect establishment</h2>", df_html(d7_summary),
                                  "<p class='small'>D7 separates estimation, robustness evidence, and specialist decision-making. Agreement between algorithms is not a vote, and no result is implemented automatically.</p></div>") else "",
    if (!is.null(methods)) paste0("<div class='card'><h2>Methods indirect globals</h2>", df_html(methods),
                                  if (is_d7) paste0("<p class='small'>", esc(if (identical(design$tail, "two_sided")) "refineR is the primary estimator and reflimR provides an independent estimate. Agreement provides robustness but is not a vote. RIbench is used as methodological benchmarking context and does not validate this specific RI." else "In one-sided mode, refineR estimates the active P5/P95 limit. reflimR 1.1.0 is shown only as descriptive P2.5/P97.5 support and does not confirm the same percentile. RIbench provides methodological context and does not validate this specific limit."), "</p></div>") else "<p class='small'>refineR is the primary method. kosmic is confirmatory when available; reflimR is used as support/screening. TMC and TML are not yet part of the validated engine in this version.</p></div>") else "",
    if (nzchar(d7_refine_graph)) paste0("<div class='card'><h2>D7 · Overall refineR graphical diagnostic</h2><div class='plotbox'>", d7_refine_graph, "</div><p class='small'>Visual inspection of fit is part of methodological review. If a covariate blocks the overall RI, this plot is retained for traceability rather than as a result to implement.</p></div>") else "",
    if (!is.null(d7_partition_criteria)) paste0("<div class='card'><h2>D7 · Justification of the partition for qualitative variable</h2>", df_html(d7_partition_criteria),
                                  if (!is.null(d7_lahti)) paste0("<h3>Lahti · Modeled proportions</h3>", df_html(d7_lahti)) else "",
                                  if (!is.null(d7_lahti_distance)) paste0("<h3>Lahti · distances between limits</h3>", df_html(d7_lahti_distance)) else "",
                                  paste0("<p class='small'>", esc(if (identical(design$tail, "two_sided")) "Lahti is adapted to the indirect context using the refineR RI for each group, rather than applying the criterion directly to a contaminated routine-data mixture. For modeled tail proportions, RIveR interprets <0.9% or >4.1% as supporting partitioning, 1.8–3.2% as compatible with a common RI, and intermediate values as marginal; for the distance between limits, ≥0.75 of the smaller SD supports partitioning and <0.25 supports a common RI. Harris–Boyd and SDR are used as supporting evidence on central subsets compatible with refineR. No criterion replaces biological plausibility or specialist judgment." else "One-sided P5/P95 mode: Lahti proportion thresholds operationalized for a nominal 2.5% tail are not extrapolated to a 5% tail. The active-limit distance, Harris–Boyd, and SDR are shown only as supporting evidence; RIveR does not automatically resolve one-sided partitioning."), "</p></div>")) else "",
    if (!is.null(d7_partition)) paste0("<div class='card'><h2>", esc(if (identical(design$tail, "two_sided")) "D7 · Candidate partitioned RIs · main result by group" else paste0("D7 · ", reference_limit_name(design), " candidate for group")), "</h2>", df_html(d7_partition),
                                  if (!is.null(d7_partition_methods)) paste0("<h3>Complete D7 engine by group</h3>", df_html(d7_partition_methods)) else "",
                                  if (length(d7_partition_graphs)) paste(d7_partition_graphs, collapse="") else "",
                                  paste0("<p class='small'>", esc(if (identical(design$tail, "two_sided")) "Each group runs refineR, bootstrap, reflimR, agreement assessment, non-pathological-fraction estimation, and its own D7 decision. Group-specific candidate RIs require biological-plausibility review and specialist approval." else "Each group runs refineR and bootstrap for the active P5/P95 limit; reflimR provides descriptive P2.5/P97.5 support. One-sided partitioning is not resolved automatically and requires biological-plausibility review and specialist approval."), "</p></div>")) else "",
    if (!is.null(d7_age)) paste0("<div class='card'><h2>D7 · Dependence screening · ", esc(quantitative_covariate_label), "</h2>", df_html(d7_age),
                                  "<p class='small'>The screening detects the pattern; do not creates bands arbitrary.</p></div>") else "",
    if (!is.null(d7_age_model)) paste0("<div class='card'><h2>", esc(if (identical(design$tail, "two_sided")) paste0("D7 · Candidate continuous RI by ", quantitative_covariate_label) else paste0("D7 · Candidate continuous ", reference_limit_name(design), " by ", quantitative_covariate_label)), "</h2>",
                                  if (nzchar(d7_age_graph)) paste0("<div class='plotbox'>", d7_age_graph, "</div>") else "",
                                  "<h3>Representative points of the continuous curve</h3>", df_html(d7_age_model),
                                  if (!is.null(d7_age_application)) paste0("<h3>Values of the continuous model · table of application</h3>", df_html(d7_age_application),
                                    "<p class='small'>", esc(if (quantitative_age_like_report) "Each row is the value predicted by the curve at that age; it is NOT an age band. For intermediate ages, the continuous curve takes precedence." else "Each row is a point on the continuous curve; it does NOT define categories or bands. For intermediate values, the continuous curve takes precedence."), "</p>") else "",
                                  if (!is.null(d7_age_sil_tab)) paste0("<h3>Non-overlapping operational proposal for the LIS</h3><p>", esc(d7_age_sil$recommendation %||% ""), "</p>", df_html(d7_age_sil_tab),
                                    if (!is.null(d7_age_sil_tradeoff)) paste0("<h3>Table of trade-off of the discretization</h3>", df_html(d7_age_sil_tradeoff)) else "",
                                    "<p class='small'><b>Important:</b> these bands are an operational approximation of the continuous curve; they are not independently established RIs for each band. For indirect data, raw dataset coverage is not used as an approval criterion because routine data may include pathological results.</p>") else "",
                                  if (!is.null(d7_age_windows)) paste0("<h3>Local calculation windows — NOT application intervals</h3>",
                                    "<p class='small'>The windows intentionally overlap because they are used to estimate and locally validate the refineR/reflimR curve. They must not be used as age bands or clinical intervals.</p>", df_html(d7_age_windows)) else "",
                                  "<p class='small'>", esc(main_result$age$model$note %||% "Internal RIveR model under validation."), "</p></div>") else if (!is.null(d7_age_windows)) paste0(
                                  "<div class='card'><h2>D7 · Diagnostic of quantitative-model windows · ", esc(quantitative_covariate_label), "</h2>",
                                  "<p>", esc(main_result$age$model$message %||% "The continuous curve could not be completed."), "</p>",
                                  "<h3>Local calculation windows — NOT application intervals</h3>",
                                  "<p class='small'>The windows is overlap intentionally and only have function methodological. No have of use as a bands of application.</p>",
                                  df_html(d7_age_windows),
                                  "<p class='small'>This table is retained explicitly when continuous modeling does not reach candidate status, to identify whether the blocker originates from LRL, P50, URL, or from the geometric ordering LRL &lt; P50 &lt; URL.</p></div>") else "",
    if (!is.null(d7_mclust_components)) paste0("<div class='card'><h2>D7 · Exploration mclust · components</h2>", df_html(d7_mclust_components),
                                  "<p class='small'>Statistical components are used only to generate hypotheses about heterogeneity/mixture. RIveR does not label any component as a healthy or pathological population and does not automatically select a component for RI establishment.</p></div>") else "",
    if (!is.null(d7_environment)) paste0("<div class='card'><h2>D7 · Computational environment and reproducibility</h2>", df_html(d7_environment),
                                  "<p class='small'>Package versions, the number of bootstrap replicates, and the random seed are part of estimation traceability. For functional validation, 30 replicates may be used; for final analysis, RIveR recommends ≥200.</p></div>") else "",
    if (length(d7_references)) paste0("<div class='card'><h2>D7 methodological references</h2><ul>", paste0("<li>", esc(d7_references), "</li>", collapse=""), "</ul><p class='small'>The references support prudent use of routine data, sensitivity to sample size/pathological contamination, and detailed reporting of the process. RIveR robustness thresholds are internal caution rules, not universal standards.</p></div>") else "",
    if (!is.null(d6_methods)) paste0("<div class='card'><h2>D6 · Evidence of indirect verification</h2>", df_html(d6_methods),
                                  paste0("<p class='small'>", esc(if (identical(design$tail, "two_sided")) "Staged workflow: reflimR with equivalence limits (EL) for screening; refineR/VeRUS with uncertainty margins (UM90, reference n=120) when screening is not green/green. Green VeRUS = point estimates remain within the corresponding margins; yellow = only the margins overlap; red = no overlap. refineR bootstrap CIs and VeRUS UM90 are distinct concepts. EL/UM discordance is treated as inconclusive, not as a vote." else "One-sided P5/P95 verification: reflimR 1.1.0 is retained as descriptive P2.5/P97.5 support and does not provide an equivalent EL for the active percentile. The decision is based on refineR/VeRUS (UM90) for the selected P5 or P95. Bootstrap CIs and UM90 are distinct concepts."), "</p></div>")) else "",
    if (!is.null(d6_environment)) paste0("<div class='card'><h2>D6 · Computational environment and reproducibility</h2>", df_html(d6_environment),
                                  "<p class='small'>Package versions and the random seed are part of result traceability. The D6 engine in this version requires reflimR ≥1.1.0 and refineR ≥2.0.0.</p></div>") else "",
    if (!is.null(d6_mclust_components)) paste0("<div class='card'><h2>D6 · Exploration mclust · components</h2>", df_html(d6_mclust_components),
                                  "<p class='small'>Statistical components estimated by mclust. Proportions, means, and SDs describe mixture structure and serve only to generate hypotheses; RIveR does not label any component as a healthy or pathological population and does not automatically modify the candidate RI.</p></div>") else "",
    if (length(d6_references)) paste0("<div class='card'><h2>D6 methodological references</h2><ul>", paste0("<li>", esc(d6_references), "</li>", collapse=""), "</ul><p class='small'>These references correspond to recent literature used to define the indirect-verification workflow. RIveR keeps verification of the candidate RI separate from estimation of a potential new local RI.</p></div>") else "",
    if (!is.null(partition_result)) paste0("<div class='card'><h2>Qualitative variable · ", esc(qualitative_covariate_label), "</h2><p><b>",
                                            if (identical(partition_result$decision,"indeterminate")) "Assessment statistical: evidence discordant" else paste0("Assessment statistical: ", decision_label(partition_result$decision)),
                                            "</b></p><p>", esc(partition_result$recommendation %||% ""), "</p>",
                                            if (!is.null(hb_text)) paste0("<p><b>", esc(hb_text), "</b></p>") else "", df_html(ptab),
                                            if (!is.null(partition_pairwise_tab)) paste0("<h3>Comparisons between categories</h3>", df_html(partition_pairwise_tab), "<p class='small'>With more than two categories, the same partitioning criteria are applied exhaustively pairwise. RIveR does not merge categories automatically.</p>") else "",
                                            if (!is.null(shape_tab)) paste0("<h3>Diagnostic of shape/tails used in the guidance</h3>", df_html(shape_tab)) else "",
                                            if (!is.null(partition_result$discordance_guidance)) paste0("<p><b>Guidance RIveR versus the discordance:</b> ", esc(partition_result$discordance_guidance$text %||% ""), "</p>") else "",
                                            if (!is.null(partition_result$lahti)) paste0("<p class='small'>", esc(if (identical(design$tail, "two_sided")) paste0("Lahti assesses the proportion of each subgroup outside the active limits. The nominal tail proportion is ", formatC(100*(1-design$coverage)/2, format="fg", digits=4, decimal.mark=","), "%. A red signal indicates evidence supporting group separation, not a study error.") else "One-sided mode: proportions relative to P5/P95 are documented, but Lahti thresholds for a 2.5% tail are not used to decide partitioning automatically."), "</p>") else "", "</div>") else "",
    if (!is.null(pdecision_tab)) paste0("<div class='card ", if (report_is_final) "green" else "yellow", "'><h2>Specialist partition decision</h2>", df_html(pdecision_tab), "<p class='small'>The specialist decision is recorded separately from the automated recommendation and determines the reference interval displayed in the final report.</p></div>") else "",
    if (!is.null(sdecision_tab)) paste0("<div class='card ", if (report_is_final) "green" else "yellow", "'><h2>", if ((sres$decision %||% "") %in% c("investigate","review_population","increase_no_model","no_ri","other")) "Specialist resolution — RI not established" else "Specialist resolution with n&lt;120", "</h2>", df_html(sdecision_tab), "<p class='small'>The specialist decision is recorded separately from the statistical assessment. A study can be closed traceably without establishing an RI when the evidence does not support a defensible estimate.</p></div>") else "",
    if (!is.null(age_result)) paste0("<div class='card'><h2>Quantitative variable · ", esc(quantitative_covariate_label), "</h2><p><b>Decision: ", esc(decision_label(age_result$decision)), "</b></p><p>", esc(age_result$recommendation %||% ""), "</p>",
      if (!is.null(age_diag_tab)) paste0("<h3>Diagnostic of the model</h3>", df_html(age_diag_tab)) else "",
      if (!is.null(age_cut_validation_tab) && isTRUE(age_result$step_like)) paste0("<h3>Validation of the cut-point</h3>", df_html(age_cut_validation_tab),
        "<p class='small'>The statistical cut-point retains the full precision of the location procedure. The operational cut-point is shown only when a simpler boundary preserves exactly the same subject assignment.</p>") else "",
      if (!is.null(age_boundary_tab)) paste0("<h3>Boundary-observation protection</h3><p><b>Possible observation misclassified because of cut-point location.</b> Review the cut-point first before considering exclusion.</p>", df_html(age_boundary_tab)) else "",
      if (!is.null(age_curve_tab) && identical(age_result$decision %||% "", "continuous")) paste0("<h3>Examples of the continuous band</h3>", df_html(age_curve_tab)) else "",
      if (!is.null(age_sil_tab) && identical(age_result$decision %||% "", "continuous")) paste0(
        "<h3>LIS adaptation when a continuous RI is not supported</h3><p>", esc(age_result$sil_adaptation$recommendation %||% ""), "</p>",
        df_html(age_sil_tab),
        if (!is.null(age_sil_tradeoff_tab)) paste0("<h3>Table of trade-off</h3>", df_html(age_sil_tradeoff_tab)) else "",
        "<p class='small'>This discretization is an operational approximation of the continuous model. The limits are derived from the overall GAMLSS model and are not independently re-established RIs for each band. Exact binomial 95% CIs are shown as individual diagnostics; operational approval requires geometric error ≤10% and overall coverage compatible with expected coverage using two-sided exact binomial tests with Holm adjustment. A borderline status indicates that the actual value slightly exceeds 10% even if rounding to one decimal would display 10.0%. Lahti/Harris-Boyd remain reserved for true biological partitioning.</p>"
      ) else "",
      if (!is.null(atab)) paste0("<h3>RI for segments of ", esc(quantitative_covariate_label), "</h3>", df_html(atab)) else "", "</div>", age_graph) else "",
    recommended_html,
    "<div class='card ", esc(final_status), "'><h2>", esc(final_head), "</h2><p>", esc(final_text), "</p>",
    if (length(final$notes %||% character(0))) paste0("<ul>", paste0("<li>", esc(final$notes), "</li>", collapse = ""), "</ul>") else "", "</div>",
    "<p class='small'>RIveR v", app_version, " · ", report_state_label, ". Specialist decisions and their justifications are part of study traceability.</p>",
    "</body></html>"
  )
  html <- gsub("RIveR", "RIveR", html, fixed = TRUE)
  writeLines(html, file, useBytes = TRUE)
}

# Report PDF visual ---------------------------------------------------------
# Generated using only grDevices + grid (included with R), without depending on
# No LaTeX, Chromium, or external packages. The objective is an archivable report
# and legible; the HTML retains the complete tabular detail.

pdf_safe_text <- function(x) {
  z <- as.character(x %||% "")
  z <- gsub("RIveR", "RIveR", z, fixed = TRUE)
  z <- gsub("≥", ">=", z, fixed = TRUE)
  z <- gsub("≤", "<=", z, fixed = TRUE)
  z <- gsub("–", "-", z, fixed = TRUE)
  z <- gsub("—", "-", z, fixed = TRUE)
  z <- gsub("×", "x", z, fixed = TRUE)
  z <- gsub("≈", "~", z, fixed = TRUE)
  z <- gsub("·", " | ", z, fixed = TRUE)
  z <- gsub("🟢", "", z, fixed = TRUE)
  z <- gsub("🟡", "", z, fixed = TRUE)
  z <- gsub("🔴", "", z, fixed = TRUE)
  z <- gsub("⚪", "", z, fixed = TRUE)
  trimws(iconv(z, from = "UTF-8", to = "latin1", sub = ""))
}

pdf_wrap <- function(x, width = 95) paste(strwrap(pdf_safe_text(x), width = width), collapse = "\n")

pdf_status_colour <- function(status) {
  switch(status, green = "#2f855a", yellow = "#b7791f", red = "#c53030", grey = "#718096", "#718096")
}

pdf_draw_status <- function(status, label, x, y, cex = 0.9) {
  grid::grid.circle(x = grid::unit(x, "npc"), y = grid::unit(y, "npc"),
                    r = grid::unit(0.009, "npc"), gp = grid::gpar(fill = pdf_status_colour(status), col = NA))
  grid::grid.text(pdf_safe_text(label), x = grid::unit(x + 0.018, "npc"), y = grid::unit(y, "npc"),
                  just = "left", gp = grid::gpar(fontsize = 10 * cex, fontface = "bold", col = "#263238"))
}

pdf_card <- function(title, text, y_top, height, status = NULL, title_size = 12, text_size = 9.5) {
  x <- 0.055; w <- 0.89
  border <- if (is.null(status)) "#d9dee5" else pdf_status_colour(status)
  grid::grid.roundrect(x = grid::unit(x, "npc"), y = grid::unit(y_top - height, "npc"),
                       width = grid::unit(w, "npc"), height = grid::unit(height, "npc"),
                       just = c("left", "bottom"), r = grid::unit(0.012, "npc"),
                       gp = grid::gpar(fill = "white", col = "#d9dee5", lwd = 1))
  if (!is.null(status)) {
    grid::grid.rect(x = grid::unit(x, "npc"), y = grid::unit(y_top - height, "npc"),
                    width = grid::unit(0.008, "npc"), height = grid::unit(height, "npc"),
                    just = c("left", "bottom"), gp = grid::gpar(fill = border, col = NA))
  }
  grid::grid.text(title, x = grid::unit(x + 0.025, "npc"), y = grid::unit(y_top - 0.027, "npc"),
                  just = c("left", "top"), gp = grid::gpar(fontsize = title_size, fontface = "bold", col = "#1f2933"))
  grid::grid.text(pdf_wrap(text, 105), x = grid::unit(x + 0.025, "npc"), y = grid::unit(y_top - 0.065, "npc"),
                  just = c("left", "top"), gp = grid::gpar(fontsize = text_size, col = "#39434d", lineheight = 1.15))
}

pdf_simple_table <- function(df, y_top, height = 0.20, col_widths = NULL, font_size = 7.8) {
  if (is.null(df) || !nrow(df)) return(invisible(NULL))
  x0 <- 0.055; w <- 0.89
  nr <- nrow(df) + 1; nc <- ncol(df)
  if (is.null(col_widths)) col_widths <- rep(1/nc, nc)
  col_widths <- col_widths / sum(col_widths)
  row_h <- height / nr
  grid::grid.rect(x = grid::unit(x0, "npc"), y = grid::unit(y_top - height, "npc"),
                  width = grid::unit(w, "npc"), height = grid::unit(height, "npc"),
                  just = c("left", "bottom"), gp = grid::gpar(fill = "white", col = "#d9dee5"))
  xs <- x0 + w * c(0, cumsum(col_widths))
  for (j in seq_len(nc)) {
    grid::grid.rect(x = grid::unit(xs[j], "npc"), y = grid::unit(y_top - row_h, "npc"),
                    width = grid::unit(w * col_widths[j], "npc"), height = grid::unit(row_h, "npc"),
                    just = c("left", "bottom"), gp = grid::gpar(fill = "#f3f6f8", col = "#d9dee5"))
    grid::grid.text(pdf_wrap(names(df)[j], max(8, floor(20 * col_widths[j] * nc))),
                    x = grid::unit(xs[j] + 0.006, "npc"), y = grid::unit(y_top - row_h/2, "npc"),
                    just = "left", gp = grid::gpar(fontsize = font_size, fontface = "bold", col = "#263238"))
  }
  for (i in seq_len(nrow(df))) {
    yy <- y_top - row_h * (i + 1)
    for (j in seq_len(nc)) {
      grid::grid.rect(x = grid::unit(xs[j], "npc"), y = grid::unit(yy, "npc"),
                      width = grid::unit(w * col_widths[j], "npc"), height = grid::unit(row_h, "npc"),
                      just = c("left", "bottom"), gp = grid::gpar(fill = "white", col = "#e4e8ec"))
      grid::grid.text(pdf_wrap(as.character(df[i, j]), max(7, floor(18 * col_widths[j] * nc))),
                      x = grid::unit(xs[j] + 0.006, "npc"), y = grid::unit(yy + row_h/2, "npc"),
                      just = "left", gp = grid::gpar(fontsize = font_size, col = "#374151"))
    }
  }
}

write_visual_pdf_report <- function(file, state, main_result, partition_result = NULL, age_result = NULL, final = NULL) {
  grDevices::pdf(file, width = 8.27, height = 11.69, paper = "special", onefile = TRUE, useDingbats = FALSE)
  on.exit(grDevices::dev.off(), add = TRUE)
  outlier_decisions <- state$outlier_decisions %||% NULL
  app_version <- state$app_version %||% "1.0.1"
  report_is_final <- isTRUE(final$is_final)
  report_state_label <- if (report_is_final) "FINAL REPORT" else "PROVISIONAL REPORT"
  pres <- state$partition_resolution %||% NULL
  sres <- state$small_sample_resolution %||% NULL
  meta <- state$metadata %||% list()
  design <- normalize_reference_design(main_result$reference_design %||% meta$reference_design)
  final_status <- final$status %||% main_result$status %||% "grey"
  final_action <- final$action %||% main_result$recommendation %||% ""
  digits <- main_result$display_digits %||% 2

  qualitative_label_pdf <- partition_result$covariate_label %||% main_result$qualitative_label %||% "Qualitative variable"
  quantitative_label_pdf <- age_result$covariate_label %||% main_result$quantitative_label %||% "Quantitative variable"
  quantitative_age_like_pdf <- isTRUE(age_result$covariate_is_age) ||
    (exists("quantitative_is_age_like", mode="function") && quantitative_is_age_like(quantitative_label_pdf))
  quantitative_suffix_pdf <- if (quantitative_age_like_pdf) " years" else ""
  genericize_quantitative_pdf_table <- function(tab) {
    if (is.null(tab) || !is.data.frame(tab)) return(tab)
    if (!quantitative_age_like_pdf && exists("quantitative_relabel_text", mode="function")) {
      names(tab) <- vapply(names(tab), function(nm) quantitative_relabel_text(nm, quantitative_label_pdf), character(1))
      for (nm in names(tab)) if (is.character(tab[[nm]])) tab[[nm]] <- quantitative_relabel_text(tab[[nm]], quantitative_label_pdf)
    }
    tab
  }

  # Page 1: decision and primary result
  grid::grid.newpage()
  grid::grid.rect(gp = grid::gpar(fill = "#f6f8fa", col = NA))
  grid::grid.rect(x = 0, y = 0.90, width = 1, height = 0.10, just = c("left", "bottom"),
                  gp = grid::gpar(fill = "#175a8b", col = NA))
  grid::grid.text("RIveR", x = 0.055, y = 0.965, just = "left",
                  gp = grid::gpar(fontsize = 23, fontface = "bold", col = "white"))
  grid::grid.text(paste0("Report visual · RIveR v", app_version, " · ", report_state_label), x = 0.055, y = 0.925, just = "left",
                  gp = grid::gpar(fontsize = 9.5, col = "white"))
  id_txt <- paste0(
    "Study: ", meta$study_name %||% "—", " | Measurand: ", meta$analyte %||% "—", " | Unit: ", meta$unit %||% "—", "\n",
    "System: ", meta$system %||% "—", "\n",
    "Population: ", meta$population %||% "—", " | Design: ", reference_design_label(design)
  )
  pdf_card("Identification", id_txt, y_top = 0.875, height = 0.12)
  pdf_card(if (report_is_final && identical(final$final_decision %||% NULL, "RI NOT ESTABLISHED.")) "Final documented decision - RI not established" else if (report_is_final) "Final decision approved" else "Required specialist action", final_action, y_top = 0.735, height = 0.145, status = final_status, title_size = 13, text_size = 10.5)

  ri_txt <- if (!is.null(main_result$ri)) reference_result_text(main_result$ri, design, digits) else if (!is.null(main_result$target)) reference_result_text(main_result$target, design, digits) else "—"
  is_d7_pdf <- identical(main_result$module %||% "", "D7") && identical(main_result$type %||% "", "indirect_establishment")
  d7_pdf_sil <- if (is_d7_pdf && identical(main_result$age$model$decision %||% "", "continuous_candidate")) {
    qlab0 <- main_result$age$model$covariate_label %||% quantitative_label_pdf
    qage0 <- exists("quantitative_is_age_like", mode="function") && quantitative_is_age_like(qlab0)
    tryCatch(d7_continuous_operational_discretization(main_result$age$model, age_like=qage0, tolerance=0.10, max_groups=10L), error=function(e) NULL)
  } else NULL
  if (is_d7_pdf && identical(main_result$partition$decision %||% "", "partition")) {
    subs <- main_result$partition$substudies %||% list(); gr <- main_result$partition$groups %||% names(subs)
    parts <- vapply(seq_along(subs), function(ii) {
      z <- subs[[ii]]
      paste0(gr[ii] %||% paste0("Group ", ii), ": ", if (!is.null(z$ri)) reference_result_text(z$ri, z$reference_design %||% design, digits) else "—", " · ", z$decision %||% "—")
    }, character(1))
    principal_txt <- paste0("overall refineR RI (DO NOT ADOPT): ", ri_txt, "\n", paste(parts, collapse="\n"),
                            "\nDecision: ", main_result$decision %||% "CANDIDATE PARTITIONED RIs",
                            "\nPending biological-plausibility review and specialist approval.")
  } else if (is_d7_pdf && identical(main_result$age$model$decision %||% "", "continuous_candidate")) {
    qmodel <- main_result$age$model
    qlab <- qmodel$covariate_label %||% quantitative_label_pdf
    q_age_like <- (exists("quantitative_is_age_like", mode="function") && quantitative_is_age_like(qlab))
    qsuffix <- if (q_age_like) " years" else ""
    ar <- qmodel$supported_quantitative %||% qmodel$supported_age %||% c(NA_real_, NA_real_)
    principal_txt <- paste0("overall refineR RI (DO NOT ADOPT): ", ri_txt, "\n",
                            main_result$decision %||% if (identical(design$tail, "two_sided")) paste0("CONTINUOUS RI BY ", toupper(qlab), " — CANDIDATE") else paste0(toupper(reference_limit_name(design)), " CONTINUOUS BY ", toupper(qlab), " — CANDIDATE"),
                            if (all(is.finite(ar))) paste0(" · modeled interval ", round(ar[1],1), "–", round(ar[2],1), qsuffix) else "",
                            if (identical(design$tail, "two_sided")) paste0("\nThe continuous curve is the main result. Details include an application table and", if (!is.null(d7_pdf_sil) && isTRUE(d7_pdf_sil$ok)) " an operational proposal of non-overlapping bands for the LIS." else " LRL/P50/URL values according to the quantitative covariate.") else paste0("\nSee ", reference_limit_name(design), " ", reference_design_percentile_text(design), " by ", qlab, " in the methodological/HTML detail."))
  } else if (is_d7_pdf && is.null(main_result$ri)) {
    principal_txt <- paste0("D7 decision: ", main_result$decision %||% "NOT EVALUABLE", "\n",
                            main_result$sample_context$text %||% main_result$recommendation %||% "—",
                            "\nn analyzed: ", fmt_integer(main_result$n %||% NA))
  } else if (identical(main_result$type %||% "", "direct_verification") && isTRUE(main_result$partitioned)) {
    parts <- vapply(main_result$partition_results %||% list(), function(z) {
      paste0(z$partition_label %||% z$partition_key %||% "Partition", ": ",
             reference_result_text(z$target, z$reference_design %||% design, digits), " · ",
             if (is.finite(z$outside_first20 %||% NA_real_)) paste0(z$outside_first20, "/20") else paste0("n=", z$first_n %||% 0L),
             if (is.finite(z$outside_second20 %||% NA_real_)) paste0(" + ", z$outside_second20, "/20") else "",
             " · ", z$decision %||% "—")
    }, character(1))
    principal_txt <- paste0("Direct verification of partitioned RIs
", paste(parts, collapse = "
"),
                            "
Overall decision: ", main_result$decision %||% "—",
                            "
n total analyzed: ", fmt_integer(main_result$n %||% NA))
  } else if (!is.null(pres) && identical(pres$decision,"partition") && !is.null(partition_result$group1) && !is.null(partition_result$group2)) {
    principal_txt <- paste0("Common assessed RI (not adopted): ", ri_txt, "\n",
      pretty_group_label(partition_result$groups[1]), ": ", reference_result_text(partition_result$group1$ri, partition_result$group1$reference_design %||% design, digits), "\n",
      pretty_group_label(partition_result$groups[2]), ": ", reference_result_text(partition_result$group2$ri, partition_result$group2$reference_design %||% design, digits),
      "\nMethod: ", main_result$method %||% "—", " · n total: ", fmt_integer(main_result$n %||% NA))
  } else {
    final_single <- (!is.null(pres) && identical(pres$decision,"common")) || (!is.null(sres) && identical(sres$decision,"adopt"))
    no_model <- grepl("exploratory", tolower(main_result$method %||% ""))
    global_not_adopted <- identical(partition_result$decision %||% "", "partition") || (age_result$decision %||% "") %in% c("partition","continuous")
    base_term <- if (identical(design$tail, "two_sided")) "RI" else reference_limit_name(design)
    principal_txt <- paste0(if (no_model) paste0(base_term, " exploratory non-parametric (not adopted): ") else if (final_single) paste0(base_term, " final adopted: ") else if (global_not_adopted) paste0(base_term, " overall assessed (not adopted): ") else paste0(base_term, " primary/candidate: "), ri_txt, "\nMethod: ", main_result$method %||% main_result$decision %||% "—", "\nn analyzed: ", fmt_integer(main_result$n %||% NA))
  }
  principal_tall <- (is_d7_pdf && identical(main_result$partition$decision %||% "", "partition")) ||
                    (identical(main_result$type %||% "", "direct_verification") && isTRUE(main_result$partitioned))
  pdf_card("Primary result", principal_txt, y_top = 0.565,
           height = if (principal_tall) 0.19 else if (is_d7_pdf) 0.15 else 0.125)

  if (identical(main_result$type, "direct_establishment")) {
    sides <- names(design$active)[design$active]
    prtab <- do.call(rbind, lapply(sides, function(side) {
      ii <- if (identical(side, "lower")) 1L else 2L
      ci <- if (identical(side, "lower")) main_result$ci90_lower else main_result$ci90_upper
      data.frame(
        Limit = reference_percentile_label(design$pair_percentiles[[side]]),
        Estimate = format_lab_number(main_result$ri[ii], digits),
        IC90 = format_lab_interval(ci, digits),
        Precision = format_percent(main_result$precision_ratio[ii], 1),
        Criterion = if (isTRUE(main_result$precision_ratio[ii] < .20)) "Meets" else "Does not meet",
        check.names = FALSE, stringsAsFactors = FALSE
      )
    }))
    grid::grid.text("Precision of the active limits", x = 0.055, y = 0.415, just = "left", gp = grid::gpar(fontsize = 12, fontface = "bold"))
    pdf_simple_table(prtab, y_top = 0.39, height = 0.13, col_widths = c(.12,.18,.30,.18,.22), font_size = 7.8)
    grid::grid.text("RIveR internal criterion: 90% CI width of each active limit <20% of the computational RI width.",
                    x = 0.055, y = 0.245, just = "left", gp = grid::gpar(fontsize = 7.7, col = "#66717d"))
  }

  if (!is.null(partition_result) && !is.null(partition_result$harris_boyd)) {
    hb <- partition_result$harris_boyd
    hb_status <- if (isTRUE(hb$supports_partition)) "red" else "green"
    grid::grid.text("Partition - summary", x = 0.055, y = 0.205, just = "left", gp = grid::gpar(fontsize = 12, fontface = "bold"))
    pdf_draw_status(hb_status, paste0("Harris-Boyd: ", if (hb_status == "red") "supports partitioning" else "does not support partitioning",
                                     " | Z=", formatC(hb$z, digits=2, format="f"),
                                     " Z*=", formatC(hb$z_critical, digits=2, format="f"),
                                     " SD ratio=", formatC(hb$sd_ratio, digits=2, format="f")),
                    x = 0.07, y = 0.17)
    lahti_st <- worst_status(partition_result$lahti$status %||% character(0))
    lahti_pdf_st <- if (lahti_st == "red") "red" else if (lahti_st == "green") "green" else "yellow"
    pdf_draw_status(lahti_pdf_st, paste0("Lahti: ", if (lahti_pdf_st == "red") "supports partitioning" else if (lahti_pdf_st == "green") "does not support partitioning" else "inconclusive"), x = 0.07, y = 0.13)
    pdf_draw_status(partition_result$status %||% "grey", paste0("Integrated decision: ", switch(partition_result$decision %||% "", partition="partition recommended", common="common RI", indeterminate="inconclusive", "not evaluable")), x = 0.07, y = 0.09)
  }

  grid::grid.text(paste0("RIveR v", app_version, " · ", report_state_label, "."),
                  x = 0.055, y = 0.028, just = "left", gp = grid::gpar(fontsize = 7.5, col = "#707b86"))

  # Page 2: traceability, extreme values, and partition details
  grid::grid.newpage()
  grid::grid.rect(gp = grid::gpar(fill = "#f6f8fa", col = NA))
  grid::grid.text("RIveR · Methodological detail", x = 0.055, y = 0.965, just = "left", gp = grid::gpar(fontsize = 18, fontface = "bold", col = "#175a8b"))

  audit <- state$audit %||% NULL
  if (!is.null(audit) && nrow(audit)) {
    ad <- audit
    names(ad) <- c("Stage", "n")
    ad$n <- vapply(ad$n, fmt_integer, character(1))
    grid::grid.text("Data traceability", x = 0.055, y = 0.92, just = "left", gp = grid::gpar(fontsize = 12, fontface = "bold"))
    pdf_simple_table(ad, y_top = 0.895, height = min(0.24, 0.035 * (nrow(ad)+1)), col_widths = c(.78,.22), font_size = 8)
  }

  if (is_d7_pdf && identical(main_result$partition$decision %||% "", "partition") && !is.null(main_result$partition$table)) {
    grid::grid.text("D7 · Candidate RIs by group", x = 0.055, y = 0.62, just = "left",
                    gp = grid::gpar(fontsize = 12, fontface = "bold"))
    pt <- main_result$partition$table
    keep <- intersect(c("Group","n","RI refineR","RI reflimR","Agreement","Non-pathological fraction","D7 group decision"), names(pt))
    pt <- pt[, keep, drop = FALSE]
    pdf_simple_table(pt, y_top = 0.595, height = min(0.22, 0.065 * (nrow(pt)+1)),
                     col_widths = rep(1/ncol(pt), ncol(pt)), font_size = 6.6)
    grid::grid.text(pdf_wrap("Lahti + Harris-Boyd + SDR provide supporting evidence; biological plausibility and specialist approval remain mandatory.", 120),
                    x = 0.055, y = 0.34, just = c("left","top"), gp = grid::gpar(fontsize = 7.7, col = "#66717d"))
  } else if (is_d7_pdf && identical(main_result$age$model$decision %||% "", "continuous_candidate") && !is.null(main_result$age$model$table)) {
    qlab <- main_result$age$model$covariate_label %||% quantitative_label_pdf
    if (!is.null(d7_pdf_sil) && isTRUE(d7_pdf_sil$ok)) {
      grid::grid.text(paste0("D7 · Non-overlapping operational proposal for the LIS · ", qlab), x = 0.055, y = 0.62, just = "left",
                      gp = grid::gpar(fontsize = 12, fontface = "bold"))
      at <- d7_continuous_operational_display_table(d7_pdf_sil)
      keep <- intersect(c("Interval of application","LRL","URL","Error maximum vs model"), names(at))
      at <- at[, keep, drop=FALSE]
      pdf_simple_table(at, y_top = 0.595, height = min(0.32, 0.038*(nrow(at)+1)), col_widths = c(.42,.16,.16,.26)[seq_len(ncol(at))], font_size = 6.7)
      grid::grid.text(pdf_wrap(paste0(d7_pdf_sil$recommendation %||% "", " The windows locals refineR/reflimR is overlap intentionally and NO are intervals of application."), 122),
                      x = 0.055, y = 0.22, just = c("left","top"), gp = grid::gpar(fontsize = 7.1, col = "#66717d"))
    } else {
      grid::grid.text(paste0("D7 · Candidate continuous RI by ", qlab), x = 0.055, y = 0.62, just = "left",
                      gp = grid::gpar(fontsize = 12, fontface = "bold"))
      at <- main_result$age$model$table
      xcols <- setdiff(names(at), c("LRL","P50","URL"))
      numcols <- intersect(c(if (length(xcols)) xcols[1] else character(0),"LRL","P50","URL"), names(at))
      for (nm in numcols) at[[nm]] <- signif(at[[nm]], 5)
      pdf_simple_table(at, y_top = 0.595, height = 0.30, col_widths = rep(1/ncol(at), ncol(at)), font_size = 7.0)
      grid::grid.text(pdf_wrap("The continuous curve is the main result. Local refineR/reflimR windows intentionally overlap and are NOT application intervals; see the HTML report for the parameterized table.", 120),
                      x = 0.055, y = 0.25, just = c("left","top"), gp = grid::gpar(fontsize = 7.4, col = "#66717d"))
    }
  } else if (is_d7_pdf && is.null(main_result$ri)) {
    grid::grid.text("D7 · Study not evaluable", x = 0.055, y = 0.62, just = "left",
                    gp = grid::gpar(fontsize = 12, fontface = "bold"))
    grid::grid.text(pdf_wrap(main_result$sample_context$text %||% main_result$recommendation %||% "—", 120),
                    x = 0.055, y = 0.585, just = c("left","top"), gp = grid::gpar(fontsize = 8.5, col = "#39434d"))
  }

  if (identical(main_result$type %||% "", "direct_verification") && isTRUE(main_result$partitioned)) {
    vt <- tryCatch(direct_partitioned_verification_display_table(main_result), error = function(e) NULL)
    if (!is.null(vt) && nrow(vt)) {
      grid::grid.text("Verification details by partition", x = 0.055, y = 0.62, just = "left",
                      gp = grid::gpar(fontsize = 12, fontface = "bold"))
      keep <- intersect(c("Partition","candidate RI","First cohort","Second cohort","Decision"), names(vt))
      vtc <- vt[, keep, drop = FALSE]
      pdf_simple_table(vtc, y_top = 0.595, height = min(0.22, 0.055 * (nrow(vtc)+1)),
                       col_widths = c(.18,.20,.20,.20,.22)[seq_len(ncol(vtc))], font_size = 7.1)
    }
  }

  y_out <- 0.62
  ot <- tryCatch(direct_outlier_display_table(main_result), error = function(e) NULL)
  if (!is.null(ot)) {
    grid::grid.text("Extreme / aberrant values", x = 0.055, y = y_out, just = "left", gp = grid::gpar(fontsize = 12, fontface = "bold"))
    # Compact PDF table: HTML retains all details.
    otc <- ot[, c("Method", "Assessment", "Result", "Values"), drop = FALSE]
    pdf_simple_table(otc, y_top = y_out - 0.025, height = 0.19, col_widths = c(.17,.31,.30,.22), font_size = 7.2)
    grid::grid.text("Detection does not imply exclusion. Exclusion requires a documented specialist decision and complete recalculation.",
                    x = 0.055, y = y_out - 0.235, just = "left", gp = grid::gpar(fontsize = 7.7, col = "#66717d"))
  }

  if (!is.null(partition_result) && !is.null(partition_result$lahti)) {
    grid::grid.text("Partition details", x = 0.055, y = 0.33, just = "left", gp = grid::gpar(fontsize = 12, fontface = "bold"))
    pt <- direct_partition_display_table(partition_result)
    if (!is.null(pt)) {
      keep <- intersect(c("Group", "n", "Group RI", "Exploratory group RI", "Precision RI", "Precision of the group RI", "Below common LRL", "Criterion lower", "Above common URL", "Criterion upper"), names(pt))
      pt <- pt[, keep, drop = FALSE]
      # Keep only key columns for A4 readability.
      ri_col <- if ("Group RI" %in% names(pt)) "Group RI" else "Exploratory group RI"
      prec_col <- if ("Precision RI" %in% names(pt)) "Precision RI" else "Precision of the group RI"
      compact <- data.frame(
        Group = pt[["Group"]], n = pt[["n"]], IR = pt[[ri_col]], Precision = pt[[prec_col]],
        Lahti_inf = pt[["Criterion lower"]], Lahti_sup = pt[["Criterion upper"]],
        check.names = FALSE
      )
      names(compact)[5:6] <- c("Lahti lower", "Lahti upper")
      pdf_simple_table(compact, y_top = 0.305, height = 0.13, col_widths = c(.14,.10,.22,.18,.18,.18), font_size = 6.8)
    }
    grid::grid.text(pdf_wrap(partition_result$recommendation %||% "", 120), x = 0.055, y = 0.15, just = c("left","top"), gp = grid::gpar(fontsize = 8.2, col = "#39434d"))
  }

  grid::grid.text("Criteria documented in the HTML: Dixon/Reed, Tukey, CI precision, Lahti, and Harris-Boyd. Retain the prepared dataset and the report with the study record.",
                  x = 0.055, y = 0.045, just = "left", gp = grid::gpar(fontsize = 7.5, col = "#707b86"))

  if (!is.null(outlier_decisions) && is.data.frame(outlier_decisions) && nrow(outlier_decisions)) {
    grid::grid.newpage()
    grid::grid.rect(gp = grid::gpar(fill = "#f6f8fa", col = NA))
    grid::grid.text("RIveR · Decisions on extreme values", x = 0.055, y = 0.965, just = "left",
                    gp = grid::gpar(fontsize = 18, fontface = "bold", col = "#175a8b"))
    od <- data.frame(
      Data = outlier_decisions$timestamp,
      Iteration = outlier_decisions$iteration,
      `ID/row` = ifelse(nzchar(outlier_decisions$patient_id), paste0(outlier_decisions$patient_id, "/", outlier_decisions$row_id), outlier_decisions$row_id),
      Value = outlier_decisions$value,
      Scope = if ("scope" %in% names(outlier_decisions)) outlier_decisions$scope else "Complete population",
      Decision = outlier_decisions$decision,
      Reason = vapply(outlier_decisions$category %||% rep("", nrow(outlier_decisions)), function(x) switch(as.character(x), preanalytical="Confirmed preanalytical error", analytical="Analytical error / invalid result", selection="Selection-criteria issue", legitimate="Legitimate observation", other="Other", as.character(x)), character(1)),
      Justification = outlier_decisions$reason,
      check.names = FALSE, stringsAsFactors = FALSE
    )
    pdf_simple_table(od, y_top = 0.91, height = min(0.55, 0.07 * (nrow(od) + 1)),
                     col_widths = c(.12,.07,.11,.08,.16,.10,.12,.24), font_size = 6.8)
    grid::grid.text(pdf_wrap("Exclusions are applied only after a documented specialist decision. After each exclusion, RIveR recalculates the reference interval, CIs, precision, and partitions. If recalculation reveals another extreme value, a new decision is required.", 120),
                    x = 0.055, y = 0.27, just = c("left","top"), gp = grid::gpar(fontsize = 8.5, col = "#39434d"))
    grid::grid.text(paste0("RIveR v", app_version, " · Traceability of specialist decisions"), x = 0.055, y = 0.04, just = "left",
                    gp = grid::gpar(fontsize = 7.5, col = "#707b86"))
  }
  if (!is.null(pres) && (pres$decision %||% "") %in% c("partition","common")) {
    grid::grid.newpage()
    grid::grid.rect(gp = grid::gpar(fill = "#f6f8fa", col = NA))
    grid::grid.text("RIveR · Specialist resolution of partitioning", x=0.055, y=0.965, just="left",
                    gp=grid::gpar(fontsize=18, fontface="bold", col="#175a8b"))
    sys <- switch(pres$system_favored %||% "review", partition="RIveR favored the partition", common="RIveR favored a common RI", "RIveR did not prioritize either alternative")
    dec <- if (identical(pres$decision,"partition")) "ADOPT SEPARATE RIs" else "RETAIN COMMON RI"
    pdf_card("System proposal", paste(sys, pres$system_text %||% "", sep="\n"), y_top=0.90, height=0.18, status="yellow")
    cat_pdf <- switch(as.character(pres$category %||% ""), biological="Biological / physiological plausibility", clinical="Classification / clinical impact", distribution="Shape distribution / tails", literature="Literature / consensus", operational="Operational limitation / LIS", other="Other", pres$category %||% "—")
    pdf_card("Specialist decision", paste0(dec, "\nBasis: ", cat_pdf, "\nJustification: ", pres$reason %||% "—"),
             y_top=0.69, height=0.24, status=if (report_is_final) "green" else "yellow")
    group_results_pdf <- partition_result$group_results %||% list()
    if (length(group_results_pdf)) {
      d <- main_result$display_digits %||% 2
      group_names_pdf <- names(group_results_pdf)
      if (is.null(group_names_pdf) || any(!nzchar(group_names_pdf))) group_names_pdf <- paste0("Group ", seq_along(group_results_pdf))
      alternatives <- data.frame(
        Alternative = c("common RI", group_names_pdf),
        `Limit / RI` = c(reference_result_text(main_result$ri, design, d), vapply(group_results_pdf, function(z) reference_result_text(z$ri, z$reference_design %||% design, z$display_digits %||% d), character(1))),
        check.names=FALSE, stringsAsFactors=FALSE
      )
      grid::grid.text("Alternatives assessed", x=0.055, y=0.405, just="left", gp=grid::gpar(fontsize=12,fontface="bold"))
      pdf_simple_table(alternatives, y_top=0.38, height=min(0.24, 0.05*(nrow(alternatives)+1)), col_widths=c(.45,.55), font_size=7.4)
    } else if (!is.null(partition_result$group1) && !is.null(partition_result$group2) && !is.null(partition_result$groups)) {
      d <- main_result$display_digits %||% 2
      alternatives <- data.frame(
        Alternative=c("common RI", as.character(partition_result$groups[1]), as.character(partition_result$groups[2])),
        `Limit / RI`=c(reference_result_text(main_result$ri, design, d), reference_result_text(partition_result$group1$ri, partition_result$group1$reference_design %||% design, partition_result$group1$display_digits %||% d), reference_result_text(partition_result$group2$ri, partition_result$group2$reference_design %||% design, partition_result$group2$display_digits %||% d)),
        check.names=FALSE
      )
      grid::grid.text("Alternatives assessed", x=0.055, y=0.405, just="left", gp=grid::gpar(fontsize=12,fontface="bold"))
      pdf_simple_table(alternatives, y_top=0.38, height=0.16, col_widths=c(.45,.55), font_size=8)
    }
    grid::grid.text(paste0("RIveR v", app_version, " · ", report_state_label, " · Decision and justification archivable with the study record."),
                    x=0.055, y=0.04, just="left", gp=grid::gpar(fontsize=7.5, col="#707b86"))
  }
  if (!is.null(sres) && (sres$decision %||% "") %in% c("adopt","increase")) {
    grid::grid.newpage()
    grid::grid.rect(gp = grid::gpar(fill = "#f6f8fa", col = NA))
    grid::grid.text("RIveR · Specialist resolution with n<120", x=0.055, y=0.965, just="left",
                    gp=grid::gpar(fontsize=18, fontface="bold", col="#175a8b"))
    dec <- if (identical(sres$decision,"adopt")) "ADOPT PROPOSED RI" else "DO NOT ADOPT YET / INCREASE SAMPLE SIZE"
    cat_pdf <- switch(as.character(sres$category %||% ""), model="Assumptions of the model / distribution", precision="Precision of the limits", biological="Biological / clinical plausibility", literature="Literature / consensus", other="Other", sres$category %||% "—")
    precision_parts <- character(0)
    if (isTRUE(design$active[["lower"]])) precision_parts <- c(precision_parts, paste0(reference_percentile_label(design$pair_percentiles[["lower"]]), " = ", format_percent(sres$precision_lower %||% main_result$precision_ratio[1],1)))
    if (isTRUE(design$active[["upper"]])) precision_parts <- c(precision_parts, paste0(reference_percentile_label(design$pair_percentiles[["upper"]]), " = ", format_percent(sres$precision_upper %||% main_result$precision_ratio[2],1)))
    proposal <- paste0("Method selected: ", sres$method %||% main_result$method %||% "—", "\n",
                       "n = ", fmt_integer(sres$n %||% main_result$n %||% NA), " · Result = ", reference_result_text(sres$ri %||% main_result$ri, design, digits), "\n",
                       "Precision: ", paste(precision_parts, collapse=" · "))
    pdf_card("RIveR proposal", proposal, y_top=0.90, height=0.19, status="yellow")
    pdf_card("Specialist decision", paste0(dec, "\nBasis: ", cat_pdf, "\nJustification: ", sres$reason %||% "—"),
             y_top=0.68, height=0.25, status=if (report_is_final && identical(sres$decision,"adopt")) "green" else "yellow")
    diag <- data.frame(
      Indicator=c("Anderson-Darling original", "Symmetry", "Lambda Box-Cox", "Anderson-Darling after Box-Cox"),
      Result=c(format_p_value(sres$normality_p %||% main_result$normality_p), format_p_value(sres$symmetry_p %||% main_result$symmetry_p),
                  if (is.finite(sres$boxcox_lambda %||% main_result$boxcox_lambda)) formatC(sres$boxcox_lambda %||% main_result$boxcox_lambda,format="f",digits=3,decimal.mark=",") else "—",
                  format_p_value(sres$boxcox_normality_p %||% main_result$boxcox_normality_p)),
      check.names=FALSE, stringsAsFactors=FALSE)
    grid::grid.text("Diagnostic used in the selection", x=0.055, y=0.39, just="left", gp=grid::gpar(fontsize=12,fontface="bold"))
    pdf_simple_table(diag, y_top=0.365, height=0.18, col_widths=c(.62,.38), font_size=8)
    grid::grid.text("The specialist decision does not change the statistical calculations; it documents whether available evidence is sufficient to adopt the RI with n<120.",
                    x=0.055, y=0.12, just="left", gp=grid::gpar(fontsize=8,col="#66717d"))
    grid::grid.text(paste0("RIveR v", app_version, " · ", report_state_label, " · Decision archived with the study record."),
                    x=0.055, y=0.04, just="left", gp=grid::gpar(fontsize=7.5,col="#707b86"))
  }

  if (!is.null(age_result) && !is.null(age_result$bins)) {
    grid::grid.newpage()
    grid::grid.rect(gp = grid::gpar(fill = "#f6f8fa", col = NA))
    grid::grid.text(paste0("RIveR · Quantitative variable · ", quantitative_label_pdf), x=0.055, y=0.965, just="left",
                    gp=grid::gpar(fontsize=17, fontface="bold", col="#175a8b"))
    adec_txt <- switch(age_result$decision %||% "",
      common=if (identical(design$tail,"two_sided")) paste0("common RI relative to ", quantitative_label_pdf) else paste0(reference_limit_name(design), " common relative to ", quantitative_label_pdf),
      continuous=if (identical(design$tail,"two_sided")) paste0("Continuous RI by ", quantitative_label_pdf) else paste0("Continuous ", reference_limit_name(design), " by ", quantitative_label_pdf),
      partition=paste0("Partition for ", quantitative_label_pdf), indeterminate="Inconclusive", "Not evaluable")
    pdf_card("Conclusion", paste0(adec_txt, "\n", age_result$recommendation %||% ""),
             y_top=0.91, height=0.19, status=age_result$status %||% "grey", text_size=8.5)
    adt <- tryCatch(genericize_quantitative_pdf_table(age_diagnostics_display_table(age_result)), error=function(e) NULL)
    if (!is.null(adt)) {
      grid::grid.text("Diagnostic of the model", x=0.055, y=0.68, just="left", gp=grid::gpar(fontsize=11.5,fontface="bold"))
      # Compact version: the three complete columns can be extensive; the PDF shows
      # the principal indicators; the HTML retains the full detail.
      adtc <- adt[, c("Assessment","Result"), drop=FALSE]
      pdf_simple_table(adtc, y_top=0.655, height=0.30, col_widths=c(.68,.32), font_size=6.8)
    }
    abt <- tryCatch(genericize_quantitative_pdf_table(age_boundary_outlier_display_table(age_result)), error=function(e) NULL)
    apt <- tryCatch(genericize_quantitative_pdf_table(age_partition_display_table(age_result)), error=function(e) NULL)
    if (!is.null(abt)) {
      grid::grid.text("Protected boundary observations", x=0.055, y=0.31, just="left", gp=grid::gpar(fontsize=11.5,fontface="bold"))
      pdf_simple_table(abt, y_top=0.285, height=0.14, col_widths=c(.25,.12,.14,.16,.33), font_size=5.8)
    } else if (!is.null(apt)) {
      grid::grid.text(paste0("RI for segments of ", quantitative_label_pdf), x=0.055, y=0.31, just="left", gp=grid::gpar(fontsize=11.5,fontface="bold"))
      keep <- intersect(c("Group", paste0(quantitative_label_pdf," minimum"), paste0(quantitative_label_pdf," maximum"), "Age minimum","Age maximum","n","LRL","URL","Precision LRL","Precision URL"), names(apt))
      apt <- apt[,keep,drop=FALSE]
      pdf_simple_table(apt, y_top=0.285, height=0.14, col_widths=rep(1/ncol(apt),ncol(apt)), font_size=5.8)
    } else if (identical(age_result$decision %||% "", "continuous")) {
      act <- tryCatch(genericize_quantitative_pdf_table(age_curve_display_table(age_result)), error=function(e) NULL)
      if (!is.null(act)) {
        grid::grid.text("Examples of the continuous band", x=0.055, y=0.31, just="left", gp=grid::gpar(fontsize=11.5,fontface="bold"))
        pdf_simple_table(act, y_top=0.285, height=0.14, col_widths=c(.2,.27,.26,.27), font_size=7)
      }
    }
    age_graph_note <- if (identical(design$tail, "two_sided")) paste0("The plots of ", reference_percentile_label(design$pair_percentiles[["lower"]]), "/P50/", reference_percentile_label(design$pair_percentiles[["upper"]]), " and the residuals versus ", quantitative_label_pdf, "/fitted values are included in the application and HTML report.") else paste0("The plot of the ", reference_limit_name(design), " ", reference_design_percentile_text(design), " and the residuals versus ", quantitative_label_pdf, "/fitted values are included in the application and HTML report.")
    grid::grid.text(age_graph_note,
                    x=0.055, y=0.08, just="left", gp=grid::gpar(fontsize=7.7,col="#66717d"))
    grid::grid.text(paste0("RIveR v", app_version, " · ", report_state_label, "."),
                    x=0.055, y=0.028, just="left", gp=grid::gpar(fontsize=7.5,col="#707b86"))
  }


  if (!is.null(age_result) && isTRUE(age_result$step_like) && !is.null(age_result$cut_validation)) {
    acv <- tryCatch(genericize_quantitative_pdf_table(age_cut_validation_display_table(age_result)), error=function(e) NULL)
    if (!is.null(acv) && nrow(acv)) {
      grid::grid.newpage()
      grid::grid.rect(gp = grid::gpar(fill = "#f6f8fa", col = NA))
      grid::grid.text(paste0("RIveR · Validation of the cut-point · ", quantitative_label_pdf), x=0.055, y=0.965, just="left",
                      gp=grid::gpar(fontsize=17, fontface="bold", col="#175a8b"))
      pdf_card("Cut-point traceability",
               "rpart delineates the candidate zone; the robust search sets the statistical cut-point. An operational cut-point for the LIS is proposed only if a simpler boundary preserves exactly the same subjects.",
               y_top=0.91, height=0.17, status=age_result$status %||% "yellow", text_size=8.2)
      grid::grid.text("Validation criteria", x=0.055, y=0.69, just="left", gp=grid::gpar(fontsize=11.5,fontface="bold"))
      pdf_simple_table(acv, y_top=0.665, height=0.48, col_widths=c(.23,.25,.31,.21), font_size=5.6)
      grid::grid.text("Lahti and Harris-Boyd apply here because a true biological partition is being assessed; they are not used as mandatory gates for operational discretization of a continuous model.",
                      x=0.055, y=0.105, just="left", gp=grid::gpar(fontsize=7.4,col="#66717d"))
      grid::grid.text(paste0("RIveR v", app_version, " · ", report_state_label, "."),
                      x=0.055, y=0.035, just="left", gp=grid::gpar(fontsize=7.5,col="#707b86"))
    }
  }

  if (!is.null(age_result) && identical(age_result$decision %||% "", "continuous") &&
      !is.null(age_result$sil_adaptation$table) && nrow(age_result$sil_adaptation$table)) {
    grid::grid.newpage()
    grid::grid.rect(gp = grid::gpar(fill = "#f6f8fa", col = NA))
    grid::grid.text("RIveR · Adaptation of the continuous model in the LIS", x=0.055, y=0.965, just="left",
                    gp=grid::gpar(fontsize=17, fontface="bold", col="#175a8b"))
    sa <- age_result$sil_adaptation
    sil_model_term <- if (identical(design$tail, "two_sided")) "continuous RI" else paste0("continuous ", reference_limit_name(design), " (", reference_design_percentile_text(design), ")")
    pdf_card("Implementation principle",
             paste0("Preferred scientific model: ", sil_model_term, " dependent on ", quantitative_label_pdf, ".\n",
                    if (quantitative_age_like_pdf) "If the LIS supports an age table, use the annual export. If it supports only bands, this page shows the discrete approximation derived from the same model.\n" else paste0("If the LIS supports a parameterized table for ", quantitative_label_pdf, ", use it. If it supports only bands, this page shows the discrete approximation derived from the same model.\n"),
                    sa$recommendation %||% ""),
             y_top=0.91, height=0.23, status=sa$status %||% "yellow", text_size=8.1)
    sat <- tryCatch(genericize_quantitative_pdf_table(age_sil_display_table(age_result)), error=function(e) NULL)
    if (!is.null(sat)) {
      grid::grid.text("Proposed operational bands", x=0.055, y=0.63, just="left", gp=grid::gpar(fontsize=11.5,fontface="bold"))
      pdf_simple_table(sat, y_top=0.605, height=0.34, col_widths=rep(1/ncol(sat), ncol(sat)), font_size=4.8)
    }
    grid::grid.text("The discretization does not demonstrate biological subpopulations; it approximates a validated continuous band. Lahti/Harris-Boyd are not mandatory gates in this operational conversion.",
                    x=0.055, y=0.15, just="left", gp=grid::gpar(fontsize=7.6,col="#66717d"))
    grid::grid.text(paste0("RIveR v", app_version, " · ", report_state_label, "."),
                    x=0.055, y=0.035, just="left", gp=grid::gpar(fontsize=7.5,col="#707b86"))
  }

  if (!is.null(age_result) && identical(age_result$decision %||% "", "continuous") &&
      !is.null(age_result$sil_adaptation$tradeoff_table) && nrow(age_result$sil_adaptation$tradeoff_table)) {
    grid::grid.newpage()
    grid::grid.rect(gp = grid::gpar(fill = "#f6f8fa", col = NA))
    grid::grid.text("RIveR · LIS discretization trade-off", x=0.055, y=0.965, just="left",
                    gp=grid::gpar(fontsize=17, fontface="bold", col="#175a8b"))
    pdf_card("Internal criterion",
             paste0("Operational discretization approval simultaneously requires: actual maximum error of the active limit(s) ≤10% of the typical computational width and overall coverage compatible with the expected ", round(100 * (age_result$sil_adaptation$expected_coverage %||% design$coverage), 1), " % using exact two-sided binomial tests with Holm adjustment. The individual 95% CIs are shown as diagnostics. The preferred solution is the simplest one that strictly meets both criteria."),
             y_top=0.91, height=0.19, status=age_result$sil_adaptation$status %||% "yellow", text_size=8.1)
    stt <- tryCatch(genericize_quantitative_pdf_table(age_sil_tradeoff_display_table(age_result)), error=function(e) NULL)
    if (!is.null(stt)) {
      grid::grid.text("Solutions assessed", x=0.055, y=0.66, just="left", gp=grid::gpar(fontsize=11.5,fontface="bold"))
      pdf_simple_table(stt, y_top=0.635, height=0.42, col_widths=rep(1/ncol(stt), ncol(stt)), font_size=5.2)
    }
    grid::grid.text("Adding more bands is useful only if the improvement in geometry or coverage justifies the operational complexity. The table documents this trade-off.",
                    x=0.055, y=0.12, just="left", gp=grid::gpar(fontsize=7.7,col="#66717d"))
    grid::grid.text(paste0("RIveR v", app_version, " · ", report_state_label, "."),
                    x=0.055, y=0.035, just="left", gp=grid::gpar(fontsize=7.5,col="#707b86"))
  }

  if (identical(main_result$type, "direct_establishment")) {
    grid::grid.newpage()
    grid::grid.rect(gp = grid::gpar(fill = "#f6f8fa", col = NA))
    grid::grid.text("RIveR · Distribution and method selection", x = 0.055, y = 0.965, just = "left",
                    gp = grid::gpar(fontsize = 17, fontface = "bold", col = "#175a8b"))
    dt <- tryCatch(direct_distribution_display_table(main_result), error=function(e) NULL)
    mt <- tryCatch(direct_method_candidates_display_table(main_result), error=function(e) NULL)
    if (!is.null(dt)) {
      grid::grid.text("Assessment of the distribution", x=0.055, y=0.915, just="left", gp=grid::gpar(fontsize=12,fontface="bold"))
      pdf_simple_table(dt, y_top=0.89, height=0.25, col_widths=c(.34,.18,.48), font_size=7.1)
    }
    if (!is.null(mt)) {
      grid::grid.text("Candidate methods", x=0.055, y=0.60, just="left", gp=grid::gpar(fontsize=12,fontface="bold"))
      pdf_simple_table(mt, y_top=0.575, height=0.25, col_widths=c(.25,.18,.34,.23), font_size=6.7)
    }
    pdf_card("Method selected", paste0(main_result$method %||% "—", "\n", main_result$method_selection_reason %||% ""),
             y_top=0.285, height=0.16, status=if (main_result$n>=120) "green" else if (grepl("exploratory",tolower(main_result$method %||% ""))) "red" else "yellow", text_size=9)
    grid::grid.text("With n>=120, normality and Box-Cox are traceability diagnostics and do not replace the standard CLSI non-parametric method. Histograms and QQ plots are included in the application and HTML report.",
                    x=0.055, y=0.09, just="left", gp=grid::gpar(fontsize=7.7,col="#66717d"))
    grid::grid.text(paste0("RIveR v", app_version, " · ", report_state_label, "."),
                    x=0.055, y=0.028, just="left", gp=grid::gpar(fontsize=7.5,col="#707b86"))
  }
  # Graphical pages for extreme values. Base graphics on the PDF device
  # create a new page for each box plot and avoid external dependencies.
  if (identical(main_result$type, "direct_establishment")) {
    pd <- state$prepared_data %||% NULL
    bd <- state$outlier_baseline_data %||% NULL
    od <- state$outlier_decisions %||% data.frame()
    excluded_ids <- if (is.data.frame(od) && nrow(od) && all(c("row_id","decision") %in% names(od))) unique(od$row_id[od$decision == "Exclude"]) else integer(0)
    has_before <- length(excluded_ids) && is.data.frame(bd) && nrow(bd)
    yl <- paste((state$metadata %||% list())$analyte %||% "Result", (state$metadata %||% list())$unit %||% "")
    if (has_before) plot_outlier_boxplot(bd, main="Complete population · before review", ylab=yl, marked_row_ids=excluded_ids)
    if (is.data.frame(pd) && nrow(pd)) plot_outlier_boxplot(pd, main=if (has_before) "Complete population · after recalculation" else "Complete population", ylab=yl)
    qg_col <- if ("qualitative" %in% names(pd)) "qualitative" else if ("sex" %in% names(pd)) "sex" else NULL
    if (!is.null(partition_result) && !is.null(qg_col) && is.data.frame(pd)) {
      if (has_before && qg_col %in% names(bd)) plot_outlier_boxplot(bd, group=as.character(bd[[qg_col]]), main=paste0(qualitative_label_pdf," · before the review"), ylab=yl, marked_row_ids=excluded_ids)
      plot_outlier_boxplot(pd, group=as.character(pd[[qg_col]]), main=if (has_before) paste0(qualitative_label_pdf," · after recalculation") else qualitative_label_pdf, ylab=yl)
    }
    cut <- age_result$selected_cut %||% NA_real_
    qn_col <- if ("quantitative" %in% names(pd)) "quantitative" else if ("age" %in% names(pd)) "age" else NULL
    if (!is.null(age_result$cut_validation) && is.finite(cut) && !is.null(qn_col) && is.data.frame(pd)) {
      fc <- formatC(cut, format="fg", digits=6, decimal.mark=",")
      suffix <- quantitative_suffix_pdf
      if (has_before && qn_col %in% names(bd)) {
        bg <- ifelse(bd[[qn_col]] <= cut, paste0("≤ ",fc,suffix), paste0("> ",fc,suffix))
        plot_outlier_boxplot(bd, group=bg, main=paste0(quantitative_label_pdf," · before the review"), ylab=yl, marked_row_ids=excluded_ids)
      }
      pg <- ifelse(pd[[qn_col]] <= cut, paste0("≤ ",fc,suffix), paste0("> ",fc,suffix))
      plot_outlier_boxplot(pd, group=pg, main=if (has_before) paste0(quantitative_label_pdf," · after recalculation") else quantitative_label_pdf, ylab=yl)
    }
  }
  invisible(file)
}
