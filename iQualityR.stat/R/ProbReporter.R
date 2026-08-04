# =============================================================================
# File: R/ProbReporter.R
# Description: Probability distribution report engine (optimized + Excel export)
# =============================================================================

#' @title ProbReporter: Probability distribution report engine
#' @description
#' Responsible for outputting calculation results in table and text format, supports:
#' - Console printing
#' - Data frame export
#' - Excel report export
#'
#' @export
ProbReporter <- R6::R6Class("ProbReporter",
  inherit = StatReporter,
  public = list(
    #' @description Unified report entry point (Contract 2 signature).
    #'
    #' Dispatches on `format`:
    #' - `"console"`: prints a human-readable summary to stdout.
    #' - `"data.frame"`: returns a tidy data frame.
    #' - `"excel"`: writes a themed xlsx file via `ExcelExporter`.
    #'
    #' `result` may be either a list bundling `calc_results` and `nodes` (the
    #' natural L3 packaging), or the `calc_results` list directly with `nodes`
    #' supplied through `...` (needed only for Excel export).
    #'
    #' @param result A list with `$calc_results` and optionally `$nodes`, or the
    #'   `calc_results` list directly.
    #' @param format Output format: `"data.frame"` (default), `"console"`, or `"excel"`.
    #' @param path File path for `format = "excel"`.
    #' @param audience Audience level (reserved for future interpretation sheet).
    #' @param ... Backward-compat channel: `nodes` is extracted from here when
    #'   not bundled in `result` (required for Excel export).
    #' @return For `"data.frame"`: a data frame. For `"console"`/`"excel"`:
    #'   invisible(NULL) / invisible(path).
    report = function(result, format = c("data.frame", "console", "excel"),
                      path = NULL, audience = "manager", ...) {
      format <- match.arg(format)
      dots <- list(...)

      # Resolve result into calc_results + nodes. A bundled list carries both;
      # otherwise result is the calc_results and nodes come from ... .
      if (is.list(result) && !is.null(result$calc_results)) {
        calc_results <- result$calc_results
        nodes <- result$nodes %||% dots$nodes
      } else {
        calc_results <- result
        nodes <- dots$nodes
      }

      switch(format,
        "console"    = self$print_console(calc_results),
        "data.frame" = self$to_dataframe(calc_results),
        "excel"      = {
          if (is.null(nodes)) {
            stop("[ProbReporter] nodes are required for Excel export (pass via result$nodes or ...).",
                 call. = FALSE)
          }
          if (!requireNamespace("iQualityR.core", quietly = TRUE)) {
            stop("[ProbReporter] iQualityR.core is required for Excel export.",
                 call. = FALSE)
          }
          config <- IqrTheme$new("academic")$config
          exporter <- iQualityR.core::ExcelExporter$new(config)
          path <- path %||% paste0("probability_analysis_report_",
                                   format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx")
          self$export_excel(calc_results, nodes, path, exporter)
          invisible(path)
        },
        stop("Unknown format: ", format)
      )
    },

    #' @description Print report to console
    #' @param calc_results Calculation result list
    print_console = function(calc_results) {
      cat("\n[iQualityR Probability Analysis Report]\n")
      cat(rep("=", 45), "\n")

      for (id in names(calc_results)) {
        res_bundle <- calc_results[[id]]
        
        # Check structure integrity
        if (is.null(res_bundle$all_res) || !is.list(res_bundle$all_res)) {
          warning(sprintf("[ProbReporter] Node %s result structure abnormal, skipping", id))
          next
        }
        
        cat(sprintf("Node ID: %s | Mode: %s\n", id, res_bundle$mode))

        rows <- lapply(res_bundle$all_res, function(item) {
          x_vals <- if (!is.null(unlist(item$target_x))) unlist(item$target_x) else 0
          x_str  <- paste(round(as.numeric(x_vals), 4), collapse = ", ")
          p_res  <- if (!is.null(item$result_p)) sprintf("%.6f", item$result_p) else "NA"

          if (item$is_prob_mode) {
            data.frame("Input_X" = x_str, "Result_P" = p_res, stringsAsFactors = FALSE)
          } else {
            x_res <- if (!is.null(item$result_x)) round(item$result_x, 4) else NA
            data.frame("Input_P" = item$target_p, "Result_X" = x_res, stringsAsFactors = FALSE)
          }
        })

        print(do.call(rbind, rows), row.names = FALSE)
        cat(sprintf("Text description: %s\n\n", res_bundle$pdf_lbl))
      }
    },

    #' @description Export as data frame
    #' @param calc_results Calculation result list
    #' @return Data frame
    to_dataframe = function(calc_results) {
      if (is.null(calc_results)) return(NULL)

      # Ensure calc_results is a named list
      if (!is.list(calc_results)) {
        stop("[ProbReporter] calc_results must be a list structure", call. = FALSE)
      }

      result_list <- lapply(names(calc_results), function(id) {
        res_bundle <- calc_results[[id]]
        
        # Check res_bundle structure
        if (is.null(res_bundle$all_res)) {
          warning(sprintf("[ProbReporter] Node %s missing all_res field, skipping", id))
          return(NULL)
        }
        
        # Ensure all_res is a list
        if (!is.list(res_bundle$all_res)) {
          warning(sprintf("[ProbReporter] Node %s all_res is not a list, skipping", id))
          return(NULL)
        }
        
        do.call(rbind, lapply(res_bundle$all_res, function(item) {
          data.frame(
            node_id    = id,
            mode       = res_bundle$mode,
            input_val  = if (item$is_prob_mode) paste(item$target_x, collapse = ",") else item$target_p,
            result_val = if (item$is_prob_mode) item$result_p else item$result_x,
            formula    = res_bundle$pdf_lbl,
            stringsAsFactors = FALSE
          )
        }))
      })
      
      # Remove NULL elements
      result_list <- Filter(Negate(is.null), result_list)
      
      if (length(result_list) == 0) return(NULL)
      
      do.call(rbind, result_list)
    },

    #' @description Export to Excel report
    #' @param calc_results Calculation result list
    #' @param nodes Distribution node list
    #' @param path Output path
    #' @param excel_exporter ExcelExporter instance
    export_excel = function(calc_results, nodes, path, excel_exporter) {
      sheets <- list()

      # 1. Calculation result table
      sheets[["Calculation Results"]] <- self$to_dataframe(calc_results)

      # 2. Distribution parameters table
      sheets[["Distribution Parameters"]] <- do.call(rbind, lapply(names(nodes), function(id) {
        node <- nodes[[id]]
        info <- node$get_node_info()
        data.frame(
          Node_ID = id,
          Distribution_Type = info$type,
          Description = info$description,
          Support = info$support,
          Is_Discrete = ifelse(info$is_discrete, "Yes", "No"),
          Parameters = paste(names(info$params), "=", unlist(info$params), collapse = "; "),
          stringsAsFactors = FALSE
        )
      }))

      # 3. Usage instructions
      sheets[["Usage Instructions"]] <- data.frame(
        Item = c("mode", "calc_type", "Output meaning"),
        Description = c(
          "prob = probability calculation, quant = quantile calculation",
          "lower = left tail, upper = right tail, between = within interval, outside = outside interval",
          "prob mode returns probability P, quant mode returns quantile X"
        ),
        stringsAsFactors = FALSE
      )

      # Export Excel
      if (!is.null(excel_exporter)) {
        excel_exporter$export_excel(data = sheets, path = path)
      } else {
        stop("[ProbReporter] ExcelExporter instance not provided")
      }
    }
  )
)
