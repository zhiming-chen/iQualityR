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
  public = list(
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
