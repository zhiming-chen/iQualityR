# =============================================================================
# File: R/DoeReporter.R
# Description: DOE report generator
# =============================================================================

#' DOE Report Generator
#'
#' @title DoeReporter: DOE Report Generator
#' @description Generates DOE (Design of Experiments) reports in Excel and HTML
#'   formats. Acts as the unified entry point for exporting analysis results to
#'   either an Excel workbook or an HTML report rendered from an R Markdown
#'   template.
#'
#' @importFrom R6 R6Class
#'
#' @export
DoeReporter <- R6::R6Class("DoeReporter",
  public = list(

    #' @description Unified export entry point. Dispatches to the appropriate
    #'   format-specific exporter based on the `format` argument. If no `path`
    #'   is provided, a default file name is generated using a timestamp.
    #' @param results A list of analysis results, typically containing
    #'   `design_info` and `anova_results` data frames.
    #' @param plan An `IqrDoePlan` object describing the DOE design (factors,
    #'   design type, replication, center points, etc.).
    #' @param format Character scalar specifying the output format. Supported
    #'   values are `"excel"` and `"html"`. Defaults to `"excel"`.
    #' @param path Character scalar specifying the output file path. If `NULL`,
    #'   a default path of the form `doe_report_<timestamp>.<format>` is used.
    #' @param theme_obj An optional `IqrTheme` object used to style the HTML
    #'   report. Ignored for Excel output.
    #' @param ... Additional arguments passed to the underlying format-specific
    #'   exporter (`export_excel` or `export_html`).
    #' @return Character scalar. Invisibly returns the path to the generated
    #'   report file.
    output = function(results, plan, format = "excel", path = NULL, theme_obj = NULL, ...) {
      if (is.null(path)) {
        timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
        path <- paste0("doe_report_", timestamp, ".", format)
      }

      switch(format,
        "excel" = self$export_excel(results, plan, path, ...),
        "html" = self$export_html(results, plan, path, theme_obj, ...),
        "word" = self$export_word(results, plan, path, ...),
        "powerpoint" = self$export_powerpoint(results, plan, path, ...),
        stop("Unsupported format: ", format, call. = FALSE)
      )

      message("[iQualityR] DOE report exported to: ", path)
      invisible(path)
    },

    #' @description Export an Excel report. Builds a workbook containing
    #'   sheets for the design summary, the design table, and the ANOVA
    #'   results (when available), then saves it to `path`. Uses the
    #'   `openxlsx` package.
    #' @param results A list of analysis results, typically containing
    #'   `design_info` and `anova_results` data frames.
    #' @param plan An `IqrDoePlan` object describing the DOE design.
    #' @param path Character scalar specifying the output `.xlsx` file path.
    #' @param ... Additional arguments (currently ignored).
    #' @return Character scalar. Invisibly returns the path to the saved
    #'   Excel file.
    export_excel = function(results, plan, path, ...) {
      if (!requireNamespace("openxlsx", quietly = TRUE)) {
        stop("[iQualityR] The 'openxlsx' package is required for Excel export. ",
             "Please install it with install.packages('openxlsx').",
             call. = FALSE)
      }
      wb <- openxlsx::createWorkbook()

      # Sheet 1: Design Summary
      design_df <- data.frame(
        Parameter = c("Design Type", "Number of Factors", "Replication",
                      "Center Points", "Total Runs"),
        Value = c(
          plan$design_type,
          length(plan$factors),
          plan$replication,
          plan$center_points,
          ifelse(!is.null(results$design_info), nrow(results$design_info), "N/A")
        )
      )
      openxlsx::addWorksheet(wb, "Design_Summary")
      openxlsx::writeData(wb, "Design_Summary", design_df)

      # Sheet 2: Design Table
      if (!is.null(results$design_info)) {
        openxlsx::addWorksheet(wb, "Design_Table")
        openxlsx::writeData(wb, "Design_Table", results$design_info)
      }

      # Sheet 3: ANOVA Results
      if (!is.null(results$anova_results)) {
        openxlsx::addWorksheet(wb, "ANOVA")
        openxlsx::writeData(wb, "ANOVA", results$anova_results)
      }

      openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
      invisible(path)
    },

    #' @description Export an HTML report. Renders the DOE R Markdown template
    #'   using `rmarkdown::render`, passing the analysis results, the design
    #'   plan, and the current report date as parameters.
    #' @param results A list of analysis results passed to the template via
    #'   the `results` parameter.
    #' @param plan An `IqrDoePlan` object passed to the template via the
    #'   `plan` parameter.
    #' @param path Character scalar specifying the output `.html` file path.
    #' @param theme_obj An optional `IqrTheme` object used to style the report.
    #' @param ... Additional arguments (currently ignored).
    #' @return Character scalar. Invisibly returns the path to the rendered
    #'   HTML file.
    export_html = function(results, plan, path, theme_obj = NULL, ...) {
      template_path <- system.file("templates", "doe_template.Rmd", package = "iQualityR.doe")

      if (!file.exists(template_path)) {
        stop("DOE HTML template not found: ", template_path, call. = FALSE)
      }

      rmarkdown::render(
        input = template_path,
        output_format = "html_document",
        output_file = basename(path),
        output_dir = dirname(path),
        params = list(
          results = results,
          plan = plan,
          report_date = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
        ),
        quiet = TRUE
      )

      invisible(path)
    },

    #' @description Export a Word (.docx) report. Uses the `officer` package
    #'   to assemble a structured document containing the design summary,
    #'   ANOVA tables, coefficient tables, and embedded diagnostic plots.
    #'   `officer` is a soft dependency installed on demand.
    #' @param results A list of analysis results.
    #' @param plan An `IqrDoePlan` object.
    #' @param path Character scalar specifying the output `.docx` file path.
    #' @param plots Optional named list of ggplot/patchwork objects to embed.
    #'   If `NULL`, no plots are embedded.
    #' @param ... Additional arguments (currently ignored).
    #' @return Character scalar. Invisibly returns the path to the saved
    #'   Word file.
    export_word = function(results, plan, path, plots = NULL, ...) {
      if (!requireNamespace("officer", quietly = TRUE)) {
        stop("[iQualityR] The 'officer' package is required for Word export. ",
             "Please install it with install.packages('officer').",
             call. = FALSE)
      }

      doc <- officer::read_docx()
      doc <- officer::body_add_par(doc,
        paste("DOE Report:", plan$task_tag),
        style = "Normal")

      # Design summary
      doc <- officer::body_add_par(doc, "Design Summary", style = "Normal")
      summary_df <- data.frame(
        Parameter = c("Design Type", "Factors", "Replication",
                      "Center Points", "Total Runs"),
        Value = c(
          plan$design_type,
          length(plan$factors),
          plan$replication,
          ifelse(is.null(plan$center_points), 0, plan$center_points),
          ifelse(!is.null(results$design_info), nrow(results$design_info), "N/A")
        ),
        stringsAsFactors = FALSE
      )
      doc <- officer::body_add_table(doc, summary_df)

      # ANOVA results
      if (!is.null(results$anova_results)) {
        doc <- officer::body_add_par(doc, "ANOVA Results", style = "Normal")
        anova_df <- if (is.data.frame(results$anova_results)) {
          results$anova_results
        } else if (is.list(results$anova_results) &&
                   !is.null(results$anova_results[[1]])) {
          # Multi-response: use the first response's ANOVA table.
          av <- results$anova_results[[1]]
          if (is.data.frame(av)) av else av$anova_table
        } else {
          NULL
        }
        if (!is.null(anova_df) && is.data.frame(anova_df)) {
          doc <- officer::body_add_table(doc, anova_df)
        }
      }

      # Embedded plots
      if (!is.null(plots) && length(plots) > 0) {
        doc <- officer::body_add_par(doc, "Diagnostic Plots",
                                      style = "Normal")
        for (plot_name in names(plots)) {
          doc <- officer::body_add_par(doc, plot_name, style = "Normal")
          tmp_img <- tempfile(fileext = ".png")
          ggplot2::ggsave(tmp_img, plots[[plot_name]],
                          width = 6, height = 4, dpi = 150)
          doc <- officer::body_add_img(doc, src = tmp_img,
                                        width = 6, height = 4)
          unlink(tmp_img)
        }
      }

      print(doc, target = path)
      invisible(path)
    },

    #' @description Export a PowerPoint (.pptx) report. Uses the `officer`
    #'   package to create a slide deck with one slide per section (design
    #'   summary, ANOVA, plots). `officer` is a soft dependency installed on
    #'   demand.
    #' @param results A list of analysis results.
    #' @param plan An `IqrDoePlan` object.
    #' @param path Character scalar specifying the output `.pptx` file path.
    #' @param plots Optional named list of ggplot/patchwork objects to embed.
    #' @param ... Additional arguments (currently ignored).
    #' @return Character scalar. Invisibly returns the path to the saved
    #'   PowerPoint file.
    export_powerpoint = function(results, plan, path, plots = NULL, ...) {
      if (!requireNamespace("officer", quietly = TRUE)) {
        stop("[iQualityR] The 'officer' package is required for PowerPoint ",
             "export. Please install it with install.packages('officer').",
             call. = FALSE)
      }

      pres <- officer::read_pptx()

      # Title slide — use "Title and Content" layout (universally available)
      # to avoid placeholder-type mismatches across officer/pptx versions.
      pres <- officer::add_slide(pres, layout = "Title and Content",
                                  master = "Office Theme")
      pres <- officer::ph_with(pres,
        paste("DOE Report:", plan$task_tag),
        location = officer::ph_location_type(type = "title"))
      pres <- officer::ph_with(pres,
        format(Sys.time(), "%Y-%m-%d"),
        location = officer::ph_location_type(type = "body"))

      # Design summary slide
      pres <- officer::add_slide(pres, layout = "Title and Content",
                                  master = "Office Theme")
      pres <- officer::ph_with(pres, "Design Summary",
        location = officer::ph_location_type(type = "title"))
      summary_df <- data.frame(
        Parameter = c("Design Type", "Factors", "Replication",
                      "Center Points", "Total Runs"),
        Value = c(
          plan$design_type,
          length(plan$factors),
          plan$replication,
          ifelse(is.null(plan$center_points), 0, plan$center_points),
          ifelse(!is.null(results$design_info), nrow(results$design_info), "N/A")
        ),
        stringsAsFactors = FALSE
      )
      pres <- officer::ph_with(pres, summary_df,
        location = officer::ph_location_type(type = "body"))

      # ANOVA slide
      if (!is.null(results$anova_results)) {
        anova_df <- if (is.data.frame(results$anova_results)) {
          results$anova_results
        } else if (is.list(results$anova_results) &&
                   !is.null(results$anova_results[[1]])) {
          av <- results$anova_results[[1]]
          if (is.data.frame(av)) av else av$anova_table
        } else {
          NULL
        }
        if (!is.null(anova_df) && is.data.frame(anova_df)) {
          pres <- officer::add_slide(pres, layout = "Title and Content",
                                      master = "Office Theme")
          pres <- officer::ph_with(pres, "ANOVA Results",
            location = officer::ph_location_type(type = "title"))
          pres <- officer::ph_with(pres, anova_df,
            location = officer::ph_location_type(type = "body"))
        }
      }

      # Plot slides
      if (!is.null(plots) && length(plots) > 0) {
        for (plot_name in names(plots)) {
          pres <- officer::add_slide(pres, layout = "Title and Content",
                                      master = "Office Theme")
          pres <- officer::ph_with(pres, plot_name,
            location = officer::ph_location_type(type = "title"))
          tmp_img <- tempfile(fileext = ".png")
          ggplot2::ggsave(tmp_img, plots[[plot_name]],
                          width = 8, height = 5, dpi = 150)
          pres <- officer::ph_with(pres, officer::external_img(tmp_img,
                                                                 width = 8,
                                                                 height = 5),
            location = officer::ph_location_type(type = "body"))
          unlink(tmp_img)
        }
      }

      print(pres, target = path)
      invisible(path)
    }
  ),
  private = list()
)
