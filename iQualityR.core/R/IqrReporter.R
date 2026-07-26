#' @title IqrReporter — Global Report Generation Service
#'
#' @description
#' Responsible for exporting analysis results from all task modules to Excel,
#' HTML, PDF, Word (docx), or PowerPoint (pptx). Theme styles are obtained from
#' an IqrTheme object, but Excel export is delegated to the independent ExcelExporter
#' class. Supports template discovery via both registered templates and
#' convention-based file paths.
#'
#' @field theme_obj IqrTheme object (for configuration and plot themes).
#' @field excel_exporter ExcelExporter instance for generating xlsx files.
#' @field templates List of registered templates (rmd + excel_gen per task).
#'
#' @importFrom R6 R6Class
#' @importFrom rmarkdown render
#' @export
IqrReporter <- R6::R6Class("IqrReporter",
  public = list(
    theme_obj = NULL,
    excel_exporter = NULL,
    templates = list(),

    #' @description Create an IqrReporter instance.
    #' @param theme_obj IqrTheme object providing colors and plot themes.
    initialize = function(theme_obj) {
      if (!inherits(theme_obj, "IqrTheme")) {
        stop("theme_obj must be an IqrTheme instance", call. = FALSE)
      }
      self$theme_obj <- theme_obj
      self$excel_exporter <- ExcelExporter$new(theme_obj$config)
    },

    #' @description Register a task template for later use.
    #' @param task_tag Character. Task tag (e.g., "capability", "desc", "msa").
    #' @param rmd_template Character. Rmd template file path (for HTML/PDF/Word/PPTX output).
    #' @param excel_generator Optional function. Custom Excel generation function
    #'   (receives \code{results} and \code{plan} and returns a named list of data frames).
    register = function(task_tag,
                        rmd_template = NULL,
                        excel_generator = NULL) {
      self$templates[[task_tag]] <- list(
        rmd = rmd_template,
        excel_gen = excel_generator
      )
      invisible(self)
    },

    #' @description Backward-compatible Rmd rendering entry point.
    #' @param task_tag Character. Task tag used to locate the template.
    #' @param params Named list. Parameters passed to the Rmd template.
    #' @param path Character. Output file path (auto-generated if NULL).
    #' @param format Character. Output format (html, pdf, docx, pptx).
    #' @param ... Additional parameters passed to \code{rmarkdown::render}.
    render = function(task_tag,
                      params = list(),
                      path = NULL,
                      format = "html",
                      ...) {
      if (!is.list(params)) {
        stop("params must be a list.", call. = FALSE)
      }

      results <- params$results
      plan <- params$plan
      extra_params <- params[setdiff(names(params), c("results", "plan"))]

      if (is.null(path)) {
        timestamp <- base::format(Sys.time(), "%Y%m%d_%H%M%S")
        ext <- switch(format,
          html = "html",
          pdf = "pdf",
          docx = "docx",
          pptx = "pptx",
          "html"
        )
        path <- paste0("report_", task_tag, "_", timestamp, ".", ext)
      }

      args <- c(
        list(
          results = results,
          plan = plan,
          task_tag = task_tag,
          path = path,
          format = format
        ),
        extra_params,
        list(...)
      )
      do.call(self$export_rmd, args)
      invisible(path)
    },

    #' @description Unified export entry point supporting all formats.
    #' @param results Analysis results list.
    #' @param plan Plan configuration object (optional).
    #' @param task_tag Character. Task tag used to find the registered template.
    #' @param format Character. Output format (excel, html, pdf, docx, pptx).
    #' @param path Character. Output file path (auto-generated if NULL).
    #' @param ... Additional parameters passed to \code{rmarkdown::render}.
    export = function(results,
                      plan = NULL,
                      task_tag = "default",
                      format = "excel",
                      path = NULL, ...) {
      if (is.null(path)) {
        timestamp <- base::format(Sys.time(), "%Y%m%d_%H%M%S")
        ext <- switch(format,
          excel = "xlsx",
          html  = "html",
          pdf   = "pdf",
          docx  = "docx",
          pptx  = "pptx",
          "html"
        )
        path <- paste0(
          "report_", task_tag, "_", timestamp, ".", ext
        )
      }

      switch(format,
        excel = self$export_excel(results, plan, task_tag, path, ...),
        html = self$export_rmd(results, plan, task_tag, path, format = "html", ...),
        pdf = self$export_rmd(results, plan, task_tag, path, format = "pdf", ...),
        docx = self$export_rmd(results, plan, task_tag, path, format = "docx", ...),
        pptx = self$export_rmd(results, plan, task_tag, path, format = "pptx", ...),
        stop("Unsupported format: ", format, call. = FALSE)
      )
      invisible(path)
    },

    #' @description Export results to an Excel file (uses custom generator,
    #'   results$to_excel(), or a default flattening strategy).
    #' @param results Analysis results list.
    #' @param plan Plan configuration object (optional).
    #' @param task_tag Character. Task tag for registered generator lookup.
    #' @param path Character. Output file path.
    #' @param ... Additional parameters forwarded to ExcelExporter$export_excel.
    export_excel = function(results,
                            plan = NULL,
                            task_tag,
                            path, ...) {
      if (is.function(self$templates[[task_tag]]$excel_gen)) {
        sheets <- self$templates[[task_tag]]$excel_gen(results, plan)
      } else if (is.function(results$to_excel)) {
        sheets <- results$to_excel(plan)
      } else {
        flat <- unlist(results, recursive = FALSE)
        flat <- flat[!sapply(flat, is.list)]
        sheets <- list(Summary = as.data.frame(flat))
      }
      self$excel_exporter$export_excel(data = sheets, path = path, ...)
    },

    #' @description Export results via R markdown (supports html, pdf, docx, pptx).
    #' @param results Analysis results list.
    #' @param plan Plan configuration object (optional).
    #' @param task_tag Character. Task tag for template lookup.
    #' @param path Character. Output file path.
    #' @param format Character. Rmd output format (html, pdf, docx, pptx).
    #' @param ... Additional parameters forwarded to \code{rmarkdown::render}.
    export_rmd = function(results,
                          plan = NULL,
                          task_tag,
                          path,
                          format = "html",
                          ...) {
      template <- self$templates[[task_tag]]$rmd
      template_file <- paste0(task_tag, "_template.Rmd")

      if (is.null(template)) {
        template <- system.file("templates", template_file, package = "iQualityR")
        if (template == "") {
          template <- system.file("templates", template_file, package = "iQualityR.core")
        }
        if (template == "") {
          candidate_packages <- c(
            "iQualityR.msa", "iQualityR.sampling", "iQualityR.predict",
            "iQualityR.doe", "iQualityR.reliability", "iQualityR.capa",
            "iQualityR.spc", "iQualityR.stat"
          )
          for (pkg in candidate_packages) {
            template <- system.file("templates", template_file, package = pkg)
            if (template != "") break
          }
        }
        if (template == "") {
          search_roots <- unique(c(getwd(), dirname(getwd())))
          repo_templates <- unlist(lapply(search_roots, function(root) {
            Sys.glob(file.path(root, "iQualityR.*", "inst", "templates", template_file))
          }), use.names = FALSE)
          if (length(repo_templates) > 0) {
            template <- repo_templates[[1]]
          }
        }
        if (template == "") {
          stop(
            "No Rmd template found for task: ", task_tag,
            ". Please create inst/templates/", task_tag, "_template.Rmd",
            " or register a template via $register().",
            call. = FALSE
          )
        }
        self$templates[[task_tag]]$rmd <- template
      }

      out_dir <- dirname(path)
      if (!dir.exists(out_dir)) {
        dir.create(out_dir, recursive = TRUE)
      }

      params <- list(
        results = results,
        plan = plan,
        theme_obj = self$theme_obj,
        timestamp = Sys.time(),
        ...
      )
      if (is.list(results)) {
        for (name in c("study_type", "statistics", "diagnostics", "data_tables", "raw_output")) {
          if (is.null(params[[name]]) && !is.null(results[[name]])) {
            params[[name]] <- results[[name]]
          }
        }
      }

      output_format <- switch(format,
        html = "html_document",
        pdf  = "pdf_document",
        docx = "word_document",
        pptx = "powerpoint_presentation"
      )
      rmarkdown::render(
        input = template,
        output_file = basename(path),
        output_dir = out_dir,
        output_format = output_format,
        params = params,
        envir = new.env(parent = globalenv()),
        quiet = TRUE
      )
    }
  )
)
