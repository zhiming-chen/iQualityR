#' General chart save function
#'
#' Supports DiagrammeR (grViz, mermaid, dgr_graph), ggplot2, patchwork and other objects.
#'
#' @importFrom ggplot2 ggsave
#' @importFrom patchwork wrap_elements
#' @importFrom utils modifyList
#' @name iQualityR.plot-save
#' @keywords internal
NULL

#' General chart save function
#'
#' Supports DiagrammeR (grViz, mermaid, dgr_graph), ggplot2, patchwork and other objects.
#'
#' @param obj Graph object
#' @param filename Output file name (can have extension)
#' @param type File type, used when filename has no extension, optional "png","svg","pdf"
#' @param width Width (pixels for grViz/htmlwidget; inches for dgr_graph; inches for ggplot/patchwork)
#' @param height Height
#' @param dpi Resolution (ggplot/patchwork only)
#' @param ... Other arguments passed to underlying functions
#'
#' @return No return value, saves file directly
#' @export
#'
#' @examples
#' \dontrun{
#' g <- grViz("digraph { A -> B }")
#' save_diagram(g, "my_graph.png")
#'
#' p <- ggplot(mtcars, aes(mpg, wt)) + geom_point()
#' save_diagram(p, "scatter.pdf", width = 6, height = 4)
#' }
save_diagram <- function(obj, filename, type = c("png","svg","pdf"),
                         width = NULL, height = NULL, dpi = 300, ...) {
    type <- match.arg(type)

    # Process file name extension
    if (grepl("\\.(png|svg|pdf)$", filename, ignore.case = TRUE)) {
        ext <- tolower(gsub(".*\\.", "", filename))
        type <- ext
    } else {
        filename <- paste0(filename, ".", type)
    }

    # Determine object type and save
    if (inherits(obj, "grViz") || inherits(obj, "htmlwidget")) {
        # DiagrammeR grViz or mermaid
        if (!requireNamespace("DiagrammeRsvg", quietly = TRUE))
            stop("Please install 'DiagrammeRsvg' package")
        if (type != "svg" && !requireNamespace("rsvg", quietly = TRUE))
            stop("Please install 'rsvg' package")

        svg_str <- DiagrammeRsvg::export_svg(obj)
        if (type == "svg") {
            writeLines(svg_str, filename)
        } else if (type == "png") {
            rsvg::rsvg_png(charToRaw(svg_str), file = filename,
                           width = width, height = height)
        } else if (type == "pdf") {
            rsvg::rsvg_pdf(charToRaw(svg_str), file = filename,
                           width = width, height = height)
        }

    } else if (inherits(obj, "dgr_graph")) {
        # DiagrammeR dgr_graph
        if (!requireNamespace("DiagrammeR", quietly = TRUE))
            stop("Please install 'DiagrammeR' package")
        DiagrammeR::export_graph(obj, file_name = filename,
                                 file_type = toupper(type),
                                 width = width, height = height, ...)

    } else if (inherits(obj, "gg") || inherits(obj, "ggplot")) {
        # ggplot2 object
        if (!requireNamespace("ggplot2", quietly = TRUE))
            stop("Please install 'ggplot2' package")
        # Ensure width/height have default values
        if (is.null(width)) width <- 7
        if (is.null(height)) height <- 5
        ggplot2::ggsave(filename = filename, plot = obj, device = type,
                        width = width, height = height, dpi = dpi, ...)

    } else if (inherits(obj, "patchwork")) {
        # patchwork combined plot
        if (!requireNamespace("patchwork", quietly = TRUE))
            stop("Please install 'patchwork' package")
        # Convert patchwork object to saveable ggplot object
        p <- patchwork::wrap_elements(obj)
        if (is.null(width)) width <- 7
        if (is.null(height)) height <- 5
        ggplot2::ggsave(filename = filename, plot = p, device = type,
                        width = width, height = height, dpi = dpi, ...)

    } else {
        stop("Unsupported object type: ", paste(class(obj), collapse = ", "))
    }

    message("Image saved to: ", filename)
}
