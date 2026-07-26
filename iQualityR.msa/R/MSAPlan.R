# =============================================================================
# File: iQualityR.msa/R/MSAPlan.R
# Description: MSA Measurement System Analysis plan class - AIAG MSA 4th Edition standard
# Follows iQualityR framework specification v3.0
# =============================================================================

#' @title MSAPlan Class
#' @description MSA measurement plan configuration class, inherits from IqrPlanBase
#' Supports Type 1/Type 2 GRR, crossed/nested designs, multi-specification configuration
#'
#' @field study_type Character scalar identifying the MSA study type.
#' @field method Design method: `"crossed"` or `"nested"`.
#' @field num_operators Integer number of operators.
#' @field num_parts Integer number of parts.
#' @field num_measurements Integer number of measurements (replications) per part.
#' @field operator_names Character vector of operator names.
#' @field part_names Character vector of part names.
#' @field design_matrix Data frame holding the experimental design and run order.
#' @field randomization_seed Integer seed used for randomization, or `NULL`.
#' @field specifications List of specification entries keyed by spec ID.
#' @field equipment_info List describing the measurement equipment.
#' @field measurement_sequences List of measurement sequence definitions.
#'
#' @export
MSAPlan <- R6::R6Class("MSAPlan",
                        inherit = IqrPlanBase,
                        public = list(
                            # MSA configuration fields
                            study_type = NULL,
                            method = NULL,
                            num_operators = NULL,
                            num_parts = NULL,
                            num_measurements = NULL,
                            operator_names = NULL,
                            part_names = NULL,
                            design_matrix = NULL,
                            randomization_seed = NULL,
                            specifications = NULL,
                            equipment_info = NULL,
                            measurement_sequences = NULL,

                            #' @description Create MSAPlan instance
                            #' @param plan_name plan name
                            #' @param objectives analysis objectives
                            #' @param operators number of operators or name vector
                            #' @param parts number of parts or name vector
                            #' @param measurements number of measurements
                            #' @param method design method: "crossed" or "nested"
                            #' @param randomize whether to randomize
                            #' @param randomization_seed random seed
                            initialize = function(plan_name,
                                                  objectives,
                                                  operators = 3,
                                                  parts = 10,
                                                  measurements = 3,
                                                  method = c("crossed", "nested"),
                                                  randomize = TRUE,
                                                  randomization_seed = NULL) {
                                method <- match.arg(method)
                                self$task_tag <- "msa"
                                self$study_type <- "gage_rr"
                                self$method <- method
                                self$randomization_seed <- randomization_seed
                                self$num_measurements <- as.integer(measurements)
                                self$specifications <- list()
                                self$equipment_info <- list()
                                self$measurement_sequences <- list()

                                if (is.numeric(operators)) {
                                    self$num_operators <- as.integer(operators)
                                    self$operator_names <- paste0("Operator_", 1:self$num_operators)
                                } else {
                                    self$operator_names <- as.character(operators)
                                    self$num_operators <- length(self$operator_names)
                                }

                                if (is.numeric(parts)) {
                                    self$num_parts <- as.integer(parts)
                                    self$part_names <- paste0("Part_", 1:self$num_parts)
                                } else {
                                    self$part_names <- as.character(parts)
                                    self$num_parts <- length(self$part_names)
                                }

                                super$initialize(task_tag = "msa", conf_level = 0.95)
                                self$set_meta("project", plan_name = plan_name, objectives = objectives)
                                self$generate_design(randomize)
                                invisible(self)
                            },

                            #' @description Generate experimental design matrix
                            #' @param randomize whether to randomize
                            generate_design = function(randomize = TRUE) {
                                op_vec <- factor(self$operator_names)
                                part_vec <- factor(self$part_names)

                                if (self$method == "crossed") {
                                    design <- expand.grid(
                                        Operator = op_vec,
                                        Part = part_vec,
                                        Replication = 1:self$num_measurements
                                    ) %>% dplyr::arrange(Operator, Part, Replication)
                                } else {
                                    nested_parts <- expand.grid(
                                        Operator = op_vec,
                                        PartGroup = 1:self$num_parts
                                    ) %>% dplyr::mutate(Part = factor(paste0(Operator, "_Part_", PartGroup)))

                                    design <- expand.grid(
                                        Operator = nested_parts$Operator,
                                        Part = nested_parts$Part,
                                        Replication = 1:self$num_measurements
                                    ) %>% dplyr::arrange(Operator, Part, Replication)
                                }

                                if (randomize) {
                                    if (!is.null(self$randomization_seed)) {
                                        set.seed(self$randomization_seed)
                                    }
                                    design <- self$practical_randomization(design)
                                } else {
                                    design$RunOrder <- seq_len(nrow(design))
                                    design$StandardOrder <- seq_len(nrow(design))
                                }

                                design$MeasurementValue <- NA_real_
                                design$Comments <- ""
                                self$design_matrix <- design
                                invisible(self)
                            },

                            #' @description MSA-compliant randomization
                            #' @param design design matrix
                            practical_randomization = function(design) {
                                operator_sequences <- list()
                                current_run_order <- 1

                                for (op in unique(design$Operator)) {
                                    part_order <- sample(unique(design$Part))
                                    op_tasks <- list()

                                    for (replication in 1:self$num_measurements) {
                                        current_part_order <- sample(part_order)
                                        for (part in current_part_order) {
                                            task <- design %>%
                                                dplyr::filter(Operator == op, Part == part, Replication == replication) %>%
                                                dplyr::mutate(RunOrder = current_run_order, StandardOrder = current_run_order)
                                            current_run_order <- current_run_order + 1
                                            op_tasks[[length(op_tasks) + 1]] <- task
                                        }
                                    }
                                    operator_sequences[[op]] <- dplyr::bind_rows(op_tasks)
                                }

                                randomized_design <- dplyr::bind_rows(operator_sequences) %>% dplyr::arrange(RunOrder)
                                return(randomized_design)
                            },

                            #' @description Add specification information
                            #' @param spec_name specification name
                            #' @param spec_id specification ID
                            #' @param part_name part name
                            #' @param part_no part number
                            #' @param nominal_value nominal value
                            #' @param usl upper specification limit
                            #' @param lsl lower specification limit
                            #' @param unit unit
                            add_specification = function(spec_name, spec_id,
                                                          part_name = NULL, part_no = NULL,
                                                          nominal_value = NULL, usl = NULL, lsl = NULL,
                                                          unit = NULL) {
                                spec <- list(
                                    name = spec_name,
                                    id = spec_id,
                                    part_name = part_name,
                                    part_no = part_no,
                                    nominal_value = nominal_value,
                                    usl = usl,
                                    lsl = lsl,
                                    unit = unit,
                                    tolerance = if (!is.null(usl) && !is.null(lsl)) usl - lsl else NULL
                                )
                                self$specifications[[spec_id]] <- spec
                                invisible(self)
                            },

                            #' @description Add equipment information
                            #' @param equipment_name equipment name
                            #' @param equipment_id equipment ID
                            #' @param equipment_type equipment type
                            #' @param equipment_model equipment model
                            #' @param resolution resolution
                            #' @param calibration_due calibration due date
                            #' @param capability_index equipment capability index (e.g. Cg/Cgk)
                            add_equipment = function(equipment_name, equipment_id,
                                                     equipment_type = "measurement_device",
                                                     equipment_model = NULL,
                                                     resolution = NA_real_,
                                                     calibration_due = NULL,
                                                     capability_index = NA_real_) {
                                self$equipment_info <- list(
                                    name = equipment_name,
                                    id = equipment_id,
                                    type = equipment_type,
                                    model = equipment_model,
                                    resolution = resolution,
                                    calibration_due = calibration_due,
                                    capability_index = capability_index
                                )
                                invisible(self)
                            },

                            #' @description Get measurement plan sheet
                            #' @return measurement plan as data frame
                            get_measurement_sheet = function() {
                                if (is.null(self$design_matrix)) {
                                    return(data.frame())
                                }
                                self$design_matrix %>%
                                    dplyr::select(RunOrder, StandardOrder, Operator, Part, Replication, MeasurementValue, Comments) %>%
                                    dplyr::arrange(RunOrder)
                            },

                            #' @description Export measurement plan
                            #' @param format export format: "excel" or "csv"
                            #' @param file_path output file path
                            export_plan = function(format = c("excel", "csv"), file_path = NULL) {
                                format <- match.arg(format)
                                sheet <- self$get_measurement_sheet()

                                if (format == "excel") {
                                    if (is.null(file_path)) {
                                        file_path <- paste0("MSA_Plan_", self$task_tag, "_", Sys.Date(), ".xlsx")
                                    }
                                    wb <- openxlsx::createWorkbook()
                                    openxlsx::addWorksheet(wb, "Measurement Plan")
                                    openxlsx::writeData(wb, "Measurement Plan", sheet)
                                    openxlsx::setColWidths(wb, "Measurement Plan", cols = 1:ncol(sheet), widths = "auto")
                                    openxlsx::saveWorkbook(wb, file_path, overwrite = TRUE)
                                } else {
                                    if (is.null(file_path)) {
                                        file_path <- paste0("MSA_Plan_", self$task_tag, "_", Sys.Date(), ".csv")
                                    }
                                    utils::write.csv(sheet, file_path, row.names = FALSE, fileEncoding = "UTF-8")
                                }
                                message("Measurement plan has been exported to: ", file_path)
                                invisible(sheet)
                            },

                            #' @description Validate plan configuration
                            validate = function() {
                                super$validate()
                                if (self$num_operators < 2) {
                                    stop("MSA requires at least 2 operators.")
                                }
                                if (self$num_parts < 5) {
                                    stop("MSA requires at least 5 parts.")
                                }
                                invisible(self)
                            },

                            #' @description Print plan summary
                            print = function() {
                                cat("=== MSA Measurement Plan ===\n")
                                cat("Plan Name:", self$meta_data$project$plan_name, "\n")
                                cat("Objectives:", self$meta_data$project$objectives, "\n")
                                cat("Study Type:", self$study_type, "\n")
                                cat("Design Method:", self$method, "\n")
                                cat("Number of Operators:", self$num_operators, "\n")
                                cat("Number of Parts:", self$num_parts, "\n")
                                cat("Number of Measurements:", self$num_measurements, "\n")
                                cat("Total Trials:", nrow(self$design_matrix), "\n")
                                if (length(self$specifications) > 0) {
                                    cat("Number of Specifications:", length(self$specifications), "\n")
                                }
                            }
                        )
)
