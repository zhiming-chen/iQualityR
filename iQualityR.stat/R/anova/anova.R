# =============================================================================
# 文件: R/anova/anova_helpers.R
# 描述: ANOVA 内部辅助函数（效应量、多重比较、模型诊断）
# =============================================================================

#' 计算效应量（η², 偏 η², ω²）
#' @param model lm/aov 或 lmer 模型
#' @param type 效应量类型（"eta", "partial_eta", "omega"）
#' @return 效应量数值或列表
.calc_effect_size <- function(model, type = c("eta", "partial_eta", "omega")) {
    type <- match.arg(type)
    if (inherits(model, "lmerMod")) {
        # 混合模型使用 r2glmm 或 MuMIn
        if (requireNamespace("MuMIn", quietly = TRUE)) {
            return(MuMIn::r.squaredGLMM(model))
        } else {
            warning("MuMIn package required for mixed model effect size.")
            return(NULL)
        }
    }
    # 常规 lm/aov
    s <- summary(model)
    aov_table <- as.data.frame(s[[1]])
    ss <- aov_table[, "Sum Sq"]
    df <- aov_table[, "Df"]
    ss_total <- sum(ss)
    ss_res <- ss[length(ss)]
    
    if (type == "eta") {
        eta <- ss / ss_total
        names(eta) <- rownames(aov_table)
        return(eta)
    } else if (type == "partial_eta") {
        partial_eta <- ss / (ss + ss_res)
        names(partial_eta) <- rownames(aov_table)
        return(partial_eta)
    } else {  # omega
        ms <- ss / df
        ms_res <- ss_res / df[length(df)]
        omega <- (ss - df * ms_res) / (ss_total + ms_res)
        names(omega) <- rownames(aov_table)
        return(omega)
    }
}

#' 多重比较
#' @param model lm/aov 模型
#' @param factor 因子名（用于单因素），或 list(因子名, ...) 用于多因素交互
#' @param method 方法（"tukey", "bonferroni", "dunnett", "lsd", "scheffe"）
#' @param alpha 显著性水平
#' @return 多重比较结果数据框
.multiple_comparisons <- function(model, factor, method = "tukey", alpha = 0.05) {
    if (!requireNamespace("multcomp", quietly = TRUE)) {
        stop("multcomp package required for multiple comparisons.")
    }
    if (!requireNamespace("emmeans", quietly = TRUE)) {
        stop("emmeans package required for multiple comparisons.")
    }
    
    # 使用 emmeans 计算
    emm <- emmeans::emmeans(model, as.formula(paste("~", factor)))
    contrast_method <- switch(method,
                              "tukey" = "pairwise",
                              "bonferroni" = "pairwise",
                              "dunnett" = "dunnett",
                              "lsd" = "pairwise",
                              "scheffe" = "pairwise"
    )
    contrast <- emmeans::contrast(emm, method = contrast_method)
    p_adj <- switch(method,
                    "tukey" = "tukey",
                    "bonferroni" = "bonferroni",
                    "dunnett" = "dunnett",
                    "lsd" = "none",
                    "scheffe" = "scheffe"
    )
    summary_contrast <- summary(contrast, adjust = p_adj, level = 1 - alpha)
    return(summary_contrast)
}


# =============================================================================
# 文件: R/anova/AnovaAnalyzer.R
# 描述: ANOVA 计算引擎（纯计算，零图形）
# =============================================================================

#' @title AnovaAnalyzer: 方差分析计算引擎
#' @description
#' 执行各类方差分析，返回结构化结果。
#'
#' 支持：
#' - 单因素 ANOVA
#' - 双因素 ANOVA（含交互）
#' - 多因素 ANOVA
#' - 重复测量 ANOVA
#' - 混合模型（线性混合效应模型）
#' - 多元方差分析（MANOVA）
#'
#' @export
AnovaAnalyzer <- R6::R6Class("AnovaAnalyzer",
                             public = list(
                                 #' @description 单因素 ANOVA
                                 #' @param formula 公式 (response ~ group)
                                 #' @param data 数据框
                                 #' @param ... 其他参数传给 aov
                                 #' @return 结构化结果
                                 anova_oneway = function(formula, data, ...) {
                                     private$.anova_oneway(list(formula = formula, data = data, ...))
                                 },
                                 
                                 #' @description 双因素 ANOVA（含交互）
                                 #' @param formula 公式 (response ~ factor1 * factor2)
                                 #' @param data 数据框
                                 #' @param ... 其他参数
                                 #' @return 结构化结果
                                 anova_twoway = function(formula, data, ...) {
                                     private$.anova_twoway(list(formula = formula, data = data, ...))
                                 },
                                 
                                 #' @description 多因素 ANOVA（3+ 因素）
                                 #' @param formula 公式
                                 #' @param data 数据框
                                 #' @param ... 其他参数
                                 #' @return 结构化结果
                                 anova_multifactor = function(formula, data, ...) {
                                     private$.anova_multifactor(list(formula = formula, data = data, ...))
                                 },
                                 
                                 #' @description 重复测量 ANOVA
                                 #' @param formula 公式 (response ~ factor1 + Error(subject/factor))
                                 #' @param data 数据框
                                 #' @param ... 其他参数传给 aov
                                 #' @return 结构化结果
                                 anova_repeated = function(formula, data, ...) {
                                     private$.anova_repeated(list(formula = formula, data = data, ...))
                                 },
                                 
                                 #' @description 混合模型（线性混合效应模型）
                                 #' @param formula 公式 (response ~ fixed + (1|random))
                                 #' @param data 数据框
                                 #' @param method 拟合方法（"REML", "ML"）
                                 #' @param ... 其他参数传给 lmer
                                 #' @return 结构化结果
                                 anova_mixed = function(formula, data, method = "REML", ...) {
                                     private$.anova_mixed(list(formula = formula, data = data, method = method, ...))
                                 },
                                 
                                 #' @description 多元方差分析（MANOVA）
                                 #' @param formula 公式 (cbind(y1, y2) ~ group)
                                 #' @param data 数据框
                                 #' @param test 检验统计量（"Wilks", "Pillai", "Hotelling-Lawley", "Roy"）
                                 #' @return 结构化结果
                                 manova = function(formula, data, test = c("Wilks", "Pillai", "Hotelling-Lawley", "Roy")) {
                                     private$.manova(list(formula = formula, data = data, test = match.arg(test)))
                                 },
                                 
                                 #' @description 统一入口：根据公式结构自动选择
                                 #' @param formula 公式
                                 #' @param data 数据框
                                 #' @param ... 其他参数
                                 #' @return 结构化结果
                                 analyze = function(formula, data, ...) {
                                     # 检测公式类型
                                     private$.detect_and_run(formula, data, ...)
                                 },
                                 report = function(results = NULL, format = "excel", path = NULL, theme_obj = NULL) {
                                     if (is.null(results)) {
                                         if (is.null(self$last_results)) {
                                             stop("No results available. Run analysis first.")
                                         }
                                         results <- self$last_results
                                     }
                                     if (is.null(theme_obj)) {
                                         theme_obj <- IqrTheme$new()
                                     }
                                     reporter <- AnovaReporter$new(theme_obj)
                                     reporter$report(results, format = format, path = path)
                                 }
                             )
)
                             ),
                             
                             private = list(
                                 .detect_and_run = function(formula, data, ...) {
                                     # 检测是否有 Error() 项（重复测量）
                                     if (grepl("Error\\(", deparse(formula))) {
                                         return(self$anova_repeated(formula, data, ...))
                                     }
                                     # 检测是否有随机项 (1|...) 或 (1|...)
                                     if (grepl("\\(1\\|", deparse(formula))) {
                                         return(self$anova_mixed(formula, data, ...))
                                     }
                                     # 检测响应是否为多变量 cbind()
                                     response <- as.character(formula[[2]])
                                     if (grepl("^cbind\\(", response)) {
                                         return(self$manova(formula, data, ...))
                                     }
                                     # 否则根据因子数决定
                                     factors <- attr(stats::terms(formula), "factors")
                                     n_factors <- ncol(factors) - 1  # 减去响应
                                     if (n_factors == 1) {
                                         return(self$anova_oneway(formula, data, ...))
                                     } else if (n_factors == 2) {
                                         return(self$anova_twoway(formula, data, ...))
                                     } else {
                                         return(self$anova_multifactor(formula, data, ...))
                                     }
                                 },
                                 
                                 .anova_oneway = function(args) {
                                     model <- do.call(stats::aov, args)
                                     model_summary <- summary(model)
                                     anova_table <- as.data.frame(model_summary[[1]])
                                     # 多重比较
                                     factors <- labels(stats::terms(model))
                                     comp <- tryCatch(.multiple_comparisons(model, factors[1], method = "tukey"), error = function(e) NULL)
                                     # 效应量
                                     eta <- .calc_effect_size(model, type = "eta")
                                     partial_eta <- .calc_effect_size(model, type = "partial_eta")
                                     
                                     list(
                                         test_type = "One-way ANOVA",
                                         formula = deparse(args$formula),
                                         data_name = deparse(substitute(args$data)),
                                         model = model,
                                         anova_table = anova_table,
                                         summary = model_summary,
                                         coefficients = coef(model),
                                         r_squared = summary(model)$r.squared,
                                         adj_r_squared = summary(model)$adj.r.squared,
                                         multiple_comparisons = comp,
                                         effect_size = list(eta = eta, partial_eta = partial_eta),
                                         residuals = residuals(model),
                                         fitted = fitted(model),
                                         n = length(residuals(model)),
                                         factors = labels(stats::terms(model)),
                                         method = "aov"
                                     )
                                 },
                                 
                                 .anova_twoway = function(args) {
                                     model <- do.call(stats::aov, args)
                                     anova_table <- as.data.frame(summary(model)[[1]])
                                     factors <- labels(stats::terms(model))
                                     comp <- tryCatch(.multiple_comparisons(model, factors[1], method = "tukey"), error = function(e) NULL)
                                     eta <- .calc_effect_size(model, type = "eta")
                                     partial_eta <- .calc_effect_size(model, type = "partial_eta")
                                     
                                     list(
                                         test_type = "Two-way ANOVA",
                                         formula = deparse(args$formula),
                                         data_name = deparse(substitute(args$data)),
                                         model = model,
                                         anova_table = anova_table,
                                         summary = summary(model),
                                         coefficients = coef(model),
                                         r_squared = summary(model)$r.squared,
                                         adj_r_squared = summary(model)$adj.r.squared,
                                         multiple_comparisons = comp,
                                         effect_size = list(eta = eta, partial_eta = partial_eta),
                                         residuals = residuals(model),
                                         fitted = fitted(model),
                                         n = length(residuals(model)),
                                         factors = labels(stats::terms(model)),
                                         method = "aov"
                                     )
                                 },
                                 
                                 .anova_multifactor = function(args) {
                                     # 与双因素类似，但处理更多因子
                                     model <- do.call(stats::aov, args)
                                     anova_table <- as.data.frame(summary(model)[[1]])
                                     factors <- labels(stats::terms(model))
                                     # 多重比较可能针对第一个因子
                                     comp <- tryCatch(.multiple_comparisons(model, factors[1], method = "tukey"), error = function(e) NULL)
                                     eta <- .calc_effect_size(model, type = "eta")
                                     partial_eta <- .calc_effect_size(model, type = "partial_eta")
                                     
                                     list(
                                         test_type = "Multi-factor ANOVA",
                                         formula = deparse(args$formula),
                                         data_name = deparse(substitute(args$data)),
                                         model = model,
                                         anova_table = anova_table,
                                         summary = summary(model),
                                         coefficients = coef(model),
                                         r_squared = summary(model)$r.squared,
                                         adj_r_squared = summary(model)$adj.r.squared,
                                         multiple_comparisons = comp,
                                         effect_size = list(eta = eta, partial_eta = partial_eta),
                                         residuals = residuals(model),
                                         fitted = fitted(model),
                                         n = length(residuals(model)),
                                         factors = labels(stats::terms(model)),
                                         method = "aov"
                                     )
                                 },
                                 
                                 .anova_repeated = function(args) {
                                     model <- do.call(stats::aov, args)
                                     # 处理 Error() 结构
                                     anova_summary <- summary(model)
                                     # 提取 Error 部分
                                     error_terms <- names(anova_summary)
                                     anova_tables <- lapply(anova_summary, as.data.frame)
                                     names(anova_tables) <- error_terms
                                     
                                     list(
                                         test_type = "Repeated Measures ANOVA",
                                         formula = deparse(args$formula),
                                         data_name = deparse(substitute(args$data)),
                                         model = model,
                                         anova_tables = anova_tables,
                                         summary = anova_summary,
                                         coefficients = coef(model),
                                         residuals = residuals(model),
                                         fitted = fitted(model),
                                         n = length(residuals(model)),
                                         method = "aov"
                                     )
                                 },
                                 
                                 .anova_mixed = function(args) {
                                     if (!requireNamespace("lme4", quietly = TRUE)) {
                                         stop("lme4 package required for mixed models.")
                                     }
                                     if (!requireNamespace("lmerTest", quietly = TRUE)) {
                                         warning("lmerTest not installed; using lme4 without p-values.")
                                     }
                                     # 使用 lmer 或 lmerTest
                                     model <- if (requireNamespace("lmerTest", quietly = TRUE)) {
                                         lmerTest::lmer(args$formula, data = args$data, REML = (args$method == "REML"), ...)
                                     } else {
                                         lme4::lmer(args$formula, data = args$data, REML = (args$method == "REML"), ...)
                                     }
                                     anova_table <- if (inherits(model, "lmerModLmerTest")) {
                                         as.data.frame(lmerTest::anova(model))
                                     } else {
                                         # 使用 lme4 的 anova 近似
                                         as.data.frame(stats::anova(model))
                                     }
                                     # 随机效应方差
                                     var_rand <- as.data.frame(lme4::VarCorr(model))
                                     # 固定效应系数
                                     coef_fixed <- lme4::fixef(model)
                                     # 多重比较（针对固定因子）
                                     factors <- labels(stats::terms(model))
                                     comp <- NULL
                                     if (length(factors) > 0) {
                                         comp <- tryCatch(.multiple_comparisons(model, factors[1], method = "tukey"), error = function(e) NULL)
                                     }
                                     
                                     list(
                                         test_type = "Linear Mixed Model",
                                         formula = deparse(args$formula),
                                         data_name = deparse(substitute(args$data)),
                                         model = model,
                                         anova_table = anova_table,
                                         fixed_effects = coef_fixed,
                                         random_effects = var_rand,
                                         multiple_comparisons = comp,
                                         residuals = residuals(model),
                                         fitted = fitted(model),
                                         n = length(residuals(model)),
                                         method = "lmer"
                                     )
                                 },
                                 
                                 .manova = function(args) {
                                     model <- stats::manova(args$formula, data = args$data)
                                     summary_manova <- summary(model, test = args$test)
                                     summary_manova_aov <- summary(model)
                                     
                                     list(
                                         test_type = "MANOVA",
                                         formula = deparse(args$formula),
                                         data_name = deparse(substitute(args$data)),
                                         model = model,
                                         summary = summary_manova,
                                         test_statistic = args$test,
                                         coefficients = coef(model),
                                         residuals = residuals(model),
                                         fitted = fitted(model),
                                         n = length(residuals(model)),
                                         method = "manova"
                                     )
                                 }
                             )
)


# =============================================================================
# 文件: R/anova/AnovaPlotter.R
# 描述: ANOVA 图形协调器（优化版 - 最大化复用 .plot 包现有函数）
# =============================================================================

#' @title AnovaPlotter: ANOVA 绘图协调器
#' @description
#' 基于 .plot 包现有函数构建的 ANOVA 图形协调器。
#' 最大化复用现有函数，减少重复代码。
#'
#' @export
AnovaPlotter <- R6::R6Class("AnovaPlotter",
                            public = list(
                                theme_obj = NULL,
                                
                                #' @description 初始化
                                #' @param theme 主题名称或 IqrTheme 对象
                                initialize = function(theme = "academic") {
                                    if (inherits(theme, "IqrTheme")) {
                                        self$theme_obj <- theme
                                    } else {
                                        tryCatch({
                                            self$theme_obj <- IqrTheme$new(theme)
                                        }, error = function(e) {
                                            self$theme_obj <- NULL
                                        })
                                    }
                                },
                                
                                #' @description 自动绘图
                                #' @param result ANOVA 结果列表
                                #' @param plot_type 图形类型
                                #' @param ... 其他参数
                                #' @return ggplot2 或 patchwork 对象
                                plot = function(result, plot_type = "auto", ...) {
                                    plot_type <- match.arg(plot_type, c("auto", "residual", "effects", "interaction",
                                                                        "comparison", "variance", "f_curve", "summary"))
                                    if (plot_type == "auto") {
                                        plot_type <- private$.auto_select(result)
                                    }
                                    switch(plot_type,
                                           "residual"   = self$plot_residuals(result, ...),
                                           "effects"    = self$plot_effects(result, ...),
                                           "interaction"= self$plot_interaction(result, ...),
                                           "comparison" = self$plot_comparison(result, ...),
                                           "variance"   = self$plot_variance(result, ...),
                                           "f_curve"    = self$plot_f_curve(result, ...),
                                           "summary"    = self$plot_summary(result, ...)
                                    )
                                },
                                
                                # ========================================================================
                                # 1. 残差诊断图 —— 复用 plot_qq + 新增组件
                                # ========================================================================
                                
                                #' @description 残差诊断四合一图
                                #' @param result ANOVA 结果
                                #' @param add_qq 是否包含 QQ 图
                                #' @return patchwork 对象
                                plot_residuals = function(result, add_qq = TRUE, ...) {
                                    if (!requireNamespace("patchwork", quietly = TRUE)) {
                                        stop("patchwork package required.")
                                    }
                                    
                                    model <- result$model
                                    res <- residuals(model)
                                    fitted <- fitted(model)
                                    
                                    # 检查是否使用 .plot 包中的 plot_qq 函数
                                    if (add_qq && exists("plot_qq", where = asNamespace("iQualityR.plot"))) {
                                        p_qq <- iQualityR.plot::plot_qq(
                                            data = data.frame(res = res),
                                            sample_col = "res",
                                            dist_family = "norm",
                                            theme = self$theme_obj,
                                            add_test = TRUE
                                        ) + ggplot2::labs(title = "Normal Q-Q Plot")
                                    } else {
                                        # 后备：使用基础 R 或简单 ggplot
                                        p_qq <- ggplot2::ggplot(data.frame(res = res), ggplot2::aes(sample = res)) +
                                            ggplot2::stat_qq() +
                                            ggplot2::stat_qq_line(color = "red") +
                                            ggplot2::labs(title = "Normal Q-Q Plot") +
                                            as_iqr_theme(self$theme_obj)
                                    }
                                    
                                    # 残差 vs 拟合值
                                    df_rf <- data.frame(fitted = fitted, residual = res)
                                    p_rf <- base_plot(df_rf, ggplot2::aes(x = fitted, y = residual), theme = self$theme_obj) +
                                        ggplot2::geom_point(alpha = 0.6) +
                                        ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
                                        ggplot2::geom_smooth(method = "loess", se = TRUE, color = "blue", alpha = 0.2) +
                                        ggplot2::labs(x = "Fitted Values", y = "Residuals", title = "Residuals vs Fitted")
                                    
                                    # Scale-Location 图
                                    df_sl <- data.frame(fitted = fitted, sqrt_res = sqrt(abs(scale(res))))
                                    p_sl <- base_plot(df_sl, ggplot2::aes(x = fitted, y = sqrt_res), theme = self$theme_obj) +
                                        ggplot2::geom_point(alpha = 0.6) +
                                        ggplot2::geom_smooth(method = "loess", se = TRUE, color = "blue", alpha = 0.2) +
                                        ggplot2::labs(x = "Fitted Values", y = "√|Standardized Residuals|",
                                                      title = "Scale-Location")
                                    
                                    # 残差 vs 因子（如果是单因素 ANOVA）
                                    if (length(result$factors) == 1 && "model" %in% names(result)) {
                                        df <- result$model$model
                                        factor_name <- result$factors[1]
                                        df_plot <- data.frame(factor = df[[factor_name]], residual = res)
                                        p_factor <- base_plot(df_plot, ggplot2::aes(x = factor, y = residual), theme = self$theme_obj) +
                                            layers_boxplot(add_jitter = TRUE, boxplot_args = list(fill = "steelblue", alpha = 0.3)) +
                                            ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
                                            ggplot2::labs(x = factor_name, y = "Residuals", title = "Residuals by Factor")
                                    } else {
                                        # 残差 vs 序号（时间顺序）
                                        df_seq <- data.frame(index = seq_along(res), residual = res)
                                        p_factor <- base_plot(df_seq, ggplot2::aes(x = index, y = residual), theme = self$theme_obj) +
                                            ggplot2::geom_point(alpha = 0.6) +
                                            ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
                                            ggplot2::labs(x = "Observation Order", y = "Residuals", title = "Residuals vs Order")
                                    }
                                    
                                    # 组合
                                    p <- (p_rf + p_qq) / (p_sl + p_factor) +
                                        patchwork::plot_annotation(
                                            title = "Residual Diagnostic Plots",
                                            subtitle = sprintf("ANOVA: %s", result$formula %||% ""),
                                            theme = as_iqr_theme(self$theme_obj)
                                        )
                                    p
                                },
                                
                                # ========================================================================
                                # 2. 主效应图 —— 直接复用 plot_scatter_basic
                                # ========================================================================
                                
                                #' @description 主效应图（均值 ± SE）
                                #' @param result ANOVA 结果
                                #' @param factor 因子名
                                #' @param show_table 是否显示统计表
                                #' @return ggplot2 对象
                                plot_effects = function(result, factor = NULL, show_table = FALSE, ...) {
                                    if (is.null(factor)) factor <- result$factors[1]
                                    if (is.null(factor)) stop("No factor available for effects plot.")
                                    
                                    # 提取均值数据
                                    df <- result$model$model
                                    y_var <- names(df)[1]
                                    y <- df[[1]]
                                    x <- df[[factor]]
                                    
                                    means <- tapply(y, x, mean)
                                    se <- tapply(y, x, function(x) sd(x)/sqrt(length(x)))
                                    n <- tapply(y, x, length)
                                    
                                    df_plot <- data.frame(
                                        group = factor(names(means), levels = names(means)),
                                        mean = as.numeric(means),
                                        se = as.numeric(se),
                                        n = as.numeric(n)
                                    )
                                    
                                    # 使用 .plot 包中的 plot_scatter_basic（如果可用）
                                    if (exists("plot_scatter_basic", where = asNamespace("iQualityR.plot"))) {
                                        p <- iQualityR.plot::plot_scatter_basic(
                                            data = df_plot,
                                            x_var = "group",
                                            y_var = "mean",
                                            add_regression = FALSE,
                                            add_correlation = FALSE,
                                            theme = self$theme_obj,
                                            title = sprintf("Main Effects Plot: %s", factor),
                                            subtitle = sprintf("Means ± 1 SE (n = %s)", paste(df_plot$n, collapse = ", "))
                                        )
                                    } else {
                                        # 后备方案
                                        p <- base_plot(df_plot, ggplot2::aes(x = group, y = mean), theme = self$theme_obj) +
                                            ggplot2::geom_point(size = 3, color = "steelblue") +
                                            ggplot2::geom_errorbar(
                                                ggplot2::aes(ymin = mean - se, ymax = mean + se),
                                                width = 0.15, color = "steelblue", linewidth = 1
                                            ) +
                                            ggplot2::labs(x = factor, y = "Mean Response", title = sprintf("Main Effects Plot: %s", factor))
                                    }
                                    
                                    # 可选：添加统计表
                                    if (show_table) {
                                        if (exists("create_stat_table", where = asNamespace("iQualityR.plot"))) {
                                            table_grob <- iQualityR.plot::create_stat_table(df_plot, theme = self$theme_obj)
                                        } else {
                                            table_grob <- gridExtra::tableGrob(df_plot)
                                        }
                                        p <- p + ggplot2::annotation_custom(
                                            grob = table_grob,
                                            xmin = Inf, xmax = Inf,
                                            ymin = -Inf, ymax = -Inf
                                        )
                                    }
                                    
                                    p
                                },
                                
                                # ========================================================================
                                # 3. 交互效应图 —— 直接复用 plot_interaction_line
                                # ========================================================================
                                
                                #' @description 交互效应图
                                #' @param result ANOVA 结果
                                #' @param factor1 因子1
                                #' @param factor2 因子2
                                #' @return ggplot2 对象
                                plot_interaction = function(result, factor1 = NULL, factor2 = NULL, ...) {
                                    if (is.null(factor1)) factor1 <- result$factors[1]
                                    if (is.null(factor2)) factor2 <- result$factors[2]
                                    
                                    if (is.null(factor1) || is.null(factor2)) {
                                        stop("Two factors required for interaction plot.")
                                    }
                                    
                                    # 使用 .plot 包中的 plot_interaction_line（完美复用）
                                    if (exists("plot_interaction_line", where = asNamespace("iQualityR.plot"))) {
                                        p <- iQualityR.plot::plot_interaction_line(
                                            data = result$model$model,
                                            x_var = factor1,
                                            y_var = names(result$model$model)[1],
                                            group_var = factor2,
                                            fun = "mean",
                                            theme = self$theme_obj
                                        ) + ggplot2::labs(title = sprintf("Interaction Plot: %s × %s", factor1, factor2))
                                    } else {
                                        # 后备方案
                                        df <- result$model$model
                                        y <- df[[1]]
                                        x1 <- df[[factor1]]
                                        x2 <- df[[factor2]]
                                        means <- tapply(y, list(x1, x2), mean)
                                        df_plot <- as.data.frame.table(means)
                                        names(df_plot) <- c(factor1, factor2, "mean")
                                        
                                        p <- base_plot(df_plot, ggplot2::aes(x = .data[[factor1]], y = mean,
                                                                             color = .data[[factor2]], group = .data[[factor2]]),
                                                       theme = self$theme_obj) +
                                            ggplot2::geom_line(linewidth = 1.2) +
                                            ggplot2::geom_point(size = 3) +
                                            ggplot2::labs(y = "Mean Response", title = sprintf("Interaction Plot: %s × %s", factor1, factor2))
                                    }
                                    
                                    p
                                },
                                
                                # ========================================================================
                                # 4. 多重比较图 —— 新增（森林图风格）
                                # ========================================================================
                                
                                #' @description 多重比较置信区间图（森林图风格）
                                #' @param result ANOVA 结果
                                #' @param factor 因子名
                                #' @param alpha 显著性水平
                                #' @return ggplot2 对象
                                plot_comparison = function(result, factor = NULL, alpha = 0.05, ...) {
                                    if (is.null(factor)) factor <- result$factors[1]
                                    if (is.null(result$multiple_comparisons)) {
                                        # 如果没有多重比较结果，尝试用 emmeans 计算
                                        if (requireNamespace("emmeans", quietly = TRUE)) {
                                            emm <- emmeans::emmeans(result$model, as.formula(paste("~", factor)))
                                            comp <- summary(emmeans::contrast(emm, method = "pairwise"), adjust = "tukey")
                                        } else {
                                            stop("No multiple comparisons available. Install 'emmeans' package.")
                                        }
                                    } else {
                                        comp <- result$multiple_comparisons
                                    }
                                    
                                    # 提取对比数据
                                    if (inherits(comp, "summary_em")) {
                                        df_plot <- data.frame(
                                            contrast = comp$contrast,
                                            estimate = comp$estimate,
                                            se = comp$SE,
                                            lower = comp$lower.CL,
                                            upper = comp$upper.CL,
                                            p_value = comp$p.value,
                                            significant = comp$p.value < alpha
                                        )
                                    } else if (is.data.frame(comp)) {
                                        # 尝试自动映射
                                        df_plot <- comp
                                        if (!all(c("estimate", "lower", "upper") %in% names(df_plot))) {
                                            stop("Cannot parse comparison result format.")
                                        }
                                    } else {
                                        stop("Unsupported comparison result format.")
                                    }
                                    
                                    # 排序（按估计值）
                                    df_plot <- df_plot[order(df_plot$estimate), ]
                                    df_plot$contrast <- factor(df_plot$contrast, levels = df_plot$contrast)
                                    
                                    # 森林图
                                    p <- base_plot(df_plot, ggplot2::aes(x = contrast, y = estimate), theme = self$theme_obj) +
                                        ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
                                        ggplot2::geom_errorbar(
                                            ggplot2::aes(ymin = lower, ymax = upper, color = significant),
                                            width = 0.2, linewidth = 1
                                        ) +
                                        ggplot2::geom_point(
                                            ggplot2::aes(color = significant, size = -log10(p_value + 1e-10)),
                                            shape = 18
                                        ) +
                                        ggplot2::scale_color_manual(
                                            values = c("TRUE" = "red", "FALSE" = "steelblue"),
                                            name = "Significant",
                                            labels = c("TRUE" = "p < alpha", "FALSE" = "p >= alpha")
                                        ) +
                                        ggplot2::scale_size_continuous(
                                            range = c(2, 6),
                                            name = "-log10(p-value)"
                                        ) +
                                        ggplot2::coord_flip() +
                                        ggplot2::labs(
                                            x = "Comparison",
                                            y = "Estimate (Difference)",
                                            title = sprintf("Multiple Comparisons: %s", factor),
                                            subtitle = sprintf("α = %.2f, method: Tukey HSD", alpha)
                                        )
                                    
                                    p
                                },
                                
                                # ========================================================================
                                # 5. 方差成分图 —— 直接复用 plot_variance_components
                                # ========================================================================
                                
                                #' @description 方差成分图（用于随机效应/混合模型）
                                #' @param result ANOVA 结果
                                #' @return ggplot2 对象
                                plot_variance = function(result, ...) {
                                    # 检查是否是混合模型
                                    if (!inherits(result$model, "lmerMod")) {
                                        stop("Variance components plot is only available for mixed models (lmer).")
                                    }
                                    
                                    # 提取方差成分
                                    var_comp <- as.data.frame(lme4::VarCorr(result$model))
                                    var_comp_df <- data.frame(
                                        source = var_comp$grp,
                                        variance_percent = var_comp$vcov / sum(var_comp$vcov) * 100
                                    )
                                    
                                    # 使用 .plot 包中的 plot_variance_components
                                    if (exists("plot_variance_components", where = asNamespace("iQualityR.plot"))) {
                                        p <- iQualityR.plot::plot_variance_components(
                                            data = var_comp_df,
                                            theme = self$theme_obj,
                                            sort_by_variance = TRUE
                                        )
                                    } else {
                                        # 后备方案：简单条形图
                                        p <- base_plot(var_comp_df, ggplot2::aes(x = source, y = variance_percent, fill = source),
                                                       theme = self$theme_obj) +
                                            ggplot2::geom_col() +
                                            ggplot2::geom_text(ggplot2::aes(label = sprintf("%.1f%%", variance_percent)),
                                                               vjust = -0.5) +
                                            ggplot2::labs(x = "Variance Source", y = "Variance (%)",
                                                          title = "Variance Components")
                                    }
                                    
                                    p
                                },
                                
                                # ========================================================================
                                # 6. F 分布拒绝域图 —— 借鉴 plot_hypothesis_curve
                                # ========================================================================
                                
                                #' @description F 分布拒绝域图（ANOVA 全局 F 检验）
                                #' @param result ANOVA 结果
                                #' @param alpha 显著性水平
                                #' @return ggplot2 对象
                                plot_f_curve = function(result, alpha = 0.05, ...) {
                                    # 从结果中提取 F 统计量和自由度
                                    if (!is.null(result$anova_table)) {
                                        f_stat <- result$anova_table[1, "F value"]
                                        df1 <- result$anova_table[1, "Df"]
                                        df2 <- result$anova_table[2, "Df"]
                                    } else if (!is.null(result$summary)) {
                                        f_summary <- summary(result$model)
                                        f_stat <- f_summary$fstatistic[1]
                                        df1 <- f_summary$fstatistic[2]
                                        df2 <- f_summary$fstatistic[3]
                                    } else {
                                        stop("Cannot extract F statistic from result.")
                                    }
                                    
                                    # 计算临界值
                                    crit <- qf(1 - alpha, df1, df2)
                                    
                                    # 构建 F 分布曲线数据
                                    x_max <- max(crit * 1.5, f_stat * 1.3)
                                    x_seq <- seq(0, x_max, length.out = 1000)
                                    y_seq <- df(x_seq, df1, df2)
                                    
                                    df_curve <- data.frame(x = x_seq, y = y_seq)
                                    df_reject <- data.frame(x = x_seq[x_seq >= crit], y = y_seq[x_seq >= crit])
                                    
                                    p <- base_plot(df_curve, ggplot2::aes(x = x, y = y), theme = self$theme_obj) +
                                        ggplot2::geom_line(color = "steelblue", linewidth = 1.2) +
                                        ggplot2::geom_ribbon(
                                            data = df_reject,
                                            ggplot2::aes(x = x, ymin = 0, ymax = y),
                                            fill = "red", alpha = 0.3
                                        ) +
                                        ggplot2::geom_vline(xintercept = f_stat, color = "blue", linewidth = 1.2) +
                                        ggplot2::geom_vline(xintercept = crit, color = "gray50", linetype = "dashed") +
                                        ggplot2::annotate("text", x = f_stat, y = df(f_stat, df1, df2) * 1.1,
                                                          label = sprintf("F = %.2f", f_stat), color = "blue") +
                                        ggplot2::annotate("text", x = crit, y = df(crit, df1, df2) * 1.2,
                                                          label = sprintf("Critical = %.2f", crit), color = "gray50") +
                                        ggplot2::annotate("text", x = x_max * 0.7, y = max(y_seq) * 0.3,
                                                          label = sprintf("p = %s", .format_p_value(result$anova_table[1, "Pr(>F)"])),
                                                          size = 4) +
                                        ggplot2::labs(
                                            x = "F-value",
                                            y = "Density",
                                            title = "F-distribution: ANOVA Global Test",
                                            subtitle = sprintf("df1 = %.0f, df2 = %.0f, α = %.2f", df1, df2, alpha)
                                        )
                                    
                                    p
                                },
                                
                                # ========================================================================
                                # 7. 组合摘要图 —— 复用 combine_plots
                                # ========================================================================
                                
                                #' @description 组合摘要图（效应图 + 残差 + 表格）
                                #' @param result ANOVA 结果
                                #' @param layout 布局 ("2x2", "1x3")
                                #' @return patchwork 对象
                                plot_summary = function(result, layout = "2x2", ...) {
                                    if (!requireNamespace("patchwork", quietly = TRUE)) {
                                        stop("patchwork package required for summary plot.")
                                    }
                                    
                                    # 构建各个子图
                                    p1 <- self$plot_effects(result, ...)
                                    p2 <- tryCatch(self$plot_residuals(result, add_qq = TRUE, ...),
                                                   error = function(e) NULL)
                                    
                                    # 如果有交互项，添加交互图
                                    if (length(result$factors) >= 2) {
                                        p3 <- self$plot_interaction(result, ...)
                                    } else {
                                        p3 <- NULL
                                    }
                                    
                                    # 添加 ANOVA 表
                                    if (exists("create_anova_table", where = asNamespace("iQualityR.plot"))) {
                                        table_grob <- iQualityR.plot::create_anova_table(result$anova_table, theme = self$theme_obj)
                                        p4 <- ggplot2::ggplot() +
                                            ggplot2::annotation_custom(table_grob, xmin = 0, xmax = 1, ymin = 0, ymax = 1) +
                                            ggplot2::theme_void()
                                    } else {
                                        # 后备：使用 gridExtra
                                        table_grob <- gridExtra::tableGrob(
                                            round(result$anova_table, 4),
                                            theme = gridExtra::ttheme_default(base_size = 10)
                                        )
                                        p4 <- ggplot2::ggplot() +
                                            ggplot2::annotation_custom(table_grob, xmin = 0, xmax = 1, ymin = 0, ymax = 1) +
                                            ggplot2::theme_void()
                                    }
                                    
                                    # 组合
                                    if (layout == "2x2") {
                                        plots <- list(p1, p3 %||% p2, p2, p4)
                                        p <- patchwork::wrap_plots(plots, ncol = 2) +
                                            patchwork::plot_annotation(
                                                title = "ANOVA Summary",
                                                subtitle = sprintf("Model: %s", result$formula %||% ""),
                                                theme = as_iqr_theme(self$theme_obj)
                                            )
                                    } else {
                                        # 1x3 布局
                                        plots <- list(p1, p3 %||% p2, p4)
                                        p <- patchwork::wrap_plots(plots, ncol = 3) +
                                            patchwork::plot_annotation(
                                                title = "ANOVA Summary",
                                                subtitle = sprintf("Model: %s", result$formula %||% ""),
                                                theme = as_iqr_theme(self$theme_obj)
                                            )
                                    }
                                    
                                    p
                                },
                                
                                #' @description 设置主题
                                #' @param theme 主题名称或 IqrTheme 对象
                                set_theme = function(theme) {
                                    if (inherits(theme, "IqrTheme")) {
                                        self$theme_obj <- theme
                                    } else if (is.character(theme)) {
                                        tryCatch({
                                            self$theme_obj <- IqrTheme$new(theme)
                                        }, error = function(e) {
                                            warning("Invalid theme name, keeping existing theme.")
                                        })
                                    }
                                    invisible(self)
                                }
                            ),
                            
                            private = list(
                                .auto_select = function(result) {
                                    # 根据结果自动选择图形类型
                                    if (inherits(result$model, "lmerMod")) {
                                        return("variance")
                                    }
                                    if (length(result$factors) >= 2) {
                                        return("interaction")
                                    }
                                    if (!is.null(result$multiple_comparisons)) {
                                        return("comparison")
                                    }
                                    return("effects")
                                }
                            )
)

# =============================================================================
# 文件: R/anova/AnovaReporter.R
# 描述: ANOVA 报告引擎（复用 .core 的 IqrReporter 框架）
# =============================================================================

#' @title AnovaReporter: ANOVA 报告输出引擎
#' @description
#' 继承自 IqrReporter，为 ANOVA 模块提供标准化的报告生成能力。
#' 支持 Excel、HTML、PDF、Word 和 PowerPoint 格式。
#'
#' @export
AnovaReporter <- R6::R6Class("AnovaReporter",
                             inherit = IqrReporter,
                             
                             public = list(
                                 #' @description 初始化 ANOVA 报告器
                                 #' @param theme_obj IqrTheme 对象
                                 initialize = function(theme_obj) {
                                     super$initialize(theme_obj)
                                     # 注册 ANOVA 任务模板（Rmd 模板和 Excel 生成器）
                                     self$register(
                                         task_tag = "anova",
                                         rmd_template = system.file("templates", "anova_template.Rmd",
                                                                    package = "iQualityR.stat"),
                                         excel_generator = function(results, plan) {
                                             anova_to_excel_data(results, plan)
                                         }
                                     )
                                     invisible(self)
                                 },
                                 
                                 #' @description 生成 ANOVA 报告（便捷入口）
                                 #' @param results ANOVA 结果列表（来自 AnovaAnalyzer）
                                 #' @param plan 计划对象（可选）
                                 #' @param format 输出格式（"excel", "html", "pdf", "docx", "pptx"）
                                 #' @param path 输出路径（可选）
                                 #' @param ... 其他参数传递给底层 export 方法
                                 #' @return 输出文件路径（不可见）
                                 report = function(results, plan = NULL, format = "excel", path = NULL, ...) {
                                     self$export(
                                         results = results,
                                         plan = plan,
                                         task_tag = "anova",
                                         format = format,
                                         path = path,
                                         ...
                                     )
                                 }
                             )
)

# =============================================================================
# 文件: R/anova/anova_to_excel_data.R
# 描述: 将 ANOVA 结果转换为 Excel 导出的数据列表
# =============================================================================

#' 将 ANOVA 结果转换为 Excel 数据列表
#' @param results AnovaAnalyzer 返回的结果列表
#' @param plan 可选计划对象（预留）
#' @return 命名列表，每个元素是一个数据框，对应一个 Excel 工作表
#' @keywords internal
anova_to_excel_data <- function(results, plan = NULL) {
    sheets <- list()
    
    # 1. ANOVA 主表
    if (!is.null(results$anova_table)) {
        sheets[["ANOVA"]] <- cbind(
            Source = rownames(results$anova_table),
            results$anova_table
        )
    } else if (!is.null(results$anova_tables)) {
        # 重复测量 ANOVA 可能有多个表
        for (nm in names(results$anova_tables)) {
            sheets[[paste0("ANOVA_", nm)]] <- cbind(
                Source = rownames(results$anova_tables[[nm]]),
                results$anova_tables[[nm]]
            )
        }
    }
    
    # 2. 模型摘要（R²、调整 R²、系数等）
    if (!is.null(results$summary)) {
        # 提取 F 统计量、R² 等
        summ <- summary(results$model)
        if (!is.null(summ$r.squared)) {
            model_stats <- data.frame(
                Metric = c("R-squared", "Adj R-squared", "Residual SE", "F-statistic", "F p-value"),
                Value = c(
                    round(summ$r.squared, 4),
                    round(summ$adj.r.squared, 4),
                    round(summ$sigma, 4),
                    if (!is.null(summ$fstatistic)) round(summ$fstatistic[1], 2) else NA,
                    if (!is.null(summ$fstatistic)) format.pval(pf(summ$fstatistic[1], summ$fstatistic[2], summ$fstatistic[3], lower.tail = FALSE), digits = 4) else NA
                )
            )
            sheets[["Model_Summary"]] <- model_stats
        }
    }
    
    # 3. 系数表（固定效应）
    if (!is.null(results$coefficients)) {
        coef_df <- as.data.frame(results$coefficients)
        if (ncol(coef_df) > 0) {
            coef_df <- cbind(Term = rownames(coef_df), coef_df)
            sheets[["Coefficients"]] <- coef_df
        }
    }
    
    # 4. 多重比较（如果存在）
    if (!is.null(results$multiple_comparisons)) {
        if (is.data.frame(results$multiple_comparisons)) {
            sheets[["Multiple_Comparisons"]] <- results$multiple_comparisons
        } else if (is.list(results$multiple_comparisons)) {
            # 可能按因子分组的列表
            for (nm in names(results$multiple_comparisons)) {
                df <- results$multiple_comparisons[[nm]]
                if (is.data.frame(df)) {
                    sheets[[paste0("MC_", nm)]] <- df
                }
            }
        }
    }
    
    # 5. 效应量
    if (!is.null(results$effect_size)) {
        eff_df <- data.frame()
        if (!is.null(results$effect_size$eta)) {
            eff_df <- rbind(eff_df, data.frame(
                Effect = names(results$effect_size$eta),
                Eta_Squared = round(results$effect_size$eta, 4),
                stringsAsFactors = FALSE
            ))
        }
        if (!is.null(results$effect_size$partial_eta)) {
            eff_df <- rbind(eff_df, data.frame(
                Effect = names(results$effect_size$partial_eta),
                Partial_Eta_Squared = round(results$effect_size$partial_eta, 4),
                stringsAsFactors = FALSE
            ))
        }
        if (nrow(eff_df) > 0) sheets[["Effect_Sizes"]] <- eff_df
    }
    
    # 6. 诊断数据（残差、拟合值等）
    if (!is.null(results$residuals)) {
        diag_df <- data.frame(
            Residual = results$residuals,
            Fitted = results$fitted,
            stringsAsFactors = FALSE
        )
        if (!is.null(results$n) && length(diag_df$Residual) == results$n) {
            diag_df$Index <- seq_along(diag_df$Residual)
            sheets[["Diagnostics"]] <- diag_df
        }
    }
    
    # 7. 原始数据（如果包含在结果中）
    if (!is.null(results$data)) {
        sheets[["Data"]] <- results$data
    }
    
    sheets
}
# =============================================================================
# 文件: iQualityR.stat/R/anova/iqr_anova.R
# 描述: ANOVA 用户入口类 + 便捷函数
# =============================================================================

# ---- iqr_anova R6 类 ----
iqr_anova <- R6::R6Class("iqr_anova",
                         public = list(
                             # ... 类定义 ...
                         )
)

# ---- 便捷函数（在文件末尾） ----

#' @title 便捷 ANOVA 分析函数
#' @description
#' 无需创建 R6 对象，直接执行 ANOVA 分析。
#' 适合快速分析和探索性场景。
#'
#' @param formula 模型公式
#' @param data 数据框
#' @param ... 其他参数传给 AnovaAnalyzer
#' @param plot 是否自动绘图
#' @param plot_type 图形类型
#' @param interpret 是否输出解释
#' @param audience 受众级别
#' @param theme 主题名称
#'
#' @return 分析结果列表（invisible）
#' @export
#'
#' @examples
#' result <- anova_run(Adhesion ~ PaintType * Pressure, data = painting_data)
#' result <- anova_run(Strength ~ Supplier, data = supplier_data, plot = TRUE)
anova_run <- function(formula, data, ..., plot = FALSE, plot_type = "auto",
                      interpret = FALSE, audience = "manager", theme = "academic") {
    obj <- iqr_anova$new(theme = theme)
    obj$run(formula, data, ..., plot = plot, plot_type = plot_type,
            interpret = interpret, audience = audience)
    invisible(obj$last_results)
}
# =============================================================================
# 文件: R/anova/iqr_anova.R（追加部分）
# =============================================================================

#' @title 生成 ANOVA 报告（便捷函数）
#' @param results ANOVA 结果列表
#' @param format 输出格式
#' @param path 输出路径
#' @param theme 主题
#' @param ... 其他参数传给 AnovaReporter
#' @export
anova_report <- function(results, format = "excel", path = NULL,
                         theme = "academic", ...) {
    theme_obj <- if (inherits(theme, "IqrTheme")) theme else IqrTheme$new(theme)
    reporter <- AnovaReporter$new(theme_obj)
    reporter$report(results, format = format, path = path, ...)
}




