# =============================================================================
# File: R/zzz.R
# Title: Package-level Imports and Setup
# =============================================================================

#' @title iQualityR.stat package
#' @description
#' Statistical Foundation for iQualityR. Provides foundational statistical
#' tools including descriptive statistics, probability distributions,
#' standard deviation estimation, SPC control chart constants, and
#' statistical result interpretation.
#'
#' @importFrom R6 R6Class
#' @importFrom ggplot2 ggplot aes geom_point geom_line geom_histogram geom_density
#' @importFrom ggplot2 geom_boxplot geom_vline geom_hline geom_text geom_label
#' @importFrom ggplot2 geom_segment geom_col geom_smooth geom_ribbon geom_tile
#' @importFrom ggplot2 geom_violin geom_errorbar geom_jitter geom_abline
#' @importFrom ggplot2 stat_qq stat_qq_line stat_density_2d geom_bin2d geom_hex
#' @importFrom ggplot2 scale_color_manual scale_fill_manual scale_fill_gradient2
#' @importFrom ggplot2 scale_color_brewer scale_fill_brewer scale_size
#' @importFrom ggplot2 scale_x_continuous scale_y_continuous scale_x_discrete
#' @importFrom ggplot2 scale_y_discrete sec_axis coord_flip coord_fixed
#' @importFrom ggplot2 coord_cartesian labs theme element_text element_blank
#' @importFrom ggplot2 element_rect element_line rel margin annotate arrow
#' @importFrom ggplot2 after_stat facet_wrap facet_grid annotation_custom
#' @importFrom ggplot2 stat_function expand_limits guides guide_legend ggsave
#' @importFrom stats ks.test shapiro.test sd var cor median quantile residuals
#' @importFrom stats coef fitted confint predict pnorm qnorm dnorm
#' @importFrom stats pt qt dt pf qf df pcauchy qcauchy dcauchy
#' @importFrom stats pgamma qgamma dgamma pbeta qbeta dbeta plnorm qlnorm dlnorm
#' @importFrom stats ppois qpois dpois pexp qexp dexp pweibull qweibull dweibull
#' @importFrom stats pnbinom qnbinom dnbinom pchisq qchisq dchisq
#' @importFrom stats binom.test prop.test t.test cor.test var.test wilcox.test
#' @importFrom stats kruskal.test friedman.test aov lm glm nls optim
#' @importFrom stats approx spline drop1 update formula terms anova sigma
#' @importFrom stats rnorm rexp rweibull rgamma rbeta rlnorm rpois rnbinom
#' @importFrom stats rchisq rcauchy runif rt rf
#' @importFrom stats IQR na.omit na.exclude complete.cases
#' @importFrom stats qbinom pbinom dbinom qlogis plogis dlogis
#' @importFrom stats qtukey ptukey integrate uniroot
#' @importFrom stats mad cooks.distance hatvalues fitted.values model.matrix
#' @importFrom nortest ad.test cvm.test lillie.test pearson.test sf.test
#' @importFrom moments skewness kurtosis
#' @importFrom patchwork plot_annotation wrap_plots wrap_elements plot_layout area
#' @importFrom ggrepel geom_text_repel geom_label_repel
#' @importFrom openxlsx createWorkbook addWorksheet writeData saveWorkbook
#' @importFrom openxlsx createStyle addStyle
#' @importFrom gridExtra tableGrob ttheme_default grid.arrange
#' @importFrom grid unit viewport grid.text grid.rect grid.lines grid.points
#' @importFrom utils head tail str capture.output modifyList
#' @importFrom iQualityR.core IqrTheme iqr_t
#' @name iQualityR.stat-package
#' @keywords internal
NULL
