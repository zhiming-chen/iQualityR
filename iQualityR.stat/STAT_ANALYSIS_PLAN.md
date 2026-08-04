# iQualityR.stat 子包重构框架

> 版本：v2.0（架构重构版）  日期：2026-07-28
> 范围：`packages/iQualityR.stat` 全量代码
> 基础：v1.0 代码事实清单（4 路并行扫描）+ 调研草稿 16 功能域对照
> 性质：重构框架，不动代码

---

## 摘要

iQualityR.stat 当前的问题不是"缺哪个函数补哪个函数"，而是**没有一致的架构主线**：18 个 R6 类与 88 个纯函数散落在 10 个子目录，4 个模块有 R6 三件套、10 个模块只有函数，plot/report 签名各不相同，导出断链、依赖冗余、功能缺失并存。

本框架给出 **一条主线 + 三层架构 + 十个板块 + 四份契约**：

- **一条主线**：.stat 是统计计算内核，只做"计算 + 解读"，不做"绘图渲染 + 报告编排"（下沉到 .plot/.core）。
- **三层架构**：L0 原语层（无状态函数）/ L1 引擎层（R6 Analyzer，纯计算）/ L2 表现层（Plotter+Reporter+Interpreter，调 .plot/.core）/ L3 入口层（R6 整合器 + 便捷函数双套）。
- **十个功能板块**：basic / htest / distributions / anova / regression / spc-foundation / quality-metrics / sample-size / diagnostics / intervals，每板块自包含 `R/<domain>/` 目录，遵循统一架构契约。
- **四份契约**：命名契约、签名契约、返回契约、依赖方向契约，确保架构一致 + 体验一致。

多元分析 / 时间序列 / 可靠性 / MSA 不入 .stat，独立子包承载，避免内核膨胀。

---

## 第一部分 设计原则

### 原则 1：分层而治，职责单一

.stat 内部分四层，每层不可越界：

| 层 | 职责 | 不可做 |
|---|---|---|
| L0 原语 | 无状态纯函数，提供常数/注册表/验证 | 不绘图、不报告、不缓存状态 |
| L1 引擎 | R6 Analyzer，纯统计计算，返回 stat_result | 不调用 ggplot、不写文件、不 print |
| L2 表现 | Plotter/Reporter/Interpreter，调 .plot 与 .core | 不重做统计计算 |
| L3 入口 | R6 整合器 + 便捷函数，编排 L1+L2 | 不实现新统计算法 |

### 原则 2：板块自包含，边界清晰

十个功能板块，每板块一个 `R/<domain>/` 目录，自包含：引擎 + 表现 + 入口 + 测试 + 文档。板块间只通过 L0 原语或显式 `::` 调用耦合，禁止相互私有访问。

### 原则 3：双套入口，统一契约

每板块对用户暴露**双套接口**，命名/签名/返回严格遵循四份契约：
- R6 整合器（交互式、链式、状态保持）
- 便捷函数（脚本式、管道、单次调用）

### 原则 4：内核不膨胀，领域独立子包

.stat 只承载"基础统计 + 质量度量 + SPC 基础设施"。多元分析、时间序列、可靠性、MSA、DOE 各自独立子包，.stat 只向它们提供原语。

### 原则 5：依赖单向，只向下

`.core ← .stat ← {功能子包}`。.stat 反向依赖 .plot 的 9 处调用必须解除（L2 表现层改为延迟注入或上移到功能子包）。

---

## 第二部分 分层架构

### L0 原语层（Foundation Primitives）

**定位**：无状态、无副作用、可被任何层和任何子包调用的底层原语。

**包含**：
- SPC 常数：`get_d2/d3/d4/c4/c4_prime/A2/A3/B3/B4/D3/D4/E2`
- 分布注册表：`DIST_REGISTRY`、`list_available_dists`、`get_dist_info`、`register_dist`、`unregister_dist`
- 参数验证：`validate_dist_params`（从 internal 升级为正式 API，或下沉为 L1 内部）
- Sigma 估计：`sigma_estimate`、`sigma_decomposition`
- SPC 规则：`detect_spc_violations`、`check_spc_rule`、`calc_control_limits`、`summarize_spc_rules`
- 质量指标：`capability_to_ppm`、`sigma_to_ppm`、`ppm_to_sigma`、`z_bench`、`throughput_yield`、`capability_interpret`、`benchmark_compare`
- 效应量：`effect_size`（Cohen's d/g/h、η²、ω²、OR、RR）
- 区间估计原语：置信区间计算函数
- C++ 加速：`get_c4_prime_cpp`（带纯 R fallback）

**契约**：纯函数、无 R6、无 ggplot、无文件 IO、无 print。返回原子向量/list/data.frame。

### L1 引擎层（Statistical Engines）

**定位**：每板块一个 R6 Analyzer，纯统计计算，返回统一 `stat_result` 结构。

**包含 10 个 Analyzer**：

| 板块 | Analyzer | 主要方法 |
|---|---|---|
| basic | `BasicAnalyzer` | desc_calc / desc_analyze / detect_outliers_* |
| htest | `HTestAnalyzer` | z/t/prop/f/chisq 检验 + 非参数 + 等效性 |
| distributions | `DistAnalyzer` | fit_distribution / auto_fit / compare_fits / prob_calc |
| anova | `AnovaAnalyzer` | oneway/twoway/multifactor/repeated/mixed/manova |
| regression | `RegressionAnalyzer` | lm/glm/nls/logit/poisson/cox/pls/stepwise/subset |
| spc-foundation | （无独立 Analyzer，L0 原语足够） | — |
| quality-metrics | （无独立 Analyzer，L0 原语足够） | — |
| sample-size | `SampleSizeAnalyzer` | sample_size_* / calc_power / power_table |
| diagnostics | `DiagnosticAnalyzer` | diagnose_lm / test_residual_* / diagnose_multicollinearity |
| intervals | `IntervalAnalyzer` | 置信/预测/公差区间 |

**契约**：
- 无 `initialize`（除 SampleSizeAnalyzer 可能需要缓存）
- 无 `theme_obj`、无 `last_plot` 字段
- 公共方法返回 `stat_result`（见契约 3）
- 私有方法 `.<verb>` 前缀
- 不 `library()`/`requireNamespace()` 任何绘图包

### L2 表现层（Presentation）

**定位**：把 L1 的 `stat_result` 转成图/表/文案，全部通过 .plot/.core 实现，不重做计算。

**包含**：
- `StatPlotter`（统一基类，各板块继承）：`$plot(result, plot_type, theme_obj)`
- `StatReporter`（统一基类）：`$report(result, format, path, audience)`
- `StatInterpreter`（已存在，扩展到全板块）：`$interpret(result, audience)`

**契约**：
- 所有 Plotter 继承 `StatPlotter` 基类，统一 `initialize(theme)` + `theme_obj` 字段
- `$plot(result, plot_type = "auto", show_table = FALSE, theme_obj = NULL)` 四参数签名固定
- `$report(result, format = c("data.frame","console","excel"), path = NULL, audience = "manager")` 四参数签名固定
- 不调用任何统计计算函数

**关键整改**：当前 .stat 反向依赖 .plot 9 处（anova 7 + normality 2 + desc 3），全部下沉到 L2 Plotter，通过 `iQualityR.plot::` 显式调用，且 Plotter 类本身可由用户跳过（L3 入口的 `plot = FALSE` 默认）。

### L3 入口层（User Entry）

**定位**：用户唯一接触面，编排 L1 + L2，提供双套接口。

**双套接口模式**（每板块必有）：

```r
# 套路 A：R6 整合器（交互式、链式）
iqr_<domain>$new(theme = "academic")$
  run(...)$
  plot(plot_type = "auto")$
  interpret(audience = "manager")$
  report(format = "data.frame")

# 套路 B：便捷函数（脚本式、单次）
<domain>_run(...)          # 返回 stat_result
<domain>_plot(result, ...) # 返回 ggplot
<domain>_interpret(result) # 返回字符串
<domain>_report(result, format = "data.frame")
```

**契约**：
- 每板块 1 个 `iqr_<domain>` 整合器 + 4 个便捷函数（run/plot/interpret/report）
- `$run()` 返回 `stat_result`（L3 不改返回结构）
- `$plot()` / `$report()` / `$interpret()` 默认基于 `self$last_result`
- 便捷函数无状态，每次调用重新编排

---

## 第三部分 十大功能板块

每板块按统一模板描述：**定位 / 边界 / L0 原语 / L1 引擎 / L2 表现 / L3 入口 / 现状缺口 / 目标状态**。

### 板块 1：basic（基础统计）

- **定位**：描述性统计、异常值检测、数据变换。面向 EDA 第一步。
- **边界**：不做推断统计（→ htest）、不做分布拟合（→ distributions）。
- **L0**：`box_cox_transform` / `yeo_johnson_transform` / `johnson_transform` / `log_transform` / `sqrt_transform` / `reciprocal_transform` / `auto_transform` / `inverse_transform`
- **L1**：`BasicAnalyzer`（新建，封装 desc_calc/desc_analyze/detect_outliers_*）
- **L2**：`BasicPlotter`（hist/box/density/qq）、`BasicReporter`、`BasicInterpreter`
- **L3**：`iqr_basic` + `basic_run` / `basic_plot` / `basic_interpret` / `basic_report`
- **现状**：desc 10 函数 + outlier 6 函数 + transform 8 函数已存在但无 R6 架构，desc_calc 无 @examples
- **目标**：整合为三件套，保留底层函数作为 L0/L1

### 板块 2：htest（假设检验）

- **定位**：参数检验 + 非参数检验 + 等效性检验。面向"判断差异是否显著"。
- **边界**：不做 ANOVA（→ anova）、不做回归（→ regression）。
- **L0**：效应量函数（effect_size）
- **L1**：`HTestAnalyzer`（已存在，扩展）
  - 参数检验：z/t/prop/f/chisq（已有）
  - **非参数**（新增）：wilcoxon_rank_sum / wilcoxon_signed_rank / kruskal_wallis / friedman
  - **等效性**（新增）：tost_mean / tost_proportion / non_inferiority / superiority
  - **Poisson 率**（新增）：poisson_1s / poisson_2s
  - **相关检验**（新增）：cor_test_pearson / cor_test_spearman / cor_test_kendall
  - **等方差**（新增）：levene_test / bartlett_test
- **L2**：`HTestPlotter`（已有）、`HTestReporter`（已有）、`StatInterpreter`（已有）
- **L3**：`iqr_htest`（已有）+ `htest_run` / `htest_plot` / `htest_interpret` / `htest_report`
- **现状**：参数检验完整，非参数/等效性/Poisson/相关/等方差全缺
- **目标**：补全 5 类缺失检验，HTestAnalyzer 成为最完整的检验引擎

### 板块 3：distributions（概率分布）

- **定位**：分布计算 + 分布拟合 + 自动选择。面向"算概率 / 拟合数据"。
- **边界**：不做质量度量（→ quality-metrics）、不做可靠性分析（→ .reliability 子包）。
- **L0**：`DIST_REGISTRY`、`list_available_dists`、`get_dist_info`、`register_dist`、`unregister_dist`、`validate_dist_params`
- **L1**：`DistAnalyzer`（整合 ProbAnalyzer + fit_distribution）
  - 概率计算：prob / quant（已有 ProbAnalyzer）
  - 拟合：fit_distribution / auto_fit_distribution / compare_fits / empirical_distribution / calc_qq_data
- **L2**：`DistPlotter`（pdf/cdf/qq/pp）、`DistReporter`、`DistInterpreter`
- **L3**：`iqr_dist`（整合 iqr_prob）+ `dist_run` / `dist_plot` / `dist_interpret` / `dist_report`
- **现状**：ProbAnalyzer + dist_fit 两套并存，4 份 vignette 主题重叠
- **目标**：合并为单一板块，vignette 整合为 1 份

### 板块 4：anova（方差分析）

- **定位**：单/双/多因素、重复测量、混合模型、MANOVA、多重比较。
- **边界**：不做一般回归（→ regression）、不做实验设计（→ .doe）。
- **L0**：等方差检验（levene_test，与 htest 共享）、效应量 η²/ω²
- **L1**：`AnovaAnalyzer`（已存在，**导出断链必须修复**）
- **L2**：`AnovaPlotter` / `AnovaReporter` / `AnovaInterpreter`（已存在）
- **L3**：`iqr_anova` + `anova_run` / `anova_plot` / `anova_interpret` / `anova_report`（已存在但未导出）
- **现状**：6 项 @export 未进 NAMESPACE，零测试
- **目标**：P0 修复导出，补测试，补 ANOM（均值分析）

### 板块 5：regression（回归分析）

- **定位**：线性/非线性/Logistic/Poisson/Cox/PLS/逐步/子集/MARS。
- **边界**：不做 ANOVA（→ anova）、不做预测（→ .predict）。
- **L0**：无（回归无底层原语）
- **L1**：`RegressionAnalyzer`（新建）
  - 线性：lm_fit（封装 lm + diagnose_lm）
  - 非线性：nls_fit
  - Logistic：logit_fit / ordinal_logit / nominal_logit
  - Poisson：poisson_fit
  - Cox：cox_fit（依赖 survival）
  - PLS：pls_fit（依赖 pls）
  - 逐步：stepwise_fit（依赖 MASS）
  - 最佳子集：best_subset_fit（依赖 leaps）
  - MARS：mars_fit（依赖 earth）
- **L2**：`RegressionPlotter`（残差图/系数图/ROC/Lift/PR）、`RegressionReporter`、`RegressionInterpreter`
- **L3**：`iqr_regression` + `regression_run` / `regression_plot` / `regression_interpret` / `regression_report`
- **现状**：除 diagnose_lm 外全缺
- **目标**：P2 阶段逐步补全，优先 lm + Logistic + Poisson（质量预测常用）

### 板块 6：spc-foundation（SPC 基础设施）

- **定位**：控制图常数 + sigma 估计 + Nelson 规则。面向 .spc/.msa/.capa 调用。
- **边界**：不画控制图（→ .spc）、不做能力分析（→ .capa）。
- **L0**：`get_d2/d3/d4/c4/c4_prime/A2/A3/B3/B4/D3/D4/E2`、`sigma_estimate`、`sigma_decomposition`、`detect_spc_violations`、`check_spc_rule`、`calc_control_limits`、`summarize_spc_rules`、`list_spc_rules`
- **L1**：无（L0 原语足够，不需要 Analyzer）
- **L2**：无（SPC 绘图在 .spc 子包）
- **L3**：便捷函数直接暴露 L0（`get_d2(n)` 等），无 R6 整合器
- **现状**：完整，是生态热点（.spc 18 次调用、.msa 6 次、.capa 11 次）
- **目标**：保持稳定，补 C++ 纯 R fallback，补 8 个未测常数测试

### 板块 7：quality-metrics（质量指标）

- **定位**：PPM/Sigma/Yield/DPMO/可靠性基本指标/可用性/能力等级/行业基准。
- **边界**：不做完整可靠性分析（→ .reliability）、不做能力分析报告（→ .capa）。
- **L0**：`capability_to_ppm` / `sigma_to_ppm` / `ppm_to_sigma` / `z_bench` / `throughput_yield` / `yield_to_dpmo` / `dpmo_to_yield` / `reliability`（基本） / `availability` / `capability_interpret` / `benchmark_compare` / `quality_dashboard`
- **L1**：无（L0 原语足够）
- **L2**：无（仪表盘绘图在 .core/.plot）
- **L3**：便捷函数直接暴露 L0
- **现状**：完整，12 函数已导出
- **目标**：保持稳定，补 quality_dashboard 测试

### 板块 8：sample-size（样本量与功效）

- **定位**：各类检验的样本量与功效计算 + 功效曲线。
- **边界**：不做检验本身（→ htest）。
- **L0**：无
- **L1**：`SampleSizeAnalyzer`（新建，封装现有 8 函数）
  - 已有：sample_size_mean / two_means / proportion / two_proportions / anova / calc_power / effect_size / power_table
  - **新增**：sample_size_paired_t / sample_size_regression / sample_size_ci / sample_size_tolerance / sample_size_reliability
- **L2**：`SampleSizePlotter`（功效曲线）、`SampleSizeReporter`
- **L3**：`iqr_sample_size` + `sample_size_run` / `sample_size_plot` / `sample_size_interpret` / `sample_size_report`
- **现状**：8 函数已导出，无 R6，5 类样本量缺失
- **目标**：补 5 类缺失，建三件套

### 板块 9：diagnostics（模型诊断）

- **定位**：线性模型综合诊断（正态性/方差齐性/共线性/影响点/自相关）。
- **边界**：不做回归本身（→ regression）、只诊断已拟合模型。
- **L0**：无
- **L1**：`DiagnosticAnalyzer`（整合现有 6 函数）
  - diagnose_lm / test_residual_normality / test_heteroscedasticity / diagnose_multicollinearity / diagnose_influential_points / summarize_assumptions
- **L2**：`DiagnosticPlotter`（残差图/QQ/Cook's 距离）、`DiagnosticReporter`
- **L3**：`iqr_diagnostics` + `diagnostics_run` / `diagnostics_plot` / `diagnostics_interpret` / `diagnostics_report`
- **现状**：6 函数已导出，无 R6，.doe/.predict 未复用
- **目标**：建三件套，推动 .doe/.predict 复用

### 板块 10：intervals（区间估计）

- **定位**：置信区间 + 预测区间 + 公差区间 + 误差裕度。
- **边界**：不做样本量（→ sample-size）、不做检验（→ htest）。
- **L0**：区间计算原语
- **L1**：`IntervalAnalyzer`（新建）
  - 置信区间：ci_mean / ci_proportion / ci_variance / ci_diff_mean
  - 预测区间：pi_mean（封装 predict(interval="prediction")）
  - **公差区间**（新增）：tolerance_interval（封装 tolerance 包或自建）
  - **误差裕度**（新增）：margin_of_error
- **L2**：`IntervalPlotter`（区间可视化）、`IntervalReporter`
- **L3**：`iqr_intervals` + `intervals_run` / `intervals_plot` / `intervals_interpret` / `intervals_report`
- **现状**：置信区间散落在各检验中，无独立板块，公差/预测/误差裕度缺失
- **目标**：新建板块，统一区间估计入口

### 不入 .stat 的功能（独立子包）

| 领域 | 承载子包 | 理由 |
|---|---|---|
| 多元分析（PCA/因子/聚类/判别/MDS/SEM/MCA/LCA/t-SNE） | 新建 `.multivariate` | 领域独立，依赖重，避免内核膨胀 |
| 时间序列（ARIMA/季节分解/ACF/PACF/预测） | 新建 `.timeseries` 或扩展 `.predict` | 同上 |
| 可靠性完整功能（K-M/删失/加速寿命/寿命回归/Probit/Weibayes/保修） | 扩展 `.reliability` | 已有子包，集中领域逻辑 |
| MSA（量具 R&R/属性一致性） | 保留 `.msa` | 已有子包，领域特定 |
| DOE | 保留 `.doe` | 已有子包 |
| 抽样验收/OC 曲线 | 新建 `.sampling` 或放 `.capa` | 领域独立 |

.stat 只向这些子包提供 L0 原语（常数/sigma/分布拟合/质量指标）。

---

## 第四部分 四份契约

### 契约 1：命名契约

| 类别 | 规范 | 示例 |
|---|---|---|
| R6 Analyzer | `<Domain>Analyzer` | `HTestAnalyzer` / `RegressionAnalyzer` |
| R6 Plotter | `<Domain>Plotter` | `HTestPlotter` / `RegressionPlotter` |
| R6 Reporter | `<Domain>Reporter` | `HTestReporter` / `RegressionReporter` |
| R6 整合器 | `iqr_<domain>` | `iqr_htest` / `iqr_regression` |
| 便捷函数 | `<domain>_<verb>` | `htest_run` / `regression_plot` |
| L0 原语函数 | snake_case 动词 | `fit_distribution` / `get_d2` / `sigma_estimate` |
| 私有方法 | `.<verb>` | `.compute_prob` / `.test_sw` |
| 文件名 | R6 类 PascalCase，功能 snake_case | `HTestAnalyzer.R` / `dist_fit.R` |
| 目录名 | kebab-case | `R/htest/` / `R/sample-size/` |
| 测试文件 | `test-<domain>.R` | `test-htest.R` / `test-anova.R` |
| vignette | `<domain>.Rmd` | `htest.Rmd` / `regression.Rmd` |

### 契约 2：签名契约

**L1 Analyzer 方法**：
```r
<analyzer>$<test_name>(x, ..., alternative = "two.sided", conf_level = 0.95)
# 返回 stat_result（见契约 3）
```

**L2 表现层**（统一 4 参数）：
```r
<plotter>$plot(result, plot_type = "auto", show_table = FALSE, theme_obj = NULL)
<reporter>$report(result, format = c("data.frame","console","excel"), path = NULL, audience = "manager")
<interpreter>$interpret(result, audience = c("manager","technical","client"))
```

**L3 整合器**（统一 5 方法）：
```r
iqr_<domain>$new(theme = "academic")
iqr_<domain>$run(...)              # 返回 stat_result，缓存到 self$last_result
iqr_<domain>$plot(plot_type = "auto", show_table = FALSE, theme_obj = NULL)
iqr_<domain>$interpret(audience = "manager")
iqr_<domain>$report(format = "data.frame", path = NULL, audience = "manager")
```

**L3 便捷函数**（统一 4 函数）：
```r
<domain>_run(...)              # 返回 stat_result
<domain>_plot(result, ...)     # 返回 ggplot/patchwork
<domain>_interpret(result, audience = "manager")
<domain>_report(result, format = "data.frame", path = NULL)
```

### 契约 3：返回契约

**stat_result 统一结构**（所有 L1 Analyzer 返回）：
```r
list(
  domain      = "htest",              # 板块名
  test_type   = "t_test_1s",          # 检验/方法名
  method      = "One Sample t-test",  # 人类可读方法名
  statistic   = c(t = 2.34),          # 统计量（named vector）
  parameter   = c(df = 29),           # 参数（自由度等）
  p.value     = 0.026,                # p 值
  conf.int    = c(0.12, 0.88),        # 置信区间
  estimate    = c(mean = 50.3),       # 点估计
  alternative = "two.sided",          # 备择假设
  alpha       = 0.05,                 # 显著性水平
  data        = list(x = <data>),     # 原始数据（可选）
  extra       = list(...),            # 领域特定字段
  class       = c("stat_result", "htest_result")
)
```

**非检验类返回**（描述统计/区间估计/样本量等）遵循类似 list 结构，`domain`/`method`/`estimate` 必填，`statistic`/`p.value` 可选。

### 契约 4：依赖方向契约

```
.core
  ↑
.stat（L0 原语 + L1 引擎）
  ↑
{.spc, .msa, .capa, .reliability, .doe, .predict, ...}
```

**规则**：
1. .stat 只 Imports `.core`，**禁止 Imports `.plot`**
2. L2 表现层需要 .plot 时，通过 `requireNamespace("iQualityR.plot")` 软依赖，或由 L3 入口延迟注入
3. 功能子包 Imports `.stat` + `.plot` + `.core`
4. .stat 的 L0 原语对功能子包是稳定 API，签名变更必须 bump 版本

**当前违规**：.stat Imports iQualityR.plot（9 处真实调用），违反规则 1。**整改**：9 处调用全部下沉到 L2 Plotter 类，Plotter 通过 `requireNamespace` 软依赖 .plot，DESCRIPTION 移除 iQualityR.plot from Imports → Suggests。

---

## 第五部分 重构路线图

### 阶段 R0：架构对齐（1-2 周，先立骨架）

**目标**：建立四层骨架，不新增功能，不改用户接口。

| 编号 | 任务 | 契约 |
|---|---|---|
| R0-1 | 创建 `StatPlotter` / `StatReporter` 基类（.core 或 .stat 内） | L2 统一 |
| R0-2 | 修复 ANOVA 6 项导出断链 | P0 |
| R0-3 | 移除 `.iqr_plotter` 重复定义、`%||%` 重复定义 | 代码卫生 |
| R0-4 | `validate_dist_params` 导出决策 + 文档补全 | API 边界 |
| R0-5 | 路径注释修正（StatInterpreter.R / model_diag.R / RcppExports.R） | 代码卫生 |
| R0-6 | 重跑 compileAttributes + document，同步 C++ 文档 | C++ 规范 |
| R0-7 | 移除 memoise / SuppDists from Suggests | 依赖清理 |
| R0-8 | 清理 _problems 目录、重命名 getting-started_error.Rmd | 测试卫生 |
| R0-9 | 4 个 $plot() 签名统一为契约 2 | L2 一致 |
| R0-10 | 4 个 $report() 签名统一为契约 2 | L2 一致 |
| R0-11 | ProbPlotter 补 initialize/theme_obj | L2 一致 |
| R0-12 | stat_result 结构定义 + class 属性 + S3 print 方法（轻量，不引入 S3 泛型） | 契约 3 |

### 阶段 R1：依赖与接口整改（1 周）

| 编号 | 任务 |
|---|---|
| R1-1 | rlang/dplyr/data.table 冗余 importFrom 清理（24+25+25 符号） |
| R1-2 | magrittr `%>%` 改 R 4.1+ `\|>` |
| R1-3 | .stat 反向依赖 .plot 的 9 处下沉到 L2 Plotter，DESCRIPTION 改 .plot 为 Suggests |
| R1-4 | vignette 整合（11 → 10 份，合并 4 份概率分布） |
| R1-5 | R6 examples 去 \dontrun{}，改 requireNamespace 包裹 |
| R1-6 | desc_calc / get_c4_prime_cpp 补 @examples |

### 阶段 R2：测试与文档加固（1-2 周）

| 编号 | 任务 |
|---|---|
| R2-1 | 拆分 test_stat.R 为 10 份按板块独立测试文件 |
| R2-2 | ANOVA 全模块测试（当前零测试） |
| R2-3 | 描述统计绘图 7 函数测试 |
| R2-4 | SPC 常数 8 函数测试 |
| R2-5 | HTestAnalyzer 6 个未覆盖方法测试 |
| R2-6 | effect_size 全类型测试 |
| R2-7 | 覆盖率目标 60% → 85% |
| R2-8 | 每板块 1 份 vignette（10 份） |

### 阶段 R3：功能补全（4-8 周，按板块推进）

**优先级 A（基础统计补全，2 周）**：
- R3-A1 htest 非参数（Wilcoxon/Kruskal-Wallis/Friedman）✅ 已完成
- R3-A2 htest 等效性（TOST 均值/比例/非劣效/优效）✅ 已完成
- R3-A3 htest Poisson 率（单/双样本）✅ 已完成
- R3-A4 htest 相关检验（Pearson/Spearman/Kendall）✅ 已完成
- R3-A5 htest 等方差（Levene/Bartlett）✅ 已完成：HTestAnalyzer$levene_test（Brown-Forsythe 默认 center="median"，经典 Levene center="mean"，car::leveneTest 回退）+ bartlett_test（封装 stats::bartlett.test）；StatInterpreter 新增 .interpret_variance_equality 分支。
- R3-A6 intervals 公差区间 + 误差裕度 ✅ 已完成：IntervalAnalyzer$tolerance_interval（正态理论 k-content/p-coverage，支持双侧/下限/上限）+ margin_of_error（均值/比例路径）。

**优先级 B（回归与样本量，3 周）**：
- R3-B1 regression lm + Logistic + Poisson（质量预测常用）✅ 已完成
- R3-B2 regression Cox + PLS + 逐步 + 子集 ✅ 已完成
- R3-B3 regression ROC/Lift/PR ✅ 已完成
- R3-B4 sample-size 配对 t / 回归 / 置信区间 / 公差 / 可靠性 ✅ 已完成：sample_size.R 新增 5 项扩展函数——sample_size_paired（配对 t 检验，基于差值 d=y2-y1 的单样本 t，t 校正迭代）、sample_size_regression（回归全局 F 检验，Cohen's f²=R²/(1-R²)，非中心 F 迭代搜索达到目标功效）、sample_size_ci（CI 宽度驱动，支持 mean/proportion 两种类型，n=(z·σ/h)² 或 n=z²·p(1-p)/h²）、sample_size_tolerance（正态公差区间驱动，Howe/精确卡方 k 因子 + c4(n) 期望 s，约束 k·s≤max_half_width·σ）、sample_size_reliability（可靠性/成功运行研究，零失效 n=log(1-γ)/log(R)，允许多失效时二项累积 P(X≤r|n,1-R)≤1-γ 控制消费者风险）。新增 35 项测试覆盖公式正确性、功效达成、趋势单调性、输入校验；全量 2826 测试通过，R CMD check 0 errors/0 notes（1 既有网络 WARNING，1 既有 $set_theme 文档 NOTE）。

**优先级 C（重抽样与效应量，1 周）**：
- R3-C1 重抽样：Bootstrap（BCa）+ 置换检验 ✅ 已完成：ResamplingAnalyzer$bootstrap_ci（BCa/perc/basic/norm，jackknife 加速）+ permutation_test（单样本符号翻转/双样本标签置换/配对）；完整 L1/L2/L3 模块（ResamplingPlotter/Reporter + iqr_resampling）。
- R3-C2 效应量补全：ω² / 优势比 / 相对风险 ✅ 已完成：effect_size() 统一分派（cohens_d/hedges_g/eta_squared/omega_squared/cohens_h/r）+ odds_ratio() 2×2 表估计；anova.R 内联 .calc_effect_size（eta/partial_eta/omega）。

**优先级 D（高级，2 周）**：
- R3-D1 regression MARS + 样条 ✅ 已完成：RegressionAnalyzer 新增 mars_fit（依赖 earth 包，支持 degree/nk/pmethod/thresh 参数，通过预过滤 NA 解决 earth 不接受 na.action 的限制）与 spline_fit（依赖 splines 包，支持 B 样条 bs 与自然样条 ns、df/degree/knots 参数、自动展开首个预测变量为样条 basis、保留额外协变量）。L1 analyze() 分派扩展为 9 种模型类型；L2 RegressionPlotter 新增 spline 绘图类型（自动选择数据散点+拟合曲线，通过原始公式重建数据定位主预测变量）；L2 RegressionReporter 新增 mars_fit/spline_fit 控制台与 data.frame 输出分支；L3 iqr_regression/regression_run 支持 mars_fit/spline_fit；StatInterpreter 新增 mars_fit（广义 R²/GCV/项数）与 spline_fit（basis/df/F 统计量）解读分支。新增 30+ 项测试覆盖功能正确性（非线性结构捕捉、分段线性拟合、交互项）、输入校验、analyze() 分派、L3 整合器、便捷函数、解释器、报告器、绘图器；全量 2763 测试通过，R CMD check 0 errors/0 notes（1 既有网络 WARNING，1 既有 $set_theme 文档 NOTE）。
- R3-D2 intervals 预测区间统一 ✅ 已完成：IntervalAnalyzer$pi_mean 扩展为统一入口，支持两种模式——Mode 1 单样本正态预测区间（基于 t 分布，x +/- t_{α/2,n-1}·s·√(1+1/n)）与 Mode 2 模型预测区间（封装 stats::predict.lm(interval="prediction")）。私有方法重构为 .pi_mean_sample / .pi_mean_model；通过 pi_mode 字段（"sample"/"model"）区分。模型模式支持显式 lm 对象、formula+data 内部拟合、newdata 多行预测（结果存入 prediction_table），单行时 conf.int 直接对应、多行时 conf.int 跨 min(lwr)/max(upr)。StatInterpreter 按 pi_mode 分支解读；IntervalPlotter .auto_select 模型模式回退至 errorbar（避免直方图路径对 data.frame 的 x 报错）。新增 26 项测试覆盖两种模式的计算正确性、predict.lm 一致性、多预测点、输入校验、单侧强制转双侧、L3 整合器、便捷函数、解释器、绘图器、报告器；全量 2697 测试通过，R CMD check 0 errors/0 notes（仅 1 个既有网络 WARNING）。
- R3-D3 anova ANOM（均值分析）✅ 已完成：AnovaAnalyzer$anova_anom + AnovaPlotter$plot_anom + AnovaReporter(ANOM sheet) + iqr_anova/anova_run(method="anom") + StatInterpreter(.interpret_anom)；决策限采用 Studentized range 近似 h=qtukey(1-α,k,df_e)/√2，支持均衡/非均衡设计；新增 30 项测试，全量 2639 测试通过。

### 阶段 R4：跨包复用推进（2-4 周）

| 编号 | 任务 | 影响子包 |
|---|---|---|
| R4-1 | .doe ANOVA 改用 AnovaAnalyzer | .doe |
| R4-2 | .doe 回归诊断改用 DiagnosticAnalyzer | .doe |
| R4-3 | .doe 效应量改用 effect_size | .doe |
| R4-4 | .predict 模型诊断改用 DiagnosticAnalyzer | .predict |
| R4-5 | .predict 数据变换改用 box_cox_transform / auto_transform | .predict |
| R4-6 | .plot QQ 图数据改用 calc_qq_data | .plot |
| R4-7 | .spc 清理 13 个未用 importFrom | .spc |

### 阶段 R5：长期演进（评估后决策）

| 编号 | 任务 | 决策点 |
|---|---|---|
| R5-1 | 新建 `.multivariate` 子包（PCA/因子/聚类/判别/MDS/SEM/MCA/LCA/t-SNE） | 独立子包 |
| R5-2 | 新建 `.timeseries` 子包或扩展 `.predict`（ARIMA/季节分解/预测） | 独立子包 |
| R5-3 | `.reliability` 完整功能（K-M/删失/加速寿命/寿命回归/Probit/Weibayes/保修） | .stat 只提供原语 |
| R5-4 | 新建 `.sampling` 子包（抽样验收/OC 曲线） | 独立子包 |
| R5-5 | get_c4_prime 纯 R fallback | 鲁棒性 |

---

## 第六部分 板块 × 层 矩阵（目标状态）

| 板块 | L0 原语 | L1 Analyzer | L2 Plotter | L2 Reporter | L3 整合器 | L3 便捷函数 | 测试 | vignette |
|---|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|
| basic | ✓ | BasicAnalyzer | BasicPlotter | BasicReporter | iqr_basic | basic_run/plot/interpret/report | test-basic | basic.Rmd |
| htest | effect_size | HTestAnalyzer ✓ | HTestPlotter ✓ | HTestReporter ✓ | iqr_htest ✓ | htest_run/plot/interpret/report ✓ | test-htest | htest.Rmd |
| distributions | DIST_REGISTRY etc ✓ | DistAnalyzer | DistPlotter | DistReporter | iqr_dist | dist_run/plot/interpret/report | test-distributions | distributions.Rmd |
| anova | levene/η² | AnovaAnalyzer ✓ | AnovaPlotter ✓ | AnovaReporter ✓ | iqr_anova ✓ | anova_run/plot/interpret/report ✓ | test-anova | anova.Rmd |
| regression | — | RegressionAnalyzer | RegressionPlotter | RegressionReporter | iqr_regression | regression_run/plot/interpret/report | test-regression | regression.Rmd |
| spc-foundation | 12 常数 + sigma + 规则 ✓ | — | — | — | — | 直接暴露 L0 | test-spc-foundation | spc-foundation.Rmd |
| quality-metrics | 12 指标 ✓ | — | — | — | — | 直接暴露 L0 | test-quality-metrics | quality-metrics.Rmd |
| sample-size | — | SampleSizeAnalyzer | SampleSizePlotter | SampleSizeReporter | iqr_sample_size | sample_size_run/plot/interpret/report | test-sample-size | sample-size.Rmd |
| diagnostics | — | DiagnosticAnalyzer | DiagnosticPlotter | DiagnosticReporter | iqr_diagnostics | diagnostics_run/plot/interpret/report | test-diagnostics | diagnostics.Rmd |
| intervals | 区间原语 | IntervalAnalyzer | IntervalPlotter | IntervalReporter | iqr_intervals | intervals_run/plot/interpret/report | test-intervals | intervals.Rmd |

✓ = 已存在，需对齐契约；空 = 需新建。

---

## 第七部分 关键决策点

### 决策 1：stat_result 是否引入 S3 类

**建议**：引入轻量 S3（仅 `class <- c("stat_result", "<domain>_result")`），配 `print.stat_result` / `format.stat_result`，但不引入 S3 泛型分发。收益：统一打印格式、便于用户判断结果类型；成本：极低。

### 决策 2：.stat 反向依赖 .plot 如何解除

**方案 A**（推荐）：L2 Plotter 类通过 `requireNamespace("iQualityR.plot")` 软依赖，DESCRIPTION 改 .plot 为 Suggests。用户调 `$plot()` 时才加载 .plot。

**方案 B**：L2 Plotter 类移到 `.plot` 子包，.stat 只到 L1。成本高，破坏 .stat 自包含性。

**建议选 A**：保留 .stat 自包含，但软依赖 .plot。

### 决策 3：多元分析 / 时间序列是否独立子包

**建议**：独立。.stat 已 99 导出，再加多元/时间序列会突破 150+，违背"内核不膨胀"。新建 `.multivariate` / `.timeseries`，.stat 只提供分布原语支持。

### 决策 4：regression 板块是否引入 .stat

**建议**：引入。回归是基础统计核心，质量预测/DOE 都依赖。优先 lm + Logistic + Poisson + Cox，高级（MARS/样条）后补。

### 决策 5：MSA/DOE/可靠性底层原语是否下沉 .stat

**建议**：不下沉。这些是领域特定功能，保留在各子包。.stat 只提供通用原语（常数/分布/sigma/质量指标）。

---

## 附录 A：板块目录结构（目标状态）

```
iQualityR.stat/
├── R/
│   ├── package.R                      # .iqr_plotter 单例（仅 1 处）
│   ├── zzz.R                          # 集中 @importFrom
│   ├── RcppExports.R                  # 标准位置（非 R/sigma/）
│   ├── basic/
│   │   ├── BasicAnalyzer.R
│   │   ├── BasicPlotter.R
│   │   ├── BasicReporter.R
│   │   ├── iqr_basic.R
│   │   ├── desc.R                     # L0 描述统计
│   │   ├── outlier.R                  # L0 异常值
│   │   └── transform.R                # L0 变换
│   ├── htest/
│   │   ├── HTestAnalyzer.R           # ✓ 已存在，扩展
│   │   ├── HTestPlotter.R            # ✓ 已存在
│   │   ├── HTestReporter.R           # ✓ 已存在
│   │   ├── iqr_htest.R               # ✓ 已存在
│   │   └── effect_size.R             # L0 效应量
│   ├── distributions/
│   │   ├── DistAnalyzer.R            # 整合 ProbAnalyzer + fit
│   │   ├── DistPlotter.R
│   │   ├── DistReporter.R
│   │   ├── iqr_dist.R
│   │   ├── dist_fit.R                # L0 拟合
│   │   ├── dist_registry.R           # L0 注册表
│   │   └── validate_dist_params.R    # L0 验证
│   ├── anova/                         # 整体保留
│   │   ├── AnovaAnalyzer.R           # ✓ 修复导出
│   │   ├── AnovaPlotter.R            # ✓
│   │   ├── AnovaReporter.R           # ✓
│   │   └── iqr_anova.R               # ✓
│   ├── regression/                    # 新建
│   │   ├── RegressionAnalyzer.R
│   │   ├── RegressionPlotter.R
│   │   ├── RegressionReporter.R
│   │   └── iqr_regression.R
│   ├── spc-foundation/                # 重命名 from constant/ + sigma/ + spc_rules/
│   │   ├── constants.R               # L0 常数
│   │   ├── sigma_estimate.R          # L0 sigma
│   │   └── spc_rules.R               # L0 规则
│   ├── quality-metrics/               # 重命名 from quality_metric/
│   │   └── quality_metrics.R         # L0 质量指标
│   ├── sample-size/
│   │   ├── SampleSizeAnalyzer.R
│   │   ├── SampleSizePlotter.R
│   │   ├── SampleSizeReporter.R
│   │   ├── iqr_sample_size.R
│   │   └── sample_size.R             # L0 样本量
│   ├── diagnostics/                   # 重命名 from dodel_diag/
│   │   ├── DiagnosticAnalyzer.R
│   │   ├── DiagnosticPlotter.R
│   │   ├── DiagnosticReporter.R
│   │   ├── iqr_diagnostics.R
│   │   ├── model_diag.R              # L0 诊断
│   │   └── StatInterpreter.R         # L2 共享解读器
│   └── intervals/                     # 新建
│       ├── IntervalAnalyzer.R
│       ├── IntervalPlotter.R
│       ├── IntervalReporter.R
│       ├── iqr_intervals.R
│       └── intervals.R               # L0 区间
├── src/
│   ├── get_c4_prime.cpp
│   └── RcppExports.cpp
├── tests/testthat/
│   ├── test-basic.R
│   ├── test-htest.R
│   ├── test-distributions.R
│   ├── test-anova.R
│   ├── test-regression.R
│   ├── test-spc-foundation.R
│   ├── test-quality-metrics.R
│   ├── test-sample-size.R
│   ├── test-diagnostics.R
│   └── test-intervals.R
├── vignettes/
│   ├── getting-started.Rmd
│   ├── basic.Rmd
│   ├── htest.Rmd
│   ├── distributions.Rmd
│   ├── anova.Rmd
│   ├── regression.Rmd
│   ├── spc-foundation.Rmd
│   ├── quality-metrics.Rmd
│   ├── sample-size.Rmd
│   ├── diagnostics.Rmd
│   └── intervals.Rmd
├── DESCRIPTION
├── NAMESPACE
└── STAT_ANALYSIS_PLAN.md              # 本文档
```

---

## 附录 B：v1.0 代码事实清单摘要

（v1.0 报告的 20 项代码质量问题、依赖冗余清单、跨包调用清单等事实仍有效，作为本框架的整改输入。此处不再重复，完整事实见 v1.0 存档。）

---

## 结语

本框架的核心贡献：

1. **一条主线**：.stat 是统计计算内核，只做计算+解读，不做渲染+编排。
2. **四层架构**：L0 原语 / L1 引擎 / L2 表现 / L3 入口，每层职责不可越界。
3. **十个板块**：自包含目录，边界清晰，避免功能交叉。
4. **四份契约**：命名/签名/返回/依赖，确保架构一致 + 体验一致。
5. **五阶段路线**：R0 架构对齐 → R1 依赖整改 → R2 测试加固 → R3 功能补全 → R4 跨包复用 → R5 长期演进。

**下一步建议**：先执行 R0（1-2 周，立骨架），再按 R1-R2-R3 顺序推进。R0 完成后，.stat 将具备一致的架构骨架，后续功能补全只需"填板块"，不再"打补丁"。
