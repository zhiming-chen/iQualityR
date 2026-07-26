# iQualityR

> R 语言集成质量工程框架

`iQualityR` 是一个元包（meta-package），整合了九个协同工作的 R 子包，
覆盖质量工程的完整工作流：核心基础设施、统计基础、可视化、测量系统
分析、过程能力、实验设计、抽样方案、可靠性分析以及预测质量建模。

加载 `iQualityR` 会自动 attach 全部成员包，使其 API 立即可用，遵循
tidyverse 的约定。

## 仓库结构

本仓库为 **Monorepo**：每个成员包是仓库根目录下的一个独立文件夹，
元包 `iQualityR/` 将它们串联起来。

```
iQualityR/                  # 元包：一次性安装/加载全部成员包
iQualityR.core/             # R6 基类、主题系统、i18n、工具函数
iQualityR.plot/             # ggplot2 图层、帕累托图、鱼骨图、乌龟图、方差分析图
iQualityR.stat/             # 描述统计、假设检验、分布拟合、SPC 规则
iQualityR.msa/              # Type 1、线性、量具 R&R（交叉/嵌套）、属性一致性
iQualityR.capa/             # 正态/非正态/非参数过程能力分析
iQualityR.doe/              # 因子设计、RSM、田口、贝叶斯优化、时间效应建模
iQualityR.sampling/         # 单次/二次/多次抽样方案、OC 曲线、ASN
iQualityR.reliability/      # Kaplan-Meier、Cox 模型、参数可靠性
iQualityR.predict/          # 机器学习建模、诊断、可解释性（SHAP）
iQualityR.spc/              # Shewhart、时间加权、多元、稀有事件、ML 增强控制图
```

## 环境要求

- R >= 4.1.0
- 源码包编译工具链（Windows 需 Rtools；Linux/macOS 需 gcc/clang）
- 各子包 `DESCRIPTION` 中声明的 CRAN 依赖

## 安装方式

由于成员包之间存在相互依赖，推荐通过元包安装，这样依赖顺序会自动
解析。`iQualityR/DESCRIPTION` 的 `Remotes:` 字段已将
`remotes::install_github()` 指向本仓库中各成员包对应的子目录。

```r
# install.packages("remotes")
remotes::install_github(
  "zhiming-chen/iQualityR",
  subdir = "iQualityR"        # 元包所在子目录
)
```

此命令会：

1. 根据 `Remotes:` 字段从对应子目录拉取每个成员包
   （如 `iQualityR.core`、`iQualityR.stat` 等）。
2. 按依赖顺序构建并安装。
3. 最后安装元包 `iQualityR` 本身。

安装完成后：

```r
library(iQualityR)
iQualityR_packages()   # 列出已 attach 的成员包及其版本
```

### 单独安装某个成员包

如果只需要某一个成员包，可直接指定其 `subdir`：

```r
remotes::install_github("zhiming-chen/iQualityR", subdir = "iQualityR.msa")
```

注意：每个成员包已在其 `DESCRIPTION` 中声明对其他 `iQualityR.*` 包的
依赖，因此 `remotes` / `pak` 会自动拉取所需的兄弟包。

## 使用说明

各成员包导出各自的 API，详情参见包级帮助和 vignette：

```r
help(package = "iQualityR.msa")
vignette(package = "iQualityR.doe")
```

## 许可证

MIT + file LICENSE，详见各子包目录下的 `LICENSE` 文件。

## 作者

Zhiming Chen <zhimingc383@gmail.com>
