# gpr: Power Analysis for R

A comprehensive power analysis package for R implementing statistical power analyses for t-tests, F-tests, chi-square tests, z-tests, and exact tests. Includes a Shiny GUI, interactive console menu, and direct function API.

**Author:** Steve Jean-Baptiste  
**Institution:** West Chester University of Pennsylvania  
**GitHub:** https://github.com/Stevejb1/gpr

---

## Installation
```r
# Recommended
pak::pak("Stevejb1/gpr")

# Or using devtools
devtools::install_github("Stevejb1/gpr")
```

---

## How to Use

There are 3 ways to use gpr:

### 1. Shiny GUI
```r
library(gpr)
launch_gpr()
```

A clean point-and-click interface that auto-detects your RStudio dark or light theme. Select your test, enter parameters, and click Calculate to get results, distribution plots, and power curves instantly.

### 2. Interactive Console Menu
```r
library(gpr)
gpr()
```

A text-based menu similar to G*Power. Navigate by typing numbers to select your test family, statistical test, analysis type, and input parameters.

### 3. Direct Functions
```r
library(gpr)

# t-test: how many participants do I need?
gpr_ttest(test = "two.sample", analysis = "a_priori",
          effect_size = 0.5, alpha = 0.05, power = 0.80)

# One-way ANOVA
gpr_ftest(test = "anova.one.way", analysis = "a_priori",
          effect_size = 0.25, alpha = 0.05, power = 0.80, groups = 3)

# Chi-square goodness of fit
gpr_chisq(test = "gof", analysis = "a_priori",
          effect_size = 0.30, alpha = 0.05, power = 0.80, df = 5)

# z-test: two independent proportions
gpr_ztest(test = "proportions.two.indep", analysis = "a_priori",
          p1 = 0.50, p2 = 0.70, alpha = 0.05, power = 0.80)

# Exact binomial test
gpr_exact(test = "proportion.one.sample", analysis = "a_priori",
          p0 = 0.50, p1 = 0.65, alpha = 0.05, power = 0.80)
```

---

## Features

- **5 test families** — t-tests, F-tests, Chi-square, z-tests, Exact tests
- **49 statistical tests** across all families
- **5 analysis modes** — A priori, Post hoc, Sensitivity, Criterion, Compromise
- **Effect size converters** — Cohen's d, f, w, h, r, R², eta², partial eta², omega²
- **Publication-quality plots** — distribution plots and power curves
- **3 interfaces** — Shiny GUI, console menu, direct R functions
- **Auto theme detection** — Shiny app matches your RStudio dark or light theme

---

## Analysis Modes

| Mode | Solves For | Requires |
|---|---|---|
| A priori | Sample size | Effect size, alpha, power |
| Post hoc | Power | Effect size, alpha, n |
| Sensitivity | Effect size | Alpha, power, n |
| Criterion | Alpha | Effect size, power, n |
| Compromise | Alpha and power | Effect size, n, beta/alpha ratio |

---

## Supported Tests

### t tests
Two independent groups, one sample, paired, point biserial, Wilcoxon signed-rank (matched and one sample), Wilcoxon-Mann-Whitney, linear bivariate regression (one group slope, two groups intercepts and slopes), linear multiple regression single coefficient, generic t test.

### F tests
ANCOVA, one-way ANOVA, fixed effects ANOVA, repeated measures ANOVA (between, within, interaction), Hotelling's T2 (one and two group), MANOVA (global, special, repeated measures), linear multiple regression (R2 deviation and increase), variance equality, generic F test.

### Chi-square tests
Goodness of fit, variance difference from constant, generic chi-square.

### z tests
Tetrachoric correlation, two dependent correlations (common and no common index), two independent correlations, logistic regression, Poisson regression, two independent proportions, generic z test.

### Exact tests
Bivariate normal correlation, random model regression, one sample proportion (binomial), McNemar, Fisher's exact, unconditional proportions, offset proportions, sign test, generic binomial.

---

## Effect Size Converters
```r
# Convert any effect size to all others
gpr_effect_size(0.5,  from = "d")
gpr_effect_size(0.25, from = "f")
gpr_effect_size(0.09, from = "r2")
gpr_effect_size(0.06, from = "eta2")

# Print Cohen's benchmarks for all families
effect_size_benchmarks()
```

---

## Dependencies

- `ggplot2` — distribution plots and power curves
- `scales` — axis formatting

**Suggested:** `shiny` for the GUI, `rstudioapi` for theme detection

---

## Citation

If you use gpr in your research please cite both the package and the original G*Power methodology:

**Package:**  
Jean-Baptiste, S. (2025). *gpr: Power Analysis for R*. R package version 0.0.0.9000. https://github.com/Stevejb1/gpr

**Methodology:**  
Faul, F., Erdfelder, E., Lang, A.-G., & Buchner, A. (2007). G*Power 3: A flexible statistical power analysis program for the social, behavioral, and biomedical sciences. *Behavior Research Methods, 39*, 175-191.

---

## License

GPL-3 — see LICENSE.md for details.
