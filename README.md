# gpr: Power Analysis for R

A comprehensive power analysis package for R implementing statistical power 
analyses similar to those available in G*Power 3.1 (Faul et al., 2007).
Supports t-tests, F-tests, chi-square tests, z-tests, and exact tests.

**Author:** Steve Jean-Baptiste  
**Institution:** West Chester University of Pennsylvania

## Features

- Power analysis for t-tests, F-tests, chi-square, z-tests, and exact tests
- All 5 analysis modes: A priori, Post hoc, Sensitivity, Criterion, Compromise
- Effect size converters (Cohen's d, f, w, h, r, R², eta², omega²)
- Publication-quality distribution plots and power curves
- Programmatic API for use in R scripts and R Markdown

## Installation
```r
devtools::install_github("Stevejb1/gpr")
```

## Usage
```r library(gpr)

# t-test: how many participants do I need?
gpr_ttest(test = "two.sample", analysis = "a_priori",
          effect_size = 0.5, alpha = 0.05, power = 0.80)

# One-way ANOVA
gpr_ftest(test = "anova.one.way", analysis = "a_priori",
          effect_size = 0.25, alpha = 0.05, power = 0.80, groups = 3)

# Convert effect sizes
gpr_effect_size(0.5, from = "d")
```

## Reference

Faul, F., Erdfelder, E., Lang, A.-G., & Buchner, A. (2007). G*Power 3: 
A flexible statistical power analysis program for the social, behavioral, 
and biomedical sciences. *Behavior Research Methods, 39*, 175-191.
