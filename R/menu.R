#' Launch the gpr Interactive Console Menu
#'
#' Launches an interactive console menu for power analysis, similar to
#' the menu-driven interface in G*Power 3.1 (Faul et al., 2007).
#' Navigate by typing numbers and pressing Enter.
#'
#' @return Invisibly returns the result of the selected analysis.
#' @export
#' @examples
#' # See vignette for examples
gpr <- function() {

  cat("\n")
  cat("==================================================================\n")
  cat("  gpr: Power Analysis for R\n")
  cat("  By Steve Jean-Baptiste\n")
  cat("==================================================================\n\n")

  # ── Step 1: Select test family ─────────────────────────────────────────────
  repeat {
    cat("  SELECT TEST FAMILY:\n\n")
    cat("  1. t tests\n")
    cat("  2. F tests\n")
    cat("  3. Chi-square tests\n")
    cat("  4. z tests\n")
    cat("  5. Exact tests\n")
    cat("  0. Exit\n\n")

    family <- readline("  Enter number: ")

    if (family == "0") {
      cat("\n  Goodbye!\n\n")
      return(invisible(NULL))
    }

    if (!family %in% c("1","2","3","4","5")) {
      cat("\n  Invalid choice. Please try again.\n\n")
      next
    }

    break
  }

  # ── Step 2: Select statistical test ───────────────────────────────────────
  test_choice <- gpr_menu_test(family)
  if (is.null(test_choice)) return(invisible(NULL))

  # ── Step 3: Select analysis type ──────────────────────────────────────────
  cat("\n  TYPE OF POWER ANALYSIS:\n\n")
  cat("  1. A priori    - Compute required sample size\n")
  cat("  2. Post hoc    - Compute achieved power\n")
  cat("  3. Sensitivity - Compute required effect size\n")
  cat("  4. Criterion   - Compute required alpha\n")
  cat("  5. Compromise  - Compute implied alpha & power\n")
  cat("  0. Back\n\n")

  analysis_choice <- readline("  Enter number: ")

  if (analysis_choice == "0") return(gpr())

  analysis <- switch(analysis_choice,
                     "1" = "a_priori",
                     "2" = "post_hoc",
                     "3" = "sensitivity",
                     "4" = "criterion",
                     "5" = "compromise",
                     {cat("\n  Invalid choice.\n\n"); return(gpr())}
  )

  # ── Step 4: Collect input parameters ──────────────────────────────────────
  params <- gpr_menu_inputs(test_choice, analysis)
  if (is.null(params)) return(invisible(NULL))

  # ── Step 5: Run analysis ───────────────────────────────────────────────────
  cat("\n")
  result <- gpr_menu_run(test_choice, analysis, params)

  # ── Step 6: Ask what to do next ───────────────────────────────────────────
  cat("\n  What would you like to do?\n\n")
  cat("  1. Run another analysis\n")
  cat("  2. Exit\n\n")

  next_choice <- readline("  Enter number: ")
  if (next_choice == "1") return(gpr())

  invisible(result)
}

# ── Test selection menu ────────────────────────────────────────────────────
gpr_menu_test <- function(family) {

  cat("\n  SELECT STATISTICAL TEST:\n\n")

  if (family == "1") {
    cat("  1.  Means: Two independent groups (two-sample t-test)\n")
    cat("  2.  Means: Difference from constant (one-sample t-test)\n")
    cat("  3.  Means: Two dependent means (paired t-test)\n")
    cat("  4.  Correlation: Point biserial model\n")
    cat("  5.  Wilcoxon signed-rank test (matched pairs)\n")
    cat("  6.  Wilcoxon signed-rank test (one sample)\n")
    cat("  7.  Wilcoxon-Mann-Whitney test (two groups)\n")
    cat("  8.  Linear bivariate regression: one group, slope\n")
    cat("  9.  Linear bivariate regression: two groups, intercepts\n")
    cat("  10. Linear bivariate regression: two groups, slopes\n")
    cat("  11. Linear multiple regression: single coefficient\n")
    cat("  12. Generic t test\n")
    cat("  0.  Back\n\n")

    choice <- readline("  Enter number: ")
    if (choice == "0") return(NULL)

    test <- switch(choice,
                   "1"  = list(fn = "ttest", test = "two.sample"),
                   "2"  = list(fn = "ttest", test = "one.sample"),
                   "3"  = list(fn = "ttest", test = "paired"),
                   "4"  = list(fn = "ttest", test = "point.biserial"),
                   "5"  = list(fn = "ttest", test = "wilcoxon.matched"),
                   "6"  = list(fn = "ttest", test = "wilcoxon.onesample"),
                   "7"  = list(fn = "ttest", test = "wilcoxon.twosample"),
                   "8"  = list(fn = "ttest", test = "reg.slope.one"),
                   "9"  = list(fn = "ttest", test = "reg.intercepts"),
                   "10" = list(fn = "ttest", test = "reg.slopes"),
                   "11" = list(fn = "ttest", test = "reg.coefficient"),
                   "12" = list(fn = "ttest", test = "generic"),
                   {cat("\n  Invalid choice.\n\n"); return(NULL)}
    )

  } else if (family == "2") {
    cat("  1.  ANCOVA: Fixed effects, main effects and interactions\n")
    cat("  2.  ANOVA: Fixed effects, omnibus, one-way\n")
    cat("  3.  ANOVA: Fixed effects, special, main effects and interactions\n")
    cat("  4.  ANOVA: Repeated measures, between factors\n")
    cat("  5.  ANOVA: Repeated measures, within factors\n")
    cat("  6.  ANOVA: Repeated measures, within-between interaction\n")
    cat("  7.  Hotellings T2: One group mean vector\n")
    cat("  8.  Hotellings T2: Two group mean vectors\n")
    cat("  9.  MANOVA: Global effects\n")
    cat("  10. MANOVA: Special effects and interactions\n")
    cat("  11. MANOVA: Repeated measures, between factors\n")
    cat("  12. MANOVA: Repeated measures, within factors\n")
    cat("  13. MANOVA: Repeated measures, within-between interaction\n")
    cat("  14. Linear multiple regression: Fixed model, R2 deviation from zero\n")
    cat("  15. Linear multiple regression: Fixed model, R2 increase\n")
    cat("  16. Variance: Test of equality (two sample case)\n")
    cat("  17. Generic F test\n")
    cat("  0.  Back\n\n")

    choice <- readline("  Enter number: ")
    if (choice == "0") return(NULL)

    test <- switch(choice,
                   "1"  = list(fn = "ftest", test = "ancova"),
                   "2"  = list(fn = "ftest", test = "anova.one.way"),
                   "3"  = list(fn = "ftest", test = "anova.fixed.special"),
                   "4"  = list(fn = "ftest", test = "anova.rm.between"),
                   "5"  = list(fn = "ftest", test = "anova.rm.within"),
                   "6"  = list(fn = "ftest", test = "anova.rm.interaction"),
                   "7"  = list(fn = "ftest", test = "hotelling.one"),
                   "8"  = list(fn = "ftest", test = "hotelling.two"),
                   "9"  = list(fn = "ftest", test = "manova.global"),
                   "10" = list(fn = "ftest", test = "manova.special"),
                   "11" = list(fn = "ftest", test = "manova.rm.between"),
                   "12" = list(fn = "ftest", test = "manova.rm.within"),
                   "13" = list(fn = "ftest", test = "manova.rm.interaction"),
                   "14" = list(fn = "ftest", test = "reg.r2.deviation"),
                   "15" = list(fn = "ftest", test = "reg.r2.increase"),
                   "16" = list(fn = "ftest", test = "variance.equality"),
                   "17" = list(fn = "ftest", test = "generic"),
                   {cat("\n  Invalid choice.\n\n"); return(NULL)}
    )

  } else if (family == "3") {
    cat("  1. Goodness-of-fit tests: Contingency tables\n")
    cat("  2. Variance: Difference from constant (one sample case)\n")
    cat("  3. Generic chi-square test\n")
    cat("  0. Back\n\n")

    choice <- readline("  Enter number: ")
    if (choice == "0") return(NULL)

    test <- switch(choice,
                   "1" = list(fn = "chisq", test = "gof"),
                   "2" = list(fn = "chisq", test = "variance"),
                   "3" = list(fn = "chisq", test = "generic"),
                   {cat("\n  Invalid choice.\n\n"); return(NULL)}
    )

  } else if (family == "4") {
    cat("  1. Correlation: Tetrachoric model\n")
    cat("  2. Correlations: Two dependent Pearson r's (common index)\n")
    cat("  3. Correlations: Two dependent Pearson r's (no common index)\n")
    cat("  4. Correlations: Two independent Pearson r's\n")
    cat("  5. Logistic regression\n")
    cat("  6. Poisson regression\n")
    cat("  7. Proportions: Difference between two independent proportions\n")
    cat("  8. Generic z test\n")
    cat("  0. Back\n\n")

    choice <- readline("  Enter number: ")
    if (choice == "0") return(NULL)

    test <- switch(choice,
                   "1" = list(fn = "ztest", test = "correlation.tetrachoric"),
                   "2" = list(fn = "ztest", test = "correlation.two.dep.common"),
                   "3" = list(fn = "ztest", test = "correlation.two.dep.none"),
                   "4" = list(fn = "ztest", test = "correlation.two.indep"),
                   "5" = list(fn = "ztest", test = "logistic.regression"),
                   "6" = list(fn = "ztest", test = "poisson.regression"),
                   "7" = list(fn = "ztest", test = "proportions.two.indep"),
                   "8" = list(fn = "ztest", test = "generic"),
                   {cat("\n  Invalid choice.\n\n"); return(NULL)}
    )

  } else if (family == "5") {
    cat("  1. Correlation: Bivariate normal model\n")
    cat("  2. Linear multiple regression: Random model\n")
    cat("  3. Proportion: Difference from constant (binomial, one sample)\n")
    cat("  4. Proportions: Inequality, two dependent groups (McNemar)\n")
    cat("  5. Proportions: Inequality, two independent groups (Fisher's exact)\n")
    cat("  6. Proportions: Inequality, two independent groups (unconditional)\n")
    cat("  7. Proportions: Inequality (offset), two independent groups\n")
    cat("  8. Proportion: Sign test (binomial test)\n")
    cat("  9. Generic binomial test\n")
    cat("  0. Back\n\n")

    choice <- readline("  Enter number: ")
    if (choice == "0") return(NULL)

    test <- switch(choice,
                   "1" = list(fn = "exact", test = "correlation.bivariate"),
                   "2" = list(fn = "exact", test = "regression.random"),
                   "3" = list(fn = "exact", test = "proportion.one.sample"),
                   "4" = list(fn = "exact", test = "proportion.mcnemar"),
                   "5" = list(fn = "exact", test = "proportion.fisher"),
                   "6" = list(fn = "exact", test = "proportion.unconditional"),
                   "7" = list(fn = "exact", test = "proportion.offset"),
                   "8" = list(fn = "exact", test = "proportion.sign"),
                   "9" = list(fn = "exact", test = "generic.binomial"),
                   {cat("\n  Invalid choice.\n\n"); return(NULL)}
    )
  }

  test
}

# ── Input collection ───────────────────────────────────────────────────────
gpr_menu_inputs <- function(test_choice, analysis) {

  cat("\n  INPUT PARAMETERS:\n")
  cat("  (Press Enter to use default values)\n\n")

  params <- list()

  # Effect size
  if (analysis != "sensitivity") {
    prompt <- switch(test_choice$fn,
                     "ttest" = "  Effect size (Cohen's d) [small=0.2, medium=0.5, large=0.8] (default=medium/0.5): ",
                     "ftest" = "  Effect size (Cohen's f) [small=0.1, medium=0.25, large=0.4] (default=medium/0.25): ",
                     "chisq" = "  Effect size (Cohen's w) [small=0.1, medium=0.3, large=0.5] (default=medium/0.3): ",
                     "ztest" = "  Effect size [small=0.2, medium=0.5, large=0.8] (default=medium/0.5): ",
                     "exact" = "  Effect size g (= p1 - p0) [default=medium/0.15]: ",
                     "  Effect size (default=medium): "
    )

    # Special case for proportions
    if (test_choice$test == "proportions.two.indep") {
      p1 <- as.numeric(readline("  Proportion p1 [default 0.50]: ") |>
                         (\(x) if (x == "") "0.50" else x)())
      p2 <- as.numeric(readline("  Proportion p2 [default 0.70]: ") |>
                         (\(x) if (x == "") "0.70" else x)())
      params$p1 <- p1
      params$p2 <- p2
    } else if (test_choice$test == "proportion.one.sample") {
      p0 <- as.numeric(readline("  P (H0) constant proportion [default 0.50]: ") |>
                         (\(x) if (x == "") "0.50" else x)())
      p1 <- as.numeric(readline("  P (H1) alternative proportion [default 0.65]: ") |>
                         (\(x) if (x == "") "0.65" else x)())
      params$p0 <- p0
      params$p1 <- p1
    } else {
      es_input <- readline(prompt)
      default_es <- switch(test_choice$fn,
        "ttest" = 0.5,
        "ftest" = 0.25,
        "chisq" = 0.3,
        "ztest" = 0.5,
        "exact" = 0.15,
        0.5
      )
      params$effect_size <- if (es_input == "") default_es else as.numeric(es_input)
    }
  }

  # Alpha
  if (!analysis %in% c("criterion", "compromise")) {
    alpha_input <- readline("  Alpha (significance level) [default 0.05]: ")
    params$alpha <- if (alpha_input == "") 0.05 else as.numeric(alpha_input)
  }

  # Power
  if (analysis %in% c("a_priori", "sensitivity", "criterion")) {
    power_input <- readline("  Power (1-beta) [default 0.80]: ")
    params$power <- if (power_input == "") 0.80 else as.numeric(power_input)
  }

  # Sample size
  if (analysis %in% c("post_hoc", "sensitivity", "criterion", "compromise")) {
    n_input <- readline("  Sample size (n): ")
    if (n_input != "") params$n <- as.integer(n_input)
  }

  # Beta/alpha ratio for compromise
  if (analysis == "compromise") {
    ratio_input <- readline("  Beta/alpha ratio [default 1]: ")
    params$beta_alpha_ratio <- if (ratio_input == "") 1 else as.numeric(ratio_input)
  }

  # Extra params for specific tests
  if (test_choice$fn == "ftest") {
    if (test_choice$test %in% c("anova.one.way", "anova.fixed.special",
                                "anova.rm.between", "anova.rm.within",
                                "anova.rm.interaction", "ancova",
                                "manova.global", "manova.special",
                                "manova.rm.between", "manova.rm.within",
                                "manova.rm.interaction")) {
      groups_input <- readline("  Number of groups: ")
      if (groups_input != "") params$groups <- as.integer(groups_input)
    } else if (test_choice$test %in% c("reg.r2.deviation", "reg.r2.increase",
                                       "hotelling.one", "hotelling.two")) {
      pred_input <- readline("  Number of predictors/variables: ")
      if (pred_input != "") params$predictors <- as.integer(pred_input)
    }
  }

  if (test_choice$fn == "chisq") {
    df_input <- readline("  Degrees of freedom (df): ")
    if (df_input != "") params$df <- as.integer(df_input)
  }

  # Tails
  if (test_choice$fn %in% c("ttest", "ztest", "exact")) {
    tails_input <- readline("  Tails [1 or 2, default 2]: ")
    params$tails <- if (tails_input == "") 2 else as.integer(tails_input)
  }

  params
}

# ── Run the analysis ───────────────────────────────────────────────────────
gpr_menu_run <- function(test_choice, analysis, params) {

  result <- tryCatch({
    base_args <- c(
      list(test = test_choice$test, analysis = analysis),
      params
    )

    switch(test_choice$fn,
           "ttest" = do.call(gpr_ttest, base_args),
           "ftest" = do.call(gpr_ftest, base_args),
           "chisq" = do.call(gpr_chisq, base_args),
           "ztest" = do.call(gpr_ztest, base_args),
           "exact" = do.call(gpr_exact, base_args)
    )
  }, error = function(e) {
    cat(sprintf("\n  Error: %s\n", e$message))
    NULL
  })

  result
}
