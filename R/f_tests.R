#' Power Analysis for F-tests
#'
#' Performs power analyses for F-test variants similar to those available
#' in G*Power 3.1 (Faul et al., 2007). Supports all 5 analysis modes:
#' a priori, post hoc, sensitivity, criterion, and compromise.
#'
#' @param test Type of F-test. One of:
#'   \itemize{
#'     \item "anova.one.way"          - ANOVA: Fixed effects, omnibus, one-way
#'     \item "anova.fixed.special"    - ANOVA: Fixed effects, special, main effects and interactions
#'     \item "anova.rm.between"       - ANOVA: Repeated measures, between factors
#'     \item "anova.rm.within"        - ANOVA: Repeated measures, within factors
#'     \item "anova.rm.interaction"   - ANOVA: Repeated measures, within-between interaction
#'     \item "ancova"                 - ANCOVA: Fixed effects, main effects and interactions
#'     \item "manova.global"          - MANOVA: Global effects
#'     \item "manova.special"         - MANOVA: Special effects and interactions
#'     \item "manova.rm.between"      - MANOVA: Repeated measures, between factors
#'     \item "manova.rm.within"       - MANOVA: Repeated measures, within factors
#'     \item "manova.rm.interaction"  - MANOVA: Repeated measures, within-between interaction
#'     \item "hotelling.one"          - Hotellings T2: One group mean vector
#'     \item "hotelling.two"          - Hotellings T2: Two group mean vectors
#'     \item "reg.r2.deviation"       - Linear multiple regression: Fixed model, R2 deviation from zero
#'     \item "reg.r2.increase"        - Linear multiple regression: Fixed model, R2 increase
#'     \item "variance.equality"      - Variance: Test of equality (two sample case)
#'     \item "generic"                - Generic F test
#'   }
#' @param analysis Type of power analysis. One of:
#'   \itemize{
#'     \item "a_priori"    - Solve for sample size
#'     \item "post_hoc"    - Solve for power
#'     \item "sensitivity" - Solve for effect size
#'     \item "criterion"   - Solve for alpha
#'     \item "compromise"  - Solve for alpha and power
#'   }
#' @param effect_size Cohen's f. Small=0.10, Medium=0.25, Large=0.40.
#'   For regression tests, this is f2 (R2/(1-R2)).
#' @param n Total sample size.
#' @param alpha Significance level. Default 0.05.
#' @param power Desired power (1 - beta). Default 0.80.
#' @param num_df Numerator degrees of freedom (number of groups - 1, or number of predictors).
#' @param groups Number of groups (for ANOVA). Used to compute num_df automatically.
#' @param predictors Number of predictors (for regression). Used to compute num_df automatically.
#' @param corr_among_means Correlation among repeated measures. Default 0.5.
#' @param beta_alpha_ratio For compromise only: ratio of beta to alpha. Default 1.
#' @param allocation_ratio For variance equality test: ratio of n2/n1. Default 1.
#' @param plot Logical. Whether to print plots. Default TRUE.
#'
#' @return A list of class "gpr_result" with all parameters and plots.
#' @export
#'
#' @examples
#' # See vignette for examples
gpr_ftest <- function(test               = "anova.one.way",
                      analysis           = "a_priori",
                      effect_size        = NULL,
                      n                  = NULL,
                      alpha              = NULL,
                      power              = NULL,
                      num_df             = NULL,
                      groups             = NULL,
                      predictors         = NULL,
                      corr_among_means   = 0.5,
                      beta_alpha_ratio   = 1,
                      allocation_ratio   = 1,
                      plot             = TRUE) {

  # -- Step 1: Validate inputs --------------------------------------------------
  valid_analyses <- c("a_priori", "post_hoc", "sensitivity", "criterion", "compromise")
  if (!analysis %in% valid_analyses)
    stop(sprintf("'analysis' must be one of: %s", paste(valid_analyses, collapse = ", ")))

  valid_tests <- c("anova.one.way", "anova.fixed.special", "anova.rm.between",
                   "anova.rm.within", "anova.rm.interaction", "ancova",
                   "manova.global", "manova.special", "manova.rm.between",
                   "manova.rm.within", "manova.rm.interaction",
                   "hotelling.one", "hotelling.two",
                   "reg.r2.deviation", "reg.r2.increase",
                   "variance.equality", "generic")
  if (!test %in% valid_tests)
    stop(sprintf("'test' must be one of: %s", paste(valid_tests, collapse = ", ")))

  # -- Step 2: Compute numerator df from groups or predictors ------------------
  # num_df is the degrees of freedom for the effect being tested
  if (is.null(num_df)) {
    if (test %in% c("anova.one.way", "anova.fixed.special",
                    "anova.rm.between", "anova.rm.within",
                    "anova.rm.interaction", "ancova",
                    "manova.global", "manova.special",
                    "manova.rm.between", "manova.rm.within",
                    "manova.rm.interaction")) {
      if (is.null(groups))
        stop("Please provide 'groups' (number of groups) or 'num_df' directly")
      num_df <- groups - 1

    } else if (test %in% c("reg.r2.deviation", "reg.r2.increase")) {
      if (is.null(predictors))
        stop("Please provide 'predictors' (number of predictors) or 'num_df' directly")
      num_df <- predictors

    } else if (test == "hotelling.one") {
      if (is.null(predictors))
        stop("Please provide 'predictors' (number of dependent variables) or 'num_df' directly")
      num_df <- predictors

    } else if (test == "hotelling.two") {
      if (is.null(predictors))
        stop("Please provide 'predictors' (number of dependent variables) or 'num_df' directly")
      num_df <- predictors

    } else if (test == "variance.equality") {
      num_df <- 1

    } else {
      if (is.null(groups) && is.null(predictors))
        stop("Please provide 'groups', 'predictors', or 'num_df'")
      num_df <- if (!is.null(groups)) groups - 1 else predictors
    }
  }

  # -- Step 3: Set defaults based on analysis type ------------------------------
  if (analysis == "a_priori") {
    if (is.null(effect_size)) stop("a_priori requires effect_size")
    if (is.null(alpha))       alpha <- 0.05
    if (is.null(power))       power <- 0.80
    n <- NULL

  } else if (analysis == "post_hoc") {
    if (is.null(effect_size)) stop("post_hoc requires effect_size")
    if (is.null(n))           stop("post_hoc requires n")
    if (is.null(alpha))       alpha <- 0.05
    power <- NULL

  } else if (analysis == "sensitivity") {
    if (is.null(n))     stop("sensitivity requires n")
    if (is.null(alpha)) alpha <- 0.05
    if (is.null(power)) power <- 0.80
    effect_size <- NULL

  } else if (analysis == "criterion") {
    if (is.null(effect_size)) stop("criterion requires effect_size")
    if (is.null(n))           stop("criterion requires n")
    if (is.null(power))       power <- 0.80
    alpha <- NULL

  } else if (analysis == "compromise") {
    if (is.null(effect_size)) stop("compromise requires effect_size")
    if (is.null(n))           stop("compromise requires n")
    alpha <- NULL
    power <- NULL
  }

  # -- Step 4: Denominator df and noncentrality parameter ----------------------
  # These vary by test type

  den_df <- function(n) {
    if (test %in% c("anova.one.way", "anova.fixed.special", "ancova")) {
      # Standard between-subjects: N - k (k = num_df + 1 groups)
      n - (num_df + 1)

    } else if (test %in% c("anova.rm.within", "anova.rm.interaction")) {
      # Within-subjects: (N - groups) * num_df
      # Uses epsilon correction via corr_among_means
      (n - (num_df + 1)) * num_df

    } else if (test == "anova.rm.between") {
      n - (num_df + 1)

    } else if (test %in% c("manova.global", "manova.special",
                           "manova.rm.between", "manova.rm.within",
                           "manova.rm.interaction")) {
      # MANOVA denominator
      n - num_df - 1

    } else if (test == "hotelling.one") {
      n - num_df

    } else if (test == "hotelling.two") {
      n - num_df - 1

    } else if (test %in% c("reg.r2.deviation", "reg.r2.increase")) {
      # Regression: N - predictors - 1
      n - num_df - 1

    } else if (test == "variance.equality") {
      # F-test for equality of variances: both groups n-1
      ceiling(n / 2) - 1

    } else {
      # Generic
      n - num_df - 1
    }
  }

  # Noncentrality parameter lambda = f^2 * N
  # For repeated measures, adjusted by correlation among measures
  ncp_factor <- function(n, f) {
    if (test %in% c("anova.rm.within", "anova.rm.interaction")) {
      # Within-subjects ncp is amplified by the correction factor
      # lambda = f^2 * N * num_df * 2 * (1 - corr) / (1 - corr)  simplified:
      f^2 * n * (num_df + 1) * 2 * (1 - corr_among_means)
    } else {
      f^2 * n
    }
  }

  # -- Step 5: Critical value ---------------------------------------------------
  crit_value <- function(alpha, n) {
    qf(1 - alpha, num_df, den_df(n))
  }

  # -- Step 6: Core power computation ------------------------------------------
  compute_power <- function(n, f, alpha) {
    ddf  <- den_df(n)
    ncp  <- ncp_factor(n, f)
    crit <- crit_value(alpha, n)

    if (ddf <= 0) return(NA)

    # Power = P(F > critical value | ncp)
    pf(crit, num_df, ddf, ncp = ncp, lower.tail = FALSE)
  }

  # -- Step 7: Solve for missing parameter -------------------------------------
  min_n <- num_df + 2   # minimum n to have positive denominator df

  if (analysis == "a_priori") {
    res      <- uniroot(function(n) compute_power(n, effect_size, alpha) - power,
                        c(min_n, 1e6), tol = 1e-6)
    n_raw    <- ceiling(res$root)
    # Round up to nearest multiple of groups so groups are equal size
    k        <- num_df + 1
    n        <- ceiling(n_raw / k) * k
    power    <- compute_power(n, effect_size, alpha)

  } else if (analysis == "post_hoc") {
    power <- compute_power(n, effect_size, alpha)

  } else if (analysis == "sensitivity") {
    res         <- uniroot(function(f) compute_power(n, f, alpha) - power,
                           c(1e-6, 10), tol = 1e-6)
    effect_size <- res$root

  } else if (analysis == "criterion") {
    res   <- uniroot(function(a) compute_power(n, effect_size, a) - power,
                     c(1e-10, 1 - 1e-10), tol = 1e-6)
    alpha <- res$root

  } else if (analysis == "compromise") {
    res   <- uniroot(
      function(a) compute_power(n, effect_size, a) - (1 - beta_alpha_ratio * a),
      c(1e-10, 1 / (1 + beta_alpha_ratio) - 1e-10), tol = 1e-6)
    alpha <- res$root
    power <- 1 - beta_alpha_ratio * alpha
  }

  # -- Step 8: Build results ----------------------------------------------------
  beta_val   <- round(1 - power, 4)
  ddf_final  <- den_df(n)
  ncp_final  <- ncp_factor(n, effect_size)
  crit_final <- crit_value(alpha, n)

  k          <- num_df + 1
  n_per_group <- floor(n / k)

  results <- list(
    test             = test,
    analysis         = analysis,
    effect_size      = round(effect_size, 4),
    n                = n,
    n_per_group      = n_per_group,
    alpha            = round(alpha, 6),
    power            = round(power, 7),
    beta             = beta_val,
    beta_alpha_ratio = if (analysis == "compromise") round(beta_val / alpha, 4) else NULL,
    num_df           = num_df,
    den_df           = ddf_final,
    noncentrality    = round(ncp_final, 7),
    critical_f       = round(crit_final, 7)
  )

  class(results) <- "gpr_result"

  # -- Step 9: Print summary ----------------------------------------------------
  analysis_labels <- c(
    a_priori    = "A Priori: Compute required sample size",
    post_hoc    = "Post Hoc: Compute achieved power",
    sensitivity = "Sensitivity: Compute required effect size",
    criterion   = "Criterion: Compute required alpha",
    compromise  = "Compromise: Compute implied alpha & power"
  )

  cat("\n-- gpr: Power Analysis for F-tests -----------------------------\n")
  cat(sprintf("  Test:                %s\n", test))
  cat(sprintf("  Analysis:            %s\n", analysis_labels[analysis]))
  cat("------------------------------------------------------------------\n")
  cat(sprintf("  Effect size (f):     %.4f\n", results$effect_size))
  cat(sprintf("  Alpha (\u03b1):           %.6f\n", results$alpha))
  cat(sprintf("  Power (1-\u03b2):         %.7f\n", results$power))
  cat(sprintf("  Beta (\u03b2):            %.4f\n", results$beta))
  if (analysis == "compromise")
    cat(sprintf("  \u03b2/\u03b1 ratio:           %.4f\n", results$beta_alpha_ratio))
  cat(sprintf("  Noncentrality (\u03bb):   %.7f\n", results$noncentrality))
  cat(sprintf("  Critical F:          %.7f\n", results$critical_f))
  cat(sprintf("  Numerator df:        %d\n",   results$num_df))
  cat(sprintf("  Denominator df:      %d\n",   results$den_df))
  cat(sprintf("  Sample size per group: %d\n", results$n_per_group))
  cat(sprintf("  Total sample size:     %d\n", results$n))
  cat("------------------------------------------------------------------\n\n")

  # -- Step 10: Distribution plot -----------------------------------------------
  # Central F (H0) vs Noncentral F (H1) with shaded alpha and beta regions
  ddf    <- ddf_final
  ncp    <- ncp_final
  crit   <- crit_final

  x_max  <- max(qf(0.9999, num_df, ddf, ncp = ncp), crit * 2)
  x      <- seq(0, x_max, length.out = 1000)

  h0_density <- df(x, num_df, ddf)
  h1_density <- df(x, num_df, ddf, ncp = ncp)

  dist_data <- data.frame(
    x            = rep(x, 2),
    density      = c(h0_density, h1_density),
    distribution = rep(c("H0 (Central F)", "H1 (Noncentral F)"), each = length(x))
  )

  # Alpha region: right tail of H0 beyond critical F
  alpha_region <- data.frame(
    x       = x[x >= crit],
    density = h0_density[x >= crit]
  )

  # Beta region: left tail of H1 up to critical F
  beta_region <- data.frame(
    x       = x[x <= crit],
    density = h1_density[x <= crit]
  )

  p1 <- ggplot2::ggplot(dist_data, ggplot2::aes(x = x, y = density,
                                                color = distribution)) +
    ggplot2::geom_area(data = beta_region,
                       ggplot2::aes(x = x, y = density),
                       fill = "#6BAED6", alpha = 0.5, inherit.aes = FALSE) +
    ggplot2::geom_area(data = alpha_region,
                       ggplot2::aes(x = x, y = density),
                       fill = "#FC8D59", alpha = 0.5, inherit.aes = FALSE) +
    ggplot2::geom_line(linewidth = 1.1) +
    ggplot2::geom_vline(xintercept = crit, linetype = "dashed",
                        color = "gray30", linewidth = 0.8) +
    ggplot2::annotate("text", x = crit, y = max(h0_density) * 0.95,
                      label = sprintf("critical F = %.4f", crit),
                      hjust = -0.05, size = 3.5, color = "gray30") +
    ggplot2::annotate("text",
                      x = crit * 0.5,
                      y = max(h1_density) * 0.25,
                      label = "\u03b2", size = 7, color = "#2171B5") +
    ggplot2::annotate("text",
                      x = crit + (x_max - crit) * 0.25,
                      y = max(h0_density) * 0.15,
                      label = "\u03b1", size = 7, color = "#D94701") +
    ggplot2::scale_color_manual(values = c(
      "H0 (Central F)"    = "#D94701",
      "H1 (Noncentral F)" = "#2171B5"
    )) +
    ggplot2::labs(
      title    = "Central and Noncentral F Distributions",
      subtitle = bquote(f == .(round(results$effect_size,2)) ~
                          "| N =" ~ .(n) ~
                          "| " ~ alpha == .(round(results$alpha,4)) ~
                          "| Power =" ~ .(round(results$power,4))),
      x        = "F",
      y        = "Density",
      color    = NULL
    ) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(
      plot.title       = ggplot2::element_text(face = "bold"),
      plot.subtitle    = ggplot2::element_text(color = "grey40"),
      legend.position  = "top",
      panel.grid.minor = ggplot2::element_blank()
    )

  if (plot) print(p1)

  # -- Step 11: Power curve -----------------------------------------------------
  n_seq     <- seq(min_n + 1, max(n * 2, 50), length.out = 300)
  power_seq <- sapply(n_seq, function(ni) {
    pw <- tryCatch(compute_power(ni, effect_size, alpha), error = function(e) NA)
    pw
  })

  plot_data <- data.frame(n = n_seq, power = power_seq)
  plot_data <- plot_data[!is.na(plot_data$power), ]

  p2 <- ggplot2::ggplot(plot_data, ggplot2::aes(x = n, y = power)) +
    ggplot2::geom_line(color = "#2171B5", linewidth = 1.2) +
    ggplot2::geom_hline(yintercept = results$power, linetype = "dashed",
                        color = "#D94701", linewidth = 0.8) +
    ggplot2::geom_vline(xintercept = n, linetype = "dashed",
                        color = "#238B45", linewidth = 0.8) +
    ggplot2::annotate("point", x = n, y = results$power,
                      color = "#D94701", size = 4) +
    ggplot2::annotate("text", x = n, y = results$power,
                      label = sprintf("  N=%d\n  Power=%.4f", n, results$power),
                      hjust = 0, vjust = -0.4, size = 3.5) +
    ggplot2::scale_y_continuous(limits = c(0, 1),
                                labels = scales::percent_format()) +
    ggplot2::labs(
      title    = sprintf("Power Curve \u2014 %s", test),
      subtitle = bquote(f == .(round(results$effect_size,2)) ~
                          "| " ~ alpha == .(round(results$alpha,4)) ~
                          "| num df =" ~ .(num_df)),
      x        = "Total Sample Size (N)",
      y        = expression(paste("Power (1 - ", beta, ")")),
    ) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(
      plot.title       = ggplot2::element_text(face = "bold"),
      plot.subtitle    = ggplot2::element_text(color = "grey40"),
      panel.grid.minor = ggplot2::element_blank()
    )

  if (plot) print(p2)
  invisible(results)
}
