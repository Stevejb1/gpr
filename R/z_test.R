#' Power Analysis for z-tests
#'
#' Performs power analyses for z-test variants similar to those available
#' in G*Power 3.1 (Faul et al., 2007). Supports all 5 analysis modes:
#' a priori, post hoc, sensitivity, criterion, and compromise.
#'
#' @param test Type of z-test. One of:
#'   \itemize{
#'     \item "correlation.tetrachoric"     - Correlation: Tetrachoric model
#'     \item "correlation.two.dep.common"  - Correlations: Two dependent Pearson r's (common index)
#'     \item "correlation.two.dep.none"    - Correlations: Two dependent Pearson r's (no common index)
#'     \item "correlation.two.indep"       - Correlations: Two independent Pearson r's
#'     \item "logistic.regression"         - Logistic regression
#'     \item "poisson.regression"          - Poisson regression
#'     \item "proportions.two.indep"       - Proportions: Difference between two independent proportions
#'     \item "generic"                     - Generic z test
#'   }
#' @param analysis Type of power analysis. One of:
#'   \itemize{
#'     \item "a_priori"    - Solve for sample size
#'     \item "post_hoc"    - Solve for power
#'     \item "sensitivity" - Solve for effect size
#'     \item "criterion"   - Solve for alpha
#'     \item "compromise"  - Solve for alpha and power
#'   }
#' @param effect_size Effect size. For correlations: r. For proportions: h (Cohen's h).
#'   For logistic/poisson regression: log odds ratio. Small=0.2, Medium=0.5, Large=0.8.
#' @param n Total sample size.
#' @param alpha Significance level. Default 0.05.
#' @param power Desired power (1 - beta). Default 0.80.
#' @param tails Number of tails. 1 (one-tailed) or 2 (two-tailed). Default 2.
#' @param alternative "two.sided", "greater", or "less". Default "two.sided".
#' @param p1 For proportions: first proportion. Required for proportions.two.indep.
#' @param p2 For proportions: second proportion. Required for proportions.two.indep.
#' @param r1 For two dependent correlations: first correlation.
#' @param r2 For two dependent correlations: second correlation.
#' @param r12 For two dependent correlations with common index: correlation between r1 and r2.
#' @param beta_alpha_ratio For compromise only: ratio of beta to alpha. Default 1.
#' @param plot Logical. Whether to print plots. Default TRUE.
#' @param plot Logical. Whether to print plots. Default TRUE.
#'
#' @return A list of class "gpr_result" with all parameters and plots.
#' @export
#'
#' @examples
#' # See vignette for examples
gpr_ztest <- function(test            = "proportions.two.indep",
                      analysis        = "a_priori",
                      effect_size     = NULL,
                      n               = NULL,
                      alpha           = NULL,
                      power           = NULL,
                      tails           = 2,
                      alternative     = NULL,
                      p1              = NULL,
                      p2              = NULL,
                      r1              = NULL,
                      r2              = NULL,
                      r12             = NULL,
                      beta_alpha_ratio = 1,
                      plot = TRUE) {

  # Convert tails to alternative if alternative not explicitly set
  if (is.null(alternative)) {
    alternative <- switch(as.character(tails),
                          "2" = "two.sided",
                          "1" = "greater",
                          stop("tails must be 1 or 2")
    )
  }

  # -- Step 1: Validate inputs --------------------------------------------------
  valid_analyses <- c("a_priori", "post_hoc", "sensitivity", "criterion", "compromise")
  if (!analysis %in% valid_analyses)
    stop(sprintf("'analysis' must be one of: %s", paste(valid_analyses, collapse = ", ")))

  valid_tests <- c("correlation.tetrachoric", "correlation.two.dep.common",
                   "correlation.two.dep.none", "correlation.two.indep",
                   "logistic.regression", "poisson.regression",
                   "proportions.two.indep", "generic")
  if (!test %in% valid_tests)
    stop(sprintf("'test' must be one of: %s", paste(valid_tests, collapse = ", ")))

  # -- Step 2: Compute effect size from proportions if needed ------------------
  # For proportions test, compute Cohen's h from p1 and p2
  # h = 2 * arcsin(sqrt(p1)) - 2 * arcsin(sqrt(p2))
  # Save original p1 and p2 before anything overwrites them
  p1_orig <- p1
  p2_orig <- p2

  if (test == "proportions.two.indep") {
    if (!is.null(p1) && !is.null(p2)) {
      effect_size <- abs(2 * asin(sqrt(p1)) - 2 * asin(sqrt(p2)))
    } else if (is.null(effect_size)) {
      stop("proportions.two.indep requires either p1 & p2, or effect_size (Cohen's h)")
    }
  }

  # For two dependent correlations compute effect size from r1, r2, r12
  if (test == "correlation.two.dep.common") {
    if (!is.null(r1) && !is.null(r2) && !is.null(r12)) {
      # Fisher z transformation of r1 and r2
      z1 <- 0.5 * log((1 + r1) / (1 - r1))
      z2 <- 0.5 * log((1 + r2) / (1 - r2))
      effect_size <- abs(z1 - z2)
    } else if (is.null(effect_size)) {
      stop("correlation.two.dep.common requires r1, r2, r12 or effect_size")
    }
  }

  if (test == "correlation.two.dep.none") {
    if (!is.null(r1) && !is.null(r2)) {
      z1 <- 0.5 * log((1 + r1) / (1 - r1))
      z2 <- 0.5 * log((1 + r2) / (1 - r2))
      effect_size <- abs(z1 - z2)
    } else if (is.null(effect_size)) {
      stop("correlation.two.dep.none requires r1, r2 or effect_size")
    }
  }

  # For single correlation tests, convert r to Fisher's z
  if (test %in% c("correlation.two.indep", "correlation.tetrachoric")) {
    if (is.null(effect_size))
      stop(sprintf("%s requires effect_size (correlation r)", test))
    # Convert r to Fisher's z for the ncp calculation
    effect_size_z <- 0.5 * log((1 + effect_size) / (1 - effect_size))
  } else {
    effect_size_z <- effect_size
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
    effect_size   <- NULL
    effect_size_z <- NULL

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

  # -- Step 4: Noncentrality parameter -----------------------------------------
  ncp_factor <- function(n, ez) {
    if (test %in% c("correlation.two.indep")) {
      ez * sqrt(n / 2 - 3)
    } else if (test %in% c("correlation.two.dep.common",
                           "correlation.two.dep.none")) {
      ez * sqrt(n - 3)
    } else if (test %in% c("correlation.tetrachoric")) {
      ez * sqrt(n - 3)
    } else if (test == "proportions.two.indep") {
      # Note: uses unpooled variance formula. G*Power uses a two-step
      # null/alternative variance adjustment which may give n differing
      # by a few units. Results are statistically equivalent.
      if (!is.null(p1_orig) && !is.null(p2_orig)) {
        abs(p1_orig - p2_orig) / sqrt((p1_orig*(1-p1_orig) + p2_orig*(1-p2_orig)) / n)
      } else {
        ez * sqrt(n)
      }
    } else {
      ez * sqrt(n)
    }
  }

  # -- Step 5: Critical value ---------------------------------------------------
  crit_value <- function(alpha) {
    if (alternative == "two.sided") qnorm(1 - alpha / 2)
    else qnorm(1 - alpha)
  }

  # -- Step 6: Core power computation ------------------------------------------
  compute_power <- function(n, ez, alpha) {
    ncp  <- ncp_factor(n, ez)
    crit <- crit_value(alpha)

    if (alternative == "two.sided") {
      pnorm(crit - ncp, lower.tail = FALSE) +
        pnorm(-crit - ncp, lower.tail = TRUE)
    } else if (alternative == "greater") {
      pnorm(crit - ncp, lower.tail = FALSE)
    } else {
      pnorm(-crit - ncp, lower.tail = TRUE)
    }
  }

  # -- Step 7: Solve for missing parameter -------------------------------------
  ez <- if (!is.null(effect_size_z)) effect_size_z else effect_size

  if (analysis == "a_priori") {
    if (test == "proportions.two.indep") {
      # Solve for n per group directly
      res  <- uniroot(function(n_pg) {
        ncp  <- ncp_factor(n_pg, ez)
        crit <- crit_value(alpha)
        pnorm(crit - ncp, lower.tail = FALSE) +
          pnorm(-crit - ncp, lower.tail = TRUE) - power
      }, c(2, 1e6), tol = 1e-6)
      n_pg <- ceiling(res$root)
      n    <- n_pg * 2
    } else {
      res <- uniroot(function(n) compute_power(n, ez, alpha) - power,
                     c(4, 1e6), tol = 1e-6)
      n   <- ceiling(res$root)
    }
    power <- compute_power(n, ez, alpha)

  } else if (analysis == "post_hoc") {
    power <- compute_power(n, ez, alpha)

  } else if (analysis == "sensitivity") {
    res         <- uniroot(function(e) compute_power(n, e, alpha) - power,
                           c(1e-6, 10), tol = 1e-6)
    effect_size <- res$root
    ez          <- effect_size

  } else if (analysis == "criterion") {
    res   <- uniroot(function(a) compute_power(n, ez, a) - power,
                     c(1e-10, 1 - 1e-10), tol = 1e-6)
    alpha <- res$root

  } else if (analysis == "compromise") {
    res   <- uniroot(
      function(a) compute_power(n, ez, a) - (1 - beta_alpha_ratio * a),
      c(1e-10, 1 / (1 + beta_alpha_ratio) - 1e-10), tol = 1e-6)
    alpha <- res$root
    power <- 1 - beta_alpha_ratio * alpha
  }

  # -- Step 8: Build results ----------------------------------------------------
  beta_val   <- round(1 - power, 4)
  ncp_final  <- ncp_factor(n, ez)
  crit_final <- crit_value(alpha)

  is_two_group_z <- test %in% c("proportions.two.indep", "correlation.two.indep")
  n_per_group    <- if (is_two_group_z) n / 2 else n
  total_n        <- n

  results <- list(
    test             = test,
    analysis         = analysis,
    alternative      = alternative,
    effect_size      = round(effect_size, 4),
    n_per_group      = n_per_group,
    total_n          = total_n,
    alpha            = round(alpha, 6),
    power            = round(power, 7),
    beta             = beta_val,
    beta_alpha_ratio = if (analysis == "compromise") round(beta_val / alpha, 4) else NULL,
    noncentrality    = round(ncp_final, 7),
    critical_z       = round(crit_final, 7)
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

  cat("\n-- gpr: Power Analysis for z-tests -----------------------------\n")
  cat(sprintf("  Test:                %s\n", test))
  cat(sprintf("  Analysis:            %s\n", analysis_labels[analysis]))
  cat(sprintf("  Alternative:         %s\n", alternative))
  cat("------------------------------------------------------------------\n")
  cat(sprintf("  Effect size:         %.4f\n", results$effect_size))
  cat(sprintf("  Alpha (\u03b1):           %.6f\n", results$alpha))
  cat(sprintf("  Power (1-\u03b2):         %.7f\n", results$power))
  cat(sprintf("  Beta (\u03b2):            %.4f\n", results$beta))
  if (analysis == "compromise")
    cat(sprintf("  \u03b2/\u03b1 ratio:           %.4f\n", results$beta_alpha_ratio))
  cat(sprintf("  Noncentrality (\u03b4):   %.7f\n", results$noncentrality))
  cat(sprintf("  Critical z:          %.7f\n", results$critical_z))
  if (is_two_group_z)
    cat(sprintf("  Sample size group 1: %d\n", results$n_per_group))
  if (is_two_group_z)
    cat(sprintf("  Sample size group 2: %d\n", results$n_per_group))
  cat(sprintf("  Total sample size:   %d\n",   results$total_n))
  cat("------------------------------------------------------------------\n\n")

  # -- Step 10: Distribution plot -----------------------------------------------
  ncp  <- ncp_final
  crit <- crit_final

  x_min <- min(-4, ncp - 4)
  x_max <- max(4,  ncp + 4)
  x     <- seq(x_min, x_max, length.out = 1000)

  h0_density <- dnorm(x, mean = 0,   sd = 1)
  h1_density <- dnorm(x, mean = ncp, sd = 1)

  dist_data <- data.frame(
    x            = rep(x, 2),
    density      = c(h0_density, h1_density),
    distribution = rep(c("H0 (z | H0 true)",
                         "H1 (z | H1 true)"), each = length(x))
  )

  alpha_region <- data.frame(
    x       = x[x >= crit],
    density = h0_density[x >= crit]
  )

  beta_region <- data.frame(
    x       = x[x <= crit],
    density = h1_density[x <= crit]
  )

  alpha_left <- data.frame(
    x       = x[x <= -crit],
    density = h0_density[x <= -crit]
  )

  p1 <- ggplot2::ggplot(dist_data, ggplot2::aes(x = x, y = density,
                                                color = distribution)) +
    ggplot2::geom_area(data = beta_region,
                       ggplot2::aes(x = x, y = density),
                       fill = "#6BAED6", alpha = 0.5, inherit.aes = FALSE) +
    ggplot2::geom_area(data = alpha_region,
                       ggplot2::aes(x = x, y = density),
                       fill = "#FC8D59", alpha = 0.5, inherit.aes = FALSE) +
    {if (alternative == "two.sided")
      ggplot2::geom_area(data = alpha_left,
                         ggplot2::aes(x = x, y = density),
                         fill = "#FC8D59", alpha = 0.5, inherit.aes = FALSE)} +
    ggplot2::geom_line(linewidth = 1.1) +
    ggplot2::geom_vline(xintercept = crit, linetype = "dashed",
                        color = "gray30", linewidth = 0.8) +
    ggplot2::annotate("text", x = crit, y = max(h0_density) * 0.95,
                      label = sprintf("critical z = %.4f", crit),
                      hjust = -0.05, size = 3.5, color = "gray30") +
    ggplot2::annotate("text",
                      x = crit - (crit - x_min) * 0.3,
                      y = max(h1_density) * 0.25,
                      label = "\u03b2", size = 7, color = "#2171B5") +
    ggplot2::annotate("text",
                      x = crit + (x_max - crit) * 0.3,
                      y = max(h0_density) * 0.15,
                      label = if (alternative == "two.sided") "\u03b1/2" else "\u03b1",
                      size = 7, color = "#D94701") +
    ggplot2::scale_color_manual(values = c(
      "H0 (z | H0 true)" = "#D94701",
      "H1 (z | H1 true)" = "#2171B5"
    )) +
    ggplot2::labs(
      title    = "Standard Normal Distributions - z-test",
      subtitle = bquote("Effect =" ~ .(round(results$effect_size,2)) ~
                          "| N =" ~ .(n) ~
                          "| " ~ alpha == .(round(results$alpha,4)) ~
                          "| Power =" ~ .(round(results$power,4))),
      x        = "z",
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
  n_seq     <- seq(4, max(n * 2, 50), length.out = 300)
  power_seq <- sapply(n_seq, function(ni) compute_power(ni, ez, alpha))

  plot_data <- data.frame(n = n_seq, power = power_seq)

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
      subtitle = bquote("Effect =" ~ .(round(results$effect_size,2)) ~
                          "| " ~ alpha == .(round(results$alpha,4)) ~
                          "| " ~ .(alternative)),
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
