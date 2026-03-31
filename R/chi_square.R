#' Power Analysis for Chi-Square Tests
#'
#' Performs power analyses for chi-square test variants similar to those
#' available in G*Power 3.1 (Faul et al., 2007). Supports all 5 analysis
#' modes: a priori, post hoc, sensitivity, criterion, and compromise.
#'
#' @param test Type of chi-square test. One of:
#'   \itemize{
#'     \item "gof"      - Goodness-of-fit tests: Contingency tables
#'     \item "variance" - Variance: Difference from constant (one sample case)
#'     \item "generic"  - Generic chi-square test
#'   }
#' @param analysis Type of power analysis. One of:
#'   \itemize{
#'     \item "a_priori"    - Solve for sample size
#'     \item "post_hoc"    - Solve for power
#'     \item "sensitivity" - Solve for effect size
#'     \item "criterion"   - Solve for alpha
#'     \item "compromise"  - Solve for alpha and power
#'   }
#' @param effect_size Cohen's w. Small=0.10, Medium=0.30, Large=0.50.
#'   For variance test, this is the ratio of variance to null variance.
#' @param n Total sample size.
#' @param alpha Significance level. Default 0.05.
#' @param power Desired power (1 - beta). Default 0.80.
#' @param df Degrees of freedom. For gof: number of categories - 1.
#'   For contingency tables: (rows-1)*(cols-1).
#' @param beta_alpha_ratio For compromise only: ratio of beta to alpha. Default 1.
#' @param plot Logical. Whether to print plots. Default TRUE.
#' @param plot Logical. Whether to print plots. Default TRUE.
#'
#' @return A list of class "gpr_result" with all parameters and plots.
#' @export
#'
#' @examples
#' # See vignette for examples
gpr_chisq <- function(test            = "gof",
                      analysis        = "a_priori",
                      effect_size     = NULL,
                      n               = NULL,
                      alpha           = NULL,
                      power           = NULL,
                      df              = NULL,
                      beta_alpha_ratio = 1,
                      plot = TRUE) {

  # -- Step 1: Validate inputs --------------------------------------------------
  valid_analyses <- c("a_priori", "post_hoc", "sensitivity", "criterion", "compromise")
  if (!analysis %in% valid_analyses)
    stop(sprintf("'analysis' must be one of: %s", paste(valid_analyses, collapse = ", ")))

  valid_tests <- c("gof", "variance", "generic")
  if (!test %in% valid_tests)
    stop(sprintf("'test' must be one of: %s", paste(valid_tests, collapse = ", ")))

  if (is.null(df))
    stop("'df' (degrees of freedom) is required. For goodness-of-fit: categories - 1. For contingency tables: (rows-1)*(cols-1).")

  if (df < 1)
    stop("'df' must be at least 1")

  # -- Step 2: Set defaults based on analysis type ------------------------------
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

  # -- Step 3: Noncentrality parameter -----------------------------------------
  # lambda = w^2 * N  (Cohen's w squared times sample size)
  ncp_factor <- function(n, w) w^2 * n

  # -- Step 4: Critical value ---------------------------------------------------
  crit_value <- function(alpha) qchisq(1 - alpha, df)

  # -- Step 5: Core power computation ------------------------------------------
  # Power = P(chi2 > critical value | noncentrality)
  compute_power <- function(n, w, alpha) {
    ncp  <- ncp_factor(n, w)
    crit <- crit_value(alpha)
    pchisq(crit, df, ncp = ncp, lower.tail = FALSE)
  }

  # -- Step 6: Solve for missing parameter -------------------------------------
  if (analysis == "a_priori") {
    res   <- uniroot(function(n) compute_power(n, effect_size, alpha) - power,
                     c(2, 1e6), tol = 1e-6)
    n     <- ceiling(res$root)
    power <- compute_power(n, effect_size, alpha)

  } else if (analysis == "post_hoc") {
    power <- compute_power(n, effect_size, alpha)

  } else if (analysis == "sensitivity") {
    res         <- uniroot(function(w) compute_power(n, w, alpha) - power,
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

  # -- Step 7: Build results ----------------------------------------------------
  beta_val   <- round(1 - power, 4)
  ncp_final  <- ncp_factor(n, effect_size)
  crit_final <- crit_value(alpha)

  results <- list(
    test             = test,
    analysis         = analysis,
    effect_size      = round(effect_size, 4),
    n                = n,
    n_per_group      = n,
    total_n          = n,
    alpha            = round(alpha, 6),
    power            = round(power, 7),
    beta             = beta_val,
    beta_alpha_ratio = if (analysis == "compromise") round(beta_val / alpha, 4) else NULL,
    df               = df,
    noncentrality    = round(ncp_final, 7),
    critical_chisq   = round(crit_final, 7)
  )

  class(results) <- "gpr_result"

  # -- Step 8: Print summary ----------------------------------------------------
  analysis_labels <- c(
    a_priori    = "A Priori: Compute required sample size",
    post_hoc    = "Post Hoc: Compute achieved power",
    sensitivity = "Sensitivity: Compute required effect size",
    criterion   = "Criterion: Compute required alpha",
    compromise  = "Compromise: Compute implied alpha & power"
  )

  cat("\n-- gpr: Power Analysis for Chi-Square Tests ---------------------\n")
  cat(sprintf("  Test:                %s\n", test))
  cat(sprintf("  Analysis:            %s\n", analysis_labels[analysis]))
  cat("------------------------------------------------------------------\n")
  cat(sprintf("  Effect size (w):     %.4f\n", results$effect_size))
  cat(sprintf("  Alpha (\u03b1):           %.6f\n", results$alpha))
  cat(sprintf("  Power (1-\u03b2):         %.7f\n", results$power))
  cat(sprintf("  Beta (\u03b2):            %.4f\n", results$beta))
  if (analysis == "compromise")
    cat(sprintf("  \u03b2/\u03b1 ratio:           %.4f\n", results$beta_alpha_ratio))
  cat(sprintf("  Noncentrality (\u03bb):   %.7f\n", results$noncentrality))
  cat(sprintf("  Critical Chi-sq:         %.7f\n", results$critical_chisq))
  cat(sprintf("  Df:                  %d\n",   results$df))
  cat(sprintf("  Total sample size:   %d\n",   results$total_n))
  cat("------------------------------------------------------------------\n\n")

  # -- Step 9: Distribution plot ------------------------------------------------
  # Central chi-square (H0) vs noncentral chi-square (H1)
  ncp  <- ncp_final
  crit <- crit_final

  x_max <- max(qchisq(0.9999, df, ncp = ncp), crit * 2)
  x     <- seq(0, x_max, length.out = 1000)

  h0_density <- dchisq(x, df)
  h1_density <- dchisq(x, df, ncp = ncp)

  dist_data <- data.frame(
    x            = rep(x, 2),
    density      = c(h0_density, h1_density),
    distribution = rep(c("H0 (Central Chi-sq)",
                         "H1 (Noncentral Chi-sq)"), each = length(x))
  )

  alpha_region <- data.frame(
    x       = x[x >= crit],
    density = h0_density[x >= crit]
  )

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
                      label = sprintf("critical Chi-sq = %.4f", crit),
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
      "H0 (Central Chi-sq)"    = "#D94701",
      "H1 (Noncentral Chi-sq)" = "#2171B5"
    )) +
    ggplot2::labs(
      title    = "Central and Noncentral Chi-Square Distributions",
      subtitle = bquote(w == .(round(results$effect_size,2)) ~
                          "| N =" ~ .(n) ~
                          "| " ~ alpha == .(round(results$alpha,4)) ~
                          "| Power =" ~ .(round(results$power,4))),
      x        = "Chi-squared",
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

  # -- Step 10: Power curve -----------------------------------------------------
  n_seq     <- seq(2, max(n * 2, 50), length.out = 300)
  power_seq <- sapply(n_seq, function(ni) compute_power(ni, effect_size, alpha))

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
      subtitle = bquote(w == .(round(results$effect_size,2)) ~
                          "| " ~ alpha == .(round(results$alpha,4)) ~
                          "| df =" ~ .(df)),
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
