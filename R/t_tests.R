#' Power Analysis for t-tests
#'
#' Performs power analyses for t-test variants similar to those available
#' in G*Power 3.1 (Faul et al., 2007). Supports all 5 analysis modes:
#' a priori, post hoc, sensitivity, criterion, and compromise.
#'
#' @param test Type of t-test. One of:
#'   \itemize{
#'     \item "two.sample"           - Means: Two independent groups
#'     \item "one.sample"           - Means: Difference from constant
#'     \item "paired"               - Means: Two dependent means (matched pairs)
#'     \item "point.biserial"       - Correlation: Point biserial model
#'     \item "wilcoxon.matched"     - Wilcoxon signed-rank (matched pairs)
#'     \item "wilcoxon.onesample"   - Wilcoxon signed-rank (one sample)
#'     \item "wilcoxon.twosample"   - Wilcoxon-Mann-Whitney (two groups)
#'     \item "reg.slope.one"        - Linear bivariate regression: one group, slope
#'     \item "reg.intercepts"       - Linear bivariate regression: two groups, intercepts
#'     \item "reg.slopes"           - Linear bivariate regression: two groups, slopes
#'     \item "reg.coefficient"      - Linear multiple regression: single coefficient
#'     \item "generic"              - Generic t test
#'   }
#' @param analysis Type of power analysis. One of:
#'   \itemize{
#'     \item "a_priori"    - Solve for sample size
#'     \item "post_hoc"    - Solve for power
#'     \item "sensitivity" - Solve for effect size
#'     \item "criterion"   - Solve for alpha
#'     \item "compromise"  - Solve for alpha and power
#'   }
#' @param effect_size Cohen's d. Small=0.2, Medium=0.5, Large=0.8.
#' @param n Sample size per group.
#' @param alpha Significance level. Default 0.05.
#' @param power Desired power (1 - beta). Default 0.80.
#' @param allocation_ratio Ratio of n2/n1. Default 1 (equal groups).
#' @param plot Logical. Whether to print plots. Default TRUE.
#' @param beta_alpha_ratio For compromise only: ratio of beta to alpha. Default 1.
#' @param tails Number of tails. 1 (one-tailed) or 2 (two-tailed). Default 2.
#' @param alternative "two.sided", "greater", or "less". Default "two.sided".
#' @param relative_efficiency For Wilcoxon tests. Default 0.864.
#'
#' @return A list of class "gpr_result" with all parameters and plots.
#' @export
#'
#' @examples
#' # See vignette for examples
gpr_ttest <- function(test                = "two.sample",
                      analysis            = "a_priori",
                      effect_size         = NULL,
                      n                   = NULL,
                      alpha               = NULL,
                      power               = NULL,
                      allocation_ratio    = 1,
                      beta_alpha_ratio    = 1,
                      tails               = 2,
                      alternative         = NULL,
                      relative_efficiency = 0.864,
                      plot             = TRUE) {

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

  valid_tests <- c("two.sample", "one.sample", "paired", "point.biserial",
                   "wilcoxon.matched", "wilcoxon.onesample", "wilcoxon.twosample",
                   "reg.slope.one", "reg.intercepts", "reg.slopes",
                   "reg.coefficient", "generic")
  if (!test %in% valid_tests)
    stop(sprintf("'test' must be one of: %s", paste(valid_tests, collapse = ", ")))

  if (allocation_ratio <= 0)
    stop("allocation_ratio must be greater than 0")

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

  # -- Step 3: Wilcoxon efficiency adjustment -----------------------------------
  is_wilcoxon  <- test %in% c("wilcoxon.matched", "wilcoxon.onesample", "wilcoxon.twosample")
  is_two_group <- test %in% c("two.sample", "wilcoxon.twosample", "reg.intercepts", "reg.slopes")

  effective_effect <- if (is_wilcoxon && !is.null(effect_size)) {
    effect_size * sqrt(relative_efficiency)
  } else {
    effect_size
  }

  # -- Step 4: Degrees of freedom -----------------------------------------------
  # With unequal groups: n1 = n, n2 = n * allocation_ratio
  # df = n1 + n2 - 2 = n(1 + ratio) - 2
  df_factor <- function(n) {
    if (is_two_group) n * (1 + allocation_ratio) - 2 else n - 1
  }

  # -- Step 5: Noncentrality parameter -----------------------------------------
  # With allocation ratio r: ncp = d * sqrt(n * r / (1 + r))
  # When r=1 (equal groups): ncp = d * sqrt(n/2) - same as before
  ncp_factor <- function(n, d) {
    if (is_two_group) d * sqrt(n * allocation_ratio / (1 + allocation_ratio))
    else d * sqrt(n)
  }

  # -- Step 6: Critical value ---------------------------------------------------
  crit_value <- function(alpha, df) {
    if (alternative == "two.sided") qt(1 - alpha / 2, df)
    else qt(1 - alpha, df)
  }

  # -- Step 7: Core power computation ------------------------------------------
  compute_power <- function(n, d, alpha) {
    df   <- df_factor(n)
    ncp  <- ncp_factor(n, d)
    crit <- crit_value(alpha, df)

    if (alternative == "two.sided") {
      pt(crit,  df, ncp = ncp, lower.tail = FALSE) +
        pt(-crit, df, ncp = ncp, lower.tail = TRUE)
    } else if (alternative == "greater") {
      pt(crit, df, ncp = ncp, lower.tail = FALSE)
    } else {
      pt(-crit, df, ncp = ncp, lower.tail = TRUE)
    }
  }

  # -- Step 8: Solve for missing parameter -------------------------------------
  if (analysis == "a_priori") {
    res   <- uniroot(function(n) compute_power(n, effective_effect, alpha) - power,
                     c(2, 1e6), tol = 1e-6)
    n     <- ceiling(res$root)
    power <- compute_power(n, effective_effect, alpha)

  } else if (analysis == "post_hoc") {
    power <- compute_power(n, effective_effect, alpha)

  } else if (analysis == "sensitivity") {
    res              <- uniroot(function(d) compute_power(n, d, alpha) - power,
                                c(1e-6, 10), tol = 1e-6)
    effect_size      <- res$root
    effective_effect <- effect_size

  } else if (analysis == "criterion") {
    res   <- uniroot(function(a) compute_power(n, effective_effect, a) - power,
                     c(1e-10, 1 - 1e-10), tol = 1e-6)
    alpha <- res$root

  } else if (analysis == "compromise") {
    res   <- uniroot(
      function(a) compute_power(n, effective_effect, a) - (1 - beta_alpha_ratio * a),
      c(1e-10, 1 / (1 + beta_alpha_ratio) - 1e-10), tol = 1e-6)
    alpha <- res$root
    power <- 1 - beta_alpha_ratio * alpha
  }

  # -- Step 9: Build results ----------------------------------------------------
  n1       <- n
  n2       <- ceiling(n * allocation_ratio)
  total_n  <- n1 + n2
  beta_val <- round(1 - power, 4)
  df_final <- df_factor(n)
  ncp_final <- ncp_factor(n, if (!is.null(effective_effect)) effective_effect else effect_size)
  crit_final <- crit_value(alpha, df_final)

  results <- list(
    test             = test,
    analysis         = analysis,
    alternative      = alternative,
    effect_size      = round(if (!is.null(effect_size)) effect_size else effective_effect, 4),
    n1               = n1,
    n2               = n2,
    total_n          = total_n,
    allocation_ratio = allocation_ratio,
    alpha            = round(alpha, 6),
    power            = round(power, 4),
    beta             = beta_val,
    beta_alpha_ratio = if (analysis == "compromise") round(beta_val / alpha, 4) else NULL,
    df               = df_final,
    noncentrality    = round(ncp_final, 7),
    critical_t       = round(crit_final, 7)
  )

  class(results) <- "gpr_result"

  # -- Step 10: Print summary ---------------------------------------------------
  analysis_labels <- c(
    a_priori    = "A Priori: Compute required sample size",
    post_hoc    = "Post Hoc: Compute achieved power",
    sensitivity = "Sensitivity: Compute required effect size",
    criterion   = "Criterion: Compute required alpha",
    compromise  = "Compromise: Compute implied alpha & power"
  )

  cat("\n-- gpr: Power Analysis for t-tests -----------------------------\n")
  cat(sprintf("  Test:                %s\n", test))
  cat(sprintf("  Analysis:            %s\n", analysis_labels[analysis]))
  cat(sprintf("  Alternative:         %s\n", alternative))
  if (allocation_ratio != 1)
    cat(sprintf("  Allocation ratio:    %.4f\n", allocation_ratio))
  cat("------------------------------------------------------------------\n")
  cat(sprintf("  Effect size (d):     %.4f\n", results$effect_size))
  cat(sprintf("  Alpha (\u03b1):           %.6f\n", results$alpha))
  cat(sprintf("  Power (1-\u03b2):         %.7f\n", results$power))
  cat(sprintf("  Beta (\u03b2):            %.4f\n", results$beta))
  if (analysis == "compromise")
    cat(sprintf("  \u03b2/\u03b1 ratio:           %.4f\n", results$beta_alpha_ratio))
  cat(sprintf("  Noncentrality (\u03b4):   %.7f\n", results$noncentrality))
  cat(sprintf("  Critical t:          %.7f\n", results$critical_t))
  cat(sprintf("  Df:                  %d\n",   results$df))
  cat(sprintf("  Sample size group 1: %d\n",   results$n1))
  if (is_two_group)
    cat(sprintf("  Sample size group 2: %d\n", results$n2))
  cat(sprintf("  Total sample size:   %d\n",   results$total_n))
  cat("------------------------------------------------------------------\n\n")

  # -- Step 11: Distribution plot -----------------------------------------------
  # Shows central (H0) and noncentral (H1) t-distributions
  # with shaded alpha and beta regions and critical value line
  df_val  <- df_final
  ncp_val <- ncp_final
  crit    <- crit_final

  # Build x range wide enough to show both distributions
  x_min <- min(-4, ncp_val - 4)
  x_max <- max(4,  ncp_val + 4)
  x     <- seq(x_min, x_max, length.out = 1000)

  # Central t (H0) and noncentral t (H1) densities
  h0_density <- dt(x, df = df_val)
  h1_density <- dt(x, df = df_val, ncp = ncp_val)

  dist_data <- data.frame(
    x          = rep(x, 2),
    density    = c(h0_density, h1_density),
    distribution = rep(c("H\u2080 (Central t)", "H\u2081 (Noncentral t)"), each = length(x))
  )

  # Shaded regions
  # Alpha region: right tail of H0 beyond critical t
  alpha_region <- data.frame(
    x       = x[x >= crit],
    density = h0_density[x >= crit]
  )

  # Beta region: left tail of H1 up to critical t (missed detections)
  beta_region <- data.frame(
    x       = x[x <= crit],
    density = h1_density[x <= crit]
  )

  # Left tail alpha for two sided
  alpha_left <- data.frame(
    x       = x[x <= -crit],
    density = h0_density[x <= -crit]
  )

  p1 <- ggplot2::ggplot(dist_data, ggplot2::aes(x = x, y = density,
                                                color = distribution)) +
    # Shaded beta region (Type II error)
    ggplot2::geom_area(data = beta_region,
                       ggplot2::aes(x = x, y = density),
                       fill = "#6BAED6", alpha = 0.5, inherit.aes = FALSE) +
    # Shaded alpha region right tail (Type I error)
    ggplot2::geom_area(data = alpha_region,
                       ggplot2::aes(x = x, y = density),
                       fill = "#FC8D59", alpha = 0.5, inherit.aes = FALSE) +
    # Shaded alpha left tail (two sided only)
    {if (alternative == "two.sided")
      ggplot2::geom_area(data = alpha_left,
                         ggplot2::aes(x = x, y = density),
                         fill = "#FC8D59", alpha = 0.5, inherit.aes = FALSE)} +
    # Distribution curves
    ggplot2::geom_line(linewidth = 1.1) +
    # Critical value line
    ggplot2::geom_vline(xintercept = crit, linetype = "dashed",
                        color = "gray30", linewidth = 0.8) +
    # Critical t label
    ggplot2::annotate("text", x = crit, y = max(h0_density) * 0.95,
                      label = sprintf("critical t = %.4f", crit),
                      hjust = -0.05, size = 3.5, color = "gray30") +
    # Beta label
    ggplot2::annotate("text",
                      x = crit - (crit - x_min) * 0.3,
                      y = max(h1_density) * 0.25,
                      label = "\u03b2", size = 7, color = "#2171B5") +
    # Alpha label
    ggplot2::annotate("text",
                      x = crit + (x_max - crit) * 0.3,
                      y = max(h0_density) * 0.15,
                      label = if (alternative == "two.sided") "\u03b1/2" else "\u03b1",
                      size = 7, color = "#D94701") +
    ggplot2::scale_color_manual(values = c(
      "H\u2080 (Central t)"    = "#D94701",
      "H\u2081 (Noncentral t)" = "#2171B5"
    )) +
    ggplot2::labs(
      title    = "Central and Noncentral t Distributions",
      subtitle = bquote(d == .(round(results$effect_size,2)) ~
                          "| n =" ~ .(n1) ~
                          "| " ~ alpha == .(round(results$alpha,4)) ~
                          "| Power =" ~ .(round(results$power,4))),
      x        = "t",
      y        = "Density",
      color    = NULL
    ) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(
      plot.title      = ggplot2::element_text(face = "bold"),
      plot.subtitle   = ggplot2::element_text(color = "grey40"),
      legend.position = "top",
      panel.grid.minor = ggplot2::element_blank()
    )

  if (plot) print(p1)

  # -- Step 12: Power curve plot ------------------------------------------------
  n_seq     <- seq(2, max(total_n * 2, 50), length.out = 300)
  power_seq <- sapply(n_seq, function(ni) compute_power(ni, results$effect_size, results$alpha))

  plot_data <- data.frame(
    n     = if (is_two_group) n_seq * (1 + allocation_ratio) else n_seq,
    power = power_seq
  )

  p2 <- ggplot2::ggplot(plot_data, ggplot2::aes(x = n, y = power)) +
    ggplot2::geom_line(color = "#2171B5", linewidth = 1.2) +
    ggplot2::geom_hline(yintercept = results$power, linetype = "dashed",
                        color = "#D94701", linewidth = 0.8) +
    ggplot2::geom_vline(xintercept = total_n, linetype = "dashed",
                        color = "#238B45", linewidth = 0.8) +
    ggplot2::annotate("point", x = total_n, y = results$power,
                      color = "#D94701", size = 4) +
    ggplot2::annotate("text", x = total_n, y = results$power,
                      label = sprintf("  N=%d\n  Power=%.4f", total_n, results$power),
                      hjust = 0, vjust = -0.4, size = 3.5) +
    ggplot2::scale_y_continuous(limits = c(0, 1),
                                labels = scales::percent_format()) +
    ggplot2::labs(
      title    = sprintf("Power Curve - %s", test),
      subtitle = bquote(d == .(round(results$effect_size,2)) ~
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
