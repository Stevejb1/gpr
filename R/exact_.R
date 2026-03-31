#' Power Analysis for Exact Tests
#'
#' Performs power analyses for exact test variants similar to those available
#' in G*Power 3.1 (Faul et al., 2007). Supports all 5 analysis modes:
#' a priori, post hoc, sensitivity, criterion, and compromise.
#'
#' @param test Type of exact test. One of:
#'   \itemize{
#'     \item "correlation.bivariate"    - Correlation: Bivariate normal model
#'     \item "regression.random"        - Linear multiple regression: Random model
#'     \item "proportion.one.sample"    - Proportion: Difference from constant (binomial, one sample)
#'     \item "proportion.mcnemar"       - Proportions: Inequality, two dependent groups (McNemar)
#'     \item "proportion.fisher"        - Proportions: Inequality, two independent groups (Fisher's exact)
#'     \item "proportion.unconditional" - Proportions: Inequality, two independent groups (unconditional)
#'     \item "proportion.offset"        - Proportions: Inequality (offset), two independent groups
#'     \item "proportion.sign"          - Proportion: Sign test (binomial test)
#'     \item "generic.binomial"         - Generic binomial test
#'   }
#' @param analysis Type of power analysis. One of:
#'   \itemize{
#'     \item "a_priori"    - Solve for sample size
#'     \item "post_hoc"    - Solve for power
#'     \item "sensitivity" - Solve for effect size
#'     \item "criterion"   - Solve for alpha
#'     \item "compromise"  - Solve for alpha and power
#'   }
#' @param effect_size Effect size g (= p1 - p0) for proportion tests.
#'   For correlation tests: r.
#' @param n Total sample size.
#' @param alpha Significance level. Default 0.05.
#' @param power Desired power (1 - beta). Default 0.80.
#' @param p0 Null hypothesis proportion. Default 0.50.
#' @param p1 Alternative hypothesis proportion. If provided with p0,
#'   effect size g is computed automatically as p1 - p0.
#' @param tails Number of tails. 1 or 2. Default 2.
#' @param beta_alpha_ratio For compromise only: ratio of beta to alpha. Default 1.
#' @param plot Logical. Whether to print plots. Default TRUE.
#' @param plot Logical. Whether to print plots. Default TRUE.
#'
#' @return A list of class "gpr_result" with all parameters and plots.
#' @export
#'
#' @examples
#' # See vignette for examples
gpr_exact <- function(test             = "proportion.one.sample",
                      analysis         = "a_priori",
                      effect_size      = NULL,
                      n                = NULL,
                      alpha            = NULL,
                      power            = NULL,
                      p0               = 0.50,
                      p1               = NULL,
                      tails            = 2,
                      beta_alpha_ratio = 1,
                      plot = TRUE) {

  # -- Step 1: Validate inputs --------------------------------------------------
  valid_analyses <- c("a_priori", "post_hoc", "sensitivity", "criterion", "compromise")
  if (!analysis %in% valid_analyses)
    stop(sprintf("'analysis' must be one of: %s", paste(valid_analyses, collapse = ", ")))

  valid_tests <- c("correlation.bivariate", "regression.random",
                   "proportion.one.sample", "proportion.mcnemar",
                   "proportion.fisher", "proportion.unconditional",
                   "proportion.offset", "proportion.sign",
                   "generic.binomial")
  if (!test %in% valid_tests)
    stop(sprintf("'test' must be one of: %s", paste(valid_tests, collapse = ", ")))

  if (!tails %in% c(1, 2))
    stop("tails must be 1 or 2")

  # -- Step 2: Compute effect size g from p0 and p1 if needed ------------------
  # Effect size g = p1 - p0 (G*Power convention for exact binomial tests)
  p0_orig <- p0
  p1_orig <- p1

  is_binomial <- test %in% c("proportion.one.sample", "proportion.sign",
                             "generic.binomial")
  is_corr     <- test %in% c("correlation.bivariate", "regression.random")

  if (is_binomial) {
    if (!is.null(p1) && !is.null(p0)) {
      # Compute effect size g = p1 - p0
      effect_size <- p1 - p0
    } else if (!is.null(effect_size) && !is.null(p0)) {
      # Compute p1 from g and p0
      p1_orig <- p0 + effect_size
    } else if (is.null(effect_size)) {
      stop("Proportion tests require p0 and p1, or effect_size g (= p1 - p0) and p0")
    }
  }

  # For correlation tests convert r to Fisher's z
  if (is_corr) {
    if (is.null(effect_size))
      stop("Correlation tests require effect_size (r)")
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

  # -- Step 4: Core power computation ------------------------------------------
  compute_power <- function(n, es, alpha) {
    n <- round(n)   # force integer for binomial

    if (is_binomial) {
      # Exact binomial test using G*Power's g = p1 - p0 convention
      p_null <- p0_orig
      p_alt  <- p0_orig + es   # p1 = p0 + g

      if (tails == 2) {
        # Find critical region under H0
        crit_low  <- qbinom(alpha / 2, n, p_null) - 1
        crit_high <- qbinom(1 - alpha / 2, n, p_null)
        # Power = P(X <= crit_low | p_alt) + P(X > crit_high | p_alt)
        pbinom(crit_low, n, p_alt) +
          pbinom(crit_high, n, p_alt, lower.tail = FALSE)
      } else {
        if (p_alt > p_null) {
          crit_high <- qbinom(1 - alpha, n, p_null, lower.tail = FALSE)
          pbinom(crit_high - 1, n, p_alt, lower.tail = FALSE)
        } else {
          crit_low <- qbinom(alpha, n, p_null)
          pbinom(crit_low, n, p_alt)
        }
      }

    } else if (is_corr) {
      # Noncentral t via Fisher's z transformation
      ez   <- if (!is.null(effect_size_z)) effect_size_z else
        0.5 * log((1 + es) / (1 - es))
      ncp  <- ez * sqrt(n - 3)
      df   <- n - 2
      crit <- if (tails == 2) qt(1 - alpha / 2, df) else qt(1 - alpha, df)

      if (tails == 2) {
        pt(crit,  df, ncp = ncp, lower.tail = FALSE) +
          pt(-crit, df, ncp = ncp, lower.tail = TRUE)
      } else {
        pt(crit, df, ncp = ncp, lower.tail = FALSE)
      }

    } else {
      # Normal approximation for McNemar, Fisher, unconditional, offset
      ncp  <- es * sqrt(n)
      crit <- if (tails == 2) qnorm(1 - alpha / 2) else qnorm(1 - alpha)

      if (tails == 2) {
        pnorm(crit - ncp, lower.tail = FALSE) +
          pnorm(-crit - ncp, lower.tail = TRUE)
      } else {
        pnorm(crit - ncp, lower.tail = FALSE)
      }
    }
  }

  # -- Step 5: Solve for missing parameter -------------------------------------
  es <- if (!is.null(effect_size_z) && is_corr) effect_size_z else effect_size

  if (analysis == "a_priori") {
    if (is_binomial) {
      # Exact binomial is discrete - search integer n directly
      n <- 2
      repeat {
        if (compute_power(n, es, alpha) >= power) break
        n <- n + 1
        if (n > 1e5) stop("Could not find solution - try different parameters")
      }
      power <- compute_power(n, es, alpha)
    } else {
      res   <- uniroot(function(n) compute_power(n, es, alpha) - power,
                       c(4, 1e6), tol = 1e-6)
      n     <- ceiling(res$root)
      power <- compute_power(n, es, alpha)
    }

  } else if (analysis == "post_hoc") {
    power <- compute_power(n, es, alpha)

  } else if (analysis == "sensitivity") {
    res         <- uniroot(function(e) compute_power(n, e, alpha) - power,
                           c(1e-6, 10), tol = 1e-6)
    effect_size <- res$root
    es          <- effect_size

  } else if (analysis == "criterion") {
    res   <- uniroot(function(a) compute_power(n, es, a) - power,
                     c(1e-10, 1 - 1e-10), tol = 1e-6)
    alpha <- res$root

  } else if (analysis == "compromise") {
    res   <- uniroot(
      function(a) compute_power(n, es, a) - (1 - beta_alpha_ratio * a),
      c(1e-10, 1 / (1 + beta_alpha_ratio) - 1e-10), tol = 1e-6)
    alpha <- res$root
    power <- 1 - beta_alpha_ratio * alpha
  }

  # -- Step 6: Build results ----------------------------------------------------
  beta_val <- round(1 - power, 4)

  # Compute critical N for binomial tests
  if (is_binomial) {
    # G*Power convention: find critical region edges
    crit_low_n   <- qbinom(alpha / 2, n, p0_orig) - 1
    crit_high_n  <- n - qbinom(alpha / 2, n, p0_orig) + 1
    actual_alpha <- pbinom(crit_low_n, n, p0_orig) +
      pbinom(crit_high_n - 1, n, p0_orig, lower.tail = FALSE)
  } else {
    crit_low_n  <- NULL
    crit_high_n <- NULL
    actual_alpha <- alpha
  }

  results <- list(
    test             = test,
    analysis         = analysis,
    tails            = tails,
    effect_size      = round(effect_size, 4),
    p0               = p0_orig,
    p1               = p1_orig,
    n                = n,
    total_n          = n,
    alpha            = round(alpha, 6),
    actual_alpha     = round(actual_alpha, 7),
    power            = round(power, 7),
    beta             = beta_val,
    beta_alpha_ratio = if (analysis == "compromise") round(beta_val / alpha, 4) else NULL,
    lower_critical_n = crit_low_n,
    upper_critical_n = crit_high_n
  )

  class(results) <- "gpr_result"

  # -- Step 7: Print summary ----------------------------------------------------
  analysis_labels <- c(
    a_priori    = "A Priori: Compute required sample size",
    post_hoc    = "Post Hoc: Compute achieved power",
    sensitivity = "Sensitivity: Compute required effect size",
    criterion   = "Criterion: Compute required alpha",
    compromise  = "Compromise: Compute implied alpha & power"
  )

  cat("\n-- gpr: Power Analysis for Exact Tests --------------------------\n")
  cat(sprintf("  Test:                %s\n", test))
  cat(sprintf("  Analysis:            %s\n", analysis_labels[analysis]))
  cat(sprintf("  Tails:               %d\n", tails))
  cat("------------------------------------------------------------------\n")
  cat(sprintf("  Effect size (g):     %.4f\n", results$effect_size))
  if (!is.null(p0_orig))
    cat(sprintf("  P (H0):              %.4f\n", p0_orig))
  if (!is.null(p1_orig))
    cat(sprintf("  P (H1):              %.4f\n", p1_orig))
  cat(sprintf("  Alpha (\u03b1):           %.6f\n", results$alpha))
  cat(sprintf("  Actual \u03b1:            %.7f\n", results$actual_alpha))
  cat(sprintf("  Power (1-\u03b2):         %.7f\n", results$power))
  cat(sprintf("  Beta (\u03b2):            %.4f\n", results$beta))
  if (analysis == "compromise")
    cat(sprintf("  \u03b2/\u03b1 ratio:           %.4f\n", results$beta_alpha_ratio))
  if (!is.null(crit_low_n))
    cat(sprintf("  Lower critical N:    %d\n",   results$lower_critical_n))
  if (!is.null(crit_high_n))
    cat(sprintf("  Upper critical N:    %d\n",   results$upper_critical_n))
  cat(sprintf("  Total sample size:   %d\n",   results$total_n))
  cat("------------------------------------------------------------------\n\n")

  # -- Step 8: Distribution plot ------------------------------------------------
  if (is_binomial) {
    p_null <- p0_orig
    p_alt  <- p0_orig + effect_size

    # Show binomial distributions around the sample size
    x_vals   <- max(0, round(n * p_null) - round(n * 0.4)):
      min(n, round(n * p_alt) + round(n * 0.4))
    h0_probs <- dbinom(x_vals, n, p_null)
    h1_probs <- dbinom(x_vals, n, p_alt)

    crit_h <- qbinom(1 - alpha / (if (tails == 2) 2 else 1),
                     n, p_null, lower.tail = FALSE)
    crit_l <- if (tails == 2) qbinom(alpha / 2, n, p_null) else -1

    dist_data <- data.frame(
      x    = rep(x_vals, 2),
      prob = c(h0_probs, h1_probs),
      dist = rep(c("H0 (p = p0)", "H1 (p = p1)"), each = length(x_vals))
    )

    p1_plot <- ggplot2::ggplot(dist_data,
                               ggplot2::aes(x = x, y = prob, fill = dist)) +
      ggplot2::geom_col(position = "identity", alpha = 0.6, width = 0.8) +
      ggplot2::geom_vline(xintercept = crit_h, linetype = "dashed",
                          color = "gray30", linewidth = 0.8) +
      {if (tails == 2)
        ggplot2::geom_vline(xintercept = crit_l, linetype = "dashed",
                            color = "gray30", linewidth = 0.8)} +
      ggplot2::annotate("text", x = crit_h, y = max(h0_probs) * 0.95,
                        label = sprintf("critical N = %d", crit_h),
                        hjust = -0.05, size = 3.5, color = "gray30") +
      ggplot2::scale_fill_manual(values = c(
        "H0 (p = p0)" = "#D94701",
        "H1 (p = p1)" = "#2171B5"
      )) +
      ggplot2::labs(
        title    = "Binomial Distributions - Exact Test",
        subtitle = bquote("p0 =" ~ .(round(p_null,2)) ~
                            "| p1 =" ~ .(round(p_alt,2)) ~
                            "| N =" ~ .(n) ~
                            "| Power =" ~ .(round(results$power,4))),
        x        = "Number of successes",
        y        = "Probability",
        fill     = NULL
      ) +
      ggplot2::theme_minimal(base_size = 13) +
      ggplot2::theme(
        plot.title       = ggplot2::element_text(face = "bold"),
        plot.subtitle    = ggplot2::element_text(color = "grey40"),
        legend.position  = "top",
        panel.grid.minor = ggplot2::element_blank()
      )

    if (plot) print(p1_plot)

  } else {
    # Normal/t distribution plot for correlation and other tests
    ez   <- if (is_corr) effect_size_z else effect_size
    ncp  <- ez * sqrt(max(n - 3, 1))
    crit <- if (tails == 2) qnorm(1 - alpha / 2) else qnorm(1 - alpha)

    x_min <- min(-4, ncp - 4)
    x_max <- max(4,  ncp + 4)
    x     <- seq(x_min, x_max, length.out = 1000)

    h0_density <- dnorm(x, mean = 0,   sd = 1)
    h1_density <- dnorm(x, mean = ncp, sd = 1)

    dist_data <- data.frame(
      x            = rep(x, 2),
      density      = c(h0_density, h1_density),
      distribution = rep(c("H0", "H1"), each = length(x))
    )

    alpha_region <- data.frame(x = x[x >= crit],  density = h0_density[x >= crit])
    beta_region  <- data.frame(x = x[x <= crit],  density = h1_density[x <= crit])
    alpha_left   <- data.frame(x = x[x <= -crit], density = h0_density[x <= -crit])

    p1_plot <- ggplot2::ggplot(dist_data,
                               ggplot2::aes(x = x, y = density,
                                            color = distribution)) +
      ggplot2::geom_area(data = beta_region,
                         ggplot2::aes(x = x, y = density),
                         fill = "#6BAED6", alpha = 0.5, inherit.aes = FALSE) +
      ggplot2::geom_area(data = alpha_region,
                         ggplot2::aes(x = x, y = density),
                         fill = "#FC8D59", alpha = 0.5, inherit.aes = FALSE) +
      {if (tails == 2)
        ggplot2::geom_area(data = alpha_left,
                           ggplot2::aes(x = x, y = density),
                           fill = "#FC8D59", alpha = 0.5, inherit.aes = FALSE)} +
      ggplot2::geom_line(linewidth = 1.1) +
      ggplot2::geom_vline(xintercept = crit, linetype = "dashed",
                          color = "gray30", linewidth = 0.8) +
      ggplot2::scale_color_manual(values = c(
        "H0" = "#D94701", "H1" = "#2171B5")) +
      ggplot2::labs(
        title    = sprintf("Distribution Plot \u2014 %s", test),
        subtitle = sprintf("Effect = %.2f | N = %d | \u03b1 = %.4f | Power = %.4f",
                           results$effect_size, n, results$alpha, results$power),
        x = "z", y = "Density", color = NULL
      ) +
      ggplot2::theme_minimal(base_size = 13) +
      ggplot2::theme(
        plot.title       = ggplot2::element_text(face = "bold"),
        plot.subtitle    = ggplot2::element_text(color = "grey40"),
        legend.position  = "top",
        panel.grid.minor = ggplot2::element_blank()
      )

    if (plot) print(p1_plot)
  }

  # -- Step 9: Power curve ------------------------------------------------------
  n_seq     <- seq(2, max(n * 2, 50), length.out = 200)
  power_seq <- sapply(n_seq, function(ni) {
    tryCatch(compute_power(ni, es, alpha), error = function(e) NA)
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
      subtitle = bquote("g =" ~ .(round(results$effect_size,2)) ~
                          "| p0 =" ~ .(p0_orig) ~
                          "| " ~ alpha == .(round(results$alpha,4)) ~
                          "| tails =" ~ .(tails)),
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
