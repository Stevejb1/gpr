#' Effect Size Conversions and Calculators
#'
#' A comprehensive set of effect size conversion functions similar to the
#' "Determine" button in G*Power 3.1 (Faul et al., 2007). Converts between
#' different effect size measures and computes effect sizes from raw values.
#'
#' @name effect_sizes
NULL

# \u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550
# COHEN'S d - for t-tests
# \u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550

#' Compute Cohen's d from means and standard deviations
#'
#' @param m1 Mean of group 1
#' @param m2 Mean of group 2
#' @param sd1 Standard deviation of group 1
#' @param sd2 Standard deviation of group 2 (if NULL uses sd1)
#' @param n1 Sample size of group 1 (for pooled SD)
#' @param n2 Sample size of group 2 (for pooled SD)
#' @return Cohen's d
#' @export
#' @examples
#' cohens_d(m1 = 10, m2 = 12, sd1 = 4, sd2 = 4)
cohens_d <- function(m1, m2, sd1, sd2 = NULL, n1 = NULL, n2 = NULL) {
  if (is.null(sd2)) sd2 <- sd1
  if (!is.null(n1) && !is.null(n2)) {
    sd_pooled <- sqrt(((n1 - 1) * sd1^2 + (n2 - 1) * sd2^2) / (n1 + n2 - 2))
  } else {
    sd_pooled <- sqrt((sd1^2 + sd2^2) / 2)
  }
  d <- abs(m1 - m2) / sd_pooled
  cat("\n-- gpr: Cohen's d -----------------------------------------------\n")
  cat(sprintf("  Mean 1:              %.4f\n", m1))
  cat(sprintf("  Mean 2:              %.4f\n", m2))
  cat(sprintf("  SD (pooled):         %.4f\n", sd_pooled))
  cat(sprintf("  Cohen's d:           %.4f\n", d))
  cat(sprintf("  Interpretation:      %s\n",   interpret_d(d)))
  cat("------------------------------------------------------------------\n\n")
  invisible(d)
}

#' Convert Cohen's d to Pearson r
#' @param d Cohen's d
#' @return Pearson r
#' @export
#' @examples
#' d_to_r(0.5)
d_to_r <- function(d) {
  r <- d / sqrt(d^2 + 4)
  cat(sprintf("\n  Cohen's d = %.4f  ->  Pearson r = %.4f\n\n", d, r))
  invisible(r)
}

#' Convert Pearson r to Cohen's d
#' @param r Pearson r
#' @return Cohen's d
#' @export
#' @examples
#' r_to_d(0.243)
r_to_d <- function(r) {
  d <- 2 * r / sqrt(1 - r^2)
  cat(sprintf("\n  Pearson r = %.4f  ->  Cohen's d = %.4f\n\n", r, d))
  invisible(d)
}

#' Interpret Cohen's d magnitude
#' @param d Cohen's d
#' @return Character string interpretation
#' @keywords internal
interpret_d <- function(d) {
  d <- abs(d)
  if (d < 0.2)      "Negligible"
  else if (d < 0.5) "Small"
  else if (d < 0.8) "Medium"
  else               "Large"
}

# \u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550
# COHEN'S f - for ANOVA / F-tests
# \u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550

#' Compute Cohen's f from group means and SD
#'
#' @param means Vector of group means
#' @param sd_within Within-group standard deviation
#' @param n Vector of group sample sizes (or single n for equal groups)
#' @return Cohen's f
#' @export
#' @examples
#' cohens_f(means = c(10, 12, 14), sd_within = 8)
cohens_f <- function(means, sd_within, n = NULL) {
  k <- length(means)
  if (is.null(n)) n <- rep(1, k)
  if (length(n) == 1) n <- rep(n, k)
  grand_mean <- weighted.mean(means, n)
  sd_between <- sqrt(sum(n * (means - grand_mean)^2) / sum(n))
  f          <- sd_between / sd_within
  cat("\n-- gpr: Cohen's f -----------------------------------------------\n")
  cat(sprintf("  Number of groups:    %d\n",   k))
  cat(sprintf("  Grand mean:          %.4f\n", grand_mean))
  cat(sprintf("  SD between:          %.4f\n", sd_between))
  cat(sprintf("  SD within:           %.4f\n", sd_within))
  cat(sprintf("  Cohen's f:           %.4f\n", f))
  cat(sprintf("  Interpretation:      %s\n",   interpret_f(f)))
  cat("------------------------------------------------------------------\n\n")
  invisible(f)
}

#' Convert eta-squared to Cohen's f
#' @param eta2 Eta-squared
#' @return Cohen's f
#' @export
#' @examples
#' eta2_to_f(0.06)
eta2_to_f <- function(eta2) {
  f <- sqrt(eta2 / (1 - eta2))
  cat(sprintf("\n  Eta-squared = %.4f  ->  Cohen's f = %.4f\n\n", eta2, f))
  invisible(f)
}

#' Convert Cohen's f to eta-squared
#' @param f Cohen's f
#' @return Eta-squared
#' @export
#' @examples
#' f_to_eta2(0.25)
f_to_eta2 <- function(f) {
  eta2 <- f^2 / (1 + f^2)
  cat(sprintf("\n  Cohen's f = %.4f  ->  Eta-squared = %.4f\n\n", f, eta2))
  invisible(eta2)
}

#' Convert partial eta-squared to Cohen's f
#' @param partial_eta2 Partial eta-squared
#' @return Cohen's f
#' @export
#' @examples
#' partial_eta2_to_f(0.06)
partial_eta2_to_f <- function(partial_eta2) {
  f <- sqrt(partial_eta2 / (1 - partial_eta2))
  cat(sprintf("\n  Partial eta-squared = %.4f  ->  Cohen's f = %.4f\n\n",
              partial_eta2, f))
  invisible(f)
}

#' Convert Cohen's f to partial eta-squared
#' @param f Cohen's f
#' @return Partial eta-squared
#' @export
#' @examples
#' f_to_partial_eta2(0.25)
f_to_partial_eta2 <- function(f) {
  peta2 <- f^2 / (1 + f^2)
  cat(sprintf("\n  Cohen's f = %.4f  ->  Partial eta-squared = %.4f\n\n",
              f, peta2))
  invisible(peta2)
}

#' Convert omega-squared to Cohen's f
#' @param omega2 Omega-squared
#' @return Cohen's f
#' @export
#' @examples
#' omega2_to_f(0.06)
omega2_to_f <- function(omega2) {
  f <- sqrt(omega2 / (1 - omega2))
  cat(sprintf("\n  Omega-squared = %.4f  ->  Cohen's f = %.4f\n\n", omega2, f))
  invisible(f)
}

#' Convert Cohen's f to omega-squared
#' @param f Cohen's f
#' @return Omega-squared
#' @export
#' @examples
#' f_to_omega2(0.25)
f_to_omega2 <- function(f) {
  omega2 <- f^2 / (1 + f^2)
  cat(sprintf("\n  Cohen's f = %.4f  ->  Omega-squared = %.4f\n\n", f, omega2))
  invisible(omega2)
}

#' Interpret Cohen's f magnitude
#' @param f Cohen's f
#' @return Character string
#' @keywords internal
interpret_f <- function(f) {
  f <- abs(f)
  if (f < 0.10)      "Negligible"
  else if (f < 0.25) "Small"
  else if (f < 0.40) "Medium"
  else                "Large"
}

# \u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550
# COHEN'S w - for chi-square tests
# \u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550

#' Compute Cohen's w from observed and expected proportions
#'
#' @param p_obs Vector of observed proportions
#' @param p_exp Vector of expected proportions under H0
#' @return Cohen's w
#' @export
#' @examples
#' cohens_w(p_obs = c(0.40, 0.35, 0.25), p_exp = c(0.33, 0.33, 0.34))
cohens_w <- function(p_obs, p_exp) {
  if (abs(sum(p_obs) - 1) > 1e-6) stop("p_obs must sum to 1")
  if (abs(sum(p_exp) - 1) > 1e-6) stop("p_exp must sum to 1")
  w <- sqrt(sum((p_obs - p_exp)^2 / p_exp))
  cat("\n-- gpr: Cohen's w -----------------------------------------------\n")
  cat(sprintf("  Cohen's w:           %.4f\n", w))
  cat(sprintf("  Interpretation:      %s\n",   interpret_w(w)))
  cat("------------------------------------------------------------------\n\n")
  invisible(w)
}

#' Interpret Cohen's w magnitude
#' @param w Cohen's w
#' @return Character string
#' @keywords internal
interpret_w <- function(w) {
  w <- abs(w)
  if (w < 0.10)      "Negligible"
  else if (w < 0.30) "Small"
  else if (w < 0.50) "Medium"
  else                "Large"
}

# \u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550
# COHEN'S h - for proportions
# \u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550

#' Compute Cohen's h from two proportions
#'
#' @param p1 First proportion
#' @param p2 Second proportion
#' @return Cohen's h
#' @export
#' @examples
#' cohens_h(0.50, 0.70)
cohens_h <- function(p1, p2) {
  h <- abs(2 * asin(sqrt(p1)) - 2 * asin(sqrt(p2)))
  cat("\n-- gpr: Cohen's h -----------------------------------------------\n")
  cat(sprintf("  Proportion 1:        %.4f\n", p1))
  cat(sprintf("  Proportion 2:        %.4f\n", p2))
  cat(sprintf("  Cohen's h:           %.4f\n", h))
  cat(sprintf("  Interpretation:      %s\n",   interpret_h(h)))
  cat("------------------------------------------------------------------\n\n")
  invisible(h)
}

#' Interpret Cohen's h magnitude
#' @param h Cohen's h
#' @return Character string
#' @keywords internal
interpret_h <- function(h) {
  h <- abs(h)
  if (h < 0.20)      "Negligible"
  else if (h < 0.50) "Small"
  else if (h < 0.80) "Medium"
  else                "Large"
}

# \u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550
# PEARSON r - for correlations
# \u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550

#' Interpret Pearson r magnitude
#'
#' @param r Pearson r correlation coefficient
#' @return Named list with r, r2, and interpretation
#' @export
#' @examples
#' interpret_r(0.35)
interpret_r <- function(r) {
  r2 <- r^2
  interp <- if (abs(r) < 0.10)      "Negligible"
  else if (abs(r) < 0.30) "Small"
  else if (abs(r) < 0.50) "Medium"
  else                     "Large"
  cat("\n-- gpr: Pearson r -----------------------------------------------\n")
  cat(sprintf("  Pearson r:           %.4f\n", r))
  cat(sprintf("  R-squared:           %.4f\n", r2))
  cat(sprintf("  Interpretation:      %s\n",   interp))
  cat("------------------------------------------------------------------\n\n")
  invisible(list(r = r, r2 = r2, interpretation = interp))
}

#' Convert Pearson r to Fisher's z
#' @param r Pearson r
#' @return Fisher's z
#' @export
#' @examples
#' r_to_z(0.35)
r_to_z <- function(r) {
  z <- 0.5 * log((1 + r) / (1 - r))
  cat(sprintf("\n  Pearson r = %.4f  ->  Fisher's z = %.4f\n\n", r, z))
  invisible(z)
}

#' Convert Fisher's z to Pearson r
#' @param z Fisher's z
#' @return Pearson r
#' @export
#' @examples
#' z_to_r(0.3654)
z_to_r <- function(z) {
  r <- (exp(2 * z) - 1) / (exp(2 * z) + 1)
  cat(sprintf("\n  Fisher's z = %.4f  ->  Pearson r = %.4f\n\n", z, r))
  invisible(r)
}

# \u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550
# ODDS RATIO - for logistic regression
# \u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550

#' Convert odds ratio to Cohen's d
#' @param or Odds ratio
#' @return Cohen's d
#' @export
#' @examples
#' or_to_d(1.68)
or_to_d <- function(or) {
  d <- log(or) * sqrt(3) / pi
  cat(sprintf("\n  Odds ratio = %.4f  ->  Cohen's d = %.4f\n\n", or, d))
  invisible(d)
}

#' Convert Cohen's d to odds ratio
#' @param d Cohen's d
#' @return Odds ratio
#' @export
#' @examples
#' d_to_or(0.5)
d_to_or <- function(d) {
  or <- exp(d * pi / sqrt(3))
  cat(sprintf("\n  Cohen's d = %.4f  ->  Odds ratio = %.4f\n\n", d, or))
  invisible(or)
}

#' Convert odds ratio to Pearson r
#' @param or Odds ratio
#' @return Pearson r
#' @export
#' @examples
#' or_to_r(1.68)
or_to_r <- function(or) {
  d <- log(or) * sqrt(3) / pi
  r <- d / sqrt(d^2 + 4)
  cat(sprintf("\n  Odds ratio = %.4f  ->  Pearson r = %.4f\n\n", or, r))
  invisible(r)
}

# \u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550
# R\u00b2 - for regression
# \u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550

#' Convert R-squared to Cohen's f-squared
#'
#' @param r2 R-squared value
#' @return Cohen's f-squared (f2)
#' @export
#' @examples
#' r2_to_f2(0.13)
r2_to_f2 <- function(r2) {
  f2 <- r2 / (1 - r2)
  cat("\n-- gpr: R-squared to Cohen's f-squared --------------------------\n")
  cat(sprintf("  R-squared:           %.4f\n", r2))
  cat(sprintf("  Cohen's f-squared:   %.4f\n", f2))
  cat(sprintf("  Cohen's f:           %.4f\n", sqrt(f2)))
  cat(sprintf("  Interpretation:      %s\n",   interpret_f2(f2)))
  cat("------------------------------------------------------------------\n\n")
  invisible(f2)
}

#' Convert Cohen's f-squared to R-squared
#' @param f2 Cohen's f-squared
#' @return R-squared
#' @export
#' @examples
#' f2_to_r2(0.15)
f2_to_r2 <- function(f2) {
  r2 <- f2 / (1 + f2)
  cat(sprintf("\n  Cohen's f-squared = %.4f  ->  R-squared = %.4f\n\n", f2, r2))
  invisible(r2)
}

#' Interpret Cohen's f-squared magnitude
#' @param f2 Cohen's f-squared
#' @return Character string
#' @keywords internal
interpret_f2 <- function(f2) {
  if (f2 < 0.02)      "Negligible"
  else if (f2 < 0.15) "Small"
  else if (f2 < 0.35) "Medium"
  else                 "Large"
}

# \u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550
# COMPREHENSIVE EFFECT SIZE SUMMARY
# \u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550

#' Show all effect size benchmarks
#'
#' Prints a comprehensive table of Cohen's effect size benchmarks
#' for all major test families.
#'
#' @export
#' @examples
#' effect_size_benchmarks()
effect_size_benchmarks <- function() {
  cat("\n-- gpr: Effect Size Benchmarks (Cohen, 1988) --------------------\n\n")
  cat("  t-tests (Cohen's d):\n")
  cat("    Small  = 0.20\n")
  cat("    Medium = 0.50\n")
  cat("    Large  = 0.80\n\n")
  cat("  ANOVA / F-tests (Cohen's f):\n")
  cat("    Small  = 0.10\n")
  cat("    Medium = 0.25\n")
  cat("    Large  = 0.40\n\n")
  cat("  Chi-square (Cohen's w):\n")
  cat("    Small  = 0.10\n")
  cat("    Medium = 0.30\n")
  cat("    Large  = 0.50\n\n")
  cat("  Proportions (Cohen's h):\n")
  cat("    Small  = 0.20\n")
  cat("    Medium = 0.50\n")
  cat("    Large  = 0.80\n\n")
  cat("  Correlations (Pearson r):\n")
  cat("    Small  = 0.10\n")
  cat("    Medium = 0.30\n")
  cat("    Large  = 0.50\n\n")
  cat("  Regression (Cohen's f-squared):\n")
  cat("    Small  = 0.02\n")
  cat("    Medium = 0.15\n")
  cat("    Large  = 0.35\n\n")
  cat("  Eta-squared / Partial eta-squared / Omega-squared:\n")
  cat("    Small  = 0.01\n")
  cat("    Medium = 0.06\n")
  cat("    Large  = 0.14\n")
  cat("------------------------------------------------------------------\n\n")
}

# \u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550
# UNIFIED EFFECT SIZE CONVERTER
# \u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550

#' Convert between effect size measures
#'
#' A unified converter that takes any effect size measure and converts
#' it to one or more output measures. Similar to G*Power's Determine button.
#'
#' @param value Numeric value of the input effect size
#' @param from Input effect size type. One of:
#'   "d", "f", "f2", "w", "h", "r", "r2", "eta2",
#'   "partial_eta2", "omega2", "or" (odds ratio)
#' @param to Output effect size type(s). One or more of the same options.
#'   Default is "all" which shows all conversions.
#' @return Named list of converted effect sizes
#' @export
#' @examples
#' # Convert Cohen's d to everything
#' gpr_effect_size(0.5, from = "d")
#'
#' # Convert r to d and f
#' gpr_effect_size(0.3, from = "r", to = c("d", "f"))
#'
#' # Convert eta2 to f
#' gpr_effect_size(0.06, from = "eta2", to = "f")
gpr_effect_size <- function(value, from = "d", to = "all") {

  valid_types <- c("d", "f", "f2", "w", "h", "r", "r2",
                   "eta2", "partial_eta2", "omega2", "or")

  if (!from %in% valid_types)
    stop(sprintf("'from' must be one of: %s", paste(valid_types, collapse = ", ")))

  if (!identical(to, "all") && !all(to %in% valid_types))
    stop(sprintf("'to' must be 'all' or one or more of: %s",
                 paste(valid_types, collapse = ", ")))

  # -- Step 1: Convert input to Cohen's d as common currency -------------------
  d <- switch(from,
              "d"            = value,
              "r"            = 2 * value / sqrt(1 - value^2),
              "r2"           = 2 * sqrt(value) / sqrt(1 - value),
              "f"            = value * 2,
              "f2"           = 2 * sqrt(value),
              "eta2"         = 2 * sqrt(value / (1 - value)),
              "partial_eta2" = 2 * sqrt(value / (1 - value)),
              "omega2"       = 2 * sqrt(value / (1 - value)),
              "or"           = log(value) * sqrt(3) / pi,
              "w"            = value,
              "h"            = value,
              stop("Unknown 'from' type")
  )

  # -- Step 2: Convert d to all output measures ---------------------------------
  r_val      <- d / sqrt(d^2 + 4)
  r2_val     <- r_val^2
  f_val      <- abs(d) / 2
  f2_val     <- f_val^2
  eta2_val   <- f2_val / (1 + f2_val)
  omega2_val <- eta2_val
  or_val     <- exp(d * pi / sqrt(3))
  w_val      <- abs(d)
  h_val      <- abs(d)

  all_results <- list(
    d             = round(d, 4),
    r             = round(r_val, 4),
    r2            = round(r2_val, 4),
    f             = round(f_val, 4),
    f2            = round(f2_val, 4),
    eta2          = round(eta2_val, 4),
    partial_eta2  = round(eta2_val, 4),
    omega2        = round(omega2_val, 4),
    or            = round(or_val, 4),
    w             = round(w_val, 4),
    h             = round(h_val, 4)
  )

  # -- Step 3: Filter to requested outputs -------------------------------------
  if (identical(to, "all")) {
    output <- all_results
  } else {
    output <- all_results[to]
  }

  # -- Step 4: Print results ----------------------------------------------------
  labels <- c(
    d             = "Cohen's d          ",
    r             = "Pearson r          ",
    r2            = "R-squared          ",
    f             = "Cohen's f          ",
    f2            = "Cohen's f-squared  ",
    eta2          = "Eta-squared        ",
    partial_eta2  = "Partial eta-squared",
    omega2        = "Omega-squared      ",
    or            = "Odds ratio         ",
    w             = "Cohen's w          ",
    h             = "Cohen's h          "
  )

  cat("\n-- gpr: Effect Size Converter -----------------------------------\n")
  cat(sprintf("  Input:  %s = %.4f\n\n", labels[from], value))
  cat("  Conversions:\n")
  for (nm in names(output)) {
    if (nm != from) {
      cat(sprintf("    %s = %.4f\n", labels[nm], output[[nm]]))
    }
  }
  cat("------------------------------------------------------------------\n\n")

  invisible(output)
}
