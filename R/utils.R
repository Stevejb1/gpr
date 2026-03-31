#' gpr package
#'
#' @docType package
#' @name gpr-package
#' @importFrom stats dbinom dchisq df dnorm dt pbinom pchisq pf pnorm pt qbinom qchisq qf qnorm qt uniroot weighted.mean
#' @importFrom ggplot2 ggplot aes geom_line geom_area geom_col geom_vline geom_hline annotate scale_y_continuous scale_color_manual scale_fill_manual labs theme_minimal theme element_text element_blank
#' @importFrom scales percent_format
NULL

# Suppress R CMD check notes for ggplot2 column names
utils::globalVariables(c('density', 'distribution', 'dist', 'prob'))
