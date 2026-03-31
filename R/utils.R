#' gpr: General Power for R
#'
#' @name gpr-package
#' @aliases gpr-package
#' @importFrom stats dbinom dchisq df dnorm dt pbinom pchisq pf pnorm
#'   pt qbinom qchisq qf qnorm qt uniroot weighted.mean
#' @importFrom ggplot2 ggplot aes geom_line geom_area geom_col
#'   geom_vline geom_hline annotate scale_y_continuous
#'   scale_color_manual scale_fill_manual labs theme_minimal theme
#'   element_text element_blank stat_function
#' @importFrom scales percent_format
#' @importFrom shiny req
NULL

utils::globalVariables(c('density', 'distribution', 'dist', 'prob', 'y', 'x'))
