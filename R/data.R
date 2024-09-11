#' @title Indicator Weights and Categorizations
#' @description The relative weights and categorizations (i.e. EBM Pillars and Objectives, and indicator bins) for all indicators.Indicators that are part of more than one bin should be listed multiple times.
#' @format A data frame with 129 rows and 5 variables:
#' \describe{
#'   \item{\code{indicator_name}}{character the name of the indicator}
#'   \item{\code{indicator_bin}}{character the name of the indicator bin.}
#'   \item{\code{applicability}}{character the geographic applicability of the indicator}
#'   \item{\code{objective}}{character the name of the EBM objective of the bin}
#'   \item{\code{weight}}{double the relative weight}
#'}
#' @details DETAILS
"indicator_weights"
