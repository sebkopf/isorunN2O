#' @title Process IRMS runs of N2O analyses
#' @description This package facilitates the reading, processing and visualization of N2O isotope data including drift correction, 17O correction and standards calibration.
#'
#' To get started, take alook at the tutorial R markdown file.
#'
#' @name isorunN2O
#' @docType package
#' @title isorunN2O package
#' @author Sebastian Kopf
#'
#' @include loading.R
#' @include processing.R
#' @include O17.R
#' @include drift.R
#' @include calculations.R
#' @include plotting.R
#' @importFrom tidyr gather spread extract gather_ spread_ extract_
#' @importFrom lazyeval interp lazy_dots lazy_eval
#' @importFrom ggplot2 ggplot aes aes_string geom_smooth geom_point theme facet_wrap theme_bw theme labs
NULL
