% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calculations.R
\name{calibrate_d18}
\alias{calibrate_d18}
\title{Calibrate organic d15 values with the given standards}
\usage{
calibrate_d18(
  data,
  d18,
  amount = amount,
  volume = volume,
  cell_volume,
  standards = c(`USGS-34` = -27.93, `IAEA-NO3` = 25.61),
  quiet = FALSE
)
}
\arguments{
\item{data}{(can be a grouped_by data set)}

\item{d18}{the d18 column}

\item{amount}{the amount column}

\item{volume}{the volume column}

\item{cell_volume}{the volume of cells in the vial (has to be same units as volume!)}

\item{standards}{a set of isotope standards
Note: they are matched to the data by "category" (not by name)}

\item{d15}{the d15 column (can be a column already calibrated for denitrifier method)}

\item{infer_NO3_blank}{whether to infer the NO3 blank from this correction in addition to the organic blank (if using a single step correction)}

\item{ref_gas_d15}{the reference gas isotopic composition (only used if infer_NO3_blank = TRUE, to constrain culture blank d15)}
}
\value{
introduces the column d15.ocal and parameters p.15o_stds,
   p.d15o_m_true, p.d15o_m_vol, p.d15o_m_true:vol and p.d15o_b
   as well as information on the organic blank p.No_blk_conc and p.No_blk_d15 and nitrate blank (only if type is 1-step!) p.N_blk_amt and p.N_blk_d15

introduces the column d18.cal and parameters p.d18_m_true, p.d18_m_conc, p.d18_m_true:conc and p.d18_b
}
\description{
Calibrate organic d15 values with the given standards
}
\note{
implement single point correction
Calibrate d18 data with the given standards.

Uses a multivariate linear regression that takes the effective concentration
in the sample vials into consideration (as well as covariance between standards
and effective concentration).

FIXME: this should have a term for evaluating whether any extrapolation happens unepxectedly that then causes trouble!!
}
