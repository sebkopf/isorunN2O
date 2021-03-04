

#' Correct N2O data for O17
#'
#' This function takes d45 and d46 and corrects the values for O17 to derive raw d15 and d18 values.
#' The equations used are based on those derived by Jan Kaiser and Thomas Röckmann in
#' "Correction of mass spectrometric isotope ratio measurements for isobaric isotopologues of O2, CO, CO2, N2O and SO2" (Rapid Communications in Mass Spectrometry, 2008, 3997--4008).
#' It is important to note that this function does not currently take site preference into consideration
#'
#' @param data the data frame
#' @param d45 column (in permil)
#' @param d46 column (in permil)
#' @param ref_17R the 17O/16O reference ratio, VSMOW by default (value from Baertschi, 1976)
#' @param ref_18R the 18O/16O reference ratio, VSMOW by default (value from Li et al., 1988)
#' @param ref_15R_avg the 15N/14N reference ratio for the average of both alpha and beta N, N2 air by default (value from De Bievre et al., 1996)
#' @param lambda the mass dependent scaling coefficient for the oxygen isotopes, default 0.52 (value from Werner and Brandt, 2001)
#' @param d_max the maximum +/- delta value to consider in the root finding [in permil], should not need to change this unless samples are heavily enriched
#' @param quiet whether the function should output information messages or be quiet (default is to output)
#' @export
#' @return the data frame with corrected 17O
correct_N2O_for_17O <- function (data, d45, d46, ref_17R = 0.0003799, ref_18R = 0.0020052, ref_15R_avg = 0.0036782, lambda = 0.52, d_max = 1000, quiet = FALSE) {

  if (missing(d45)) stop("please specify the column that holds the d45 data")
  if (missing(d46)) stop("please specify the columm that holds the d46 data")

  # fitting parameters
  e <- ref_17R / (2 * ref_15R_avg) # constant E
  f <- ref_15R_avg * ref_17R / (2 * ref_18R) # constant F
  k46 <- 1 + 4 * f + f/e
  k45 <- -f * (6 + 4 * e + 2/e)
  k17 <- -f * (2 - 4 * e)
  k17x17 <- f * e * 3
  k45x17 <- - f * (2 + 2*e)
  k45x45 <- - f * (2 + e + 1/e)

  #' expects raw delta values (i.e. NOT in permil)
  d18_root_eq <- function(d18, d45, d46) {
    d17 <- md_scale_delta(d18, lambda, unit = 1)
    return( # note: comment out terms here to see the effect
      d18 - (k46 * d46 + k45 * d45 + k17 * d17 + k45x45 * d45^2 + k45x17 * d45 * d17 + k17x17 * d17^2
      ))
  }

  #' expects raw delta values (i.e. NOT in permil)
  calc_d18 <- function(d45, d46) {
    mapply(function(.d45, .d46) {
      uniroot(function(x) d18_root_eq(x, .d45, .d46), lower = -d_max/1000, upper = d_max/1000, tol = 1e-9)$root
    }, d45, d46)
  }

  #' expects raw delta values (i.e. NOT in permil)
  calc_d15 <- function(d18, d45) {
    d17 <- md_scale_delta(d18, lambda, unit = 1)
    return(d45 + e * (d45 - d17))
  }

  d45_expr <- rlang::enexpr(d45)
  d46_expr <- rlang::enexpr(d46)
  df <- data %>%
    mutate(
      .d45_orig = !!d45_expr,
      .d46_orig = !!d46_expr,
      .d45 = isoreader::iso_strip_units(.d45_orig),
      .d46 = isoreader::iso_strip_units(.d46_orig),
      .d18 = 1000 * calc_d18 (.d45/1000, .d46/1000),
      p.17Ocor = paste0("scaling=", lambda, "; ref 17R=", ref_17R, ", 18R=", ref_18R, ", 15N=", ref_15R_avg),
      d15.raw = 1000 * calc_d15 (.d18/1000, .d45/1000),
      d18.raw = .d18
    )

  # output
  if (!quiet) {
    sprintf(paste(
      "INFO: %s N2O analyses were corrected for 17O (new columns 'd15.raw' and 'd18.raw', and parameter 'p.17Ocor' added).",
      "\n      Correction effects: mean d45 = %.3f with resulting d15 = %.3f, mean d46 = %.3f with resulting d18 = %.3f [permil]",
      "\n      Correction term constants: k46 = %.4f, k45 = %.4f, k17 = %.4f, k45x45 = %.4f, k45x17 = %.4f, k17x17 = %.4f"),
      nrow(df), mean(df$.d45), mean(df$d15.raw), mean(df$.d46), mean(df$d18.raw), k46, k45, k17, k45x45, k45x17, k17x17) %>% message()
  }

  # units
  d45_units <- isoreader::iso_get_units(df$.d45_orig)
  d46_units <- isoreader::iso_get_units(df$.d46_orig)
  if (!is.na(d45_units) && !is.na(d46_units) && identical(d45_units, d46_units)) {
    # FIXME: should we always do permil conversion here? and/or make an earlier check that if units are provided, d45 and d46 have the same?
    df$d15.raw <- iso_double_with_units(df$d15.raw, d45_units)
    df$d18.raw <- iso_double_with_units(df$d18.raw, d45_units)
  }
  df %>% select(-.d45, -.d46, -.d45_orig, -.d46_orig, -.d18) %>% return()
}



#---------- isotope convenience functions (not exported) -----------

# multiplication factor for permil
PERMIL = 1000;

# Calculate the 45R from N2O
calculate_45R <- function (`15R`, `17R`) {
  return (2 * `15R` + `17R`)
}

# Calculate the 46R from N2O
calculate_46R <- function (`15R`, `17R`, `18R`) {
  return (`15R`^2 + `18R` + 2 * `15R` * `17R`)
}

# Convert delta to ratio
delta_to_ratio <- function (delta, ref_ratio, unit = PERMIL) {
  return ((delta/PERMIL + 1) * ref_ratio)
}

# Convert ratio to delta
ratio_to_delta <- function (ratio, ref_ratio, unit = PERMIL) {
  return ( (ratio/ref_ratio - 1) * unit )
}

# Mass scale delta
md_scale_delta <- function(delta, lambda, unit = PERMIL) {
  ((delta/unit + 1) ^ lambda - 1)*unit
}
