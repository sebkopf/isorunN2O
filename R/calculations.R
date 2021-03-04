#' Calculate background area
#'
#' Calculates the bacterial background area from the direct measurement of the
#' background peak. For setting this parameter manually, please use
#' \code{\link{set_background}}.
#'
#' @param data (can be a grouped_by data set)
#' @param area the area column
#' @param criteria the expression used to find the background analyses
#' @param update_category value to update the found records category to
#'    (pass in NULL if no update of category is desired)
#' @seealso \code{\link{change_category}}
#' @return adds the parameter column p.bgrd
#' @export
calculate_background <- function(data, area,
                                 criteria = name %in% c("background", "Background"),
                                 update_category = "background", quiet = FALSE) {

  if (missing(area)) stop("please specify the column that holds the peak area")
  criteria_quo <- rlang::enquo(criteria)
  area_expr <- rlang::enexpr(area)
  if (!is.null(update_category))
    data <- change_category(data, !!criteria_quo, update_category)

  # calculate background (preserves group_by during the mutate)
  df <- data %>%
    mutate(p.bgrd = mean(isoreader::iso_strip_units(!!area_expr)[!!criteria_quo]), na.rm = TRUE) %>%
    # make sure if it's not-a-number to switch to NA
    mutate(p.bgrd = ifelse(!is.nan(p.bgrd[1]), p.bgrd, NA_real_))

  if (!quiet) {
    # use do to preserve grouping if any is set
    df %>% do({

      # group info
      sdf <- .
      grps_text <- ""
      if (length(groups(data)) > 0) {
        grps <- groups(data) %>% as.character() %>% sapply(function(i) sdf[[i]][1])
        grps_text <- paste0(" (for ", paste0(names(grps), " = ", grps) %>% paste(collapse = ", "), ")")
      }

      if ( is.na(sdf$p.bgrd[1]) ) {
        sprintf("INFO: bacterial background could NOT be identified, 'p.bgrd' is NA%s",
                grps_text) %>% message()
      } else {
        sprintf("INFO: bacterial background identified and area stored in parameter column 'p.bgrd': %.3f%s",
                sdf$p.bgrd[1], grps_text) %>% message()
      }
      dplyr::tibble()
    })
  }

  df %>% return()
}

#' Set the bacterial background
#'
#' This can be calculated automatically based on identifying which analyses are
#' measurements of the background alone (using \code{\link{calculate_background}}).
#' This function is for setting this parameter manually in cases where the background
#' is estimated or the peak was too small to be automatically recorded.
#'
#' @param data the data set
#' @param value the value of the bacterial blank area, can be an expression
#' @return adds the parameter column p.bgrd
#' @note uses non-standard evaluation
#' @export
set_background <- function(data, value, quiet = FALSE) {

  # check if overwriting
  if(!is.null(data$p.bgrd)) {
    msg <- sprintf("OVERWRITING (previous: %s)", data$p.bgrd %>% unique() %>% round(3) %>% paste(collapse = ", "))
  } else
    msg <- "adding"

  # mutate data frame with area
  data <- data %>%
    mutate_(.dots = list(p.bgrd = interp(~x, x = lazy(value))))

  if (!quiet) {
    sprintf(
      "INFO: %s bacterial background parameter column 'p.bgrd' (new: %s)",
      msg, data$p.bgrd %>% unique() %>% round(3) %>% paste(collapse = ", ")) %>% message()
  }

  return(data)
}



#' Calculate oxidation blank area/volume
#'
#' Calculates the oxidation blank area to volume ratio from the direct
#' measurements of the blank peak. For setting this parameter manually, please use
#' \code{\link{set_oxidation_blank}}.
#'
#' @param data (can be a grouped_by data set)
#' @param area the area column
#' @param volume the volume column
#' @param criteria the expression used to find the analyses
#' @param update_category value to update the found records category to
#'    (pass in NULL if no update of category is desired)
#' @seealso \code{\link{change_category}}
#' @return adds the parameter column p.oblank
#' @export
calculate_oxidation_blank <- function(data, area, volume,
                                 criteria = grepl("POR Blank", name),
                                 update_category = "oxidation blank",
                                 quiet = FALSE) {

  if (missing(area)) stop("please specify the column that holds the peak area")
  if (missing(volume)) stop("please specify the column that holds the injection volume")
  if (!is.null(update_category))
    data <- do.call(change_category, list(data, substitute(criteria), update_category))

  # calculate background (preserves group_by during the mutate)
  fields <- list(p.oblank = interp(
    ~mean(x[crit]/as.numeric(y[crit]), na.rm = T),
    x = substitute(area), y = substitute(volume), crit = substitute(criteria)))
  df <- data %>% mutate_(.dots = fields) %>%
    # make sure if it's not-a-number to switch to NA
    mutate(p.oblank = ifelse(!is.nan(p.oblank[1]), p.oblank, NA_real_))

  if (!quiet) {
    # use do to preserve grouping if any is set
    df %>% do({

      # group info
      sdf <- .
      grps_text <- ""
      if (!is.null(groups(data))) {
        grps <- groups(data) %>% as.character() %>% sapply(function(i) sdf[[i]][1])
        grps_text <- paste0(" (for ", paste0(names(grps), " = ", grps) %>% paste(collapse = ", "), ")")
      }

      if ( is.na(sdf$p.oblank[1]) ) {
        sprintf("INFO: oxidation blank could NOT be identified, 'p.oblank' is NA%s",
                grps_text) %>% message()
      } else {
        sprintf("INFO: oxidation blank identified and area/volume ratio stored in parameter column 'p.oblank': %.3f%s",
                sdf$p.oblank[1], grps_text) %>% message()
      }
      dplyr::tibble()
    })
  }

  df %>% return()
}

#' Set the oxdiation blank arae/volume
#'
#' This can be calculated automatically based on identifying which analyses are
#' measurements of the oxidation blank alone (using \code{\link{calculate_oxidation_blank}}).
#' This function is for setting this parameter manually in cases where the background
#' is estimated or the peak was too small to be automatically recorded.
#'
#' @param data the data set
#' @param value the numeric value of the oxidation blank area/volume ratio
#' @return adds the parameter column p.oblank
#' @export
set_oxidation_blank <- function(data, value, quiet = FALSE) {

  if(!is.null(data$p.oblank)) {
    msg <- sprintf("OVERWRITING (previous: %s)", data$p.oblank %>% unique() %>% round(3) %>% paste(collapse = ", "))
  } else
    msg <- "adding"

  # mutate data frame with area
  data <- data %>%
    mutate_(.dots = list(p.oblank = interp(~x, x = lazy(value))))

  if (!quiet) {

    sprintf(
      "INFO: %s oxidation blank parameter column 'p.oblank' (new: %s)",
      msg, data$p.oblank %>% unique() %>% round(3) %>% paste(collapse = ", ")) %>% message()
  }

  return(data)
}




#' Set the run size
#'
#' Note that this is calculated automatically when using calculate_concentrations,
#' only use this function to set the run size manually or overwrite automatic
#' calculations from calculate_concentrations.
#' @param data the data set
#' @param amount the run size, should be in the same units as standard concentration * volume injection, i.e. if concentrations are in microM and volumes in mL, the run size should be in nmol
#' @return adds/overwrites the column 'p.run_size'
#' @export
set_run_size <- function(data, amount, quiet = FALSE) {
  if (!quiet) {
    if(!is.null(data$p.run_size)) {
      msg <- sprintf("OVERWRITING (previous: %s)", data$p.run_size %>% unique() %>% signif(3) %>% paste(collapse = ", "))
    } else
      msg <- "adding"

    sprintf(
        "INFO: %s run size parameter column 'p.run_size' (value = %.2f)",
        msg, amount) %>% message()
  }

  data$p.run_size <- amount
  return(data)
}

#' Calculate the concentrations of the source samples
#' @param data (can be a grouped_by data set)
#' @param area the area column
#' @param volume the volume column
#' @param dilution the dilution column (if any)
#' @param conc_pattern the regular expression pattern to capture the concentration
#'    from the standards 'name' colum
#' @param standards selection criterion for the standards to consider
#' @return introduces the columns 'conc', 'amount' and the parameters 'p.yield' and 'p.run_size'
#' @export
calculate_concentrations <- function(data, area, volume, dilution = 1,
                                     conc_pattern = "(\\d+)uM",
                                     standards = category %in% c("USGS-34", "IAEA-NO3"),
                                     quiet = FALSE) {

  area_expr <- rlang::enexpr(area)
  volume_expr <- rlang::enexpr(volume)
  dilution_expr <- rlang::enexpr(dilution)
  standards_expr <- rlang::enexpr(standards)

  if (is.null(data$name)) stop("need to have sample names, please parse_file_names first", call. = FALSE)
  if (is.null(data$category)) stop("need to have categories, please parse_file_names first", call. = FALSE)
  if (is.null(data$analysis)) stop("need to have analysis, please parse_file_names first", call. = FALSE)
  if (rlang::is_missing(area_expr)) stop("please specify the column that holds the peak area", call. = FALSE)
  if (rlang::is_missing(volume_expr)) stop("please specify the column that holds the injected volume", call. = FALSE)

  data <- data %>% do({
    # calculate yield
    df <- mutate(
      .,
      .V = iso_strip_units(!!volume_expr),
      .A = iso_strip_units(!!area_expr),
      .dilution = dplyr::if_else(is.na(!!dilution_expr), NA_real_, !!dilution_expr)
    )

    stds <-
      df %>%
      dplyr::filter(!!standards_expr) %>%
      extract(name, "conc", conc_pattern, convert = T, remove = F) %>%
      mutate( run_size = conc * .V / .dilution,
              yield = .A / run_size) %>%
      dplyr::filter(!is.na(conc))

      if (nrow(stds) == 0)
        stop("It seems no concentration standards were found. Please check that the parameters 'conc_pattern' and 'standards' are set correctly for your naming convention.", call. = F)

    # derived parameters
    yield <- mean(stds$yield)
    run_size <- mean(stds$run_size)

    # mutate data frame
    df <- df %>%
      mutate(
        p.run_size = run_size, # units nmol typically
        p.yield = yield, # units [Vs/nmol] single value
        amount = .A / yield,
        conc = .dilution * amount / .V ) %>%
      select(-.V, -.A, -.dilution) # remove temp columns

    if (!quiet) {
      sprintf(
        paste(
          "INFO: NOx concentrations and injection amounts (new columns 'conc' and 'amount') calculated from %s standards",
          "\n      Parameter columns mass spec signal 'p.yield' (%.3f) and effective 'p.run_size' (%.2f) added."),
        (stds %>% group_by(conc) %>%
           summarize(label = paste0(conc[1], "uM (", dplyr::n(), "x)")) %>% ungroup() %>%
           arrange(conc))$label %>% paste(collapse = " & "),
        yield, run_size) %>% message()
    }

    df
  })

  return(data)
}

#' method making it easier to retrieve regression coefficients by name
get_calib_coef <- function(key, model, name) {
  cof_names <- gtools::permutations(length(name), length(name), name) %>% apply(1, function(i) paste(i, collapse = ":"))
  cof <- as.data.frame(coef(summary(model)))
  cof <- cof %>% mutate(name = rownames(cof))
  if (!any(cof$name %in% cof_names)) out <- list(NA, NA, NA)
  else {
    out <- list(
      cof$Estimate[cof$name %in% cof_names],
      cof$`Std. Error`[cof$name %in% cof_names],
      cof$`Pr(>|t|)`[cof$name %in% cof_names]
    )
  }
  setNames(out, paste0(key, c("",".err", ".p")))
}

#' Calibrate d15 values with the given standards
#' @param data (can be a grouped_by data set)
#' @param d15 the d15 column
#' @param area the area (signal) column if want to use this as a calibration parameter (default is just the mean of the standards)
#' @param standards a set of isotope standards
#'  Note: they are matched to the data by "category" (not by name)
#' @param infer_ref_gas whether to infer reference gas (N2O) isotopic composition from the regression
#' @param infer_bgrd whether to infer bacterial background from the regression
#' @return introduces the column d15.cal and parameters p.15_stds, p.d15_m and p.d15_b as well as p.bgrd_area and p.bgrd_d15
#' @note implement single point correction
#' @note consider removing the storage of the regression parameters since they can be inferred from the others
#' @export
calibrate_d15 <- function(data, d15, area = mean(area[category %in% names(standards)]), standards = c("USGS-34" = -1.8, "IAEA-NO3" = 4.7),
                          infer_ref_gas = TRUE, infer_bgrd = TRUE, quiet = FALSE) {

  if (missing(d15)) stop("please specify the column that holds the d15 values to calibrate", call. = FALSE)
  if (length(standards) == 1) stop("sorry, single point correction is not implemented yet") #FIXME

  d15_expr <- rlang::enexpr(d15)
  area_expr <- rlang::enexpr(area)

  # run calibration
  data %>%
    mutate(
      .d15_orig = !!d15_expr,
      .d15 = isoreader::iso_strip_units(.d15_orig),
      .A = isoreader::iso_strip_units(!!area_expr)
    ) %>%
    run_d15_calibration(
      standards = standards, organic = FALSE, quiet = quiet,
      infer_ref_gas = infer_ref_gas, infer_bgrd = infer_bgrd) %>%
    select(-.d15, -.d15_orig, -.A)
}

#' Calibrate organic d15 values with the given standards
#' @param data (can be a grouped_by data set)
#' @param d15 the d15 column
#' @param area the area (signal) column
#' @param standards a set of isotope standards
#'  Note: they are matched to the data by "category" (not by name)
#' @param infer_ref_gas whether to infer reference gas (N2O) isotopic composition from the regression
#' @param infer_bgrd whether to infer bacterial background from the regression
#' @param infer_oblank wheter to infer the organic blank from the regression
#' @return introduces the column d15.ocal and parameters p.15o_stds,
#'    as well as information on the organic blank A/V ratio p.oblank_ratio
#'    and p.oblank_d15 and p.N_blk_amt and bacterial background area
#'    p.bgrd_area and p.bgrd_d15
#' @note should this also record the actual regression parameters?
#' @export
calibrate_d15_org <- function(
  data, d15, area, volume, standards = c("USGS-40" = -4.50, "USGS-41" = 47.60),
  infer_ref_gas = TRUE, infer_bgrd = TRUE, infer_oblank = TRUE, quiet = FALSE) {

  # safety checks
  if (missing(d15)) stop("please specify the column that holds the d15 values to calibrate", call. = FALSE)
  if (missing(area)) stop("please specify the column that holds the area values", call. = FALSE)
  if (missing(volume)) stop("please specify the column that holds the volume values", call. = FALSE)

  d15_expr <- rlang::enexpr(d15)
  area_expr <- rlang::enexpr(area)
  volume_expr <- rlang::enexpr(volume)

  # run calibration
  data %>%
    mutate(
      .d15_orig = !!d15_expr,
      .d15 = isoreader::iso_strip_units(.d15_orig),
      .A = isoreader::iso_strip_units(!!area_expr),
      .V = isoreader::iso_strip_units(!!volume_expr)
    ) %>%
    run_d15_calibration(
      standards = standards, organic = TRUE, quiet = quiet,
      infer_ref_gas = infer_ref_gas, infer_bgrd = infer_bgrd, infer_oblank = infer_oblank) %>%
    select(-.d15, -.d15_orig, -.V, -.A)
}

#' runs the d15 calibration internally
#' called from calibrate_d15 and calibrate_d15_org
run_d15_calibration <- function(data, standards, organic, infer_ref_gas = TRUE, infer_bgrd = TRUE, infer_oblank = TRUE, quiet = FALSE) {

  if (is.null(data$category)) stop("need to have categories, please parse_file_names first", call. = FALSE)
  if ( length(missing <- setdiff(names(standards), data$category)) > 0 )
    stop("some of the standards' names do not match the categories in this dataset, check spelling: ",
         paste(missing, collapse = ", "), call. = F)

  # internal checks
  if (is.null(data$.d15)) stop(".d15 missing")
  if (is.null(data$.A)) stop(".A missing")
  if (organic && is.null(data$.V)) stop(".V missing")

  stds.df <- tibble(category = names(standards), .d15.true = standards)
  stds.label <- (stds.df %>% mutate(label = paste0(category, " (", .d15.true, ")")))$label %>% paste(collapse = " & ")

  # go through (accounting for all grouping)
  data %>%
    mutate(.AI = 1/.A) %>%
    do({

      # sub frame
      sdf <- .

      # standards
      stds <- sdf %>%
        filter(., category %in% names(standards)) %>%
        left_join(stds.df, by = "category") %>%
        mutate(y = .d15 - .d15.true)

      # model
      if (organic) {
        # with volume
        m <- lm(y ~ .V : .AI : .d15.true + .AI : .d15.true + .V : .AI + .AI, data = stds)
      } else {
        # without volume
        m <- lm(y ~ .AI : .d15.true + .AI, data = stds)
      }

      # coefficients
      bs <- c(
        get_calib_coef("b0", m, "(Intercept)"),
        get_calib_coef("b1", m, c(".d15.true", ".V",".AI")),
        get_calib_coef("b2", m, c(".d15.true", ".AI")),
        get_calib_coef("b3", m, c(".V",".AI")),
        get_calib_coef("b4", m, ".AI")
      )
      # NA sanitized parameters for calibration
      bs_san <- bs
      bs_san[is.na(bs_san)] <- 0

      # calibration
      if (organic) {
        out <- sdf %>%
        mutate(
          p.d15o_stds = stds.label,
          d15.ocal = (.d15  - bs_san$b0 - bs_san$b3 * .V * .AI - bs_san$b4 * .AI) / ( 1 + bs_san$b1 * .V * .AI + bs_san$b2 * .AI ))
        d15_units <- isoreader::iso_get_units(out$.d15_orig)
        if (!is.na(d15_units)) out$d15.ocal <- iso_double_with_units(out$d15.ocal, d15_units)
      } else {
        out <- sdf %>%
        mutate(
          p.d15_stds = stds.label,
          d15.cal = (.d15  - bs_san$b0 - bs_san$b4 * .AI) / ( 1 + bs_san$b2 * .AI ))
        d15_units <- isoreader::iso_get_units(out$.d15_orig)
        if (!is.na(d15_units)) out$d15.cal <- iso_double_with_units(out$d15.cal, d15_units)
      }

      # parameter reference gas
      if (infer_ref_gas) {
        out <- out %>%
          mutate(
            p.ref_gas_d15 = -bs$b0,
            p.ref_gas_d15.err = bs$b0.err)
      }

      # parameter bacterial backgroudn
      if (infer_bgrd) {
        out <- out %>%
          mutate(
            p.bgrd_area = -bs$b2,
            p.bgrd_area.err = bs$b2.err,
            p.bgrd_d15 = -bs$b4/bs$b2,
            p.bgrd_d15.err = abs(p.bgrd_d15) * sqrt( (bs$b4.err/bs$b4)^2 + (bs$b2.err/bs$b2)^2 )
          )
      }

      # organic blank
      if (organic && infer_oblank) {
        out <- out %>%
          mutate(
          p.oblank_ratio = -bs$b1,
            p.oblank_ratio.err = bs$b1.err,
            p.oblank_d15 = -bs$b3/bs$b1,
            p.oblank_d15.err = abs(p.oblank_d15) * sqrt( (bs$b3.err/bs$b3)^2 + (bs$b1.err/bs$b1)^2 )
          )
      }

      # info messages
      if (!quiet) {

        # group info
        grps_text <- ""
        if (length(groups(data)) > 0) {
          grps <- groups(data) %>% as.character() %>% sapply(function(i) sdf[[i]][1])
          grps_text <- paste0("\n      Group: ", paste0(names(grps), " = ", grps) %>% paste(collapse = ", "))
        }

        if (organic) {
          msg <- sprintf(
            paste(
              "INFO: organic d15 values calibrated (new columm 'd15.ocal') using %s --> stored in 'p.d15o_stds'",
              "%s\n      Regression: %s (d15*V/A, p=%s), %s (d15/A, p=%s), %s (V/A, p=%s), %s (1/A, p=%s), %s (intercept, p=%s)"),
            stds.label, grps_text,
            round_to_err(bs$b1, bs$b1.err), paste(signif(bs$b1.p,1)), round_to_err(bs$b2, bs$b2.err), paste(signif(bs$b2.p,1)),
            round_to_err(bs$b3, bs$b3.err), paste(signif(bs$b3.p,1)), round_to_err(bs$b4, bs$b4.err), paste(signif(bs$b4.p,1)),
            round_to_err(bs$b0, bs$b0.err), paste(signif(bs$b0.p,1))
          )
        } else {
          msg <- sprintf(
            paste(
              "INFO: d15 values calibrated (new columm 'd15.cal') using %s --> stored in 'p.d15_stds'",
              "%s\n      Regression: %s (d15/A, p=%s), %s (1/A, p=%s), %s (intercept, p=%s)"),
            stds.label, grps_text,
            round_to_err(bs$b2, bs$b2.err), paste(signif(bs$b2.p,1)),
            round_to_err(bs$b4, bs$b4.err), paste(signif(bs$b4.p,1)),
            round_to_err(bs$b0, bs$b0.err), paste(signif(bs$b0.p,1))
          )
        }

        # parameters
        if (infer_ref_gas) {
          msg <- sprintf(
            "%s\n      Inferred reference gas isotopic composition: %s permil (added as 'p.ref_gas_d15')",
            msg, round_to_err(out$p.ref_gas_d15, out$p.ref_gas_d15.err) %>% unique() %>% paste(collapse = ", "))
        }

        if (infer_bgrd) {
          msg <- sprintf(
            "%s\n      Inferred bacterial background area: %s (added as 'p.bgrd_area') & isotopic composition: %s permil (added as 'p.bgrd_d15')",
            msg, round_to_err(out$p.bgrd_area, out$p.bgrd_area.err) %>% unique() %>% paste(collapse = ", "),
            round_to_err(out$p.bgrd_d15, out$p.bgrd_d15.err) %>% unique() %>% paste(collapse = ", ")
          )
        }

        if (organic && infer_oblank) {
          msg <- sprintf(
            "%s\n      Inferred organic blank area/volume: %s (added as 'p.oblank_ratio') & isotopic composition: %s permil (added as 'p.oblank_d15')",
            msg, round_to_err(out$p.oblank_ratio, out$p.oblank_ratio.err) %>% unique() %>% paste(collapse = ", "),
            round_to_err(out$p.oblank_d15, out$p.oblank_d15.err) %>% unique() %>% paste(collapse = ", ")
          )
        }

        message(msg)
      }

      out
    }) %>% select(-.AI)
}

#' Calibrate organic d15 values with the given standards
#' @param data (can be a grouped_by data set)
#' @param standards a set of isotope standards
#'  Note: they are matched to the data by "category" (not by name)
#' @param d15 the d15 column (can be a column already calibrated for denitrifier method)
#' @param infer_NO3_blank whether to infer the NO3 blank from this correction in addition to the organic blank (if using a single step correction)
#' @param ref_gas_d15 the reference gas isotopic composition (only used if infer_NO3_blank = TRUE, to constrain culture blank d15)
#' @return introduces the column d15.ocal and parameters p.15o_stds,
#'    p.d15o_m_true, p.d15o_m_vol, p.d15o_m_true:vol and p.d15o_b
#'    as well as information on the organic blank p.No_blk_conc and p.No_blk_d15 and nitrate blank (only if type is 1-step!) p.N_blk_amt and p.N_blk_d15
#' @note implement single point correction
# calibrate_d15_org_old <- function(data, d15, volume = volume, standards = c("USGS-40" = -4.50, "USGS-41" = 47.60),
#                               infer_NO3_blank = FALSE, ref_gas_d15 = 0, quiet = FALSE) {
#
#   # safety checks
#   if (missing(d15)) stop("please specify the column that holds the d15 values to calibrate")
#   if (is.null(data$category)) stop("need to have categories, please parse_file_names first")
#   if (is.null(data$p.run_size)) stop("need to know run size (p.run_size) to infer blank parameters, ",
#                                      "please create this column via set_run_size() or calculate_concentrations() first")
#
#   if ( length(missing <- setdiff(names(standards), data$category)) > 0 )
#     stop("some of the standards' names do not match the categories in this dataset, check spelling: ",
#          paste(missing, collapse = ", "), call. = F)
#
#   # standards and calculate fields
#   stds.df <- data_frame(category = names(standards), .d15.true = standards)
#   stds.label <- (stds.df %>% mutate(label = paste0(category, " (", .d15.true, ")")))$label %>% paste(collapse = " & ")
#   fields <- list(
#     .d15 = interp(~var, var = substitute(d15)),
#     .V = interp(~as.numeric(var), var = substitute(volume))
#   )
#
#   # ref gas value and error
#   ref_gas_d15.err <- if (length(ref_gas_d15) > 1) ref_gas_d15[2] else 0
#   ref_gas_d15 <- ref_gas_d15[1]
#
#   # go through (accounting for all grouping)
#   data %>% mutate_(.dots = fields) %>% # creating .d15 once here is easier to handle
#     do({
#
#       # sub frame
#       sdf <- .
#
#       # regression model: d15 = beta0 + beta1 * d15.true + beta2 * d15.true * V + beta3 * V
#       m <- filter(sdf, category %in% names(standards)) %>%
#         left_join(stds.df, by = "category") %>%
#         with(lm(.d15 ~ .d15.true + .V + .d15.true:.V))
#       coefs <- as.data.frame(coef(summary(m)))
#       coefs <- coefs %>% mutate(coef = rownames(coefs))
#
#       # coefficients
#       beta0 <- (coefs %>% filter(coef == "(Intercept)"))$Estimate
#       beta0.err <- (coefs %>% filter(coef == "(Intercept)"))$`Std. Error`
#       beta1 <- (coefs %>% filter(coef == ".d15.true"))$Estimate
#       beta1.err <- (coefs %>% filter(coef == ".d15.true"))$`Std. Error`
#       beta2 <- (coefs %>% filter(coef == ".d15.true:.V"))$Estimate
#       beta2.err <- (coefs %>% filter(coef == ".d15.true:.V"))$`Std. Error`
#       beta3 <- (coefs %>% filter(coef == ".V"))$Estimate
#       beta3.err <- (coefs %>% filter(coef == ".V"))$`Std. Error`
#
#       # calculate delta and add parameters
#       p.run_size.err <- 0 # FIXME, might want to allow defining this?
#       out <- mutate(
#         sdf,
#         p.d15o_stds = stds.label,
#         d15.ocal = (.d15 - beta3 * .V - beta0)/(beta1 + beta2 * .V),
#         d15.ocal_no_cross = (.d15 - beta3 * .V - beta0)/(beta1),
#         d15.ocal_no_vol = (.d15 - beta0)/(beta1 + beta2 * .V),
#         # coefficients
#         p.d15o_m_true = beta1,
#         p.d15o_m_vol = beta3,
#         `p.d15o_m_true:vol` = beta2,
#         p.d15o_b = beta0,
#         # N organic blank
#         p.No_blk_conc = -beta2/beta1 * p.run_size,
#         p.No_blk_conc.err = abs(p.No_blk_conc) * sqrt((beta1.err/beta1)^2 + (beta2.err/beta2)^2 + (p.run_size.err/p.run_size)^2),
#         p.No_blk_d15 = -beta3/beta2,
#         p.No_blk_d15.err = abs(p.No_blk_d15) * sqrt( (beta2.err/beta2)^2 + (beta3.err/beta3)^2 )
#       )
#
#       # for full step also adding the nitrate blank parameters
#       if (infer_NO3_blank) {
#         out <- out %>%
#           mutate(
#             p.ref_gas_d15 = ref_gas_d15,
#             p.N_blk_amt = p.run_size * (1 - beta1) / beta1,
#             p.N_blk_amt.err = sqrt( ( (1/beta1 + 1) * p.run_size.err )^2 + (p.run_size/beta1^2 * beta1.err)^2 ),
#             p.N_blk_d15 = (beta0 + ref_gas_d15) * 1 / (1 - beta1),
#             p.N_blk_d15.err = abs(1/(1-beta1)) * sqrt( (beta0.err)^2 + ( (beta0 + ref_gas_d15)/beta1 * beta1.err)^2 + (ref_gas_d15.err)^2 ))
#       }
#
#       # info messages
#       if (!quiet) {
#
#         # group info
#         grps_text <- ""
#         if (!is.null(groups(data))) {
#           grps <- groups(data) %>% as.character() %>% sapply(function(i) sdf[[i]][1])
#           grps_text <- paste0(names(grps), " = ", grps) %>% paste(collapse = ", ")
#           grps_text <- paste0("\n      Group: ", grps_text)
#         }
#
#         msg <- sprintf(
#           paste(
#             "INFO: organic d15 values calibrated (new columm 'd15.ocal') using %s --> stored in 'p.d15o_stds'",
#             "%s\n      Calibration with regression cofficients (measured vs. true * volume):",
#             "\n      'p.d15o_m_true' (%s), 'p.d15o_m_true:vol' (%s), 'p.d15o_m_vol' (%s) and intercept 'p.d15o_b' (%s) added."),
#           stds.label, grps_text, round_to_err(beta1, beta1.err), round_to_err(beta2, beta2.err),
#           round_to_err(beta3, beta3.err), round_to_err(beta0, beta0.err))
#
#         if (infer_NO3_blank) {
#           msg <- sprintf(
#             "%s\n      Inferred nitrate blank information (for 'p.ref_gas_d15' = %s permil) added as 'p.N_blk_amt' (%s usually nmol) and 'p.N_blk_d15' (%s permil)",
#             msg, round_to_err(ref_gas_d15, ref_gas_d15.err),
#             round_to_err(out$p.N_blk_amt, out$p.N_blk_amt.err) %>% unique() %>% paste(collapse = ", "),
#             round_to_err(out$p.N_blk_d15, out$p.N_blk_d15.err) %>% unique() %>% paste(collapse = ", "))
#         }
#
#         msg <- sprintf(
#           "%s\n      Inferred organic N blank information added as 'p.No_blk_conc' (%s usually microM if volume in mL) and 'p.No_blk_d15' (%s permil)",
#           msg,
#           round_to_err(out$p.No_blk_conc, out$p.No_blk_conc.err, 2) %>% unique() %>% paste(collapse = ", "),
#           round_to_err(out$p.No_blk_d15, out$p.No_blk_d15.err, 2) %>% unique() %>% paste(collapse = ", "))
#
#         message(msg)
#       }
#       return(out)
#
#     }) %>% select(-.d15, -.V)
# }


#' Calibrate d18 data with the given standards.
#'
#' Uses a multivariate linear regression that takes the effective concentration
#' in the sample vials into consideration (as well as covariance between standards
#' and effective concentration).
#'
#' @param data (can be a grouped_by data set)
#' @param d18 the d18 column
#' @param amount the amount column
#' @param volume the volume column
#' @param cell_volume the volume of cells in the vial (has to be same units as volume!)
#' @param standards a set of isotope standards
#'  Note: they are matched to the data by "category" (not by name)
#' @return introduces the column d18.cal and parameters p.d18_m_true, p.d18_m_conc, p.d18_m_true:conc and p.d18_b
#' @export
#' @note FIXME: this should have a term for evaluating whether any extrapolation happens unepxectedly that then causes trouble!!
calibrate_d18 <- function(data, d18, amount = amount, volume = volume, cell_volume,
                          standards = c("USGS-34" = -27.93, "IAEA-NO3" = 25.61), quiet = FALSE) {

  if (is.null(data$category)) stop("need to have categories, please parse_file_names first")
  if (missing(d18)) stop("please specify the column that holds the d18 values to calibrate")
  if (missing(cell_volume)) stop("please specify the denitrifier cells volume")
  if (length(standards) == 1) stop("sorry, single point correction is not currently supported")

  stds.df <- dplyr::tibble(category = names(standards), d18.true = standards)
  stds.label <- (stds.df %>% mutate(label = paste0(category, " (", d18.true, ")")))$label %>%
    paste(collapse = " & ")

  d18_expr <- rlang::enexpr(d18)
  amount_expr <- rlang::enexpr(amount)
  volume_expr <- rlang::enexpr(volume)

  # run calibration
  out <- data %>%
    mutate(
      .d18_orig = !!d18_expr,
      .d18 = isoreader::iso_strip_units(.d18_orig),
      .amount = !!amount_expr,
      .V = isoreader::iso_strip_units(!!volume_expr),
      .C_vial = .amount / (.V + !!cell_volume)
    ) %>%
    do({

      if (any(.$.V > 100 * cell_volume, na.rm = TRUE)) {
        warning("WARNING: injection volumes are more than 100 * cell volumes, ",
                "please make sure that cell_volume and the 'volume' column have the same units",
                call. = FALSE)
      }

      # regression model (based on true isotopic value as well as effective concentration)
      m <- filter(., category %in% names(standards)) %>%
        left_join(stds.df, by = "category") %>%
        { lm(.d18 ~ d18.true*.C_vial, data = .) }

      if (!quiet) {
        sprintf(
          paste(
            "INFO: d18 values calibrated (new columm 'd18.cal') using %s --> stored in 'p.d18_stds'",
            "\n      Effective concentration dependence of calibration regression taken into account.",
            "\n      Parameter columns for calibration (measured vs. true * concentration):",
            "\n      'p.d18_m_true' (%.3f), 'p.d18_m_conc' (%.3f), 'p.d18_m_true:conc' (%.3f) and intercept 'p.d18_b' (%.3f) added."),
          stds.label, coef(m)["d18.true"], coef(m)[".C_vial"],
          coef(m)["d18.true:.C_vial"], coef(m)["(Intercept)"]) %>% message()
      }

      # add parameters and calculate delta
      mutate(.,
             p.d18_stds = stds.label,
             p.d18_m_true = coef(m)["d18.true"],
             p.d18_m_conc = coef(m)[".C_vial"],
             `p.d18_m_true:conc` = coef(m)["d18.true:.C_vial"],
             p.d18_b = coef(m)["(Intercept)"],
             d18.cal =
               (.d18 - p.d18_b - p.d18_m_conc * .C_vial)/
               (p.d18_m_true + `p.d18_m_true:conc` *.C_vial)
      )
    })

  # units
  d18_units <- isoreader::iso_get_units(out$.d18_orig)
  if (!is.na(d18_units)) out$d18.cal <- iso_double_with_units(out$d18.cal, d18_units)

  return(select(out, -.d18, -.d18_orig, -.V, -.amount, -.C_vial))
}
