#' Calculates the background area
#' @param data (can be a grouped_by data set)
#' @param area the area column
#' @param criteria the expression used to find the background analyses
#' @param update_category value to update the found records category to
#'    (pass in NULL if no update of category is desired)
#' @seealso \code{\link{change_category}}
#' @return introduces the parameter columns p.bgrd
#' @export
calculate_background <- function(data, area,
                                 criteria = name %in% c("background", "Background"),
                                 update_category = "background", quiet = FALSE) {

  if (missing(area)) stop("please specify the column that holds the peak area")
  if (!is.null(update_category))
    data <- do.call(change_category, list(data, substitute(criteria), update_category))

  # calculate background
  fields <- list(p.bgrd = interp(~mean(x[crit]), x = substitute(area), crit = substitute(criteria)))
  df <- data %>% mutate_(.dots = fields)

  if (!quiet) {
    bgrds <- (df %>% summarize(p.bgrd = mean(p.bgrd)))$p.bgrd %>% round(3) %>% paste(collapse = ",")
    sprintf("INFO: background area calculated and stored in new parameter column 'p.bgrd': %s",
            bgrds) %>% message()
  }

  df %>% return()
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

  if (is.null(data$name)) stop("need to have sample names, please parse_file_names first")
  if (is.null(data$category)) stop("need to have categories, please parse_file_names first")
  if (is.null(data$analysis)) stop("need to have analysis, please parse_file_names first")
  if (missing(area)) stop("please specify the column that holds the peak area")
  if (missing(volume)) stop("please specify the column that holds the injected volume")

  fields <- list(
    `.V` = interp(~suppressWarnings(as.numeric(x)), x = substitute(volume)),
    `.A` = interp(~x, x = substitute(area)),
    `.dilution` = interp(~ifelse(is.na(x), 1, x), x = substitute(dilution)))
  stds_filter <- list(interp(~exp, exp = substitute(standards)))

  data <- data %>% do({
    # calculate yield
    df <- mutate_(., .dots = fields)
    stds <-
      df %>% filter_(.dots = stds_filter) %>%
      extract(name, "conc", conc_pattern, convert = T, remove = F) %>%
      mutate( run_size = conc * .V / .dilution,
              yield = .A / run_size,
              p.run_size = round(run_size, 1)) # for proper grouping average to 1 decimal (assuming this is in nmol!)
    # NOTE: might have to introduce this as a parameter
    yield <- mean(stds$yield)

    # add to whole data frame as a parameter
    # because the run size paramter depends on injection volume there are instancs
    # where this is not constant across a run (different amounts injected) and is
    # therefore added in by merge
    df <- df %>%
      left_join(stds %>% select(analysis, p.run_size), # units nmol
                by = "analysis") %>%
      mutate(
        p.yield = yield, # units [Vs/nmol] single value
        amount = .A / yield,
        conc = .dilution * amount / .V ) %>%
      select(-.V, -.A, -.dilution) # remove temp columns

    if (!quiet) {
      sprintf(
        paste(
          "INFO: NOx concentrations and injection amounts (new columns 'conc' and 'amount') calculated from %s standards",
          "\n      Parameter columns mass spec signal 'p.yield' (%.3f) and effective 'p.run_size' (%snmol) added."),
        (stds %>% group_by(conc) %>%
           summarize(label = paste0(conc[1], "uM (", n(), "x)")) %>% ungroup() %>%
           arrange(conc))$label %>% paste(collapse = " & "),
        yield, stds$p.run_size %>% unique() %>% round(3) %>% paste(collapse = ", ")) %>% message()
    }

    return(df)
  })

  return(data)
}

#' Calibrate d15 values with the given standards
#' @param data (can be a grouped_by data set)
#' @param standards a set of isotope standards
#'  Note: they are matched to the data by "category" (not by name)
#' @param d15 the d15 column
#' @return introduces the column d15.cal and parameters p.d15_m and p.d15_b + p.15_stds
#' @note implement single point correction
calibrate_d15 <- function(data, d15, standards = c("USGS-34" = -1.8, "IAEA-NO3" = 4.7), quiet = FALSE) {

  if (is.null(data$category)) stop("need to have categories, please parse_file_names first")
  if (missing(d15)) stop("please specify the column that holds the d15 values to calibrate")

  if (length(standards) == 1) stop("sorry, single point correction is not implemented yet") #FIXME

  stds.df <- data_frame(category = names(standards), .d15.true = standards)
  stds.label <- (stds.df %>% mutate(label = paste0(category, " (", .d15.true, ")")))$label %>% paste(collapse = " & ")
  fields <- list(.d15 = interp(~var, var = substitute(d15)))

  data %>% mutate_(.dots = fields) %>% # creating .d15 once here is easier to handle
    do({

      # regression model
      m <- filter(., category %in% names(standards)) %>%
        group_by(category) %>% summarize(.d15 = mean(.d15)) %>%
        left_join(stds.df, by = "category") %>%
        with(lm(.d15.true ~ .d15))

      if (!quiet) {
        sprintf(
          paste(
            "INFO: d15 values calibrated (new columm 'd15.cal') using %s --> stored in 'p.d15_stds'",
            "\n      Parameter columns for calibration slope (true/measured) 'p.d15_m' (%.3f) and intercept 'p.d15_b' (%.3f) added."),
          stds.label, coef(m)[".d15"], coef(m)["(Intercept)"]) %>% message()
      }

      # add parameters and calculate delta
      mutate(.,
             p.d15_stds = stds.label,
             p.d15_m = coef(m)[".d15"],
             p.d15_b = coef(m)["(Intercept)"],
             d15.cal = .d15 * p.d15_m + p.d15_b)
    }) %>% select(-.d15)
}

#' Calibrate d18 data with the given standards
#' @param data (can be a grouped_by data set)
#' @param d18 the d18 column
#' @param amount the amount column
#' @param volume the volume column
#' @param cell_volume the volume of cells in the vial (has to be same units as volume!)
#' @param standards a set of isotope standards
#'  Note: they are matched to the data by "category" (not by name)
#' @return introduces the column d18.cal and parameters p.d18_m and p.d18_b
#' @export
calibrate_d18 <- function(data, d18, amount = amount, volume = volume, cell_volume,
                          standards = c("USGS-34" = -27.93, "IAEA-NO3" = 25.61), quiet = FALSE) {

  if (is.null(data$category)) stop("need to have categories, please parse_file_names first")
  if (missing(d18)) stop("please specify the column that holds the d18 values to calibrate")
  if (missing(cell_volume)) stop("please specify the denitrifier cells volume")
  if(is.null(data$p.run_size)) stop("paramter p.run_size is not in data set, please make sure to calculate_concentrations first")
  if (length(standards) == 1) stop("sorry, single point correction is not currently supported")

  stds.df <- data_frame(category = names(standards), d18.true = standards)
  stds.label <- (stds.df %>% mutate(label = paste0(category, " (", d18.true, ")")))$label %>% paste(collapse = " & ")
  fields <- list(
    .d18 = interp(~var, var = substitute(d18)),
    .amount = interp(~var, var = substitute(amount)),
    .V = interp(~suppressWarnings(as.numeric(x)), x = substitute(volume)),
    .C_vial = ~.amount / (.V + cell_volume) # effective concentration
  )

  data %>% mutate_(.dots = fields) %>%
    do({

      if (any(.$.V > 100 * cell_volume, na.rm = TRUE)) {
        warning("WARNING: injection volumes are more than 100 * cell volumes, ",
                "please make sure that cell_volume and the 'volume' column have the same units",
                call. = FALSE)
      }

      # regression models (based on concentration)
      ms <- filter(., category %in% names(standards)) %>%
        group_by(category, name, p.run_size, .V) %>%
        summarize(d18 = mean(.d18),
                  C_vial = p.run_size[1] / (.V[1] + cell_volume)
        ) %>%
        left_join(stds.df, by = "category") %>%
        group_by(p.run_size, C_vial) %>% do({
          m <- lm(d18.true ~ d18, data = .)
          data_frame(p.d18_m = coef(m)["d18"],
                     p.d18_b = coef(m)["(Intercept)"])
        }) %>% arrange(p.run_size, desc(C_vial)) # ordering essentially from low to high injection volume


      # the volume dependent regressions
      mm <- lm(p.d18_m ~ C_vial, data = ms)
      mb <- lm(p.d18_b ~ C_vial, data = ms)
      slopes <- ms$p.d18_m %>% round(4) %>% paste(collapse = ", ")
      intercepts <- ms$p.d18_b %>% round(4) %>% paste(collapse = ", ")

      if (!quiet) {
        sprintf(
          paste(
            "INFO: d18 values calibrated (new columm 'd18.cal') using %s --> stored in 'p.d18_stds'",
            "\n      Effective concentration dependence of calibration regressions taken into account.",
            "\n      Parameter columns for calibration slopes (true/measured) 'p.d18_m' (%s) and intercepts 'p.d18_b' (%s) added."),
          stds.label, slopes, intercepts) %>% message()
      }

      # add parameters and calculate delta
      mutate(.,
             p.d18_stds = stds.label,
             p.d18_m = ms$p.d18_m %>% round(4) %>% paste(collapse = ", "),
             p.d18_b = ms$p.d18_b %>% round(4) %>% paste(collapse = ", "),
             d18.cal = .d18 * predict(mm, data.frame(C_vial = .C_vial)) + predict(mb, data.frame(C_vial = .C_vial)))
    }) %>% select(-.d18, -.V, -.amount, -.C_vial)
}
