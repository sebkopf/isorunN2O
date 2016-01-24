#' Evaluate drift
#'
#' This function evaluates drift and can correct for it (use paramter \code{correct = TRUE}).
#'
#' @param data the data frame, must be from a single run because this should really be evaluated
#'    for individual runs
#' @param group expression for what to group by in order to normalize properly
#'    this is also used for color coding the data points in the generated plot (if \code{plot = TRUE})
#' @param correct whether to correct for drift or not (default is FALSE)
#' @param correct_with expression for what all to include in the correction
#' @param method which method to use for drift correction, common approaches
#'    are linear models (lm) and local polynomial regression fitting (loess)
#' @param plot whether to output the drift plots [default is FALSE]
#' @param ... additional parameters for the fitting method
#' @export
#' @return introduces the new columns d15.cor and d18.cor + parameter column p.drift
evaluate_drift <- function(data, d15, d18, group = name, correct = FALSE,
                           correct_with = category %in% c("USGS-34", "IAEA-NO3", "N2O"),
                           plot = TRUE, quiet = FALSE, method = "lm", ...) {

  if (is.null(data$run_number)) stop("need to know the run_number, please parse_file_names first")
  if (missing(d15)) stop("please specify the column that holds the d15 data")
  if (missing(d18)) stop("please specify the columm that holds the d18 data")

  # prepare data frame for correction model
  fields <- list(
    x = ~run_number,
    .d15 = interp(~var, var = substitute(d15)),
    .d18 = interp(~var, var = substitute(d18)),
    .included = interp(~var, var = substitute(correct_with)),
    .group = interp(~var, var = substitute(group)))

  data <- data %>% mutate_(.dots = fields)

  mdf <- data %>%
    filter(.included) %>%
    group_by(.group) %>%
    mutate(d15 = .d15 - mean(.d15), d18 = .d18 - mean(.d18)) %>%
    ungroup()

  # regressions
  corr_cats <- mdf$category %>% unique() %>% paste(collapse = ", ") # for drift into
  args <- list(...)
  if (method == "loess") args <- c(args, list(control = loess.control(surface = "direct"))) # to allow extrapolation
  m15 <- do.call(method, c(list(quote(d15 ~ x), data = quote(mdf)), args))
  m18 <- do.call(method, c(list(quote(d18 ~ x), data = quote(mdf)), args))

  # fitting overview plot
  if (plot) {
    library(cowplot)
    (mdf %>% gather(panel, y, d15, d18) %>%
      ggplot() + aes(x, y) +
      geom_smooth(method = method, se = T, size = 1.5, color = "black", ...) +
      geom_smooth(aes(color = .group, fill = .group), method = method, se = F, ...) +
      geom_point(aes(color = .group), size = 3) +
      facet_wrap(~panel, ncol = 1, scales = "free_y") +
      theme_bw() + theme(legend.position = "left") +
      labs(x = "Run #", color = "", fill = "", y = "deviation from mean in each grouping [permil]")
    ) -> p1

    (rbind(data.frame(residuals = m15$residuals, fitted = m15$fitted, panel = "d15"),
           data.frame(residuals = m18$residuals, fitted = m18$fitted, panel = "d18")) %>%
      ggplot() +
      aes(fitted, residuals) + geom_point() +
      geom_smooth(method = "loess", color = "red", se = F) +
      facet_wrap(~panel, ncol = 1, scales = "free") + theme_bw()
    ) -> p2

    print(
      plot_grid(p1, p2, align = "h", rel_widths = c(3,2)) +
        draw_label(paste("Drift correction with fitting method", method), y = 1, vjust = 1, size = 18))
  }

  if (correct) {
    run_numbers <- data$run_number %>% unique()
    data <- data %>%
      left_join(ggplot2:::predictdf(m15, run_numbers, se = F) %>% rename(.d15.adjust = y, run_number = x), by = "run_number")  %>%
      left_join(ggplot2:::predictdf(m18, run_numbers, se = F) %>% rename(.d18.adjust = y, run_number = x), by = "run_number")  %>%
      mutate(d15.cor = .d15 - .d15.adjust, d18.cor = .d18 - .d18.adjust,
             p.drift = paste0(method, ": ", corr_cats)) %>%
      select(-.d15.adjust, -.d18.adjust)

    if (!quiet & correct) {
      grp_sum <- data %>% filter(.included) %>% group_by(.group) %>%
        summarize(d15.sd = sd(.d15), d18.sd = sd(.d18), d15.cor.sd = sd(d15.cor), d18.cor.sd = sd(d18.cor)) %>%
        mutate(before = paste0(round(d15.sd, 2), "/", round(d18.sd, 2), " (", .group, ")"),
               after = paste0(round(d15.cor.sd, 2), "/", round(d18.cor.sd, 2), " (", .group, ")"))

      sprintf(paste(
        "INFO: %s N2O analyses drift corrected (new data columns 'd15.cor' & 'd18.cor', and parameter 'p.drift' added)",
        "\n      Used the '%s' method with included categories '%s'.",
        "\n      Residual sum of squares: %.3f (d15), %.3f (d18)",
        "\n      Standard deviations in d15/d18 before and after drift correction, by groupings:",
        "\n      Before: %s",
        "\n      After: %s"),
        nrow(data), method, corr_cats,
        sqrt(sum(m15$residuals^2)), sqrt(sum(m18$residuals^2)),
        grp_sum$before %>% paste(collapse = ", "), grp_sum$after %>% paste(collapse = ", ")) %>% message()
    }
  } else {
    # no drift correction
    data <- data %>% mutate(d15.cor = .d15, d18.cor = .d18, p.drift = "none")

    if (!quiet)
      message("INFO: no drift correction is being applied to this run (parameter column 'p.drift' added)")
  }


  data %>% select(-x, -.d15, -.d18, -.included, -.group) %>% return()
}


