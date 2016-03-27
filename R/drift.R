#' Evaluate drift
#'
#' This function evaluates drift and can correct for it (use parameter \code{correct = TRUE}). Note that it corrects drift based on the 45/44 and 46/44 ratio, although one could certainly debate the merits of doing the O17 correction first. Although designed to be specific for d45 and d46, it can be used on any two columns really, only note that the new columns are currently always called d45.cor and d46.cor
#'
#' @param data the data frame, must be from a single run because this should really be evaluated
#'    for individual runs
#' @param d45 the name of the d45 column (the direct name, not in parantheses)
#' @param d46 the name of the d46 column (the direct name, not in parantheses)
#' @param group expression for what to group by in order to normalize properly
#'    this is also used for color coding the data points in the generated plot (if \code{plot = TRUE})
#' @param correct whether to correct for drift or not (default is FALSE)
#' @param correct_with expression for what all to include in the correction
#' @param method which method to use for drift correction, common approaches
#'    are linear models (lm) and local polynomial regression fitting (loess)
#' @param plot whether to output the drift plots [default is FALSE]
#' @param span degree of smoothing for the loess (if this method is used)
#' @param ... additional parameters for the fitting method
#' @export
#' @return introduces the new columns d45.cor and d46.cor + parameter column p.drift
evaluate_drift <- function(data, d45, d46, group = name, correct = FALSE,
                           correct_with = category %in% c("USGS-34", "IAEA-NO3", "N2O"),
                           plot = TRUE, quiet = FALSE, method = "lm", span = 0.75, ...) {

  if (is.null(data$run_number)) stop("need to know the run_number, please parse_file_names first")
  if (missing(d45)) stop("please specify the column that holds the d45 data")
  if (missing(d46)) stop("please specify the columm that holds the d46 data")

  # prepare data frame for correction model
  fields <- list(
    x = ~run_number,
    .d45 = interp(~var, var = substitute(d45)),
    .d46 = interp(~var, var = substitute(d46)),
    .included = interp(~var, var = substitute(correct_with)),
    .group = interp(~var, var = substitute(group)))

  data <- data %>% mutate_(.dots = fields)

  mdf <- data %>%
    filter(.included) %>%
    group_by(.group) %>%
    mutate(d45 = .d45 - mean(.d45), d46 = .d46 - mean(.d46)) %>%
    ungroup()

  # regressions
  corr_cats <- mdf$category %>% unique() %>% paste(collapse = ", ") # for drift into
  args <- list(...)
  reg_notes <- method
  if (method == "loess") reg_notes <- paste0(reg_notes, " (span: ", span, ")")
  if (method == "loess") args <- c(args, list(span = span, control = loess.control(surface = "direct")))
  # "direct" is to allow extrapolation
  m45 <- do.call(method, c(list(quote(d45 ~ x), data = quote(mdf)), args))
  m46 <- do.call(method, c(list(quote(d46 ~ x), data = quote(mdf)), args))

  # fitting overview plot
  if (plot) {
    method_args <- list(...)
    (mdf %>% gather(panel, y, d45, d46) %>%
      ggplot() + aes(x, y) +
      stat_smooth(aes(color = .group, fill = .group), method = method, se = F) +
      stat_smooth(method = method, span = span, method.args=method_args, se = T, size = 1.5, color = "black") +
      geom_point(aes(color = .group), size = 3) +
      facet_wrap(~panel, ncol = 1, scales = "free_y") +
      theme_bw() + theme(legend.position = "left") +
      labs(x = "Run #", color = "", fill = "", y = "deviation from mean in each grouping [permil]")
    ) -> p1

    (rbind(data.frame(residuals = m45$residuals, fitted = m45$fitted, panel = "d45"),
           data.frame(residuals = m46$residuals, fitted = m46$fitted, panel = "d46")) %>%
      ggplot() +
      aes(fitted, residuals) + geom_point() +
      stat_smooth(method = "loess", color = "red", se = F) +
      facet_wrap(~panel, ncol = 1, scales = "free") + theme_bw()
    ) -> p2

    print(
      cowplot::plot_grid(p1, p2, align = "h", rel_widths = c(3,2))
      #+cowplot::draw_label(paste("Drift correction with fitting method", method), y = 1, vjust = 1, size = 18)
      )
  }

  if (correct) {
    run_numbers <- data$run_number %>% unique()
    data <- data %>%
      left_join(ggplot2:::predictdf(m45, run_numbers, se = F) %>% rename(.d45.adjust = y, run_number = x), by = "run_number")  %>%
      left_join(ggplot2:::predictdf(m46, run_numbers, se = F) %>% rename(.d46.adjust = y, run_number = x), by = "run_number")  %>%
      mutate(d45.cor = .d45 - .d45.adjust, d46.cor = .d46 - .d46.adjust,
             p.drift = paste0(reg_notes, ": ", corr_cats)) %>%
      select(-.d45.adjust, -.d46.adjust)

    if (!quiet & correct) {
      grp_sum <- data %>% filter(.included) %>% group_by(.group) %>%
        summarize(d45.sd = sd(.d45), d46.sd = sd(.d46), d45.cor.sd = sd(d45.cor), d46.cor.sd = sd(d46.cor)) %>%
        mutate(before = paste0(round(d45.sd, 2), "/", round(d46.sd, 2), " (", .group, ")"),
               after = paste0(round(d45.cor.sd, 2), "/", round(d46.cor.sd, 2), " (", .group, ")"))

      sprintf(paste(
        "INFO: %s N2O analyses drift corrected (new data columns 'd45.cor' & 'd46.cor', and parameter 'p.drift' added)",
        "\n      Used the '%s' method with included categories '%s'.",
        "\n      Residual sum of squares: %.3f (d45), %.3f (d46)",
        "\n      Standard deviations in d45/d46 before and after drift correction, by groupings:",
        "\n      Before: %s",
        "\n      After: %s"),
        nrow(data), method, corr_cats,
        sqrt(sum(m45$residuals^2)), sqrt(sum(m46$residuals^2)),
        grp_sum$before %>% paste(collapse = ", "), grp_sum$after %>% paste(collapse = ", ")) %>% message()
    }
  } else {
    # no drift correction
    data <- data %>% mutate(d45.cor = .d45, d46.cor = .d46, p.drift = "none")

    if (!quiet)
      message("INFO: no drift correction is being applied to this run (parameter column 'p.drift' added)")
  }


  data %>% select(-x, -.d45, -.d46, -.included, -.group) %>% return()
}


