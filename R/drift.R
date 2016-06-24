#' Evaluate drift
#'
#' This function evaluates drift and can correct for it (use parameter \code{correct = TRUE}). Note that it corrects drift based on the 45/44 and 46/44 ratio, although one could certainly debate the merits of doing the O17 correction first. Although designed to be specific for d45 and d46, it can be used on any two columns really, only note that the new columns are currently always called d45.drift and d46.drift
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
#' @return introduces the new columns d45.drift and d46.drift + parameter column p.drift
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

  # run using do in case there is a grouping
  df <-
    data %>% do({

    # included
    mdf <-
      filter(., .included) %>%
      group_by(.group) %>%
      mutate(d45 = .d45 - mean(.d45), d46 = .d46 - mean(.d46)) %>%
      ungroup()

    # group info
    grps_text <- ""
    if (!is.null(groups(data))) {
      grps <- groups(data) %>% as.character() %>% sapply(function(i) mdf[[i]][1])
      grps_text <- paste0(names(grps), " = ", grps) %>% paste(collapse = ", ")
    }

    # regressions
    corr_cats <- mdf$category %>% unique() %>% paste(collapse = ", ") # for drift into
    args <- list(...)
    reg_notes <- method
    if (method == "loess") reg_notes <- paste0(reg_notes, " (span: ", span, ")")
    if (method == "loess") args <- c(args, list(span = span, control = loess.control(surface = "direct")))
    # "direct" is to allow extrapolation
    m45 <- do.call(method, c(list(quote(d45 ~ x), data = quote(mdf)), args))
    m46 <- do.call(method, c(list(quote(d46 ~ x), data = quote(mdf)), args))

    # correction
    run_numbers <- .$run_number %>% unique()
    data.drift <-
      left_join(., ggplot2:::predictdf(m45, run_numbers, se = F) %>%
                  rename(.d45.adjust = y, run_number = x), by = "run_number")  %>%
      left_join(ggplot2:::predictdf(m46, run_numbers, se = F) %>%
                  rename(.d46.adjust = y, run_number = x), by = "run_number")  %>%
      mutate(p.drift = paste0(reg_notes, ": ", corr_cats),
             d45.drift = .d45 - .d45.adjust, d46.drift = .d46 - .d46.adjust) %>%
      select(-.d45.adjust, -.d46.adjust)

    # fitting overview plot
    if (plot) {

      # plotting data
      mdf <-
        left_join(mdf, select(data.drift, run_number, d45.drift, d46.drift),
                  by = "run_number") %>%
        group_by(.group) %>%
        mutate(
          `d45 corrected` = d45.drift - mean(d45.drift),
          `d46 corrected` = d46.drift - mean(d46.drift)
        )

      # plot label
      label <- paste0(
        "Correction ", if (correct) "applied" else "NOT applied",
        "\n(method: ", method, ")\n", if (grps_text != "") grps_text)

      # construct plot
      method_args <- list(...)
      (mdf %>% gather(panel, y, d45, d46, `d45 corrected`, `d46 corrected`) %>%
        ggplot() + aes(x, y) +
        stat_smooth(aes(color = .group, fill = .group), method = method, se = F) +
        stat_smooth(method = method, span = span, method.args=method_args, se = T, size = 1.5, color = "black") +
        geom_point(aes(color = .group), size = 3) +
        facet_wrap(~panel, ncol = 2, scales = "free_y") +
        theme_bw() + theme(legend.position = "left") +
        labs(x = "Run #", color = "", fill = "", y = "deviation from mean in each grouping [permil]")
      ) -> p1

      (rbind(data.frame(residuals = m45$residuals, fitted = m45$fitted, panel = "d45 residual vs. fitted"),
             data.frame(residuals = m46$residuals, fitted = m46$fitted, panel = "d46 residual vs. fitted")) %>%
        ggplot() +
        aes(fitted, residuals) + geom_point() +
        stat_smooth(method = "loess", color = "red", se = F) +
        facet_wrap(~panel, ncol = 1, scales = "free") + theme_bw()
      ) -> p2

      print(
        cowplot::plot_grid(p1, p2, align = "h", rel_widths = c(5,2),
                           labels=c(label, ''), hjust = 0, vjust = 1)
        #+cowplot::draw_label(paste("Drift correction with fitting method", method), y = 1, vjust = 1, size = 18)
      )
    }

    # assign data frame
    if (correct) {
      out <- data.drift
    } else {
      out <- mutate(., p.drift = "none", d45.drift = .d45, d46.drift = .d46)
    }

    # info messages
    if (!quiet & correct) {
      grp_sum <- filter(out, .included) %>% group_by(.group) %>%
        summarize(d45.sd = sd(.d45), d46.sd = sd(.d46), d45.drift.sd = sd(d45.drift), d46.drift.sd = sd(d46.drift)) %>%
        mutate(before = paste0(round(d45.sd, 2), "/", round(d46.sd, 2), " (", .group, ")"),
               after = paste0(round(d45.drift.sd, 2), "/", round(d46.drift.sd, 2), " (", .group, ")"))

      if (grps_text != "")
        grps_text <- paste0("\n      group: ", grps_text)

      sprintf(paste(
        "INFO: %s N2O analyses drift corrected (new data columns 'd45.drift' & 'd46.drift', and parameter 'p.drift' added)%s",
        "\n      Used the '%s' method with included categories '%s'.",
        "\n      Residual sum of squares: %.3f (d45), %.3f (d46)",
        "\n      Standard deviations in d45/d46 before and after drift correction, by groupings:",
        "\n      Before: %s",
        "\n      After: %s"),
        nrow(.), grps_text, method, corr_cats,
        sqrt(sum(m45$residuals^2)), sqrt(sum(m46$residuals^2)),
        grp_sum$before %>% paste(collapse = ", "), grp_sum$after %>% paste(collapse = ", ")) %>% message()
    } else if (!quiet & !correct) {
      message("INFO: no drift correction is being applied to this run (parameter column 'p.drift' added)")
      if (grps_text != "") message("      group: ", grps_text)
    }

    return(out)
  })

  df %>% select(-x, -.d45, -.d46, -.included, -.group) %>% return()
}


