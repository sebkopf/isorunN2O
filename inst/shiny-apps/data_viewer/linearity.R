#' Linearity and on/off processing
#'
#' This file contains all data processing functions
#' for the linearity and on/off information.
#'

#' name of the file where the linearity is stored
linearity_record_csv <- "linearity_history_DO_NOT_EDIT_BY_HAND.csv"

#' base plot
linearity_base_plot <-
  ggplot(NULL, aes(x, y, fill = !include, shape = !include)) +
  scale_shape_manual (values = c(21, 22)) +
  scale_fill_manual (values = brewer.pal(9, "Set1")[c(2,1)]) +
  theme_bw() + theme(legend.position = "none", axis.title = element_text(size = 20)) +
  labs(x = "Amplitude mass 44 [V]") + facet_wrap(~analysis)

#' generate cutoff slider
make_cutoff_slider <- function(id, settings, min = 0, max = NULL) {
  value <- as.numeric(settings[[id]])
  if (value < min) value <- min
  if (value > max) value <- max
  sliderInput(id, "", min = min, max = max, step = 1, value = value, post = " V")
}

#' get plot data
get_linearity_plot_data <- function(data, y) {
  mutate(
    get_plot_data_table(data, "[Ll]inearity", "Ampl 44", y),
    x = x/1000 # convert from V to mV
  )
}

#' get regression
get_linearity_reg <- function(name, df, x_range) {
  df <- subset(df, x > x_range[1] & x < x_range[2])
  m <- lm(y ~ x, data = df)
  ml <- list(x_min = round(min(df$x), 2), x_max = round(max(df$x), 2), slope = signif(coef(m)[['x']], 3), r2 = round(summary(m)$r.squared, 2))
  ml$msg <- paste0(name, " slope: ", ml$slope, " permil/V (R2: ", sprintf("%.2f", ml$r2), ")")
  return(ml)
}

#' make linearity plot
make_linearity_plot <- function(name, df, x_range) {
  plot.df <- mutate(df, include = x > x_range[1] & x < x_range[2])
  linearity_base_plot %+% plot.df +
    stat_smooth(data = subset(plot.df, include), method = lm) +
    geom_point(size = 4) +
    labs(y = name)
}

#' get ON/OFF table
get_on_off_table <- function(df, pattern = "ON OFF.dxf") {
  on_off_table <- subset(df, grepl(pattern, file))
  if (nrow(on_off_table) > 0)
    ddply(
     on_off_table,
      .(file),
      summarise,
      `ON/OFF File` = sub("^(MAT\\d+).*", "\\1", unique(file)),
      `Std. Dev. d18O` = signif(sd(` 18O/16O`), 3),
      `Std. Dev. d15N` = signif(sd(` 15N/14N`), 3)
    )[-1]
  else
    return(on_off_table)
}

#' make linearity summary
#' @param folder full path to the folder that is being summarised
#' @param data_table the combined data table from all files
#' @param save_download_file if provided (e.g. a temp file name), saves a copy in this file
#' @param summary_dir if provided, will append to linearity summary stored here
generate_linearity_summary <- function(
    folder, data_table, reg_O, reg_N, plot_O, plot_N, # data
    file = file.path(folder, paste0(basename(folder), "_summary.pdf")),
    width = 10, height = 4, font_size = 10, # printing params
    save_download_file = NULL, summary_dir = NULL) {

  # data processing
  on_off_table <- get_on_off_table(data_table)

  # make pdf
  message("INFO: Saving summary pdf to ", file)
  pdf(file, width = width, height = height)
  grid.arrange(
    top = paste0("\nSummary for ON/OFF and linearity test for folder '", basename(folder), "'",
                 "\n", reg_O$msg, "\n", reg_N$msg, "\n"),
    if (nrow(on_off_table) > 0)
      tableGrob(on_off_table,
                theme = ttheme_default(
                  core = list(fg_params=list(cex = 0.7)),
                  colhead = list(fg_params=list(cex = 0.7))),
                rows = NULL)
    else
      textGrob("no ON/OFF data available")
    ,
    plot_O + theme(axis.title = element_text(size = font_size)),
    plot_N + theme(axis.title = element_text(size = font_size)),
    nrow=1, as.table=FALSE)
  dev.off()

  # make a copy to the temp file
  if (!is.null(save_download_file))
    file.copy(file, save_download_file)

  if (!is.null(summary_dir)) {
    summary <- data.frame(
      `Timestamp` = paste(Sys.time()),
      `Run date & time` = paste(unique(subset(data_table, grepl("[Ll]inearity", file))$date)),
      `Folder` = basename(folder),
      `Max ON/OFF Std. Dev. d18O` = if (nrow(on_off_table) > 0) max(on_off_table$`Std. Dev. d18O`) else NA,
      `Min ON/OFF Std. Dev. d18O` = if (nrow(on_off_table) > 0) min(on_off_table$`Std. Dev. d18O`) else NA,
      `Max ON/OFF Std. Dev. d15N` = if (nrow(on_off_table) > 0) max(on_off_table$`Std. Dev. d15N`) else NA,
      `Min ON/OFF Std. Dev. d15N` = if (nrow(on_off_table) > 0) min(on_off_table$`Std. Dev. d15N`) else NA,
      `Linearity d18O slope [permil/V]` = reg_O$slope,
      `d18O R2` = reg_O$r2,
      `d18O Intensity range [min V]` = reg_O$x_min,
      `d18O Intensity range [max V]` = reg_O$x_max,
      `Linearity d15N slope [permil/V]` = reg_N$slope,
      `d15N R2` = reg_N$r2,
      `d15N Intensity range [min V]` = reg_N$x_min,
      `d15N Intensity range [max V]` = reg_N$x_max,
      stringsAsFactors = F, check.names = F
    )

    summary_file <- file.path(summary_dir, linearity_record_csv)
    message("INFO: Adding linearity summary to record stored in ", summary_file)
    if (file.exists(summary_file)) {
      write.table(summary, file = summary_file, row.names = FALSE, sep = ",", append = TRUE, col.names = FALSE)
    } else {
      write.table(summary, file = summary_file, row.names = FALSE, sep = ",", col.names = TRUE)
    }
  }
}

#' process a linearity folder from start to finish
#'
#' @param data_dir the base data directory (where the summary file is kept)
#' @param folder the folder to process (relative to the data_dir)
#' @param amp44_O_min, amp44_O_max the range of considered amplitude 44 voltages for d18O [in V]
#' @param amp44_N_min, amp44_N_max the range of considered amplitude 44 voltages for d15N [in V]
#'
process_linearity_folder <- function(
  data_dir, folder, amp44_O_min = 0, amp44_O_max = 20, amp44_N_min = 0, amp44_N_max = 20) {

  # range settings
  xrange_O <- as.numeric(c(amp44_O_min, amp44_O_max))
  xrange_N <- as.numeric(c(amp44_N_min, amp44_N_max))

  # isodate objects
  folder <- file.path(data_dir, folder)
  files <- load_isodat_files (list.files(folder, pattern = "\\.dxf$", full.names = TRUE))

  # data tables
  data_table <- get_data_tables(files)
  data_table_O <- get_linearity_plot_data(data_table, " 18O/16O")
  data_table_N <- get_linearity_plot_data(data_table, " 15N/14N")

  # regressions
  reg_O <- get_linearity_reg("d18O", data_table_O, xrange_O)
  reg_N <- get_linearity_reg("d15N", data_table_N, xrange_N)

  # plots
  plot_O <- make_linearity_plot("d18O [permil]", data_table_O, xrange_O)
  plot_N <- make_linearity_plot("d15N [permil]", data_table_N, xrange_N)

  # summary
  generate_linearity_summary (folder, data_table, reg_O, reg_N, plot_O, plot_N, summary_dir = data_dir)
}
