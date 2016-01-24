#' make sure the last plot uses ggplot instead of plotly implementation
#' @note not sure this is the best way to deal with this, temporary fix while plotly causes trouble
#' @export
last_plot <- function() {
  ggplot2::last_plot()
}

#' Generate a simple overview plot
#'
#' This functions generates an overview \code{\link{ggplot}} with either one or multiple y values (latter case makes a tiled plot and ignores the panel parameter). Plot aesthetics like size, text (for hover information if made interactive), color and panelling can be specified.
#'
#' @param ... the y value columns
#' @param size the data point size aesthetic
#' @param text the hover text for data points (see \code{\link{make_itext}} for constructing more complex hover text)
#' @param color what to use for coloring
#' @param panel what to use for panelling (ignored if multiple y columns are supplied)
#'
#' @seealso \code{\link{make_interactive}}
#' @seealso \code{\link{make_itext}}
#' @export
plot_overview <- function(data, ..., size = NULL, text = name, color = category, panel = category) {

  # allow multiple y values (will be gathered if there is more than 1)
  ys <- lazyeval::lazy_dots(...)

  if(length(ys) == 0) stop("an expression for y must be provided")
  if (length(missing <- setdiff(c("run_number"), names(data))) > 0)
    stop("Required colums do not exist: ", paste(missing, collapse = ", "))

  # determine y
  if (length(ys) == 1) { # only 1 y
    fields <- list (panel = interp(~var, var = substitute(panel)))
    data <- data %>% mutate_ (.dots = fields) # create panel field
    y <- deparse(ys[[1]]$expr) # just take first y expression
  } else { # multiple ys
    fields <- lapply(ys, function(y)
      list(interp(~var, var = y$expr)) %>%
        setNames(deparse(y$expr))) %>% unlist()
    data <- data %>%
      mutate_(.dots = fields) %>% # create y fields
      gather_(".panel", ".value", names(fields)) %>% # gather y fields
      mutate(panel = .panel, value = .value) # using . notation just in case already in use
    y <- "value"
  }

  p <- data %>% mutate(.size = 10) %>%
    ggplot() +
    aes_string(x = "run_number", y = y,
               fill = deparse(substitute(color)), text = deparse(substitute(text))) +
    labs(x = "Run #", fill = "") + theme_bw() +
    theme(text = element_text(size = 18), axis.text.x = element_text(angle = 60, hjust = 1),
          legend.position = "right", legend.direction = "vertical") +
    facet_grid(panel~., scales = "free_y")

  # size
  if (!missing(size)) {
    p <- p + aes_string(size = deparse(substitute(size))) +
      scale_size_continuous(range = c(1,5)) +
      geom_point(shape = 21, colour = "black")
  } else {
    p <- p + aes(size = 10) +
    scale_size_continuous(range = c(3,5)) +
    geom_point(shape = 21, colour = "black") +
    guides(size = FALSE)
  }

    #geom_point(shape = 21, colour = "black", size = 5)

  return(p)
}

#' Makes a ggplot interactive using ggplotly.
#' @param p the ggplot to make interactive, by default the last plot
#' @param theme the theme to apply, theme_grey is default (theme_bw is hard to read)
#' @param plus additional ggplot items to add to the plot
#' @export
make_interactive <- function(p = ggplot2:::last_plot(), theme = theme_grey(), plus = NULL) {
  # replace with identiy size scale
  p <- p %+%
    aes(size = NULL) +
    labs(size = " ") +
    theme + plus
  suppressWarnings(p %>% ggplotly())
}

#' Generate more elaborate interactive hover text labels
#' @param ... expression what to use for the interactive text
#'  for example: make_itext(name, d45 = round(d45, 2), ...)
#' @export
make_itext <- function(...) {
  vals <- list(...)
  vars <- paste0(names(vals), ifelse(names(vals) != "", ": ", ""))
  br <- as.list(rep("<br>", length(vals) - 1))
  is <- order(c(seq_along(vars), seq_along(vals), seq_along(br)))
  do.call(paste0, c(vars, vals, br)[is])
}
