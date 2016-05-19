#' make sure the last plot uses ggplot instead of plotly implementation
#' @note not sure this is the best way to deal with this, temporary fix while plotly causes trouble
# last_plot <- function() {
#   ggplot2::last_plot()
# }

#' Generate a simple overview plot
#'
#' This functions generates an overview \code{\link{ggplot}} with either one or multiple y values (latter case makes a tiled plot and ignores the panel parameter). Plot aesthetics like size, text (for hover information if made interactive), color and panelling can be specified.
#'
#' @param ... the y value columns
#' @param size the data point size aesthetic
#' @param shape the data point shape aesthetic
#' @param text the tooltip text for data points (see \code{\link{make_itext}} for constructing more complex hover text)
#' @param color what to use for coloring
#' @param panel what to use for panelling (ignored if multiple y columns are supplied)
#'
#' @seealso \code{\link{make_interactive}}
#' @seealso \code{\link{make_itext}}
#' @export
plot_overview <- function(data, ..., size = NULL, shape = NULL, text = NULL, color = category, panel = category) {

  # allow multiple y values (will be gathered if there is more than 1)
  ys <- lazyeval::lazy_dots(...)

  if(length(ys) == 0) stop("an expression for y must be provided")
  if (length(missing <- setdiff(c("run_number"), names(data))) > 0)
    stop("Required colums do not exist: ", paste(missing, collapse = ", "))

  # aes
  ylab <- deparse(ys[[1]]$expr)
  fields <- list (
    x = interp(~run_number),
    y = interp(~var, var = ys[[1]]$expr),
    color = interp(~var, var = substitute(color)),
    panel = interp(~var, var = substitute(panel))
  )

  if (!missing(size)) {
    fields$size <- interp(~var, var = substitute(size))
  }

  if (!missing(shape)) {
    fields$shape <- interp(~var, var = substitute(shape))
  }

  if (!missing(text)) {
    fields$label = interp(~var, var = substitute(text))
  }

  # multiple (paneled) ys
  if (length(ys) > 1) {
    dots <- lapply(ys, function(y)
      list(interp(~var, var = y$expr)) %>%
        # add . to ensure original columns are still available for mouseover text
        setNames(paste0(".", deparse(y$expr)))) %>% unlist()
    data <- data %>%
      mutate_(.dots = dots) %>% # create y fields
      gather_(".panel", ".value", names(dots)) %>%  # gather y fields
      # revert .s (to be on the safe side during gather)
      rename(panel = .panel, value = .value) %>%
      # remove . from the panel values again
      mutate(panel = sub("^\\.", "", panel))

    # update aesthetics
    ylab <- ""
    fields$y = interp(~value)
    fields$panel = interp(~panel)
  }

  # base plot
  p <- data %>%
    mutate_(.dots = fields) %>%
    ggplot()

  # tooltip
  if (!missing(text)) {
    p <- p + aes(text = label)
  }

  # shape
  if (!missing(shape)) {
    p <- p + aes(shape = shape) +
      scale_shape_manual(values = 21:25) +
      labs(shape = "")
  }

  # size
  if (!missing(size)) {
    p <- p + aes(size = size) +
      scale_size_continuous(range = c(1,5)) +
      labs(size = deparse(substitute(size)))
  }

  # shape & size
  if (!missing(shape) && !missing(size))
    p <- p + geom_point(colour = "black")
  else if (!missing(shape) && missing(size))
    p <- p + geom_point(colour = "black", size = 4)
  else if (missing(shape) && !missing(size))
    p <- p + geom_point(colour = "black", shape = 21)
  else
    p <- p + geom_point(colour = "black", shape = 21, size = 4)


  # main plot
  p <- p +
    aes(x = x, y = y, fill = color) +
    labs(x = "Run #", y = ylab, fill = "") + theme_bw() +
    theme(text = element_text(size = 18), axis.text.x = element_text(angle = 60, hjust = 1),
          legend.position = "right", legend.direction = "vertical") +
    facet_grid(panel~., scales = "free_y")

  # NOTE: this is to avoid issues with plotly (make sure the mappings have class uneval)
  if (!is(p$mappig, "uneval"))
    class(p$mapping) <- "uneval"

  return(p)
}

#' Makes a ggplot interactive using ggplotly.
#'
#' Note that in order for these plots to render properly in knitted RMarkdown documents, you have to actually load the plotly library in your code. Not doing it in this function itself because it's generally considered bad practice.
#'
#' @param p the ggplot to make interactive, by default the last plot
#' @param tooltip vector of mouseover text to include (x, y, size, fill, text) and
#'    which order. By default, shows only "text" aesthetic if it is mapped, otherwise shows all.
#' @param theme the theme to apply, theme_bw is the default
#' @param plus additional ggplot items to add to the plot
#' @export
make_interactive <- function(p = ggplot2:::last_plot(), tooltip = tooltip_default(),
                             theme = theme_bw(), plus = NULL) {

  # default tooltips
  tooltip_default <- function() {
    if ( "text" %in% names(p$mapping))
      return("text")
    else
      return("all")
  }

  # replace with identiy size scale
  p <- p + theme + plus

  # NOTE: this is to avoid issues with plotly (make sure the mappings have class uneval)
  if (!is(p$mappig, "uneval"))
    class(p$mapping) <- "uneval"

  suppressWarnings(p %>% plotly::ggplotly(tooltip = tooltip))
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
