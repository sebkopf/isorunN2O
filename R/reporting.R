#' Generate an overview table with the data
#' @param the data frame, can already have a group_by if so desired
#' @param ... which data columns to include in data overview
#' @param cutoff the minimum number of samples per group in order to include the group
#' @param .dots traditional list of columns to include (for traditional evluation)
#' @export
generate_data_table <- function(data, ..., cutoff = 1, .dots = c()) {

  # safety checks
  include <- c(lazyeval::lazy_dots(...), .dots)
  if (length(include) == 0) stop("No data columns provided, please select at least 1")

  # generate dots for the summarize call (include mean and sd)
  dots <- c(
    list(n = ~n()),
    lapply(include, function(i) {
      variable <- if (is(i, "lazy")) i$expr else as.name(i)
      list(
        interp(~mean(var, na.rm = T), var = variable),
        interp(~sd(var, na.rm = T), var = variable)
        ) %>%
        setNames(paste0(deparse(variable), c(".avg", ".sd")))
      }) %>%
      unlist()
  )

  # generate overview
  data %>%
    summarize_(.dots = dots) %>%
    filter(n >= cutoff) %>%
    arrange(desc(n))
}

#' generate a summary of the parameters (lists all unique rows so make sure to group appropriately!)
#' @param the data frame, can already have a group_by if so desired
#' @export
generate_parameter_table <- function(data) {
  data %>%
    select(starts_with("p.")) %>%
    unique()
}
