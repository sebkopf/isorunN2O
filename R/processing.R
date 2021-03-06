#' Combine isodat files' data tables
#'
#' This function combines all the data tables from a number of isodat_files
#' into one big data frame for data processing and plotting
#'
#' @param isodat_files isodat objects
#' @param volume which isodat file parameter holds the volume information
#' @param quiet whether the function should output information messages or be quiet (default is to output)
#' @export
get_isodat_data_tables <- function(isodat_files, volume = "Identifier 2", quiet = FALSE) {

  # deprecate
  deprecate_for_switch_to_isoverse("get_isodat_data_tables()", "isoreader::iso_get_vendor_data_table()")

  df <- do.call(bind_rows, lapply(isodat_files, function(i) {
    mutate(i$get_data_table(),
           folder = basename(i$filepath),
           file = i$filename,
           date = i$creation_date,
           analysis = as.numeric(sub("^(MAT253)?(\\d+)_.*$", "\\2", file)),
           volume = suppressWarnings(as.numeric(i$data[[volume]])),
           comment = i$data$Comment,
           preparation = i$data$Preparation)
  })) %>% as_data_frame()
  if (!quiet)
    sprintf("INFO: Isodat data tables from %s files with a total number of %s peaks successfully aggregated.",
            length(isodat_files), nrow(df)) %>% message()
  return(df)
}

#' Identify the N2O peak
#' @param data_table aggregated isodat data table(s)
#' @param peak_rt peak retention time [in s] - either a single value, which will find peaks that start before and end after this retention time; or a vector with two values: c(from, to) which will find peaks that overlap with this interval
#' @param quiet whether the function should output information messages or be quiet (default is to output)
#' @export
select_N2O_peak <- function(data_table, peak_rt, quiet = FALSE) {
  if (is.numeric(peak_rt) && length(peak_rt) == 1) {
    df <- data_table %>%
      filter(Start <= peak_rt, End >= peak_rt) # peak starts before and ends after peak_rt
    rt_msg <- peak_rt
  } else if (is.numeric(peak_rt) && length(peak_rt) == 2 && peak_rt[2] > peak_rt[1]) {
    df <- data_table %>% filter(
      (Start <= peak_rt[1] & End >= peak_rt[1]) | # peak overlaps with start of interval
        (Start <= peak_rt[2] & End >= peak_rt[2]) | # peak overlaps with end of interval
        (Start <= peak_rt[1] & End >= peak_rt[2]) | # peak spans interval
        (Start >= peak_rt[1] & End <= peak_rt[2]) # peak is entirely in interval
    )
    rt_msg <- paste(peak_rt[1], "-", peak_rt[2])
  } else {
    stop("Peak retention time 'peak_rt' must be a single number or vector with two values c(from, to).", call. = FALSE)
  }

  # find file id column
  file_id_col <- if ("file_id" %in% names(df)) "file_id" else "file"

  if (!quiet) {
    sprintf("INFO: %s N2O peaks found in %s files (at retention time %ss), %s other peaks discarded.",
            nrow(df), length(data_table[[file_id_col]] %>% unique()), rt_msg, nrow(data_table) - nrow(df)) %>% message()
    no_N2O_files <- setdiff(unique(data_table[[file_id_col]]), unique(df[[file_id_col]]))
    if ( length(no_N2O_files) > 0) {
      sprintf(
        paste("      NOTE that no N2O peak was found in the following files",
              "(check retention time if there should be one):",
              "\n      %s"),
              no_N2O_files %>% paste(collapse = ", ")) %>% message()
    }
  }



  return(df)
}

#' Select relevant data columns
#'
#' This is just a convenience wrapper for \code{\link{select}} but one that provides helpful information about the selected and discarded columns.
#' @param data data frame
#' @param quiet whether the function should output information messages or be quiet (default is to output)
#' @export
select_columns <- function(data, ..., quiet = FALSE) {
  df <- data %>% select(...)
  if (!quiet)
    sprintf("INFO: Selecting %s columns (%s), discarding %s (%s).",
            ncol(df), names(df) %>% paste(collapse = ", "), ncol(data) - ncol(df),
            setdiff(names(data), names(df)) %>% paste(collapse = ", ")) %>%
    message()
  return(df)
}

#' Parse the file names into categories
#'
#' This function parses the file names for each analysis to extract the actual sample names, analysis number, run order and groupings of analyses into appropriate categories depending on shared sample name prefixes. This automatically groups by folder to properly reconstruct run order (i.e. can be run on a combination of multiple runs if need be). This should be run at the very beginning before anything else because once N2O peaks are identified, the runs without detectable N2O peak in it disappear.
#' @param data data frame
#' @param quiet whether the function should output information messages or be quiet (default is to output)
#' @export
parse_file_names <- function(data, quiet = FALSE) {

  # deprecate
  deprecate_for_switch_to_isoverse("parse_file_names()", "isoreader::iso_mutate_file_info()")

  stopifnot("file" %in% names(data), "folder" %in% names(data))

  df <- data %>% group_by(folder) %>% do({

    # get file names
    files <- .$file %>% unique()

    # remove MAT number and file ending
    groups <- sub("^(MAT)?\\d+__?([^.]+)(-\\d{4})?\\.dxf$", "\\2", files)

    # trim identical beginning and end parts
    min_chars <- min(sapply(groups, nchar))
    front <- plyr::ldply(groups, function(i) data.frame(
      text = i, pos = 1:min_chars,
      char = strsplit(i,NULL)[[1]][1:min_chars]))
    back <- plyr::ldply(groups, function(i) data.frame(
      text = i, pos = 1:min_chars,
      char = strsplit(i,NULL)[[1]][nchar(i):(nchar(i)-min_chars+1)]))
    front_cut <- min(subset(plyr::ddply(front, plyr::.(pos), summarize,
                                        same = all(char == char[1])), same == FALSE)$pos) - 1
    back_cut <- min(subset(plyr::ddply(back, plyr::.(pos), summarize,
                                       same = all(char == char[1])), same == FALSE)$pos) - 1

    groups_trimmed <- groups
    if (front_cut > 0)
      groups_trimmed <- sub(substring(groups[1], 1, front_cut), "", groups_trimmed, fixed = T)

    if (back_cut > 0)
      groups_trimmed <- sub(substring(groups[1], nchar(groups[1]) - back_cut + 1, nchar(groups[1])), "",
                            groups_trimmed, fixed = T)
    merge(., data_frame(
      file = files,
      run_number = order(files),
      name = groups_trimmed,
      category = sub("^([^ ]+) .*$", "\\1", groups_trimmed)),
      by = "file")
  }) %>% ungroup()

  if (!quiet) {
    cat_sum <- (df %>% group_by(category) %>% summarize(n = n()) %>%
                  mutate(combi_cat = ifelse(n == 2, "<< replicates >>", ifelse(n == 1, "<< singles >>", category)),
                         n = ifelse(combi_cat == "<< replicates >>", 1, n), priority = ifelse(n == 1, "second", "first")) %>%
                  group_by(combi_cat, priority) %>% summarize(n = sum(n)) %>% ungroup() %>% arrange(priority, desc(n)) %>%
                  mutate(label = paste0(combi_cat, " (", n, "x)")))$label %>% paste(collapse = ", ")
    sprintf(paste(
      "INFO: Parsed %s file names into new columns for analysis 'name', 'category' and 'run_number'.",
      "\n      Found categories %s"),
      length(data$file %>% unique()), cat_sum) %>% message()
  }
  return(df)
}

#' Change the category of specific analyses
#'
#' This can be used to assign new categories or change existing ones based on an expression that is evaluated in the context of the data frame.
#'
#' @param data (can be a grouped_by data set)
#' @param criteria an expression to identify records whose category should be change to the new value
#' @param value the new category value for the records matching the criteria
#' @export
change_category <- function(data, criteria, value, quiet = FALSE) {
  if (is.null(data$category)) stop("need to have categories, please parse_file_names first")
  if (missing(criteria)) stop("need to pass in a criterion to identify analyses")
  if (missing(value)) stop("need to pass in a value to change category too")

  criteria_quo <- rlang::enquo(criteria)
  df <- data %>% mutate(category = dplyr::if_else(!!criteria_quo, !!value, category))

  if (!quiet) {
    n <- data %>% filter(!!criteria_quo) %>% nrow()
    sprintf(
      "INFO: the category of %s analyses was changed to '%s' (by applying criteria '%s')",
      n, value, rlang::quo_text(criteria_quo)) %>% message()
  }

  return(df)
}

