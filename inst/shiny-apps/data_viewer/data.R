#' Function to get the groups from file names
#' In a data frame that can be merged back with
#' the data table.
#' @param isodat_files vector of isodat file objects
#' @return data.frame with file names and group names
get_data_file_groups <- function (isodat_files) {
  
  # get file names
  files <- names(isodat_files)
  
  # remove MAT number and file ending
  groups <- sub("^MAT\\d+__?([^.]+)(-\\d{4})?\\.dxf$", "\\1", files)
  
  # trim identical beginning and end parts
  min_chars <- min(sapply(groups, nchar)) 
  front <- ldply(groups, function(i) data.frame(text = i, pos = 1:min_chars, char = strsplit(i,NULL)[[1]][1:min_chars]))
  back <- ldply(groups, function(i) data.frame(text = i, pos = 1:min_chars, char = strsplit(i,NULL)[[1]][nchar(i):(nchar(i)-min_chars+1)]))
  front_cut <- min(subset(ddply(front, .(pos), summarize, same = all(char == char[1])), same == FALSE)$pos) - 1
  back_cut <- min(subset(ddply(back, .(pos), summarize, same = all(char == char[1])), same == FALSE)$pos) - 1
  
  groups_trimmed <- groups
  if (front_cut > 0)
    groups_trimmed <- sub(substring(groups[1], 1, front_cut), "", groups_trimmed, fixed = T)
  
  if (back_cut > 0)
    groups_trimmed <- sub(substring(groups[1], nchar(groups[1]) - back_cut + 1, nchar(groups[1])), "", 
                          groups_trimmed, fixed = T)
  data.frame(
    file = files,
    run_number = order(files),
    name = groups_trimmed,
    group = sub("^([^ ]+) .*$", "\\1", groups_trimmed),
    stringsAsFactors = FALSE)
}