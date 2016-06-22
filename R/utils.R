#' error round
round_to_err <- function(x, sd, sig_digits = 1, sep = "+-") {
  if(!is.numeric(x)) return("NA")
  stopifnot(length(x) == length(sd))
  n_deci <- nchar(sub('^[0-9]*\\.([0-9]*)0*$', '\\1', as.character(signif(sd, sig_digits))))
  n_deci[(x %% 1) == 0] <- 0 # numbers w/o any decimal places
  ifelse(is.na(x), "NA", paste0(round(x, n_deci), sep, round(sd, n_deci)))
}
