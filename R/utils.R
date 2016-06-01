#' error round
round_to_err <- function(x, sd, sig_digits = 1) {
  stopifnot(is.numeric(x))
  stopifnot(length(x) == length(sd))
  n_deci <- nchar(sub('^[0-9]*\\.([0-9]*)0*$', '\\1', as.character(x)))
  n_deci[(x %% 1) == 0] <- 0 # numbers w/o any decimal places
  n_deci <- get_n_decimals(signif(sd, sig_digits))
  paste0(round(x, n_deci), "+-", round(sd, n_deci))
}
