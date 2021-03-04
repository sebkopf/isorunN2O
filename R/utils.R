#' error round
round_to_err <- function(x, sd, sig_digits = 1, sep = "+-") {
  if(!is.numeric(x)) return("NA")
  stopifnot(length(x) == length(sd))
  n_deci <- nchar(sub('^[0-9]*\\.([0-9]*)0*$', '\\1', as.character(signif(sd, sig_digits))))
  n_deci[(x %% 1) == 0] <- 0 # numbers w/o any decimal places
  ifelse(is.na(x), "NA", paste0(round(x, n_deci), sep, round(sd, n_deci)))
}

# deprecated functions
deprecate_for_switch_to_isoverse <- function(what, instead) {
  install_cmd <- "devtools::install_github(\"sebkopf/isorunN2O\", ref = \"v0.3.0\")"
  lifecycle::deprecate_stop(
    when = "0.4.0",
    what = what,
    details = sprintf("Please use %s instead or install version 0.3.0 with %s", instead, install_cmd)
  )
}
