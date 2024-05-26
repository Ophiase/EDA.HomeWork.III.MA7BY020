#' @name pad_NA
#' @title Pad with NA
#' 
#' @description Pad values with the length of the target
pad_na <- function(values, target_length) {
  c(values, rep(NA, target_length - length(values)))
}
