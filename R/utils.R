#' @name pad_NA
#' @title Pad with NA
#' 
#' @description Pad values with the length of the target
pad_na <- function(values, target_length) {
  c(values, rep(NA, target_length - length(values)))
}


#' @name rowplot
#' @title Row plot
#' 
#' @description Generic implementation of rowplot
rowplot <- function(augment_output, ...) {
  stop("rowplot() is not implemented for this class. Please use method 'rowplot.class()'")
}

#' @name rowplot
#' @title Row plot
#' 
#' @description Generic implementation of rowplot
colplot <- function(augment_output, ...) {
  stop("rowplot() is not implemented for this class. Please use method 'rowplot.class()'")
}
