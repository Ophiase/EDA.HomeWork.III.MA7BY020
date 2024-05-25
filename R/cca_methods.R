#' @name augment.cca
#' @title Augment data with vegan::cca object
#' 
#' @description Augment data with information from a vegan::cca object
#'
#' @param x An object of class `vegan::cca` from the FactoMineR package.
#' @param data_X The original data used to create the `vegan::cca` object.
#' @param data_Y The original data used to create the `vegan::cca` object.
#' @param ... Additional arguments (not used).
#'
#' @return A `tibble` with columns containing the original data and additional columns with the row and column coordinates.
augment.cca <- function(x, data_X, data_Y, ...) {
  if (!inherits(x, "cca")) {
    stop("x is not a cca")
  }

  if (is.null(data_X) && is.null(data_Y)) {
    stop("data_X ou data_Y cannot both be null")
  }

  if (!is.null(data_X) && !is.null(data_Y)) {
    data <- as_tibble(cbind(data_X, data_Y))
  } else if (!is.null(data_X)) {
    data <- as_tibble(data_X)
  } else {
    data <- as_tibble(data_Y)
  }

  if (!is.null(x$rowsum))
    data <- cbind(data, .rowsum=x$rowsum)
  if (!is.null(x$Ybar))
    data <- cbind(data, .u=x$Ybar)  
  if (!is.null(x$rowsum))
    data <- cbind(data, .w=x$CCA$u)
  if (!is.null(x$rowsum))
    data <- cbind(data, .wa=x$CCA$wa)

  return(as_tibble(data))
}

#' @name tidy.cca
#' @title Tidy data with vegan::cca object
#' 
#' @description Tidy data with information from a vegan::cca object
#'
#' @param x An object of class `vegan::cca` from the FactoMineR package.
#' @param ... Additional arguments (not used).
#'
#' @return A `tibble` with statistics about computed parameters.
tidy.cca <- function(x, ...) {
  if (!inherits(x, "cca")) {
    stop("x is not a cca")
  }

  return(x) # not implemented
}