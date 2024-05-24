
#' Augment data with information from a MCA object
#'
#' @param x An object of class `MCA` from the FactoMineR package.
#' @param data The original data used to create the `MCA` object.
#' @param ... Additional arguments (not used).
#'
#' @return A `tibble` with columns containing the original data and additional columns with the row and column coordinates.
augment.MCA <- function(x, data, ...) {
  # candidates to add :

  data <- as_tibble(data)

  stop("Not implemented")

  return(data)
}