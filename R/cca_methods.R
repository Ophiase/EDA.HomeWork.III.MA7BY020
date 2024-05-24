
#' Augment data with information from a CCA object
#'
#' @param x An object of class `CCA` from the FactoMineR package.
#' @param data The original data used to create the `CCA` object.
#' @param ... Additional arguments (not used).
#'
#' @return A `tibble` with columns containing the original data and additional columns with the row and column coordinates.
#' @export augment cca
#' @export augment
#' 
augment.cca <- function(x, data, ...) {
  # candidates to add :

  data <- as_tibble(data)

  stop("Not implemented")

  return(data)
}