#' Augment data with information from a CA object
#'
#' @param x An object of class `CA` from the FactoMineR package.
#' @param data The original data used to create the `CA` object.
#' @param ... Additional arguments (not used).
#'
#' @return A `tibble` with columns containing the original data and additional columns with the row and column coordinates.
#' @export
#'
#' @examples
#' library(FactoMineR)
#' data(hair)
#' hair_ca <- CA(hair$hair, ncp = 5)
#' augment(hair_ca, hair$hair)
augment.CA <- function(x, data, ...) {
  #
  row_coords <- x$row$coord # nolint
  col_coords <- x$col$coord

  row_data <- cbind(data, row_coords)
  col_data <- cbind(t(data), col_coords)

  # Add unique column names
  colnames(row_data) <- c(
    paste0("V", seq_len(ncol(data))),
    paste0("Dim", seq_len(ncol(row_coords)))
  )
  colnames(col_data) <- c(
    paste0("V", seq_len(nrow(data))),
    paste0("Dim", seq_len(ncol(col_coords)))
  )

  list(
    row = as_tibble(row_data),
    col = as_tibble(col_data)
  )
}
