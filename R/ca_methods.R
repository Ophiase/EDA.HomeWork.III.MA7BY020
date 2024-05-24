
#' Augment data with information from a CA object
#'
#' @param x An object of class `CA` from the FactoMineR package.
#' @param data The original data used to create the `CA` object.
#' @param ... Additional arguments (not used).
#'
#' @return A `tibble` with columns containing the original data and additional columns with the row and column coordinates.
#' @export augment CA
#' @export augment
augment.CA <- function(x, data, ...) {
  # candidates to add :
  #   x$row
  #     x$row$coord
  #     x$row$cos2
  #     x$row$contrib
  #     x$row$inertia (list)
  #   x$call
  #     x$call$marge.row (list)
  #     x$call$row.w (list)
  #     x$call$Xtot 150x4

  data <- as_tibble(data)

  # r$row
  if (!is.null(x$row)) {
    data$.coord <- unlist(x$row$coord)
    data$.cos2 <- unlist(x$row$cos2)
    data$.contrib <- unlist(x$row$contrib)
    data$.inertia <- unlist(x$row$inertia)
  }

  # x$call
  if (!is.null(x$call)) {
    if (!is.null(x$call$marge.row)) {
      data$.marge.row <- unlist(x$call$marge.row)
    }
    if (!is.null(x$call$row.w)) {
      data$.row.w <- unlist(x$call$row.w)
    }
    if (!is.null(x$call$Xtot)) {
      data$.Xtot <- x$call$Xtot
    }
  }

  return(data)
}