#' Augment data with information from a MCA object
#'
#' @param x An object of class `MCA` from the FactoMineR package.
#' @param data The original data used to create the `MCA` object.
#' @param ... Additional arguments (not used).
#'
#' @return A `tibble` with columns containing the original data and additional columns with the row and column coordinates.
#' @export  augment MCA
#' @example augment
augment.MCA <- function(x, data, ...) {
  # candidates to add :
  # [1] "Name: call (List)"
  # [1] "  - marge.row
  # [1] "  - row.w
  # [1] "  - Xtot
  # [1] "Name: ind (List)"
  # [1] "  - coord:
  # [1] "  - contrib:
  # [1] "  - cos2:
  # [1] "Name: svd (List)"
  # [1] "  - U

  data <- as_tibble(data)

  # x$call
  if (!is.null(x$call)) {
    if (!is.null(x$call$marge.row)) {
      data$.X <- unlist(x$call$marge.row)
    }
    if (!is.null(x$call$row.w)) {
      data$.row.w <- unlist(x$call$row.w)
    }
    # if (!is.null(x$call$Xtot)) {
    #   data$.Xtot <- x$call$Xtot
    # }
  }
  
  # x$ind
  if (!is.null(x$ind)) {
    data$.coord <- unlist(x$ind$coord)
    data$.cos2 <- unlist(x$ind$cos2)
    data$.contrib <- unlist(x$ind$contrib)
  }

  if (!is.null(x$svd)) {
    data$.U <- x$svd$U
  }

  return(data)
}