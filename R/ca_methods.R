#' @name augment.CA
#' @title Augment data with CA object
#' 
#' @description Augment data with information from a CA object
#'
#' @param x An object of class `CA` from the FactoMineR package.
#' @param data The original data used to create the `CA` object.
#' @param ... Additional arguments (not used).
#'
#' @return A `tibble` with columns containing the original data and additional columns with the row and column coordinates.
augment.CA <- function(x, data, ...) {
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

#' @name tidy.CA
#' @title Tidy a CA object
#' @description statistics about a computed CA object
#'
#' @param x An object of class `CA` from the FactoMineR package.
#' @param ... Additional arguments (not used).
#'
#' @return A `tibble` with statistics about computed parameters.
tidy.CA <- function(x, ...) {
  result <- x$eig %>%
    as.data.frame() %>%
    rownames_to_column(var = "term") %>%
    as_tibble() %>%
    setNames(c("term", "estimate", "var.percentage", "var.cumulative"))

  result$coord.mean <- colMeans(x$row$coord)
  result$contrib.mean <- colMeans(x$row$contrib)
  result$cos2.mean <- colMeans(x$row$cos2)

  result
}
