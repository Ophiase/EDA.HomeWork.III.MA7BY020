#' @name augment.cc
#' @title Augment data with CCA::cc object
#' 
#' @description Augment data with information from a CCA::cc object
#'
#' @param x An object of class `CCA::cc` from the FactoMineR package.
#' @param data The original data used to create the `CCA::cc` object.
#' @param ... Additional arguments (not used).
#'
#' @return A `tibble` with columns containing the original data and additional columns with the row and column coordinates.
augment.cc <- function(x, data, ...) {
  # data <- as_tibble(data)

  names(x$scores$xscores) <- paste0(".xscores_", names(x$scores$xscores))
  names(x$scores$yscores) <- paste0(".yscores_", names(x$scores$yscores))

  if (!is.null(x$scores)) {
    
    if (!is.null(x$scores$xscores)) {
      data$X <- cbind(data$X, x$scores$xscores)
    }

    if (!is.null(x$scores$yscores)) {
      data$Y <- cbind(data$Y, x$scores$yscores)
    }

  }

  # data$X <- as_tibble(data$X)
  # data$Y <- as_tibble(data$Y)

  return(data)
}

# getS3method("augment", "cc")