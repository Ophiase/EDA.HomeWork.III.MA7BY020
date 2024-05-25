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

  result <- x$CCA$eig %>%
    as.data.frame() %>%
    rownames_to_column(var = "dimension") %>%
    as_tibble() %>%
    setNames(c("dimension", "eigen_value"))

  result$CCA.u <- colMeans(x$CCA$u)
  result$CCA.v <- colMeans(x$CCA$v)
  result$CCA.bitplot <- colMeans(x$CCA$biplot)
  result$CCA.envcentre <- mean(x$CCA$envcentre)

  return(result)
}

#' @name glance.cca
#' @title Glance data with vegan::cca object
#' @description Global metrics about a computed cca object
#'
#' @param x An object of class `cca` from the FactoMineR package.
#' @param ... Additional arguments (not used).
#'
#' @return A `tibble` with statistics about computed parameters.
glance.cca <- function(x, ...) {
  if (!inherits(x, "cca")) {
    stop("x is not a cca")
  }

  result <- tidy(x)[, -1] %>%
    colMeans() %>% t() %>%
    as_tibble()

  # result <- cbind(result, tot.chi=x$tot.chi)
  result <- cbind(result, CCA.tot.chi=x$tot.chi)
  result <- cbind(result, grand.total=x$tot.chi)

  result <- cbind(result, rank=x$CCA$rank)
  result <- cbind(result, qrank=x$CCA$qrank)

  return(as_tibble(result))
}