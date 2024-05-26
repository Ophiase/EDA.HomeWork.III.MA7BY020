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

  result <- as_tibble(data)
  class(result) <- c("cca", "tbl_df", "tbl", "data.frame")
  result
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

  target_length = dim(result)[1]

  result$CCA.u <- pad_na(colMeans(x$CCA$u), target_length)
  result$CCA.v <- pad_na(colMeans(x$CCA$v), target_length)
  result$CCA.bitplot <- pad_na(colMeans(x$CCA$biplot), target_length)
  result$CCA.envcentre <- pad_na(mean(x$CCA$envcentre), target_length)

  class(result) <- c("cca", "tbl_df", "tbl", "data.frame")
  result
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

  result <- as_tibble(result)
  class(result) <- c("cca", "tbl_df", "tbl", "data.frame")
  result
}