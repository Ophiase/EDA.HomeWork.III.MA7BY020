#' @name augment.CA
#' @title Augment data with CA object
#'
#' @description Augment data with information from a CA object
#'
#' @param x An oebject of class `CA` from the FactoMineR package.
#' @param data The original data used to create the `CA` object.
#' @param for_columns augment columns instead
#' @param ... Additional arguments (not used).
#'
#' @return A `tibble` with columns containing the original data and additional columns with the row and column coordinates.
augment.CA <- function(x, data, for_columns=FALSE, ...) {
  if (!inherits(x, "CA")) {
    stop("x is not a CA")
  }

  if (for_columns) {
    result <- as_tibble(t(data))
    result$.coord <- unlist(x$col)
    class(result) <- c("CA_processed", "tbl df", "tbl", "data.frame")
    return(result)
  }

  result <- as_tibble(data)

  # r$row
  if (!is.null(x$row)) {
    result$.coord <- unlist(x$row$coord)
    result$.cos2 <- unlist(x$row$cos2)
    result$.contrib <- unlist(x$row$contrib)
    result$.inertia <- unlist(x$row$inertia)
  }

  # x$call
  if (!is.null(x$call)) {
    if (!is.null(x$call$marge.row)) {
      result$.marge.row <- unlist(x$call$marge.row)
    }
    if (!is.null(x$call$row.w)) {
      result$.row.w <- unlist(x$call$row.w)
    }
    if (!is.null(x$call$Xtot)) {
      result$.Xtot <- x$call$Xtot
    }
  }

  class(result) <- c("CA_processed", "tbl df", "tbl", "data.frame")
  return(result)
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
  if (!inherits(x, "CA")) {
    stop("x is not a CA")
  }

  result <- x$eig %>%
    as.data.frame() %>%
    rownames_to_column(var = "eigen") %>%
    as_tibble() %>%
    setNames(c("eigen", "estimate", "var.percentage", "var.cumulative"))

  target_length <- dim(result)[1]
  result$coord.mean <- pad_na(colMeans(x$row$coord), target_length)
  result$contrib.mean <- pad_na(colMeans(x$row$contrib), target_length)
  result$cos2.mean <- pad_na(colMeans(x$row$cos2), target_length)

  class(result) <- c("CA_processed", "tbl f", "tbl", "data.frame")
  result
}

#' @name glance.CA
#' @title Glance a CA object
#' @description Global metrics about a computed CA object
#'
#' @param x An object of class `CA` from the FactoMineR package.
#' @param ... Additional arguments (not used).
#'
#' @return A `tibble` with statistics about computed parameters.
glance.CA <- function(x, ...) {
  if (!inherits(x, "CA")) {
    stop("x is not a CA")
  }

  result <- colMeans(x$eig) %>%
    t() %>%
    as_tibble() %>%
    setNames(c("eig.mean", "var.percent.mean", "var.cumul.mean"))

  result$eig.1 <- x$eig[1, 1]
  result$eig.2 <- x$eig[2, 1]
  result$tot.inertia <- sum(x$eig[, 1])
  result$rows <- length(x$row$coord)
  result$cols <- length(x$col$coord)

  class(result) <- c("CA_processed", "tbl df", "tbl", "data.frame")
  return(result)
}

# -----------------------------------------------------------------------------------
# GRAPHICS

#' @name screeplot.CA_processed
#' @title screeplot for CA_processed
#'
#' @param tidy_output Result of tidy function over CA
#' @param ... Additional arguments (not used).
#'
#' @return A ggplot
screeplot.CA_processed <- function(tidy_output, ...) {
  ggplot(tidy_output, aes(x = eigen, y = var.percentage)) +
    geom_bar(stat = "identity") +
    labs(title = "Scree Plot for CA", x = "Component", y = "Variance Explained (%)")
}

#' @name rowplot.CA_processed
#' @title rowplot for CA_processed
#'
#' @param augment_output Result of the augment function over CA
#' @param ... Additional arguments (not used).
#'
#' @return A ggplot
rowplot.CA_processed <- function(augment_output, ...) {
  ggplot(augment_output, aes(x = .coord[, "Dim 1"], y = .coord[, "Dim 2"], label = rownames(augment_output))) +
    geom_point() +
    geom_text(vjust = -0.5) +
    labs(title = "Row Plot for CA", x = "Dimension 1", y = "Dimension 2")
}
