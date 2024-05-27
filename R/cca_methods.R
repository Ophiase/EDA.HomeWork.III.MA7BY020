#' @name augment.cca
#' @title Augment data with vegan::cca object
#' 
#' @description Augment data with information from a vegan::cca object
#'
#' @param x An object of class `vegan::cca` from the FactoMineR package.
#' @param data_X The original data used to create the `vegan::cca` object.
#' @param data_Y The original data used to create the `vegan::cca` object.
#' @param for_columns augment columns instead
#' @param ... Additional arguments (not used).
#'
#' @return A `tibble` with columns containing the original data and additional columns with the row and column coordinates.
augment.cca <- function(x, data_X, data_Y, for_columns=FALSE, ...) {
  if (!inherits(x, "cca")) {
    stop("x is not a cca")
  }

  if (is.null(data_X)) {
    stop("data_X cannot be null")
  }

  if (for_columns) {
    result <- data_X %>% 
      t() %>%
      as.data.frame() %>%
      as_tibble()
    
    result$.colsum <- x$.colsum
    result$.coord <- x$CCA$v

    class(result) <- c("cca_processed", "tbl df", "tbl", "data.frame")
    return(result)
  }

  if (!is.null(data_Y)) {
    data <- as_tibble(cbind(data_X, data_Y))
  } else {
    data <- as_tibble(data_X)
  }

  if (!is.null(x$rowsum))
    data <- cbind(data, .rowsum=x$rowsum)
  if (!is.null(x$Ybar))
    data <- cbind(data, .Ybar=x$Ybar)  
  if (!is.null(x$rowsum))
    data <- cbind(data, .coord=x$CCA$u)
  if (!is.null(x$rowsum))
    data <- cbind(data, .wa=x$CCA$wa)

  result <- as_tibble(data)
  class(result) <- c("cca_processed", "tbl df", "tbl", "data.frame")
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
    rownames_to_column(var = "eigen") %>%
    as_tibble() %>%
    setNames(c("eigen", "value"))

  target_length = dim(result)[1]

  result$CCA.u <- pad_na(colMeans(x$CCA$u), target_length)
  result$CCA.v <- pad_na(colMeans(x$CCA$v), target_length)
  result$CCA.bitplot <- pad_na(colMeans(x$CCA$biplot), target_length)
  result$CCA.envcentre <- pad_na(mean(x$CCA$envcentre), target_length)

  class(result) <- c("cca_processed", "tbl df", "tbl", "data.frame")
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
  class(result) <- c("cca_processed", "tbl df", "tbl", "data.frame")
  result
}

# -----------------------------------------------------------------------------------
# GRAPHICS

#' @name screeplot.cca_processed
#' @title screeplot for cca_processed
#'
#' @param tidy_output Result of tidy function over cca
#' @param ... Additional arguments (not used).
#'
#' @return A ggplot
screeplot.cca_processed <- function(tidy_output, ...) {
  ggplot(tidy_output, aes(x = reorder(eigen, value), y = value)) +
    geom_bar(stat = "identity") +
    labs(title = "Scree Plot for CCA", x = "Component", y = "Variance Explained (%)") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
}

#' @name rowplot.cca_processed
#' @title rowplot for cca_processed
#'
#' @param augment_output Result of the augment function over cca
#' @param ... Additional arguments (not used).
#'
#' @return A ggplot
#' 
#' @export 
#' @method rowplot cca_processed
rowplot.cca_processed <- function(augment_output, ...) {
  ggplot(augment_output, aes(x = .coord.CCA1, y = .coord.CCA2, label = rownames(augment_output))) +
    geom_point() +
    geom_text(vjust = -0.5) +
    labs(title = "Row Plot for cca", x = "Dimension 1", y = "Dimension 2")
}

#' @name colplot.cca_processed
#' @title colplot for cca_processed
#'
#' @param augment_output Result of the augment function over cca
#' @param ... Additional arguments (not used).
#'
#' @return A ggplot
#' 
#' @export 
#' @method colplot cca_processed
colplot.cca_processed <- function(augment_output, ...) {
  ggplot(augment_output, aes(x = .coord[, "CCA1"], y = .coord[, "CCA2"], label = rownames(augment_output))) +
    geom_point() +
    geom_text(vjust = -0.5) +
    labs(title = "Col Plot for cca", x = "Dimension 1", y = "Dimension 2")
}

#' @name symmetricplot.cca_processed
#' @title Symmetric plot for cca_processed
#'
#' @param row_output Result of the augment function over cca for rows
#' @param col_output Result of the augment function over cca for columns
#' @param ... Additional arguments (not used).
#'
#' @return A ggplot
#' 
#' @export 
#' @method symmetricplot cca_processed
symmetricplot.cca_processed <- function(row_output, col_output, ...) {
  ggplot() +
    geom_point(data = row_output, aes(x = .coord.CCA1, y = .coord.CCA2, color = "Rows")) +
    geom_point(data = col_output, aes(x = .coord[, "CCA1"], y = .coord[, "CCA2"], color = "Columns")) +
    geom_text(data = row_output, aes(x = .coord.CCA1, y = .coord.CCA2, label = rownames(row_output)), vjust = -0.5, color = "blue") +
    geom_text(data = col_output, aes(x = .coord[, "CCA1"], y = .coord[, "CCA2"], label = rownames(col_output)), vjust = -0.5, color = "red") +
    labs(title = "Symmetric Plot for cca", x = "Dimension 1", y = "Dimension 2") +
    scale_color_manual(values = c("Rows" = "blue", "Columns" = "red")) +
    theme_minimal()
}


