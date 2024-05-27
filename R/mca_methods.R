#' @name augment.MCA
#' @title Augment data with MCA object
#' 
#' @description Augment data with information from a MCA object
#'
#' @param x An object of class `MCA` from the FactoMineR package.
#' @param data The original data used to create the `MCA` object.
#' @param for_columns augment columns instead
#' @param ... Additional arguments (not used).
#'
#' @return A `tibble` with columns containing the original data and additional columns with the row and column coordinates.
#' 
augment.MCA <- function(x, data, for_columns=FALSE, ...) {
  if (!inherits(x, "MCA")) {
    stop("x is not a MCA")
  }

  if (for_columns) {
    result <- x$call$marge.col %>% 
      t() %>% t() %>% as.data.frame() %>% 
      rownames_to_column(var = "column") %>% 
      as_tibble() %>% 
      setNames(c("column", "marge")) %>%
      cbind(.coord=x$svd$V) %>%
      as_tibble()
    
    class(result) <- c("MCA_processed", "tbl df", "tbl", "data.frame")
    return(result)
  }


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

  result <- as_tibble(data)
  class(result) <- c("MCA_processed", "tbl df", "tbl", "data.frame")
  result
}

#' @name tidy.MCA
#' @title Tidy a MCA object
#' @description statistics about a computed MCA object
#'
#' @param x An object of class `MCA` from the FactoMineR package.
#' @param ... Additional arguments (not used).
#'
#' @return A `tibble` with statistics about computed parameters.
tidy.MCA <- function(x, ...) {
  if (!inherits(x, "MCA")) {
    stop("x is not a MCA")
  }

  result <- x$eig %>%
    as.data.frame() %>%
    rownames_to_column(var = "term") %>%
    as_tibble() %>%
    setNames(c("term", "estimate", "var.percentage", "var.cumulative"))

  target_length = dim(result)[1]

  result$coord.mean <- pad_na(rowMeans(x$var$eta2), target_length)

  class(result) <- c("MCA_processed", "tbl df", "tbl", "data.frame")
  result
}

#' @name glance.MCA
#' @title Glance a MCA object
#' @description Global metrics about a computed MCA object
#'
#' @param x An object of class `MCA` from the FactoMineR package.
#' @param ... Additional arguments (not used).
#'
#' @return A `tibble` with statistics about computed parameters.
glance.MCA <- function(x, ...) {
  if (!inherits(x, "MCA")) {
    stop("x is not a MCA")
  }

  result <- colMeans(x$eig) %>%
    t() %>%
    as_tibble() %>%
    setNames(c("eig.mean", "var.percent.mean", "var.cumul.mean"))

  result$eig.1 <- x$eig[1, 1]
  result$eig.2 <- x$eig[2, 1]
  result$tot.inertia <- sum(x$eig[,1])
  # result$rows <- length(x$row$coord)
  # result$cols <- length(x$col$coord)

  class(result) <- c("MCA_processed", "tbl df", "tbl", "data.frame")
  result
}

# -----------------------------------------------------------------------------------
# GRAPHICS

#' @name screeplot.MCA_processed
#' @title screeplot for MCA_processed
#'
#' @param tidy_output Result of tidy function over MCA
#' @param ... Additional arguments (not used).
#'
#' @return A ggplot
screeplot.MCA_processed <- function(tidy_output, ...) {
  ggplot(tidy_output, aes(x = reorder(term, var.percentage), y = var.percentage)) +
    geom_bar(stat = "identity") +
    labs(title = "Scree Plot for MCA", x = "Component", y = "Variance Explained (%)") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
}

#' @name rowplot.MCA_processed
#' @title rowplot for MCA_processed
#'
#' @param augment_output Result of the augment function over MCA
#' @param ... Additional arguments (not used).
#'
#' @return A ggplot
#' 
#' @export 
#' @method rowplot MCA_processed
rowplot.MCA_processed <- function(augment_output, ...) {
  ggplot(augment_output, aes(x = .coord[, "Dim 1"], y = .coord[, "Dim 2"], label = rownames(augment_output))) +
    geom_point() +
    geom_text(vjust = -0.5) +
    labs(title = "Row Plot for MCA", x = "Dimension 1", y = "Dimension 2")
}

#' @name colplot.MCA_processed
#' @title colplot for MCA_processed
#'
#' @param augment_output Result of the augment function over MCA
#' @param ... Additional arguments (not used).
#'
#' @return A ggplot
#' 
#' @export 
#' @method colplot MCA_processed
colplot.MCA_processed <- function(augment_output, ...) {
  ggplot(augment_output, aes(x = .coord.1, y = .coord.2, label = rownames(augment_output))) +
    geom_point() +
    geom_text(vjust = -0.5) +
    labs(title = "Col Plot for MCA", x = "Dimension 1", y = "Dimension 2")
}

#' @name symmetricplot.MCA_processed
#' @title Symmetric plot for MCA_processed
#'
#' @param row_output Result of the augment function over MCA for rows
#' @param col_output Result of the augment function over MCA for columns
#' @param ... Additional arguments (not used).
#'
#' @return A ggplot
#' 
#' @export 
#' @method symmetricplot MCA_processed
symmetricplot.MCA_processed <- function(row_output, col_output, ...) {
  ggplot() +
    geom_point(data = row_output, aes(x = .coord[, "Dim 1"], y = .coord[, "Dim 2"], color = "Rows")) +
    geom_point(data = col_output, aes(x = .coord.1, y = .coord.2, color = "Columns")) +
    geom_text(data = row_output, aes(x = .coord[, "Dim 1"], y = .coord[, "Dim 2"], label = rownames(row_output)), vjust = -0.5, color = "blue") +
    geom_text(data = col_output, aes(x = .coord.1, y = .coord.2, label = rownames(col_output)), vjust = -0.5, color = "red") +
    labs(title = "Symmetric Plot for MCA", x = "Dimension 1", y = "Dimension 2") +
    scale_color_manual(values = c("Rows" = "blue", "Columns" = "red")) +
    theme_minimal()
}

#' @name autoplot.MCA_processed
#' @title Autoplot for MCA_processed
#' @description Automatically generate plots for MCA_processed object
#'
#' @param x An object of class `MCA` from the FactoMineR package.
#' @param type Type of plot to generate. Options are "all", "scree", "row", "col", "symmetric". Defaults to "all".
#' @param ... Additional arguments (not used).
#'
#' @return A ggplot object or a combined grid of ggplot objects
#' 
#' @export 
#' @method autoplot MCA
autoplot.MCA <- function(x, type = c("all", "scree", "row", "col", "symmetric"), ...) {  
  type <- match.arg(type)
  
  tidy_output <- tidy(x)
  augment_output <- augment(x, x$call$X)
  augment_output_col <- augment(x, x$call$X, for_columns=TRUE)
  
  plots <- list()

  if (type == "all" || type == "scree") {
    plots <- append(plots, list(screeplot(tidy_output)))
  }
  
  if (type == "all" || type == "row") {
    plots <- append(plots, list(rowplot.MCA_processed(augment_output)))
  } 
  
  if (type == "all" || type == "col") {
    plots <- append(plots, list(colplot.MCA_processed(augment_output_col)))
  } 
  
  if (type == "all" || type == "symmetric") {
    plots <- append(plots, list(symmetricplot.MCA_processed(augment_output, augment_output_col)))
  }

  if (type == "all") {
    combined_plot <- do.call(ggarrange, c(plots, ncol=2, nrow=2))
    return(combined_plot)
  } else {
    return(plots[[1]])
  }
}
