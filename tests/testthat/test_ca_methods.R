source("../utils.R", local=TRUE)

test_that("augment.CA works correctly", {
  VERBOSE=FALSE

  library(FactoMineR)

  data(iris)

  if (VERBOSE) {
    cat("Dataset : Iris\n")
    sep()
    print(head(iris))
    sep()
  }

  iris_df <- as.data.frame(
    lapply(
      iris[, 1:4], function(x) as.numeric(as.factor(x))
    )
  )
  if (VERBOSE) {
    cat("Iris DF (Species is not a scalar)\n")
    print(names(iris_df))
    sep()
  }

  iris_ca <- CA(iris_df, ncp = 2, graph=FALSE)
  if (VERBOSE) {
    cat("Iris CA\n")
    print(names(iris_ca))
    sep()
  }

  augmented <- augment(iris_ca, iris_df)
  if (VERBOSE) {
    cat("Augmented IRIS\n")
    print(names(augmented))
    sep()
  }

  # # Check that the output is a list with two elements
  expect_type(augmented, "list")
  expect_length(augmented, 12)

  # # # Check the row data
  # expect_s3_class(augmented$row, "tbl")
  # expect_equal(nrow(augmented$row), nrow(iris_df))
  # expect_equal(ncol(augmented$row), ncol(iris_df) + ncol(iris_ca$row$coord))

  # # # Check the column data
  # expect_s3_class(augmented$col, "tbl")
  # expect_equal(nrow(augmented$col), ncol(iris_df))
  # expect_equal(ncol(augmented$col), nrow(iris_df) + ncol(iris_ca$col$coord))
})
