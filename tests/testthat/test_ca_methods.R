sep <- function() {
  cat("------------------------------------------------------------\n")
}

test_that("augment.CA works correctly", {
  cat("Dataset : Iris\n")
  sep()
  data(iris)
  print(head(iris))
  sep()

  cat("Iris DF (Species is not a scalar)\n")
  iris_df <- as.data.frame(
    lapply(
      iris[, 1:4], function(x) as.numeric(as.factor(x))
    )
  )
  print(head(iris_df))
  sep()


  cat("Iris CA\n")
  iris_ca <- CA(iris_df, ncp = 2)
  print(names(iris_ca))
  sep()

  # TODO: Fix it, it doesn't work as intended
  cat("Augmented IRIS\n")
  augmented <- augment(iris_ca, iris_df) 
  print(names(augmented))
  sep()

  # Check that the output is a list with two elements
  expect_type(augmented, "list")
  expect_length(augmented, 2)

  # Check the row data
  expect_s3_class(augmented$row, "tbl")
  expect_equal(nrow(augmented$row), nrow(iris_df))
  expect_equal(ncol(augmented$row), ncol(iris_df) + ncol(iris_ca$row$coord))

  # Check the column data
  expect_s3_class(augmented$col, "tbl")
  expect_equal(nrow(augmented$col), ncol(iris_df))
  expect_equal(ncol(augmented$col), nrow(iris_df) + ncol(iris_ca$col$coord))
})
