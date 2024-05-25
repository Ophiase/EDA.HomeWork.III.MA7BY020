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

  expect_type(augmented, "list")
  expect_length(augmented, 11)
  expect_s3_class(augmented, "tbl")
})


test_that("tidy.CA works correctly", {
  VERBOSE=FALSE

  library(FactoMineR)
  data(iris)
  iris_df <- as.data.frame(
    lapply(
      iris[, 1:4], function(x) as.numeric(as.factor(x))
    )
  )

  iris_ca <- CA(iris_df, ncp = 5, graph=FALSE)

  iris_tidy <- tidy(iris_ca, iris_df)

  if (VERBOSE) {
    cat("Tidy result:\n")
    # print(names(iris_tidy))
    print(iris_tidy)
    sep()
  }

  expect_type(iris_tidy, "list")
  expect_length(iris_tidy, 7)
  expect_s3_class(iris_tidy, "tbl")

})
