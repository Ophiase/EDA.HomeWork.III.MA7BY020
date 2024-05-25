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

  res.ca <- CA(iris_df, ncp = 5, graph=FALSE)
  res.tidy <- tidy(res.ca, iris_df)

  if (VERBOSE) {
    cat("Tidy result:\n")
    print(res.tidy)
    sep()
  }

  expect_type(res.tidy, "list")
  expect_length(res.tidy, 7)
  expect_s3_class(res.tidy, "tbl")

})

test_that("glance.CA works correctly", {
  VERBOSE=TRUE

  library(FactoMineR)
  data(iris)
  iris_df <- as.data.frame(
    lapply(
      iris[, 1:4], function(x) as.numeric(as.factor(x))
    )
  )

  res.ca <- CA(iris_df, ncp = 5, graph=FALSE)
  res.glance <- glance(res.ca, iris_df)

  if (VERBOSE) {
    cat("Tidy result:\n")
    print(res.glance)
    sep()
  }

  expect_type(res.glance, "list")
  expect_length(res.glance, 8)
  expect_s3_class(res.glance, "tbl")

})
