source("../utils.R", local=TRUE)

test_that("augment.cca works correctly", {
  VERBOSE = FALSE
  require(vegan)
  data(varechem)

  X <- varechem[, 1:7]
  Y <- varechem[, 8:14]

  if (VERBOSE) {
    print(names(X))
    print(names(Y))
  }

  res_cca <- cca(X, Y)
  res_augmented <- augment(res_cca, X, Y)

  if (VERBOSE) {
    print(res_augmented)
    print(names(res_augmented))
  }

  expect_type(res_augmented, "list")
  expect_length(res_augmented, 34)
  expect_s3_class(res_augmented, "tbl")
})
