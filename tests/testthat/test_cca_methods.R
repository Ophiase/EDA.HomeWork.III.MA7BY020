source("../utils.R", local=TRUE)

test_that("augment.cc works correctly", {
  require(CCA)
  data(nutrimouse)
  
  res <- list(X = NULL, Y = NULL)
  res$X <- as.matrix(nutrimouse$gene[,1:10])
  res$Y <- as.matrix(nutrimouse$lipid)
  
  res_cc <- cc(res$X, res$Y)

  # print(dim(res$X))
  # print(dim(res$Y))
  # print(dim(res_cc$scores$xscores))
  # print(dim(res_cc$scores$yscores))

  res_augmented <- augment.cc(res_cc, res)

  expect_type(res_augmented, "list")
  expect_equal(dim(res_augmented$X)[2], dim(res$X)[2] + dim(res_cc$scores$xscores)[2])
  expect_equal(dim(res_augmented$Y)[2], dim(res$Y)[2] + dim(res_cc$scores$yscores)[2])

})