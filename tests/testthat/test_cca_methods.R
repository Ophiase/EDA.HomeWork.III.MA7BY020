source("../utils.R", local=TRUE)

test_that("augment.cc works correctly", {
  require(CCA)
  data(nutrimouse)
  
  res <- list(X = NULL, Y = NULL)
  res$X <- as.matrix(nutrimouse$gene[,1:10])
  res$Y <- as.matrix(nutrimouse$lipid)
  
  res.cc <- cc(res$X, res$Y)

  # print(dim(res$X))
  # print(dim(res$Y))
  # print(dim(res.cc$scores$xscores))
  # print(dim(res.cc$scores$yscores))

  augmented <- augment.cc(res.cc, res)

  expect_type(augmented, "list")
  expect_equal(dim(augmented$X)[2], dim(res$X)[2] + dim(res.cc$scores$xscores)[2])
  expect_equal(dim(augmented$Y)[2], dim(res$Y)[2] + dim(res.cc$scores$yscores)[2])

})