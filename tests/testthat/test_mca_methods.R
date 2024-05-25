source("../utils.R", local = TRUE)

test_that("augment.MCA works correctly", {
  library(FactoMineR)
  library(broom)
  data(poison)
  res_mca <- MCA(poison, quanti.sup = 1:2, quali.sup = 3:4, graph = FALSE)

  augmented <- augment(res_mca, poison)

  expect_type(augmented, "list")
  expect_length(augmented, 21)
})


test_that("tidy.MCA works correctly", {
  VERBOSE=FALSE

  library(FactoMineR)
  library(broom)
  data(poison)
  res_mca <- MCA(poison, quanti.sup = 1:2, quali.sup = 3:4, graph = FALSE)
  res_tidy <- tidy(res_mca)

  if (VERBOSE) {
    cat("Tidy result:\n")
    print(res_tidy)
    sep()
  }

  expect_type(res_tidy, "list")
  expect_length(res_tidy, 5)
  expect_s3_class(res_tidy, "tbl")

})

test_that("glance.MCA works correctly", {
  VERBOSE=FALSE

  library(FactoMineR)
  library(broom)
  data(poison)
  res_mca <- MCA(poison, quanti.sup = 1:2, quali.sup = 3:4, graph = FALSE)
  res_glance <- glance(res_mca)

  if (VERBOSE) {
    cat("Glance result:\n")
    print(res_glance)
    sep()
  }

  expect_type(res_glance, "list")
  expect_length(res_glance, 6)
  expect_s3_class(res_glance, "tbl")

})