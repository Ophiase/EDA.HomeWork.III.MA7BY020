source("../utils.R", local=TRUE)

test_that("augment.CA works correctly", {
  VERBOSE=FALSE

  library(FactoMineR)
  data(mortality)
  dataset <- mortality[, 1:9]
  res_ca <- CA(dataset, ncp = 2, graph=FALSE)

  augmented <- augment(res_ca, dataset)
  if (VERBOSE) {
    cat("Augmented\n")
    print(names(augmented))
    print(augmented)
    sep()
  }

  expect_type(augmented, "list")
  expect_length(augmented, 16)
  expect_s3_class(augmented, "tbl")
})


test_that("tidy.CA works correctly", {
  VERBOSE=FALSE

  library(FactoMineR)
  data(mortality)
  dataset <- mortality[, 1:9]
  res_ca <- CA(dataset, ncp = 2, graph=FALSE)
  res_tidy <- tidy(res_ca)

  if (VERBOSE) {
    cat("Tidy result:\n")
    print(res_tidy)
    sep()
  }

  expect_type(res_tidy, "list")
  expect_length(res_tidy, 7)
  expect_s3_class(res_tidy, "tbl")

})

test_that("glance.CA works correctly", {
  VERBOSE=FALSE

  library(FactoMineR)
  data(mortality)
  dataset <- mortality[, 1:9]
  res_ca <- CA(dataset, ncp = 2, graph=FALSE)
  res_glance <- glance(res_ca, iris_df)

  if (VERBOSE) {
    cat("Tidy result:\n")
    print(res_glance)
    sep()
  }

  expect_type(res_glance, "list")
  expect_length(res_glance, 8)
  expect_s3_class(res_glance, "tbl")

})

test_that("ggplot for CA works correctly", {
  library(gridExtra)

  ENABLED=FALSE

  library(FactoMineR)
  data(iris)
  iris_df <- as.data.frame(
    lapply(
      iris[, 1:4], function(x) as.numeric(as.factor(x))
    )
  )

  res_ca <- CA(iris_df, ncp = 5, graph=FALSE)

  res_augmented <- augment(res_ca, iris_df)
  res_tidy <- tidy(res_ca)
  res_glance <- glance(res_ca)

  res_screeplot <- screeplot(res_tidy)
  res_rowplot <- rowplot.CA(res_augmented)

  if (ENABLED) {
    combined_plot <- grid.arrange(res_screeplot, res_rowplot, ncol=1, nrow=2) %>% 
      show()
  }

  expect_true(TRUE)
})
