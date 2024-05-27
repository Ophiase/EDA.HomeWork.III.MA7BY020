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

test_that("augment.CA works for columns correctly", {
  VERBOSE=FALSE

  library(FactoMineR)
  data(mortality)
  dataset <- mortality[, 1:9]
  res_ca <- CA(dataset, ncp = 2, graph=FALSE)
  augmented <- augment(res_ca, dataset, for_columns=TRUE)

  expect_type(augmented, "list")
  expect_length(augmented, dim(dataset)[1] + 1 + 4)
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
  ENABLED=FALSE

  library(gridExtra)
  library(FactoMineR)
  data(mortality)
  dataset <- mortality[, 1:9]
  res_ca <- CA(dataset, ncp = 2, graph=FALSE)

  res_augmented <- augment(res_ca, dataset)
  res_augmented_cols <- augment(res_ca, dataset, for_columns=TRUE)
  res_tidy <- tidy(res_ca)
  res_glance <- glance(res_ca)

  res_screeplot <- screeplot(res_tidy)
  res_rowplot <- rowplot.CA_processed(res_augmented)
  res_colplot <- colplot.CA_processed(res_augmented_cols)
  res_symmetricplot <- symmetricplot.CA_processed(res_augmented, res_augmented_cols)

  if (ENABLED) {
    combined_plot <- grid.arrange(
      res_screeplot, res_symmetricplot,
      res_rowplot, res_colplot,
      ncol=2, nrow=2) %>% 
      show()
  }

  expect_true(TRUE)
})
