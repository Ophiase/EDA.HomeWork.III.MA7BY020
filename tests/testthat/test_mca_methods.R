source("../utils.R", local = TRUE)

test_that("augment.MCA works correctly", {
  library(FactoMineR)
  library(broom)
  data(poison)
  res_mca <- MCA(poison, quanti.sup = 1:2, quali.sup = 3:4, graph = FALSE)
  res_augmented <- augment(res_mca, poison)

  # print(res_augmented)

  expect_type(res_augmented, "list")
  expect_length(res_augmented, 21)
  expect_s3_class(res_augmented, "tbl")
})

test_that("augment.MCA works for columns correctly", {
  library(FactoMineR)
  library(broom)
  data(poison)
  res_mca <- MCA(poison, quanti.sup = 1:2, quali.sup = 3:4, graph = FALSE)
  res_augmented <- augment(res_mca, poison, for_columns=TRUE)

  # print(res_augmented)

  expect_type(res_augmented, "list")
  expect_length(res_augmented, 7)
  expect_s3_class(res_augmented, "tbl")
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

test_that("ggplot for MCA works correctly", {
  ENABLED=FALSE

  library(FactoMineR)
  library(broom)
  data(poison)
  dataset <- poison
  res_mca <- MCA(dataset, quanti.sup = 1:2, quali.sup = 3:4, graph = FALSE)

  res_augmented <- augment(res_mca, dataset)
  res_augmented_cols <- augment(res_mca, dataset, for_columns=TRUE)
  res_tidy <- tidy(res_mca)
  res_glance <- glance(res_mca)

  res_screeplot <- screeplot(res_tidy)
  res_rowplot <- rowplot.MCA_processed(res_augmented)
  res_colplot <- colplot.MCA_processed(res_augmented_cols)
  res_symmetricplot <- symmetricplot.MCA_processed(res_augmented, res_augmented_cols)

  if (ENABLED) {
    combined_plot <- grid.arrange(
      res_screeplot, res_symmetricplot,
      res_rowplot, res_colplot,
      ncol=2, nrow=2) %>% 
      show()
  }

  expect_true(TRUE)
})

test_that("autoplot for MCA works correctly", {
  ENABLED=FALSE

  library(FactoMineR)
  library(broom)
  data(poison)
  dataset <- poison
  res_mca <- MCA(dataset, quanti.sup = 1:2, quali.sup = 3:4, graph = FALSE)

  # availibles types : all, scree, row, col, symmetric
  plot = autoplot(res_mca, type="all") 
  
  if (ENABLED) {
    plot %>% show()
  }

  expect_true(TRUE)
})
