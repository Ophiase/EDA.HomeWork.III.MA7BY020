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

test_that("augment.cca for columns works correctly", {
  VERBOSE = FALSE
  require(vegan)
  data(varechem)
  X <- varechem[, 1:7]
  Y <- varechem[, 8:12]
  res_cca <- cca(X, Y)
  res_augmented <- augment(res_cca, X, Y, for_columns=TRUE)

  expect_type(res_augmented, "list")
  expect_length(res_augmented, 25)
  expect_s3_class(res_augmented, "tbl")
})


test_that("tidy.cca works correctly", {
  VERBOSE = FALSE
  require(vegan)
  data(varechem)
  X <- varechem[, 1:7]
  Y <- varechem[, 8:14]
  res_cca <- cca(X, Y)
  res_tidy <- tidy(res_cca, X, Y)

  if (VERBOSE) {
    print(res_tidy, width=Inf)
  }

  expect_type(res_tidy, "list")
  expect_length(res_tidy, 6)
  expect_s3_class(res_tidy, "tbl")
})

test_that("glance.cca works correctly", {
  VERBOSE = FALSE
  require(vegan)
  data(varechem)
  X <- varechem[, 1:7]
  Y <- varechem[, 8:14]
  res_cca <- cca(X, Y)
  res_glance <- glance(res_cca)

  if (VERBOSE) {
    print(res_glance)
  }

  expect_type(res_glance, "list")
  expect_length(res_glance, 9)
  expect_s3_class(res_glance, "tbl")
})

test_that("glance.cca works correctly", {
  ENABLED = FALSE
  
  require(vegan)
  data(varechem)
  X <- varechem[, 1:7]
  Y <- varechem[, 8:14]
  res_cca <- cca(X, Y)

  res_augmented <- augment(res_cca, X, Y)
  res_augmented_cols <- augment(res_cca, X, Y, for_columns=TRUE)
  res_tidy <- tidy(res_cca)
  res_glance <- glance(res_cca)

  res_screeplot <- screeplot(res_tidy)
  res_rowplot <- rowplot.cca_processed(res_augmented)
  res_colplot <- colplot.cca_processed(res_augmented_cols)
  res_symmetricplot <- symmetricplot.cca_processed(res_augmented, res_augmented_cols)


  if (ENABLED) {
    combined_plot <- grid.arrange(
      res_screeplot, res_symmetricplot, 
      res_rowplot, res_colplot, 
      ncol=2, nrow=2) %>% 
      show()
  }

  expect_true(TRUE)
})
