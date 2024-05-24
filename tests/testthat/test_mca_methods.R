source("../utils.R", local = TRUE)

test_that("augment.MCA works correctly", {
  library(FactoMineR)
  library(broom)

  data(poison)

  res_mca <- MCA(poison, quanti.sup = 1:2, quali.sup = 3:4, graph = FALSE)

  augmented <- augment(res_mca, poison)

  print(names(augmented))
})
