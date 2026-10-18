test_that("analysis coding follows the locked conventions", {
  expect_true(all(DAT$Sex %in% 0:1))
  expect_identical(levels(DAT$Locality_3group), c("Haifa and Krayot", "Or Akiva and surroundings", "Tamra and Nahariya"))
  expect_equal(levels(DAT$Locality_3group)[1L], "Haifa and Krayot")                  # reference level
  expect_equal(as.integer(DAT$Locality_3group), as.integer(SYN$Area))
  expect_equal(mean(DAT$AssessmentDay_centered), 0, tolerance = 1e-12)
  expect_identical(DAT$Ethnicity, as.numeric(SYN$Jewish))                            # Jewish = 1, Muslim or Christian = 0
  expect_equal(DAT$PedestLength_km, SYN$PedestLength_m / 1000)
  expect_identical(DAT$Int_dummy, as.numeric(SYN$integration_score == 4))            # high integration = score 4
  X <- siw_tier_matrix(DAT, CFG$tiers$M2_L)
  expect_identical(colnames(X), c("(Intercept)", "Sex", "Age", "AssessmentDay_centered", "Locality_3groupOr Akiva and surroundings", "Locality_3groupTamra and Nahariya", "Econ_STS"))
})

test_that("the seven tiers use exactly the specified covariates", {
  expect_identical(CFG$tier_order, c("M0", "M1", "M2_L", "M2_E", "M2_EL", "M3", "M4_CC"))
  expect_identical(CFG$tiers$M0, character())
  expect_true("Ethnicity" %in% CFG$tiers$M2_E && !("Locality_3group" %in% CFG$tiers$M2_E))
  expect_true(all(c("Locality_3group", "Ethnicity") %in% CFG$tiers$M2_EL))
  expect_true("Memory" %in% CFG$tiers$M3 && !("Memory_assess" %in% CFG$tiers$M3))
  expect_true("Memory_assess" %in% CFG$tiers$M4_CC && !("Memory" %in% CFG$tiers$M4_CC))
})

test_that("inconsistent coding is reported but never corrected in the primary run", {
  x <- SYN; x$Memory_assess[1] <- 1 - x$Memory_assess[1]; orig <- x$Memory_assess
  d <- siw_prepare_data(x, CFG)
  expect_identical(d$Memory_assess, as.numeric(orig))
  i <- which(x$Jewish == 0)[1]; y <- SYN; y$Muslim[i] <- 0; y$Christian[i] <- 0
  expect_identical(siw_prepare_data(y, CFG)$Ethnicity, as.numeric(y$Jewish))
})
