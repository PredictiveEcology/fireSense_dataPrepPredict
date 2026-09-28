## fireSense_dataPrepPredict.R ~459 (prepare_SpreadPredict()): fireSenseCovariatesCreate() was
## never given `rstLCC`, and there was no way to tell it which dom/sec fuel classes a fit chose.
## Prediction reads that from sim$studyAreaWithSpreadParams (the SpreadFit ledger rows
## fireSense_ELFs supplies): each row's params[[1]] column names are the fitted formula's terms,
## and fuelClassRolesFromTermNames() strips "dom_agb_"/"sec_agb_" off any that have them. Prediction
## must build the SAME dom_agb_*/sec_agb_* columns the fit used, never re-derive them from the
## prediction area (PG1+PG4 make class1 the naturally dominant class in this toy landscape); an ELF
## whose fitted terms name no dom_agb_* (an old, per-species fit, or none yet) must still get the
## previous per-fuel-class columns.

## a toy studyAreaWithSpreadParams row: one best-parameter-set row, column names = fitted terms
toyLedgerRow <- function(termNames) {
  p <- matrix(1, nrow = 1, ncol = length(termNames), dimnames = list(NULL, termNames))
  data.frame(polygonID = "toyELF", params = I(list(p)))
}

test_that("fuelClassRolesFromTermNames() strips the prefix, and is NA/NA with no dom_agb_ term", {
  expect_identical(fuelClassRolesFromTermNames(c("b0", "MDC", "dom_agb_Pice_mar", "sec_agb_Pinu_ban")),
                   list(domClass = "Pice_mar", secClass = "Pinu_ban"))
  expect_identical(fuelClassRolesFromTermNames(c("b0", "MDC", "dom_agb_Pice_mar")),
                   list(domClass = "Pice_mar", secClass = NA_character_))
  expect_identical(fuelClassRolesFromTermNames(c("b0", "MDC", "class1", "class2")),
                   list(domClass = NA_character_, secClass = NA_character_))
})

test_that("with no studyAreaWithSpreadParams, prediction gets the previous per-fuel-class columns", {
  out <- toyPrepRun(toyObjects())
  sp <- covDF(out$fireSense_SpreadCovariates)
  expect_true(all(c("class1", "class2") %in% names(sp)))
  expect_false(any(grepl("^dom_agb_|^sec_agb_|^other_agb$", names(sp))))
  ## rstLCC is now passed regardless of fuelCovariates: treedWetland appears (all 0: no LCC 81 here)
  expect_true("treedWetland" %in% names(sp))
  expect_true(all(sp$treedWetland == 0))
})

test_that("a per-species fitted row (no dom_agb_ term) predicts with the previous per-fuel-class columns", {
  o <- toyObjects()
  o$studyAreaWithSpreadParams <- toyLedgerRow(c("b0", "MDC", "youngAge", "class1", "class2"))
  out <- toyPrepRun(o)
  sp <- covDF(out$fireSense_SpreadCovariates)
  expect_true(all(c("class1", "class2") %in% names(sp)))
  expect_false(any(grepl("^dom_agb_|^sec_agb_", names(sp))))
})

test_that("a fitted dom_agb_class2/sec_agb_class1 row builds THOSE columns, not the locally dominant one", {
  ## class1 (Pice_mar + Pinu_ban) totals 3100 here, class2 (Popu_tre) only 550: class1 naturally
  ## dominates. The fitted terms name the opposite, as if the fit had chosen class2 as dominant.
  o <- toyObjects()
  o$studyAreaWithSpreadParams <- toyLedgerRow(c("b0", "MDC", "youngAge", "dom_agb_class2", "sec_agb_class1"))
  out <- toyPrepRun(o)
  sp <- covDF(out$fireSense_SpreadCovariates)

  expect_true(all(c("dom_agb_class2", "sec_agb_class1", "other_agb") %in% names(sp)))
  expect_false(any(c("class1", "class2") %in% names(sp)))

  ## pixel 1 (PG1): class1 (Pice_mar 2000 + Pinu_ban 1000) = 3000, class2 (Popu_tre) = 0
  px1 <- sp[sp$pixelID == 1, ]
  expect_equal(px1$sec_agb_class1, fireSenseUtils::logMinB(3000))
  expect_equal(px1$dom_agb_class2, fireSenseUtils::logMinB(0))

  ## pixel 3 (PG2): class2 (Popu_tre) = 500, class1 = 0
  px3 <- sp[sp$pixelID == 3, ]
  expect_equal(px3$dom_agb_class2, fireSenseUtils::logMinB(500))
  expect_equal(px3$sec_agb_class1, fireSenseUtils::logMinB(0))
})

test_that("two ELFs with different fitted roles each build their own dom/sec columns", {
  ## ELF A (sppA, class1 = conifer, class2 -> decid): fitted dominant = conifer.
  ## ELF B (sppB, per-species Pice_mar/Pinu_ban, class2 -> decid): an old per-species fit.
  sppA <- function() data.table::data.table(LandR = c("Pice_mar", "Pinu_ban", "Popu_tre"),
                                            FuelClass = c("conifer", "conifer", "decid"))
  sppB <- function() data.table::data.table(LandR = c("Pice_mar", "Pinu_ban", "Popu_tre"),
                                            FuelClass = c("Pice_mar", "Pinu_ban", "decid"))
  nfA <- list(wetland = 19L, grass = 16L); nfB <- list(wetgrs = c(16L, 19L))

  o <- toyObjects()
  o$landcoverDT <- NULL
  o$sppEquivs <- list(sppA(), sppB())
  o$nonForestedLCCGroupsList <- list(nfA, nfB)
  o$missingLCCgroupList <- list("grass", "wetgrs")
  o$studyAreaWithSpreadParams <- rbind(
    toyLedgerRow(c("b0", "MDC", "youngAge", "dom_agb_conifer", "sec_agb_decid")),
    toyLedgerRow(c("b0", "MDC", "youngAge", "Pice_mar", "Pinu_ban", "decid"))
  )
  out <- toyPrepRun(o)
  sp <- covDF(out$fireSense_SpreadCovariates)

  ## ELF A's dom/sec columns
  expect_true(all(c("dom_agb_conifer", "sec_agb_decid") %in% names(sp)))
  ## ELF B's per-species columns
  expect_true(all(c("Pice_mar", "Pinu_ban", "decid") %in% names(sp)))
  ## neither ELF ever names a plain "conifer" column (that was ELF A's pre-collapse class)
  expect_false("conifer" %in% names(sp))
})
