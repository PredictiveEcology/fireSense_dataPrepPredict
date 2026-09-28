## fireSense_dataPrepPredict.R ~459 (prepare_SpreadPredict()): fireSenseCovariatesCreate() was
## never given `rstLCC`, and there was no way to tell it which dom/sec fuel classes a fit chose.
## Prediction must build the SAME dom_agb_*/sec_agb_* columns the fit used, never re-derive them
## from the prediction area (PG1+PG4 make class1 the naturally dominant class in this toy
## landscape); an ELF with no fuelClassRoles (an old, per-species fit) must still get the previous
## per-fuel-class columns.

test_that("with no fuelClassRoles, prediction gets the previous per-fuel-class columns", {
  out <- toyPrepRun(toyObjects())
  sp <- covDF(out$fireSense_SpreadCovariates)
  expect_true(all(c("class1", "class2") %in% names(sp)))
  expect_false(any(grepl("^dom_agb_|^sec_agb_|^other_agb$", names(sp))))
  ## rstLCC is now passed regardless of fuelCovariates: treedWetland appears (all 0: no LCC 81 here)
  expect_true("treedWetland" %in% names(sp))
  expect_true(all(sp$treedWetland == 0))
})

test_that("a forced fuelClassRoles builds dom_agb_*/sec_agb_* using THAT class, not the locally dominant one", {
  ## class1 (Pice_mar + Pinu_ban) totals 3100 here, class2 (Popu_tre) only 550: class1 naturally
  ## dominates. Force the opposite, as if the fit had chosen class2 as dominant.
  o <- toyObjects()
  o$fuelClassRoles <- list(domClass = "class2", secClass = "class1")
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

test_that("fuelClassRoles with domClass = NA (an unforced or species-fit ELF) predicts as before", {
  o <- toyObjects()
  o$fuelClassRoles <- list(domClass = NA_character_, secClass = NA_character_)
  out <- toyPrepRun(o)
  sp <- covDF(out$fireSense_SpreadCovariates)
  expect_true(all(c("class1", "class2") %in% names(sp)))
  expect_false(any(grepl("^dom_agb_|^sec_agb_", names(sp))))
})
