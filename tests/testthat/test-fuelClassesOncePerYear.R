## The ignition and the spread covariates are built from the same fuel classes, which are made once per
## year and shared. Nothing about the covariates changes: a run that prepares both gives exactly what
## runs that prepare each alone give (those build their own fuel classes, as before the sharing).

countFuelClassCalls <- function(code) {
  n <- 0L
  orig <- fireSenseUtils::cohortsToFuelClasses
  testthat::local_mocked_bindings(
    cohortsToFuelClasses = function(...) { n <<- n + 1L; orig(...) },
    .package = "fireSenseUtils")
  force(code)
  n
}

test_that("with both predict modules prepared, the fuel classes are made once per year", {
  times <- list(start = 2001, end = 2003)
  nBoth <- countFuelClassCalls(toyPrepRun(toyObjects(), times = times))
  nIg <- countFuelClassCalls(toyPrepRun(toyObjects(), times = times,
                                        params = list(whichModulesToPrepare = "fireSense_ignitionPredict")))
  nSp <- countFuelClassCalls(toyPrepRun(toyObjects(), times = times,
                                        params = list(whichModulesToPrepare = "fireSense_spreadPredict")))
  expect_identical(nIg, 3L)
  expect_identical(nSp, 3L)
  expect_identical(nBoth, 3L) # was 6
})

test_that("the ignition and spread covariates of a run that shares the fuel classes are unchanged", {
  times <- list(start = 2001, end = 2003)
  both <- toyPrepRun(toyObjects(), times = times)
  ig <- toyPrepRun(toyObjects(), times = times,
                   params = list(whichModulesToPrepare = "fireSense_ignitionPredict"))
  sp <- toyPrepRun(toyObjects(), times = times,
                   params = list(whichModulesToPrepare = "fireSense_spreadPredict"))
  expect_identical(covDF(both$fireSense_SpreadCovariates), covDF(sp$fireSense_SpreadCovariates))
  expect_identical(covDF(both$fireSense_igAndEscapePred_Covariates),
                   covDF(ig$fireSense_igAndEscapePred_Covariates))
})

test_that("with two ELFs the covariates are unchanged and each ELF's fuel classes are made once per year", {
  o <- toyObjects()
  o$landcoverDT <- NULL
  o$sppEquivs <- list(
    data.table::data.table(LandR = c("Pice_mar", "Pinu_ban", "Popu_tre"), FuelClass = c("conifer", "conifer", "decid")),
    data.table::data.table(LandR = c("Pice_mar", "Pinu_ban", "Popu_tre"), FuelClass = c("Pice_mar", "Pinu_ban", "decid")))
  o$nonForestedLCCGroupsList <- list(list(wetland = 19L, grass = 16L), list(wetgrs = c(16L, 19L)))
  o$missingLCCgroupList <- list("grass", "wetgrs")
  n <- 0L
  both <- local({
    nn <- countFuelClassCalls(res <- toyPrepRun(o))
    n <<- nn
    res
  })
  ig <- toyPrepRun(o, params = list(whichModulesToPrepare = "fireSense_ignitionPredict"))
  sp <- toyPrepRun(o, params = list(whichModulesToPrepare = "fireSense_spreadPredict"))
  expect_identical(n, 2L) # one per ELF, was 4
  expect_identical(covDF(both$fireSense_SpreadCovariates), covDF(sp$fireSense_SpreadCovariates))
  expect_identical(covDF(both$fireSense_igAndEscapePred_Covariates),
                   covDF(ig$fireSense_igAndEscapePred_Covariates))
})

test_that("a change to cohortData between the two events is not served from the shared table", {
  run <- function(sim, ev) SpaDES.core::spades(sim, events = ev, debug = FALSE)
  sim <- toyPrepSim(toyObjects())
  ## ignition event, then another module changes the cohorts, then the spread event
  sim <- run(sim, c("init", "getClimateRasters", "prepIgAndEscPredictData"))
  sim$cohortData$B <- sim$cohortData$B * 2L
  both <- run(sim, "prepSpreadPredictData")
  changed <- toyObjects(); changed$cohortData$B <- changed$cohortData$B * 2L
  alone <- toyPrepRun(changed, params = list(whichModulesToPrepare = "fireSense_spreadPredict"))
  expect_identical(covDF(both$fireSense_SpreadCovariates), covDF(alone$fireSense_SpreadCovariates))
})
