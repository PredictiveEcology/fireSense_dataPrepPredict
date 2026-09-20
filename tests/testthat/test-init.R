## `Init()` and the `init` event: what gets built, and what gets scheduled.

test_that("init produces every declared output on the toy landscape", {
  sim <- toyPrepRun()
  md <- SpaDES.core::moduleMetadata(module = moduleName, path = modulePath)
  for (obj in md$outputObjects$objectName) {
    expect_false(is.null(sim[[obj]]), info = obj)
  }
  expect_s4_class(sim$currentClimateRasters, "SpatRaster")
  expect_s4_class(sim$nonForest_timeSinceDisturbance, "SpatRaster")
  expect_s3_class(sim$fireSense_igAndEscapePred_Covariates, "data.table")
  expect_s3_class(sim$fireSense_SpreadCovariates, "data.table")
})

test_that("Init expands a length-one climateVariablesForFire to ignition and spread", {
  objs <- toyObjects()
  objs$climateVariablesForFire <- list("MDC")
  sim <- SpaDES.core::spades(toyPrepSim(objs), events = "init", debug = FALSE)
  expect_identical(sim$climateVariablesForFire, list(ignition = list("MDC"), spread = list("MDC")))
})

test_that("Init leaves a two-element climateVariablesForFire alone", {
  objs <- toyObjects()
  objs$climateVariablesForFire <- list(ignition = "Tmax", spread = "MDC")
  sim <- SpaDES.core::spades(toyPrepSim(objs), events = "init", debug = FALSE)
  expect_identical(sim$climateVariablesForFire, list(ignition = "Tmax", spread = "MDC"))
})

test_that("Init builds landcoverDT from rstLCC_RTM when it is not supplied", {
  ## toy land cover: cells 9, 10, 15 are wetland (19), cells 11, 12 are grass (16),
  ## cells 1-8, 13, 14 are forest (210) and cell 16 is class 20, which is non-flammable
  ## in flammableRTM, so it is dropped. Every value below follows from that map.
  ##
  ## `rstLCC_RTM` is set directly on the simList rather than passed to `simInit()`, because
  ## `.inputObjects` destroys any supplied value (see test-inputObjects.R). Calling `Init()`
  ## directly keeps this test about `Init()`'s landcover logic rather than about that defect.
  ## `landcoverDT` must still be supplied to `simInit()`: without it `.inputObjects` (l.552)
  ## calls `makeLandcoverDT()` with the NULL `rstLCC_RTM` and errors before `Init()` is
  ## reached. It is cleared afterwards so that `Init()` is the thing that builds it.
  sim <- toyPrepSim(toyObjects())
  sim$landcoverDT <- NULL
  sim <- SpaDES.core::spades(sim, events = "init", debug = FALSE)
  dt <- covDF(sim$landcoverDT)
  expect_identical(sort(names(dt)), c("grass", "pixelID", "wetland"))
  ## only the 15 flammable pixels, in cell order
  expect_identical(dt$pixelID, 1:15)
  expect_identical(which(dt$wetland == 1L), c(9L, 10L, 15L))
  expect_identical(which(dt$grass == 1L), c(11L, 12L))
  ## no pixel is in two groups at once
  expect_true(all(dt$wetland + dt$grass <= 1L))
})

test_that("KNOWN BUG: Init resamples landcover continuously, destroying the class codes", {
  ## `Init()` (l.241-245) realigns `rstLCC_RTM` with `postProcess(..., to = rasterToMatch)`
  ## when the geometries differ. `postProcess()` defaults to bilinear resampling, which is
  ## wrong for a *categorical* raster: averaging class codes 19, 16, 210 and 20 produces
  ## values like 186.08 and 43.91, which are not landcover classes at all. `makeLandcoverDT()`
  ## then matches none of them, so every non-forest column comes back NA -- silently, with
  ## the right number of rows and the right column names.
  ##
  ## Verified directly: postProcess(disagg(toyLCC(), 2), to = rasterToMatch) returns
  ## c(210, 210, 210, 210, 186.125, ...) rather than the original integer codes.
  ## The fix is nearest-neighbour (`method = "near"`), as `disagg()` uses here.
  ##
  ## INTENDED: realigning a land-cover map that merely differs in resolution must give the
  ## same landcoverDT as the aligned one. Stated and recorded as failing.
  sim <- toyPrepSim(toyObjects())
  sim$landcoverDT <- NULL
  sim$rstLCC_RTM <- terra::disagg(toyLCC(), fact = 2, method = "near")
  sim <- SpaDES.core::spades(sim, events = "init", debug = FALSE)
  dt <- covDF(sim$landcoverDT)

  ## the shape is right, which is what makes this dangerous
  expect_identical(dt$pixelID, 1:15)
  expect_identical(sort(names(dt)), c("grass", "pixelID", "wetland"))

  ## INTENDED: the same groups as the matching-resolution case above
  expect_failure(expect_identical(which(dt$wetland == 1L), c(9L, 10L, 15L)))
  expect_failure(expect_identical(which(dt$grass == 1L), c(11L, 12L)))

  ## recorded actual state: the classes are gone entirely
  expect_true(all(is.na(dt$wetland)))
  expect_true(all(is.na(dt$grass)))
})

test_that("init schedules exactly the events implied by whichModulesToPrepare", {
  evs <- function(which) {
    sim <- SpaDES.core::spades(
      toyPrepSim(params = list(whichModulesToPrepare = which)), events = "init", debug = FALSE)
    sort(as.data.frame(SpaDES.core::events(sim))$eventType)
  }
  ## ageNonForest and getClimateRasters are unconditional; the two prep events are not
  expect_identical(evs("fireSense_SpreadPredict"),
                   c("ageNonForest", "getClimateRasters", "prepSpreadPredictData"))
  expect_identical(evs("fireSense_IgnitionPredict"),
                   c("ageNonForest", "getClimateRasters", "prepIgAndEscPredictData"))
  ## the ignition/escape table is shared: EscapePredict alone schedules it too
  expect_identical(evs("fireSense_EscapePredict"),
                   c("ageNonForest", "getClimateRasters", "prepIgAndEscPredictData"))
  expect_identical(evs(c("fireSense_SpreadPredict", "fireSense_IgnitionPredict")),
                   c("ageNonForest", "getClimateRasters", "prepIgAndEscPredictData",
                     "prepSpreadPredictData"))
  ## a Fit module name is not a Predict module name: no covariate table is prepared
  expect_identical(evs("fireSense_EscapeFit"), c("ageNonForest", "getClimateRasters"))
})

test_that("KNOWN BUG: the default whichModulesToPrepare prepares no escape covariates", {
  ## The shipped default includes `fireSense_EscapeFit`, but `doEvent` tests for
  ## `fireSense_EscapePredict`; the owner's decision is that the default should be
  ## `fireSense_dataPrepPredict`'s own Predict trio. Either way the default is wrong, so this
  ## records the INTENDED assertion -- that the default contains only Predict module names --
  ## as a currently-failing expectation rather than blessing the shipped value.
  md <- SpaDES.core::moduleMetadata(module = moduleName, path = modulePath)
  default <- md$parameters$default[[which(md$parameters$paramName == "whichModulesToPrepare")]]
  expect_failure(expect_true(all(grepl("Predict$", default))))
  ## the offending element, named so the fix is unambiguous
  expect_true("fireSense_EscapeFit" %in% default)
})

test_that("events reschedule themselves one fireTimeStep ahead", {
  sim <- toyPrepRun(times = list(start = 2001, end = 2001))
  queued <- as.data.frame(SpaDES.core::events(sim))
  expect_setequal(queued$eventType,
                  c("ageNonForest", "getClimateRasters",
                    "prepIgAndEscPredictData", "prepSpreadPredictData"))
  expect_identical(unique(queued$eventTime), 2002)
})
