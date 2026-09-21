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
  ## `landcoverDT` is supplied to `simInit()` and cleared afterwards, so that `Init()`, not
  ## `.inputObjects`, is the thing that builds it.
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

test_that("Init realigns landcover with nearest neighbour, keeping the class codes", {
  ## `Init()` realigns `rstLCC_RTM` onto `rasterToMatch` when the geometries differ. The
  ## `postProcess()` default is bilinear, which averages class codes 19, 16, 210 and 20 into
  ## values like 186.125 that are no landcover class, so every non-forest column came back NA
  ## with the right shape. A landcover map that merely differs in resolution must give the
  ## same landcoverDT as the aligned one.
  sim <- toyPrepSim(toyObjects())
  sim$landcoverDT <- NULL
  sim$rstLCC_RTM <- terra::disagg(toyLCC(), fact = 2, method = "near")
  sim <- SpaDES.core::spades(sim, events = "init", debug = FALSE)
  dt <- covDF(sim$landcoverDT)

  expect_identical(dt$pixelID, 1:15)
  expect_identical(sort(names(dt)), c("grass", "pixelID", "wetland"))
  expect_identical(which(dt$wetland == 1L), c(9L, 10L, 15L))
  expect_identical(which(dt$grass == 1L), c(11L, 12L))
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

test_that("the default whichModulesToPrepare is the three Predict modules doEvent tests for", {
  ## The default used to include `fireSense_EscapeFit`, which `doEvent` never tests for.
  md <- SpaDES.core::moduleMetadata(module = moduleName, path = modulePath)
  default <- md$parameters$default[[which(md$parameters$paramName == "whichModulesToPrepare")]]
  expect_setequal(default, c("fireSense_IgnitionPredict", "fireSense_EscapePredict",
                             "fireSense_SpreadPredict"))

  ## with the default, init schedules both covariate events
  sim <- SpaDES.core::spades(toyPrepSim(), events = "init", debug = FALSE)
  expect_identical(sort(as.data.frame(SpaDES.core::events(sim))$eventType),
                   c("ageNonForest", "getClimateRasters", "prepIgAndEscPredictData",
                     "prepSpreadPredictData"))
})

test_that("the save event does nothing", {
  sim <- SpaDES.core::spades(toyPrepSim(), events = "init", debug = FALSE)
  before <- sapply(ls(sim), function(nm) reproducible::.robustDigest(sim[[nm]]))
  queued <- as.data.frame(SpaDES.core::events(sim))

  sim <- SpaDES.core::scheduleEvent(sim, SpaDES.core::start(sim), moduleName, "save")
  expect_message(
    expect_no_warning(sim <- SpaDES.core::spades(sim, events = "save", debug = FALSE)),
    "the save event does nothing")

  expect_true("save" %in% evOf(SpaDES.core::completed(sim), "save")$eventType)
  expect_identical(sapply(ls(sim), function(nm) reproducible::.robustDigest(sim[[nm]])), before)
  ## nothing new is scheduled
  expect_identical(as.data.frame(SpaDES.core::events(sim)), queued)
})

test_that("events reschedule themselves one fireTimeStep ahead", {
  sim <- toyPrepRun(times = list(start = 2001, end = 2001))
  queued <- as.data.frame(SpaDES.core::events(sim))
  expect_setequal(queued$eventType,
                  c("ageNonForest", "getClimateRasters",
                    "prepIgAndEscPredictData", "prepSpreadPredictData"))
  expect_identical(unique(queued$eventTime), 2002)
})
