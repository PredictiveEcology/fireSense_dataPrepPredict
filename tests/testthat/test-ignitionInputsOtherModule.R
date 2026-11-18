## fireSense_dataPrepFit declares `lightningMaps` and `ignitionFitRTM` as outputs, so they count as supplied by
## another module; but when it skips its ignition prep (fireSense_ignitionFit only reuses a ledger fit) it
## never makes them. This module then builds them itself, in prepIgAndEscPredictData (priority 5), after
## dataPrepFit's prepIgnitionFitData (start(sim), priority 1) would have run.

stubFitModule <- function() {
  mp <- withr::local_tempdir(.local_envir = parent.frame())
  dir.create(file.path(mp, "fitStub"))
  writeLines('
defineModule(sim, list(name = "fitStub", description = "", keywords = "",
  authors = person("A", "B", email = "a@b.com", role = c("aut", "cre")),
  childModules = character(0), version = list(fitStub = "0.0.1"),
  timeframe = as.POSIXlt(c(NA, NA)), timeunit = "year", citation = list(), documentation = list(),
  reqdPkgs = list(), parameters = rbind(),
  inputObjects = bindrows(),
  outputObjects = bindrows(createsOutput("lightningMaps", "SpatRaster", desc = ""),
                           createsOutput("ignitionFitRTM", "SpatRaster", desc = ""))))
doEvent.fitStub <- function(sim, eventTime, eventType, debug = FALSE) invisible(sim)
', file.path(mp, "fitStub", "fitStub.R"))
  mp
}

declaredOnlySim <- function(objects) {
  mp <- stubFitModule()
  SpaDES.core::simInit(times = list(start = 2001, end = 2001, timeunit = "year"),
                       modules = c(moduleName, "fitStub"),
                       params = stats::setNames(list(list(igAggFactor = 2)), moduleName),
                       objects = objects,
                       paths = utils::modifyList(testPaths, list(modulePath = c(testPaths$modulePath, mp))))
}

test_that("lightningMaps and ignitionFitRTM another module declares but does not make are built here", {
  objs <- toyObjects()
  lm <- objs$lightningMaps
  objs$lightningMaps <- NULL
  n <- 0L
  testthat::local_mocked_bindings(
    prepare_LightningData = function(...) { n <<- n + 1L; lm }, .package = "fireSenseUtils")
  sim <- suppressMessages(SpaDES.core::spades(declaredOnlySim(objs), debug = FALSE))
  expect_identical(n, 1L)
  expect_s4_class(sim$lightningMaps, "SpatRaster")
  expect_s4_class(sim$ignitionFitRTM, "SpatRaster")
  expect_identical(sort(sim$fireSense_igAndEscapePred_Covariates$pixelID), which(!is.na(terra::values(sim$ignitionFitRTM))) |> sort())
})

test_that("lightningMaps and ignitionFitRTM another module makes are used, not rebuilt", {
  objs <- toyObjects()
  objs$ignitionFitRTM <- terra::setValues(terra::rast(toyRast(1), nlyrs = 1), 1)
  names(objs$ignitionFitRTM) <- "mine"
  testthat::local_mocked_bindings(
    prepare_LightningData = function(...) stop("lightning rebuilt"), .package = "fireSenseUtils")
  sim <- suppressMessages(SpaDES.core::spades(declaredOnlySim(objs), debug = FALSE))
  expect_identical(names(sim$ignitionFitRTM), "mine")
})
