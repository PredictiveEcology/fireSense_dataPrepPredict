## The vegetation a run predicts with: at init, each vegetation object is the latest dataset available
## at start(sim), unless another module simulates the object (declares it as an output), when the
## simulated state is the latest. A hindcast is a run with no vegetation module and times in a past
## year: it takes the element of fireSense_dataPrepFit's per-year lists of the data year strictly before
## floor(start(sim)), as the fit pairs a fire year with its vegetation (SCANFI of year Y already shows
## Y's burns). A forecast predicts with Biomass_core's vegetation as before, and the objects no module
## simulates take the data year not after floor(start(sim)).
##
## The singletons in `toyObjects()` are the decoy ("last year", what a run without lists uses). Year
## 2002 differs from them in: cohort biomass (Pice_mar in pixel group 1 is 5000, not 2000),
## flammability (cell 15 is not flammable) and non-forest time since disturbance (wetland cell 9
## burned 3 years ago). Year 2003 differs again (Pice_mar 6000; cell 9 TSD 4).

yearLists <- function() {
  decoy <- toyObjects()
  yrs <- paste0("year", 2001:2003)
  perYear <- function(x) stats::setNames(rep(list(x), length(yrs)), yrs)

  cd <- toyCohortData()
  cd$B[cd$pixelGroup == 1L & cd$speciesCode == "Pice_mar"] <- 5000L
  cd3 <- data.table::copy(cd)
  cd3$B[cd3$pixelGroup == 1L & cd3$speciesCode == "Pice_mar"] <- 6000L
  flam <- toyRast(c(rep(1, 14), 0, 0))
  tsd <- toyRast(c(5, rep(30, 13), 5, 30))
  tsd[9] <- 3
  tsd3 <- toyRast(c(5, rep(30, 13), 5, 30))
  tsd3[9] <- 4
  lcDT <- data.table::data.table(pixelID = 1:14, wetland = as.integer(1:14 %in% c(9, 10)),
                                 grass = as.integer(1:14 %in% c(11, 12)))

  lists <- list(cohortDatas = perYear(decoy$cohortData),
                pixelGroupMaps = perYear(decoy$pixelGroupMap),
                landcoverDTs = perYear(decoy$landcoverDT),
                nonForest_timeSinceDisturbances = perYear(decoy$nonForest_timeSinceDisturbance),
                rstLCCs = perYear(toyLCC()),
                flammableRTMs = perYear(decoy$flammableRTM),
                standAgeMaps = perYear(decoy$standAgeMap))
  lists$cohortDatas$year2002 <- cd
  lists$cohortDatas$year2003 <- cd3
  lists$landcoverDTs$year2002 <- lcDT
  lists$landcoverDTs$year2003 <- lcDT
  lists$nonForest_timeSinceDisturbances$year2002 <- tsd
  lists$nonForest_timeSinceDisturbances$year2003 <- tsd3
  lists$flammableRTMs$year2002 <- flam
  lists$flammableRTMs$year2003 <- flam
  ## the decoy's own `rstLCCs` (unnamed, one element) is replaced by the per-year list
  utils::modifyList(decoy, lists, keep.null = TRUE)
}

## a hindcast job: no vegetation module, reps as steps of 0.001 year inside one past data year
hindcast <- function(objs = yearLists(), start = 2003, end = start + 0.0025, ...) {
  toyPrepRun(objs, params = list(fireTimeStep = 0.001, ...), times = list(start = start, end = end))
}

## a toy vegetation module, as Biomass_core: it declares cohortData and pixelGroupMap as outputs and sets
## them at init (B 7000 in pixel group 1, newer than any real year)
toyVegModulePath <- function() {
  root <- withr::local_tempdir(.local_envir = parent.frame())
  dir.create(file.path(root, "toyVeg"))
  writeLines(c(
    'defineModule(sim, list(',
    '  name = "toyVeg", description = "", keywords = "", authors = person("a", "b", role = "aut"),',
    '  childModules = character(0), version = list(toyVeg = "0.0.1"), timeframe = as.POSIXlt(c(NA, NA)),',
    '  timeunit = "year", citation = list(), documentation = list(), reqdPkgs = list(),',
    '  parameters = rbind(),',
    '  inputObjects = bindrows(expectsInput("toyVegCohortData", "data.table", ""),',
    '                          expectsInput("toyVegPixelGroupMap", "SpatRaster", "")),',
    '  outputObjects = bindrows(createsOutput("cohortData", "data.table", ""),',
    '                           createsOutput("pixelGroupMap", "SpatRaster", ""))))',
    'doEvent.toyVeg <- function(sim, eventTime, eventType) {',
    '  if (eventType == "init") {',
    '    sim$cohortData <- sim$toyVegCohortData',
    '    sim$pixelGroupMap <- sim$toyVegPixelGroupMap',
    '  }',
    '  invisible(sim)',
    '}'), file.path(root, "toyVeg", "toyVeg.R"))
  root
}

forecast <- function(start = 2003, end = 2005) {
  objs <- yearLists()
  cd <- toyCohortData()
  cd$B[cd$pixelGroup == 1L & cd$speciesCode == "Pice_mar"] <- 7000L
  objs$toyVegCohortData <- cd
  objs$toyVegPixelGroupMap <- objs$pixelGroupMap
  objs$projectedClimateRasters <- toyClimate(2001:2005)
  vegPath <- toyVegModulePath()
  sim <- SpaDES.core::simInit(
    times = list(start = start, end = end, timeunit = "year"),
    modules = c(moduleName, "toyVeg"),
    params = stats::setNames(list(list(igAggFactor = 2)), moduleName),
    objects = objs,
    paths = utils::modifyList(testPaths, list(modulePath = c(testPaths$modulePath, vegPath))))
  SpaDES.core::spades(sim, debug = FALSE)
}

test_that("a hindcast takes the data year strictly before start(sim) from every per-year list", {
  sim <- hindcast()
  df <- covDF(sim$fireSense_SpreadCovariates)
  ## year 2002's flammableRTM has 14 flammable cells, the singleton 15
  expect_identical(df$pixelID, 1:14)
  ## year 2002's TSD: wetland cell 9 burned 3 y ago, so it is young (and so not wetland)
  expect_identical(which(df$youngAge == 1), c(5L, 6L, 9L))
  expect_identical(which(df$wetland == 1L), 10L)
  ## year 2002's cohorts: pixel group 1 has more class1 biomass than the singleton's
  base <- covDF(toyPrepRun()$fireSense_SpreadCovariates)
  expect_gt(df$class1[1], base$class1[1])
  ## the objects other modules read are year 2002's too
  expect_identical(as.vector(terra::values(sim$flammableRTM)), c(rep(1, 14), 0, 0))
  expect_identical(as.vector(terra::values(sim$nonForest_timeSinceDisturbance))[9], 3)
  expect_identical(max(sim$cohortData$B), 5000L)
  expect_identical(sim$landcoverDT, yearLists()$landcoverDTs$year2002)
  ## 2003.5 still takes 2002's; a hindcast of a data year does not see that year's data (2002 takes
  ## 2001's decoys); a year after the last data year takes the last
  expect_identical(max(hindcast(start = 2003.5, end = 2003.5025)$cohortData$B), 5000L)
  sim2002 <- hindcast(start = 2002)
  expect_identical(max(sim2002$cohortData$B), 2000L)
  expect_identical(sim2002$landcoverDT, yearLists()$landcoverDTs$year2001)
  objs <- yearLists()
  objs$projectedClimateRasters <- toyClimate(2001:2010)
  expect_identical(max(hindcast(objs, start = 2010, end = 2010.0025)$cohortData$B), 6000L)
})

test_that("vegetation and time since disturbance stay static over the steps inside a hindcast year", {
  ## three steps of 0.001 year, as a hindcast job runs its reps: ageNonForest runs but no calendar year
  ## passes, so TSD is unchanged (a burn in that year counts from the next, as in the fit)
  sim <- hindcast()
  done <- as.data.frame(SpaDES.core::completed(sim))
  expect_identical(sum(done$eventType == "prepSpreadPredictData"), 3L)
  expect_identical(sum(done$eventType == "prepIgAndEscPredictData"), 3L)
  expect_true("ageNonForest" %in% done$eventType)
  expect_identical(as.vector(terra::values(sim$nonForest_timeSinceDisturbance))[9], 3)
  ## the climate layer is that of the calendar year (2003) at every step; MDC 2003 is 300 + cell
  df <- covDF(sim$fireSense_SpreadCovariates)
  expect_identical(df$MDC, 300 + 1:14)
  expect_identical(which(df$youngAge == 1), c(5L, 6L, 9L))
  expect_gt(NROW(sim$fireSense_igAndEscapePred_Covariates), 0L)
})

test_that("a forecast predicts with the simulated vegetation, newer than any real year, as before", {
  sim <- forecast()
  ## cohortData and pixelGroupMap are the vegetation module's (B 7000), not year 2003's (6000)
  expect_identical(max(sim$cohortData$B), 7000L)
  df <- covDF(sim$fireSense_SpreadCovariates)
  expect_gt(df$class1[1], covDF(hindcast(start = 2003, end = 2003.001)$fireSense_SpreadCovariates)$class1[1])
  ## the objects no module simulates are the latest real year's (2003)
  expect_identical(sim$landcoverDT, yearLists()$landcoverDTs$year2003)
  expect_identical(as.vector(terra::values(sim$flammableRTM)), c(rep(1, 14), 0, 0))
  ## TSD started as 2003's (cell 9: 4) and aged by one each whole year (2004, 2005)
  expect_true("ageNonForest" %in% as.data.frame(SpaDES.core::completed(sim))$eventType)
  expect_identical(as.vector(terra::values(sim$nonForest_timeSinceDisturbance))[9], 6)
})

test_that("a run with no vegetation module from the second data year takes the first's; TSD ages", {
  ## year 2001's elements are the decoys, so this is what a run without lists gave
  sim <- toyPrepRun(yearLists(), times = list(start = 2002, end = 2003))
  expect_identical(max(sim$cohortData$B), 2000L)
  expect_true("ageNonForest" %in% as.data.frame(SpaDES.core::completed(sim))$eventType)
  ## TSD of cell 15 is 5 in 2001's data, aged in 2003
  expect_identical(as.vector(terra::values(sim$nonForest_timeSinceDisturbance))[15], 6)
})

test_that("without per-year lists the supplied objects stand; a start before the first data year stops", {
  sim <- toyPrepRun(toyObjects(), times = list(start = 2002, end = 2002))
  expect_identical(max(sim$cohortData$B), 2000L)
  objs <- yearLists()
  objs$standAgeMaps <- NULL
  expect_identical(as.vector(terra::values(hindcast(objs)$standAgeMap)), rep(50, 16))
  ## a list not named by year (toyObjects' one-element rstLCCs) is not a dated dataset either
  objs$rstLCCs <- list(toyLCC())
  expect_identical(terra::values(hindcast(objs)$rstLCC_RTM), terra::values(toyLCC()))
  expect_error(hindcast(start = 1999, end = 1999.001), "cohortDatas.*before 1999")
  ## nor does a hindcast of the first data year: no vegetation before it
  expect_error(hindcast(start = 2001, end = 2001.001), "cohortDatas.*before 2001")
})

test_that("a cached init restores the vegetation it took from the per-year lists", {
  ## `init` cached: a cache hit restores only the objects declared `createsOutput`, so every object
  ## `Init` sets must be one, else it reverts to what was supplied.
  cachePath <- withr::local_tempdir()
  runInit <- function() {
    sim <- SpaDES.core::simInit(
      times = list(start = 2003, end = 2003, timeunit = "year"),
      modules = moduleName,
      params = stats::setNames(list(list(igAggFactor = 2, .useCache = "init")), moduleName),
      objects = yearLists(),
      paths = utils::modifyList(testPaths, list(cachePath = cachePath)))
    SpaDES.core::spades(sim, debug = FALSE)
  }
  first <- runInit()
  second <- runInit()
  expect_true(any(grepl("init", reproducible::showCache(cachePath)$tagValue)))
  for (sim in list(first, second)) {
    expect_identical(max(sim$cohortData$B), 5000L)
    expect_identical(as.vector(terra::values(sim$flammableRTM)), c(rep(1, 14), 0, 0))
    expect_identical(as.vector(terra::values(sim$nonForest_timeSinceDisturbance))[9], 3)
    expect_identical(sim$landcoverDT, yearLists()$landcoverDTs$year2002)
  }
  expect_false(is.null(second$pixelGroupMap))
  expect_identical(terra::values(second$pixelGroupMap), terra::values(yearLists()$pixelGroupMaps$year2002))
})
