## A replay job (`replayFrom`) takes the spread-prediction inputs that do not depend on the spread parameters
## from a reference hindcast job (`replaySave = TRUE`), in place of building them.

## a ledger-shaped row: list columns (a params table, a species table), as readSpreadFitLedger() returns
ledgerRow <- function() {
  poly <- terra::as.polygons(terra::ext(0, 500, 0, 500), crs = toyCRSp)
  sfRow <- sf::st_as_sf(poly)
  sfRow$polygonID <- "11.3"
  sfRow$spreadLink <- "powerLawSmooth"
  sfRow$powerLawGamma_spread <- 2.79
  sfRow$params <- list(data.frame(maxAsymptote = c(0.25, 0.26), `(Intercept)` = c(7.9, 8.8), CMD = c(21.9, 18.3),
                                  dom_agb_Pice_mar = c(71, 37), check.names = FALSE))
  sfRow$sppEquiv <- list(data.table::data.table(LandR = c("Pice_mar", "Popu_tre"), FuelClass = c("class1", "class2")))
  sfRow$covMinMax_spread <- list(data.table::data.table(CMD = c(1, 5), youngAge = c(0, 40)))
  sfRow
}

replayObjs <- function() {
  pts <- terra::vect(cbind(x = c(100, 600), y = c(200, 900)), type = "points", crs = toyCRSp)
  pts$escaped <- c(TRUE, FALSE)
  poly <- terra::as.polygons(terra::ext(0, 500, 0, 500), crs = toyCRSp)
  poly$YEAR <- 2000L
  elf <- toyRastP(rep(c(1L, 2L), 8), "ELF")
  levels(elf) <- data.frame(value = 1:2, ELF = c("11.3", "11.4"))
  list(
    flammableRTM = toyRastP(c(rep(1, 15), 0), "flammable"),
    rasterToMatch = toyRastP(rep(1L, 16), "rtm"),
    rasterToMatchELF = toyRastP(rep(c(1L, 2L), 8), "ELF"),
    rasterToMatchLargeELF = elf,
    fireSense_SpreadCovariates = data.table::data.table(pixelID = 1:3, MDC = c(1, 2, 3), youngAge = c(0, 1, 0)),
    studyAreaWithSpreadParams = ledgerRow(),
    nonEscapedFireSizesHa = c(1.5, 20, 300),
    ignitionFirePoints = pts,
    firePolys = list(year1999 = poly, year2000 = poly)
  )
}

## SpatRaster/SpatVector are pointers: compare what they hold
asData <- function(x) {
  if (inherits(x, "SpatRaster")) list(names(x), terra::values(x, mat = FALSE), as.vector(terra::ext(x)), terra::crs(x),
                                      terra::levels(x))
  else if (inherits(x, "SpatVector")) list(as.data.frame(x), terra::geom(x), terra::crs(x))
  else if (inherits(x, "sf")) list(lapply(as.list(as.data.frame(x)), asData), sf::st_crs(x))
  else if (is.data.frame(x)) lapply(as.list(x), asData) # a data.table's internal pointer is not content
  else if (is.list(x)) lapply(x, asData)
  else x
}

test_that("this module declares no output that other modules make", {
  ## an output makes SpaDES.core's suppliedElsewhere() report the object supplied for every module in a run,
  ## which stops the module that would build it from doing so
  md <- SpaDES.core::moduleMetadata(module = moduleName, path = modulePath)
  expect_length(intersect(md$outputObjects$objectName,
                          c("rasterToMatch", "rasterToMatchELF", "rasterToMatchLargeELF", "nonEscapedFireSizesHa",
                            "ignitionFirePoints", "firePolys")), 0L)
})

test_that("without replaySave nothing is written, and the parameters default to no replay", {
  ref <- toyPrepRun(toyObjects(), params = list(whichModulesToPrepare = "fireSense_spreadPredict"))
  expect_length(list.files(SpaDES.core::outputPath(ref), pattern = "^replay_"), 0L)
  md <- SpaDES.core::moduleMetadata(module = moduleName, path = modulePath)
  expect_true(is.na(md$parameters[md$parameters$paramName == "replayFrom", ]$default[[1]]))
  expect_false(md$parameters[md$parameters$paramName == "replaySave", ]$default[[1]])
})

## a reference job, then its save event over the full set of objects, written to a fresh folder
referenceJob <- function(objs = replayObjs()) {
  ref <- toyPrepRun(toyObjects(), params = list(whichModulesToPrepare = "fireSense_spreadPredict", replaySave = TRUE))
  for (nm in names(objs)) ref[[nm]] <- objs[[nm]]
  SpaDES.core::outputPath(ref) <- file.path(withr::local_tempdir(.local_envir = parent.frame()), "out")
  ref <- SpaDES.core::scheduleEvent(ref, SpaDES.core::start(ref), moduleName, "save")
  SpaDES.core::spades(ref, events = c("save", "later", "replayUnstack"), debug = FALSE)
}

test_that("saved replay inputs load back identical, through outputs() and inputs()", {
  objs <- replayObjs()
  ref <- referenceJob(objs)
  path <- SpaDES.core::outputPath(ref)
  expect_length(list.files(path, "^replay_flammableRTM_year[0-9]+\\.tif$"), 1L)
  expect_length(list.files(path, "^replay_studyAreaWithSpreadParams_year[0-9]+\\.qs2$"), 1L)
  expect_false(exists("replay_firePolys", envir = SpaDES.core::envir(ref))) # the stacked form is not left in the sim
  rep <- toyPrepRun(list(), params = list(whichModulesToPrepare = "fireSense_spreadPredict", replayFrom = path))
  for (nm in names(objs)) expect_identical(asData(rep[[nm]]), asData(objs[[nm]]), info = nm)
  ## list columns survive (a GeoPackage turns a params table into text)
  expect_s3_class(rep$studyAreaWithSpreadParams, "sf")
  expect_identical(rep$studyAreaWithSpreadParams$params, objs$studyAreaWithSpreadParams$params)
  expect_identical(rep$studyAreaWithSpreadParams$sppEquiv[[1]]$LandR, c("Pice_mar", "Popu_tre"))
  ## categories of a labelled raster
  expect_identical(terra::levels(rep$rasterToMatchLargeELF)[[1]]$ELF, c("11.3", "11.4"))
  ## the year names of firePolys
  expect_identical(names(rep$firePolys), c("year1999", "year2000"))
})

test_that("a smaller second save leaves no stale polygon from the first, and a NULL year is dropped", {
  objs <- replayObjs()
  objs$firePolys <- list(year1999 = objs$firePolys[[1]], year2000 = NULL, year2001 = objs$firePolys[[2]])
  ref <- referenceJob(objs)
  rep <- toyPrepRun(list(), params = list(whichModulesToPrepare = "fireSense_spreadPredict",
                                          replayFrom = SpaDES.core::outputPath(ref)))
  expect_identical(names(rep$firePolys), c("year1999", "year2001"))
  objs2 <- replayObjs()
  objs2$firePolys <- objs2$firePolys[1]
  ## save again to the same folder: the earlier, longer set is not picked up
  ref <- SpaDES.core::scheduleEvent(ref, SpaDES.core::start(ref), moduleName, "save")
  ref$firePolys <- objs2$firePolys
  ref <- SpaDES.core::spades(ref, events = c("save", "later", "replayUnstack"), debug = FALSE)
  rep <- toyPrepRun(list(), params = list(whichModulesToPrepare = "fireSense_spreadPredict",
                                          replayFrom = SpaDES.core::outputPath(ref)))
  expect_identical(names(rep$firePolys), "year1999")
})

test_that("a terra object or an unnamed firePolys list is refused, not written as a table", {
  bad <- replayObjs()
  bad$nonEscapedFireSizesHa <- bad$flammableRTM # a terra object where a table is expected
  expect_error(referenceJob(bad), "cannot be saved")
  bad <- replayObjs()
  bad$firePolys <- unname(bad$firePolys)
  expect_error(referenceJob(bad), "named list")
})

test_that("a replay job takes the saved inputs and builds none", {
  ref <- toyPrepRun(utils::modifyList(toyObjects(), replayObjs()[c("rasterToMatchELF", "rasterToMatchLargeELF",
                                                                    "nonEscapedFireSizesHa", "ignitionFirePoints", "firePolys")]),
                    params = list(whichModulesToPrepare = "fireSense_spreadPredict", replaySave = TRUE))
  path <- SpaDES.core::outputPath(ref)
  expect_length(list.files(path, "^replay_fireSense_SpreadCovariates_year[0-9]+\\.qs2$"), 1L)
  n <- 0L
  orig <- fireSenseUtils::cohortsToFuelClasses
  testthat::local_mocked_bindings(cohortsToFuelClasses = function(...) { n <<- n + 1L; orig(...) },
                                  .package = "fireSenseUtils")
  rep <- toyPrepRun(list(), params = list(whichModulesToPrepare = "fireSense_spreadPredict", replayFrom = path))
  expect_identical(n, 0L)
  expect_identical(covDF(rep$fireSense_SpreadCovariates), covDF(ref$fireSense_SpreadCovariates))
  for (nm in c("flammableRTM", "rasterToMatch", "rasterToMatchELF", "rasterToMatchLargeELF", "nonEscapedFireSizesHa",
               "ignitionFirePoints", "firePolys"))
    expect_identical(asData(rep[[nm]]), asData(ref[[nm]]), info = nm)
  expect_false(any(c("getClimateRasters", "prepSpreadPredictData") %in% SpaDES.core::completed(rep)$eventType))
})

test_that("an object missing from a later save is not replayed from an earlier one", {
  ref <- referenceJob(replayObjs())
  expect_false(is.null(toyPrepRun(list(), params = list(whichModulesToPrepare = "fireSense_spreadPredict",
                                                         replayFrom = SpaDES.core::outputPath(ref)))$firePolys))
  ref <- SpaDES.core::scheduleEvent(ref, SpaDES.core::start(ref), moduleName, "save")
  rm("firePolys", envir = SpaDES.core::envir(ref))
  ref <- SpaDES.core::spades(ref, events = c("save", "later", "replayUnstack"), debug = FALSE)
  rep <- toyPrepRun(list(), params = list(whichModulesToPrepare = "fireSense_spreadPredict",
                                          replayFrom = SpaDES.core::outputPath(ref)))
  expect_null(rep$firePolys)
  expect_length(list.files(SpaDES.core::outputPath(ref), pattern = "firePolys"), 0L)
})
