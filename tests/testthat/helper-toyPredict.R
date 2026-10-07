## A tiny in-memory landscape for the event-level tests. Nothing is downloaded: every input the
## module's `.inputObjects()` would otherwise build is supplied, so the guards there short-circuit.
##
## 4 x 4 pixels of 250 m. Landcover is categorical (210 forest, 20 water, 50 herb), which is the
## point of the `method = "near"` test. Climate is one MDC stack whose `year<yyyy>` layer is
## constant at (yyyy - 1900), so a wrong year is visible in the values.

toyCRSp <- "EPSG:3978"

## setup.R's `moduleName`/`testPaths` are not in scope for helpers, so resolve them here.
toyModuleName <- "fireSense_dataPrepPredict"

toyRastP <- function(vals, nam = "layer", n = 4) {
  r <- terra::rast(nrows = n, ncols = n, xmin = 0, xmax = 1000, ymin = 0, ymax = 1000,
                   crs = toyCRSp)
  r <- terra::setValues(r, vals)
  names(r) <- nam
  r
}

toyLCCvalsP <- function() rep(c(210L, 210L, 50L, 20L), times = 4)

## MDC stack: layer "year2001" is 101 everywhere, "year2002" is 102, ...
toyClimateP <- function(years = 2001:2005) {
  lyrs <- lapply(years, function(y) toyRastP(rep(y - 1900, 16), paste0("year", y)))
  list(MDC = terra::rast(lyrs))
}

toyInputsP <- function(...) {
  rtm <- toyRastP(rep(1L, 16), "rtm")
  lcc <- toyRastP(toyLCCvalsP(), "lcc")
  out <- list(
    climateVariablesForFire = list(ignition = "MDC", spread = "MDC"),
    projectedClimateRasters = toyClimateP(),
    pixelGroupMap = toyRastP(rep(1L, 16), "pixelGroup"),
    rasterToMatch = rtm,
    rstLCC_RTM = lcc,
    ## supplying rstLCCs takes the `suppliedElsewhere("rstLCCs", ...)` branch of
    ## .inputObjects(), so nothing is downloaded
    rstLCCs = list(year2001 = lcc),
    flammableRTM = toyRastP(as.integer(toyLCCvalsP() != 20L), "flammable"),
    standAgeMap = toyRastP(rep(80L, 16), "standAge"),
    nonForest_timeSinceDisturbance = toyRastP(rep(30L, 16), "TSD"),
    nonForestedLCCGroups = list(herb = 50L),
    sppEquiv = data.table::data.table(LandR = "Pice_mar", FuelClass = "BlkSprc"),
    cohortData = data.table::data.table(pixelGroup = 1L,
                                        speciesCode = factor("Pice_mar"),
                                        age = 80L, B = 3000L)
  )
  utils::modifyList(out, list(...))
}

toyParamsP <- function(...) {
  utils::modifyList(list(whichModulesToPrepare = character(0),
                         dataYear = 2001,
                         .useCache = FALSE), list(...))
}

toyPathsP <- function() {
  root <- withr::local_tempdir(.local_envir = parent.frame())
  mp <- dirname(normalizePath(file.path("..", ".."), winslash = "/", mustWork = TRUE))
  list(cachePath = file.path(root, "cache"), inputPath = file.path(root, "inputs"),
       modulePath = mp, outputPath = file.path(root, "outputs"))
}

toySimInitP <- function(objects = toyInputsP(), params = toyParamsP(),
                        start = 2001, end = 2003) {
  withr::local_options(spades.useRequire = FALSE, spades.moduleCodeChecks = FALSE,
                       reproducible.verbose = -2)
  suppressMessages(SpaDES.core::simInit(
    times = list(start = start, end = end), modules = toyModuleName,
    params = stats::setNames(list(params), toyModuleName),
    objects = objects, paths = toyPathsP()))
}

## this module's mod$ objects in a simList
toyModP <- function(sim) sim[[".modObjs"]][[toyModuleName]]

## The module's own functions. Under `convertToPackage()` they live in the module's namespace;
## reaching them through the simList works either way.
toyFunP <- function(sim, nm) get(nm, envir = sim[[".mods"]][[toyModuleName]])

## Run named events of this module through spades(), which is what gives the module's functions
## their `mod` and `P()` context. Messages are muffled.
toyRunEventsP <- function(sim, events) {
  withr::local_options(spades.useRequire = FALSE, reproducible.verbose = -2)
  suppressMessages(suppressWarnings(
    SpaDES.core::spades(sim, events = stats::setNames(list(events), toyModuleName), debug = FALSE)))
}
