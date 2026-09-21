## A 4 x 4 toy landscape on which every covariate can be worked out by hand.
## A setup file rather than a helper, so that it shares an environment with `moduleName`
## and `testPaths` from setup.R.
##
## Tests run inside the namespace of the package rendition; tables are handled with base
## subsetting and data.table::set() only, so nothing depends on data.table-awareness there.
##
## The module calls postProcess() and Cache() unqualified but does not list `reproducible`
## in `reqdPkgs`; in a project another module attaches it. It is always installed
## (SpaDES.core imports it), so attach it here.
suppressPackageStartupMessages(library(reproducible))

## Cell numbers (row-major) and what is in them:
##
##    1  2  3  4      PG1 PG1 PG2 PG2      pixel groups 1-4 are forest with cohorts
##    5  6  7  8      PG3 PG3 PG4 PG4
##    9 10 11 12      wet wet grs grs      non-forest: wetland, grass
##   13 14 15 16      --- --- wet  X       13, 14: forest land cover but no cohorts
##                                         15: wetland burned 5 y ago; 16: not flammable
##
## PG1: Pice_mar 80 y B 2000 + Pinu_ban 40 y B 1000 -> class1 B = 3000
## PG2: Popu_tre 50 y B 500                         -> class2 B = 500
## PG3: Pice_mar 10 y B 300 (max age 10 <= cutoff 15) -> youngAge
## PG4: Pice_mar 60 y B 100 + Popu_tre 60 y B 50    -> class1 B = 100, class2 B = 50
toyRast <- function(vals) {
  terra::rast(nrows = 4, ncols = 4, xmin = 0, xmax = 4, ymin = 0, ymax = 4, vals = vals,
              crs = "EPSG:3005")
}

toyLCC <- function() toyRast(c(rep(210, 8), 19, 19, 16, 16, 210, 210, 19, 20))

toyCohortData <- function() {
  data.table::data.table(
    pixelGroup  = c(1L, 1L, 2L, 3L, 4L, 4L),
    speciesCode = factor(c("Pice_mar", "Pinu_ban", "Popu_tre", "Pice_mar", "Pice_mar", "Popu_tre")),
    age         = c(80L, 40L, 50L, 10L, 60L, 60L),
    B           = c(2000L, 1000L, 500L, 300L, 100L, 50L)
  )
}

toySppEquiv <- function() {
  data.table::data.table(LandR = c("Pice_mar", "Pinu_ban", "Popu_tre"),
                         FuelClass = c("class1", "class1", "class2"))
}

toyLandcoverDT <- function() {
  data.table::data.table(pixelID = 1:15,
                         wetland = as.integer(1:15 %in% c(9, 10, 15)),
                         grass   = as.integer(1:15 %in% c(11, 12)))
}

## climate: layer `year<Y>` of MDC is (Y - 2000) * 100 + cell number, so both the year that
## was taken and the cell it came from can be read off any value; `Tmax` is its negative
toyClimate <- function(years = 2001:2003) {
  mk <- function(sign) {
    r <- terra::rast(lapply(years, function(y) toyRast(sign * ((y - 2000) * 100 + 1:16))))
    names(r) <- paste0("year", years)
    r
  }
  list(MDC = mk(1), Tmax = mk(-1))
}

toyObjects <- function() {
  list(
    rasterToMatch = toyRast(1),
    studyArea = terra::as.polygons(terra::ext(toyRast(1)), crs = "EPSG:3005"),
    flammableRTM = toyRast(c(rep(1, 15), 0)),
    pixelGroupMap = toyRast(c(1, 1, 2, 2, 3, 3, 4, 4, rep(NA, 8))),
    cohortData = toyCohortData(),
    sppEquiv = toySppEquiv(),
    landcoverDT = toyLandcoverDT(),
    nonForestedLCCGroups = list(wetland = 19L, grass = 16L),
    missingLCCgroup = "grass",
    ## years since fire: 30 everywhere except the wetland cell 15 and the forest cell 1
    nonForest_timeSinceDisturbance = toyRast(c(5, rep(30, 13), 5, 30)),
    standAgeMap = toyRast(50),
    ## supplying `rstLCCs` takes the branch of `.inputObjects` that does not download NTEMS
    ## landcover; `rstLCC_RTM` is then its last element
    rstLCCs = list(toyLCC()),
    projectedClimateRasters = toyClimate(),
    climateVariablesForFire = list(ignition = "MDC", spread = "MDC"),
    lightningMaps = {
      r <- c(toyRast(1:16), toyRast(1001:1016))
      names(r) <- c("lightningDays", "lightningDensity")
      r
    }
  )
}

toyPrepSim <- function(objects = toyObjects(), params = list(),
                       times = list(start = 2001, end = 2001)) {
  params <- utils::modifyList(list(igAggFactor = 2), params)
  sim <- SpaDES.core::simInit(
    times = c(times, timeunit = "year"),
    modules = moduleName,
    params = stats::setNames(list(params), moduleName),
    objects = objects,
    paths = testPaths
  )
  sim
}

toyPrepRun <- function(...) SpaDES.core::spades(toyPrepSim(...), debug = FALSE)

## a covariate table as a data.frame ordered by pixelID
covDF <- function(dt) {
  df <- as.data.frame(dt)
  df[order(df$pixelID), , drop = FALSE]
}

evOf <- function(dt, type) {
  df <- as.data.frame(dt)
  df[df$moduleName == "fireSense_dataPrepPredict" & df$eventType == type, , drop = FALSE]
}
