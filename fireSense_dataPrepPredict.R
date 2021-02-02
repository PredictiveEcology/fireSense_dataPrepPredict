## Everything in this file gets sourced during `simInit()`,
## and all functions and objects are put into the `simList`.
## To use objects, use `sim$xxx` (they are globally available to all modules).
## Functions can be used without `sim$` as they are namespaced to the module,
## just like functions in R packages.
## If exact location is required, functions will be: `sim$<moduleName>$FunctionName`.
defineModule(sim, list(
  name = "fireSense_dataPrepPredict",
  description = "",
  keywords = "",
  authors = structure(list(list(given = c("First", "Middle"), family = "Last", role = c("aut", "cre"), email = "email@example.com", comment = NULL)), class = "person"),
  childModules = character(0),
  version = list(SpaDES.core = "1.0.4.9003", fireSense_dataPrepPredict = "0.0.0.9000"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = deparse(list("README.txt", "fireSense_dataPrepPredict.Rmd")),
  reqdPkgs = list("data.table", "PredictiveEcology/fireSenseUtils@development (>=0.0.4.9007)", "raster"),
  parameters = rbind(
    defineParameter(name = "fireTimeStep", "numeric", 1, NA, NA, )
    defineParameter(name = "whichModulesToPrepare", class = "character",
                    default = c("fireSense_SpreadPredict", "fireSense_IgnitionPredict", "fireSense_EscapeFit"),
                    NA, NA, desc = "Which fireSense fit modules to prep? defaults to all 3")
    defineParameter(".plotInitialTime", "numeric", NA, NA, NA,
                    "Describes the simulation time at which the first plot event should occur."),
    defineParameter(".plotInterval", "numeric", NA, NA, NA,
                    "Describes the simulation time interval between plot events."),
    defineParameter(".saveInitialTime", "numeric", NA, NA, NA,
                    "Describes the simulation time at which the first save event should occur."),
    defineParameter(".saveInterval", "numeric", NA, NA, NA,
                    "This describes the simulation time interval between save events."),
    defineParameter(".useCache", "logical", FALSE, NA, NA,
                    paste("Should this entire module be run with caching activated?",
                          "This is generally intended for data-type modules, where stochasticity",
                          "and time are not relevant"))
  ),
  inputObjects = bindrows(
    #expectsInput("objectName", "objectClass", "input object description", sourceURL, ...),
    expectsInput(objectName = "climateComponentsToUse", objectClass = "character",
                 desc = "names of the climate components to use in ignition, escape, and spread models"),
    expectsInput(objectName = "cohortData", objectClass = "data.table",
                 desc = "table that defines the cohorts by pixelGroup"),
    expectsInput(objectName = "covMinMax", objectClass = "data.table",
                 desc = "range used to rescale coefficients during spreadFit"),
    expectsInput(objectName = 'nonForest_timeSinceDisturbance', objectClass = 'RasterLayer',
                 desc = 'time since burn for non-forested pixels')
    expectsInput(objectName = "PCAclimate", objectClass = "prcomp",
                  desc = "PCA model for climate covariates, needed for fireSensePredict"),
    expectsInput(objectName = "PCAveg", objectClass = "prcomp",
                  desc = "PCA model for veg and LCC covariates, needed for FS models"),
    expectsInput(objectName = 'pixelGroupMap', objectClass = "RasterLayer",
                 'RasterLayer that defines the pixelGroups for cohortData table'),
    expectsInput(objectName = "projectedClimateRasters", objectClass = "list",
                 desc = paste("list of projected climate variables in raster stack form",
                              "named according to variable, with names of individual raster layers",
                              "following the convention 'year<year>'")),
    expectsInput(objectName = "landcoverDT", objectClass = "data.table",
                 desc = "data.table with pixelID and relevant landcover classes")
    expectsInput(objectName = "terrainDT", objectClass = "data.table",
                 desc = "data.table with pixelID and relevant terrain variables"),
    expectsInput(objectName = "vegComponentsToUse", objectClass = "character",
                 desc = "names of the veg components to use in ignition, escape, and spread predict models")
  ),
  outputObjects = bindrows(
    #createsOutput("objectName", "objectClass", "output object description", ...),
    createsOutput(objectName = 'fireSense_EscapePredictCovariates', objectClass = 'data.table',
                  desc = NA),
    createsOutput(objectName = 'fireSense_IgnitionPredictCovariates', objectClass = 'data.table',
                  desc = NA),
    createsOutput(objectName = 'fireSense_SpreadPredictCovariates', objectClass = 'data.table',
                  desc = NA),
    createsOutput(objectName = 'nonForest_timeSinceDisturbance', objectClass = 'RasterLayer',
                  desc = 'time since burn for non-forest pixels')
  )
))

## event types
#   - type `init` is required for initialization

doEvent.fireSense_dataPrepPredict = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {
      ### check for more detailed object dependencies:
      ### (use `checkObject` or similar)

      # do stuff for this event
      sim <- Init(sim)
      sim <- scheduleEvent(sim, time(sim) + 1, "fireSense_dataPrepPredict", "ageNonForest")
      if ("fireSense_IgnitionPredict" %in% P(sim)$whichModulesToPrepare)
        sim <- scheduleEvent(sim, start(sim), "fireSense_dataPrepPredict", "prepIgnitionPredictData")
      if ("fireSense_EscapePredict" %in% P(sim)$whichModulesToPrepare)
        sim <- scheduleEvent(sim, start(sim), "fireSense_dataPrepPredict", "prepEscapePredictData")
      if ("fireSense_SpreadPredict" %in% P(sim)$whichModulesToPrepare)
        sim <- scheduleEvent(sim, start(sim), "fireSense_dataPrepPredict", "prepSpreadPredictData")
      # schedule future event(s)
      sim <- scheduleEvent(sim, P(sim)$.plotInitialTime, "fireSense_dataPrepPredict", "plot")
      sim <- scheduleEvent(sim, P(sim)$.saveInitialTime, "fireSense_dataPrepPredict", "save")
    },
    plot = {

    },
    save = {

    },
    ageNonForest = {
      sim$ageNonForest <- ageNonForestPixels(sim)
    }

    prepIgnitionPredictData = {
      sim <- prepare_IgnitionPredict(sim)
      sim <- scheduleEvent(sim, time(sim) + P(sim)$fireTimeStep,
                           "fireSense_dataPrepPredict", "prepIgnitionPredictData")
    },
    prepEscapePredictData = {
      sim <- prepare_EscapePredict(sim)
      sim <- scheduleEvent(sim, time(sim) + P(sim)$fireTimeStep,
                           "fireSense_dataPrepPredict", "prepEscapePredictData")
    },
    prepSpreadPredictData = {
      sim <- prepare_SpreadPredict(sim) #see what is specific, maybe don't pass sim
      sim <- scheduleEvent(sim, time(sim) + P(sim)$fireTimeStep,
                           "fireSense_dataPrepPredict", "prepSpreadPredictData")
    },
    warning(paste("Undefined event type: \'", current(sim)[1, "eventType", with = FALSE],
                  "\' in module \'", current(sim)[1, "moduleName", with = FALSE], "\'", sep = ""))
  )
  return(invisible(sim))
}

## event functions
#   - keep event functions short and clean, modularize by calling subroutines from section below.

### template initialization
Init <- function(sim) {
  # # ! ----- EDIT BELOW ----- ! #
 #what has to happen in the init? everthing should be dynamic, so... likeyl nothing

  # ! ----- STOP EDITING ----- ! #

  return(invisible(sim))
}

### template for save events
Save <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # do stuff for this event
  sim <- saveFiles(sim)

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

### template for plot events
plotFun <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # do stuff for this event
  #Plot(sim$object)

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

### template for your event1
prepare_spreadPredict <- function(sim) {
  browser()
  #cohortData, pixelGroupMap, nonforest_standAge, terrainDT, landcoverDT, PCA....
  #1) build fireSense vegData from cohortData + landcoverDT
  terrainAndLCC <- sim$landcoverDT[sim$terrainDT, on = "pixelIndex"]



  return(invisible(sim))
}

### template for your event2
Event2 <- function(sim) {


  return(invisible(sim))
}

.inputObjects <- function(sim) {

  cacheTags <- c(currentModule(sim), "function:.inputObjects")
  dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)
  message(currentModule(sim), ": using dataPath '", dPath, "'.")

  if (!suppliedElsewhere("landcoverDT", sim)) {
    stop("Please supply this object by running fireSense_dataPrepFit")
  }

  if (!suppliedElsewhere("nonForest_timeSinceDisturbance", sim)) {
    stop("Please supply this object by running fireSense_dataPrepFit")
    #It is a lot of work to supply some of these, fraught with assumptions
    #initial TSD will be derived from 1995-2010 fires
    # firePolys <- Cache(fireSenseUtils::getFirePolygons,
    #                    fireSenseUtils, years = 1995:2010,
    #                    studyArea = sim$studyArea,
    #                    destinationPath = dPath,
    #                    useInnerCache = FALSE,
    #                    usertags = c("firePolys", cacheTags))
    # standAgeMap <- setValues(sim$rasterToMatch, 16) #defaults to not young
    # #this is a complicated object to create from scratch. Requires nonforest lcc, lcc raster, firePolys...
    # sim$nonForest_timeSinceDisturbance <- makeTSD(year = 2011, firePolys = firePolys,
    #                                               standAgeMap = standAgeMap,
    #                                               lcc = sim$landcoverDT)
  }

  return(invisible(sim))
}

### add additional events as needed by copy/pasting from above
