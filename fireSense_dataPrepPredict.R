defineModule(sim, list(
  name = "fireSense_dataPrepPredict",
  description = "",
  keywords = "",
  authors = c(
    person("Ian", "Eddy", role = c("aut", "cre"), email = "ian.eddy@nrcan-rncan.gc.ca"),
    person("Eliot", "McIntire", role = "aut", email = "eliot.mcintire@nrcan-rncan.gc.ca"),
    person("Alex M", "Chubaty", role = "ctb", email = "achubaty@for-cast.ca")
  ),
  childModules = character(0),
  version = list(fireSense_dataPrepPredict = "1.0.0"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = deparse(list("README.txt", "fireSense_dataPrepPredict.Rmd")),
  loadOrder = list(after = "Biomass_borealDataPrep", "fireSense_dataPrepFit"),
  reqdPkgs = list("data.table",
                  "PredictiveEcology/fireSenseUtils@development (>= 0.0.5.9050)",
                  "terra"),
  parameters = rbind(
    defineParameter("cutoffForYoungAge", "numeric", 15, NA, NA,
                    desc = paste("Age at and below which pixels are considered 'young'",
                                 "(i.e., `age <= cutoffForYoungAge`).")),
    defineParameter("fireTimeStep", "numeric", 1, NA, NA, desc = "time step of fire model"),
    defineParameter("forestedLCC", "numeric", 1:6, NA, NA,
                    desc = "forested landcover classes in `rstLCC` - only relevant if `landcoverDT` is not supplied"),
    defineParameter("ignitionFuelClassCol", "character", "FuelClass", NA, NA,
                    desc = "the column in sppEquiv that defines unique fuel classes for ignition"),
    defineParameter("missingLCCgroup", "character", "nonForest_highFlam", NA, NA,
                    desc = paste("if a pixel is forested but is absent from `cohortData`,",
                                 "it will be grouped in this class.",
                                 "Must be one of the names in `sim$nonForestedLCCGroups`.")),
    defineParameter("nonForestCanBeYoungAge", "logical", TRUE, NA, NA,
                    desc = "update non-forest when burned, to become youngAge"),
    defineParameter("sppEquivCol", "character", "LandR", NA, NA,
                    desc = "column name in `sppEquiv` object that defines unique species in `cohortData`"),
    defineParameter("spreadFuelClassCol", "character", "FuelClass", NA, NA,
                    desc = "if using fuel classes for spread, the column in sppEquiv that defines unique fuel classes"),
    defineParameter("whichModulesToPrepare", "character",
                    default = c("fireSense_SpreadPredict", "fireSense_IgnitionPredict", "fireSense_EscapeFit"),
                    NA, NA,
                    desc = "Which fireSense fit modules to prep? defaults to all 3"),
    defineParameter(".plotInitialTime", "numeric", NA, NA, NA,
                    "Describes the simulation time at which the first plot event should occur."),
    defineParameter(".plotInterval", "numeric", NA, NA, NA,
                    "Describes the simulation time interval between plot events."),
    defineParameter(".runInitialTime", "numeric", start(sim), NA, NA,
                    "time to simulate initial fire"),
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
    expectsInput("cohortData", "data.table", sourceURL = NA,
                 desc = "table that defines the cohorts by pixelGroup"),
    expectsInput("flammableRTM", "SpatRaster", sourceURL = NA,
                 desc = "RTM without ice/rocks/urban/water. Flammable map with 0 and 1."),
    expectsInput("nonForestedLCCGroups", "list", sourceURL = NA,
                 desc = paste("a named list of non-forested landcover groups,",
                              "e.g. list('wetland' = c(19, 23, 32)).",
                              "This is only relevant if landcoverDT is not supplied")),
    expectsInput("nonForest_timeSinceDisturbance", "SpatRaster", sourceURL = NA,
                 desc = "time since burn for non-forested pixels"),
    expectsInput("pixelGroupMap", "SpatRaster", sourceURL = NA,
                 desc = "SpatRaster that defines the pixelGroups for cohortData table"),
    expectsInput("projectedClimateRasters", "list", sourceURL = NA,
                 desc = paste("list of projected climate variables in raster stack form",
                              "named according to variable, with names of individual raster layers",
                              "following the convention 'year<year>'")),
    expectsInput("landcoverDT", "data.table", sourceURL = NA,
                 desc = "data.table with pixelID and relevant landcover classes"),
    expectsInput("rstCurrentBurn", "SpatRaster", sourceURL = NA,
                 desc = "binary raster with 1 representing annual burn"),
    expectsInput("rstLCC", "SpatRaster", sourceURL = NA,
                 desc = "a landcover raster - only used if landcoverDT is unsupplied"),
    expectsInput("sppEquiv", "data.table", sourceURL = NA,
                 desc = "table of LandR species equivalencies")
  ),
  outputObjects = bindrows(
    createsOutput("currentClimateRasters", "list",
                  desc = "list of project climate rasters at current time of sim"),
    createsOutput("fireSense_IgnitionAndEscapeCovariates", "data.table",
                  desc = paste("data.table of covariates for ignition prediction, with pixelID column",
                                "corresponding to flammableRTM pixel index")),
    createsOutput("fireSense_SpreadCovariates", "data.table",
                  desc = paste("data.table of covariates for spread prediction, with pixelID column",
                                "corresponding to flammableRTM pixel index")),
    createsOutput("nonForest_timeSinceDisturbance", "SpatRaster",
                  desc = "time since burn for non-forest pixels")
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
      sim <- scheduleEvent(sim, P(sim)$.runInitialTime, "fireSense_dataPrepPredict", "getClimateRasters")

      if ("fireSense_IgnitionPredict" %in% P(sim)$whichModulesToPrepare |
          "fireSense_EscapePredict" %in% P(sim)$whichModulesToPrepare) {
        sim <- scheduleEvent(sim, P(sim)$.runInitialTime, "fireSense_dataPrepPredict",
                             "prepIgnitionAndEscapePredictData",
                             eventPriority = 5.10)
      }

      if ("fireSense_SpreadPredict" %in% P(sim)$whichModulesToPrepare) {
        sim <- scheduleEvent(sim, P(sim)$.runInitialTime, "fireSense_dataPrepPredict", "prepSpreadPredictData",
                             eventPriority = 5.10)
      }
      # schedule future event(s)
      # sim <- scheduleEvent(sim, P(sim)$.plotInitialTime, "fireSense_dataPrepPredict", "plot", eventPriority = 5.12)
    },
    ageNonForest = {
      sim$nonForest_timeSinceDisturbance <- ageNonForest(TSD = sim$nonForest_timeSinceDisturbance,
                                                         rstCurrentBurn = sim$rstCurrentBurn,
                                                         timeStep = P(sim)$fireTimeStep)
      sim <- scheduleEvent(sim, time(sim) + P(sim)$fireTimeStep,
                           "fireSense_dataPrepPredict", "ageNonForest")
    },
    getClimateRasters = {
      sim$currentClimateRasters <- getCurrentClimate(sim$projectedClimateRasters,
                                                    time(sim),
                                                    rasterToMatch = sim$flammableRTM)
      sim <- scheduleEvent(sim, time(sim) + P(sim)$fireTimeStep,
                           "fireSense_dataPrepPredict", "getClimateRasters")

    },
    prepIgnitionAndEscapePredictData = {
      sim <- prepare_IgnitionAndEscapePredict(sim)
      sim <- scheduleEvent(sim, time(sim) + P(sim)$fireTimeStep,
                           "fireSense_dataPrepPredict", "prepIgnitionAndEscapePredictData",
                           eventPriority = 5.1)
    },
    prepSpreadPredictData = {
      sim <- prepare_SpreadPredict(sim)
      sim <- scheduleEvent(sim, time(sim) + P(sim)$fireTimeStep,
                           "fireSense_dataPrepPredict", "prepSpreadPredictData",
                           eventPriority = 5.1)
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

   if (!compareGeom(sim$pixelGroupMap, sim$projectedClimateRasters[[1]], stopOnError = FALSE)) {
     stop("mismatch in resolution detected - please review the resolution of sim$projectedClimateRasters")
   }

  return(invisible(sim))
}

### template for plot events

getCurrentClimate <- function(projectedClimateRasters, time, rasterToMatch) {

  availableYears <- as.numeric(gsub(pattern = "year",
                               x = names(projectedClimateRasters[[1]]),
                               replacement = ""))
  if (time > max(availableYears)) {
    cutoff <- quantile(availableYears, probs = 0.9)
    time <- sample(availableYears[availableYears >= cutoff], size = 1)
    message(paste0("re-using projected climate layers from ", time))
  }
  ## this will work with a list of raster stacks
  thisYearsClimate <- lapply(projectedClimateRasters, FUN = function(x, rtm = rasterToMatch, TIME = time) {
    ras <- x[[paste0("year", TIME)]]
    if (!compareGeom(ras, rtm, stopOnError = FALSE)) {
      message("reprojecting fireSense climate layers")
      ras <- postProcess(ras, rasterToMatch = rtm)
    }
    return(ras)
  })

  return(thisYearsClimate)
}

ageNonForest <- function(TSD, rstCurrentBurn, timeStep) {
  TSDvals <- values(TSD, mat = FALSE)
  TSDvals <- TSDvals + 1
  if (!is.null(rstCurrentBurn)) {
    burnVals <- values(rstCurrentBurn, mat = FALSE)
    unburned <- is.na(burnVals) | burnVals == 0

    TSDvals[!unburned] <- 0
  }
  TSD <- setValues(TSD, TSDvals)
  rm(TSDvals, burnVals, unburned)
  gc()
  return(TSD)
}

prepare_IgnitionAndEscapePredict <- function(sim) {
  ## get fuel classes
  fuelClasses <- cohortsToFuelClasses(cohortData = sim$cohortData,
                                      sppEquiv = sim$sppEquiv,
                                      sppEquivCol = P(sim)$sppEquivCol,
                                      pixelGroupMap = sim$pixelGroupMap,
                                      landcoverDT = sim$landcoverDT,
                                      flammableRTM = sim$flammableRTM,
                                      fuelClassCol = P(sim)$ignitionFuelClassCol,
                                      cutoffForYoungAge = P(sim)$cutoffForYoungAge)
  ## make columns for each fuel class
  fcs <- names(fuelClasses)

  getPix <- function(fc, type, index) {
    fuelVals <- values(fc[[type]], mat = FALSE)
    return(fuelVals[index])
  }
  fuelDT <- data.table(pixelID = sim$landcoverDT$pixelID)
  fuelDT[, c(fcs) := nafill(x = lapply(fcs, FUN = getPix, fc = fuelClasses, index = fuelDT$pixelID), fill = 0)]


  ignitionCovariates <- fuelDT[sim$landcoverDT, on = c("pixelID")]
  ignitionCovariates[, rowcheck := rowSums(.SD), .SD = setdiff(names(ignitionCovariates), "pixelID")]
  ## if all rows are 0, it must be a forested LCC absent from cohortData
  ignitionCovariates[rowcheck == 0, eval(P(sim)$missingLCC) := 1]
  set(ignitionCovariates, NULL, "rowcheck", NULL)

  if (P(sim)$nonForestCanBeYoungAge) {
    ignitionCovariates[, YA_NF := sim$nonForest_timeSinceDisturbance[ignitionCovariates$pixelID] <=
                         P(sim)$cutoffForYoungAge]
    ignitionCovariates[YA_NF == TRUE, youngAge := 1]
    ignitionCovariates[, YA_NF := NULL]
  }

  exclusiveCols <- c("class", names(sim$landcoverDT))
  exclusiveCols <- setdiff(exclusiveCols, "pixelID")
  ignitionCovariates <- makeMutuallyExclusive(dt = ignitionCovariates,
                                              mutuallyExclusive = list("youngAge" = exclusiveCols))
  ignitionCovariates[, clim := as.vector(sim$currentClimateRasters[[1]])[ignitionCovariates$pixelID]]
  ignitionCovariates <- ignitionCovariates[!is.na(clim)] # don't predict with no climate data
  setnames(ignitionCovariates, "clim", new = names(sim$currentClimateRasters))

  sim$fireSense_IgnitionAndEscapeCovariates <- ignitionCovariates
  gc()
  return(invisible(sim))
}

prepare_SpreadPredict <- function(sim) {

  #much of this chunk can now be combined into a function, called for both ig and spread prep
  vegData <- cohortsToFuelClasses(cohortData = sim$cohortData,
                                  pixelGroupMap = sim$pixelGroupMap,
                                  flammableRTM = sim$flammableRTM,
                                  sppEquiv = sim$sppEquiv,
                                  landcoverDT = sim$landcoverDT,
                                  fuelClassCol = P(sim)$spreadFuelClassCol,
                                  sppEquivCol = P(sim)$sppEquivCol,
                                  cutoffForYoungAge = P(sim)$cutoffForYoungAge)
  fcs <- names(vegData)
  getPix <- function(fc, type, index) { fc[[type]][index]}
  fuelDT <- data.table(pixelID = sim$landcoverDT$pixelID)
  fuelDT[, c(fcs) := nafill(lapply(fcs, getPix, fc = vegData, index = fuelDT$pixelID), fill = 0)]
  vegData <- fuelDT[sim$landcoverDT, on = c("pixelID")]
  vegData[, rowcheck := rowSums(.SD), .SD = setdiff(names(vegData), 'pixelID')]
  #if all rows are 0, it must be a forested LCC absent from cohortData
  vegData[rowcheck == 0, eval(P(sim)$missingLCC) := 1]
  set(vegData, NULL, 'rowcheck', NULL)

  if (P(sim)$nonForestCanBeYoungAge) {
    vegData[, YA_NF := sim$nonForest_timeSinceDisturbance[vegData$pixelID] <= P(sim)$cutoffForYoungAge]
    vegData[YA_NF == TRUE, youngAge := 1]
    vegData[, YA_NF := NULL]
  }
  exclusiveCols <- c("class", names(sim$landcoverDT))
  exclusiveCols <- setdiff(exclusiveCols, "pixelID")



  climVar <- names(sim$currentClimateRasters)
  vegData[, clim := values(sim$currentClimateRasters[[1]], mat = FALSE)[vegData$pixelID]]
  vegData <- vegData[!is.na(clim)] #don't predict with no climate data
  setnames(vegData, "clim", new = climVar)
  gc()

  spreadData <- makeMutuallyExclusive(dt = vegData,
                                      mutuallyExclusive = list("youngAge" = exclusiveCols))

  setcolorder(spreadData, neworder = c("pixelID", climVar, "youngAge"))
  sim$fireSense_SpreadCovariates <- spreadData

  return(invisible(sim))
}

.inputObjects <- function(sim) {
  cacheTags <- c(currentModule(sim), "function:.inputObjects")
  dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)
  message(currentModule(sim), ": using dataPath '", dPath, "'.")

  if (!suppliedElsewhere("flammableRTM", sim)){
    stop("please supply flammableRTM")
  }


  if (!suppliedElsewhere("landcoverDT", sim)) {
    if (!suppliedElsewhere("rstLCC", sim)) {
      sim$rstLCC <- prepInputsLCC(year = 2010,
                                  destinationPath = dPath,
                                  rasterToMatch = sim$flammableRTM)
    }

    if (!suppliedElsewhere("nonForestedLCCGroups", sim)) {
      #there is potential for problems if rstLCC is supplied and nonForestedLCCGroups is not, and vice versa
      sim$nonForestedLCCGroups <- list(
        "nonForest_highFlam" = c(8, 10, 14),#shrubland, grassland, wetland
        "nonForest_lowFlam" = c(11, 12, 15) #shrub-lichen-moss + cropland. 2 barren classes are nonflam
      )
    }

    if (!suppliedElsewhere("flammableRTM", ))

    sim$landcoverDT <- makeLandcoverDT(rstLCC = sim$rstLCC,
                                       flammableRTM = sim$flammableRTM,
                                       forestedLCC = P(sim)$forestedLCC,
                                       nonForestedLCCGroups = sim$nonForestedLCCGroups)
  }

  if (!suppliedElsewhere("nonForest_timeSinceDisturbance", sim)) {
    message("nonForest_timeSinceDisturbance not supplied - generating simulated map")
    #this is untested
    sim$nonForest_timeSinceDisturbance <- setValues(sim$flammableRTM,
                                                    round(P(sim)$cutoffForYoungAge/2))
    #users can crop the object generated by dataPrepFit
  }

  return(invisible(sim))
}
