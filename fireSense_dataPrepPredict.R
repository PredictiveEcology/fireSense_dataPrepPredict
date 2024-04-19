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
  version = list(fireSense_dataPrepPredict = "1.0.1"),
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
    expectsInput("climateVariablesForFire", "list", sourceURL = NA,
                 paste("A list detailing which climate variables in `sim$projectedClimateRasters`",
                       "to use for which fire processes (ignition and spread). If the list is length one,",
                       "both processes will use the same variables. The default is to use 'MDC'.")),
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
    expectsInput("rasterToMatch", "SpatRaster", sourceURL = NA,
                 desc = "template raster used only to derive flammableRTM if the latter is absent"),
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
      sim <- getCurrentClimate(sim)
      sim$currentClimateRasters <- lapply(sim$currentClimateRasters, terra::unwrap)
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
  ## TODO: assume a vector of variables has been passed?
  if (length(sim$climateVariablesForFire) == 1) {
    sim$climateVariablesForFire <- list("ignition" = sim$climateVariablesForFire,
                                        "spread" = sim$climateVariablesForFire)
  }

   if (!compareGeom(sim$pixelGroupMap, sim$projectedClimateRasters[[1]], stopOnError = FALSE)) {
     stop("mismatch in resolution detected - please review the resolution of sim$projectedClimateRasters")
   }

  return(invisible(sim))
}

### template for plot events

getCurrentClimate <- function(sim) {
  ## this function has been rewritten due to an undiagnosed bug involving
  ##   digest of a file-backed SpatRaster, and restartSpades()
  availableYears <- as.numeric(gsub(pattern = "year",
                               x = names(sim$projectedClimateRasters[[1]]),
                               replacement = ""))
  if (time(sim) > max(availableYears)) {
    cutoff <- quantile(availableYears, probs = 0.9)
    time <- sample(availableYears[availableYears >= cutoff], size = 1)
    message(paste0("re-using projected climate layers from ", time))
  }
  ## this will work with a list of raster stacks
  thisYearsClimate <- lapply(sim$projectedClimateRasters,
                             FUN = function(x, rtm = sim$rasterToMatch, currentYear = time(sim)) {
    ras <- x[[paste0("year", currentYear)]]
    if (!compareGeom(ras, rtm, stopOnError = FALSE)) {
      message("reprojecting fireSense climate layers")
      ras <- postProcess(ras, rasterToMatch = rtm)
    }
    return(ras)
  })

  sim$currentClimateRasters <- lapply(thisYearsClimate, terra::wrap)

  return(sim)
}

ageNonForest <- function(TSD, rstCurrentBurn, timeStep) {

  TSDvals <- as.vector(TSD)
  TSDvals <- TSDvals + 1
  if (!is.null(rstCurrentBurn)) {
    burnVals <-as.vector(rstCurrentBurn)
    unburned <- is.na(burnVals) | burnVals == 0
    TSDvals[!unburned] <- 0
  }
  TSD <- setValues(TSD, TSDvals)
  rm(TSDvals, burnVals, unburned)
  gc()
  return(TSD)
}

prepare_IgnitionAndEscapePredict <- function(sim) {
  ## get climate
  ignitionClimate <- sim$currentClimateRasters[sim$climateVariablesForFire$ignition]

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

  ## TODO: this was relevant when SpatRaster values couldn't be subset using square brackets
  ## It shouldn't be necessary now
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

  #approach must allow for multiple potential climate variable, due to shift from "hockey stick" model
  climateCovariates <- rast(ignitionClimate)
  climateCovariates <- na.omit(as.data.frame(climateCovariates, cells = TRUE))
  set.names(climateCovariates, new = c("pixelID", names(ignitionClimate)))
  ignitionCovariates <- climateCovariates[ignitionCovariates, on = c("pixelID")]

  sim$fireSense_IgnitionAndEscapeCovariates <- ignitionCovariates

  gc()
  return(invisible(sim))
}

prepare_SpreadPredict <- function(sim) {
  spreadClimate <- sim$currentClimateRasters[sim$climateVariablesForFire$spread]

  ## much of this chunk can now be combined into a function, called for both ig and spread prep
  ## this fits cohortData into fuel classes
  ##  if pixels are missing/absent but are able to be forested as determined by landcoverDT,
  ##  they receive 0 values - e.g. pixelGroup zero
  vegData <- cohortsToFuelClasses(cohortData = sim$cohortData,
                                  pixelGroupMap = sim$pixelGroupMap,
                                  flammableRTM = sim$flammableRTM,
                                  sppEquiv = sim$sppEquiv,
                                  landcoverDT = sim$landcoverDT,
                                  fuelClassCol = P(sim)$spreadFuelClassCol,
                                  sppEquivCol = P(sim)$sppEquivCol,
                                  cutoffForYoungAge = P(sim)$cutoffForYoungAge)

  fcs <- names(vegData)
  ## Nov 2023 - changes to terra package necessitate `as.vector`included
  getPix <- function(fc, type, index) {as.vector(fc[type])[index]}
  fuelDT <- data.table(pixelID = sim$landcoverDT$pixelID)
  fuelDT[, c(fcs) := lapply(fcs, getPix, fc = vegData, index = fuelDT$pixelID)]
  vegData <- fuelDT[sim$landcoverDT, on = c("pixelID")]

  ## Nov 2023 - there should not be NA values - previously this used nafill
  ## if they return - use x <- as.data.table(nafill(vegData), 0) and setnames(x, names(vegData))
  vegData[, rowcheck := rowSums(.SD), .SD = setdiff(names(vegData), 'pixelID')]
  if (any(is.na(vegData$rowCheck))) {
    stop("NA in vegData columns of fireSense_dataPrepPredict... please contact module developers")
  }
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

  #approach must allow for multiple potential climate variable, due to shift from "hockey stick" model
  climateCovariates <- rast(spreadClimate)
  climateCovariates <- as.data.frame(climateCovariates, cells = TRUE)
  climateCovariates <- na.omit(climateCovariates)
  set.names(climateCovariates, new = c("pixelID", names(spreadClimate)))
  vegData <- climateCovariates[vegData, on = c("pixelID")]

  spreadData <- makeMutuallyExclusive(dt = vegData, mutuallyExclusive = list("youngAge" = exclusiveCols))

  setcolorder(spreadData, neworder = c("pixelID", names(spreadClimate), "youngAge"))
  sim$fireSense_SpreadCovariates <- spreadData

  return(invisible(sim))
}

.inputObjects <- function(sim) {
  cacheTags <- c(currentModule(sim), "otherFunctions:.inputObjects")
  dPath <- asPath(inputPath(sim), 1)
  message(currentModule(sim), ": using dataPath '", dPath, "'.")

  if (!suppliedElsewhere("climateVariablesForFire", sim)) {
    sim$climateVariablesForFire <- list(spread = "MDC",
                                        ignition = "MDC")
  }

  if (!suppliedElsewhere("flammableRTM", sim)) {
    rstLCC <- prepInputsLCC(year = 2010, destinationPath = dPath,
                            rasterToMatch = sim$rasterToMatch)
    sim$flammableRTM <- defineFlammable(rstLCC, nonFlammClasses = c(13, 16, 17, 18, 19),
                                        mask = sim$rasterToMatch)
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
