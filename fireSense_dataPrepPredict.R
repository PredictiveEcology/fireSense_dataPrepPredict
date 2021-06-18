defineModule(sim, list(
  name = "fireSense_dataPrepPredict",
  description = "",
  keywords = "",
  authors = c(
    person("Ian", "Eddy", role = c("aut", "cre"), email = "ian.eddy@#canada.ca"),
    person("Eliot", "McIntire", role = "aut", email = "eliot.mcintire@#canada.ca"),
    person("Alex M", "Chubaty", role = "ctb", email = "achubaty@for-cast.ca")
  ),
  childModules = character(0),
  version = list(SpaDES.core = "1.0.4.9003", fireSense_dataPrepPredict = "0.0.0.9000"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = deparse(list("README.txt", "fireSense_dataPrepPredict.Rmd")),
  reqdPkgs = list("data.table", "PredictiveEcology/fireSenseUtils@development (>=0.0.4.9052)", "raster"),
  parameters = rbind(
    defineParameter("cutoffForYoungAge", class = "numeric", 15, NA, NA,
                    desc = paste("Age at and below which pixels are considered 'young' --> young <- age <= cutoffForYoungAge")),
    defineParameter(name = "fireTimeStep", "numeric", 1, NA, NA, desc = "time step of fire model"),
    defineParameter(name = "missingLCCgroup", class = "character", "nonForest_highFlam", NA, NA,
                    desc = paste("if a pixel is forested but is absent from cohortData, it will be grouped in this class.",
                                 "Must be one of the names in sim$nonForestedLCCGroups")),
    defineParameter(name = "sppEquivCol", class = "character", default = "LandR", NA, NA,
                    desc = "column name in sppEquiv object that defines unique species in cohortData"),
    defineParameter(name = "whichModulesToPrepare", class = "character",
                    default = c("fireSense_SpreadPredict", "fireSense_IgnitionPredict", "fireSense_EscapeFit"),
                    NA, NA, desc = "Which fireSense fit modules to prep? defaults to all 3"),
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
    expectsInput(objectName = "climateComponentsToUse", objectClass = "character",
                 desc = "names of the climate components to use in ignition, escape, and spread models"),
    expectsInput(objectName = "cohortData", objectClass = "data.table",
                 desc = "table that defines the cohorts by pixelGroup"),
    expectsInput(objectName = "flammableRTM", objectClass = "RasterLayer", sourceURL = NA,
                 desc = "RTM without ice/rocks/urban/water. Flammable map with 0 and 1."),
    expectsInput(objectName = 'nonForest_timeSinceDisturbance', objectClass = 'RasterLayer',
                 desc = 'time since burn for non-forested pixels'),
    expectsInput(objectName = "PCAveg", objectClass = "prcomp",
                  desc = "PCA model for veg and LCC covariates, needed for FS models"),
    expectsInput(objectName = 'pixelGroupMap', objectClass = "RasterLayer",
                 'RasterLayer that defines the pixelGroups for cohortData table'),
    expectsInput(objectName = "projectedClimateLayers", objectClass = "list",
                 desc = paste("list of projected climate variables in raster stack form",
                              "named according to variable, with names of individual raster layers",
                              "following the convention 'year<year>'")),
    expectsInput(objectName = "landcoverDT", objectClass = "data.table",
                 desc = "data.table with pixelID and relevant landcover classes"),
    expectsInput(objectName = "rstCurrentBurn", objectClass = "RasterLayer",
                 desc = "binary raster with 1 representing annual burn"),
    expectsInput(objectName = "sppEquiv", objectClass = "data.table", sourceURL = NA,
                 desc = "table of LandR species equivalencies"),
    expectsInput(objectName = "terrainDT", objectClass = "data.table",
                 desc = "data.table with pixelID and relevant terrain variables"),
    expectsInput(objectName = "vegComponentsToUse", objectClass = "character",
                 desc = "names of the veg components to use in ignition, escape, and spread predict models")
  ),
  outputObjects = bindrows(
    createsOutput(objectName = "currentClimateLayers", objectClass = "list",
                  desc = "list of project climate rasters at current time of sim"),
    createsOutput(objectName = 'fireSense_IgnitionAndEscapeCovariates', objectClass = 'data.table',
                  desc = paste("data.table of covariates for ignition prediction, with pixelID column",
                                "corresponding to flammableRTM pixel index")),
    createsOutput(objectName = 'fireSense_SpreadCovariates', objectClass = 'data.table',
                  desc = paste("data.table of covariates for spread prediction, with pixelID column",
                                "corresponding to flammableRTM pixel index")),
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
      sim <- scheduleEvent(sim, start(sim), "fireSense_dataPrepPredict", "getClimateLayers")

      if ("fireSense_IgnitionPredict" %in% P(sim)$whichModulesToPrepare |
          "fireSense_EscapePredict" %in% P(sim)$whichModulesToPrepare) {
        sim <- scheduleEvent(sim, start(sim), "fireSense_dataPrepPredict", "prepIgnitionAndEscapePredictData",
                             eventPriority = 5.10)
      }

      if ("fireSense_SpreadPredict" %in% P(sim)$whichModulesToPrepare) {
        sim <- scheduleEvent(sim, start(sim), "fireSense_dataPrepPredict", "prepSpreadPredictData",
                             eventPriority = 5.10)
      }
      # schedule future event(s)
      sim <- scheduleEvent(sim, P(sim)$.plotInitialTime, "fireSense_dataPrepPredict", "plot", eventPriority = 5.12)
      sim <- scheduleEvent(sim, P(sim)$.saveInitialTime, "fireSense_dataPrepPredict", "save", eventPriority = 5.12)
    },

    plot = {
      if ("fireSense_IgnitionPredict" %in% P(sim)$whichModulesToPrepare) {
        sim <- plotIgnitionCovariates(sim)
      }
      if ("fireSense_SpreadPredict" %in% P(sim)$whichModulesToPrepare) {
        sim <- plotSpreadCovariates(sim)
      }

    },
    save = {

    },
    ageNonForest = {
      sim$nonForest_timeSinceDisturbance <- ageNonForest(TSD = sim$nonForest_timeSinceDisturbance,
                                                         rstCurrentBurn = sim$rstCurrentBurn,
                                                         timeStep = P(sim)$fireTimeStep)
      sim <- scheduleEvent(sim, time(sim) + P(sim)$fireTimeStep,
                           "fireSense_dataPrepPredict", "ageNonForest")
    },
    getClimateLayers = {
      sim$currentClimateLayers <- getCurrentClimate(sim$projectedClimateLayers,
                                                    time(sim),
                                                    rasterToMatch = sim$rasterToMatch)
      sim <- scheduleEvent(sim, time(sim) + P(sim)$fireTimeStep,
                           "fireSense_dataPrepPredict", "getClimateLayers")

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

  # if (!compareRaster(sim$pixelGroupMap, sim$projectedClimateLayers[[1]])) {
  #   stop("mismatch in resolution detected - please review the resolution of sim$projectedClimateLayers")
  # }


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
plotIgnitionCovariates <- function(sim) {

  fuelClasses <- setdiff(names(sim$fireSense_IgnitionAndEscapeCovariates),
                         c("pixelID", names(sim$currentClimateLayers)))
  fuelRas <- raster(sim$flammableRTM)
  fuelRas[!is.na(sim$flammableRTM[]) & sim$flammableRTM[] == 0] <- 0
  #melting is faster than as.numeric(factor(paste0(<fuelClasses))) and reliably tested
  fuels <- melt.data.table(data = sim$fireSense_IgnitionAndEscapeCovariates,
                           id.vars = "pixelID", measure.vars = fuelClasses,
                           variable.name = "fuel", variable.factor = TRUE)

  fuels <- fuels[value == 1]
  fuelRas[fuels$pixelID] <- fuels$fuel
  fuelClasses <- c("nonflammable", fuelClasses)

  fuelRas <- as.data.frame(as(fuelRas, "SpatialPixelsDataFrame"))
  names(fuelRas) <- c("value", "x", "y")
  fuelRas$value <- as.factor(fuelRas$value)
  levels(fuelRas$value) <- fuelClasses

  g <- ggplot() +
    geom_raster(data = fuelRas,
                aes(x = x, y = y, fill = value),
                show.legend = TRUE) +
    theme_minimal() +
    labs(title = paste0("ignition fuel classes in ", time(sim)))

  #assign a default scale if using 'default'
  if (all(c("class2", "class3", "youngAge", "nonForest_lowFlam", "nonForest_highFlam") %in% fuelClasses)){
    pal <- c("#C5C6D0", "#74B72E", "#234F1E", "#68ff5f", "#E3B104", "#8E762C")
    names(pal) <- fuelClasses
    g <- g  +  scale_fill_manual(name = "fuel class",
                           values = pal,
                           labels = names(pal))
  }

  ggsave(filename = paste0("ignitionFuelClasses_", time(sim), ".png"), plot = g,
        device = "png", path = filePath(outputPath(sim), "figures"))
 #consider making Utils functino
  return(invisible(sim))
}


plotSpreadCovariates <- function(sim) {
  #This would not be easy to construct in a function, unless you save spreadCovariates
  #if you dont save spreadCovariates, then the entire dataPrep event must be made into functions

  spreadPlot <- lapply(sim$vegComponentsToUse, FUN = function(pc, ras = raster(sim$flammableRTM),
                                                              dt = sim$fireSense_SpreadCovariates) {
    ras[dt$pixelID] <- dt[,get(pc)]
    return(ras)
  })
  names(spreadPlot) <- sim$vegComponentsToUse
  spreadPlot <- stack(spreadPlot)

  spreadPlot <- as.data.frame(as(spreadPlot, "SpatialPixelsDataFrame"))

  #making it a single plot will not work unless each PC uses separate colour scale
  spreadPlot <- lapply(sim$vegComponentsToUse, FUN = function(pc, df = spreadPlot){
    g <- ggplot() +
      geom_raster(data = df,
                  aes_string(x = "x", y = "y", fill = eval(pc)),
                  show.legend = TRUE) +
      scale_colour_gradient2(aesthetics = c("fill"),
                             low = "#4575b4",
                             mid = "#ffffbf",
                             high = "#d73027",
                             midpoint = 0,
                             na.value = "black") +
      theme_minimal()
    return(g)
  })

  names(spreadPlot) <- paste0(sim$vegComponentsToUse, "plot_", time(sim))

  lapply(names(spreadPlot), FUN = function(name, thePath = filePath(outputPath(sim), "figures"),
                                           sp = spreadPlot){
    ggsave(plot = sp[[name]], filename = paste0(name, ".png"),
           path = thePath,
           device = "png", )
  })

  return(invisible(sim))
}

getCurrentClimate <- function(projectedClimateLayers, time, rasterToMatch) {

  availableYears <- as.numeric(gsub(pattern = "year",
                               x = names(projectedClimateLayers[[1]]),
                               replacement = ""))
  if (time > max(availableYears)) {
    cutoff <- quantile(availableYears, probs = 0.9)
    time <- sample(availableYears[availableYears >= cutoff], size = 1)
    message(paste0("re-using projected climate layers from ", time))
  }
  ## this will work with a list of raster stacks
  thisYearsClimate <- lapply(projectedClimateLayers, FUN = function(x, rtm = rasterToMatch) {
    ras <- x[[paste0("year", time)]]
    if (!compareRaster(ras, rtm, stopiffalse = FALSE)){
      message("reprojecting fireSense climate layers")
      ras <- postProcess(ras, rasterToMatch = rtm)
    }
    return(ras)
  })

  return(thisYearsClimate)
}

ageNonForest <- function(TSD, rstCurrentBurn, timeStep) {
  TSD <- setValues(TSD, getValues(TSD) + timeStep)
  if (!is.null(rstCurrentBurn)) {
    unburned <- is.na(rstCurrentBurn[]) | rstCurrentBurn[] == 0
    TSD[!unburned] <- 0
  }
  return(TSD)
}

prepare_IgnitionAndEscapePredict <- function(sim) {

  #get fuel classes
  fuelClasses <- cohortsToFuelClasses(cohortData = sim$cohortData,
                                      sppEquiv = sim$sppEquiv,
                                      sppEquivCol = P(sim)$sppEquivCol,
                                      pixelGroupMap = sim$pixelGroupMap,
                                      flammableMap = sim$flammableRTM,
                                      cutoffForYoungAge = P(sim)$cutoffForYoungAge)
  #make columns for each fuel class
  fcs <- names(fuelClasses)
  getPix <- function(fc, type, index) { fc[[type]][index]}
  fuelDT <- data.table(pixelID = sim$landcoverDT$pixelID)
  fuelDT[, c(fcs) := nafill(lapply(fcs, getPix, fc = fuelClasses, index = fuelDT$pixelID), fill = 0)]

  #
  ignitionCovariates <- fuelDT[sim$landcoverDT, on = c("pixelID")]
  ignitionCovariates[, rowcheck := rowSums(.SD), .SD = setdiff(names(ignitionCovariates), 'pixelID')]
  #if all rows are 0, it must be a forested LCC absent from cohortData
  ignitionCovariates[rowcheck == 0, eval(P(sim)$missingLCC) := 1]
  set(ignitionCovariates, NULL, 'rowcheck', NULL)

  # this was using fireSenseUtils::climateRasterToDataTable - but it was very slow for this simple case of 1 raster
  #climData <- climateRasterToDataTable(historicalClimateRasters = sim$currentClimateLayers,
                                       # Index = ignitionCovariates$pixelID)
  ignitionCovariates[, clim := getValues(sim$currentClimateLayers[[1]])[ignitionCovariates$pixelID]]
  ignitionCovariates <- ignitionCovariates[!is.na(clim)] #don't predict with no climate data
  setnames(ignitionCovariates, 'clim', new = names(sim$currentClimateLayers))

  sim$fireSense_IgnitionAndEscapeCovariates <- ignitionCovariates

  return(invisible(sim))
}


prepare_SpreadPredict <- function(sim) {

  #cohortData, pixelGroupMap, nonforest_standAge, terrainDT, landcoverDT, PCA....
  #1) build fireSense vegData from cohortData + landcoverDT
  vegPCAdat <- castCohortData(cohortData = sim$cohortData,
                            pixelGroupMap = sim$pixelGroupMap,
                            ageMap = sim$nonForest_timeSinceDisturbance,
                            terrainDT = sim$terrainDT,
                            lcc = sim$landcoverDT,
                            missingLCC = P(sim)$missingLCCgroup)

  #redo PCA -
  vegList <- makeVegTerrainPCA(dataForPCA = vegPCAdat, PCA = sim$PCAveg,
                               dontWant = c("pixelGroup", "pixelID", "youngAge"))

  #returns a list with only one usable object (PCA is null due to predict)
  vegData <- vegList$vegComponents
  rm(vegList)

  #rename vegcolumns
  colsToRename <- colnames(vegData[, -c("pixelID", "youngAge"),])
  setnames(vegData, colsToRename, paste0("veg", colsToRename))
  #subset by columns used in model
  keep <- c("pixelID", "youngAge", sim$vegComponentsToUse)
  remove <- setdiff(colnames(vegData), keep)
  set(vegData, NULL, remove, NULL)

  #the index is the cells in vegData - these are flammable cells only
  #because landcoverDT contains only flammable cells
  climVar <- names(sim$currentClimateLayers)
  vegData[, clim := getValues(sim$currentClimateLayers[[1]])[vegData$pixelID]]
  vegData <- vegData[!is.na(clim)] #don't predict with no climate data
  setnames(vegData, "clim", new = climVar)

  #TODO: this vegPC will have to become a param if we plan on running with biomass instead of PCA
  spreadData <- makeMutuallyExclusive(dt = vegData,
                                      mutuallyExclusive = list("youngAge" = "vegPC"))

  setcolorder(spreadData, neworder = c("pixelID", climVar, 'youngAge'))
  sim$fireSense_SpreadCovariates <- spreadData

  return(invisible(sim))
}


.inputObjects <- function(sim) {

  cacheTags <- c(currentModule(sim), "function:.inputObjects")
  dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)
  message(currentModule(sim), ": using dataPath '", dPath, "'.")

  if (!suppliedElsewhere("PCAveg", sim)) {
    stop("Please supply PCAveg by running fireSense_dataPrepFit")
  }

  if (!suppliedElsewhere("landcoverDT", sim)) {
    stop("Please supply landcoverDT by running fireSense_dataPrepFit")
  }

  if (!suppliedElsewhere("nonForest_timeSinceDisturbance", sim)) {
    stop("Please supply nonForest_timeSinceDisturbance by running fireSense_dataPrepFit")
    #It is a lot of work to supply a meaningful version of this object
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
