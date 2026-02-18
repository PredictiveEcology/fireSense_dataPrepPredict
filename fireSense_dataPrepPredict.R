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
  version = list(fireSense_dataPrepPredict = "1.0.2"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = deparse(list("README.txt", "fireSense_dataPrepPredict.Rmd")),
  loadOrder = list(after = c("Biomass_borealDataPrep", "fireSense_dataPrepFit",
                             "fireSense_IgnitionFit", "fireSense_SpreadFit")),
  reqdPkgs = list(
    "data.table",
    "PredictiveEcology/fireSenseUtils@development (>= 0.0.5.9050)",
    "terra"
    #reproducible@ai
  ),
  parameters = rbind(
    defineParameter("cutoffForYoungAge", "numeric", 15, NA, NA,
      desc = paste(
        "Age at and below which pixels are considered 'young'",
        "(i.e., `age <= cutoffForYoungAge`)."
      )
    ),
    defineParameter(
      "dataYear", "numeric", 2011, 1985, 2022,
      "Used to override the default 'sourceURL' of NTEMS data for objects when not supplied"
    ),
    defineParameter("fireTimeStep", "numeric", 1, NA, NA, desc = "time step of fire model"),
    defineParameter("forestedLCC", "numeric", c(81, 210, 220, 230, 240), NA, NA,
      desc = "forested landcover classes in `rstLCC` - only relevant if `landcoverDT` is not supplied"
    ),
    defineParameter(
      "flammabilityThreshold", "numeric", 0.1, 0, 1,
      paste("Minimum proportion of flammable old pixel needed to define a new pixel
                          as flammable when upscaling the default flammable maps`.")
    ),
    defineParameter("fuelClassCol", "character", "FuelClass", NA, NA,
      desc = "the column in sppEquiv that defines unique fuel classes for ignition"
    ),
    defineParameter("nonflammableLCC", "numeric", c(0, 20, 31, 32, 33), NA, NA,
      desc = paste(
        "used to create flammableRTM if unsupplied.",
        "The non-flammable LCC in rstLCC layers - which",
        "default to water, snow/ice, rock, and barren land in NTEMS LCC"
      )
    ),
    defineParameter("nonForestCanBeYoungAge", "logical", TRUE, NA, NA,
                    desc = "update non-forest when burned, to become youngAge"),
    defineParameter("sppEquivCol", "character", "LandR", NA, NA,
                    desc = "column name in `sppEquiv` object that defines unique species in `cohortData`"),
    defineParameter("whichModulesToPrepare", "character",
                    default = c("fireSense_SpreadPredict", "fireSense_IgnitionPredict", "fireSense_EscapeFit"),
                    NA, NA,
                    desc = "Which fireSense fit modules to prep? defaults to all 3"),
    defineParameter(".plotInitialTime", "numeric", NA, NA, NA,
      "Describes the simulation time at which the first plot event should occur."
    ),
    defineParameter(
      ".plotInterval", "numeric", NA, NA, NA,
      "Describes the simulation time interval between plot events."
    ),
    defineParameter(
      ".runInitialTime", "numeric", start(sim), NA, NA, "time to simulate initial fire"
    ),
    defineParameter(
      ".saveInitialTime", "numeric", NA, NA, NA,
      "Describes the simulation time at which the first save event should occur."
    ),
    defineParameter(
      ".saveInterval", "numeric", NA, NA, NA,
      "This describes the simulation time interval between save events."
    ),
    defineParameter(
      ".useCache", "logical", FALSE, NA, NA,
      paste(
        "Should this entire module be run with caching activated?",
        "This is generally intended for data-type modules, where stochasticity",
        "and time are not relevant"
      )
    )
  ),
  inputObjects = bindrows(
    expectsInput("climateVariablesForFire", "list",
      sourceURL = NA,
                 paste(
                   "A list detailing which climate variables in `sim$projectedClimateRasters`",
                   "to use for which fire processes (ignition and spread). If the list is length one,",
                   "both processes will use the same variables. The default is to use 'MDC'.")),
    expectsInput("climateYear", "character",
                 paste("optional character vector giving year (e.g. 'year2009') for preparing",
                       "the `currentClimateRasters` object. If unsupplied, `time(sim)` is used.",
                       "see PredictiveEcology/climateYear")),
    expectsInput("cohortData", "data.table", NA,
                 "table that defines the cohorts by pixelGroup"),
    expectsInput("fireSense_IgnitionFitted", "fireSense_IgnitionFit", NA,
        "object containing slot `fittingRes` - the spatial resolution at which ignition will be predicted"),
    expectsInput("missingLCCgroup", "character", NA, paste(
        "if a pixel is forested but is absent from `cohortData`, it will be grouped in this class.",
        "It can be estimated if `P(sim)$estimateFuelClasses` is TRUE.",
        "If supplied, it must be one of the names in `sim$nonForestedLCCGroups`")),
    expectsInput("flammableRTM", "SpatRaster", "Flammable landcover i.e, conditions at start(sim). Taken from last layer of rstLCCs"),
    expectsInput("nonForestedLCCGroups", "list", NA, paste(
        "a named list of non-forested landcover groups, e.g. `list('wetland' = c(19, 23, 32))`.",
        "This is only relevant if `landcoverDT` is not supplied")),
    expectsInput("lightningMaps", "SpatRaster", NA, paste(
        "A 4-layer SpatRaster of lightning: lightningDays, lightningDensity, positiveCG, positiveCGdensity")),
    expectsInput("pixelGroupMap", "SpatRaster", NA, 
        "SpatRaster that defines the pixelGroups for cohortData table"),
    expectsInput("projectedClimateRasters", "list", NA, paste(
        "list of projected climate variables in raster stack form",
        "named according to variable, with names of individual raster layers",
        "following the convention 'year<year>'")),
    expectsInput("propFlammable", "SpatRaster", NA, paste(
        "a conditional object created if rstLCC is also not supplied, ",
        "a raster representing the proportion of flammable landcover in a pixel")),
    expectsInput("rasterToMatch", "SpatRaster", NA, 
        "template raster used only to derive `flammableRTM` if the latter is absent"),
    expectsInput("rstCurrentBurn", "SpatRaster", "binary raster with 1 representing annual burn"),
    expectsInput("rstLCC", "SpatRaster", "a landcover raster - only used if `landcoverDT` is not supplied"),
    expectsInput("sppEquiv", "data.table", "table of LandR species equivalencies"),
    # expectsInput("standAgeMaps", "list", sourceURL = NA,
    #              "list of length 2 of maps of stand age in dataYear[[1]] and dataYear[[2]]",
    #              " used to create `cohortDatas`. This is only used if standAgeMap is not supplied"),
    expectsInput("standAgeMap", "SpatRaster", "stand age map in study area; assumed to be ages at `start(sim)`"),
    expectsInput("landcoverDT", "data.table",
                  "`pixelID` and relevant landcover classes for flammable pixels in each layer, ",
                  "i.e, conditions at start(sim). Taken from last layer of landcoverDTs"),
    
  ),
  outputObjects = bindrows(
    createsOutput("currentClimateRasters", "list", 
        "list of project climate rasters at current time of sim"),
    createsOutput("fireSense_igAndEscapePred_Covariates", "data.table", paste(
        "data.table of covariates for ignition prediction, with pixelID column",
        "corresponding to flammableRTM pixel index")),
    createsOutput("fireSense_SpreadCovariates", "data.table", paste(
        "data.table of covariates for spread prediction, with pixelID column",
        "corresponding to flammableRTM pixel index")),
    # createsOutput("flammableRTM", "list", "List of (2) binary SpatRaster of flammable landcover for years given by the list names"),
    # createsOutput("landcoverDT", "data.table",
    #   "data.table with `pixelID` and relevant landcover classes for flammable pixels"),
    createsOutput("nonForest_timeSinceDisturbance", "SpatRaster",
      desc = "time since burn for non-forest pixels")
  )
))

## event types
#   - type `init` is required for initialization

doEvent.fireSense_dataPrepPredict <- function(sim, eventTime, eventType) {
  switch(eventType,
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
          "prepIgAndEscPredictData"
        )
      }

      if ("fireSense_SpreadPredict" %in% P(sim)$whichModulesToPrepare) {
        sim <- scheduleEvent(sim, P(sim)$.runInitialTime, "fireSense_dataPrepPredict", "prepSpreadPredictData"
        )
      }
      # schedule future event(s)
      # sim <- scheduleEvent(sim, P(sim)$.plotInitialTime, "fireSense_dataPrepPredict", "plot", eventPriority = 5.12)
    },
    ageNonForest = {
      sim$nonForest_timeSinceDisturbance <- ageNonForest(
        TSD = sim$nonForest_timeSinceDisturbance,
        rstCurrentBurn = sim$rstCurrentBurn,
        timeStep = P(sim)$fireTimeStep
      )
      sim <- scheduleEvent(
        sim, time(sim) + P(sim)$fireTimeStep,
        "fireSense_dataPrepPredict", "ageNonForest"
      )
    },
    getClimateRasters = {
      sim <- getCurrentClimate(sim)
      sim$currentClimateRasters <- lapply(sim$currentClimateRasters, terra::unwrap)
      sim <- scheduleEvent(
        sim, time(sim) + P(sim)$fireTimeStep,
        "fireSense_dataPrepPredict", "getClimateRasters"
      )
    },
    prepIgAndEscPredictData = {
      sim <- prepare_IgnitionAndEscapePredict(sim)
      sim <- scheduleEvent(sim, time(sim) + P(sim)$fireTimeStep,
        "fireSense_dataPrepPredict", "prepIgAndEscPredictData"
      )
    },
    prepSpreadPredictData = {
      sim <- prepare_SpreadPredict(sim)
      sim <- scheduleEvent(sim, time(sim) + P(sim)$fireTimeStep,
        "fireSense_dataPrepPredict", "prepSpreadPredictData"
      )
    },
    warning(paste("Undefined event type: \'", current(sim)[1, "eventType", with = FALSE],
      "\' in module \'", current(sim)[1, "moduleName", with = FALSE], "\'",
      sep = ""
    ))
  )
  return(invisible(sim))
}

## event functions
#   - keep event functions short and clean, modularize by calling subroutines from section below.

### template initialization
Init <- function(sim) {

  objs <- c(sim$standAgeMap, sim$rstLCC)
  
  if (!LandR::.compareRas(sim$rasterToMatch, objs[[1]], stopOnError = FALSE)) {
    objs <- lapply(objs, FUN = postProcess, to = sim$rasterToMatch)
  }
  if (!isInt(objs[[1]]) | !isInt(objs[[2]])) {
    objs <- lapply(objs, LandR::asInt)
  }
  standAgeMap <- objs[[1]]
  rstLCC <- objs[[2]]
  
  # sim$flammableRTM <- defineFlammable(rstLCC,
  #                                     nonFlammClasses = P(sim)$nonflammableLCC,
  #                                     to = sim$rasterToMatch)

  if (is.null(sim$landcoverDT)) {
    sim$landcoverDT <- makeLandcoverDT(
      rstLCC = rstLCC,
      flammableRTM = sim$flammableRTM,
      forestedLCC = P(sim)$forestedLCC,
      nonForestedLCCGroups = sim$nonForestedLCCGroups
    )
  }

  if (is.null(sim$nonForest_timeSinceDisturbance)) {
    fireYears <- c(P(sim)$dataYear - P(sim)$cutoffForYoungAge):P(sim)$dataYear
    firePolys <- fireSenseUtils::getFirePolygons(
                       fun = "sf::st_read", #I think it must be SF?
                       years = fireYears,
                       useInnerCache = FALSE,
                       destinationPath = inputPath(sim),
                       cropTo = sim$rasterToMatch,
                       maskTo = sim$studyArea,
                       projectTo = sim$rasterToMatch)# |>
      # Cache(userTags = c("firePolys",
                         # paste0(fireYears, collapse = ":")))

    sim$nonForest_timeSinceDisturbance <- makeTSD(
      year = P(sim)$dataYear,
      firePolys = firePolys,
      standAgeMap = standAgeMap,
      lcc = sim$landcoverDT,
      cutoffForYoungAge = P(sim)$cutoffForYoungAge
    )
  }

  ## TODO: assume a vector of variables has been passed?
  if (length(sim$climateVariablesForFire) == 1) {
    sim$climateVariablesForFire <- list(
      "ignition" = sim$climateVariablesForFire,
      "spread" = sim$climateVariablesForFire
    )
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
  availableYears <- as.numeric(gsub(
    pattern = "year",
    x = names(sim$projectedClimateRasters[[1]]),
    replacement = ""
  ))


  if (is.null(sim$climateYear)) {
    currentYear <- time(sim)
  } else {
    currentYear <- sim$climateYear
  }

  if (currentYear > max(availableYears)) {
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
    }
  )

  sim$currentClimateRasters <- lapply(thisYearsClimate, terra::wrap)

  return(sim)
}

ageNonForest <- function(TSD, rstCurrentBurn, timeStep) {
  TSDvals <- as.vector(TSD)
  TSDvals <- TSDvals + 1
  if (!is.null(rstCurrentBurn)) {
    burnVals <- as.vector(rstCurrentBurn)
    unburned <- is.na(burnVals) | burnVals == 0
    TSDvals[!unburned] <- 0
    rm(TSDvals, burnVals, unburned)
  }
  TSD <- setValues(TSD, TSDvals)
  # gc()
  return(TSD)
}

prepare_IgnitionAndEscapePredict <- function(sim) {
  ## get climate
  ignitionClimate <- sim$currentClimateRasters[sim$climateVariablesForFire$ignition]

  # Coming out of the CacheGeo, this is unreliably a data.frame instead of a data.table
  if (!data.table::is.data.table(sim$sppEquiv)) data.table::setDT(sim$sppEquiv)

  ## get fuel classes
  # if ignition and spread fuel classes are the same, this should use Mod
  # to avoid doing it twice (in spreadFit, assuming people run both events)
  fuelClasses <- cohortsToFuelClasses(
    cohortData = sim$cohortData,
    sppEquiv = sim$sppEquiv,
    sppEquivCol = P(sim)$sppEquivCol,
    pixelGroupMap = sim$pixelGroupMap,
    landcoverDT = sim$landcoverDT,
    flammableRTM = sim$flammableRTM,
    fuelClassCol = P(sim)$fuelClassCol,
    cutoffForYoungAge = P(sim)$cutoffForYoungAge
  )

  fcs <- setdiff(names(fuelClasses), "youngAge")
  fuelClasses <- as.data.table(as.data.frame(fuelClasses, cells = TRUE))
  setnames(fuelClasses, old = "cell", new = "pixelID")

  # make sure join is only landcoverDT
  ignitionCovariates <- fuelClasses[sim$landcoverDT, on = c("pixelID")]

  ignitionCovariates[, rowcheck := rowSums(.SD), .SD = setdiff(names(ignitionCovariates), "pixelID")]
  ## if all rows are 0, it must be a forested LCC absent from cohortData
  ignitionCovariates[rowcheck == 0, eval(sim$missingLCCgroup) := 1]
  set(ignitionCovariates, NULL, "rowcheck", NULL)

  # this must happen after the missingLC are evaluated
  ignitionCovariates <- ignitionCovariates[, eval(fcs) := lapply(.SD, FUN = logMinB), .SDcols = fcs]

  if (P(sim)$nonForestCanBeYoungAge) {
    ignitionCovariates[, YA_NF := as.vector(sim$nonForest_timeSinceDisturbance)[ignitionCovariates$pixelID] <=
                         P(sim)$cutoffForYoungAge]
    ignitionCovariates[YA_NF == TRUE, youngAge := 1]
    ignitionCovariates[, YA_NF := NULL]
  }

  exclusiveCols <- c(fcs, names(sim$landcoverDT))
  exclusiveCols <- setdiff(exclusiveCols, c("pixelID", "youngAge"))
  # TODO: I believe this triggers a warning
  ignitionCovariates <- makeMutuallyExclusive(
    dt = ignitionCovariates,
    mutuallyExclusive = list("youngAge" = exclusiveCols)
  )

  climateCovariates <- rast(ignitionClimate)

  climateCovariates <- na.omit(as.data.table(climateCovariates, cells = TRUE))
  setnames(climateCovariates, new = c("pixelID", names(ignitionClimate)))
  ignitionCovariates <- climateCovariates[ignitionCovariates, on = c("pixelID")]

  #put back into raster for aggregate
  #TODO: this should be put in fireSenseUtils
  covCols <- setdiff(names(ignitionCovariates), "pixelID")
  rasObj <- rast(sim$flammableRTM)
  rasObj[!is.na(sim$flammableRTM[])] <- 0 #make sure NA is 0 for mean during aggregate
  out <- lapply(covCols, FUN = function(cov, ras = rasObj, covDT = ignitionCovariates) {
    ras[covDT$pixelID] <- covDT[[cov]]
    return(ras)
  })
  names(out) <- covCols
  ignitionCovariates <- rast(out)

  # now aggregate to match fitting resolution...
  mods <- sim$fireSense_IgnitionFitted$modelList
  if (!is.null(mods$fittingRes)) {
    # following changes to ignitionModel - prediction will now occur at same spatial scale,
    # location of predicted ignitions will be randomly drawn from finer scale
    igAggFactor <- ceiling(mods$fittingRes / c(res(sim$rasterToMatch)[1]))
    ignitionCovariates <- terra::aggregate(ignitionCovariates, fact = igAggFactor)
  }

  # Lightning --> was at 1km resolution, so don't add prior to aggregation; add after
  a <- postProcess(sim$lightningMaps[[2]], to = ignitionCovariates)
  lightningTxt <- grep("lightn", unlist(sim$fireSense_IgnitionFitted$scaleData$dimnames), ignore.case = TRUE, value = TRUE)
  names(a) <- lightningTxt

  ignitionCovariates <- c(ignitionCovariates, lightning = a)

  ignitionCovariates <- as.data.table(ignitionCovariates, cells = TRUE)
  setnames (ignitionCovariates, old = "cell", new = "pixelID")

  sim$fireSense_igAndEscapePred_Covariates <- ignitionCovariates

  # gc()
  return(invisible(sim))
}

prepare_SpreadPredict <- function(sim) {
  spreadClimate <- sim$currentClimateRasters[sim$climateVariablesForFire$spread]

  ## much of this chunk can now be combined into a function, called for both ig and spread prep
  ## this fits cohortData into fuel classes
  ##  if pixels are missing/absent but are able to be forested as determined by landcoverDT,
  ##  they receive 0 values - e.g. pixelGroup zero
  spreadCovariates <- fireSenseUtils:::fireSenseCovariatesCreate(
    cohortData = sim$cohortData,
    pixelGroupMap = sim$pixelGroupMap,
    flammableRTM = sim$flammableRTM,
    landcoverDT = sim$landcoverDT,
    
    sppEquiv = sim$sppEquiv,
    sppEquivCol = P(sim)$sppEquivCol,
    fuelClassCol = P(sim)$fuelClassCol,
    cutoffForYoungAge = P(sim)$cutoffForYoungAge,
    missingLCCgroup = sim$missingLCCgroup,
    nonForestedLCCGroups = sim$nonForestedLCCGroups,
    nonForestCanBeYoungAge = P(sim)$nonForestCanBeYoungAge,
    nonForest_timeSinceDisturbance = sim$nonForest_timeSinceDisturbance,
    studyAreaName = P(sim)$.studyAreaName,
    useCache = FALSE # predict is annual, no point in caching
    
    #fuelClassCol = P(sim)$fuelClassCol,
    #sppEquivCol = P(sim)$sppEquivCol,
    #cutoffForYoungAge = P(sim)$cutoffForYoungAge,
    #missingLCCgroup = sim$missingLCCgroup,
    #nonForestedLCCGroups = sim$nonForestedLCCGroups,
    #nonForest_timeSinceDisturbance = sim$nonForest_timeSinceDisturbance,
    #nonForestCanBeYoungAge = P(sim)$nonForestCanBeYoungAge
  ) 
  
  # fuelClasses <- cohortsToFuelClasses(
  #   cohortData = sim$cohortData,
  #   pixelGroupMap = sim$pixelGroupMap,
  #   flammableRTM = sim$flammableRTM,
  #   sppEquiv = sim$sppEquiv,
  #   landcoverDT = sim$landcoverDT,
  #   fuelClassCol = P(sim)$fuelClassCol,
  #   sppEquivCol = P(sim)$sppEquivCol,
  #   cutoffForYoungAge = P(sim)$cutoffForYoungAge
  # )
  # 
  # 
  # ## make columns for each fuel class
  # # fuelClasses <- terra::app(fuelClasses, fun = logMinB)
  # # terra app is horrifically slow
  # # fcs <- setdiff(names(fuelClasses), "youngAge")
  # fuelClasses <- as.data.table(as.data.frame(fuelClasses, cells = TRUE))
  # setnames(fuelClasses, old = "cell", new = "pixelID")
  # 
  # # make sure join is only landcoverDT -- this adds the nonForest that are in sim$landcoverDT
  # spreadCovariates <- fuelClasses[sim$landcoverDT, on = c("pixelID")]
  # 
  # 
  # ## Nov 2023 - there should not be NA values - previously this used nafill
  # ## if they return - use x <- as.data.table(nafill(vegData), 0) and setnames(x, names(vegData))
  # spreadCovariates[, rowcheck := rowSums(.SD), .SD = setdiff(names(spreadCovariates), "pixelID")]
  # if (any(is.na(spreadCovariates$rowCheck))) {
  #   stop("NA in vegData columns of fireSense_dataPrepPredict... please contact module developers")
  # }
  # # if all rows are 0, it must be a forested LCC absent from cohortData
  # spreadCovariates[rowcheck == 0, eval(sim$missingLCCgroup) := 1]
  # set(spreadCovariates, NULL, "rowcheck", NULL)
  # 
  # # Making exclusive has to be prior to logMinB, or else the 0 biomass become -0.59 or so
  # #   --> they need to stay at the minimum of 3.605
  # exclusiveCols <- c(fcs, names(sim$landcoverDT))
  # exclusiveCols <- setdiff(exclusiveCols, "pixelID")
  # spreadCovariates <- makeMutuallyExclusive(dt = spreadCovariates,
  #                                     mutuallyExclusive = list("youngAge" = exclusiveCols))
  # 
  # spreadCovariates <- spreadCovariates[, eval(fcs) := lapply(.SD, FUN = logMinB), .SDcols = fcs]
  # 
  # 
  # if (P(sim)$nonForestCanBeYoungAge) {
  #   # this should only alter non-forest
  #   spreadCovariates[, isNonForest := rowSums(.SD) > 0, .SDcol = names(sim$nonForestedLCCGroups)]
  #   spreadCovariates[, YA_NF := as.vector(sim$nonForest_timeSinceDisturbance)[spreadCovariates$pixelID] <= P(sim)$cutoffForYoungAge &
  #     isNonForest == TRUE]
  #   spreadCovariates[YA_NF == TRUE, youngAge := 1]
  #   spreadCovariates[, c("YA_NF", "isNonForest") := NULL]
  # }

  # exclusiveCols <- c(fcs, names(sim$landcoverDT))
  # exclusiveCols <- setdiff(exclusiveCols, "pixelID")

  # TODO: this chunk is untested 18/12/2024
  climateCovariates <- rast(spreadClimate) |> as.data.frame(cells = TRUE)
  climateCovariates <- na.omit(climateCovariates) |> as.data.table()

  setnames(climateCovariates, new = c("pixelID", names(spreadClimate)))
  spreadCovariates <- climateCovariates[spreadCovariates, on = c("pixelID")]

  # spreadCovariates <- makeMutuallyExclusive(dt = spreadCovariates,
  #                                     mutuallyExclusive = list("youngAge" = exclusiveCols))

  nonFuelNames <- c("pixelID", names(spreadClimate), "youngAge")
  setcolorder(spreadCovariates, neworder = nonFuelNames)
  sim$fireSense_SpreadCovariates <- spreadCovariates

  # Sanity check - make sure the nonForest pixels have no forest fuels
  nfCols <- setdiff(names(sim$landcoverDT), "pixelID")
  df <- sim$fireSense_SpreadCovariates
  fcs <- setdiff(colnames(df), c(nonFuelNames, nfCols))
  df[df - min(df[[fcs[[1]]]], na.rm = TRUE) == 0] <- NA
  # df[df - df[[fcs[[1]]]][1] == 0] <- NA
  fuelInSamePixelAsNonForest <- any(rowSums(!is.na(df[, ..fcs])) > 0 & 
                                      (rowSums(df[, ..nfCols]) > 0))
  
  if (fuelInSamePixelAsNonForest)
    stop("Flammable fuels (i.e., trees) are present in pixels that are identified as non-fore")
  
  return(invisible(sim))
}


.inputObjects <- function(sim) {
  cacheTags <- c(currentModule(sim), "otherFunctions:.inputObjects")
  dPath <- asPath(inputPath(sim), 1)
  message(currentModule(sim), ": using dataPath '", dPath, "'.")

  # objectSyns <- list(c("standAgeMap", "standAgeMap2011"),
  #                    c("rstLCC", "rstLCC2011"))
  # sim <- objectSynonyms(sim, objectSyns)

  if (!suppliedElsewhere("climateVariablesForFire", sim)) {
    sim$climateVariablesForFire <- list(
      spread = "MDC",
      ignition = "MDC"
    )
  }

  # if (!suppliedElsewhere("rstLCC", sim)) {
    if (suppliedElsewhere("rstLCCs", sim)) {
      sim$rstLCC <- tail(sim$rstLCCs, 1)[[1]]
    } else {
      rstLCC <- Cache(makeFireSenseLCC,
                      neededYear = P(sim)$dataYear,
                      writeTo = .suffix(
                        "rstLCC.tif",
                        paste0(P(sim)$dataYear, "_", P(sim)$.studyAreaName)
                      ),
                      destinationPath = inputPath(sim),
                      studyArea = sim$studyArea,
                      rasterToMatch = sim$rasterToMatch,
                      overwrite=  TRUE,
                      nonflammableLCC = P(sim)$nonflammableLCC,
                      flammabilityThreshold = P(sim)$flammabilityThreshold,
                      userTags = c("makeFireSenseLCC", "predict")
      )
      sim$rstLCC <- rstLCC$lcc
      sim$propFlammable <- rstLCC$flammableProp
    }

  # }

  if (!suppliedElsewhere("standAgeMap", sim)) {
    # if (suppliedElsewhere("standAgeMaps", sim)) {
    #   sim$standAgeMap <- tail(sim$standAgeMaps, 1)[[1]]
    # } else {
      sim$standAgeMap <- Cache(prepInputsStandAgeMap,
                               rasterToMatch = sim$rasterToMatch,
                               studyArea = sim$studyArea,
                               destinationPath = dPath,
                               startTime = P(sim)$dataYear,
                               userTags = c(cacheTags, "prepInputsStandAgeMap2011"))
    # }
  }

  if (!suppliedElsewhere("fireSense_IgnitionFitted", sim)) {
    if ("fireSense_IgnitionFit" %in% P(sim)$whichModulesToPrepare) {
      stop("please supply fireSense_IgnitionFitted object")
    }
  }
  
  if (!suppliedElsewhere("flammableRTM", sim)) {
    sim$flammableRTM <- defineFlammable(rstLCC,
                                        nonFlammClasses = P(sim)$nonflammableLCC,
                                        to = sim$rasterToMatch)
    
  }
  
  if (!suppliedElsewhere("landcoverDT", sim)) {
    sim$landcoverDT <- makeLandcoverDT(
      rstLCC = sim$rstLCC,
      flammableRTM = sim$flammableRTM,
      forestedLCC = P(sim)$forestedLCC,
      nonForestedLCCGroups = sim$nonForestedLCCGroups
    )
  }
  

  return(invisible(sim))
}
