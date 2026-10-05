defineModule(sim, list(
  name = "fireSense_dataPrepPredict",
  description = paste(
    "Prepares, each year, the covariate tables used by fireSense_ignitionPredict",
    "(ignition and escape) and fireSense_spreadPredict."),
  keywords = "",
  authors = c(
    person("Ian", "Eddy", role = c("aut", "cre"), email = "ian.eddy@nrcan-rncan.gc.ca"),
    person("Eliot", "McIntire", role = "aut", email = "eliot.mcintire@nrcan-rncan.gc.ca"),
    person("Alex M", "Chubaty", role = "ctb", email = "achubaty@for-cast.ca")
  ),
  childModules = character(0),
  version = list(fireSense_dataPrepPredict = "1.0.4.9011"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = deparse(list("README.txt", "fireSense_dataPrepPredict.Rmd")),
  loadOrder = list(after = c("Biomass_borealDataPrep", "fireSense_dataPrepFit",
                             "fireSense_ignitionFit", "fireSense_spreadFit")),
  reqdPkgs = list(
    "data.table",
    "PredictiveEcology/fireSenseUtils@development (>= 0.2.3.9081)",
    "PredictiveEcology/LandR@development",
    "PredictiveEcology/reproducible@development",
    "PredictiveEcology/SpaDES.core@development (>= 3.0.4)",
    "terra"
  ),
  parameters = rbind(
    defineParameter("cutoffForYoungAge", "numeric", fireSenseUtils::fireSenseYoungAgeCutoff, NA, NA,
      desc = paste(
        "Age at and below which pixels are considered 'young'",
        "(i.e., `age <= cutoffForYoungAge`)."
      )
    ),
    defineParameter("dataYear", "numeric", 2011, 1985, 2022,
                    paste("Year of the default landcover and stand age maps, and last year of the fires",
                          "used to initialise `nonForest_timeSinceDisturbance`.")),
    defineParameter("fireTimeStep", "numeric", 1, NA, NA, desc = "Interval between events of this module, in years."),
    defineParameter("forestedLCC", "numeric", fireSenseUtils::fireSenseForestedLCC, NA, NA,
                    "Forested landcover classes in `rstLCC_RTM`. Only used if `landcoverDT` is not supplied."),
    defineParameter("flammabilityThreshold", "numeric", fireSenseUtils::fireSenseFlammabilityThreshold, 0, 1,
      paste("Minimum proportion of flammable pixels for an upscaled pixel to be flammable,",
            "when building the default landcover.")),
    defineParameter("fuelClassCol", "character", fireSenseUtils::fireSenseFuelClassCol, NA, NA,
                    "Column of `sppEquiv` that defines the fuel classes, for both ignition and spread."),
    defineParameter("igAggFactor", "numeric", fireSenseUtils::fireSenseIgAggFactor, 1, NA,
                    paste("Aggregation factor for the ignition and escape covariates.",
                          "Overwritten in `init` by the value set in other modules.")),
    defineParameter("nonflammableLCC", "numeric", fireSenseUtils::fireSenseNonflammableLCC, NA, NA,
      desc = paste(
        "Non-flammable landcover classes, used to create `flammableRTM` and the default landcover",
        "if not supplied. The default, `fireSenseUtils::fireSenseNonflammableLCC`, is no data,",
        "water, rock, snow/ice and barren land."
      )
    ),
    defineParameter("nonForestCanBeYoungAge", "logical", fireSenseUtils::fireSenseNonForestCanBeYoungAge, NA, NA,
                    desc = "Should burned non-forest pixels be `youngAge` until `cutoffForYoungAge`?"),
    defineParameter("scanfiVersion", "character", fireSenseUtils::fireSenseSCANFIVersion, NA, NA,
                    desc = paste(
                      "SCANFI land-cover version for non-forest land cover, when this module builds its own",
                      "landcover (only used if `rstLCC_RTM`/`rstLCCs` is not supplied)."
                    )),
    defineParameter("sppEquivCol", "character", "LandR", NA, NA,
                    desc = "Column of `sppEquiv` with the species names used in `cohortData`."),
    defineParameter("whichModulesToPrepare", "character",
                    default = c("fireSense_spreadPredict", "fireSense_ignitionPredict"),
                    NA, NA,
                    desc = paste("Predict modules to prepare covariates for: `fireSense_ignitionPredict` for the",
                                 "ignition/escape table, `fireSense_spreadPredict` for the spread table.",
                                 "Defaults to both.")),
    defineParameter(
      ".runInitialTime", "numeric", start(sim), NA, NA, "Time of the first climate and covariate preparation events."
    ),
    defineParameter(".studyAreaName", "character", NA, NA, NA,
                    "Human-readable name for the study area used."),
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
    expectsInput("climateVariablesForFire", "list", sourceURL = NA,
      desc = paste(
        "Named list (`ignition`, `spread`) of the layer names in `currentClimateRasters`",
        "used by each process. A length-one list is used for both. Default is 'MDC' for both.")),
    expectsInput("climateYear", "numeric", sourceURL = NA,
      desc = paste(
        "Optional year (e.g. 2009) used instead of `time(sim)` to build `currentClimateRasters`.",
        "See PredictiveEcology/climateYear.")),
    expectsInput("currentClimateRasters", "SpatRaster", sourceURL = NA,
      desc = paste(
        "Climate layers for the current year, one layer per climate variable.",
        "If absent, built from `projectedClimateRasters`.")),
    expectsInput("cohortData", "data.table", sourceURL = NA,
      desc = "Cohorts by `pixelGroup` (LandR)."),
    expectsInput("missingLCCgroup", "character", sourceURL = NA,
      desc = paste(
        "Forested pixels that are absent from `cohortData` are assigned to this class.",
        "Must be one of the names of `nonForestedLCCGroups`.")),
    expectsInput("flammableRTM", "SpatRaster", sourceURL = NA,
      desc = paste(
        "Binary raster of flammable pixels at `start(sim)`.",
        "If absent, derived from the default landcover and `nonflammableLCC`.")),
    expectsInput("nonForestedLCCGroups", "list", sourceURL = NA,
      desc = paste(
        "Named list of non-forested landcover groups, e.g. `list('wetland' = c(19, 23, 32))`.",
        "Only used if `landcoverDT` is not supplied.")),
    expectsInput("lightningMaps", "SpatRaster", sourceURL = NA,
      desc = paste(
        "Lightning layers: lightningDays, lightningDensity, positiveCG, positiveCGdensity.",
        "Only `lightningDays` is used.")),
    expectsInput("pixelGroupMap", "SpatRaster", sourceURL = NA,
      desc = "Map of the `pixelGroup`s in `cohortData`."),
    expectsInput("projectedClimateRasters", "list", sourceURL = NA,
      desc = paste(
        "Named list (one element per climate variable) of SpatRasters whose layers are named",
        "'year<year>'. Only used if `currentClimateRasters` is not supplied.")),
    expectsInput("rasterToMatch", "SpatRaster", sourceURL = NA,
      desc = "Template raster for the study area."),
    expectsInput("rstCurrentBurn", "SpatRaster", sourceURL = NA,
      desc = "Binary raster with 1 where the pixel burned this year."),
    expectsInput("rstLCC_RTM", "SpatRaster", sourceURL = NA,
      desc = paste(
        "Landcover raster, only used if `landcoverDT` is not supplied.",
        "Defaults to the last layer of `rstLCCs`, else NTEMS landcover for `dataYear`.")),
    expectsInput("rstLCCs", "list", sourceURL = NA,
      desc = paste(
        "Optional named list of landcover `SpatRaster`s, one per data year, as produced by",
        "`fireSense_dataPrepFit`. If supplied, the last element is used as `rstLCC_RTM`.")),
    expectsInput("sppEquivs", "list", sourceURL = NA,
      desc = paste("Only with several fitted ELFs: one `sppEquiv` per ELF, from `fireSense_dataPrepFit`. Each ELF's",
                   "fuel covariates are made with its own, for every pixel.")),
    expectsInput("nonForestedLCCGroupsList", "list", sourceURL = NA,
      desc = "Only with several fitted ELFs: one `nonForestedLCCGroups` per ELF, in the order of `sppEquivs`."),
    expectsInput("missingLCCgroupList", "list", sourceURL = NA,
      desc = "Only with several fitted ELFs: one `missingLCCgroup` per ELF, in the order of `sppEquivs`."),
    expectsInput("sppEquiv", "data.table", sourceURL = NA,
      desc = "Table of LandR species equivalencies; must have columns `sppEquivCol` and `fuelClassCol`."),
    expectsInput("studyAreaWithSpreadParams", "sf", sourceURL = NA,
      desc = paste("The fitted SpreadFit ledger rows (from `fireSense_ELFs`; also read, undeclared, by",
                   "`fireSense_spreadPredict`), one row per fitted ELF, in the order of `sppEquivs`. Each",
                   "row's `params[[1]]` column names are the fitted formula's terms: an ELF whose terms",
                   "include `dom_agb_<class>`/`sec_agb_<class>` predicts with those classes' AGB columns,",
                   "matching what that ELF was fitted with; otherwise (an older, per-species fit) with the",
                   "previous one-column-per-fuel-class covariates. Unsupplied: every ELF predicts",
                   "per-fuel-class, as before this was read.")),
    expectsInput("standAgeMap", "SpatRaster", sourceURL = NA,
      desc = "Stand age (years) at `start(sim)`."),
    expectsInput("studyArea", "SpatVector", sourceURL = NA,
      desc = paste("Polygon of the study area. The fire polygons, land cover and stand age made in",
                   "`.inputObjects` are masked to it.")),
    expectsInput("landcoverDT", "data.table", sourceURL = NA,
      desc = paste(
        "`pixelID` plus one binary column per non-forest landcover group, for flammable pixels,",
        "at `start(sim)`."))
  ),
  outputObjects = bindrows(
    createsOutput("currentClimateRasters", "SpatRaster",
      desc = paste(
        "Climate layers for the current year, one layer per climate variable.",
        "Built from `projectedClimateRasters` if not supplied.")),
    createsOutput("fireSense_igAndEscapePred_Covariates", "data.table",
      desc = paste(
        "Ignition and escape covariates at the aggregated (`igAggFactor`) resolution;",
        "`pixelID` is the cell index of the aggregated raster.")),
    createsOutput("fireSense_SpreadCovariates", "data.table",
      desc = "Spread covariates; `pixelID` is the cell index of `flammableRTM`."),
    createsOutput("nonForest_timeSinceDisturbance", "SpatRaster",
      desc = "Years since last burn, used to set `youngAge` in non-forest pixels.")
  )
))

doEvent.fireSense_dataPrepPredict <- function(sim, eventTime, eventType) {
  switch(eventType,
    init = {
      if ("fireSense_EscapePredict" %in% P(sim)$whichModulesToPrepare)
        stop("fireSense_EscapePredict no longer exists as a module; escape covariates are prepared with ",
             "fireSense_ignitionPredict. Remove it from parameter whichModulesToPrepare.")
      sim <- Init(sim)
      sim <- scheduleEvent(sim, time(sim) + 1, "fireSense_dataPrepPredict", "ageNonForest")
      sim <- scheduleEvent(sim, P(sim)$.runInitialTime, "fireSense_dataPrepPredict", "getClimateRasters")

      if ("fireSense_ignitionPredict" %in% P(sim)$whichModulesToPrepare) {
        sim <- scheduleEvent(sim, P(sim)$.runInitialTime, "fireSense_dataPrepPredict",
          "prepIgAndEscPredictData"
        )
      }

      if ("fireSense_spreadPredict" %in% P(sim)$whichModulesToPrepare) {
        sim <- scheduleEvent(sim, P(sim)$.runInitialTime, "fireSense_dataPrepPredict", "prepSpreadPredictData"
        )
      }
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
    save = {
      message("fireSense_dataPrepPredict: the save event does nothing")
    },
    warning(paste("Undefined event type: \'", current(sim)[1, "eventType", with = FALSE],
      "\' in module \'", current(sim)[1, "moduleName", with = FALSE], "\'",
      sep = ""
    ))
  )
  return(invisible(sim))
}

#' Initialize the module
#'
#' Aligns `standAgeMap` and `rstLCC_RTM` to `rasterToMatch`, builds `landcoverDT` and
#' `nonForest_timeSinceDisturbance` if absent, and expands `climateVariablesForFire`.
#'
#' @param sim A `simList`.
#'
#' @return The `simList`, invisibly.
Init <- function(sim) {
  sim <- resolveStudyAreaName(sim)

  # force it to be same as the other module's value
  params(sim)[[currentModule(sim)]][["igAggFactor"]] <- SpaDES.core::paramCheckOtherMods(sim, "igAggFactor")
  
  standAgeMap <- if (!LandR::.compareRas(sim$rasterToMatch, sim$standAgeMap, stopOnError = FALSE)) {
    postProcess(sim$standAgeMap, to = sim$rasterToMatch)
  } else {
    sim$standAgeMap
  }
  
  rstLCC <- if (!LandR::.compareRas(sim$rasterToMatch, sim$rstLCC_RTM, stopOnError = FALSE)) {
    ## landcover is categorical: nearest-neighbour, never the bilinear default
    reproducible::postProcess(sim$rstLCC_RTM, to = sim$rasterToMatch, method = "near")
  } else {
    sim$rstLCC_RTM
  }
  
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
                       fun = "sf::st_read",
                       years = fireYears,
                       useInnerCache = FALSE,
                       destinationPath = inputPath(sim),
                       cropTo = sim$rasterToMatch,
                       maskTo = sim$studyArea,
                       projectTo = sim$rasterToMatch)

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

  return(invisible(sim))
}

#' Set `sim$currentClimateRasters` for the current year
#'
#' A supplied `sim$currentClimateRasters` is left alone. If it is absent, or this module built
#' it for another year, takes layer `year<Y>` of each element of `sim$projectedClimateRasters`,
#' where `Y` is `sim$climateYear` if supplied, else `time(sim)`. Stops if the object used does
#' not match `sim$pixelGroupMap`.
#'
#' @param sim A `simList`.
#'
#' @return The `simList`.
getCurrentClimate <- function(sim) {
  ## The climate year actually wanted for this event: `sim$climateYear`, when supplied,
  ## overrides simulation time (see PredictiveEcology/climateYear).
  if (is.null(sim$climateYear)) {
    currentYear <- time(sim)
  } else {
    currentYear <- as.numeric(sim$climateYear)
  }
  currentYear <- as.numeric(currentYear) # drop time(sim)'s "unit" attribute

  ## Ownership rule: a supplied `currentClimateRasters` is left alone; only an object this
  ## module built is refreshed, when the wanted year changes. In the project the `climateYear`
  ## module runs first every year and supplies it (preferring `historicalClimateRasters`), so a
  ## year-only key would discard that and rebuild from `projectedClimateRasters`. An
  ## `is.null()`-only guard is also wrong: the object is a module output, so it persists, and
  ## a standalone run would serve the first year's layers for ever.
  if (is.null(sim$currentClimateRasters) ||
      (isTRUE(mod$builtCurrentClimate) && !isTRUE(mod$currentClimateYear == currentYear))) {
    
    sim$currentClimateRasters <- sim$projectedClimateRasters[[1]]
    if (!compareGeom(sim$pixelGroupMap, sim$currentClimateRasters, stopOnError = FALSE)) {
      stop("mismatch in resolution detected - please review the resolution of sim$projectedClimateRasters")
    }
    availableYears <- as.numeric(gsub(
      pattern = "year",
      x = names(sim$projectedClimateRasters[[1]]),
      replacement = ""
    ))
    
    
    if (currentYear > max(availableYears)) {
      cutoff <- quantile(availableYears, probs = 0.9)
      time <- sample(availableYears[availableYears >= cutoff], size = 1)
      message(paste0("re-using projected climate layers from ", time))
    }
    ## this will work with a list of raster stacks
    thisYearsClimate <- lapply(sim$projectedClimateRasters,
                               FUN = function(x, rtm = sim$rasterToMatch) {
                                 ras <- x[[paste0("year", currentYear)]]
                                 if (!compareGeom(ras, rtm, stopOnError = FALSE)) {
                                   message("reprojecting fireSense climate layers")
                                   ras <- postProcess(ras, rasterToMatch = rtm)
                                 }
                                 return(ras)
                               }
    )
    
    sim$currentClimateRasters <- terra::rast(thisYearsClimate)
    mod$builtCurrentClimate <- TRUE
    mod$currentClimateYear <- currentYear
    
  } 
  if (!compareGeom(sim$pixelGroupMap, sim$currentClimateRasters, stopOnError = FALSE)) {
    stop("mismatch in resolution detected - please review the resolution of sim$projectedClimateRasters")
  }
  
  
  return(sim)
}

#' Age the time-since-disturbance raster by one year
#'
#' @param TSD `SpatRaster` of years since last burn.
#' @param rstCurrentBurn `SpatRaster` with 1 where the pixel burned this year, or `NULL`.
#'   Burned pixels are reset to 0.
#' @param timeStep Not used; `TSD` always increases by 1.
#'
#' @return `TSD`, updated.
ageNonForest <- function(TSD, rstCurrentBurn, timeStep) {
  TSDvals <- as.vector(TSD)
  TSDvals <- TSDvals + 1
  if (!is.null(rstCurrentBurn)) {
    burnVals <- as.vector(rstCurrentBurn)
    unburned <- is.na(burnVals) | burnVals == 0
    TSDvals[!unburned] <- 0
    rm(burnVals, unburned)
  }
  TSD <- setValues(TSD, TSDvals)
  return(TSD)
}

#' Build this year's ignition and escape covariates
#'
#' Creates `sim$fireSense_igAndEscapePred_Covariates` from fuel classes, non-forest landcover,
#' ignition climate and lightning days, aggregated by `igAggFactor`.
#'
#' @param sim A `simList`.
#'
#' @return The `simList`, invisibly.
prepare_IgnitionAndEscapePredict <- function(sim) {
  ## get climate
  ignitionClimate <- sim$currentClimateRasters[[sim$climateVariablesForFire$ignition]]
  if (is.null(ignitionClimate))
    stop("ignitionClimate is NULL; there is a problem to debug")
  
  # Coming out of the CacheGeo, this is unreliably a data.frame instead of a data.table
  if (!data.table::is.data.table(sim$sppEquiv)) data.table::setDT(sim$sppEquiv)
  ## one fuel set per fitted ELF (one, as before, when there is one ELF); the covariate tables are merged,
  ## each ELF's columns alongside the others', for fireSense_ignitionPredict to pick its own
  fuelSets <- ELFfuelSets(sim)
  fuelClassTables <- fuelClassTablesThisYear(sim, fuelSets)
  fuelCovsCoarse <- mergeCovariateTables(lapply(seq_along(fuelSets), function(i) {
    fs <- fuelSets[[i]]
    prepare_FuelCovsCoarse(
    fuelClassTable = fuelClassTables[[i]],
    cohortData = sim$cohortData,
    pixelGroupMap = sim$pixelGroupMap,
    flammableRTM = sim$flammableRTM,
    landcoverDT = fs$landcoverDT,
    nonForest_timeSinceDisturbance = sim$nonForest_timeSinceDisturbance,
    sppEquiv = fs$sppEquiv,
    sppEquivCol = P(sim)$sppEquivCol,
    fuelClassCol = P(sim)$fuelClassCol,
    requiredFuelClasses = fs$requiredFuelClasses,
    cutoffForYoungAge = P(sim)$cutoffForYoungAge,
    missingLCCgroup = fs$missingLCCgroup,
    nonForestedLCCGroups = fs$nonForestedLCCGroups,
    nonForestCanBeYoungAge = P(sim)$nonForestCanBeYoungAge,
    studyAreaName = P(sim)$.studyAreaName,
    rasTemplate = sim$flammableRTM, fact = Par$igAggFactor,
    useCache = FALSE
  )}))

  ignitionClimateCoarse <- prepare_ignitionClimate(
    ignitionClimateList = as.list(ignitionClimate) |> setNames(names(ignitionClimate)), 
    fact = P(sim)$igAggFactor,
    useCache = FALSE)
  
  yrLab <- paste0("year", time(sim))
  ignitionClimateCoarseList <- Map(r = ignitionClimateCoarse, function(r) r |> setNames(yrLab))
  sim$fireSense_igAndEscapePred_Covariates <- 
    mergePreparedCovs(years = list(yrLab) |> setNames(time(sim)), 
                      list(fuelCovsCoarse) |> setNames(yrLab), 
                      ignitionFirePoints = NULL, 
                      unionLCCGroups(fuelSets),
                      ignitionClimateCoarseList, 
                      sim$lightningMaps["lightningDays"], 
                      digest = append(NULL, list(P(sim)$igAggFactor)),
                      useCache = FALSE)
  
  set(sim$fireSense_igAndEscapePred_Covariates, NULL, "ignitions", NULL)
  return(invisible(sim))
}

#' Build this year's spread covariates
#'
#' Creates `sim$fireSense_SpreadCovariates` from fuel classes, non-forest landcover and
#' spread climate, at the resolution of `flammableRTM`. Stops if a non-forest pixel has forest fuel.
#'
#' @param sim A `simList`.
#'
#' @return The `simList`, invisibly.
prepare_SpreadPredict <- function(sim) {
  spreadClimate <- sim$currentClimateRasters[[sim$climateVariablesForFire$spread]]
  if (is.null(spreadClimate))
    stop("spreadClimate is NULL; there is a problem to debug")

  ## one fuel set per fitted ELF (one, as before, when there is one ELF). Every ELF's covariates are made for
  ## every pixel and the tables merged, so fireSense_spreadPredict can apply each ELF's model wherever it
  ## predicts, including the blend zone around its own pixels. Column names say what they hold (fuel class,
  ## non-forest LCC codes), so a column two ELFs share means the same thing in both.
  fuelSets <- ELFfuelSets(sim)
  fuelClassTables <- fuelClassTablesThisYear(sim, fuelSets)
  spreadCovariates <- mergeCovariateTables(lapply(seq_along(fuelSets), function(i) {
    fs <- fuelSets[[i]]
    ## this fits cohortData into fuel classes
    ##  if pixels are missing/absent but are able to be forested as determined by landcoverDT,
    ##  they receive 0 values - e.g. pixelGroup zero
    ## fs$fuelClassRoles is read from this ELF's fitted parameter names (fuelClassRolesForELF(),
    ## from sim$studyAreaWithSpreadParams) -- domClass/secClass are forced here, never re-derived
    ## from this prediction area, so an ELF predicts with the same dom_agb_*/sec_agb_* columns
    ## its fit used even where a different class dominates here. domClass = NA (a fit made with
    ## fuelCovariates = "species", or with no fitted parameters yet) predicts with the previous
    ## per-fuel-class columns.
    covs <- fireSenseUtils::fireSenseCovariatesCreate(
      fuelClassTable = fuelClassTables[[i]],
      cohortData = sim$cohortData,
      pixelGroupMap = sim$pixelGroupMap,
      flammableRTM = sim$flammableRTM,
      landcoverDT = fs$landcoverDT,
      rstLCC = fs$rstLCC,

      sppEquiv = fs$sppEquiv,
      sppEquivCol = P(sim)$sppEquivCol,
      fuelClassCol = P(sim)$fuelClassCol,
      requiredFuelClasses = fs$requiredFuelClasses,
      cutoffForYoungAge = P(sim)$cutoffForYoungAge,
      missingLCCgroup = fs$missingLCCgroup,
      nonForestedLCCGroups = fs$nonForestedLCCGroups,
      nonForestCanBeYoungAge = P(sim)$nonForestCanBeYoungAge,
      nonForest_timeSinceDisturbance = sim$nonForest_timeSinceDisturbance,
      studyAreaName = P(sim)$.studyAreaName,
      useCache = FALSE, # predict is annual, no point in caching
      fuelCovariates = if (is.na(fs$fuelClassRoles$domClass)) "species" else "domSecWetland",
      domClass = fs$fuelClassRoles$domClass,
      secClass = fs$fuelClassRoles$secClass,
      treedWetland = fs$fuelClassRoles$treedWetland
    )
    # Sanity check - make sure the nonForest pixels have no forest fuels
    nfCols <- setdiff(names(fs$landcoverDT), "pixelID")
    df <- copy(covs)
    ## treedWetland (the species-mode 0/1 indicator) is not on the logMinB() floor scale the other
    ## fuel columns are, so it is not "fuel" for this check -- otherwise its 0s (never NA below)
    ## keep every row's rowSums(!is.na(fcs)) > 0, flagging every non-forest pixel as if it had forest
    ## fuel. treedWetland_agb (domSecWetland mode) IS on that scale (fireSenseUtils::fireSenseCovariatesCreate()
    ## logs it with logMinB() like dom_agb_* and sec_agb_*), but the pooled treed-wetland biomass is
    ## also left out of this check
    fcs <- setdiff(colnames(df), c("pixelID", "youngAge", nfCols,
                                    fireSenseUtils::treedWetlandTxt, fireSenseUtils::treedWetlandAgbTxt))
    if (length(fcs)) {
      df[df - min(df[[fcs[[1]]]], na.rm = TRUE) == 0] <- NA
      fuelInSamePixelAsNonForest <- any(rowSums(!is.na(df[, ..fcs])) > 0 &
                                          (rowSums(df[, ..nfCols]) > 0))
      if (fuelInSamePixelAsNonForest)
        stop("Flammable fuels (i.e., trees) are present in pixels that are identified as non-fore")
    }
    covs
  }))

  # TODO: this chunk is untested 18/12/2024
  climateCovariates <- spreadClimate |> as.data.frame(cells = TRUE)
  climateCovariates <- na.omit(climateCovariates) |> as.data.table()

  setnames(climateCovariates, new = c("pixelID", names(spreadClimate)))
  spreadCovariates <- climateCovariates[spreadCovariates, on = c("pixelID")]

  nonFuelNames <- c("pixelID", names(spreadClimate), "youngAge")
  setcolorder(spreadCovariates, neworder = nonFuelNames)
  sim$fireSense_SpreadCovariates <- spreadCovariates
  
  return(invisible(sim))
}


#' The fuel-class tables for this year, one per fuel set, shared by the ignition and spread covariates
#'
#' `fireSenseUtils::fireSenseCovariatesCreate()` is called for ignition (through
#' `prepare_FuelCovsCoarse()`) and for spread with the same `cohortData`, `pixelGroupMap`,
#' `flammableRTM`, `landcoverDT`, `sppEquiv`, `requiredFuelClasses` and `cutoffForYoungAge`; they differ
#' only in `rstLCC`, `fuelCovariates` and `domClass`/`secClass`, which act after the fuel classes are
#' made. So the fuel classes (`fireSenseUtils::cohortsToFuelClasses(asTable = TRUE)`) are made once per
#' year: by whichever of the two events runs first, kept in `mod` when both are being prepared, and used
#' (and dropped) by the second. The second event uses them only if `cohortData`, `pixelGroupMap` and
#' `flammableRTM` still sum to what they did for the first -- another module's event may run between the
#' two -- and otherwise builds its own, as before.
#'
#' @param sim A `simList`.
#' @param fuelSets as returned by `ELFfuelSets()`.
#'
#' @return list of `data.table`s, one per element of `fuelSets`.
fuelClassTablesThisYear <- function(sim, fuelSets) {
  year <- time(sim)
  stamp <- function() {
    cd <- sim$cohortData
    list(NROW(cd), sum(as.numeric(cd$B), na.rm = TRUE), sum(as.numeric(cd$age), na.rm = TRUE),
         sum(as.numeric(cd$pixelGroup), na.rm = TRUE),
         sum(terra::values(sim$pixelGroupMap, mat = FALSE), na.rm = TRUE),
         sum(terra::values(sim$flammableRTM, mat = FALSE), na.rm = TRUE))
  }
  make <- function() lapply(fuelSets, function(fs) fireSenseUtils::cohortsToFuelClasses(
    cohortData = sim$cohortData,
    pixelGroupMap = sim$pixelGroupMap,
    flammableRTM = sim$flammableRTM,
    landcoverDT = fs$landcoverDT,
    sppEquiv = fs$sppEquiv,
    sppEquivCol = P(sim)$sppEquivCol,
    fuelClassCol = P(sim)$fuelClassCol,
    requiredFuelClasses = fs$requiredFuelClasses,
    cutoffForYoungAge = P(sim)$cutoffForYoungAge,
    asTable = TRUE
  ))
  both <- all(c("fireSense_ignitionPredict", "fireSense_spreadPredict") %in% P(sim)$whichModulesToPrepare)
  if (!both) return(make())

  kept <- mod$fuelClassTables
  if (!is.null(kept) && identical(kept$year, year)) { # the second event of this year
    mod$fuelClassTables <- NULL
    if (identical(kept$stamp, stamp())) return(kept$tables)
    return(make())
  }
  tables <- make()
  mod$fuelClassTables <- list(year = year, stamp = stamp(), tables = tables)
  tables
}

#' The fuel sets to build covariates with: one per fitted ELF
#'
#' With several fitted ELFs, `fireSense_dataPrepFit` supplies one species table, non-forest grouping and
#' missing-LCC group per ELF (`sppEquivs`, `nonForestedLCCGroupsList`, `missingLCCgroupList`); each gets its
#' own `landcoverDT`, made once and kept in `mod`. This holds for one ELF too whenever the per-ELF lists are
#' present (a predict-only run has only the module defaults in `nonForestedLCCGroups`); the single set of
#' objects is used only when they are absent.
#'
#' @param sim A `simList`.
#' @return list of lists, each with `sppEquiv`, `nonForestedLCCGroups`, `missingLCCgroup`, `landcoverDT`,
#'   `requiredFuelClasses`, `rstLCC` (aligned to `flammableRTM`, for `treedWetland`/`treedWetland_agb`) and
#'   `fuelClassRoles` (`list(domClass =, secClass =, treedWetland =)`; `domClass = NA` predicts with
#'   per-fuel-class columns).

#' The fuel-class name after `dom_agb_`/`sec_agb_` in a fitted covariate name
#'
#' `fireSenseUtils::fireSenseCovariatesCreate(fuelCovariates = "domSecWetland")` names those columns
#' `dom_agb_<domClass>`/`sec_agb_<secClass>` with the fuel class's own name unchanged (no further
#' mangling), so recovering `domClass`/`secClass` from a fitted term name is stripping the prefix.
#'
#' @param termNames character vector, e.g. `colnames(sim$studyAreaWithSpreadParams$params[[1]])`.
#' @return `list(domClass =, secClass =, treedWetland =)`; `domClass`, `secClass` both `NA` when
#'   `termNames` has no `dom_agb_*` term (an older, per-species fit -- predicts with the previous
#'   one-column-per-fuel-class covariates). `treedWetland` is whether the fit has a treed-wetland term
#'   (`treedWetland_agb`, or an older fit's `treedWetland`): `fireSense_dataPrepFit` leaves it out of an
#'   ELF with too little treed wetland (`minCovariateProp`), and its AGB is then ordinary fuel.
#'   Stops when `termNames` has `other_agb` (a fit made before that covariate was removed).
fuelClassRolesFromTermNames <- function(termNames) {
  if ("other_agb" %in% termNames)
    stop("fireSense_dataPrepPredict: the fitted model has an `other_agb` term, a fuel covariate that no longer exists ",
         "(fuels are now dom_agb_<class>, sec_agb_<class> and treedWetland_agb). Refit the spread model with the current ",
         "fireSense_dataPrepFit and fireSense_spreadFit.")
  domTerm <- grep("^dom_agb_", termNames, value = TRUE)
  secTerm <- grep("^sec_agb_", termNames, value = TRUE)
  treedWetland <- any(c(fireSenseUtils::treedWetlandAgbTxt, fireSenseUtils::treedWetlandTxt) %in% termNames)
  if (!length(domTerm))
    return(list(domClass = NA_character_, secClass = NA_character_, treedWetland = treedWetland))
  list(domClass = sub("^dom_agb_", "", domTerm[1]),
       secClass = if (length(secTerm)) sub("^sec_agb_", "", secTerm[1]) else NA_character_,
       treedWetland = treedWetland)
}

#' `fuelClassRoles` for one row of `sim$studyAreaWithSpreadParams`
#'
#' @param sim A `simList`.
#' @param i integer, the row (ELF), in the order of `sppEquivs` -- the same order
#'   `fireSense_spreadPredict::spreadPredictRun()` indexes `sa$params[[i]]` by.
#' @return `list(domClass =, secClass =, treedWetland =)`, from [fuelClassRolesFromTermNames()]; `NA`, `NA`, `TRUE` when
#'   `studyAreaWithSpreadParams` is absent, too short, or that ELF has no fitted parameters yet.
fuelClassRolesForELF <- function(sim, i = 1L) {
  sa <- sim$studyAreaWithSpreadParams
  noRoles <- list(domClass = NA_character_, secClass = NA_character_, treedWetland = TRUE)
  if (is.null(sa) || NROW(sa) < i) return(noRoles)
  p <- sa$params[[i]]
  if (is.null(p) || !NROW(p)) return(noRoles)
  fuelClassRolesFromTermNames(colnames(p))
}

ELFfuelSets <- function(sim) {
  fcc <- P(sim)$fuelClassCol
  if (length(sim$sppEquivs) > 1L || length(sim$nonForestedLCCGroupsList)) {
    n <- length(sim$sppEquivs)
    if (length(sim$nonForestedLCCGroupsList) != n || length(sim$missingLCCgroupList) != n)
      stop("fireSense_dataPrepPredict: sppEquivs, nonForestedLCCGroupsList and missingLCCgroupList must have one ",
           "element per ELF")
    if (is.null(mod$ELFrstLCC)) {
      mod$ELFrstLCC <- if (!LandR::.compareRas(sim$flammableRTM, sim$rstLCC_RTM, stopOnError = FALSE))
        reproducible::postProcess(sim$rstLCC_RTM, to = sim$flammableRTM, method = "near") else sim$rstLCC_RTM
    }
    if (is.null(mod$ELFlandcoverDTs)) {
      mod$ELFlandcoverDTs <- lapply(sim$nonForestedLCCGroupsList, function(g)
        makeLandcoverDT(rstLCC = mod$ELFrstLCC, flammableRTM = sim$flammableRTM,
                        forestedLCC = P(sim)$forestedLCC, nonForestedLCCGroups = g))
    }
    return(lapply(seq_len(n), function(i) {
      se <- data.table::as.data.table(sim$sppEquivs[[i]])
      list(sppEquiv = se, nonForestedLCCGroups = sim$nonForestedLCCGroupsList[[i]],
           missingLCCgroup = sim$missingLCCgroupList[[i]], landcoverDT = mod$ELFlandcoverDTs[[i]],
           requiredFuelClasses = se[[fcc]], rstLCC = mod$ELFrstLCC,
           fuelClassRoles = fuelClassRolesForELF(sim, i))
    }))
  }
  if (!data.table::is.data.table(sim$sppEquiv)) data.table::setDT(sim$sppEquiv)
  if (is.null(mod$requiredFuelClasses))
    mod$requiredFuelClasses <- sim$sppEquiv[[fcc]]
  if (is.null(mod$ELFrstLCC)) {
    mod$ELFrstLCC <- if (!LandR::.compareRas(sim$flammableRTM, sim$rstLCC_RTM, stopOnError = FALSE))
      reproducible::postProcess(sim$rstLCC_RTM, to = sim$flammableRTM, method = "near") else sim$rstLCC_RTM
  }
  list(list(sppEquiv = sim$sppEquiv, nonForestedLCCGroups = sim$nonForestedLCCGroups,
            missingLCCgroup = sim$missingLCCgroup, landcoverDT = sim$landcoverDT,
            requiredFuelClasses = mod$requiredFuelClasses, rstLCC = mod$ELFrstLCC,
            fuelClassRoles = fuelClassRolesForELF(sim, 1L)))
}

#' Merge the covariates of several ELFs
#'
#' One ELF: its covariates, untouched. Several: tables are merged by `pixelID` keeping every pixel, and
#' rasters by adding the layers the first lacks; a column or layer the ELFs share (same name, so the same
#' content) is taken from the first.
#'
#' @param tabs list, one element per ELF: `data.table`s with `pixelID`, or `SpatRaster`s.
#' @return one `data.table` or `SpatRaster`.
mergeCovariateTables <- function(tabs) {
  if (length(tabs) == 1L) return(tabs[[1]])
  if (inherits(tabs[[1]], "SpatRaster"))
    return(Reduce(function(a, b) {
      extra <- setdiff(names(b), names(a))
      if (length(extra)) c(a, b[[extra]]) else a
    }, tabs))
  tabs <- lapply(tabs, data.table::as.data.table)
  Reduce(function(a, b) {
    extra <- setdiff(names(b), names(a))
    if (!length(extra)) return(a)
    merge(a, b[, c("pixelID", extra), with = FALSE], by = "pixelID", all = TRUE)
  }, tabs)
}

## the ELFs' non-forest groups together (a group two ELFs share has the same name and codes); one ELF: its own
unionLCCGroups <- function(fuelSets) {
  if (length(fuelSets) == 1L) return(fuelSets[[1]]$nonForestedLCCGroups)
  g <- do.call(c, lapply(fuelSets, `[[`, "nonForestedLCCGroups"))
  g[!duplicated(names(g))]
}

#' Supply default inputs
#'
#' Defaults for `climateVariablesForFire`, `rstLCC_RTM`, `standAgeMap`,
#' `flammableRTM` and `landcoverDT`.
#'
#' @param sim A `simList`.
#'
#' @return The `simList`, invisibly.
.inputObjects <- function(sim) {
  cacheTags <- c(currentModule(sim), "otherFunctions:.inputObjects")
  dPath <- asPath(inputPath(sim), 1)
  message(currentModule(sim), ": using dataPath '", dPath, "'.")

  if (!suppliedElsewhere("climateVariablesForFire", sim)) {
    sim$climateVariablesForFire <- list(
      spread = "MDC",
      ignition = "MDC"
    )
  }

  if (!suppliedElsewhere("rstLCC_RTM", sim)) {
    if (suppliedElsewhere("rstLCCs", sim)) {
      sim$rstLCC_RTM <- tail(sim$rstLCCs, 1)[[1]]
    } else {
      sim <- resolveStudyAreaName(sim)
      rstLCC <- Cache(makeFireSenseLCC,
                      neededYear = P(sim)$dataYear,
                      writeTo = .suffix(
                        "rstLCC.tif",
                        paste0(P(sim)$dataYear, "_", P(sim)$.studyAreaName)
                      ),
                      destinationPath = inputPath(sim),
                      maskTo = sim$studyArea,
                      to = sim$rasterToMatch,
                      overwrite=  TRUE,
                      nonflammableLCC = P(sim)$nonflammableLCC,
                      flammabilityThreshold = P(sim)$flammabilityThreshold,
                      scanfiVersion = P(sim)$scanfiVersion,
                      userTags = c("makeFireSenseLCC", "predict")
      )
      sim$rstLCC_RTM <- rstLCC$lcc
    }
  }

  if (!suppliedElsewhere("standAgeMap", sim)) {
      sim$standAgeMap <- Cache(prepInputsStandAgeMap,
                               rasterToMatch = sim$rasterToMatch,
                               studyArea = sim$studyArea,
                               destinationPath = dPath,
                               startTime = P(sim)$dataYear,
                               userTags = c(cacheTags, "prepInputsStandAgeMap2011"))
  }

  if (!suppliedElsewhere("flammableRTM", sim)) {
    ## `rstLCC` only exists when the landcover was built above; otherwise derive it from
    ## `rstLCC_RTM`, the same way `Init()` does.
    rstLCCforFlammable <- if (!LandR::.compareRas(sim$rasterToMatch, sim$rstLCC_RTM,
                                                 stopOnError = FALSE)) {
      reproducible::postProcess(sim$rstLCC_RTM, to = sim$rasterToMatch, method = "near")
    } else {
      sim$rstLCC_RTM
    }
    sim$flammableRTM <- LandR::defineFlammable(rstLCCforFlammable,
                                        nonFlammClasses = P(sim)$nonflammableLCC,
                                        to = sim$rasterToMatch)
    
  }
  
  if (!suppliedElsewhere("landcoverDT", sim)) {
    sim$landcoverDT <- makeLandcoverDT(
      rstLCC = sim$rstLCC_RTM,
      flammableRTM = sim$flammableRTM,
      forestedLCC = P(sim)$forestedLCC,
      nonForestedLCCGroups = sim$nonForestedLCCGroups
    )
  }

  return(invisible(sim))
}

## An NA `.studyAreaName` becomes a hash of `studyArea`, as in Biomass_borealDataPrep. Without a
## `studyArea` it stays NA (see PredictiveEcology/LandR#246).
resolveStudyAreaName <- function(sim) {
  if (is.na(P(sim)$.studyAreaName) && !is.null(sim$studyArea)) {
    params(sim)[[currentModule(sim)]][[".studyAreaName"]] <- reproducible::studyAreaName(sim$studyArea)
    message("The .studyAreaName is not supplied; derived name from the study area: ",
            params(sim)[[currentModule(sim)]][[".studyAreaName"]])
  }
  sim
}
