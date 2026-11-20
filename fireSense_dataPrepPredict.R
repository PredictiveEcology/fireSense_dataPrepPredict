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
  version = list(fireSense_dataPrepPredict = "1.1.0.9013"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = deparse(list("README.txt", "fireSense_dataPrepPredict.Rmd")),
  loadOrder = list(after = c("Biomass_borealDataPrep", "fireSense_dataPrepFit",
                             "fireSense_ignitionFit", "fireSense_spreadFit")),
  reqdPkgs = list(
    "PredictiveEcology/climateData@development", # latestHistoricalYear()
    "data.table",
    "googledrive",
    "PredictiveEcology/fireSenseUtils@development (>= 0.2.3.9183)", # 9183: addCoreFraction(), coreFractionByYear(); 9142: readSpreadFitLedger() and the other predict-only helpers (#206); fuelCovsToCoarse() hands back the static it used (#191)
    "httr2",
    "PredictiveEcology/LandR@development",
    "PredictiveEcology/reproducible@development",
    "PredictiveEcology/SpaDES.core@development (>= 3.0.4)",
    "qs2",
    "rlang",
    "sf",
    "terra"
  ),
  parameters = rbind(
    defineParameter("blendNeighbourELFs", "logical", TRUE, NA, NA,
      desc = paste(
        "`sim$studyAreaWithSpreadParams` holds every ledger row whose polygon touches the study area: the",
        "simulated ELF (`sim$.ELFind`) and its neighbours. `FALSE` keeps only the simulated ELF's row.",
        "`TRUE` also keeps each neighbour that `fireSense_spreadPredict` can blend: fitted with the current",
        "fuel covariates, labelled in `sim$rasterToMatchLargeELF` and with its own `sppEquivs`,",
        "`nonForestedLCCGroupsList` and `missingLCCgroupList` entries; any other neighbour is dropped with a",
        "warning. A single-ELF run's `rasterToMatchLargeELF` labels no neighbour, so there `TRUE` keeps",
        "only the simulated ELF's row too, and says so in one message instead of a warning per neighbour."
      )
    ),
    defineParameter("cutoffForYoungAge", "numeric", fireSenseUtils::fireSenseYoungAgeCutoff, NA, NA,
      desc = paste(
        "Age at and below which pixels are considered 'young'",
        "(i.e., `age <= cutoffForYoungAge`)."
      )
    ),
    defineParameter("dataYear", "numeric", 2020, 1985, 2025,
                    paste("Year of the default landcover and stand age maps, and last year of the fires",
                          "used to initialise `nonForest_timeSinceDisturbance`, when those inputs are not",
                          "supplied (as in a run without `fireSense_dataPrepFit`). The same parameter as in",
                          "Biomass_core, Biomass_borealDataPrep, Biomass_speciesData and burnSummaries (2020);",
                          "a module with another value is an error. Set it for all with `.globals = list(dataYear = )`.")),
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
    defineParameter("replayFrom", "character", NA_character_, NA, NA,
                    desc = paste("Path of a reference hindcast job's `outputPath` (one ELF x year folder) made with",
                                 "`replaySave = TRUE`. When set, `.inputObjects` loads the spread-prediction inputs",
                                 "that do not depend on the spread parameters from it (`replayObjects`) and no",
                                 "covariate events are scheduled. `NA`: build as usual. The same name is a parameter",
                                 "of `fireSense_burn`, so `.globals = list(replayFrom = ...)` sets both.")),
    defineParameter("replaySave", "logical", FALSE, NA, NA,
                    desc = paste("Write those inputs (`replay_spreadInputs.qs2`, `replay_<name>.tif`,",
                                 "`replay_<name>.gpkg`) to `outputPath(sim)` once, after the first covariate build,",
                                 "for a later job's `replayFrom`. For a job of one year.")),
    defineParameter("scanfiVersion", "character", fireSenseUtils::fireSenseSCANFIVersion, NA, NA,
                    desc = paste(
                      "SCANFI land-cover version for non-forest land cover, when this module builds its own",
                      "landcover (only used if `rstLCC_RTM`/`rstLCCs` is not supplied)."
                    )),
    defineParameter("sppEquivCol", "character", "LandR", NA, NA,
                    desc = "Column of `sppEquiv` with the species names used in `cohortData`."),
    defineParameter("spreadFitFilename", "character", "latest", NA, NA,
                    paste("As in `fireSense_dataPrepFit`: the SpreadFit ledger file in `spreadFitGoogleDriveFolder`;",
                          "`\"latest\"` takes each ELF's newest fit this run can use, from any ledger file; a name (e.g. an",
                          "experiment's) reads that file only. Read only in a run without `fireSense_dataPrepFit`.")),
    defineParameter("spreadFitGoogleDriveFolder", "character", fireSenseUtils::spreadFitLedgerFolder(), NA, NA,
                    paste("As in `fireSense_dataPrepFit`: the Google Drive folder of the SpreadFit ledger",
                          "(`fireSenseUtils::spreadFitLedgerFolder()`). Read only in a run without `fireSense_dataPrepFit`.")),
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
    expectsInput(".ELFind", "character", sourceURL = NA,
      desc = paste("The simulated ELF (from `fireSense_ELFs`), the `polygonID` of its row in",
                   "`studyAreaWithSpreadParams`. Unsupplied: the rows are used as they are.")),
    expectsInput("climateVariables", "list", sourceURL = NA,
      desc = paste(
        "The climate layers canClimateData prepares (`climateData::climateLayers()`). Unsupplied (no",
        "`fireSense_dataPrepFit`), `.inputObjects` asks for the stored fits' variables: historical climate",
        "for the simulated years up to `climateData::latestHistoricalYear()`, projected for canClimateData's",
        "`projectedClimateYears`.")),
    expectsInput("climateVariablesForFire", "list", sourceURL = NA,
      desc = paste(
        "Named list (`ignition`, `spread`) of the layer names in `currentClimateRasters`",
        "used by each process. A length-one list is used for both. Unsupplied: the stored fits' variables",
        "when `.inputObjects` reads the SpreadFit ledger (no `fireSense_dataPrepFit`), else 'MDC' for both.")),
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
    expectsInput("cohortDatas", "list", sourceURL = NA,
      desc = paste("Optional: the real per-year datasets of `cohortData`, a named list (`year<year>`) from",
                   "`fireSense_dataPrepFit`. Used only when no module simulates `cohortData` (a hindcast):",
                   "`Init` takes the latest data year before the start year (see `createsOutput`).")),
    expectsInput("flammableRTMs", "list", sourceURL = NA,
      desc = "Optional: as `cohortDatas`, for `flammableRTM`."),
    expectsInput("landcoverDTs", "list", sourceURL = NA,
      desc = "Optional: as `cohortDatas`, for `landcoverDT`."),
    expectsInput("nonForest_timeSinceDisturbances", "list", sourceURL = NA,
      desc = "Optional: as `cohortDatas`, for `nonForest_timeSinceDisturbance`."),
    expectsInput("pixelGroupMaps", "list", sourceURL = NA,
      desc = "Optional: as `cohortDatas`, for `pixelGroupMap`."),
    expectsInput("standAgeMaps", "list", sourceURL = NA,
      desc = "Optional: as `cohortDatas`, for `standAgeMap`."),
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
        "Only `lightningDays` is used. Unsupplied, or declared by `fireSense_dataPrepFit` but not made (it",
        "skips its ignition prep): `fireSenseUtils::prepare_LightningData()`, as `fireSense_dataPrepFit` makes it.")),
    expectsInput("pixelGroupMap", "SpatRaster", sourceURL = NA,
      desc = "Map of the `pixelGroup`s in `cohortData`."),
    expectsInput("projectedClimateRasters", "list", sourceURL = NA,
      desc = paste(
        "Named list (one element per climate variable) of SpatRasters whose layers are named",
        "'year<year>'. Only used if `currentClimateRasters` is not supplied.")),
    expectsInput("rasterToMatch", "SpatRaster", sourceURL = NA,
      desc = "Template raster for the study area."),
    expectsInput("rasterToMatchELF", "SpatRaster", sourceURL = NA,
      desc = "From `fireSense_ELFs`. Only saved and replayed (`replaySave`, `replayFrom`)."),
    expectsInput("nonEscapedFireSizesHa", "numeric", sourceURL = NA,
      desc = "From `fireSense_dataPrepFit`. Only saved and replayed (`replaySave`, `replayFrom`)."),
    expectsInput("ignitionFirePoints", "SpatVector", sourceURL = NA,
      desc = "From `fireSense_dataPrepFit`. Only saved and replayed (`replaySave`, `replayFrom`)."),
    expectsInput("firePolys", "list", sourceURL = NA,
      desc = "Annual fire polygons, if supplied. Only saved and replayed (`replaySave`, `replayFrom`)."),
    expectsInput("firePolysForAge", "list", sourceURL = NA,
      desc = paste("From `fireSense_dataPrepFit`: annual fire polygons, named `year<year>`, from before the",
                   "first fit year. A hindcast needs it or `historicalFireRaster`: the fires after the data year make a",
                   "pixel `youngAge` in the hindcast year.")),
    expectsInput("historicalFireRaster", "SpatRaster", sourceURL = NA,
      desc = "From `fireSense_dataPrepFit`, optional: fire year of each pixel. Replaces `firePolysForAge`."),
    expectsInput("rasterToMatchLargeELF", "SpatRaster", sourceURL = NA,
      desc = paste("From `fireSense_ELFs`. A neighbour ELF is blended (`blendNeighbourELFs`) only if this",
                   "raster labels its pixels, which is how `fireSense_spreadPredict` weights each ELF.")),
    expectsInput("rstCurrentBurn", "SpatRaster", sourceURL = NA,
      desc = "Binary raster with 1 where the pixel burned this year."),
    expectsInput("rstLCC_RTM", "SpatRaster", sourceURL = NA,
      desc = paste(
        "Landcover raster, only used if `landcoverDT` is not supplied.",
        "Defaults to the last layer of `rstLCCs`, else NTEMS landcover for `dataYear`.")),
    expectsInput("rstLCCs", "list", sourceURL = NA,
      desc = paste(
        "Optional named list of landcover `SpatRaster`s, one per data year, as produced by",
        "`fireSense_dataPrepFit`: as `cohortDatas`, for `rstLCC_RTM` (`.inputObjects` takes the last",
        "element when `rstLCC_RTM` is not supplied).")),
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
      desc = paste("The fitted SpreadFit ledger rows (from `fireSense_dataPrepFit`, else read by `.inputObjects`; also read, undeclared, by",
                   "`fireSense_spreadPredict`), one row per fitted ELF, matched to `sppEquivs` by",
                   "`polygonID` when `sppEquivs` is named (as `fireSense_dataPrepFit` makes it), else by order.",
                   "With `.ELFind`, reduced to the simulated ELF's row and the neighbours",
                   "`blendNeighbourELFs` keeps (see `createsOutput`). Each",
                   "row's `params[[1]]` column names are the fitted formula's terms: an ELF whose terms",
                   "include `dom_agb_<class>`/`sec_agb_<class>` predicts with those classes' AGB columns,",
                   "matching what that ELF was fitted with; otherwise (an older, per-species fit) with the",
                   "previous one-column-per-fuel-class covariates. Unsupplied (a run without",
                   "`fireSense_dataPrepFit`), `.inputObjects` reads it from the SpreadFit ledger",
                   "(`predictInputsFromLedger()`). Supplied as `NULL`: no stored fits, and every ELF predicts",
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
    ## Also `expectsInput`. A cached `init` restores only the objects declared here, so each object `Init`
    ## sets must be listed, else a cache hit leaves the supplied (or no) value in its place.
    createsOutput("climateVariablesForFire", "list",
      desc = "As the input; a single entry is expanded in `init` to `ignition` and `spread`."),
    createsOutput("cohortData", "data.table",
      desc = paste("The latest vegetation available. When another module simulates it (declares it as an",
                   "output, e.g. Biomass_core in a forecast), it is that module's current state, read by every",
                   "covariate event, and this module never replaces it. Otherwise (a hindcast: no vegetation",
                   "module, times in a past year) `Init` sets it to the element of `cohortDatas` of the latest",
                   "data year strictly before the start year, as the fit pairs a fire year with its vegetation",
                   "(`fireSenseUtils::dataYearElement(before = TRUE)`): SCANFI of year Y already shows Y's",
                   "burns. In a forecast, objects no module simulates take the latest data year not after the",
                   "start year, so an NRV run from 2020 uses 2020's land cover. If `cohortDatas` is not",
                   "supplied, the input stands. The same rule for the other objects below, from their per-year",
                   "lists.")),
    createsOutput("flammableRTM", "SpatRaster",
      desc = "As `cohortData`, from `flammableRTMs`."),
    createsOutput("landcoverDT", "data.table",
      desc = "As `cohortData`, from `landcoverDTs`; built in `init` if neither is supplied."),
    createsOutput("pixelGroupMap", "SpatRaster",
      desc = "As `cohortData`, from `pixelGroupMaps`."),
    createsOutput("rstLCC_RTM", "SpatRaster",
      desc = "As `cohortData`, from `rstLCCs`."),
    createsOutput("standAgeMap", "SpatRaster",
      desc = "As `cohortData`, from `standAgeMaps`."),
    createsOutput("currentClimateRasters", "SpatRaster",
      desc = paste(
        "Climate layers for the current year, one layer per climate variable.",
        "Built from `projectedClimateRasters` if not supplied.")),
    createsOutput("fireSense_igAndEscapePred_Covariates", "data.table",
      desc = paste(
        "Ignition and escape covariates at the aggregated (`igAggFactor`) resolution;",
        "`pixelID` is the cell index of the aggregated raster. `coreFraction` is the share of the pixel's flammable",
        "cells in the core of `rasterToMatchLargeELF` (`fireSenseUtils::addCoreFraction()`, as `fireSense_dataPrepFit`",
        "gives it to the fit); it is an exposure, not a covariate.")),
    createsOutput("fireSense_SpreadCovariates", "data.table",
      desc = "Spread covariates; `pixelID` is the cell index of `flammableRTM`."),
    createsOutput("nonForest_timeSinceDisturbance", "SpatRaster",
      desc = paste("Years since last burn, used to set `youngAge` in non-forest pixels. At `init` as",
                   "`cohortData`, from `nonForest_timeSinceDisturbances`; then aged by `ageNonForest`.")),
    createsOutput("climateVariables", "list",
                  desc = "Set by `.inputObjects` when unsupplied (see `expectsInput`), for canClimateData."),
    createsOutput("ignitionFitRTM", "SpatRaster",
                  desc = paste("Unless `fireSense_dataPrepFit` (which skips its ignition prep when it reuses a ledger fit) or a user",
                               "has made it by then: the coarse grid",
                               "(`igAggFactor`) of `fireSense_igAndEscapePred_Covariates`, whose `pixelID`s are its",
                               "cells; `fireSense_ignitionPredict` predicts on it. Set with those covariates.")),
    createsOutput("lightningMaps", "SpatRaster",
                  desc = "Set by `.inputObjects`, or at `prepIgAndEscPredictData` if still NULL (see `expectsInput`)."),
    createsOutput("sppEquivs", "list",
                  desc = "Set by `.inputObjects` from the SpreadFit ledger in a run without `fireSense_dataPrepFit`."),
    createsOutput("nonForestedLCCGroupsList", "list",
                  desc = "Set by `.inputObjects` from the SpreadFit ledger in a run without `fireSense_dataPrepFit`."),
    createsOutput("missingLCCgroupList", "list",
                  desc = "Set by `.inputObjects` from the SpreadFit ledger in a run without `fireSense_dataPrepFit`."),
    createsOutput("studyAreaWithSpreadParams", "sf",
      desc = paste("With `.ELFind`: the simulated ELF's row first, then the neighbours `blendNeighbourELFs`",
                   "keeps; set by the covariate events, before `fireSense_spreadPredict` reads it. In a run",
                   "without `fireSense_dataPrepFit` (prediction only), `.inputObjects` reads it from the SpreadFit",
                   "ledger first: the rows whose polygons intersect `studyArea` (`fireSenseUtils::readSpreadFitLedger()`)."))
  )
))

doEvent.fireSense_dataPrepPredict <- function(sim, eventTime, eventType) {
  switch(eventType,
    init = {
      if ("fireSense_EscapePredict" %in% P(sim)$whichModulesToPrepare)
        stop("fireSense_EscapePredict no longer exists as a module; escape covariates are prepared with ",
             "fireSense_ignitionPredict. Remove it from parameter whichModulesToPrepare.")
      if (isReplay(sim)) return(invisible(sim)) # `.inputObjects` loaded everything; nothing to build
      sim <- Init(sim)
      sim <- scheduleEvent(sim, time(sim) + P(sim)$fireTimeStep, "fireSense_dataPrepPredict", "ageNonForest")
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
      ## after the first build (priority 5)
      if (isTRUE(P(sim)$replaySave))
        sim <- scheduleEvent(sim, P(sim)$.runInitialTime, "fireSense_dataPrepPredict", "save", eventPriority = 7)
    },
    ageNonForest = {
      ## time since disturbance is in whole years: it advances by the calendar years passed since the last
      ## event (1 with `fireTimeStep` = 1; 0 for the steps inside one year of a hindcast, whose burns in that
      ## year count from the next, as fireSenseUtils::youngAgeAtYear() counts them in the fit)
      sim$nonForest_timeSinceDisturbance <- ageNonForest(
        TSD = sim$nonForest_timeSinceDisturbance,
        rstCurrentBurn = sim$rstCurrentBurn,
        yearsElapsed = floor(as.numeric(time(sim))) - floor(as.numeric(time(sim)) - P(sim)$fireTimeStep)
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
      ## `fireSense_dataPrepFit` makes `lightningMaps` at start(sim), priority 1, before this (priority 5); when it
      ## declares them but skips its ignition prep (ledger fit reused) they are still NULL here
      if (is.null(sim$lightningMaps)) sim$lightningMaps <- lightningMapsFor(sim)
      if (!covariatesBuilt(sim, "ignition", list(sim$lightningMaps[["lightningDays"]])))
        sim <- prepare_IgnitionAndEscapePredict(sim)
      sim <- scheduleEvent(sim, time(sim) + P(sim)$fireTimeStep,
        "fireSense_dataPrepPredict", "prepIgAndEscPredictData"
      )
    },
    prepSpreadPredictData = {
      if (!covariatesBuilt(sim, "spread")) sim <- prepare_SpreadPredict(sim)
      sim <- scheduleEvent(sim, time(sim) + P(sim)$fireTimeStep,
        "fireSense_dataPrepPredict", "prepSpreadPredictData"
      )
    },
    save = {
      if (isTRUE(P(sim)$replaySave)) {
        ## SpaDES.core's own save event writes the rows added to `outputs(sim)`; the stacked `firePolys` is
        ## dropped from `sim` after it
        sim <- replayStack(sim)
        ## a file of an earlier save would be replayed for an object this save does not have
        unlink(list.files(outputPath(sim), pattern = "^replay_", full.names = TRUE))
        outputs(sim) <- rbind(outputs(sim), replayOutputs(sim))
        sim <- scheduleEvent(sim, time(sim), "save", "later", SpaDES.core::.last())
        sim <- scheduleEvent(sim, time(sim), "fireSense_dataPrepPredict", "replayUnstack", SpaDES.core::.last() + 1)
      } else {
        message("fireSense_dataPrepPredict: the save event does nothing")
      }
    },
    replayUnstack = {
      sim <- replayUnstack(sim, drop = TRUE)
    },
    warning(paste("Undefined event type: \'", current(sim)[1, "eventType", with = FALSE],
      "\' in module \'", current(sim)[1, "moduleName", with = FALSE], "\'",
      sep = ""
    ))
  )
  return(invisible(sim))
}

## vegetation object -> the per-year list (from fireSense_dataPrepFit) of its real datasets
vegetationLists <- c(cohortData = "cohortDatas", pixelGroupMap = "pixelGroupMaps",
                     landcoverDT = "landcoverDTs",
                     nonForest_timeSinceDisturbance = "nonForest_timeSinceDisturbances",
                     rstLCC_RTM = "rstLCCs", flammableRTM = "flammableRTMs",
                     standAgeMap = "standAgeMaps")

#' Take the latest vegetation available
#'
#' Each object in `vegetationLists` is the latest dataset available at `time(sim)`, among its real
#' per-year datasets (`fireSense_dataPrepFit`'s lists, named `year<dataYear>`) and the simulated state.
#' In a hindcast (no module simulates `cohortData`) the data year is the latest strictly before
#' `floor(time(sim))`, the rule the fit pairs a fire year with its vegetation by
#' (`fireSenseUtils::dataYearElement(before = TRUE)`): SCANFI of year Y already shows year Y's burns, so
#' a hindcast of a SCANFI year would otherwise see the fires it predicts. In a forecast the objects no
#' module simulates (land cover, flammability) take the data year closest to and not after
#' `floor(time(sim))`, so an NRV run from 2020 uses 2020's data. An object another module simulates
#' ([simulatedObjects()]) is dated `time(sim)`, so it is always the latest and is left as it is; so is an object with no per-year list (or one whose elements
#' are not named by year). Called at `init`:
#' after that the state evolves by simulation (`ageNonForest`, the vegetation modules).
#'
#' @param sim A `simList`.
#'
#' @return The `simList`.
takeLatestVegetation <- function(sim) {
  year <- floor(as.numeric(time(sim)))
  simulated <- simulatedObjects(sim)
  hindcast <- isHindcast(sim)
  for (obj in setdiff(names(vegetationLists), simulated)) {
    lst <- vegetationLists[[obj]]
    ## no list, or one whose elements are not named by year (an unnamed `rstLCCs` of one element, as a
    ## project may supply instead of `rstLCC_RTM`): nothing dated to choose from, the object stands
    if (!any(grepl("[0-9]", names(sim[[lst]])))) next
    el <- tryCatch(fireSenseUtils::dataYearElement(sim[[lst]], year, before = hindcast), error = function(e)
      stop("fireSense_dataPrepPredict: `", lst, "` has no element ", if (hindcast) "before " else "at or before ", year,
           " (it has: ", paste(names(sim[[lst]]), collapse = ", "), "): ", conditionMessage(e), call. = FALSE))
    sim[[obj]] <- sim[[lst]][[el]]
  }
  sim
}

#' Is this run a hindcast?
#'
#' A hindcast has no module that simulates `cohortData`: the vegetation is the data year's.
#'
#' @param sim A `simList`.
#' @return `TRUE` or `FALSE`.
isHindcast <- function(sim) !"cohortData" %in% simulatedObjects(sim)

#' The objects another module simulates
#'
#' The outputs (`createsOutput`) of every module in the simulation but this one and
#' `fireSense_dataPrepFit` (whose outputs are the last real data year): e.g. `cohortData` and
#' `pixelGroupMap` of Biomass_core. Their current value is the simulated state at `time(sim)`.
#'
#' @param sim A `simList`.
#'
#' @return character vector of object names.
simulatedObjects <- function(sim) {
  deps <- SpaDES.core::depends(sim)@dependencies
  deps <- deps[setdiff(names(deps), c(currentModule(sim), "fireSense_dataPrepFit"))]
  unique(unlist(lapply(deps, function(d) d@outputObjects$objectName)))
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
  sim <- takeLatestVegetation(sim)

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
  ## `floor()`: the climate layers are named by whole years, and time(sim) may be a fraction of a year
  ## (several reps inside one year, `fireTimeStep` < 1). Also drops time(sim)'s "unit" attribute.
  currentYear <- floor(as.numeric(currentYear))

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

#' Age the time-since-disturbance raster by the years passed
#'
#' @param TSD `SpatRaster` of years since last burn.
#' @param rstCurrentBurn `SpatRaster` with 1 where the pixel burned this year, or `NULL`.
#'   Burned pixels are reset to 0.
#' @param yearsElapsed integer, the calendar years passed since the last event (see the `ageNonForest`
#'   event). `0` (another event inside the same year) returns `TSD` as it is: time since disturbance is in
#'   whole years, and a burn counts from the next year.
#'
#' @return `TSD`, updated.
ageNonForest <- function(TSD, rstCurrentBurn, yearsElapsed = 1L) {
  if (yearsElapsed < 1) return(TSD)
  TSDvals <- as.vector(TSD)
  TSDvals <- TSDvals + yearsElapsed
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
  ignitionClimate <- climateLayers(sim, "ignition")
  if (is.null(ignitionClimate))
    stop("ignitionClimate is NULL; there is a problem to debug")
  
  # Coming out of the CacheGeo, this is unreliably a data.frame instead of a data.table
  if (!data.table::is.data.table(sim$sppEquiv)) data.table::setDT(sim$sppEquiv)
  sim <- useOwnELFRows(sim)
  ## one fuel set per fitted ELF (one, as before, when there is one ELF); the covariate tables are merged,
  ## each ELF's columns alongside the others', for fireSense_ignitionPredict to pick its own
  fuelSets <- ELFfuelSets(sim)
  fuelClassTables <- fuelClassTablesThisYear(sim, fuelSets)
  hy <- hindcastYoungAgeInputs(sim)
  ## the part of the coarse covariates that does not change from year to year, one per fuel set; the call
  ## hands back the one it used (rebuilt if land cover or flammableRTM changed)
  if (is.null(mod$fuelCovsStatic)) mod$fuelCovsStatic <- vector("list", length(fuelSets))
  fuelCovsCoarse <- mergeCovariateTables(lapply(seq_along(fuelSets), function(i) {
    fs <- fuelSets[[i]]
    fcArgs <- list(
    fuelClassTable = fuelClassTables[[i]],
    cohortData = sim$cohortData,
    pixelGroupMap = sim$pixelGroupMap,
    flammableRTM = sim$flammableRTM,
    landcoverDT = fs$landcoverDT,
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
  )
    coarse <- if (is.null(hy)) {
      do.call(prepare_FuelCovsCoarse, c(fcArgs, list(
        nonForest_timeSinceDisturbance = sim$nonForest_timeSinceDisturbance, static = mod$fuelCovsStatic[[i]])))
    } else {
      ## a hindcast resolves youngAge for its year as the fit does
      do.call(fireSenseUtils::prepare_FuelCovsCoarseByYear, c(fcArgs, list(
        years = paste0("year", hy$year), dataYear = hy$dataYear, nonForest_timeSinceDisturbance = hy$tsd,
        firePixelsByYear = hy$firePixels)))[[1]]
    }
    mod$fuelCovsStatic[i] <- list(attr(coarse, "static"))
    coarse
  }))

  ignitionClimateCoarse <- prepare_ignitionClimate(
    ignitionClimateList = as.list(ignitionClimate) |> setNames(names(ignitionClimate)), 
    fact = P(sim)$igAggFactor,
    useCache = FALSE)
  
  ## the coarse grid of these covariates (their `pixelID`s), which fireSense_ignitionPredict predicts on;
  ## fireSense_dataPrepFit makes it for the fit, from the same template, when it is in the run and prepares
  ## ignition (it declares it either way, so test for the object, not for who supplies it)
  if (is.null(sim$ignitionFitRTM))
    sim$ignitionFitRTM <- terra::setValues(terra::rast(fuelCovsCoarse, nlyrs = 1), 1)

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
  ## the share of each coarse pixel's flammable cells that are core, as fireSense_dataPrepFit gives it to the fit
  ## (same function, same `igAggFactor`: forced to the fit module's value above): the year climate term of
  ## fireSense_ignitionPredict averages the core pixels only, as the fit does
  fireSenseUtils::addCoreFraction(sim$fireSense_igAndEscapePred_Covariates, list(yrLab) |> setNames(time(sim)),
                                  list(sim$flammableRTM), sim$rasterToMatchLargeELF, P(sim)$igAggFactor)
  return(invisible(sim))
}

#' What a hindcast needs to resolve `youngAge` as the fit does
#'
#' `fireSense_dataPrepFit` gives a fire year its `youngAge` from the time since disturbance of the data year
#' aged to that year and every fire after it (`fireSenseUtils::youngAgeAtYear()`; for ignition
#' `fireSenseUtils::prepare_FuelCovsCoarseByYear()`). A hindcast of year `Y` (no module simulates `cohortData`)
#' does the same: the data year of its vegetation (`fireSenseUtils::dataYearElement(before = TRUE)`, that of
#' `takeLatestVegetation()`), `Y = floor(time(sim))`, and the fires in `historicalFireRaster`, else
#' `firePolysForAge` (`fireSenseUtils::firePixelsByYear()`, as the fit chooses; it stops if there is neither). It stops on
#' `nonForestCanBeYoungAge = FALSE`, as the fit does. A forecast, or a run with no per-year
#' `nonForest_timeSinceDisturbances`, returns `NULL`: its `youngAge` comes from the simulated cohorts and the aged
#' `nonForest_timeSinceDisturbance`. Made once per year (kept in `mod`).
#'
#' @param sim A `simList`.
#' @return `NULL`, or a list of `tsd` (`SpatRaster`, the data year's time since disturbance), `dataYear`, `year`
#'   and `firePixels` (`firePixelsByYear()`).
hindcastYoungAgeInputs <- function(sim) {
  year <- floor(as.numeric(time(sim)))
  if (identical(mod$hindcastYoungAge$year, year)) return(mod$hindcastYoungAge$inputs) # once per year
  tsds <- sim$nonForest_timeSinceDisturbances
  inputs <- NULL
  if (isHindcast(sim) && any(grepl("[0-9]", names(tsds)))) {
    fireSenseUtils::stopIfNonForestCannotBeYoung(P(sim)$nonForestCanBeYoungAge)
    if (is.null(sim$firePolysForAge) && is.null(sim$historicalFireRaster))
      stop("fireSense_dataPrepPredict: a hindcast resolves youngAge as the fit does, from the fires after the data ",
           "year: supply firePolysForAge or historicalFireRaster (made by fireSense_dataPrepFit)", call. = FALSE)
    el <- fireSenseUtils::dataYearElement(tsds, year, before = TRUE)
    ## the fit's choice (fireSense_dataPrepFit allFirePixelsByYear()): the raster if there is one, else firePolysForAge
    inputs <- list(tsd = tsds[[el]], dataYear = as.integer(gsub("[^0-9]", "", el)), year = year,
                   firePixels = fireSenseUtils::firePixelsByYear(firePolys = sim$firePolysForAge,
                                                                 fireRaster = sim$historicalFireRaster,
                                                                 template = sim$flammableRTM))
  }
  mod$hindcastYoungAge <- list(year = year, inputs = inputs)
  inputs
}

#' The layers of `currentClimateRasters` that the ignition or spread covariates use
#'
#' @param sim A `simList`.
#' @param what `"ignition"` or `"spread"`.
#' @return A `SpatRaster`, or `NULL` when the variables are `character(0)` (a spread model `~ 1` has none).
#'   Stops if `climateVariablesForFire[[what]]` is `NULL` (absent): that is a malformed `sim`, not a model without one.
climateLayers <- function(sim, what) {
  vars <- sim$climateVariablesForFire[[what]]
  if (is.null(vars))
    stop("climateVariablesForFire$", what, " is NULL; use character(0) for a model with no climate variable")
  if (length(vars)) sim$currentClimateRasters[[vars]]
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
  spreadClimate <- climateLayers(sim, "spread")

  sim <- useOwnELFRows(sim)
  ## one fuel set per fitted ELF (one, as before, when there is one ELF). Every ELF's covariates are made for
  ## every pixel and the tables merged, so fireSense_spreadPredict can apply each ELF's model wherever it
  ## predicts, including the blend zone around its own pixels. Column names say what they hold (fuel class,
  ## non-forest LCC codes), so a column two ELFs share means the same thing in both.
  fuelSets <- ELFfuelSets(sim)
  fuelClassTables <- fuelClassTablesThisYear(sim, fuelSets)
  hy <- hindcastYoungAgeInputs(sim)
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
      youngAge = is.null(hy), # a hindcast resolves it below, as the fit does
      fuelCovariates = if (is.na(fs$fuelClassRoles$domClass)) "species" else "domSecWetland",
      domClass = fs$fuelClassRoles$domClass,
      secClass = fs$fuelClassRoles$secClass,
      treedWetland = fs$fuelClassRoles$treedWetland
    )
    if (!is.null(hy)) {
      data.table::set(covs, NULL, "youngAge",
                      fireSenseUtils::youngAgeAtYear(fireSenseUtils::tsdTable(hy$tsd), dataYear = hy$dataYear,
                                                     year = hy$year, firePixelsByYear = hy$firePixels,
                                                     cutoffForYoungAge = P(sim)$cutoffForYoungAge,
                                                     pixelID = covs$pixelID))
      fireSenseUtils::clearYoungAge(covs) # fuel, non-forest land cover, treed wetland: as the fit, linear or log fuel
    }
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
  if (!is.null(spreadClimate)) { # a '~ 1' spread fit has no climate variable
    climateCovariates <- spreadClimate |> as.data.frame(cells = TRUE)
    climateCovariates <- na.omit(climateCovariates) |> as.data.table()

    setnames(climateCovariates, new = c("pixelID", names(spreadClimate)))
    spreadCovariates <- climateCovariates[spreadCovariates, on = c("pixelID")]
  }

  nonFuelNames <- c("pixelID", names(spreadClimate), "youngAge")
  setcolorder(spreadCovariates, neworder = nonFuelNames)
  sim$fireSense_SpreadCovariates <- spreadCovariates
  
  return(invisible(sim))
}


#' A digest of an input, to tell whether it changed
#'
#' `rlang::hash()` (xxhash128) of the values (a `SpatRaster`: its names and values; a table: its columns), so a change that keeps
#' the sum (two pixels swapped) is seen too.
#'
#' @param x a `SpatRaster`, table, list or `NULL`.
#' @return a character.
inputDigest <- function(x) {
  if (inherits(x, "SpatRaster")) x <- if (terra::hasValues(x)) list(names(x), terra::values(x, mat = FALSE))
  if (is.data.frame(x)) x <- as.list(x)
  rlang::hash(x)
}

#' A stamp of the vegetation the fuel classes are made from
#'
#' Digests of `cohortData`, `pixelGroupMap` and `flammableRTM`: equal stamps mean the same inputs.
#'
#' @param sim A `simList`.
#' @return A list of digests.
vegetationStamp <- function(sim) {
  lapply(list(sim$cohortData, sim$pixelGroupMap, sim$flammableRTM), inputDigest)
}

#' Are this year's covariates already built from these inputs?
#'
#' The ignition and spread covariates depend only on the year, the climate, the vegetation, the land cover,
#' the non-forest time since disturbance and the fitted rows (which ELF, which polygons, which fitted
#' parameter names, from which the fuel-class roles are read), so the steps inside one year (the reps of a
#' hindcast, `fireTimeStep` < 1) would rebuild the same tables. The stamp is of the actual inputs, not of
#' time alone: land cover and vegetation can change within a run. It is kept in `mod` per `what`; it is
#' replaced when it differs, and `FALSE` is returned so the caller builds. The rows are reduced to the own
#' ELF's first (`useOwnELFRows()`, as the builders do), so the stamp sees what the builders see.
#'
#' @param sim A `simList`.
#' @param what `"ignition"` or `"spread"`: which covariates, and so which climate variables.
#' @param extra a list of further inputs of those covariates.
#'
#' @return `TRUE` if the covariates in `sim` were built from these inputs, else `FALSE`.
covariatesBuilt <- function(sim, what, extra = list()) {
  sim <- useOwnELFRows(sim)
  sa <- sim$studyAreaWithSpreadParams
  rows <- list(as.character(sim$.ELFind), as.character(sa[[fireSenseUtils::polygonIDTxt]]),
               if (!is.null(sa$params)) lapply(sa$params, colnames))
  climate <- climateLayers(sim, what)
  stamp <- list(floor(as.numeric(time(sim))), sharedInputStamp(sim), lapply(c(list(climate), extra), inputDigest), rows)
  built <- sim[[c(ignition = "fireSense_igAndEscapePred_Covariates", spread = "fireSense_SpreadCovariates")[[what]]]]
  if (!is.null(built) && identical(mod$covariateStamps[[what]], stamp)) return(TRUE)
  mod$covariateStamps[[what]] <- stamp
  FALSE
}

#' The stamp of the inputs both covariate events read, made once per step
#'
#' The ignition and the spread event of one step read the same vegetation, time since disturbance, `landcoverDT`
#' and `rstLCC_RTM`; hashing them is as costly as a rebuild on a large landscape, so the second event of a step
#' takes the first's stamp (kept in `mod`, keyed on `time(sim)`).
#'
#' @param sim A `simList`.
#' @return A list of digests.
sharedInputStamp <- function(sim) {
  now <- as.numeric(time(sim))
  if (!identical(mod$sharedInputStamp$time, now))
    mod$sharedInputStamp <- list(time = now, stamp = list(
      vegetationStamp(sim), lapply(list(sim$nonForest_timeSinceDisturbance, sim$landcoverDT, sim$rstLCC_RTM), inputDigest)))
  mod$sharedInputStamp$stamp
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
  stamp <- function() vegetationStamp(sim)
  make <- function() lapply(fuelSets, function(fs) fireSenseUtils::cohortsToFuelClasses(
    cohortData = sim$cohortData,
    pixelGroupMap = sim$pixelGroupMap,
    flammableRTM = sim$flammableRTM,
    landcoverDT = fs$landcoverDT,
    sppEquiv = fs$sppEquiv,
    sppEquivCol = P(sim)$sppEquivCol,
    fuelClassCol = P(sim)$fuelClassCol,
    requiredFuelClasses = fs$requiredFuelClasses,
    ## a hindcast resolves youngAge itself (hindcastYoungAgeInputs()): no cohort is young here
    cutoffForYoungAge = if (is.null(hindcastYoungAgeInputs(sim))) P(sim)$cutoffForYoungAge else -1,
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
  if (removedFuelTermTxt %in% termNames)
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

## the pooled fuel covariate that was removed; a fit with this term cannot be predicted
removedFuelTermTxt <- "other_agb"

#' `fuelClassRoles` for one row of `sim$studyAreaWithSpreadParams`
#'
#' @param sim A `simList`.
#' @param i integer, the row (ELF) of `sim$studyAreaWithSpreadParams` -- the same order
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

#' Why a ledger row cannot be predicted with the current fuel covariates
#'
#' The one definition of a usable `sim$studyAreaWithSpreadParams` row, for neighbour ELFs: it has fitted
#' parameters and `covMinMax_spread`, and its fuel terms are the current `dom_agb_<class>`,
#' `sec_agb_<class>` and `treedWetland_agb`: no `other_agb`, and no per-fuel-class term (a fuel class
#' of the row's own `sppEquiv`, e.g. `Betu_pap`) from before those existed.
#'
#' @param row one row of `sim$studyAreaWithSpreadParams`.
#' @param fuelClassCol the `sppEquiv` column naming the fuel classes.
#' @return `NA` if usable, else a sentence saying why not.
spreadParamsRowUnusable <- function(row, fuelClassCol) {
  p <- row$params[[1]]
  if (is.null(p) || !NROW(p)) return("it has no fitted parameters")
  if (!"covMinMax_spread" %in% names(row) || is.null(row$covMinMax_spread[[1]]))
    return("it has no covMinMax_spread")
  terms <- colnames(p)
  if (removedFuelTermTxt %in% terms)
    return("it was fitted with `other_agb`, a fuel covariate that no longer exists")
  se <- if ("sppEquiv" %in% names(row)) row$sppEquiv[[1]]
  perClass <- intersect(terms, se[[fuelClassCol]])
  if (length(perClass))
    return(paste0("it was fitted with per-fuel-class terms (", paste(perClass, collapse = ", "),
                  ") instead of dom_agb_<class>/sec_agb_<class>"))
  NA_character_
}

#' The ledger rows to predict with: the simulated ELF's, then the neighbours that can be blended
#'
#' @param sa `sim$studyAreaWithSpreadParams`.
#' @param ownELF character; the simulated ELF (`sim$.ELFind`), or ELFs: over a `studyArea_fireSense`,
#'   `fireSense_ELFs` makes `.ELFind` all the ELFs it covers, and each keeps its row.
#' @param blend logical; the `blendNeighbourELFs` parameter.
#' @param placeableELFs character; the ELFs `sim$rasterToMatchLargeELF` labels (see [ELFlabels()]).
#' @param fuelInputELFs character; the ELFs with their own `sppEquivs`, `nonForestedLCCGroupsList` and
#'   `missingLCCgroupList` entries.
#' @param fuelClassCol passed to [spreadParamsRowUnusable()].
#' @return `sa`'s rows: `ownELF`'s first, then the neighbours kept. Stops if `ownELF` has no row, unless
#'   `sa` is the rows of a study area over several ELFs (`placeableELFs` names some of them), which are
#'   returned unchanged.
selectSpreadParamRows <- function(sa, ownELF, blend, placeableELFs, fuelInputELFs, fuelClassCol) {
  ids <- as.character(sa[[fireSenseUtils::polygonIDTxt]])
  own <- which(ids %in% ownELF)
  ownELF <- paste(ownELF, collapse = ", ")                   # for the messages
  if (!length(own)) {
    if (any(ids %in% placeableELFs)) return(sa)
    stop("fireSense_dataPrepPredict: sim$studyAreaWithSpreadParams has no row for the simulated ELF ", ownELF,
         " (sim$.ELFind); it has ", paste(ids, collapse = ", "), ". Fit ELF ", ownELF, " first.", call. = FALSE)
  }
  ## one row per simulated ELF; was `own[1]`, which kept only the first of several (a studyArea_fireSense)
  own <- own[!duplicated(ids[own])]
  keep <- integer(0)
  if (isTRUE(blend)) {
    neighbours <- setdiff(seq_along(ids), own)
    ## A single-ELF run's rasterToMatchLargeELF labels no ELF, so no neighbour can be blended: that is
    ## expected, and said once below, not a warning per neighbour.
    if (!any(ids %in% placeableELFs)) {
      if (length(neighbours))
        message("fireSense_dataPrepPredict: single-ELF run of ELF ", ownELF, ": not blending neighbour ELF(s) ",
                paste(ids[neighbours], collapse = ", "), " (sim$rasterToMatchLargeELF labels no ELF). ",
                "Set blendNeighbourELFs = FALSE to skip this message.")
      neighbours <- integer(0)
    }
    for (i in neighbours) {
      why <- spreadParamsRowUnusable(sa[i, , drop = FALSE], fuelClassCol)
      if (is.na(why) && !ids[i] %in% placeableELFs)
        why <- "sim$rasterToMatchLargeELF does not label its pixels"
      if (is.na(why) && !ids[i] %in% fuelInputELFs)
        why <- "sppEquivs, nonForestedLCCGroupsList and missingLCCgroupList have no entry for it"
      if (is.na(why)) keep <- c(keep, i)
      else warning("fireSense_dataPrepPredict: not blending neighbour ELF ", ids[i], " into ELF ", ownELF,
                   ": ", why, ".", call. = FALSE)
    }
  }
  sa[c(own, keep), , drop = FALSE]
}

## The ELFs a raster labels: its categories, else its values (as fireSense_spreadPredict's ELFblendWeights()
## reads `sim$rasterToMatchLargeELF`)
ELFlabels <- function(r) {
  if (is.null(r)) return(character(0))
  lv <- terra::levels(r[[1]])[[1]]
  if (is.data.frame(lv) && NCOL(lv) >= 2) return(as.character(lv[[2]]))
  as.character(unique(terra::values(r[[1]], mat = FALSE)))
}

#' Reduce `sim$studyAreaWithSpreadParams` to the simulated ELF's row and the neighbours to blend
#'
#' Done in the covariate events, which run before `fireSense_spreadPredict`'s `run` each year and after
#' `fireSense_spreadFit` writes the rows; a second call finds nothing more to drop.
#'
#' @param sim A `simList`.
#' @return The `simList`, invisibly.
useOwnELFRows <- function(sim) {
  sa <- sim$studyAreaWithSpreadParams
  if (is.null(sim$.ELFind) || !NROW(sa)) return(invisible(sim))
  fuelInputELFs <- Reduce(intersect, list(names(sim$sppEquivs), names(sim$nonForestedLCCGroupsList),
                                          names(sim$missingLCCgroupList)))
  sim$studyAreaWithSpreadParams <- selectSpreadParamRows(
    sa, ownELF = as.character(sim$.ELFind), blend = P(sim)$blendNeighbourELFs,
    placeableELFs = ELFlabels(sim$rasterToMatchLargeELF), fuelInputELFs = fuelInputELFs,
    fuelClassCol = P(sim)$fuelClassCol)
  invisible(sim)
}

ELFfuelSets <- function(sim) {
  fcc <- P(sim)$fuelClassCol
  ## the land cover the fuel sets are made from is kept, and made again when `rstLCC_RTM` or `flammableRTM` changed
  lcStamp <- lapply(list(sim$rstLCC_RTM, sim$flammableRTM), inputDigest)
  if (!identical(mod$ELFlcStamp, lcStamp)) {
    mod$ELFrstLCC <- NULL
    mod$ELFlandcoverDTs <- NULL
    mod$ELFlcStamp <- lcStamp
  }
  if (length(sim$sppEquivs) > 1L || length(sim$nonForestedLCCGroupsList)) {
    n <- length(sim$sppEquivs)
    if (length(sim$nonForestedLCCGroupsList) != n || length(sim$missingLCCgroupList) != n)
      stop("fireSense_dataPrepPredict: sppEquivs, nonForestedLCCGroupsList and missingLCCgroupList must have one ",
           "element per ELF")
    ## the per-ELF entries for each row of studyAreaWithSpreadParams, by polygonID when they are named
    ids <- as.character(sim$studyAreaWithSpreadParams[[fireSenseUtils::polygonIDTxt]])
    idx <- if (length(ids) && !is.null(names(sim$sppEquivs))) match(ids, names(sim$sppEquivs)) else seq_len(n)
    if (anyNA(idx))
      stop("fireSense_dataPrepPredict: sppEquivs has no entry for ELF(s) ", paste(ids[is.na(idx)], collapse = ", "),
           " of sim$studyAreaWithSpreadParams")
    if (is.null(mod$ELFrstLCC)) {
      mod$ELFrstLCC <- if (!LandR::.compareRas(sim$flammableRTM, sim$rstLCC_RTM, stopOnError = FALSE))
        reproducible::postProcess(sim$rstLCC_RTM, to = sim$flammableRTM, method = "near") else sim$rstLCC_RTM
    }
    if (is.null(mod$ELFlandcoverDTs)) {
      mod$ELFlandcoverDTs <- lapply(sim$nonForestedLCCGroupsList, function(g)
        makeLandcoverDT(rstLCC = mod$ELFrstLCC, flammableRTM = sim$flammableRTM,
                        forestedLCC = P(sim)$forestedLCC, nonForestedLCCGroups = g))
    }
    return(lapply(seq_along(idx), function(i) {
      j <- idx[i]
      se <- data.table::as.data.table(sim$sppEquivs[[j]])
      list(sppEquiv = se, nonForestedLCCGroups = sim$nonForestedLCCGroupsList[[j]],
           missingLCCgroup = sim$missingLCCgroupList[[j]], landcoverDT = mod$ELFlandcoverDTs[[j]],
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

#' The stored fits and their inputs, in a run that only predicts
#'
#' Without `fireSense_dataPrepFit`, nothing else reads the SpreadFit ledger for this study area. Sets
#' `studyAreaWithSpreadParams` (the ledger rows whose polygons intersect `studyArea`), the per-ELF
#' `sppEquivs`, `nonForestedLCCGroupsList` and `missingLCCgroupList`, and, unless supplied,
#' `climateVariablesForFire` (the fits' climate variables) and `climateVariables` (their layers, for
#' canClimateData). Stops when the ledger has no fit for the study area.
#'
#' @param sim A `simList`.
#'
#' @return The `simList`.
predictInputsFromLedger <- function(sim) {
  sa <- sim$studyArea
  if (inherits(sa, "SpatVector")) sa <- sf::st_as_sf(sa)
  ## the escape definition is not checked here (this module has none); fireSense_burn checks it against the fit's
  rows <- fireSenseUtils::readSpreadFitLedger(P(sim)$spreadFitFilename, P(sim)$spreadFitGoogleDriveFolder,
                                              domain = sa, destinationPath = inputPath(sim))
  if (!NROW(rows))
    stop("fireSense_dataPrepPredict: the SpreadFit ledger (spreadFitFilename \"", P(sim)$spreadFitFilename,
         "\") has no stored fit for this study area, so there is nothing to predict from. Name a ledger file ",
         "that has one, or fit first (fireSense_dataPrepFit and fireSense_spreadFit with doFit = NA).", call. = FALSE)
  sim$studyAreaWithSpreadParams <- rows
  ins <- fireSenseUtils::spreadFitLedgerInputs(rows, sppEquivCol = P(sim)$sppEquivCol,
                                               fuelClassCol = P(sim)$fuelClassCol, studyArea = sim$studyArea)
  lists <- fireSenseUtils::spreadFitLedgerInputLists(ins)
  for (nm in c("sppEquivs", "nonForestedLCCGroupsList", "missingLCCgroupList")) sim[[nm]] <- lists[[nm]]
  fitClimVars <- fireSenseUtils::spreadFitClimateVariables(ins)
  if (!suppliedElsewhere("climateVariablesForFire", sim))
    sim$climateVariablesForFire <- fireSenseUtils::addFitClimateVariablesForFire(
      fireSenseUtils::defaultClimateVariablesForFire, fitClimVars)
  if (!suppliedElsewhere("climateVariables", sim, where = c("sim", "user"))) {
    yrs <- seq(floor(start(sim)), floor(end(sim)))
    sim$climateVariables <- fireSenseUtils::fireClimateLayers(
      sim$climateVariablesForFire, historicalYears = yrs[yrs <= climateData::latestHistoricalYear()],
      projectedYears = fireSenseUtils::canClimateProjectedYears(sim))
  }
  sim
}

#' Supply default inputs
#'
#' The stored fits, in a run without `fireSense_dataPrepFit` ([predictInputsFromLedger()]), lightning, and
#' defaults for `climateVariablesForFire`, `rstLCC_RTM`, `standAgeMap`, `flammableRTM` and `landcoverDT`.
#'
#' @param sim A `simList`.
#'
#' @return The `simList`, invisibly.
lightningMapsFor <- function(sim)
  fireSenseUtils::prepare_LightningData(sim$rasterToMatch, P(sim)$igAggFactor, dPath = asPath(inputPath(sim), 1))

.inputObjects <- function(sim) {
  if (isReplay(sim)) { # a replay job: the saved inputs, nothing built
    message(currentModule(sim), ": replaying the inputs saved in '", P(sim)$replayFrom, "'.")
    inputs(sim) <- rbind(inputs(sim), replayInputs(sim))
    sim <- SpaDES.core::loadFiles(sim)
    return(invisible(replayUnstack(sim)))
  }
  cacheTags <- c(currentModule(sim), "otherFunctions:.inputObjects")
  dPath <- asPath(inputPath(sim), 1)
  ## the same data year as the vegetation modules (Biomass_core, Biomass_borealDataPrep, ...)
  SpaDES.core::paramCheckOtherMods(sim, "dataYear", ifSetButDifferent = "error")
  message(currentModule(sim), ": using dataPath '", dPath, "'.")

  ## A run without fireSense_dataPrepFit only predicts: the stored fits, and the species, fuel and climate
  ## inputs they need, come from the SpreadFit ledger, as fireSense_dataPrepFit's Init() takes them.
  if (!suppliedElsewhere("studyAreaWithSpreadParams", sim))
    sim <- predictInputsFromLedger(sim)

  ## only the ignition covariates use lightning
  if ("fireSense_ignitionPredict" %in% P(sim)$whichModulesToPrepare && !suppliedElsewhere("lightningMaps", sim))
    sim$lightningMaps <- lightningMapsFor(sim)

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

## The spread-prediction inputs that do not depend on the spread parameters: what a replay job needs in place
## of the climate, vegetation, fit and ELF modules, and how each is written. This module builds
## `fireSense_SpreadCovariates` (and `flammableRTM` from `fireSense_dataPrepFit`'s per-year list,
## `studyAreaWithSpreadParams` from the ledger when `fireSense_dataPrepFit` is absent); `fireSense_ELFs` makes the
## three `rasterToMatch*`, `fireSense_dataPrepFit` makes `nonEscapedFireSizesHa` and `ignitionFirePoints`, and
## `firePolys` is supplied by the user (or absent). They are written and read through the `outputs()` and
## `inputs()` of SpaDES.core (`saveFiles()`, `loadFiles()`): terra objects with `terra::writeRaster()`/`rast()` and
## `terra::writeVector()`/`vect()`, tables and `sf` (whose list columns a GeoPackage loses) with qs2. A terra
## object never goes to qs2.
replayObjects <- c(fireSense_SpreadCovariates = "qs2", flammableRTM = "raster", rasterToMatch = "raster",
                   rasterToMatchELF = "raster", rasterToMatchLargeELF = "raster",
                   studyAreaWithSpreadParams = "qs2", nonEscapedFireSizesHa = "qs2",
                   ignitionFirePoints = "vector", replay_firePolys = "vector") # firePolys, stacked in one
replayHow <- list(qs2 = list(save = "qs_save", load = "qs_read", package = "qs2", ext = "qs2"),
                  raster = list(save = "writeRaster", load = "rast", package = "terra", ext = "tif",
                                class = "SpatRaster"),
                  vector = list(save = "writeVector", load = "vect", package = "terra", ext = "gpkg",
                                class = "SpatVector"))
isReplay <- function(sim) !is.na(P(sim)$replayFrom)

## the file of an object in `path`: SpaDES.core's outputs() names it `replay_<name>_year<time>.<ext>`
replayFile <- function(path, name, how, pattern = FALSE) {
  base <- sub("^(replay_)?", "replay_", name)
  if (!pattern) return(file.path(path, paste0(base, ".", replayHow[[how]]$ext)))
  found <- list.files(path, pattern = paste0("^", base, "_year[0-9.]+\\.", replayHow[[how]]$ext, "$"), full.names = TRUE)
  if (length(found) > 1L)
    stop("fireSense_dataPrepPredict: ", length(found), " saves of ", name, " in '", path, "'; replay from a folder with one")
  found
}

## the list of annual fire polygons as one `SpatVector`, the list names in a column
stackFirePolys <- function(x) {
  if (is.null(names(x)) || anyNA(names(x)) || !all(nzchar(names(x))))
    stop("fireSense_dataPrepPredict: firePolys must be a named list (year<YYYY>) to be saved for a replay")
  x <- x[!vapply(x, is.null, logical(1))] # an absent year has nothing to keep
  do.call(rbind, unname(Map(function(v, nm) { v$replayElement <- nm; v }, x, names(x))))
}

unstackFirePolys <- function(v) {
  nms <- unique(v$replayElement)
  stats::setNames(lapply(nms, function(nm) { x <- v[v$replayElement == nm, ]; x$replayElement <- NULL; x }), nms)
}

## `firePolys` <-> its single-vector form, in `sim`
replayStack <- function(sim) {
  if (!is.null(sim$firePolys)) sim$replay_firePolys <- stackFirePolys(sim$firePolys)
  sim
}
replayUnstack <- function(sim, drop = FALSE) {
  if (!drop && !is.null(sim$replay_firePolys)) sim$firePolys <- unstackFirePolys(sim$replay_firePolys)
  if (!is.null(sim$replay_firePolys)) rm("replay_firePolys", envir = envir(sim))
  sim
}

## rows for `outputs(sim)`: the replay objects `sim` holds, written to `outputPath(sim)` now. An object of the
## wrong class for how it is written stops (a terra object must not reach qs2).
replayOutputs <- function(sim) {
  have <- replayObjects[vapply(names(replayObjects), function(nm) !is.null(sim[[nm]]), logical(1))]
  for (nm in names(have)) {
    x <- sim[[nm]]
    cls <- replayHow[[have[[nm]]]]$class
    if (if (is.null(cls)) inherits(x, c("SpatRaster", "SpatVector")) else !inherits(x, cls))
      stop("fireSense_dataPrepPredict: ", nm, " is a ", class(x)[1], ", which cannot be saved as ", have[[nm]],
           " for a replay")
  }
  out <- data.frame(objectName = names(have), saveTime = as.numeric(time(sim)),
             file = vapply(names(have), function(nm) replayFile(outputPath(sim), nm, have[[nm]]), character(1)),
             fun = vapply(have, function(h) replayHow[[h]]$save, character(1)),
             package = vapply(have, function(h) replayHow[[h]]$package, character(1)),
             saved = NA, stringsAsFactors = FALSE, row.names = NULL)
  out$arguments <- lapply(have, function(h) if (identical(h, "vector")) list(overwrite = TRUE)) # core sets it for writeRaster only
  out
}

## rows for `inputs(sim)`: the replay files in `replayFrom`
replayInputs <- function(sim) {
  how <- replayObjects
  files <- lapply(names(how), function(nm) replayFile(P(sim)$replayFrom, nm, how[[nm]], pattern = TRUE))
  have <- lengths(files) == 1L
  files <- unlist(files)
  if (!any(have)) stop("fireSense_dataPrepPredict: no replay_* files in replayFrom '", P(sim)$replayFrom, "'")
  data.frame(objectName = names(how)[have], file = files,
             fun = vapply(how[have], function(h) replayHow[[h]]$load, character(1)),
             package = vapply(how[have], function(h) replayHow[[h]]$package, character(1)),
             loadTime = as.numeric(time(sim)), stringsAsFactors = FALSE, row.names = NULL)
}

## An NA `.studyAreaName` becomes a hash of `studyArea`, as in Biomass_borealDataPrep. Without a
## `studyArea` it stays NA (see PredictiveEcology/LandR#246).
resolveStudyAreaName <- function(sim) {
  if ((is.null(P(sim)$.studyAreaName) || is.na(P(sim)$.studyAreaName)) && !is.null(sim$studyArea))
    P(sim)$.studyAreaName <- reproducible::studyAreaName(sim$studyArea, notSupplied = ".studyAreaName")
  sim
}
