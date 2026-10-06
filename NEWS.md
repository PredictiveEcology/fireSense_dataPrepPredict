# fireSense_dataPrepPredict (development version)

- The spread fuel roles came from row 1 of `sim$studyAreaWithSpreadParams`, by position, but that object holds every ledger row touching the study area: a single-ELF run of 13.1 took neighbour 4.2.2's row and stopped on its `other_agb` term. Rows are now matched by `polygonID`: the simulated ELF's (`sim$.ELFind`) comes first and a missing one stops. New parameter `blendNeighbourELFs` (default `TRUE`): `FALSE` keeps only that row; `TRUE` also keeps neighbours that are fitted with the current fuel covariates, labelled in `sim$rasterToMatchLargeELF` and present in the named `sppEquivs`/`nonForestedLCCGroupsList`/`missingLCCgroupList`, and drops the rest with a warning. A single-ELF run's `rasterToMatchLargeELF` labels no neighbour, so it predicts with its own row either way. `studyAreaWithSpreadParams` is now also an output, so a cached covariate event restores the reduced rows.

# fireSense_dataPrepPredict 1.0.4.9012

- A comment said `treedWetland_agb` is not on the `logMinB()` floor scale. In `domSecWetland` mode it is (`fireSenseUtils::fireSenseCovariatesCreate()` logs it like `dom_agb_*` and `sec_agb_*`); only the species-mode 0/1 `treedWetland` is not. Comment only, no behaviour change.

- Treed wetland is predicted only when the fit has its term (`treedWetland_agb`, or an older fit's `treedWetland`): `fireSense_dataPrepFit`'s new `minCovariateProp` leaves it out of ELFs with little treed wetland, whose AGB is then ordinary `dom_agb_*`/`sec_agb_*` fuel. `fuelClassRolesFromTermNames()` returns `treedWetland` and it is passed to `fireSenseUtils::fireSenseCovariatesCreate()`. Needs fireSenseUtils >= 0.2.3.9081 (PredictiveEcology/fireSenseUtils#129).
- The fuel classes are made once per year, not twice: the ignition and spread covariates are built from the same `cohortData`, `pixelGroupMap`, `flammableRTM`, `landcoverDT`, `sppEquiv` and `cutoffForYoungAge`, so whichever event runs first makes them with `fireSenseUtils::cohortsToFuelClasses(asTable = TRUE)` and the second reuses them through `fireSenseCovariatesCreate(fuelClassTable =)`. The second event builds its own if `cohortData`, `pixelGroupMap` or `flammableRTM` changed in between. The covariates are identical. Needs the `fireSenseUtils` change that adds `asTable` and `fuelClassTable` (PredictiveEcology/fireSenseUtils#121).

- The pooled `other_agb` spread covariate is gone, and `fuelCovariates = "domSecOther"` is renamed `"domSecWetland"` in the call to `fireSenseUtils::fireSenseCovariatesCreate()`: prediction builds `dom_agb_<class>`, `sec_agb_<class>` and `treedWetland_agb`. A fitted model with an `other_agb` term now stops with a message to refit, instead of being predicted without that term. Needs the `fireSenseUtils` change that renames the value (PredictiveEcology/fireSenseUtils#116).

- reqdPkgs now lists `LandR`, `reproducible` and `SpaDES.core`, which the module calls (`LandR::.compareRas`, `postProcess`, `Cache`, `asPath`, `.suffix`, `paramCheckOtherMods`); it relied on another module attaching them. Version 1.0.4.9010.

- `fireSense_EscapePredict` no longer exists (`fireSense_ignitionPredict` predicts ignition and escape): it is removed from the
  `whichModulesToPrepare` default (now `fireSense_ignitionPredict` and `fireSense_spreadPredict`) and naming it stops with a message.

- With one fitted ELF, the covariates (and that ELF's `landcoverDT`) are built from that ELF's groups in `nonForestedLCCGroupsList` and
  `missingLCCgroupList` whenever those are present, as with several ELFs. Before, a predict-only run with one ELF used the
  module default `nonForestedLCCGroups` (`nf`) and the spread prediction failed on the fitted `nfLCC_*` terms.

- `loadOrder` and the `whichModulesToPrepare` default and comparisons use the renamed `fireSense_ignitionFit`, `fireSense_spreadFit`,
  `fireSense_ignitionPredict` and `fireSense_spreadPredict` (formerly `fireSense_IgnitionFit`, `fireSense_SpreadFit`,
  `fireSense_IgnitionPredict`, `fireSense_SpreadPredict`). A project setting `whichModulesToPrepare` must use the new names.


- `forestedLCC`, `cutoffForYoungAge`, `nonForestCanBeYoungAge`, `flammabilityThreshold`,
  `fuelClassCol` and `igAggFactor` now default to `fireSenseUtils`'s shared constants
  (`fireSenseForestedLCC`, `fireSenseYoungAgeCutoff`, `fireSenseNonForestCanBeYoungAge`,
  `fireSenseFlammabilityThreshold`, `fireSenseFuelClassCol`, `fireSenseIgAggFactor`), as
  `nonflammableLCC` already did, so a fit and its predictions cannot silently use different
  values. Values are unchanged. New parameter `scanfiVersion` (default
  `fireSenseUtils::fireSenseSCANFIVersion`), the SCANFI land-cover version used when this module
  builds its own land cover, passed to `makeFireSenseLCC()`. `fireSenseCovariatesCreate()` is now
  called with `::`, not `:::` (it is exported). Needs `fireSenseUtils@development (>= 0.2.3.9062)`.
  Version 1.0.4.9006.
- Fixed: `nonflammableLCC`'s default (`c(0, 20, 31, 32, 33)`) missed SCANFI's rock/exposed code
  (`30`), so rock entered predictions as flammable non-forest. The default now comes from
  `fireSenseUtils::fireSenseNonflammableLCC`, the single source of truth `makeFireSenseLCC()`
  also uses. Needs `fireSenseUtils@development (>= 0.2.3.9060)`. Version 1.0.4.9005.
- `prepare_SpreadPredict()` now passes `rstLCC` to `fireSenseCovariatesCreate()` (previously never
  passed, so `treedWetland` never appeared). It now also reads `sim$studyAreaWithSpreadParams`
  (the fitted SpreadFit ledger rows `fireSense_ELFs` supplies, also read undeclared by
  `fireSense_SpreadPredict`): when an ELF's fitted parameter names include `dom_agb_<class>`/
  `sec_agb_<class>` (new `fuelClassRolesFromTermNames()`), prediction builds that ELF's
  `dom_agb_<class>`/`sec_agb_<class>`/`other_agb`/`treedWetland_agb` columns using THOSE classes,
  not the classes that happen to dominate the prediction area. An older, per-species fit (no such
  terms), or no fitted parameters yet, predicts as before: the previous one-column-per-fuel-class
  covariates. Needs `fireSenseUtils@development (>= 0.2.3.9057)`. Version 1.0.4.9004.

- `studyArea` is now a declared input. `.inputObjects` used it to mask the fire polygons, land cover and stand age it
  makes, but an undeclared object is not visible there, so it was always `NULL` and nothing was masked. It also made
  an unset `.studyAreaName` come from the extent of `rasterToMatch` in `.inputObjects` but from `studyArea` in `Init`.
  That `rasterToMatch` fallback is removed: without a `studyArea`, an unset `.studyAreaName` stays `NA`.
- New parameter `.studyAreaName` (default `NA`). The module already read `P(sim)$.studyAreaName` without defining it,
  so it was always `NULL`, whatever the user set. Left `NA`, it becomes a hash of `studyArea` (or of the extent of
  `rasterToMatch`), as in Biomass_borealDataPrep. It names the study area in the landcover file
  (`rstLCC_<dataYear>_<studyAreaName>.tif`) and in cache tags, so caches built before this rebuild.
- Several fitted ELFs in one study area: with `sppEquivs`, `nonForestedLCCGroupsList` and `missingLCCgroupList` from `fireSense_dataPrepFit` (one element per ELF), every ELF's fuel covariates are made for every pixel, each with its own species table, non-forest groups and `landcoverDT`, and merged by `pixelID` (spread) or by layer (ignition). `fireSense_SpreadPredict` and `fireSense_IgnitionPredict` then apply each ELF's model to its own columns. One ELF works as before.

# fireSense_dataPrepPredict 1.0.2

First release from `development` since `main` was last updated (2022-02-28). Full history: https://github.com/PredictiveEcology/fireSense_dataPrepPredict/compare/9b276ed...v1.0.2

## Breaking changes

- Removed input `PCAveg` (prcomp).
- Removed input `climateComponentsToUse` (character).
- Removed input `nonForest_timeSinceDisturbance` (RasterLayer).
- Removed input `projectedClimateLayers` (list).
- Removed input `rstLCC` (RasterLayer).
- Removed input `terrainDT` (data.table).
- Removed input `vegComponentsToUse` (character).
- Input `flammableRTM` is now `SpatRaster` (was `RasterLayer`).
- Input `pixelGroupMap` is now `SpatRaster` (was `RasterLayer`).
- Input `rstCurrentBurn` is now `SpatRaster` (was `RasterLayer`).
- Removed output `currentClimateLayers` (list).
- Removed output `fireSense_IgnitionAndEscapeCovariates` (data.table).
- Output `nonForest_timeSinceDisturbance` is now `SpatRaster` (was `RasterLayer`).
- Removed parameters: `ignitionFuelClassCol`, `missingLCCgroup`, `spreadFuelClassCol`.

## New features

- New inputs: `climateVariablesForFire`, `climateYear`, `currentClimateRasters`, `fireSense_IgnitionFitted`, `lightningMaps`, `missingLCCgroup`, `projectedClimateRasters`, `propFlammable`, `rasterToMatch`, `rstLCC_RTM`, `standAgeMap`.
- New outputs: `currentClimateRasters`, `fireSense_igAndEscapePred_Covariates`.
- New parameters: `.runInitialTime`, `dataYear`, `flammabilityThreshold`, `fuelClassCol`, `igAggFactor`, `nonForestCanBeYoungAge`, `nonflammableLCC`.

## Dependencies

- No longer depends on `raster`.
- Now depends on `terra`.

## Testing

- testthat suite and CI (`testthat-module`), including a snapshot of the module's inputs, outputs and parameters in `tests/testthat/test-metadata.R`.
