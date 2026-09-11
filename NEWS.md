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
