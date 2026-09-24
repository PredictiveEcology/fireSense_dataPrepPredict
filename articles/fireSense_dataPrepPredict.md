---
title: "fireSense_dataPrepPredict Manual"
subtitle: "v.1.0.4.9001"
date: "Last updated: 2026-09-24"
output:
  bookdown::html_document2:
    toc: true
    toc_float: true
    theme: sandstone
    number_sections: false
    df_print: paged
    keep_md: yes
editor_options:
  chunk_output_type: console
bibliography: citations/references_fireSense_dataPrepPredict.bib
link-citations: true
always_allow_html: true
pkgdown:
  as_is: true
---

# fireSense_dataPrepPredict Module

<!-- the following are text references used in captions for LaTeX compatibility -->
(ref:fireSense-dataPrepPredict) *fireSense_dataPrepPredict*



[![made-with-Markdown](figures/markdownBadge.png)](https://commonmark.org)

<!-- if knitting to pdf remember to add the pandoc_args: ["--extract-media", "."] option to yml in order to get the badge images -->

#### Authors:

Ian Eddy <ian.eddy@nrcan-rncan.gc.ca> [aut, cre], Eliot McIntire <eliot.mcintire@nrcan-rncan.gc.ca> [aut], Alex M Chubaty <achubaty@for-cast.ca> [ctb]
<!-- ideally separate authors with new lines, '\n' not working -->

## Module Overview

### Module summary

Prepares, each year, the covariate tables that the fireSense [@Marchal:2017a; @Marchal:2017b; @Marchal:2019] predict modules use:

- `fireSense_igAndEscapePred_Covariates` for *fireSense_IgnitionPredict* and *fireSense_EscapePredict*: fuel classes, non-forest landcover, `youngAge`, ignition climate and lightning days, aggregated by `igAggFactor`.
- `fireSense_SpreadCovariates` for *fireSense_SpreadPredict*: the same fuel, landcover and `youngAge` columns plus spread climate, at the resolution of `flammableRTM`.

Fuel classes come from `cohortData` and `pixelGroupMap`, grouped by the `fuelClassCol` column of `sppEquiv`.
The covariates are built by the same *fireSenseUtils* functions that *fireSense_dataPrepFit* uses, so they match the fitted models.

### Module inputs and parameters

`cohortData`, `pixelGroupMap` and `rstCurrentBurn` come from the vegetation and fire modules during the simulation.
`sppEquiv`, `nonForestedLCCGroups`, `missingLCCgroup`, `lightningMaps`, `flammableRTM` and `landcoverDT` should be the ones used by *fireSense_dataPrepFit*.
Climate is either `currentClimateRasters`, or `projectedClimateRasters` with layers named `year<year>`.
`igAggFactor` is overwritten in `init` with the value set in the other modules.

Table \@ref(tab:moduleInputs-fireSense-dataPrepPredict) shows the full list of module inputs.

<table class="table" style="margin-left: auto; margin-right: auto;">
<caption>(\#tab:moduleInputs-fireSense-dataPrepPredict)(\#tab:moduleInputs-fireSense-dataPrepPredict)List of (ref:fireSense-dataPrepPredict) input objects and their description.</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> objectName </th>
   <th style="text-align:left;"> objectClass </th>
   <th style="text-align:left;"> desc </th>
   <th style="text-align:left;"> sourceURL </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> climateVariablesForFire </td>
   <td style="text-align:left;"> list </td>
   <td style="text-align:left;"> Named list (`ignition`, `spread`) of the layer names in `currentClimateRasters` used by each process. A length-one list is used for both. Default is 'MDC' for both. </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> climateYear </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> Optional year (e.g. 2009) used instead of `time(sim)` to build `currentClimateRasters`. See PredictiveEcology/climateYear. </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> currentClimateRasters </td>
   <td style="text-align:left;"> SpatRaster </td>
   <td style="text-align:left;"> Climate layers for the current year, one layer per climate variable. If absent, built from `projectedClimateRasters`. </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> cohortData </td>
   <td style="text-align:left;"> data.table </td>
   <td style="text-align:left;"> Cohorts by `pixelGroup` (LandR). </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> missingLCCgroup </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Forested pixels that are absent from `cohortData` are assigned to this class. Must be one of the names of `nonForestedLCCGroups`. </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> flammableRTM </td>
   <td style="text-align:left;"> SpatRaster </td>
   <td style="text-align:left;"> Binary raster of flammable pixels at `start(sim)`. If absent, derived from the default landcover and `nonflammableLCC`. </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> nonForestedLCCGroups </td>
   <td style="text-align:left;"> list </td>
   <td style="text-align:left;"> Named list of non-forested landcover groups, e.g. `list('wetland' = c(19, 23, 32))`. Only used if `landcoverDT` is not supplied. </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> lightningMaps </td>
   <td style="text-align:left;"> SpatRaster </td>
   <td style="text-align:left;"> Lightning layers: lightningDays, lightningDensity, positiveCG, positiveCGdensity. Only `lightningDays` is used. </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> pixelGroupMap </td>
   <td style="text-align:left;"> SpatRaster </td>
   <td style="text-align:left;"> Map of the `pixelGroup`s in `cohortData`. </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> projectedClimateRasters </td>
   <td style="text-align:left;"> list </td>
   <td style="text-align:left;"> Named list (one element per climate variable) of SpatRasters whose layers are named 'year&lt;year&gt;'. Only used if `currentClimateRasters` is not supplied. </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> rasterToMatch </td>
   <td style="text-align:left;"> SpatRaster </td>
   <td style="text-align:left;"> Template raster for the study area. </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> rstCurrentBurn </td>
   <td style="text-align:left;"> SpatRaster </td>
   <td style="text-align:left;"> Binary raster with 1 where the pixel burned this year. </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> rstLCC_RTM </td>
   <td style="text-align:left;"> SpatRaster </td>
   <td style="text-align:left;"> Landcover raster, only used if `landcoverDT` is not supplied. Defaults to the last layer of `rstLCCs`, else NTEMS landcover for `dataYear`. </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> rstLCCs </td>
   <td style="text-align:left;"> list </td>
   <td style="text-align:left;"> Optional named list of landcover `SpatRaster`s, one per data year, as produced by `fireSense_dataPrepFit`. If supplied, the last element is used as `rstLCC_RTM`. </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> sppEquivs </td>
   <td style="text-align:left;"> list </td>
   <td style="text-align:left;"> Only with several fitted ELFs: one `sppEquiv` per ELF, from `fireSense_dataPrepFit`. Each ELF's fuel covariates are made with its own, for every pixel. </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> nonForestedLCCGroupsList </td>
   <td style="text-align:left;"> list </td>
   <td style="text-align:left;"> Only with several fitted ELFs: one `nonForestedLCCGroups` per ELF, in the order of `sppEquivs`. </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> missingLCCgroupList </td>
   <td style="text-align:left;"> list </td>
   <td style="text-align:left;"> Only with several fitted ELFs: one `missingLCCgroup` per ELF, in the order of `sppEquivs`. </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> sppEquiv </td>
   <td style="text-align:left;"> data.table </td>
   <td style="text-align:left;"> Table of LandR species equivalencies; must have columns `sppEquivCol` and `fuelClassCol`. </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> standAgeMap </td>
   <td style="text-align:left;"> SpatRaster </td>
   <td style="text-align:left;"> Stand age (years) at `start(sim)`. </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> landcoverDT </td>
   <td style="text-align:left;"> data.table </td>
   <td style="text-align:left;"> `pixelID` plus one binary column per non-forest landcover group, for flammable pixels, at `start(sim)`. </td>
   <td style="text-align:left;"> NA </td>
  </tr>
</tbody>
</table>

Summary of user-visible parameters (Table \@ref(tab:moduleParams-fireSense-dataPrepPredict))


<table class="table" style="margin-left: auto; margin-right: auto;">
<caption>(\#tab:moduleParams-fireSense-dataPrepPredict)(\#tab:moduleParams-fireSense-dataPrepPredict)List of (ref:fireSense-dataPrepPredict) parameters and their description.</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> paramName </th>
   <th style="text-align:left;"> paramClass </th>
   <th style="text-align:left;"> default </th>
   <th style="text-align:left;"> min </th>
   <th style="text-align:left;"> max </th>
   <th style="text-align:left;"> paramDesc </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> cutoffForYoungAge </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> 15 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Age at and below which pixels are considered 'young' (i.e., `age &lt;= cutoffForYoungAge`). </td>
  </tr>
  <tr>
   <td style="text-align:left;"> dataYear </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> 2011 </td>
   <td style="text-align:left;"> 1985 </td>
   <td style="text-align:left;"> 2022 </td>
   <td style="text-align:left;"> Year of the default landcover and stand age maps, and last year of the fires used to initialise `nonForest_timeSinceDisturbance`. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> fireTimeStep </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Interval between events of this module, in years. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> forestedLCC </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> 81, 210,.... </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Forested landcover classes in `rstLCC_RTM`. Only used if `landcoverDT` is not supplied. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> flammabilityThreshold </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> 0.1 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Minimum proportion of flammable pixels for an upscaled pixel to be flammable, when building the default landcover. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> fuelClassCol </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> FuelClass </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Column of `sppEquiv` that defines the fuel classes, for both ignition and spread. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> igAggFactor </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> 4 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Aggregation factor for the ignition and escape covariates. Overwritten in `init` by the value set in other modules. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> nonflammableLCC </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> 0, 20, 3.... </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Non-flammable landcover classes, used to create `flammableRTM` and the default landcover if not supplied. Defaults are water, snow/ice, rock and barren land in NTEMS LCC. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> nonForestCanBeYoungAge </td>
   <td style="text-align:left;"> logical </td>
   <td style="text-align:left;"> TRUE </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Should burned non-forest pixels be `youngAge` until `cutoffForYoungAge`? </td>
  </tr>
  <tr>
   <td style="text-align:left;"> sppEquivCol </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> LandR </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Column of `sppEquiv` with the species names used in `cohortData`. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> whichModulesToPrepare </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> fireSens.... </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Predict modules to prepare covariates for: `fireSense_IgnitionPredict` or `fireSense_EscapePredict` for the ignition/escape table, `fireSense_SpreadPredict` for the spread table. Defaults to all three. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> .runInitialTime </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Time of the first climate and covariate preparation events. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> .useCache </td>
   <td style="text-align:left;"> logical </td>
   <td style="text-align:left;"> FALSE </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Should this entire module be run with caching activated? This is generally intended for data-type modules, where stochasticity and time are not relevant </td>
  </tr>
</tbody>
</table>

### Events

All events after `init`, except `save`, repeat every `fireTimeStep` years.

- `init`: aligns `standAgeMap` and `rstLCC_RTM` to `rasterToMatch`; builds `landcoverDT` if absent; builds `nonForest_timeSinceDisturbance` if absent, from the fire polygons of the `cutoffForYoungAge` years up to `dataYear`.
- `getClimateRasters` (from `.runInitialTime`): a supplied `currentClimateRasters` (e.g. from the `climateYear` module) is left alone. If it is absent, or this module built it for another year, takes layer `year<Y>` of each element of `projectedClimateRasters`, where `Y` is `climateYear` if supplied, else `time(sim)`. Stops if it does not match `pixelGroupMap`.
- `prepIgAndEscPredictData` (from `.runInitialTime`): builds `fireSense_igAndEscapePred_Covariates`. Scheduled if `whichModulesToPrepare` has `fireSense_IgnitionPredict` or `fireSense_EscapePredict`.
- `prepSpreadPredictData` (from `.runInitialTime`): builds `fireSense_SpreadCovariates`. Scheduled if `whichModulesToPrepare` has `fireSense_SpreadPredict`.
- `ageNonForest` (from `time(sim) + 1`): adds 1 to `nonForest_timeSinceDisturbance` and resets pixels burned in `rstCurrentBurn` to 0.
- `save`: does nothing except emit a message. The module never schedules it.

The module does not plot or save anything.

### Module outputs

Description of the module outputs (Table \@ref(tab:moduleOutputs-fireSense-dataPrepPredict)).

<table class="table" style="margin-left: auto; margin-right: auto;">
<caption>(\#tab:moduleOutputs-fireSense-dataPrepPredict)(\#tab:moduleOutputs-fireSense-dataPrepPredict)List of (ref:fireSense-dataPrepPredict) outputs and their description.</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> objectName </th>
   <th style="text-align:left;"> objectClass </th>
   <th style="text-align:left;"> desc </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> currentClimateRasters </td>
   <td style="text-align:left;"> SpatRaster </td>
   <td style="text-align:left;"> Climate layers for the current year, one layer per climate variable. Built from `projectedClimateRasters` if not supplied. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> fireSense_igAndEscapePred_Covariates </td>
   <td style="text-align:left;"> data.table </td>
   <td style="text-align:left;"> Ignition and escape covariates at the aggregated (`igAggFactor`) resolution; `pixelID` is the cell index of the aggregated raster. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> fireSense_SpreadCovariates </td>
   <td style="text-align:left;"> data.table </td>
   <td style="text-align:left;"> Spread covariates; `pixelID` is the cell index of `flammableRTM`. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> nonForest_timeSinceDisturbance </td>
   <td style="text-align:left;"> SpatRaster </td>
   <td style="text-align:left;"> Years since last burn, used to set `youngAge` in non-forest pixels. </td>
  </tr>
</tbody>
</table>

### Links to other modules

Runs after *Biomass_borealDataPrep*, *fireSense_dataPrepFit*, *fireSense_IgnitionFit* and *fireSense_SpreadFit*, and supplies *fireSense_IgnitionPredict*, *fireSense_EscapePredict* and *fireSense_SpreadPredict*.
It is normally run as part of the [fireSense](https://github.com/PredictiveEcology/fireSense) module group.

### Getting help

- <https://github.com/PredictiveEcology/fireSense_dataPrepPredict/issues>

## References

<!-- autogenerated from bibligraphy -->
