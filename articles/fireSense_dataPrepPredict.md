---
title: "fireSense_dataPrepPredict Manual"
subtitle: "v.1.0.2.9000"
date: "Last updated: 2026-09-14"
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

<!-- TODO -->
fireSense [@Marchal:2017a; @Marchal:2017b; @Marchal:2019]

Provide a brief summary of what the module does / how to use the module.

Module documentation should be written so that others can use your module.
This is a template for module documentation, and should be changed to reflect your module.

### Module inputs and parameters

Describe input data required by the module and how to obtain it (e.g., directly from online sources or supplied by other modules)
If `sourceURL` is specified, `downloadData("fireSense_dataPrepPredict", "..")` may be sufficient.

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
   <td style="text-align:left;"> A list detailing which climate variables in `sim$projectedClimateRasters` to use for which fire processes (ignition and spread). If the list is length one, both processes will use the same variables. The default is to use 'MDC'. </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> climateYear </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> optional character vector giving year (e.g. 'year2009') for preparing the `currentClimateRasters` object. If unsupplied, `time(sim)` is used. see PredictiveEcology/climateYear </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> currentClimateRasters </td>
   <td style="text-align:left;"> SpatRaster </td>
   <td style="text-align:left;"> NAin this module (if this is absent) from projectedClimateRasters </td>
   <td style="text-align:left;"> SpatRaster of climate layers at current time of sim; this will be generated </td>
  </tr>
  <tr>
   <td style="text-align:left;"> cohortData </td>
   <td style="text-align:left;"> data.table </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> table that defines the cohorts by pixelGroup </td>
  </tr>
  <tr>
   <td style="text-align:left;"> fireSense_IgnitionFitted </td>
   <td style="text-align:left;"> fireSense_IgnitionFit </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> object containing slot `fittingRes` - the spatial resolution at which ignition will be predicted </td>
  </tr>
  <tr>
   <td style="text-align:left;"> missingLCCgroup </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> if a pixel is forested but is absent from `cohortData`, it will be grouped in this class. It can be estimated if `P(sim)$estimateFuelClasses` is TRUE. If supplied, it must be one of the names in `sim$nonForestedLCCGroups` </td>
  </tr>
  <tr>
   <td style="text-align:left;"> flammableRTM </td>
   <td style="text-align:left;"> SpatRaster </td>
   <td style="text-align:left;"> Flammable landcover i.e, conditions at start(sim). Taken from last layer of rstLCCs </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> nonForestedLCCGroups </td>
   <td style="text-align:left;"> list </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> a named list of non-forested landcover groups, e.g. `list('wetland' = c(19, 23, 32))`. This is only relevant if `landcoverDT` is not supplied </td>
  </tr>
  <tr>
   <td style="text-align:left;"> lightningMaps </td>
   <td style="text-align:left;"> SpatRaster </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> A 4-layer SpatRaster of lightning: lightningDays, lightningDensity, positiveCG, positiveCGdensity </td>
  </tr>
  <tr>
   <td style="text-align:left;"> pixelGroupMap </td>
   <td style="text-align:left;"> SpatRaster </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> SpatRaster that defines the pixelGroups for cohortData table </td>
  </tr>
  <tr>
   <td style="text-align:left;"> projectedClimateRasters </td>
   <td style="text-align:left;"> list </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> list of projected climate variables in raster stack form named according to variable, with names of individual raster layers following the convention 'year&lt;year&gt;'; this will only be used if `currentClimateRasters` is  not supplied </td>
  </tr>
  <tr>
   <td style="text-align:left;"> propFlammable </td>
   <td style="text-align:left;"> SpatRaster </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> a conditional object created if rstLCC is also not supplied,  a raster representing the proportion of flammable landcover in a pixel </td>
  </tr>
  <tr>
   <td style="text-align:left;"> rasterToMatch </td>
   <td style="text-align:left;"> SpatRaster </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> template raster used only to derive `flammableRTM` if the latter is absent </td>
  </tr>
  <tr>
   <td style="text-align:left;"> rstCurrentBurn </td>
   <td style="text-align:left;"> SpatRaster </td>
   <td style="text-align:left;"> binary raster with 1 representing annual burn </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> rstLCC_RTM </td>
   <td style="text-align:left;"> SpatRaster </td>
   <td style="text-align:left;"> a landcover raster - only used if `landcoverDT` is not supplied </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> sppEquiv </td>
   <td style="text-align:left;"> data.table </td>
   <td style="text-align:left;"> table of LandR species equivalencies </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> standAgeMap </td>
   <td style="text-align:left;"> SpatRaster </td>
   <td style="text-align:left;"> stand age map in study area; assumed to be ages at `start(sim)` </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> landcoverDT </td>
   <td style="text-align:left;"> data.table </td>
   <td style="text-align:left;"> `pixelID` and relevant landcover classes for flammable pixels in each layer, </td>
   <td style="text-align:left;"> i.e, conditions at start(sim). Taken from last layer of landcoverDTs </td>
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
   <td style="text-align:left;"> Used to override the default 'sourceURL' of NTEMS data for objects when not supplied </td>
  </tr>
  <tr>
   <td style="text-align:left;"> fireTimeStep </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> time step of fire model </td>
  </tr>
  <tr>
   <td style="text-align:left;"> forestedLCC </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> 81, 210,.... </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> forested landcover classes in `rstLCC` - only relevant if `landcoverDT` is not supplied </td>
  </tr>
  <tr>
   <td style="text-align:left;"> flammabilityThreshold </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> 0.1 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Minimum proportion of flammable old pixel needed to define a new pixel as flammable when upscaling the default flammable maps`. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> fuelClassCol </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> FuelClass </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> the column in sppEquiv that defines unique fuel classes for ignition </td>
  </tr>
  <tr>
   <td style="text-align:left;"> igAggFactor </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> 4 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> aggregation factor for rasters during ignition prep. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> nonflammableLCC </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> 0, 20, 3.... </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> used to create flammableRTM if unsupplied. The non-flammable LCC in rstLCC layers - which default to water, snow/ice, rock, and barren land in NTEMS LCC </td>
  </tr>
  <tr>
   <td style="text-align:left;"> nonForestCanBeYoungAge </td>
   <td style="text-align:left;"> logical </td>
   <td style="text-align:left;"> TRUE </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> update non-forest when burned, to become youngAge </td>
  </tr>
  <tr>
   <td style="text-align:left;"> sppEquivCol </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> LandR </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> column name in `sppEquiv` object that defines unique species in `cohortData` </td>
  </tr>
  <tr>
   <td style="text-align:left;"> whichModulesToPrepare </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> fireSens.... </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Which fireSense fit modules to prep? defaults to all 3 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> .plotInitialTime </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Describes the simulation time at which the first plot event should occur. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> .plotInterval </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Describes the simulation time interval between plot events. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> .runInitialTime </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> time to simulate initial fire </td>
  </tr>
  <tr>
   <td style="text-align:left;"> .saveInitialTime </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Describes the simulation time at which the first save event should occur. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> .saveInterval </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> This describes the simulation time interval between save events. </td>
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

<!-- TODO -->
Describe what happens for each event type.

### Plotting

<!-- TODO -->
Write what is plotted.

### Saving

<!-- TODO -->
Write what is saved.

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
   <td style="text-align:left;"> SpatRaster of climate layers at current time of sim; this will be generated in this module (if this is absent) from projectedClimateRasters </td>
  </tr>
  <tr>
   <td style="text-align:left;"> fireSense_igAndEscapePred_Covariates </td>
   <td style="text-align:left;"> data.table </td>
   <td style="text-align:left;"> data.table of covariates for ignition prediction, with pixelID column corresponding to flammableRTM pixel index </td>
  </tr>
  <tr>
   <td style="text-align:left;"> fireSense_SpreadCovariates </td>
   <td style="text-align:left;"> data.table </td>
   <td style="text-align:left;"> data.table of covariates for spread prediction, with pixelID column corresponding to flammableRTM pixel index </td>
  </tr>
  <tr>
   <td style="text-align:left;"> nonForest_timeSinceDisturbance </td>
   <td style="text-align:left;"> SpatRaster </td>
   <td style="text-align:left;"> time since burn for non-forest pixels </td>
  </tr>
</tbody>
</table>

### Links to other modules

<!-- TODO: link to other fireSense modules -->
Describe any anticipated linkages to other modules, such as modules that supply input data or do post-hoc analysis.

### Getting help

- <https://github.com/PredictiveEcology/fireSense_dataPrepPredict/issues>

## References

<!-- autogenerated from bibligraphy -->
