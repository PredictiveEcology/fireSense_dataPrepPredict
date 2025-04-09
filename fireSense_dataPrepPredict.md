---
title: "fireSense_dataPrepPredict Manual"
subtitle: "v.1.0.1"
date: "Last updated: 2025-04-08"
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

<table class="table" style="color: black; margin-left: auto; margin-right: auto;">
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
   <td style="text-align:left;"> cohortData </td>
   <td style="text-align:left;"> data.table </td>
   <td style="text-align:left;"> table that defines the cohorts by pixelGroup </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> flammableRTM </td>
   <td style="text-align:left;"> SpatRaster </td>
   <td style="text-align:left;"> RTM without ice/rocks/urban/water. Flammable map with 0 and 1. </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> nonForestedLCCGroups </td>
   <td style="text-align:left;"> list </td>
   <td style="text-align:left;"> a named list of non-forested landcover groups, e.g. `list('wetland' = c(19, 23, 32))`. This is only relevant if `landcoverDT` is not supplied </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> nonForest_timeSinceDisturbance </td>
   <td style="text-align:left;"> SpatRaster </td>
   <td style="text-align:left;"> time since burn for non-forested pixels </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> pixelGroupMap </td>
   <td style="text-align:left;"> SpatRaster </td>
   <td style="text-align:left;"> SpatRaster that defines the pixelGroups for cohortData table </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> projectedClimateRasters </td>
   <td style="text-align:left;"> list </td>
   <td style="text-align:left;"> list of projected climate variables in raster stack form named according to variable, with names of individual raster layers following the convention 'year&lt;year&gt;' </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> landcoverDT </td>
   <td style="text-align:left;"> data.table </td>
   <td style="text-align:left;"> data.table with `pixelID` and relevant landcover classes </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> rasterToMatch </td>
   <td style="text-align:left;"> SpatRaster </td>
   <td style="text-align:left;"> template raster used only to derive `flammableRTM` if the latter is absent </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> rstCurrentBurn </td>
   <td style="text-align:left;"> SpatRaster </td>
   <td style="text-align:left;"> binary raster with 1 representing annual burn </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> rstLCC </td>
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
</tbody>
</table>

Summary of user-visible parameters (Table \@ref(tab:moduleParams-fireSense-dataPrepPredict))


<table class="table" style="color: black; margin-left: auto; margin-right: auto;">
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
   <td style="text-align:left;"> ignitionFuelClassCol </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> FuelClass </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> the column in sppEquiv that defines unique fuel classes for ignition </td>
  </tr>
  <tr>
   <td style="text-align:left;"> missingLCCgroup </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> nonFores.... </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> if a pixel is forested but is absent from `cohortData`, it will be grouped in this class. Must be one of the names in `sim$nonForestedLCCGroups`. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> nonflammableLCC </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> 20, 31, .... </td>
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
   <td style="text-align:left;"> spreadFuelClassCol </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> FuelClass </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> if using fuel classes for spread, the column in sppEquiv that defines unique fuel classes </td>
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

<table class="table" style="color: black; margin-left: auto; margin-right: auto;">
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
   <td style="text-align:left;"> list </td>
   <td style="text-align:left;"> list of project climate rasters at current time of sim </td>
  </tr>
  <tr>
   <td style="text-align:left;"> fireSense_IgnitionAndEscapeCovariates </td>
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
