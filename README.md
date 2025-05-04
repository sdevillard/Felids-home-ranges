# Felids-home-ranges
Data and code for the paper "Global determinants of home range sizes in Felids: evidence of human disturbance impact"


# Title of Dataset: A global dataset of felid home range and core area.

***

Felid home range and core area datasets were primarily built upon the HomeRange database by Broekman et al. (2023) with additional records sourced from an extensive literature search. These records were carefully corrected, updated, and supplemented to improve data coverage and accuracy. Only individual home range values are presented. Associated values were also compiled including species names, methodological information on data collection, home-range estimation method, period of data collection, study coordinates and name of location, as well as species traits derived from the studies, such as body mass, life stage and reproductive status.
Environmental variables were extracted for each geographical location within a species specific buffer based on median dispersal. Environmental variables were scaled. 
Species specific body mass was collected from Johnson et al. (2017) and Pacifici et al. (2012) and then logged.
Both datasets were used in the accompanying code files to run Generalized Linear Mixed Models.
## Data and File Overview
File List:

1. Felids\_core\_area\_data\_species_2025\_05\_02.xlsx
2. Felids\_home\_range\_data\_species_2025\_05\_02.xlsx
3. Felids\_MODEL\_CORE\_AREA.R
4. Felids\_MODEL\_HOME\_RANGE.R

## Data-specific information for :Felids\_home\_range\_data\_species_2025\_05\_02.xlsx and Felids\_core\_area\_data\_species_2025\_05\_02.xlsx

Number of variables: 69


The datasets used in this study contain information on individual- and species-level traits, methodological descriptors, and ecological and anthropogenic covariates for 69 variables related to felid home range and core area size estimation. They were curated by collecting and filtering records from the HomeRange database (Broekman et al., 2023) and literature, following strict criteria detailed in the Methods section of the associated paper. These datasets, separated by isopleth size (95% and 100% for Home Range and 50% for Core Area), are used for the statistical analysis of Home range and Core area size in felids.


The variables included:

- A: `Species`
- B: `key`
- C: `yearStart`
- D: `subspecies`
- E: `Ind_ID`
- F: `hr`
- G: `HR_Spread_km2`
- H: `Spread_Units`
- I: `HR_Level`
- J: `HR_Source`
- K: `HR_Span`
- L: `No_Individuals`
- M: `No_HR`
- N: `Sex`
- O: `Life_Stage`
- P: `Reproductive_Status`
- Q: `bm`
- R: `Context`
- S: `Context_Details`
- T: `Locomotion.x`
- U: `Latitude`
- V: `Longitude`
- W: `LatLong_Source`
- X: `dayStart`
- Y: `monthStart`
- Z: `dayEnd`
- AA: `monthEnd`
- AB: `yearEnd`
- AC: `tm`
- AD: `HR_Method`
- AE: `hrm`
- AF: `Isopleth_Size`
- AG: `HR_Software`
- AH: `nloc`
- AI: `SE_No_Locations_Used`
- AJ: `Comments`
- AK: `AUTHORS`
- AL: `TITLE`
- AM: `JOURNAL`
- AN: `YEAR`
- AO: `VOLUME`
- AP: `NUMBER`
- AQ: `PAGES`
- AR: `PUBLISHER`
- AS: `ADDRESS`
- AT: `ISSN`
- AU: `DOI_OR_LINK`
- AV: `PUBLICATION_TYPE`
- AW: `indtime`
- AX: `Additional_Comments`
- AY: `HR_mean_sp`
- AZ: `Disp_median`
- BA: `Buffer_area_km2`
- BB: `npp`
- BC: `ele`
- BD: `hfi`
- BE: `Study_ID`
- BF: `Country`
- BG: `Locality`
- BH: `hpd`
- BI: `rd`
- BJ: `fr`
- BK: `ps`
- BL: `cr`
- BM: `ID`
- BN: `Genus.x`
- BO: `abm_sex`
- BP: `log_abm_sex`
- BQ: `log_nloc`

For detailed definitions and units of most variables originating from the original HomeRange database, we refer readers to the accompanying metadata published in:

> Broekman, Maarten; Hoeks, Selwyn; Freriks, Rosa et al. (2023). *HomeRange: A global database of mammalian home ranges* [Dataset]. Dryad. https://doi.org/10.5061/dryad.d2547d85x

The following variables are present in the original HomeRange database but their names were changed: 

- F: `hr` <- Home_Range_km2 
- Q: `bm` <- Body_mass_kg
- AC: `tm` <- Tracking_Method 
- AE: `hrm` <- HR_Method_Simple 
- AH: `nloc` <- Mean_No_Locations_Used

`key`: Unique identifier combining study location, Study_ID, and species name to ensure traceability of records.

`indtime`: Independence time between locations, used to assess sampling effort consistency (not used).

`Additional_Comments`: User-added comments relevant to data quality, context, or clarification of individual records.

`HR_mean_sp`: Mean home range size per species, calculated from available HR records.

`Disp_median`: Median dispersal distance at the species level used to create buffer (see Methods).

`Buffer_area_km2`: Area of the buffer (in km²) used for extracting environmental covariates around home range locations.

`npp`: Net Primary Productivity extracted for each location.

`ele`: Elevation extracted for each location.

`hfi`: Human Footprint Index extracted for each location.

`hpd`: Human population density extracted for each location.

`rd`: Road density extracted for each location.

`fr`: Felid richness extracted for each location.

`ps`: Pastures extracted for each location.

`cr`: Croplands extracted for each location.

`ID`: Cleaned and corrected version of Ind_ID, used for consistent identification.

`Genus.x`: Genus name of the species.

`abm_sex`: Species-specific mean body mass (kg); sex-specific when available.

`log_abm_sex`: Log-transformed version of abm_sex, used for modeling.

`log_nloc`: Log-transformed number of locations, used for modeling.

Reference list can be extracted from the following columns : 

- AK: `AUTHORS`
- AL: `TITLE`
- AM: `JOURNAL`
- AN: `YEAR`
- AO: `VOLUME`
- AP: `NUMBER`
- AQ: `PAGES`
- AR: `PUBLISHER`
- AS: `ADDRESS`
- AT: `ISSN`
- AU: `DOI_OR_LINK`
- AV: `PUBLICATION_TYPE`

For data preparation code (assembling and filtering) feel free to contact the corresponding author.


