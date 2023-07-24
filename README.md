## Integrated resource selection for grassland songbirds
This repository contains data and code for the manuscript titled "The long shadow of woody encroachment: an integrated approach to modeling grassland songbird habitat".

Silber, K.M.*, T.J. Hefley, H. N. Castro-Miller, Z. Ratajczak, W.A. Boyle 

*Corresponding author. For any questions about the data or code, please contact Katy Silber (kmmsilber@gmail.com).


### Data

##### IntegratedResourceSelectionModelMetadata.docx
This file includes detailed descriptions of the below datasets and the associated variables.

###### VegetationComposition_RawData.csv
Estimates of vegetation composition collected during in May, June, and July on Konza Prairie, 2014-2021. Vegetation data were collected from three (prior to 2017) or 10 randomly-selected locations on each watershed. We used 5 sets of Daubenmire frame measures to determine percent cover of major plant functional groups (at the center of the plot and 5 m from center at the 4 cardinal directions). Description from Konza Data Portal. Missing values = -9. The dataset used in this study is a subset; the full dataset can be accessed here: http://lter.konza.ksu.edu/content/cbs05-estimates-vegetation-structure-and-composition-collected-konza-prairie-watersheds-and
###### VegetationHeight_RawData.csv
Estimates of vegetation structure collected during in May, June, and July on Konza Prairie, 2014-2021. Vegetation data were collected from three (prior to 2017) or 10 randomly-selected locations on each watershed. We estimated visual obstruction by placing a Robel Pole in the middle, and 5 m from the middle of the plot in each of the 4 cardinal directions. For each pole placement, we stood 4 m away with eye 1 m above the ground in each of 4 directions, and counting the highest 5-cm segment not completely obscured by vegetation. Missing values = -9. The dataset used in this study is a subset; the full dataset can be accessed here: http://lter.konza.ksu.edu/content/cbs05-estimates-vegetation-structure-and-composition-collected-konza-prairie-watersheds-and
###### BurnHistories.csv
Burn date and area for each study unit, 1972-2020. This is the raw dataset that was downloaded in 2021; it can also be accessed here: http://lter.konza.ksu.edu/content/kfh01-konza-prairie-fire-history.
###### WatershedCovariates2021.csv
Management regimes (i.e., fire and grazing) and precipitation for each study unit at the Konza Prairie in 2021.
###### KNZPrecip.csv
Estimates of Spring precipitation, breeding season precipitation lagged one year, and breeding season precipitation lagged two years at the Konza Prairie, 1985-2021. Raw dataset of daily precipitation amounts can be accessed here: http://lter.konza.ksu.edu/content/apt01-daily-precipitation-amounts-measured-multiple-sites-across-konza-prairie
###### EasternMeadowlarkData.csv
Mark-resight data for Eastern Meadowlarks at the Konza Prairie, 2019-2021.
###### DickcisselData.csv
Mark-resight data for Dickcissels at the Konza Prairie, 2019-2021.
###### GrasshopperSparrowData.csv
Mark-resight data for Grasshopper Sparrows at the Konza Prairie, 2013-2020.
###### GRSPLocations2021.csv
Mark-resight data for Grasshopper Sparrows at the Konza Prairie, 2021.

### Code

To organize the pieces of this integrated model, each level of the model is housed in a separate R script. Many of the functions build on previous levels, so here is the general order of the script files:

###### 1. RSFFunctions.R
This file contains many of the important libraries and functions that are used throughout the other R scripts. Load these functions first. 
###### 2. PrepVegetationData.R
The first step of the integrated model is cleaning the vegetation data. 
###### 3. PredictCovariates_FitResourceSelectionFunction.R
Using the cleaned vegetation data, fit resource selection models to the vegetation and topography data.
###### 4. AssessModelPredictiveAccuracy.R
Assess the predictive accuracy of the vegetation models using a set of hold-out data from 2022 and resource selection model using a hold-set of Grasshopper Sparrow from 2021.
###### 5. WoodyPlantRemovalScenarios.R
Run a series of woody plant removal scenarios to quanitfy improved habitat.
###### 6. ResourceSelectionPlots.R
Make plots from the resource selection model output.
