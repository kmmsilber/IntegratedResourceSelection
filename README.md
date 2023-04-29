## Integrated resource selection for grassland songbirds
This repository contains data and code for the manuscript titled "The long shadow of woody encroachment: an integrated approach to modeling grassland songbird habitat".

Silber, K.M.*, T.J. Hefley, H. N. Castro-Miller, Z. R. Ratajczak, W.A. Boyle 

*Corresponding author. For any questions about the data or code, please contact Katy Silber (ksilber@ksu.edu).


### Data

##### IntegratedResourceSelectionModelMetadata.docx
This file includes descriptions of the below datasets and the associated variables.

The following datasets were used within the analyses. 
###### /VegetationComposition_RawData.csv
Estimates of vegetation composition (i.e., % cover of plant functional groups) collected during monthly sampling events on Konza Prairie watersheds.
###### VegetationHeight_RawData.csv
Estimates of vegetation height (dm) collected during monthly sampling events on Konza Prairie watersheds.
###### BurnHistories.csv
Burn date and area for each study unit, 1972-2020.
###### WatershedCovariates2021.csv
Management regimes (i.e., fire and grazing) and precipitation for each study unit in 2021.
###### KNZPrecip.csv
Estimates of Spring precipitation, breeding season precipitation lagged one year, and breeding season precipitation lagged two years at the Konza Prairie. Raw dataset of daily precipitation amounts can be accessed here: http://lter.konza.ksu.edu/content/apt01-daily-precipitation-amounts-measured-multiple-sites-across-konza-prairie
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
Assess the predictive accuracy of the resource selection model using a hold-set of Grasshopper Sparrow from 2021.
###### 5. WoodyPlantRemovalScenarios.R
Run a series of woody plant removal scenarios to quanitfy improved habitat.
###### 6. ResourceSelectionPlots.R
Make plots from the resource selection model output.
