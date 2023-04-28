## Integrated resource selection for grassland songbirds
This repository contains data and code for the manuscript titled "The long shadow of woody encroachment: an integrated approach to modeling grassland songbird habitat".

Silber, K.M.*, T.J. Hefley, H. N. Castro-Miller, Z. R. Ratajczak, W.A. Boyle 

*Corresponding author. For any questions about the data or code, please contact Katy Silber (ksilber@ksu.edu).


### Data
###### VegetationComposition_RawData.csv



### Code

To organize the pieces of this integrated model, each level of the model is house in a separate R script. Many of the functions build on previous levels, so here is the general order of the script files:

###### 1. RSFFunctions.R
This file contains many of the important libraries and functions that are used throughout the other R scripts. Load these functions first. 
###### 2. PrepVegetationData.R
The first step of the integrated model is cleaning the vegetation data. 
###### 3. PredictCovariates_FitResourceSelectionFunction.R
Using the cleaned vegetation data, we can fit resource selection models to the vegetation and topography data.
###### 4. AssessModelPredictiveAccuracy.R
Assess the predictive accuracy of the resource selection model using a hold-set of Grasshopper Sparrow from 2021.
###### 5. WoodyPlantRemovalScenarios.R
Run a series of woody plant removal scenarios to quanitfy improved habitat.
###### 6. ResourceSelectionPlots.R
Make plots from the resource selection model output.
