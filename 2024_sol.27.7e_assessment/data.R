### ------------------------------------------------------------------------ ###
### Preprocess data, write TAF data tables ####
### ------------------------------------------------------------------------ ###

## Before: 
## After:

library(icesTAF)

### create folder to store data 
mkdir("data")

### load data required for stock assessment
source.taf("data_assessment.R")

