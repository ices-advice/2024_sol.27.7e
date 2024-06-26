### ------------------------------------------------------------------------ ###
### Prepare plots and tables for report ####
### ------------------------------------------------------------------------ ###


## Before:
## After:

library(icesTAF)

mkdir("report")

source.taf("report_assessment.R")

source.taf("report_forecast.R")

source.taf("report_forecast_change.R")

### standard graphs
source.taf("report_sg.R")

# done
