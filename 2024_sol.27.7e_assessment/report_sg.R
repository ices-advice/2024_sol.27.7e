### ------------------------------------------------------------------------ ###
### create ICES standard graphs for advice sheet ####
### ------------------------------------------------------------------------ ###

## Before: report/tables_sag.csv
## After:  report/standard_graphs/sol.27.7e.xml
##         report/standard_graphs/historical.png
##         report/standard_graphs/stock_plot.png
##         report/standard_graphs/stock_status.png

### load packages
suppressPackageStartupMessages(library(icesTAF))
taf.libPaths()
suppressPackageStartupMessages(library(icesSAG))
suppressPackageStartupMessages(library(Cairo))

### load additional functions
source("utilities.R")

mkdir("report/standard_graphs")

if (!exists("verbose")) verbose <- FALSE 

### ------------------------------------------------------------------------ ###
### create SAG objects ####
### ------------------------------------------------------------------------ ###
### assessment year
ass_yr <- 2024

### list of possible elements:
### https://datsu.ices.dk/web/selRep.aspx?Dataset=126
### allowed units:
### https://vocab.ices.dk/?ref=155

### set up stock info
stk_info <- stockInfo(
  StockCode = "sol.27.7e", 
  AssessmentYear = ass_yr, 
  ContactPerson = "simon.fischer@cefas.gov.uk", 
  Purpose = "Advice",
  StockCategory = 1,
  ModelType = "A",
  ModelName = "XSA"
)

### add some more data manually
stk_info$MSYBtrigger <- 2900
stk_info$FMSY <- 0.29
stk_info$Blim <- 2000
stk_info$Flim <- 0.44
stk_info$Bpa <- 2900
stk_info$Fpa <- 0.39 ### changed in 2021 to Fp.05
stk_info$Fage <- "3-9"
stk_info$RecruitmentAge <- 2
stk_info$RecruitmentDescription <- "Recruitment"
stk_info$RecruitmentUnits <- "NE3" ### NE3 stands for thousands
stk_info$CatchesLandingsUnits <- "t" ### t for tonnes
#stk_info$FishingPressureUnits <- "ages 3-9"
stk_info$StockSizeUnits <- "t" ### t for tonnes
stk_info$StockSizeDescription <- "SSB"
stk_info$FishingPressureDescription <- "F"
stk_info$Purpose <- "Advice"
stk_info$ModelType <- "A"
stk_info$ModelName <- "XSA"

### load data from assessment/forecast
sag <- read.csv(file = "report/tables_sag.csv", as.is = TRUE)

### set up data
# https://datsu.ices.dk/web/selRep.aspx?Dataset=126  # Record: AF - Fish Data
stk_data <- stockFishdata(
  Year = sag$Year,
  Recruitment = sag$Recruitment,
  TBiomass = sag$TBiomass,
  StockSize = sag$StockSize,
  Landings = sag$Landings,
  Discards = sag$Discards,
  FishingPressure = sag$FishingPressure
)

### save as XML file
xmlfile <- createSAGxml(stk_info, stk_data)
writeLines(xmlfile, "report/standard_graphs/sol.27.7e.xml")
### this file can be manually uploaded at 
### https://standardgraphs.ices.dk/manage/index.aspx
### Alternatively: do it all from R

### ------------------------------------------------------------------------ ###
### automatic upload of data and configuration of plots/data ####
### ------------------------------------------------------------------------ ###

### ICES standard graphs
### create token for authentication
### go to https://standardgraphs.ices.dk/manage/index.aspx
### login
### click on create token or go directly to 
### https://standardgraphs.ices.dk/manage/CreateToken.aspx
### create new token, save in file
# file.edit("~/.Renviron")
### in the format 
### SG_PAT=some_token_......
### save and restart R

if (isTRUE(verbose)) {
  
  ### load token
  Sys.getenv("SG_PAT")
  options(icesSAG.use_token = TRUE)
  
  ## check assessments keys
  key <- findAssessmentKey("sol.27.7e", year = ass_yr)
  key_last <- findAssessmentKey("sol.27.7e", year = ass_yr - 1)
  
  ### last year's graphs
  #plot(getSAGGraphs(key_last))
  settings_last <- getSAGSettingsForAStock(key_last)
  
  ### upload 
  key_new <- uploadStock(info = stk_info, fishdata = stk_data, verbose = TRUE)
  key_new <- findAssessmentKey('sol.27.7e', ass_yr, full = TRUE)$AssessmentKey
  
  ### check upload
  windows() ### RStudio's interal plot pane causes RStudio to crash...
  plot(getSAGGraphs(key_new))
  
  ### get chart settings 
  ### should be automatically copied from last year
  chart_settings <- getSAGSettingsForAStock(key_new) 
  ### compare with last years settings
  settings_last <- getSAGSettingsForAStock(key_last)
  all.equal(chart_settings, settings_last)
  ### yes, identical (only new assessment key)
  
  ### modify chart settings
  ### possible options listed here: 
  ### https://standardgraphs.ices.dk/manage/ListSettings.aspx
  ### 10-year steps in all four plots
  ### and set same minimum/maximum
  # setSAGSettingForAStock(assessmentKey = key_new, chartKey = 0, settingKey = 35,
  #                        settingValue = 10, copyNextYear = FALSE)
  # setSAGSettingForAStock(assessmentKey = key_new, chartKey = 0, settingKey = 17,
  #                        settingValue = 1970, copyNextYear = FALSE)
  # setSAGSettingForAStock(assessmentKey = key_new, chartKey = 0, settingKey = 18,
  #                        settingValue = ass_yr, copyNextYear = FALSE)
  
  ### unshade intermediate year recruitment
  setSAGSettingForAStock(assessmentKey = key_new, chartKey = 2, settingKey = 14,
                         settingValue = ass_yr, copyNextYear = FALSE)
  
  ### check again
  getSAGSettingsForAStock(key_new) 
  windows()
  plot(getSAGGraphs(key_new))
  
  ### save plot(s) for advice sheet
  dir.create(paste0("report/standard_graphs/"), 
             recursive = TRUE)
  ### stock plot
  ### individual plots are 450 * 250 px
  png(filename = paste0("report/standard_graphs/", "stock_plot.png"), 
      width = 450*4, height = 250*4, units = "px", type = "cairo")
  plot(getSAGGraphs(key_new))
  dev.off()
  
  ### historical assessments
  ### each 300 * 300
  # png(filename = paste0("report/standard_graphs/", "historical.png"),
  #     width = 3*300, height = 300, units = "px", type = "cairo")
  # hist <- list(getSSBHistoricalPerformance(key_new)[[1]],
  #              getFishingMortalityHistoricalPerformance(key_new)[[1]],
  #              getRecruitmentHistoricalPerformance(key_new)[[1]])
  # plot.ices_standardgraph_list2(hist, c = 1, r = 3)
  # dev.off()
  
  ### stock status
  ### may need to be manually modified at sg.ices.dk to set F evaluation to "Below"
  ### 991 * 214
  png(filename = paste0("report/standard_graphs/", "stock_status_proxy.png"), 
      width = 991, height = 214, units = "px", type = "cairo")
  plot(getStockStatusTable(key_new))
  dev.off()
  
}



