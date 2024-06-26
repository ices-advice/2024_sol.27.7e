### ------------------------------------------------------------------------ ###
### load data from VPA files and create FLR input objects ####
### ------------------------------------------------------------------------ ###


## Before: VPA input files in boot/data/vpa_files/:
##           SOL7ECN.DAT SOL7ECW.DAT SOL7EDA.DAT SOL7EIND.DAT SOL7ELA.DAT
##           SOL7EMO.dat SOL7ENM.DAT SOL7EPF.DAT SOL7EPM.DAT SOL7ESW.DAT
##           SOL7ETU3a.dat

## After: data/model_input_stk.rds
##        data/model_input_idx.rds
##        data/model_input_discards.rds


### ------------------------------------------------------------------------ ###
### load data for XSA assessment ####
### ------------------------------------------------------------------------ ###

### load packages
suppressPackageStartupMessages(library(icesTAF))
taf.libPaths()
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(FLCore))

### load additional functions
source("utilities.R")

### ------------------------------------------------------------------------ ###
### load stock data ####
### ------------------------------------------------------------------------ ###

### read data
stock <- readFLStock(paste0("boot/data/vpa_files/SOL7EIND.DAT"),
                     no.discards = TRUE, quiet = TRUE)
### COMMENT FOR 2025
### argument no.discards = TRUE re-calculates total catches (and landings)
### with .wt * .n summed up and ignores catch values in SOL7ELA.DAT file
### -> negligible changes but consider changing for consistency

### define fbar age range
range(stock)[c("minfbar", "maxfbar")] <- c(3, 9)

### set units
units(stock)[1:17] <- as.list(c(rep(c("tonnes", "thousands", "kg"), 4),
                                "NA", "NA", "f", "NA", "NA"))
units <- units(stock)

### set plusgroup
stock <- setPlusGroup(stock, 12)

# summary(stock)

### ------------------------------------------------------------------------ ###
### SOP correction ####
### ------------------------------------------------------------------------ ###
SOP_factor <- landings(stock)/catch(stock)
# SOP_factor

### correct wts
landings.wt(stock) <- catch.wt(stock) <- landings.wt(stock) * 
  rep(c(SOP_factor), each = dims(stock)$age)

# computeLandings(stock)/landings(stock)
### update total catch with imported catch values
catch(stock) <- computeCatch(stock)

### set units again
units(stock) <- units

### ------------------------------------------------------------------------ ###
### read and process survey indices ####
### ------------------------------------------------------------------------ ###

### load indices
idx_all <- readFLIndices("boot/data/vpa_files/SOL7ETU3a.dat")
# summary(idx_all)

### keep only required indices
idx <- idx_all[c("UK-CBT-late", "UK-COT", "Q1SWBeam-nonoffset", "FSP-UK")]

### trim ages and years according to WGCSE 
### FSP year >= 2004, ages 2-11
idx[["FSP-UK"]] <- window(idx[["FSP-UK"]], start = 2004)
idx[["FSP-UK"]] <- trim(idx[["FSP-UK"]], age = 2:11)
### Q1SWBeam ages 2-11
idx[["Q1SWBeam-nonoffset"]] <- trim(idx[["Q1SWBeam-nonoffset"]], age = 2:11)
index(idx[["Q1SWBeam-nonoffset"]])[, ac(2022)] <- NA ### reduced survey area

### UK-CBT-late ages 3-11
idx[["UK-CBT-late"]] <- trim(idx[["UK-CBT-late"]], age = 3:11)
### UK-COT ended in 2015
idx[["UK-COT"]] <- window(idx[["UK-COT"]], end = 2015)

# summary(idx)

### ------------------------------------------------------------------------ ###
### load discards ####
### ------------------------------------------------------------------------ ###
discards <- readVPAFile("boot/data/vpa_files/SOL7EDA.DAT")
units(discards) <- "tonnes"


### ------------------------------------------------------------------------ ###
### save model input files ####
### ------------------------------------------------------------------------ ###

saveRDS(object = stock, file = "data/model_input_stk.rds")
saveRDS(object = idx, file = "data/model_input_idx.rds")
saveRDS(object = discards, file = "data/model_input_discards.rds")

