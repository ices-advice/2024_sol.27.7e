### ------------------------------------------------------------------------ ###
### Run XSA ####
### ------------------------------------------------------------------------ ###

## Before: data/model_input_stk.rds
##         data/model_input_idx.rds
## After: model/stock.rds
##        model/diags.rds
##        model/diags.txt
##        model/retro.rds

### ------------------------------------------------------------------------ ###
### run XSA assessment ####
### ------------------------------------------------------------------------ ###

### load packages
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(icesTAF))
taf.libPaths()
suppressPackageStartupMessages(library(FLCore))
suppressPackageStartupMessages(library(FLAssess))
suppressPackageStartupMessages(library(FLXSA))
suppressPackageStartupMessages(library(FLash))

### load additional functions
source("utilities.R")

### make model directory
mkdir("model")

### ------------------------------------------------------------------------ ###
### XSA
### ------------------------------------------------------------------------ ###

### load input objects
### stock object: FLStock
stk <- readRDS("data/model_input_stk.rds")
### index: FLIndices
idx <- readRDS("data/model_input_idx.rds")

### control object
control <- FLXSA.control(fse = 1.0, rage = 0, qage = 6, 
                         shk.n = FALSE, shk.f = TRUE, 
                         shk.ages = 5, shk.yrs = 3, 
                         min.nse = 0.4, 
                         tspower = 3, tsrange = 15,
                         maxit = 200)

### perform XSA
msg("running XSA")
xsa <- FLXSA(stock = stk, indices = idx, control = control)

### number of iterations
xsa@control@maxit

### stop here if model maxed out maxit
if (xsa@control@maxit >= control@maxit) stop("maxit reached")

### merge results with stock data
stk_new   <- stk + xsa
units(stk_new) <- units(stk)
### plot results
# plot(stk_new)

### save XSA results
saveRDS(stk_new, file = "model/stock.rds")
saveRDS(xsa, file = "model/diags.rds")

### save diagnostics to text file
writeLines(capture.output(diagnostics(xsa)), con = "model/diags.txt")

### ------------------------------------------------------------------------ ###
### run a retrospective analysis ###
### ------------------------------------------------------------------------ ###

msg("running retrospective")
### run retro
retro <- FLXSA_retro(stock = stk_new, index = idx, years = 5 + 1,
                     control = control)

### run retro including 1 year stf
retro_int <- FLXSA_retro2(stock = stk_new, index = idx, years = 5,
                          control = control,
                          int_yr = TRUE, int_rec_last = 0, int_rec_first = NULL)

### save retro
saveRDS(retro, file = "model/retro.rds")
saveRDS(retro_int, file = "model/retro_int.rds")

# done
