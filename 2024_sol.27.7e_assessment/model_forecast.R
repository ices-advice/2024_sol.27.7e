
### ------------------------------------------------------------------------ ###
### run short term forecast ####
### ------------------------------------------------------------------------ ###

## Before: model/stock.rds
## After: model/stk_list_int.rds
##        model/stk_list.rds
##        stf_settings.rds


### packages
suppressPackageStartupMessages(library(icesTAF))
taf.libPaths()
suppressPackageStartupMessages(library(FLCore))
suppressPackageStartupMessages(library(FLAssess))
suppressPackageStartupMessages(library(FLXSA))
suppressPackageStartupMessages(library(FLash))

### load additional functions
source("utilities.R")

### ------------------------------------------------------------------------ ###
### short term forecast ####
### ------------------------------------------------------------------------ ###

### load model results
stk_new <- readRDS("model/stock.rds")

### TAC for intermediate year (assessment WG year)
TAC_int <- 1184

### intermediate year
yr_int <- range(stk_new)[["maxyear"]] + 1

### ------------------------------------------------------------------------ ###
### reference values ####
### ------------------------------------------------------------------------ ###

### use rounded values published by WKMSYREF as requested by ADG 2018
F_MSY <- 0.29#1
F_MSY_upper <- 0.34#2
F_MSY_lower <- 0.16#0
MSY_B_trigger <- 2900#2826
B_LIM <- 2000#2039
B_PA <- 2900#2855
F_LIM <- 0.44
F_PA <- 0.39 ### updated in 2021, use Fp.05
F_MGT <- 0.27 ### Management plan

### ---------------------------------------------------------------------- ###
### calculate values for intermediate year ####
### ---------------------------------------------------------------------- ###

TACs <- read.csv("boot/data/TACs_sole_7e.csv")

### fbar age range
fbar_ages <- range(stk_new)[[c("minfbar")]]:range(stk_new)[[c("maxfbar")]]
### last (data) year
last_year <- range(stk_new)[["maxyear"]]
### projected years
proj_years <- c(c(last_year + 1):c(last_year + 3))

### fbar of last data year
fbar_last <- c(fbar(stk_new)[, ac(last_year)])

### mean harvest-at-age for last 3 years
f_target <- harvest(stk_new)[, ac((last_year - 2):last_year)]
f_target <- yearMeans(f_target)
### standardize by fbar range F
f_target <- f_target / mean(f_target[ac(fbar_ages), ])
### multiply last F
f_target <- f_target * fbar_last

### calculate Fbar from this
fbar_target <- quantMeans(f_target[ac(fbar_ages)])

### fbar mean of last 3 years
fbar_mean <- mean(fbar(stk_new[, ac((last_year - 2):last_year)]))

### geometric mean of recruitment
### 2021: use last 10 years
rec_geomean <- exp(mean(log(rec(stk_new))))
rec_geomean_10 <- exp(mean(log(tail(rec(stk_new), 10))))

### ------------------------------------------------------------------------ ###
### discard considerations ####
### ------------------------------------------------------------------------ ###

### discards
discards <- readRDS("data/model_input_discards.rds")

### calculate discard rate
catch <- window(catch(stk_new), start = dims(discards)$minyear)
discard_rate <- discards / (discards + catch)

### mean over last 3 years
discard_rate <- mean(tail(discard_rate, 3))

### adapt TAC for discards
TAC_int_landings <- TAC_int * (1 - discard_rate)

### ------------------------------------------------------------------------ ###
### extend stock for 3 years ####
### ------------------------------------------------------------------------ ###
### use default settings for weights@age: average of last 3 years
stk_stf <- stf(stk_new)

### ------------------------------------------------------------------------ ###
### intermediate year options ####
### ------------------------------------------------------------------------ ###
### 3 options:
### F of last 3 years
### F of last year
### TAC
### include catch advice year to see effect of intermediate year

### list with control objects
ctrl_int <- list(
  F_average = data.frame(year = proj_years, val = c(fbar_mean, rep(F_MSY, 2)),
                         quantity = "f"),
  F_last = data.frame(year = proj_years, val = c(fbar_last, rep(F_MSY, 2)),
                      quantity = "f"),
  TAC = data.frame(year = proj_years, val = c(TAC_int_landings, rep(F_MSY, 2)),
                   quantity = c("catch", rep("f", 2)))
  )

ctrl_int <- lapply(ctrl_int, fwdControl)

### predict 
stk_int <- lapply(ctrl_int, function(x) {
  res <- fwd(stk_stf, ctrl = x, 
             sr = list(model = "mean", params = FLPar(rec_geomean)))
  units(res) <- units(stk_new)
  return(res)
})

### get SSB and landings
sapply(stk_int, function(x) {c(landings(x)[, ac(yr_int)])})
sapply(stk_int, function(x) {c(ssb(x)[, ac(yr_int + 1)])})

### repeat for short geomean recruitment period (last 10 years)
### predict 
stk_int_10 <- lapply(ctrl_int, function(x) {
  res <- fwd(stk_stf, ctrl = x, 
             sr = list(model = "mean", params = FLPar(rec_geomean_10)))
  units(res) <- units(stk_new)
  return(res)
})
sapply(stk_int_10, function(x) {c(landings(x)[, ac(yr_int)])})
sapply(stk_int_10, function(x) {c(ssb(x)[, ac(yr_int + 1)])})

### ---------------------------------------------------------------------- ###
### select base line stf ####
### ---------------------------------------------------------------------- ###

### choose intermediate year option
stk_stf <- stk_int$TAC

int_target <- TAC_int_landings
int_quant <- "catch"

### ------------------------------------------------------------------------ ###
### list with control objects ####
### ------------------------------------------------------------------------ ###


### list with control objects
ctrl_list <- list(
  ### FMSY
  FMSY = data.frame(year = proj_years, 
                    val = c(int_target, rep(F_MSY, 2)),
                    quantity = c(int_quant, rep("f", 2))),
  ### FMSY lower
  FMSY_lower = data.frame(year = proj_years, 
                          val = c(int_target, rep(F_MSY_lower, 2)),
                          quantity = c(int_quant, rep("f", 2))),
  ### FMSY upper
  FMSY_upper = data.frame(year = proj_years, 
                          val = c(int_target, rep(F_MSY_upper, 2)),
                          quantity = c(int_quant, rep("f", 2))),
  ### Management plan, target Fmp & 15% TAC constraint
  MP2 = data.frame(year = c(proj_years[1], 
                            rep(proj_years[-1], each = 2)), 
                   val = c(int_target, 
                           rep(c(F_MGT, NA), 2)),
                   min = c(NA, rep(c(NA, 0.85), 2)),
                   max = c(NA, rep(c(NA, 1.15), 2)),
                   rel.year = c(NA,
                                rbind(NA, proj_years[-3])),
                   quantity = c(int_quant, rep(c("f", "catch"), 2))),
  ### Management plan, target Fmp & 15% TAC constraint
  ### TAC constraint only for last year...
  MP = data.frame(year = c(proj_years[1:2], 
                           rep(proj_years[3], each = 2)), 
                  val = c(int_target, F_MGT, 
                          F_MGT, NA),
                  min = c(rep(NA, 3), 0.85),
                  max = c(rep(NA, 3), 1.15),
                  rel.year = c(rep(NA, 3), proj_years[2]),
                  quantity = c(int_quant, rep("f", 2), "catch")),
  ### zero fishing
  F0 = data.frame(year = proj_years, 
                  val = c(int_target, rep(0, 2)),
                  quantity = c(int_quant, rep("f", 2))),
  ### Fsq * 0.6
  Fsq0.6 = data.frame(year = proj_years, 
                      val = c(int_target, 
                              rep(fbar_last*0.6, 2)),
                      quantity = c(int_quant, rep("f",2))),
  ### Fsq * 0.8
  Fsq0.8 = data.frame(year = proj_years, 
                      val = c(int_target, 
                              rep(fbar_last*0.8, 2)),
                      quantity = c(int_quant, rep("f",2))),
  ### TAC - 15%
  TAC085 = data.frame(year = proj_years, 
                      val = c(int_target, 
                              rep(TAC_int_landings*0.85, 2)),
                      quantity = c(int_quant, rep("catch", 2))),
  ### TAC + 15%
  TAC115 = data.frame(year = proj_years, 
                      val = c(int_target, 
                              rep(TAC_int_landings*1.15, 2)),
                      quantity = c(int_quant, rep("catch", 2))),
  ### TAC kept
  TAC = data.frame(year = proj_years, 
                   val = c(int_target, rep(TAC_int_landings, 2)),
                   quantity = c(int_quant, rep("catch", 2))),
  ### keep F constant after intermediate year
  Fsq = data.frame(year = proj_years, 
                   val = c(int_target, rep(1, 2)),
                   quantity = c(int_quant, rep("f", 2)),
                   rel.year = c(NA, rep(proj_years[1], 2))),
  ### Flim
  Flim = data.frame(year = proj_years, 
                    val = c(int_target, rep(F_LIM, 2)),
                    quantity = c(int_quant, rep("f", 2))),
  ### Fpa
  Fpa = data.frame(year = proj_years, 
                   val = c(int_target, rep(F_PA, 2)),
                   quantity = c(int_quant, rep("f", 2))),
  ### SSB = Bpa in forecast years
  Bpa = data.frame(year = proj_years, 
                   val = c(int_target, rep(B_PA, 2)),
                   quantity = c(int_quant, rep("ssb", 2))),
  ### SSB = MSY B trigger in forecast years
  Btrigger = data.frame(year = proj_years, 
                        val = c(int_target, 
                                rep(MSY_B_trigger, 2)),
                        quantity = c(int_quant, rep("ssb", 2))),
  ### SSB = Blim in forecast years
  Blim = data.frame(year = proj_years, 
                    val = c(int_target, rep(B_LIM, 2)),
                    quantity = c(int_quant, rep("ssb", 2))),
  ### keep SSB stable
  SSB_stable = data.frame(year = proj_years, 
                          val = c(int_target, 1, 1),
                          #min = c(NA, NA, NA),
                          rel.year = c(NA, proj_years[1], proj_years[1]),
                          quantity = c(int_quant, "ssb", "ssb"))
)

### add F targets in 0.01 steps for F_MSY range
### values in FMSY range
f_targets <- round(seq(from = F_MSY_lower, to = F_MSY_upper, by = 0.01), 2)
### remove existing values
f_targets <- setdiff(f_targets, c(F_MSY, F_MSY_lower, F_MSY_upper)) 

### create control object list
ctrl_ranges <- lapply(f_targets, function(x) {
  data.frame(year = proj_years,
             val = c(int_target, rep(x, 2)),
             quantity = c(int_quant, rep("f", 2)))
})
names(ctrl_ranges) <- paste0("F=", f_targets)

### combine with other scenarios
ctrl_list <- append(ctrl_list, ctrl_ranges)

### coerce into fwdControl
ctrl_list <- lapply(ctrl_list, fwdControl)

### ---------------------------------------------------------------------- ###
### project forward ####
### ---------------------------------------------------------------------- ###

stk_list <- lapply(ctrl_list, function(x) {
  
  res <- fwd(stk_stf, ctrl = x, maxF = 1e+99,
             sr = list(model = "mean", params = FLPar(rec_geomean)))
  units(res) <- units(stk_new) ### set units
  return(res)
  
})

### ---------------------------------------------------------------------- ###
### save results ####
### ---------------------------------------------------------------------- ###

### intermediate year options
saveRDS(stk_int, "model/stk_list_int.rds")
saveRDS(stk_int_10, "model/stk_list_int_10.rds")
### forecast options
saveRDS(stk_list, "model/stk_list.rds")
### some forecast settings
saveRDS(list(fbar_ages = fbar_ages, last_year = last_year,
             proj_years = proj_years, fbar_last = fbar_last,
             f_target = f_target, fbar_target = fbar_target, 
             fbar_mean = fbar_mean, rec_geomean = rec_geomean),
        "model/stf_settings.rds")

