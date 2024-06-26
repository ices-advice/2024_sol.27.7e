### ------------------------------------------------------------------------ ###
### create tables and plots with assessment results ####
### ------------------------------------------------------------------------ ###


## Before: model/stock.rds
##         model/diags.rds
##         data/model_input_idx.rds
##         model/retro.rds
##         model/retro_int.rds
##         boot/data/previous_results/diags_2017.rds
##         boot/data/previous_results/stock_2017.rds
##         boot/data/tuning/LPUE.csv
## After: dozens of plots in report/plots/ and tables in report/tables/

### load packages
suppressPackageStartupMessages(library(icesTAF))
taf.libPaths()
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(cowplot))
suppressPackageStartupMessages(library(foreach))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(tidyr))
suppressPackageStartupMessages(library(FLCore))
suppressPackageStartupMessages(library(FLAssess))
suppressPackageStartupMessages(library(FLXSA))
suppressPackageStartupMessages(library(FLash))

### load additional functions
source("utilities.R")

if (!exists("verbose")) verbose <- FALSE

### ------------------------------------------------------------------------ ###
### load assessment results ####
### ------------------------------------------------------------------------ ###

stk_new <- readRDS("model/stock.rds") ### stock object with XSA results
xsa <- readRDS("model/diags.rds") ### XSA result object
idx <- readRDS("data/model_input_idx.rds") ### tuning information
retro <- readRDS("model/retro.rds")
retro_int <- readRDS("model/retro_int.rds")

### current assessment year
year <- range(stk_new)[["maxyear"]] + 1

### results from previous year for comparison
xsa_last_yr <- readRDS(paste0("boot/data/previous_results/diags_",
                              year - 1, ".rds"))
stk_last_yr <- readRDS(paste0("boot/data/previous_results/stock_",
                              year - 1, ".rds"))
stk_last_yr2 <- readRDS(paste0("boot/data/previous_results/stock_",
                              year - 2, ".rds"))


### ------------------------------------------------------------------------ ###
### plot input data ####
### ------------------------------------------------------------------------ ###

### landings age structure
p <- plot_age_structure(stock = stk_new, slots = "landings.n", 
                        years = seq(to = dims(stk_new)$maxyear, length.out = 16))
if (isTRUE(verbose)) p
ggsave(file = "report/plots_data_landings_age_structure.png",  plot = p,
       width = 13.4, height = 15, units = "cm", dpi = 300, type = "cairo-png")
### bubble plot
p <- as.data.frame(landings.n(stk_new)) %>%
  group_by(age) %>%
  mutate(data = data/mean(data) - 1) %>%
  mutate(sign = ifelse(data <= 0, "negative", "positive")) %>%
  mutate(data_abs = abs(data)) %>%
  ggplot(aes(x = year, y = age, size = data_abs, fill = sign)) +
  geom_point(shape = 21) + 
  scale_size("difference\nto mean", range = c(0, 6), transform = "exp") + 
  scale_y_continuous(breaks = seq(from = 2, to = 12, by = 2)) + 
  scale_fill_discrete("") + 
  theme_bw(base_size = 8) +
  labs(x = "Year", y = "Age (years)")
if (isTRUE(verbose)) p
ggsave(file = "report/plots_data_landings_age_structure_bubbles.png",  plot = p,
       width = 15, height = 10, units = "cm", dpi = 300, type = "cairo-png")

### proportion of catch numbers by age
p <- as.data.frame(catch.n(stk_new)) %>%
  group_by(year) %>%
  mutate(prop = data/sum(data)) %>%
  mutate(age = factor(age, levels = rev(dimnames(stk_new)$age))) %>%
  ggplot(aes(x = year, y = prop, fill = age)) +
  geom_col() +
  scale_fill_manual("Age",
    values = scales::hue_pal()(dim(stk_new)[1])[c(1, 4, 7, 10, 2, 5, 8, 11, 3, 
                                                  6, 9)]) +
  geom_hline(yintercept = c(0.1, 0.9)) + 
  labs(x = "Year", y= "Proportion (catch numbers)") +
  theme_bw()
if (isTRUE(verbose)) p
ggsave(file = "report/plots_data_landings_numbers_proportion.png", plot = p,
       width = 13.4, height = 8, units = "cm", dpi = 300, type = "cairo-png")
### same but for fbar ages contribution
p <- as.data.frame(catch.n(stk_new)) %>%
  group_by(year) %>%
  mutate(prop = data/sum(data)) %>%
  mutate(fbar_age = age %in% c(3:9)) %>%
  ggplot(aes(x = year, y = prop, fill = fbar_age)) +
  geom_col() +
  scale_fill_discrete("Fbar\n(ages 3-9)") + 
  labs(x = "Year", y= "Proportion (catch numbers)") +
  theme_bw()
if (isTRUE(verbose)) p
ggsave(file = "report/plots_data_landings_numbers_proportion_Fbar.png", plot = p,
       width = 13.4, height = 8, units = "cm", dpi = 300, type = "cairo-png")

### plot stock and landings wts
p <- plot_weights(stock = stk_new, slots = c("stock.wt", "landings.wt"))
if (isTRUE(verbose)) p
ggsave(file = "report/plots_data_weights_at_age.png", plot = p,
       width = 13.4, height = 8, units = "cm", dpi = 300, type = "cairo-png")

### plot catch curves
### raw values
p <- plot_catch_curve(input = idx, cohort = FALSE, standardize = FALSE,
                      y_label = "LPUE")
if (isTRUE(verbose)) p
ggsave(file = "report/plots_data_survey_cc_raw.png", plot = p, 
       width = 13.4, height = 13.4, units = "cm", dpi = 300, type = "cairo-png")
### standardised & cohort
p <- plot_catch_curve(input = idx, cohort = TRUE, standardize = TRUE,
                      y_label = "means standardised LPUE")
if (isTRUE(verbose)) p
ggsave(file = "report/plots_data_survey_cc_std_cohort.png", plot = p,
       width = 13.4, height = 13.4, units = "cm", dpi = 300, type = "cairo-png")



### plot survey catch curves, standardized and cohort-wise
p <- plot_catch_curve(input = idx, cohort = TRUE, standardize = TRUE,
                      y_label = "means standardided LPUE/CPUE")
if (isTRUE(verbose)) p
ggsave(file = "report/plots_data_survey_catch_curve_cohorts.png", plot = p, 
       width = 13.4, height = 16, units = "cm", dpi = 300, type = "cairo-png")

### plot survey catch curves, including sum of ages
p <- plot_catch_curve(input = idx, cohort = FALSE, standardize = FALSE,
                      y_label = "LPUE", total = TRUE)
if (isTRUE(verbose)) p
ggsave("report/plots_data_survey_catch_curve_total.png", plot = p, 
       width = 13.4, height = 16, units = "cm", dpi = 300, type = "cairo-png")
### plot survey catch curves, including sum of ages, wide
p <- plot_catch_curve(input = idx, cohort = FALSE, standardize = FALSE,
                      y_label = "LPUE", total = TRUE, wide = TRUE)
if (isTRUE(verbose)) p
ggsave(file = "report/plots_data_survey_catch_curve_total_wide.png", 
       plot = p, width = 20, height = 10, units = "cm", dpi = 300, 
       type = "cairo-png")

### plot survey catch curves, standardized and along years
p <- plot_catch_curve(input = idx, cohort = FALSE, standardize = TRUE,
                      y_label = "means standardised LPUE/CPUE")
if (isTRUE(verbose)) p
ggsave(file = "report/plots_data_survey_catch_curve_years.png", 
       plot = p, width = 13.4, height = 16, units = "cm", dpi = 300, 
       type = "cairo-png")

### survey internal correlations
p <- idx_cor(idx = idx[c("Q1SWBeam-nonoffset")], ncol = 6)
if (isTRUE(verbose)) p
ggsave(file = "report/plots_data_survey_cor_3.png", plot = p,
       width = 17, height = 13, units = "cm", dpi = 300, type = "cairo-png")

p <- idx_cor(idx = idx[c("FSP-UK")], ncol = 4)
if (isTRUE(verbose)) p
ggsave(file = "report/plots_data_survey_cor_1.png", plot = p,
       width = 13.4, height = 15, units = "cm", dpi = 300, type = "cairo-png")

p <- idx_cor(idx = idx[1:2], ncol = 4)
if (isTRUE(verbose)) p
ggsave(file = "report/plots_data_survey_cor_2.png", plot = p, 
       width = 13.4, height = 12, units = "cm", dpi = 300, type = "cairo-png")


### ------------------------------------------------------------------------ ###
### summary table for report ####
### ------------------------------------------------------------------------ ###
summary_table <- data.frame(
  year = range(stk_new)[["minyear"]]:range(stk_new)[[ "maxyear"]],
  rec = round(c(rec(stk_new))),
  tsb = round(c(tsb(stk_new))),
  ssb = round(c(ssb(stk_new))),
  landings = round(c(landings(stk_new))),
  prop = round(c(catch(stk_new) / ssb(stk_new)), 2),
  fbar = round(c(fbar(stk_new)), 3)
)
summary_table2 <- summary_table
names(summary_table2) <- c("Year", "Recruitment Age 2 [000's]", "TSB [tonnes]",
                           "SSB [tonnes]", "Landings [tonnes]", "Yield/SSB",
                           "Fbar (Ages 3-9)")
write.csv(file = "report/tables_xsa_summary.csv", x = summary_table2,
          row.names = FALSE)


### ------------------------------------------------------------------------ ###
### stock plot ####
### ------------------------------------------------------------------------ ###

### table for storing data
summary_plot <- rbind(data.frame(year = summary_table$year,
                                 data = summary_table$rec,
                                 type = "rec"),
                      data.frame(year = summary_table$year,
                                 data = summary_table$ssb,
                                 type = "ssb"),
                      data.frame(year = summary_table$year,
                                 data = summary_table$landings,
                                 type = "landings"),
                      data.frame(year = summary_table$year,
                                 data = summary_table$fbar,
                                 type = "fbar"))

### sort quants
summary_plot$type <- factor(summary_plot$type, 
                            levels = unique(summary_plot$type)[c(3,1,4,2)])

### facet labels
wraps <- c(landings = "Landings [t]", 
           rec = "Recruitment (age 2)",
           fbar = "Fishing Mortality (ages 3-9)", 
           ssb = "Spawning Stock Biomass")

### create plot
p <- ggplot() +
  geom_line(data = summary_plot[summary_plot$type %in% c("ssb","fbar"),], 
            aes(x = year, y = data)) +
  geom_bar(data = summary_plot[summary_plot$type %in% c("rec"),], 
           aes(x = year, y = data),#, fill = year == range(stk_new)["maxyear"] 
           colour = "black",
           stat = "identity", show.legend = FALSE,
           width = 0.7) +
  scale_fill_manual(values = c("grey30","white")) +
  geom_bar(data = summary_plot[summary_plot$type %in% c("landings"),], 
           aes(x = year, y = data), 
           colour = "black",  fill = "grey30",
           stat = "identity", width = 0.7) +
  facet_wrap(~type, scales = "free_y", labeller = labeller(type = wraps)) +
  #scale_x_continuous(breaks = seq(1980,2015,5)) +
  ylim(0, NA) +
  #scale_y_continuous(expand = c(0, 0), limits = c(0,NA)) + ### 0 gap axis-bars
  labs(x = "year", y = "") + 
  theme_classic() + 
  theme_custom2 +
  theme(panel.background = element_rect(fill = NA, color = "black"),
        panel.grid = element_blank(),  ### grid lines
        strip.background = element_blank(), ### remove facet lable border
        axis.title.y = element_blank(), ### remove space for axis label
        axis.text = element_text(size = 8),
        strip.text = element_text(face = "bold", size = 9))
if (isTRUE(verbose)) p
ggsave(file = "report/plots_xsa_results.png", plot = p,
       width = 13.4, height = 13.4, units = "cm", dpi = 300)

### ------------------------------------------------------------------------ ###
### plot index residuals ####
### ------------------------------------------------------------------------ ###

p <- plot_index_residuals(xsa, names(idx))
if (isTRUE(verbose)) p
ggsave(file = "report/plots_XSA_residuals.png", 
       width = 25, height = 15, units = "cm", dpi = 300,
       type = "cairo-png", plot = plot(p))

### ------------------------------------------------------------------------ ###
### plot weights of indices in XSA assessment ####
### ------------------------------------------------------------------------ ###

### create input list
xsa_input <- list(xsa, xsa_last_yr)
names(xsa_input) <- c(year, year - 1)

### plot
p <- plot_xsa_weights(object = xsa_input, n_year_classes = 10)
if (isTRUE(verbose)) p
ggsave(file = "report/plots_XSA_weights.png", plot = p,
       width = 13.4, height = 10, units = "cm", dpi = 300, type = "cairo-png")

### ------------------------------------------------------------------------ ###
### compare stock assessment results with previous year ####
### ------------------------------------------------------------------------ ###

### create object with current and last assessment
stocks <- FLStocks(stk_new, stk_last_yr, stk_last_yr2)
names(stocks) <- c(year:c(year - 2))

### plot
p <- plot_stock_comparison(stocks = stocks, label = "WG year")
if (isTRUE(verbose)) p
ggsave(file = "report/plots_xsa_last_year_comparison.png", plot = p,
       width = 13.4, height = 10, units = "cm", dpi = 300, type = "cairo-png")


### ------------------------------------------------------------------------ ###
### plot retro ####
### ------------------------------------------------------------------------ ###

### plot
p <- plot_stock_comparison(stocks = retro, label = "last data\nyear",
                           rem_slots = "landings", ncol = 1)
if (isTRUE(verbose)) p
ggsave(file = "report/plots_xsa_retro.png", plot = p,  
       width = 13.4, height = 16, units = "cm", dpi = 300, type = "cairo-png")


### calculate mohn's rho index
mohns_rho_res <- (paste0("Mohn's rho:\n",
           "Fbar: ", mohns_rho(retro, 'fbar'), "\n",
           "SSB: ", mohns_rho(retro, 'ssb'), "\n",
           "SSB (including stf): ", mohns_rho(retro_int, 'ssb'), "\n",
           "rec: ", mohns_rho(retro, 'rec'), "\n"))
if (isTRUE(verbose)) cat(mohns_rho_res)
writeLines(text = mohns_rho_res, con = "report/tables_mohns_rho.txt")

### ------------------------------------------------------------------------ ###
### F-at-age table ####
### ------------------------------------------------------------------------ ###

### extract and round F-at-age values
F_age <- round(t(harvest(stk_new)[, drop = TRUE]), 3)
### add year
F_age <- cbind(year = as.numeric(as.character(rownames(F_age))), F_age)
### change last age to plusgroup
colnames(F_age)[ncol(F_age)] <- paste0(colnames(F_age)[ncol(F_age)], "+")
### add fbar
F_age <- cbind(F_age, fbar = round(c(fbar(stk_new)), 3))
colnames(F_age)[ncol(F_age)] <- paste0("Fbar(", range(stk_new)[["minfbar"]], 
                                       "-", range(stk_new)[["maxfbar"]], ")")
### save file
write.csv(file = "report/tables_xsa_f_at_age.csv",
          row.names = FALSE, x = F_age)

### ------------------------------------------------------------------------ ###
### Stock numbers at age ####
### ------------------------------------------------------------------------ ###

### extract and round N-at-age values
N_age <- round(t(stock.n(stk_new)[, drop = TRUE]))
### add year
N_age <- cbind(year = as.numeric(as.character(rownames(N_age))), N_age)
### change last age to plusgroup
colnames(N_age)[ncol(N_age)] <- paste0(colnames(N_age)[ncol(N_age)], "+")
### add sum
N_age <- cbind(N_age, total = round(c(quantSums(stock.n(stk_new)))))
### save file
write.csv(file = "report/tables_xsa_numbers_at_age.csv",
          row.names = FALSE, x = N_age)

### ------------------------------------------------------------------------ ###
### Landings numbers ####
### ------------------------------------------------------------------------ ###

### extract and round 
L_age <- round(t(catch.n(stk_new)[, drop = TRUE]))
### add year
L_age <- cbind(year = as.numeric(as.character(rownames(L_age))), L_age)
### change last age to plusgroup
colnames(L_age)[ncol(L_age)] <- paste0(colnames(L_age)[ncol(N_age)], "+")
### add sum
L_age <- cbind(L_age, total = round(c(quantSums(catch.n(stk_new)))))
### save file
write.csv(file = "report/tables_input_landings_numbers_at_age.csv",
          row.names = FALSE, x = L_age)

### ------------------------------------------------------------------------ ###
### landings weights at age ####
### ------------------------------------------------------------------------ ###

### extract and round 
WtL_age <- round(t(landings.wt(stk_new)[, drop = TRUE]), 3)
### add year
WtL_age <- cbind(year = as.numeric(as.character(rownames(WtL_age))), WtL_age)
### change last age to plusgroup
colnames(WtL_age)[ncol(WtL_age)] <- paste0(colnames(WtL_age)[ncol(WtL_age)], "+")
### save file
write.csv(file = "report/tables_input_landings_weights_at_age.csv",
          row.names = FALSE, x = WtL_age)

### ------------------------------------------------------------------------ ###
### stock weights at age ####
### ------------------------------------------------------------------------ ###

### extract and round F-at-age values
WtS_age <- round(t(stock.wt(stk_new)[, drop = TRUE]), 3)
### add year
WtS_age <- cbind(year = as.numeric(as.character(rownames(WtS_age))), WtS_age)
### change last age to plusgroup
colnames(WtS_age)[ncol(WtS_age)] <- paste0(colnames(WtS_age)[ncol(WtS_age)], "+")
### save file
write.csv(file = "report/tables_input_stock_weights_at_age.csv",
          row.names = FALSE, x = WtS_age)

### ------------------------------------------------------------------------ ###
### f-at-age plot ####
### ------------------------------------------------------------------------ ###

f <- as.data.frame(harvest(stk_new))
f$age <- as.factor(f$age)

p <- ggplot(data = f, aes(x = year, y = data, colour = age)) +
  geom_line() +
  theme_custom2 +
  labs(x = "Year", y = "Fishing mortality [yr^-1]")
if (isTRUE(verbose)) p
### save plot
ggsave(file = "report/plots_f_at_age.png", 
       width = 13.4, height = 8, units = "cm", dpi = 300, plot = p)


### ------------------------------------------------------------------------ ###
### summary table for report
### ------------------------------------------------------------------------ ###
summary_table <- data.frame(year = range(stk_new)[["minyear"]]:
                                   range(stk_new)[[ "maxyear"]],
                            rec = round(c(rec(stk_new))),
                            tsb = round(c(tsb(stk_new))),
                            ssb = round(c(ssb(stk_new))),
                            landings = round(c(landings(stk_new))),
                            prop =  round(c(catch(stk_new)/ssb(stk_new)), 2),
                            fbar = round(c(fbar(stk_new)), 3))
summary_table2 <- summary_table
names(summary_table2) <- c("Year", "Recruitment Age 2 [000's]", "TSB [tonnes]",
                          "SSB [tonnes]", "Landings [tonnes]", "Yield/SSB",
                          "Fbar (Ages 3-9)")

write.csv(file = "report/tables_xsa_summary_report.csv", x = summary_table2,
          row.names = FALSE)

### ------------------------------------------------------------------------ ###
### commercial effort and LPUE ####
### ------------------------------------------------------------------------ ###

### load data
LPUE <- read.csv("boot/data/tuning/LPUE.csv")

### standardise LPUE
LPUE <- LPUE %>%
  mutate(LPUE = CPUE_landings_tonnes / CPUE_effort_days) %>%
  group_by(fleet) %>%
  mutate("means standardised LPUE [t/day]" = LPUE /
           mean(LPUE, na.rm = TRUE),
         "mean standardised effort [days]" = CPUE_effort_days /
           mean(CPUE_effort_days, na.rm = TRUE))
### plot
p <- LPUE %>% gather(key = "variable", value = "value", 6:7) %>%
  ggplot(aes(x = year, y = value, colour = fleet, shape = fleet)) +
  geom_line() + geom_point() +
  theme_custom2 +
  facet_wrap(~ variable, nrow = 2, scale = "free") +
  labs(y = "") + ylim(0, NA) +
  scale_x_continuous(breaks = c(1988, 1998, 2008, 2018))
if (isTRUE(verbose)) p
### save plot
ggsave(file = "report/plots_data_LPUE.png", plot = p,
       width = 13.4, height = 10, units = "cm", dpi = 300, type = "cairo")

### arrange table for report
LPUE3 <- LPUE %>%
  transmute(Year = year,
  "Effort [days]" = round(CPUE_effort_days),
  "Landings [tonnes]" = round(CPUE_landings_tonnes),
  "LPUE [tonnes/1000 days]" = round(`LPUE`*1000, 2),
  "means standardised LPUE" = round(`means standardised LPUE [t/day]`, 2)
  )
### save to file
write.csv(file = "report/tables_data_LPUE.csv",
          x = LPUE3, row.names = FALSE)

