### ------------------------------------------------------------------------ ###
### create tables and plots with forecast results ####
### ------------------------------------------------------------------------ ###

## Before: model/stock.rds
##         model/stk_list.rds
##         model/stk_list_int.rds
##         model/stk_list_int_10.rds
##         model/stf_settings.rds
##         boot/TACs_sole_7e.csv
##         data/model_input_discards.rds
## After:  plots and tables in report/


### load packages
suppressPackageStartupMessages(library(icesTAF))
taf.libPaths()
suppressPackageStartupMessages(library(icesAdvice))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(tidyr))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(reshape))
suppressPackageStartupMessages(library(FLCore))
suppressPackageStartupMessages(library(patchwork))

### load additional functions
source("utilities.R")

if (!exists("verbose")) verbose <- FALSE

### ------------------------------------------------------------------------ ###
### load data ####
### ------------------------------------------------------------------------ ###

stk_new <- readRDS("model/stock.rds") ### stock object with XSA results
### short term forecast results
stk_list <- readRDS("model/stk_list.rds")
### intermediate year forecast
stk_list_int <- readRDS("model/stk_list_int.rds")
stk_list_int_10 <- readRDS("model/stk_list_int_10.rds")

### forecast settings
stf_settings <- readRDS("model/stf_settings.rds")

### TAC history
TACs <- read.csv("boot/data/TACs_sole_7e.csv")

### discards
discards <- readRDS("data/model_input_discards.rds")

### previous advice range
FMSY_lower_prev <- 620
FMSY_upper_prev <- 1210

### ------------------------------------------------------------------------ ###
### get some settings ####
### ------------------------------------------------------------------------ ###

### current assessment year
year <- range(stk_new)[["maxyear"]] + 1

### load MSY forecast
stk_stf <- stk_list$FMSY

### projected years
proj_years <- (range(stk_new)[["maxyear"]] + 1):range(stk_stf)[["maxyear"]]

### ------------------------------------------------------------------------ ###
### discard rate ####
### ------------------------------------------------------------------------ ###

### calculate discard rate
catch <- window(catch(stk_new), start = dims(discards)$minyear)
discard_rate <- discards / (discards + catch)

### mean over last 3 years
discard_rate <- mean(tail(discard_rate, 3))

### ---------------------------------------------------------------------- ###
### check TAC use ####
### ---------------------------------------------------------------------- ###

### create object with TACs and landings
uptake <- merge(TACs, as.data.frame(window(landings(stk_new), 
                                           start = min(TACs$year))),
                all = TRUE)
### format for plotting
uptake <- uptake %>%
  dplyr::select(year, data, advice_catch, TAC) %>%
  pivot_longer(-year) %>%
  mutate(name = factor(name, 
                       levels = c("data", "advice_catch", "TAC"),
                       labels = c("Landings", "Catch advice", "TAC")))

### plot
p <- ggplot(data = uptake %>% filter(!is.na(value)), 
            aes(x = year, y = value, fill = name)) +
  geom_col(position = position_dodge2(padding = 0, preserve = "single")) +
  geom_text(position = position_dodge2(width = 1, padding = 0, 
                                       preserve = "single"), 
            size = 1.5, angle = 45,
            aes(label = round(value), y = value + 50)) + 
  labs(y = "catch [tonnes]") +
  scale_fill_discrete("") +
  scale_x_continuous(breaks = scales::pretty_breaks()) +
  theme_bw(base_size = 10)
if (isTRUE(verbose)) p
ggsave(file = "report/plots_TAC_uptake.png", plot = p, type = "cairo-png",
       width = 13.4, height = 8, units = "cm", dpi = 300)

### ---------------------------------------------------------------------- ###
### plot forecast intermediate options ####
### ---------------------------------------------------------------------- ###

### plot the three F options
names(stk_list_int)
names(stk_list_int) <- gsub(x = names(stk_list_int), pattern = "_", 
                            replacement = " ")
p <- plot_stock_comparison(window(FLStocks(stk_list_int), end = year)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 4))
if (isTRUE(verbose)) p
ggsave(file = "report/plots_stf_intermediate_options.png", plot = p,
       width = 13.4, height = 10, units = "cm", dpi = 300, type = "cairo")
### zoom in
p <- plot_stock_comparison(window(FLStocks(stk_list_int), end = year + 1, 
                                  start = 2014), 
                           forecast = TRUE) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 4))
if (isTRUE(verbose)) p
ggsave(file = "report/plots_stf_intermediate_options_zoom.png", plot = p,
       width = 13.4, height = 10, units = "cm", dpi = 300, type = "cairo")

### effect on catch advice
p <- plot_stock_comparison(window(FLStocks(stk_list_int), end = year + 2, 
                                  start = 2014),
                           forecast = TRUE) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 4))
if (isTRUE(verbose)) p
ggsave(file = "report/plots_stf_intermediate_options_zoom_effect.png",
       width = 13.4, height = 10, units = "cm", dpi = 300, type = "cairo",
       plot = p)
              

### impact on intermediate year
if (isTRUE(verbose)) 
  sapply(stk_list_int, function(x) {c(landings(x)[, ac(year + 0)])})
if (isTRUE(verbose))
  sapply(stk_list_int, function(x) {c(ssb(x)[, ac(year + 1)])})

### impact on catch advice year
if (isTRUE(verbose))
  sapply(stk_list_int, function(x) {c(landings(x)[, ac(year + 1)])})
if (isTRUE(verbose))
  sapply(stk_list_int, function(x) {c(ssb(x)[, ac(year + 2)])})
if (isTRUE(verbose))
  sapply(stk_list_int, function(x) {
     (c(landings(x)[, ac(year + 1)]/
              TACs[TACs$year == (year), "advice_catch"]) - 1) * 100
  })

### effect of intermediate year recruitment
### plot recruitment options
stk_list_tmp <- FLStocks(full = stk_list_int$`F average`,
                            last10 = stk_list_int_10$`F_average`)
p <- plot_stock_comparison(window(FLStocks(stk_list_tmp), end = year + 2, 
                                  start = 2014),
                           forecast = TRUE)
if (isTRUE(verbose)) p
ggsave(file = "report/plots_stf_intermediate_10_options_zoom_effect.png",
       width = 13.4, height = 10, units = "cm", dpi = 300, type = "cairo",
       plot = p)


### ---------------------------------------------------------------------- ###
### plot MSY forecast ####
### ---------------------------------------------------------------------- ###

units(stk_list[["FMSY"]]) <- units(stk_new)
stk_stf_MSY <- FLStocks(STF = window(stk_list[["FMSY"]], 
                                     start = range(stk_new)[["maxyear"]]),
                        input = stk_new)
### plot
p <- plot_stock_comparison(stocks = stk_stf_MSY, label = "", forecast = TRUE) +
  facet_wrap(~ qname, scale = "free_y", ncol = 1, 
             strip.position = "left")
if (isTRUE(verbose)) p
ggsave(file = "report/plots_XSA_STF_F_MSY.png", plot = p,
       width = 13.4, height = 15, units = "cm", dpi = 300, type = "cairo-png")

### ---------------------------------------------------------------------- ###
### plot FMSY ranges ####
### ---------------------------------------------------------------------- ###

p <- plot_stock_comparison(stocks = stk_list[c("FMSY_lower", "FMSY", 
                                               "FMSY_upper")],
                      label = "target", forecast = TRUE)
if (isTRUE(verbose)) p
ggsave(file = "report/plots_stf_FMSY_ranges.png", plot = p, 
       width = 13.4, height = 7, units = "cm", dpi = 300,
       type = "cairo-png")
### zoomed in
p <- plot_stock_comparison(
  stocks = window(FLStocks(stk_list[c("FMSY_lower", "FMSY", "FMSY_upper")]),
                  start = 2010),
                      label = "target", forecast = TRUE) 
if (isTRUE(verbose)) p
ggsave(file = "report/plots_stf_FMSY_ranges_zoom.png", plot = p, 
       width = 13.4, height = 7, units = "cm", dpi = 300,
       type = "cairo-png")

### ---------------------------------------------------------------------- ###
### table for report: input data for short-term forecast ####
### ---------------------------------------------------------------------- ###


table_8.3.12 <- data.frame(Age = dims(stk_stf)$min:dims(stk_stf)$max,
                           N1 = c(stock.n(stk_stf)[, ac(proj_years[1])]),
                           N2 = c(stock.n(stk_stf)[, ac(proj_years[2])]),
                           N3 = c(stock.n(stk_stf)[, ac(proj_years[3])]),
                           M = c(m(stk_stf)[, ac(proj_years[1])]),
                           Mat = c(mat(stk_stf)[, ac(proj_years[1])]),
                           PF = c(harvest.spwn(stk_stf)[, ac(proj_years[1])]),
                           PM = c(m.spwn(stk_stf)[, ac(proj_years[1])]),
                           SWt = c(stock.wt(stk_stf)[, ac(proj_years[1])]),
                           Sel = c(stf_settings$f_target),
                           CWt = c(catch.wt(stk_stf)[, ac(proj_years[1])]))

### round
table_8.3.12[, 2:4] <- round(table_8.3.12[, 2:4])
table_8.3.12[, 5:8] <- round(table_8.3.12[, 5:8], 2)
table_8.3.12[, 9:11] <- round(table_8.3.12[, 9:11], 3)

### name numbers at age columns
names(table_8.3.12)[2:4] <- paste0("N", proj_years)

### save table
write.csv(table_8.3.12, row.names = FALSE, 
          file = "report/tables_stf_input.csv")

### ---------------------------------------------------------------------- ###
### report table: Single option output ####
### ---------------------------------------------------------------------- ###

### create table for every projected year
table_8.3.13 <- lapply(proj_years, function(year){
  
  res <- data.frame(Age = dims(stk_stf)$min:dims(stk_stf)$max,
                    F = c(harvest(stk_stf)[, ac(year)]),
                    "Catch No" = c(catch.n(stk_stf)[, ac(year)]),
                    Yield = c(catch.n(stk_stf)[, ac(year)] *
                                catch.wt(stk_stf)[, ac(year)]),
                    "Stock No" = c(stock.n(stk_stf)[, ac(year)]),
                    Biomass = c(stock.n(stk_stf)[, ac(year)] *
                                  stock.wt(stk_stf)[, ac(year)]),
                    "SSNo" = c(stock.n(stk_stf)[, ac(year)] *
                                 mat(stk_stf)[, ac(year)]),            
                    "SSB" = c(stock.n(stk_stf)[, ac(year)] *
                                stock.wt(stk_stf)[, ac(year)] *
                                mat(stk_stf)[, ac(year)]))
  
  ### sum
  res <- rbind(res, colSums(res))
  res[nrow(res), "Age"] <- "Total"
  res[nrow(res), "F"] <- NA
  
  ### round
  res[, 2] <- round(res[, 2], 3)
  res[, 3:8] <- round(res[, 3:8])
  
  return(res)
  
})

### save tables
lapply(seq_along(table_8.3.13), function(x) {  
  write.csv(table_8.3.13[[x]], row.names = FALSE, 
            file = paste0("report/tables_stf_output_", proj_years[x], ".csv")) 
})

### further values required for report table
if (isTRUE(verbose))
  tail(fbar(stk_stf), 3)

### ---------------------------------------------------------------------- ###
### report table: Year-class sources and contributions for stf ####
### ---------------------------------------------------------------------- ###

### calculate contribution of ages
### for catch
yield_contributions <- catch.n(stk_stf) * catch.wt(stk_stf) / 
  rep(c(catch(stk_stf)), 
      each = length(dimnames(catch.n(stk_stf))$age))
### for ssb
ssb_contributions <- stock.n(stk_stf) * stock.wt(stk_stf) * mat(stk_stf) / 
  rep(c(ssb(stk_stf)), 
      each = length(dimnames(stock.n(stk_stf))$age))

### convert to data frame
yield_df <- as.data.frame(yield_contributions)
ssb_df <- as.data.frame(ssb_contributions)

### find out cohort
yield_df_cohort <- as.data.frame(as(yield_contributions,
                                    "FLCohort"))
ssb_df_cohort <- as.data.frame(as(ssb_contributions,
                                  "FLCohort"))

### merge                                     
yield <- merge(yield_df, yield_df_cohort)
ssb <- merge(ssb_df, ssb_df_cohort)

### separate years into columns
yield <- pivot_wider(yield %>% arrange(cohort, year), 
                      id_cols = cohort, values_from = data, names_from = year)
ssb <- pivot_wider(ssb %>% arrange(cohort, year), 
                     id_cols = cohort, values_from = data, names_from = year)

### select years and cohorts for table and plotting
yield <- yield %>%
  select("cohort", ac(proj_years[-3])) %>%
  tail(13)
ssb <- ssb %>%
  select("cohort", ac(proj_years)) %>%
  tail(13)

### name columns
names(yield)[-1] <- paste("Yield", names(yield)[-1])
names(ssb)[-1] <- paste("SSB", names(ssb)[-1])

### merge yield and ssb
table_8.3.14 <- merge(yield, ssb)

### round and convert to percent
table_8.3.14[, -1] <- round(table_8.3.14[, -1] * 100, 1)

### save table
write.csv(table_8.3.14, row.names = FALSE, 
          file = "report/tables_stf_cohort_contributions.csv")

### modify table for plotting
table_plot <- table_8.3.14
#table_plot <- rbind(100 - colSums(table_plot, na.rm = TRUE), table_plot)
table_plot[, "cohort"] <- paste("YC", table_plot[, "cohort"])
#table_plot[1, "cohort"] <- "remaining YCs" 
d1 <- cbind(table_plot[, c(1, 3)], group = paste("Yield", proj_years[2]))
d2 <- cbind(table_plot[, c(1, 6)], group = paste("SSB", proj_years[3]))
names(d1)[2] <- names(d2)[2] <- "value"
table_plot <- rbind(d1, d2)

### plot
p <- ggplot(data = table_plot, 
            aes(x = 1, y = value, fill = cohort)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  scale_fill_discrete("Year class") +
  facet_wrap(~ group) +
  theme_custom2 +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        panel.grid = element_blank(),
        axis.ticks = element_blank()             
  )
if (isTRUE(verbose)) p
ggsave(file = "report/tables_stf_cohort_contributions_plot.png", 
       width = 13.4, height = 8, units = "cm", dpi = 300, plot = p,
       type = "cairo-png")  


### plot cohort contribution to SSB and catch over time
matb <- stock.n(stk_stf) * stock.wt(stk_stf) * mat(stk_stf)
### add negligible noise to be able to distinguish between years
matb <- matb * rnorm(n = length(matb), mean = 1, sd = 0.000001)
df_b_cohort <- as.data.frame(FLCohort(matb))
df_b_year <- as.data.frame(matb)
df_b <- full_join(df_b_cohort %>%
                    select(age, cohort, data),
                  df_b_year %>%
                    select(age, year, data), 
                  by = c("age", "data"))
df_b_plot <- df_b %>%
  filter(year >= 2015)
catch <- catch.n(stk_stf) * catch.wt(stk_stf)
catch <- catch * rnorm(n = length(catch), mean = 1, sd = 0.000001)
df_c_cohort <- as.data.frame(FLCohort(catch))
df_c_year <- as.data.frame(catch)
df_c <- full_join(df_c_cohort %>%
                    select(age, cohort, data),
                  df_c_year %>%
                    select(age, year, data), 
                  by = c("age", "data"))
df_c_plot <- df_c %>%
  filter(year >= 2015) %>%
  filter(year <= !!year + 1)
df_plot <- bind_rows(df_b_plot %>% mutate(type = "SSB"),
                     df_c_plot %>% mutate(type = "Catch")) %>%
  mutate(type = factor(type, levels = c("SSB", "Catch")))
cols <- scales::hue_pal()(length(unique(df_b_plot$cohort)))
cols <- cols[c(seq(from = 1, to = length(cols), by = 3),
               seq(from = 2, to = length(cols), by = 3),
               seq(from = 3, to = length(cols), by = 3))]

p <- ggplot() +
  geom_col(data = df_plot,
           aes(x = year, y = data, 
               fill = as.factor(cohort)),
           show.legend = FALSE) +
  geom_text(data = df_plot %>% 
              filter((year == max(year) & type == "SSB") |
                       (year == max(year) - 1 & type == "Catch")) %>%
              arrange(type, age) %>%
              unique(),
            aes(x = year + 0.5, y = data, label = paste0("age=", age)),
            position = position_stack(vjust = 0.5),
            size = 2, hjust = 0) +
  scale_fill_manual("", values = cols) +
  geom_vline(xintercept = year - 0.5) +
  facet_wrap(~ type, scales = "free_y") +
  labs(x = "Year", y = "Tonnes") +
  scale_x_continuous(breaks = c(2015, 2017, 2019, 2021, 2023, 2025), 
                     limits = c(NA, max(df_plot$year) + 1.5)) +
  theme_bw(base_size = 8)
if (isTRUE(verbose)) p
ggsave(file = "report/plots_stf_cohort_contributions_plot_catch_ssb.png", 
       width = 13.4, height = 8, units = "cm", dpi = 300, plot = p,
       type = "cairo-png") 

### same for numbers
n <- stock.n(stk_stf)
### add negligible noise to be able to distinguish between years
n <- n * rnorm(n = length(n), mean = 1, sd = 0.000001)
df_n_cohort <- as.data.frame(FLCohort(n))
df_n_year <- as.data.frame(n)
df_n <- full_join(df_n_cohort %>%
                    select(age, cohort, data),
                  df_n_year %>%
                    select(age, year, data), 
                  by = c("age", "data"))
df_n_plot <- df_n %>%
  filter(year >= 2015)
catch <- catch.n(stk_stf)
catch <- catch * rnorm(n = length(catch), mean = 1, sd = 0.000001)
df_c_cohort <- as.data.frame(FLCohort(catch))
df_c_year <- as.data.frame(catch)
df_c <- full_join(df_c_cohort %>%
                    select(age, cohort, data),
                  df_c_year %>%
                    select(age, year, data), 
                  by = c("age", "data"))
df_c_plot <- df_c %>%
  filter(year >= 2015) %>%
  filter(year <= !!year + 1)
df_plot <- bind_rows(df_n_plot %>% mutate(type = "Stock"),
                     df_c_plot %>% mutate(type = "Catch")) %>%
  mutate(type = factor(type, levels = c("Stock", "Catch")))
cols <- scales::hue_pal()(length(unique(df_n_plot$cohort)))
cols <- cols[c(seq(from = 1, to = length(cols), by = 3),
               seq(from = 2, to = length(cols), by = 3),
               seq(from = 3, to = length(cols), by = 3))]

p <- ggplot() +
  geom_col(data = df_plot,
           aes(x = year, y = data, 
               fill = as.factor(cohort)),
           show.legend = FALSE) +
  geom_text(data = df_plot %>% 
              filter((year == max(year) & type == "Stock") |
                       (year == max(year) - 1 & type == "Catch")) %>%
              arrange(type, age) %>%
              unique(),
            aes(x = year + 0.5, y = data, label = paste0("age=", age)),
            position = position_stack(vjust = 0.5),
            size = 2, hjust = 0) +
  scale_fill_manual("", values = cols) +
  geom_vline(xintercept = year - 0.5) +
  facet_wrap(~ type, scales = "free_y") +
  labs(x = "Year", y = "Numbers (thousands)") +
  scale_x_continuous(breaks = c(2015, 2017, 2019, 2021, 2023, 2025), 
                     limits = c(NA, max(df_plot$year) + 1.5)) +
  theme_bw(base_size = 8)
if (isTRUE(verbose)) p
ggsave(file = "report/plots_stf_cohort_contributions_plot_n_ssb.png", 
       width = 13.4, height = 8, units = "cm", dpi = 300, plot = p,
       type = "cairo-png")

### ---------------------------------------------------------------------- ###
### extract values for catch option table ####
### ---------------------------------------------------------------------- ###

res <- lapply(seq_along(stk_list), function(x){
  
  data.frame(
    basis = names(stk_list)[x],
    landings_advice = c(catch(stk_list[[x]])[, ac(year + 1)]),
    fbar_advice = c(fbar(stk_list[[x]])[, ac(year + 1)]),
    ssb_advice = c(ssb(stk_list[[x]])[, ac(year + 2)]),
    ssb_change = c((ssb(stk_list[[x]])[, ac(year + 2)] / 
                      ssb(stk_list[[x]])[, ac(year + 1)] - 1) * 100) 
  )
  
})
res <- do.call(rbind, res)

### calculate discards and total catch with discard rate
res$discards_advice <- res$landings_advice / (1 - discard_rate) * discard_rate
res$catch_advice <- res$landings_advice / (1 - discard_rate)

### calculate change in advice and TAC
### "% TAC change": total catch in 2019 / TAC 2018
res$TAC_change <- with(res, catch_advice / TACs$TAC[TACs$year == year] - 1)*100
res$advice_change <- with(res, catch_advice / 
                            TACs$advice_catch[TACs$year == year] - 1)*100

### manual overwrite for FMSY lower/upper, compare to previous lower/upper
### -> not used anymore
# res[res$basis == "FMSY_lower", "advice_change"] <- 
#   (res[res$basis == "FMSY_lower", "catch_advice"]/FMSY_lower_prev - 1) * 100
# res[res$basis == "FMSY_upper", "advice_change"] <- 
#   (res[res$basis == "FMSY_upper", "catch_advice"]/FMSY_upper_prev - 1) * 100


### reorder columns
res <- res[, c("basis", "catch_advice", "landings_advice", "discards_advice", 
               "fbar_advice", "ssb_advice", "ssb_change", "TAC_change",
               "advice_change")]


names(res)[2:4] <- paste0(c("catch_", "landing_", "discards_"), 
                          stf_settings$last_year + 2)
names(res)[5] <- paste0("fbar_", stf_settings$last_year + 2)
names(res)[6] <- paste0("SSB_", stf_settings$last_year + 3)

### format for report
res_report <- res[order(res[, 5]), ] ### sort by F

### save table for report
write.csv(res_report, row.names = FALSE, 
          file = "report/tables_stf_management_options_output_raw_new.csv")

### round
### catches & SSB
res[, c(2:4, 6)] <- round(res[, c(2:4, 6)])

### order for advice sheet
res$basis <- as.character(res$basis)

### select only required scenarios
res_advice <- res[c(1:3, 6, 14, 13, 17, 11, 15, 18, 12), ]

### ICES rounding
res_advice[, 5] <- icesRound(res_advice[, 5])
res_advice$ssb_change <- icesRound(res_advice$ssb_change)
res_advice$TAC_change <- icesRound(res_advice$TAC_change)
res_advice$advice_change <- icesRound(res_advice$advice_change)

### save table
write.csv(res_advice, row.names = FALSE, file = "report/tables_stf_advice.csv")

### FMSY ranges
res_ranges <- res[grepl(pattern = "^F=", res$basis), ]
res_ranges <- rbind(res_ranges, res[c(1, 4, 5), ])
res_ranges <- res_ranges[order(res_ranges[, 5]), ]

### save table
write.csv(res_ranges, row.names = FALSE, 
          file = "report/tables_stf_advice_ranges.csv")

### ---------------------------------------------------------------------- ###
### advice sheet: catch option input table ####
### ---------------------------------------------------------------------- ###

### create data frame with options
stf_input_table <- data.frame(Variable = c(paste0("F ages 3-9 (", year, ")"),
                        paste0("SSB (", year + 1, ")"),
                        paste0("R age 2 (", year, "-", year + 1, ")"),
                        paste0("Total catch (", year, ")"),
                        paste0("Wanted catch (", year, ")"),
                        paste0("Unwanted catch (", year, ")")
                        ),
           Value = c(icesRound(c(stf_settings$fbar_target)),
                     paste(round(ssb(stk_stf)[, ac(year + 1)]), "tonnes"),
                     paste(round(rec(stk_stf)[, ac(year + 1)]), "thousands"),
                     paste(round(catch(stk_stf)[, ac(year)] / 
                                   (1 - discard_rate)), "tonnes"),
                     paste(round(catch(stk_stf)[, ac(year)]), "tonnes"),
                     paste(round(catch(stk_stf)[, ac(year)] * discard_rate),
                           "tonnes")
                     ),
           Notes = c(paste0("Fsq = FAverage(", stf_settings$last_year - 2, "–",
                            stf_settings$last_year, ") rescaled to F",
                            stf_settings$last_year),
                     "Fishing at Fsq",
                     paste0("GM (", range(stk_new)[["minyear"]], "–", 
                            range(stk_new)[["maxyear"]], ")"),
                     "Wanted catches adjusted for unwanted catches",
                     "Fishing at Fsq",
                     paste0("Average ratio (", range(stk_new)[['minyear']], "–",
                            range(stk_new)[['maxyear']], ") (",
                            icesRound(discard_rate * 100), "%)"))
           )
### save table
write.csv(stf_input_table, row.names = FALSE, 
          file = "report/tables_advice_stf_input.csv")

# ### fbar
# stf_settings$fbar_target
# ### SSB
# c(round(ssb(stk_stf_list[[1]])[, ac(year + 1)]))
# ### recruitment
# c(round(rec(stk_stf_list[[1]])[, ac(year)]))
# ### wanted catch
# c(round(catch(stk_stf_list[[1]])[, ac(year)]))
# ### total catch
# c(round(catch(stk_stf)[, ac(year)] / (1 - discard_rate)))
# ### unwanted catch
# c(round(catch(stk_stf)[, ac(year)] * discard_rate))
# ### discards rate
# discard_rate

### ------------------------------------------------------------------------ ###
### MSY forecast values - for presentation ####
### ------------------------------------------------------------------------ ###

if (isTRUE(verbose))
  fbar(stk_list[["FMSY"]])[, ac((year - 1):(year + 2))]
if (isTRUE(verbose))
  ssb(stk_list[["FMSY"]])[, ac((year - 1):(year + 2))]
if (isTRUE(verbose))
  landings(stk_list[["FMSY"]])[, ac((year - 1):(year + 2))]


### ------------------------------------------------------------------------ ###
### assessment results for ICES standard graph ####
### ------------------------------------------------------------------------ ###
### save as csv table

sag <- data.frame(
  Year = as.numeric(dimnames(stk_new)$year),
  Recruitment = c(rec(stk_new)),
  TBiomass = c(tsb(stk_new)),
  StockSize = c(ssb(stk_new)),
  Landings = c(catch(stk_new)),
  Discards = c(window(discards, start = dims(stk_new)$minyear)),
  FishingPressure = c(fbar(stk_new))
)
### replace missing discards with 0
#sag$Discards[is.na(sag$Discards)] <- 0
### add intermediate year options
sag <- rbind(sag, NA)
sag$Year[nrow(sag)] <- year
sag$StockSize[nrow(sag)] <- ssb(stk_stf)[, ac(year)]
sag$Recruitment[nrow(sag)] <- stf_settings$rec_geomean

write.csv(sag, row.names = FALSE, file = "report/tables_sag.csv")

### or print them out to screen
### un-comment to view/export values:

# ### years
# cat(paste(dims(stk_new)$minyear:dims(stk_new)$maxyear, collapse = "\n"))
# ### recruitment
# cat(paste(rec(stk_new), collapse = "\n"))
# ### recruitment for intermediate year
# cat(format(stf_settings$rec_geomean, digits = 15))
# ### total biomass
# cat(paste(tsb(stk_new), collapse = "\n"))
# ### SSB
# cat(paste(ssb(stk_new), collapse = "\n"))
# ### intermediate year SSB
# cat(format(ssb(stk_stf)[, ac(year)], digits = 15))
# ### catch
# cat(paste(catch(stk_new), collapse = "\n"))
# ### discards
# cat(paste(discards, collapse = "\n"))
# ### F
# cat(paste(fbar(stk_new), collapse = "\n"))

### ------------------------------------------------------------------------ ###
### ICES advice style plot ####
### ------------------------------------------------------------------------ ###
### ICES advice style plot
stk_fc <- window(stk_stf, end = 2024)
discards(stk_fc)[, dimnames(discards)$year] <- discards

# p <- plot_stock(stk = stk_fc, rec_shade_last = TRUE, forecast = TRUE, 
#                 catch_factor = 1000, catch_units = "1000 t",
#                 rec_factor = 1000, rec_units = "millions",
#                 ssb_factor = 1000, ssb_units = "1000 t",
#                 Fpa = 0.39, Flim = 0.44, Fmsy = 0.29,
#                 Bpa = 2900/1000, Blim = 2000/1000, MSYBtrigger = 2900/1000)
# if (isTRUE(verbose)) p
# ggsave(file = "report/plots_ICES_advice_style.png", 
#        width = 20, height = 12, units = "cm", dpi = 300, plot = p,
#        type = "cairo-png")


### ------------------------------------------------------------------------ ###
### assessment summary table for advice sheet (rounded) ####
### ------------------------------------------------------------------------ ###

sag_advice <- sag[, c("Year", "Recruitment", "StockSize", "Landings",
                      "Discards", "FishingPressure")]
sag_advice$Recruitment <- round(sag_advice$Recruitment)
sag_advice$StockSize <- round(sag_advice$StockSize)
sag_advice$Landings <- round(sag_advice$Landings)
sag_advice$Discards <- round(sag_advice$Discards)
sag_advice$FishingPressure <- icesRound(sag_advice$FishingPressure)

write.csv(sag_advice, row.names = FALSE, file = "report/tables_sag_rounded.csv")


### ------------------------------------------------------------------------ ###
### assessment & stf output for ICES MIXFISH ####
### ------------------------------------------------------------------------ ###

### stock with FMSY forecast
stk_mixfish_fmsy <- stk_list$FMSY
saveRDS(stk_mixfish_fmsy, file = paste0("report/sol.27.7e_", year, 
                                        "_FLStock_MixFish_STF.rds"))
### only with intermediate year stock but without catch
stk_mixfish <- window(stk_list$FMSY, end = year)
harvest(stk_mixfish)[, ac(year)] <- NA
catch.n(stk_mixfish)[, ac(year)] <- NA
catch(stk_mixfish)[, ac(year)] <- NA
landings.n(stk_mixfish)[, ac(year)] <- NA
landings(stk_mixfish)[, ac(year)] <- NA
saveRDS(stk_mixfish_fmsy, file = paste0("report/sol.27.7e_", year, 
                                        "_FLStock.rds"))


### get discards
disc_mixfish <- window(discards, end = range(stk_mixfish)["maxyear"])
disc_calc <- dimnames(disc_mixfish)$year[which(is.na(disc_mixfish))]
disc_mixfish[, disc_calc] <- landings(stk_mixfish)[, disc_calc]  / (1 - discard_rate) * discard_rate
saveRDS(disc_mixfish, file = "report/sol.27.7e_discards.rds")

