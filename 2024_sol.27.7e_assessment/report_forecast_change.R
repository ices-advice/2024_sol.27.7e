### ------------------------------------------------------------------------ ###
### illustrate reasons for change in advice ####
### ------------------------------------------------------------------------ ###

## Before: model/stk_list.rds
##         boot/data/previous_results/stk_list_2022.rds
## After:  report/plots_advice_change_age.png
##         report/plots_advice_change_year.png


### load packages
suppressPackageStartupMessages(library(icesTAF))
taf.libPaths()
suppressPackageStartupMessages(library(icesAdvice))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(tidyr))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(FLCore))
suppressPackageStartupMessages(library(patchwork))

### load additional functions
source("utilities.R")

if (!exists("verbose")) verbose <- FALSE

### ------------------------------------------------------------------------ ###
### load data ####
### ------------------------------------------------------------------------ ###

### this year's forecast
stk_list <- readRDS("model/stk_list.rds")
### last year's forecast
stk_list_prev <- readRDS(paste0("boot/data/previous_results/stk_list_",
                                range(stk_list$FMSY)[["maxyear"]] - 3, ".rds"))


### get forecasts
stk_fc <- stk_list$FMSY
stk_fc_prev <- stk_list_prev$FMSY

yr_data <- range(stk_fc)[["maxyear"]] - 3
yr_int <- yr_data + 1
yr_fc <- yr_int + 1

### ------------------------------------------------------------------------ ###
### stock numbers/weight/biomass/selectivity at age ####
### ------------------------------------------------------------------------ ###

yr_range <- (yr_data - 2):yr_fc
yr_range_prev <- yr_range[-length(yr_range)]

WG_names <- paste0("WGCSE", c(yr_data + 1, yr_data))
df <- full_join(
  Reduce(full_join, list(
    as.data.frame(stock.n(stk_fc)[, ac(yr_range)]) %>%
      mutate(quant = "stock.n"),
    as.data.frame(stock.wt(stk_fc)[, ac(yr_range)]) %>%
      mutate(quant = "stock.wt"),
    as.data.frame((stock.n(stk_fc) * stock.wt(stk_fc))[, ac(yr_range)]) %>%
      mutate(quant = "stock.b"),
    as.data.frame((harvest(stk_fc)/(harvest(stk_fc) %=% 
                     rep(c(apply(harvest(stk_fc), 2, max)), 
                         each = 11)))[, ac(yr_range)]) %>%
      mutate(quant = "sel")
    )) %>%
      mutate(WG = WG_names[[1]]) %>%
      mutate(type = case_when(year == yr_int ~ "intermediate year",
                              year == yr_fc ~ "forecast",
                              .default = "data")),
  Reduce(full_join, list(
    as.data.frame(stock.n(stk_fc_prev)[, ac(yr_range_prev)]) %>%
      mutate(quant = "stock.n"),
    as.data.frame(stock.wt(stk_fc_prev)[, ac(yr_range_prev)]) %>%
      mutate(quant = "stock.wt"),
    as.data.frame((stock.n(stk_fc_prev) * stock.wt(stk_fc_prev))[, ac(yr_range_prev)]) %>%
      mutate(quant = "stock.b"),
    as.data.frame((harvest(stk_fc_prev)/(harvest(stk_fc_prev) %=% 
                     rep(c(apply(harvest(stk_fc_prev), 2, max)), 
                         each = 11)))[, ac(yr_range_prev)]) %>%
      mutate(quant = "sel")
  )) %>%
    mutate(WG = WG_names[[2]]) %>%
    mutate(type = case_when(year == (yr_int - 1) ~ "intermediate year",
                            year == (yr_fc - 1) ~ "forecast",
                            .default = "data"))
  ) %>%
  mutate(type = factor(type,
                       levels = c("data", "intermediate year", "forecast"))) %>%
  mutate(quant = factor(quant,
                        levels = c("stock.n", "stock.wt", "stock.b", "sel"),
                        labels = c("Stock numbers\n(thousands)",
                                   "Stock weights\n(kg)",
                                   "Stock biomass\n(tonnes)",
                                   "Selectivity"))) %>%
  mutate(WG = factor(WG, levels = c(WG_names)))
  
p <- df %>%
  ggplot(aes(x = age, y = data, colour = WG,
             shape = type)) +
  geom_line(linewidth = 0.3) +
  geom_point(stroke = 0.3, size = 1) + 
  facet_grid(quant ~ year, scales = "free", switch = "y") +
  scale_colour_brewer("", palette = "Set1") + 
  scale_shape_manual("", values = c(1, 2, 0)) + 
  ylim(c(0, NA)) +
  labs(x = "Age (years)") +
  scale_x_continuous(limits = c(0, NA), breaks = seq(0, 12, 4)) +
  theme_bw(base_size = 8) +
  theme(axis.title.y = element_blank(),
        strip.background.y = element_blank(),
        strip.placement = "outside",
        legend.key.height = unit(0.5, "lines"))
if (isTRUE(verbose)) p
ggsave(file = "report/plots_advice_change_age.png", plot = p, type = "cairo-png",
       width = 15, height = 10, units = "cm", dpi = 300)

### ------------------------------------------------------------------------ ###
### time series ####
### ------------------------------------------------------------------------ ###

p <- full_join(as.data.frame(FLQuants(catch = catch(stk_fc),
                                 rec = rec(stk_fc),
                                 fbar = fbar(stk_fc),
                                 ssb = ssb(stk_fc),
                                 tsb = tsb(stk_fc))) %>%
                 filter(year <= yr_fc) %>%
            mutate(WG = WG_names[[1]]) %>%
            mutate(type = case_when(year == yr_int ~ "intermediate year",
                                    year == yr_fc ~ "forecast",
                                    .default = "data")),
          as.data.frame(FLQuants(catch = catch(stk_fc_prev),
                                 rec = rec(stk_fc_prev),
                                 fbar = fbar(stk_fc_prev),
                                 ssb = ssb(stk_fc_prev),
                                 tsb = tsb(stk_fc_prev))) %>%
            filter(year <= yr_int) %>%
            mutate(WG = WG_names[[2]]) %>%
            mutate(type = case_when(year == (yr_int - 1) ~ "intermediate year",
                                    year == (yr_fc - 1) ~ "forecast",
                                    .default = "data"))
          ) %>%
  mutate(type = factor(type,
                       levels = c("data", "intermediate year", "forecast"))) %>%
  mutate(quant = factor(qname,
                        levels = c("catch", "rec", "fbar", "ssb", "tsb"),
                        labels = c("Catch (tonnes)",
                                   "Recruits (age 2, thousands)",
                                   "Mean F (ages 3-9)",
                                   "SSB (tonnes)",
                                   "TSB (tonnes)"))) %>%
  mutate(WG = factor(WG, levels = WG_names)) %>%
  filter(year >= 2020) %>%
  ggplot(aes(x = year, y = data, colour = WG)) +
  geom_line(linewidth = 0.3) +
  geom_point(aes(shape = type), stroke = 0.3, size = 1) + 
  facet_wrap(~ quant, scales = "free_y", strip.position = "left") +
  scale_colour_brewer("", palette = "Set1") + 
  scale_shape_manual("", values = c(1, 2, 0)) + 
  ylim(c(0, NA)) +
  labs(x = "Year") +
  theme_bw(base_size = 8) +
  theme(axis.title.y = element_blank(),
        strip.background.y = element_blank(),
        strip.placement = "outside",
        legend.key.height = unit(0.5, "lines"),
        legend.position = c(0.8, 0.2))
if (isTRUE(verbose)) p
ggsave(file = "report/plots_advice_change_year.png", plot = p, type = "cairo-png",
       width = 15, height = 10, units = "cm", dpi = 300)



