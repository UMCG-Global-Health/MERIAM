checkpoint::checkpoint("2021-07-20", checkpoint_location = getwd())

library(tibble)
library(magrittr)
library(dplyr)
library(tidyr)
library(purrr)
library(furrr)
library(forcats)
library(stringr)
library(readr)
library(countrycode)
library(lubridate)
library(truncnorm)
library(ggplot2)
library(scales)
library(gt)
library(AMR)
euro <- dollar_format(prefix = "\u20ac", accuracy = 100)
source("functions/stats.R") #load financial functions

library(showtext)
font_add_google(name = "Fira Sans", family = "firasans", regular.wt = 300, bold.wt = 600)

showtext_auto()

#other packages needed

library(RColorBrewer)


#create and set default ggplot theme to be used and define colour palette
theme_value <- function (base_size = 30, base_family = "") {
  theme_minimal() %+replace%
    theme(
      text=element_text(family="firasans", face = "plain", colour = "black", size = 14, hjust = 0.5, vjust = 0.5, angle = 0, lineheight = 0.9, debug = FALSE, inherit.blank = TRUE, margin = theme_minimal()$text$margin),
      legend.position = "bottom"
    )
}

theme_value_save <- function (base_size = 11, base_family = "") {
  theme_minimal() %+replace%
    theme(
      text=element_text(family="firasans", face = "plain", colour = "black", size = 15, hjust = 0.5, vjust = 0.5, angle = 0, lineheight = 0.9, debug = FALSE, inherit.blank = TRUE, margin = theme_minimal()$text$margin),
      legend.position = "bottom"
    )
}

theme_set(theme_value())+theme(legend.position = "bottom")

valuePalette <- c("#207BA3", "#0FB4A0", "#A88632", "#A33E10", "#1841A3", "#7593A3", "#6DB5B0", "#A8A08A", "#A37A65", "#6D7AA3")


eurostat_financial <- list( cpi = read.csv("data_input/financial/cpi.csv"),
                            ppp = read.csv("data_input/financial/ppp.csv"),
                            exchange_rate = read.csv("data_input/financial/exchange_rates.csv"))

iter <- 1:2000
data_folder <- "data_output/per_7dec/"
start_year <- 2020
date <- "2021-12-07"


results <- map_dfr(iter,
                   ~read_rds(str_c(data_folder, "results_", .x, "_", date,".RDS")))


#add training costs

## calculate costs per GP, based on annual training of 39.76 min and consultations of 10 min
training_duration <- 39.76
training_cost <- 33 * (training_duration / 10)
training_cost_ppp <- convert_price(training_cost,
                                   country = "nl",
                                   year_in = 2014,
                                   year_out = 2019,
                                   input = c("local", "EUR"),
                                   output = c("ppp", "EUR"),
                                   eurostat_data = eurostat_financial) 

## add costs every year, based on size of the population

results <- results %>% 
  mutate(gp_training = map2(pyramid, strategy,
                                     ~.x %>%
                                       mutate(n = abs(n)) %>%
                                       group_by(year) %>%
                                       summarise(n_pop = sum(n)) %>%
                                       mutate(n_gp = n_pop / 1470,
                                              cost_training = n_gp * ifelse(.y == "base",
                                                                            0,
                                                                            training_cost_ppp),
                                              week = 1) %>%
                                       select(year, week, cost_training)),
         microsim_aggregated = map2(microsim_aggregated, gp_training,
                                    ~.x %>% left_join(.y, by = c("year", "week")) %>%
                                      mutate(cost_training = ifelse(is.na(cost_training), 0, cost_training))))

#get total costs for all iterations
#convert them to local currency

conversion_factor_local <- convert_price(1,
                                         country = "nl",
                                         year_in = 2019,
                                         year_out = 2019,
                                         input = c("ppp", "EUR"),
                                         output = c("local", "EUR"),
                                         eurostat_data = eurostat_financial, 
                                         conversion_factor = TRUE)

total_costs <- results %>%
  unnest(microsim_aggregated) %>%
  select(-cost_healthcare, -cost_prod, -cost_hospital, -costs) %>%
  rowwise() %>%
  mutate(cost_total = cost_abx + cost_consult + cost_test + cost_training,
         across(.cols = starts_with("cost_"),
               .fns = ~.x*conversion_factor_local),
         across(.cols = starts_with("cost_"),
                .fns = ~.x*(1/((1+discount_economic)^(year - start_year))),
                .names = "disc_{.col}")) %>%
  ungroup() %>%
  select(country, strategy, iteration, year, week, incidence, starts_with("cost"), starts_with("disc_"))

saveRDS(total_costs, "results/crp_nl/totalcosts.RDS")

p_costsaving <- total_costs %>%
  select(strategy, iteration, year, week, disc_cost_total) %>%
  group_by(strategy, iteration) %>%
  summarise(disc_cost_total = sum(disc_cost_total),
            .groups = "drop") %>%
  pivot_wider(names_from = "strategy",
              values_from = disc_cost_total) %>%
  mutate( cost_diff_conservative = conservative - base,
          cost_diff_uncertain = uncertain - base,
          cost_saving_conservative = ifelse(cost_diff_conservative < 0, 1, 0),
          cost_saving_uncertain = ifelse(cost_diff_uncertain < 0, 1, 0))


#### MAKE SURE TO SET NA.RM TO FALSE AGAIN
cost_overview <- total_costs %>%
  select(-incidence) %>%
  pivot_longer(cols = c(starts_with("cost"), starts_with("disc"))) %>%
  pivot_wider(names_from = strategy, values_from = value) %>%
  mutate(
    diff_conservative = conservative - base,
    diff_uncertain = uncertain - base
  ) %>%
  group_by(country, name, iteration) %>%
  summarise(across(.cols = c("base", "conservative", "uncertain", "diff_conservative", "diff_uncertain"),
                   .fns = sum)) %>%
  group_by(country, name) %>%
  summarise(across(.cols = c("base", "conservative", "uncertain", "diff_conservative", "diff_uncertain"),
                   .fns = list(med = ~median(.x, na.rm = FALSE),
                               lo = ~quantile(.x, .025, na.rm = FALSE),
                               hi = ~quantile(.x, .975, na.rm = FALSE))))

cost_overview %>%
  select(country, name, base = base_med, conservative = conservative_med, uncertain = uncertain_med) %>%
  pivot_longer(cols = c("base", "conservative", "uncertain"), names_to = "Strategy", values_to = "Costs") %>%
  mutate(disc = if_else(str_sub(name, 1, 4) == "disc", T, F),
         item = case_when(
           str_detect(name, "abx") ~ "Antibiotics",
           str_detect(name, "consult") ~ "Consults",
           str_detect(name, "test") ~ "Diagnostic tests",
           str_detect(name, "training") ~ "Training",
           str_detect(name, "total") ~ "Total"),
         Strategy = case_when(
           Strategy == "base" ~ "Current standard of care",
           Strategy == "conservative" ~ "Hypothetical diagnostic algorithm (\u20ac10)",
           Strategy == "uncertain" ~ "Hypothetical diagnostic algorithm (\u20ac5)",
         ),
         Strategy = factor(Strategy, 
                           levels = c("Hypothetical diagnostic algorithm (\u20ac10)", "Hypothetical diagnostic algorithm (\u20ac5)", "Current standard of care"),
                           ordered = T),
         item = factor(item, levels = c("Antibiotics", "Diagnostic tests", "Training", "Consults"), ordered = TRUE),
         ) %>%
  filter(disc == TRUE, item != "Total") %>%
  ggplot(aes(x = Strategy, y = Costs, fill = item)) +
  geom_col() +
  #facet_wrap(~country, ncol = 1) +
  scale_fill_manual(values = valuePalette[c(1,2,6,4)], name = NULL) +
  coord_flip() +
  xlab(NULL) +
  scale_y_continuous(labels = label_dollar(prefix = "\u20ac",
                                           big.mark = ".",
                                           largest_with_cents = 1,
                                           decimal.mark = ","),
                     name = "Total costs (2020 up to 2030)") +
  theme_value_save()

ggsave("results/crp_nl/costs.png", width = 4, height = 1.5, dpi = 300)
ggsave("results/crp_nl/costs.pdf", width = 10, height = 5, dpi = 300, bg = "white")

###REMOVE NA.RM = TRUE

cost_overview_format <- total_costs %>%
  select(-incidence) %>%
  pivot_longer(cols = c(starts_with("cost"), starts_with("disc"))) %>%
  pivot_wider(names_from = strategy, values_from = value) %>%
  mutate(
    diff_conservative = conservative - base,
    diff_uncertain = uncertain - base
  ) %>%
  group_by(country, name, iteration) %>%
  summarise(across(.cols = c("base", "conservative", "uncertain", "diff_conservative", "diff_uncertain"),
                   .fns = sum)) %>%
  group_by(country, name) %>%
  summarise(across(.cols = c("base", "conservative", "uncertain", "diff_conservative", "diff_uncertain"),
                   .fns = ~str_c(euro(median(.x, na.rm = FALSE)), 
                                 " (", 
                                 euro(quantile(.x, .025, na.rm = FALSE)), 
                                 " - ", 
                                 euro(quantile(.x, .975, na.rm = FALSE)),
                                 ")")))







#add nice formatting


#export to html table, table 1 in manuscript
cost_overview_format %>%
  filter(str_detect(name, "disc")) %>%
  mutate(scenario = case_when(
    name == "cost_abx" ~ "antibiotics",
    name == "cost_consult" ~ "consults",
    name == "cost_test" ~ "diagnostics",
    name == "cost_training" ~ "training",
    name == "cost_total" ~ "total",
    name == "disc_cost_abx" ~ "antibiotics",
    name == "disc_cost_consult" ~ "consults",
    name == "disc_cost_test" ~ "diagnostics",
    name == "disc_cost_training" ~ "training",
    name == "disc_cost_total" ~ "total",
  ),
  country = countrycode(country, origin = "iso2c", destination = "country.name")) %>%
  ungroup %>%
  select(country, scenario, 
         `Current standard-of-care` = base, 
         `Incremental costs conservative scenario` = diff_conservative, 
         `Incremental costs uncertain scenario` = diff_uncertain) %>%
  group_by(country) %>%
  slice(2,1,3,5,4) %>%
  gt("scenario") %>%
  tab_source_note("All costs are discounted and displayed in local currency")


#save for inclusion in appendix
saveRDS(total_costs, "results/crp_nl/total_costs.RDS")




data <- map_dfr(iter,
                ~read_rds(str_c(data_folder, "datafile_", .x, "_", date, ".RDS")) %>%
                  select(country, strategy, iteration, incidence))

abx_consum <- results %>%
  select(country, strategy, iteration, microsim_abxcon) %>%
  unnest(microsim_abxcon) %>%
  group_by(country, strategy, iteration, abx, year) %>%
  summarise(ddd_sum = sum(ddd),
            .groups = "drop") 


abx_total <- abx_consum %>%
  group_by(country, strategy, abx, year) %>%
  summarise(across(.cols = c("ddd_sum"),
                   .fns = list(med = ~median(.x),
                               lo = ~quantile(.x, .025),
                               hi = ~quantile(.x, .975))),
            .groups = "drop")

abx_diff_base <- abx_consum %>%
  filter(strategy == "base") %>%
  select(country, iteration, abx, year, base = ddd_sum)

abx_diff <- abx_consum %>%
  filter(strategy != "base") %>%
  left_join(abx_diff_base, by = c("country", "iteration", "abx", "year")) %>%
  mutate(difference = ddd_sum - base) %>%
  select(strategy, country, iteration, abx, year, ddd_diff = difference) %>%
  group_by(strategy, country, abx, year) %>%
  remove_missing() %>%
  summarise(across(.cols = c("ddd_diff"),
                   .fns = list(med = ~median(.x),
                               lo = ~quantile(.x, .025),
                               hi = ~quantile(.x, .975))),
            .groups = "drop") %>%
  mutate(abx_name = ab_name(abx),
         abx_class = ab_group(abx))

abx_diff %>%
  mutate(across(starts_with("ddd_diff"), ~.x*-1)) %>%
  filter(abx == "AMX", strategy == "conservative") %>%
  ggplot(aes(x = year, y = ddd_diff_med, ymin = ddd_diff_lo, ymax = ddd_diff_hi)) +
  geom_line(aes(color = strategy)) +
  geom_ribbon(aes(fill = strategy), alpha = .2) +
  #facet_wrap(~country, ncol = 1) +
  scale_x_continuous(name = "Year", breaks = c(2020, 2025, 2030))+
  scale_y_continuous(name = "Reduction in consumption of BSPs (DDD)", limits = c(NA,NA)) +
  scale_fill_manual(values = c(valuePalette[1], valuePalette[4])) +
  scale_color_manual(values = c(valuePalette[1], valuePalette[4])) +
  theme_value_save()

ggsave("results/crp_nl/ab_cons.png", width = 4, height = 2.5, dpi = 300)
ggsave("results/crp_nl/ab_cons.pdf", width = 10, height = 5, dpi = 300, bg = "white")


#create table for appendix
abx_appendix <- abx_diff %>%
  select(country, strategy, abx, year, ddd_sum_med = ddd_diff_med, ddd_sum_lo = ddd_diff_lo, ddd_sum_hi = ddd_diff_hi) %>%
  mutate(indicator = "Difference") %>%
  bind_rows(mutate(abx_total, indicator = "Total consumption")) %>%
  mutate(`Antibiotic class` = ab_group(abx),
         `Antibiotic class` = case_when(abx == "AMC" ~ ab_name("AMC"),
                                        abx == "FLC" ~ "Narrow-spectrum penicillins",
                                        `Antibiotic class` == "Beta-lactams/penicillins" ~"Broad-spectrum penicillins",
                                        is.character(`Antibiotic class`) ~ `Antibiotic class`))

saveRDS(abx_appendix, "results/crp_nl/ab_consumption.RDS")

country_list <- str_to_upper(levels(as.factor(results$country)))

amr_prsp_hist <- read_csv("data_input/amr/data_projections/meriam_amr_historical.csv") %>%
  filter(pair == "PRSP", 
         year > 2009,
         geo %in% country_list) %>%
  select(c(1:4, iter+4)) %>%
  pivot_longer(cols = iter+4,
               names_to = "iteration",
               values_to = "amr") %>%
  group_by(set, geo, pair, year) %>%
  summarise(across(.cols = c("amr"),
                   .fns = list(med = ~median(.x, na.rm = FALSE),
                               lo = ~quantile(.x, .025, na.rm = FALSE),
                               hi = ~quantile(.x, .975, na.rm = FALSE))),
            .groups = "drop")

amr_prsp_proj <- read_csv("data_input/amr/data_projections/meriam_amr_forecasts.csv") %>%
  filter(pair == "PRSP", 
         year < 2021,
         year > 2018,
         geo %in% country_list) %>%
  select(c(1:4, iter+4)) %>%
  pivot_longer(cols = iter+4,
               names_to = "iteration",
               values_to = "amr") %>%
  group_by(set, geo, pair, year) %>%
  summarise(across(.cols = c("amr"),
                   .fns = list(med = ~median(.x, na.rm = FALSE),
                               lo = ~quantile(.x, .025, na.rm = FALSE),
                               hi = ~quantile(.x, .975, na.rm = FALSE))),
            .groups = "drop")

amr_prsp_past <- bind_rows(amr_prsp_hist, amr_prsp_proj) %>%
  mutate(strategy = "Current standard-of-care",
         country = str_to_lower(geo)) %>%
  select(-set)



amr_prsp <- results %>%
  select(country, strategy, iteration, amr_rates) %>%
  unnest(amr_rates) %>%
  unnest(amr) %>%
  filter(abx == "BSP",
         year > 2020)


#MAKE SURE TO SET NA.RM TO FALSE

amr_prsp_agg <- amr_prsp %>%
  group_by(country, strategy, year) %>%
  rename(amr = amr_value) %>%
  summarise(across(.cols = c("reduction","amr"),
                   .fns = list(med = ~median(.x, na.rm = FALSE),
                               lo = ~quantile(.x, .025, na.rm = FALSE),
                               hi = ~quantile(.x, .975, na.rm = FALSE))),
            mean_elasticity = mean(elasticity),
            .groups = "drop")


amr_prsp_agg %>%
  filter(strategy %in% c("base", "conservative")) %>%
  mutate(strategy = case_when(
    strategy == "base" ~ "Current standard-of-care",
    strategy == "conservative" ~ "Hypothetical diagnostic algorithm",
    strategy == "uncertain" ~ "Uncertain scenario"
  )) %>%
  bind_rows(amr_prsp_past) %>%
  mutate(year = as_date(str_c(year, "-01-01"))) %>%
  ggplot(aes(x = year, y = amr_med, ymin = amr_lo, ymax = amr_hi, fill = strategy)) +
  geom_line(aes(color = strategy))+
  geom_ribbon(alpha = .2) +
  scale_color_manual(values = c(valuePalette[4], valuePalette[1], valuePalette[2])) +
  scale_fill_manual(values = c(valuePalette[4], valuePalette[1], valuePalette[2])) +
  scale_x_date("Year", breaks = scales::breaks_width("5 years"), labels = scales::label_date("'%y")) +
  scale_y_continuous(name = "Antimicrobial resistance", limits = c(0,NA), labels = label_percent()) +
  theme_value_save()

ggsave("results/crp_nl/amr.png", width = 4, height = 2.5, dpi = 300) 
ggsave("results/crp_nl/amr.pdf", width = 10, height = 5, dpi = 300, bg = "white")


total_population <- results %>%
  select(country, strategy, iteration, pyramid) %>%
  unnest(pyramid) %>%
  mutate(n = abs(n)) %>%
  group_by(country, strategy, iteration, year) %>%
  summarise(total_pop = sum(n),
            .groups = "drop")

annual_costs <- total_costs %>%
  group_by(country, strategy, iteration, year) %>%
  summarise(cost_total = sum(cost_total),
            disc_cost_total = sum(disc_cost_total),
            .groups = "drop") %>%
  left_join(total_population, by = c("country", "strategy", "iteration", "year")) %>%
  mutate(cost_head = cost_total / total_pop,
         disc_cost_head = disc_cost_total / total_pop)

average_annual_costs_base <- annual_costs %>%
  filter(strategy == "base") %>%
  group_by(strategy, country, iteration) %>%
  summarise(disc_cost_head = mean(disc_cost_head)) %>%
  pivot_wider(names_from = "strategy", values_from = "disc_cost_head")


average_annual_costs <- annual_costs %>%
  filter(strategy != "base") %>%
  group_by(strategy, country, iteration) %>%
  summarise(disc_cost_head = mean(disc_cost_head)) %>%
  left_join(average_annual_costs_base, by = c("country", "iteration")) %>%
  mutate(cost_diff = disc_cost_head - base)

amr_prsp_base <- amr_prsp %>%
  filter(year == 2029, strategy == "base") %>%
  select(country, strategy, iteration, amr_value) %>%
  mutate(base = amr_value*100) %>%
  select(country, iteration, base)

amr_prsp_costs <- amr_prsp %>%
  filter(year == 2029, strategy != "base") %>%
  select(country, strategy, iteration, amr_value) %>%
  mutate(amr_value = amr_value*100) %>%
  left_join(amr_prsp_base, by = c("country", "iteration")) %>%
  mutate(amr_diff = base - amr_value) %>%
  left_join(select(average_annual_costs, cost_diff, iteration), by = c("iteration", "country", "strategy")) %>%
  mutate(wtp_perc = cost_diff/amr_diff)

amr_ceac <- expand_grid(strategy = c("conservative", "uncertain"),
                   wtp = 0:1000/100) %>%
  mutate(p = map2_dbl(wtp, strategy,
                       ~amr_prsp_costs %>%
                         filter(strategy == .y) %>%
                         mutate(test = ifelse(wtp_perc <= .x, 1, 0)) %>%
                         pull(test) %>%
                         sum())/length(iter))




amr_ceac %>%
  mutate(Strategy = case_when(
    strategy == "base" ~ "Current standard of care",
    strategy == "conservative" ~ "Hypothetical diagnostic algorithm (\u20ac10)",
    strategy == "uncertain" ~ "Hypothetical diagnostic algorithm (\u20ac5)",
  ),
  Strategy = factor(Strategy, 
                    levels = c("Hypothetical diagnostic algorithm (\u20ac5)", "Hypothetical diagnostic algorithm (\u20ac10)", "Current standard of care"),
                    ordered = T)) %>%
  ggplot(aes(x = wtp, y = p, color = Strategy)) +
  geom_line() +
  scale_x_continuous(labels = label_dollar(prefix = "\u20ac",
                                           big.mark = ".",
                                           largest_with_cents = 1,
                                           decimal.mark = ","),
                     name = "Annual willingness-to-pay per inhabitant for a 1 ppt reduction in AMR") +
  scale_y_continuous(limits = c(0,1), labels = label_percent(), name = "Probability cost-effective") +
  scale_color_manual(values = c(valuePalette[1], valuePalette[2], valuePalette[4])) +
  theme_value_save()



ggsave("results/crp_nl/amr_ceac.png", width = 4, height = 2.5, dpi = 300)
ggsave("results/crp_nl/amr_ceac.pdf", width = 10, height = 5, dpi = 300, bg = "white")

pyr <- results %>%
  filter(strategy == "base",
         iteration == 1) %>%
  select(country, pyramid) %>%
  unnest(pyramid)

saveRDS(pyr, "results/crp_nl/poppyramid.RDS")




incidence_data <- data %>%
  select(country, strategy, iteration, incidence) %>%
  unnest(incidence) %>%
  group_by(country, strategy, iteration, year, week, epi_week) %>%
  summarise(incidence = sum(incidence),
            .groups = "drop") %>%
  group_by(country, strategy, year, week) %>%
  summarise(across(.cols = c("incidence"),
                   .fns = list(med = ~median(.x),
                               lo = ~quantile(.x, .025),
                               hi = ~quantile(.x, .975))),
            .groups = "drop")

saveRDS(incidence_data, "results/crp_nl/incidence.RDS")










