# ---- Packages ----
library(tidyverse)

# ---- Load and format data ----
results <- readRDS("./secondary_data/results.rds")
names(results) <- c("energy_prices", "fcas_prices", "constraint_prices", "ic_flows", "unit_dispatch", "obj_value")
list2env(results, envir = .GlobalEnv)

wsb_sips_avail <- read_csv("./raw_data/scrape_2025-07-31_2026-02-01.csv") %>% 
  select(-`...1`) %>% 
  mutate(interval = ymd_hms(interval))

# Settlement payments calculation
dispatchload <- tibble()
check <- c()
dudetailsummary <- read_csv("./raw_data/dudetailsummary.csv") %>% 
  filter(END_DATE >= ymd("2021-11-29"))
dudetail_tech_mapping <- read_csv("./secondary_data/dudetail_summary_technology.csv") %>% 
  filter(REGIONID != "SNOWY1")
load_duids <- dudetailsummary %>% 
  filter(DISPATCHTYPE %in% c("LOAD", "BIDIRECTIONAL")) %>% 
  .$DUID %>% 
  unique()
dispatchinterconnector <- read_csv("./raw_data/dispatchinterconnectorres.csv")
dispatchprice <- read_csv("./raw_data/dispatchprice.csv") %>%
  mutate(period = case_when(SETTLEMENTDATE > ymd_hms("2021-11-29 04:00:00") & SETTLEMENTDATE <= ymd_hms("2022-04-01 04:00:00") ~ "SIPS1",
                            SETTLEMENTDATE > ymd_hms("2022-11-01 04:00:00") & SETTLEMENTDATE <= ymd_hms("2023-04-01 04:00:00") ~ "SIPS2",
                            SETTLEMENTDATE > ymd_hms("2023-11-01 04:00:00") & SETTLEMENTDATE <= ymd_hms("2024-04-01 04:00:00") ~ "SIPS3",
                            SETTLEMENTDATE > ymd_hms("2024-11-01 04:00:00") & SETTLEMENTDATE <= ymd_hms("2025-04-01 04:00:00") ~ "SIPS4",
                            SETTLEMENTDATE > ymd_hms("2025-07-31 04:00:00") ~ "SIPS5",
                            TRUE ~ "NONSIPS"))

for (i in c("SIPS1_1", "SIPS1_2", "SIPS2_1", "SIPS2_2", "SIPS3_1", "SIPS3_2", "SIPS4_1", "SIPS4_2", "SIPS5_1", "SIPS5_2", "SIPS5_3")) {
  temp <- read_csv(paste0("./raw_data/dispatchload", "_", i, ".csv")) %>% 
    filter(DUID %in% load_duids)
  
  dispatchload <- dispatchload %>% 
    rbind(temp)
  
  check <- c(check, head(temp, 1)$SETTLEMENTDATE, tail(temp, 1)$SETTLEMENTDATE)
  
  rm(temp)
}

dispatchload <- dispatchload %>%
  filter(INTERVENTION == 0) %>% 
  distinct()

load_actual <- dispatchload %>% 
  left_join(
    distinct(dudetailsummary, DUID, CONNECTIONPOINTID, REGIONID, DISPATCHTYPE, START_DATE, END_DATE),
    by = c("DUID", "CONNECTIONPOINTID")) %>% 
  filter(SETTLEMENTDATE >= START_DATE & SETTLEMENTDATE < END_DATE) %>% 
  mutate(TOTALCLEARED = case_when(DISPATCHTYPE == "BIDIRECTIONAL" ~ -1*TOTALCLEARED,
                                  TRUE ~ TOTALCLEARED)) %>% 
  filter(TOTALCLEARED > 0) %>% 
  group_by(SETTLEMENTDATE, REGIONID) %>% 
  summarise(dispatch_actual = sum(TOTALCLEARED)) %>% 
  ungroup() %>% 
  rename("interval"=SETTLEMENTDATE, "region"=REGIONID)

load <- unit_dispatch %>%
  filter(dispatch_type == "load" & service == "energy") %>% 
  left_join(
    distinct(
      select(dudetail_tech_mapping, DUID, REGIONID)),
    by = c("unit"="DUID")) %>% 
  group_by(interval, scenario, REGIONID) %>% 
  summarise(dispatch = sum(dispatch),
            dispatch_hist = sum(dispatch_hist)) %>% 
  ungroup() %>% 
  rename("region" = REGIONID) %>% 
  left_join(load_actual, by = c("interval", "region")) %>% 
  mutate(dispatch_actual = coalesce(dispatch_actual, 0))

ic_flows_actual <- dispatchinterconnector %>%
  filter(INTERVENTION == 0) %>% 
  rename("interconnector"=INTERCONNECTORID) %>% 
  mutate(link = case_when(interconnector == "T-V-MNSP1" & MWFLOW >= 0 ~ "BLNKTAS",
                          interconnector == "T-V-MNSP1" & MWFLOW < 0 ~ "BLNKVIC",
                          TRUE ~ interconnector)) %>% 
  rename("interval"=SETTLEMENTDATE, "flow_actual"=MWFLOW, "losses_actual"=MWLOSSES) %>% 
  select(interval, interconnector, link, flow_actual, losses_actual)

inter_regional <- ic_flows %>% 
  left_join(ic_flows_actual, by = c("interval", "interconnector", "link")) %>% 
  mutate(flow_actual = coalesce(flow_actual, 0),
         losses_actual = coalesce(losses_actual, 0)) %>% 
  mutate(to_region = case_when(link == "V-SA" & flow >= 0 ~ "SA1",
                               link == "V-SA" & flow < 0 ~ "VIC1",
                               link == "N-Q-MNSP1" & flow >= 0 ~ "QLD1",
                               link == "N-Q-MNSP1" & flow < 0 ~ "NSW1",
                               link == "NSW1-QLD1" & flow >= 0 ~ "QLD1",
                               link == "NSW1-QLD1" & flow < 0 ~ "NSW1",
                               link == "V-S-MNSP1" & flow >= 0 ~ "SA1",
                               link == "V-S-MNSP1" & flow < 0 ~ "VIC1",
                               link == "VIC1-NSW1" & flow >= 0 ~ "NSW1",
                               link == "VIC1-NSW1" & flow < 0 ~ "VIC1",
                               link == "BLNKTAS" ~ "VIC1",
                               link == "BLNKVIC" ~ "TAS1"),
         from_region = case_when(link == "V-SA" & flow >= 0 ~ "VIC1",
                                 link == "V-SA" & flow < 0 ~ "SA1",
                                 link == "N-Q-MNSP1" & flow >= 0 ~ "NSW1",
                                 link == "N-Q-MNSP1" & flow < 0 ~ "QLD1",
                                 link == "NSW1-QLD1" & flow >= 0 ~ "NSW1",
                                 link == "NSW1-QLD1" & flow < 0 ~ "QLD1",
                                 link == "V-S-MNSP1" & flow >= 0 ~ "VIC1",
                                 link == "V-S-MNSP1" & flow < 0 ~ "SA1",
                                 link == "VIC1-NSW1" & flow >= 0 ~ "VIC1",
                                 link == "VIC1-NSW1" & flow < 0 ~ "NSW1",
                                 link == "BLNKTAS" ~ "TAS1",
                                 link == "BLNKVIC" ~ "VIC1"),
         to_flow = abs(flow) - losses,
         to_region_hist = case_when(link == "V-SA" & flow_hist >= 0 ~ "SA1",
                                    link == "V-SA" & flow_hist < 0 ~ "VIC1",
                                    link == "N-Q-MNSP1" & flow_hist >= 0 ~ "QLD1",
                                    link == "N-Q-MNSP1" & flow_hist < 0 ~ "NSW1",
                                    link == "NSW1-QLD1" & flow_hist >= 0 ~ "QLD1",
                                    link == "NSW1-QLD1" & flow_hist < 0 ~ "NSW1",
                                    link == "V-S-MNSP1" & flow_hist >= 0 ~ "SA1",
                                    link == "V-S-MNSP1" & flow_hist < 0 ~ "VIC1",
                                    link == "VIC1-NSW1" & flow_hist >= 0 ~ "NSW1",
                                    link == "VIC1-NSW1" & flow_hist < 0 ~ "VIC1",
                                    link == "BLNKTAS" ~ "VIC1",
                                    link == "BLNKVIC" ~ "TAS1"),
         from_region_hist = case_when(link == "V-SA" & flow_hist >= 0 ~ "VIC1",
                                      link == "V-SA" & flow_hist < 0 ~ "SA1",
                                      link == "N-Q-MNSP1" & flow_hist >= 0 ~ "NSW1",
                                      link == "N-Q-MNSP1" & flow_hist < 0 ~ "QLD1",
                                      link == "NSW1-QLD1" & flow_hist >= 0 ~ "NSW1",
                                      link == "NSW1-QLD1" & flow_hist < 0 ~ "QLD1",
                                      link == "V-S-MNSP1" & flow_hist >= 0 ~ "VIC1",
                                      link == "V-S-MNSP1" & flow_hist < 0 ~ "SA1",
                                      link == "VIC1-NSW1" & flow_hist >= 0 ~ "VIC1",
                                      link == "VIC1-NSW1" & flow_hist < 0 ~ "NSW1",
                                      link == "BLNKTAS" ~ "TAS1",
                                      link == "BLNKVIC" ~ "VIC1"),
         to_flow_hist = abs(flow_hist) - losses_hist,
         to_region_actual = case_when(link == "V-SA" & flow_actual >= 0 ~ "SA1",
                                      link == "V-SA" & flow_actual < 0 ~ "VIC1",
                                      link == "N-Q-MNSP1" & flow_actual >= 0 ~ "QLD1",
                                      link == "N-Q-MNSP1" & flow_actual < 0 ~ "NSW1",
                                      link == "NSW1-QLD1" & flow_actual >= 0 ~ "QLD1",
                                      link == "NSW1-QLD1" & flow_actual < 0 ~ "NSW1",
                                      link == "V-S-MNSP1" & flow_actual >= 0 ~ "SA1",
                                      link == "V-S-MNSP1" & flow_actual < 0 ~ "VIC1",
                                      link == "VIC1-NSW1" & flow_actual >= 0 ~ "NSW1",
                                      link == "VIC1-NSW1" & flow_actual < 0 ~ "VIC1",
                                      link == "BLNKTAS" ~ "VIC1",
                                      link == "BLNKVIC" ~ "TAS1"),
         from_region_actual = case_when(link == "V-SA" & flow_actual >= 0 ~ "VIC1",
                                        link == "V-SA" & flow_actual < 0 ~ "SA1",
                                        link == "N-Q-MNSP1" & flow_actual >= 0 ~ "NSW1",
                                        link == "N-Q-MNSP1" & flow_actual < 0 ~ "QLD1",
                                        link == "NSW1-QLD1" & flow_actual >= 0 ~ "NSW1",
                                        link == "NSW1-QLD1" & flow_actual < 0 ~ "QLD1",
                                        link == "V-S-MNSP1" & flow_actual >= 0 ~ "VIC1",
                                        link == "V-S-MNSP1" & flow_actual < 0 ~ "SA1",
                                        link == "VIC1-NSW1" & flow_actual >= 0 ~ "VIC1",
                                        link == "VIC1-NSW1" & flow_actual < 0 ~ "NSW1",
                                        link == "BLNKTAS" ~ "TAS1",
                                        link == "BLNKVIC" ~ "VIC1"),
         to_flow_actual = abs(flow_actual) - losses_actual) %>% 
  # to price
  left_join(
    select(energy_prices, -ROP, -`historic-p`),
    by = c("interval"="interval", "scenario"="scenario", "to_region"="region", "period"="period")) %>% 
  left_join(
    select(energy_prices, -ROP, -price),
    by = c("interval"="interval", "scenario"="scenario", "to_region_hist"="region", "period"="period")) %>% 
  left_join(
    select(energy_prices, -price, -`historic-p`),
    by = c("interval"="interval", "scenario"="scenario", "to_region_hist"="region", "period"="period")) %>% 
  rename("to_price" = price,
         "to_price_hist" = `historic-p`,
         "to_price_actual" = ROP) %>% 
  # from price
  left_join(
    select(energy_prices, -ROP, -`historic-p`),
    by = c("interval"="interval", "scenario"="scenario", "from_region"="region", "period"="period")) %>% 
  left_join(
    select(energy_prices, -ROP, -price),
    by = c("interval"="interval", "scenario"="scenario", "from_region_hist"="region", "period"="period")) %>% 
  left_join(
    select(energy_prices, -price, -`historic-p`),
    by = c("interval"="interval", "scenario"="scenario", "from_region_hist"="region", "period"="period")) %>% 
  rename("from_price" = price,
         "from_price_hist" = `historic-p`,
         "from_price_actual" = ROP)

irsr_scenario <- inter_regional %>% 
  mutate(irsr = to_flow * (to_price - from_price)/12) %>% 
  group_by(interval, to_region, period, scenario) %>% 
  summarise(irsr = sum(irsr)) %>% 
  ungroup() %>% 
  rename("region" = to_region) %>% 
  arrange(interval, region, scenario)

irsr_hist <- inter_regional %>% 
  mutate(irsr_hist = to_flow_hist * (to_price_hist - from_price_hist)/12) %>% 
  group_by(interval, to_region_hist, period, scenario) %>% 
  summarise(irsr_hist = sum(irsr_hist)) %>% 
  ungroup() %>%
  rename("region" = to_region_hist) %>% 
  arrange(interval, region, scenario)

irsr_actual <- inter_regional %>% 
  mutate(irsr_actual = to_flow_actual * (to_price_actual - from_price_actual)/12) %>% 
  group_by(interval, to_region_actual, period, scenario) %>% 
  summarise(irsr_actual = sum(irsr_actual)) %>% 
  ungroup() %>%
  rename("region" = to_region_actual) %>% 
  arrange(interval, region, scenario)

irsr <- irsr_scenario %>% 
  full_join(irsr_hist,
            by = c("interval", "region", "period", "scenario")) %>% 
  full_join(irsr_actual,
            by = c("interval", "region", "period", "scenario")) %>% 
  mutate(irsr = coalesce(irsr, 0),
         irsr_hist = coalesce(irsr_hist, 0),
         irsr_actual = coalesce(irsr_actual, 0))

settlement <- energy_prices %>% 
  left_join(irsr, 
            by = c("interval", "region", "scenario", "period")) %>%
  mutate(irsr = coalesce(irsr, 0),
         irsr_hist = coalesce(irsr_hist, 0),
         irsr_actual = coalesce(irsr_actual, 0)) %>% 
  left_join(select(totaldemand, SETTLEMENTDATE, REGIONID, TOTALDEMAND),
            by = c("interval"="SETTLEMENTDATE", "region"="REGIONID")) %>% 
  left_join(load,
            by = c("interval", "region", "scenario")) %>% 
  mutate(dispatch = coalesce(dispatch, 0),
         dispatch_hist = coalesce(dispatch_hist, 0),
         dispatch_actual = coalesce(dispatch_actual, 0),
         settlement_payment = (TOTALDEMAND + dispatch) * price/12 - irsr,
         settlement_payment_hist = (TOTALDEMAND + dispatch_hist) * `historic-p`/12 - irsr_hist,
         settlement_payment_actual = (TOTALDEMAND + dispatch_actual) * ROP/12 - irsr_actual) %>% 
  arrange(interval, region, period)

# ---- Tables 3, 9, 10 & Lesson 5: Statistics -----
# Regional benefits
settlement %>% 
  filter(scenario %in% scenarios) %>% 
  group_by(scenario, region) %>% 
  summarise(total = sum(settlement_payment)/1e6,
            benefit = (sum(settlement_payment) - sum(settlement_payment_hist))/1e6*-1) %>% 
  ungroup() %>% 
  View()

# NEM benefits
settlement %>% 
  filter(scenario %in% scenarios) %>% 
  group_by(scenario) %>% 
  summarise(total = sum(settlement_payment)/1e6,
            benefit = (sum(settlement_payment) - sum(settlement_payment_hist))/1e6*-1) %>% 
  ungroup() %>% 
  View()

# Objective value
obj_value %>% 
  group_by(scenario) %>% 
  summarise(with_excl = sum(sys_cost_delta)/12/1e6,
            excl = sum(sys_cost_delta*exclude_interval)/12/1e6,
            without_excl = with_excl - excl) %>% 
  ungroup() %>% 
  View()

# Hours
obj_value %>% 
  group_by(scenario) %>% 
  summarise(hrs = n()/12) %>% 
  ungroup()

# ---- Counterfactual Virtual Transmission Scenarios ----
# WSB unavailability
wsb_sips_avail %>% 
  filter(spd_id %in% c("WTAH_SIPS_AVAIL") &
           (interval >= ymd_hms("2025-07-31 04:00:00") & interval < ymd_hms("2026-02-01 04:00:00")) &
           value == 0) %>% 
  summarise(days = n()/12/24) %>% 
  ungroup()

# Footnote: $14k/MWh price reductions
energy_prices %>% 
  filter(scenario == "wtah_350_flat-p") %>% 
  mutate(diff = `historic-p` - price) %>% 
  arrange(desc(diff)) %>% 
  head(10) %>% 
  left_join(
    filter(wsb_sips_avail, spd_id == "WTAH_SIPS_AVAIL"),
    by = "interval")

wtah_sips_constraints <- c("DSNAP_N>>NIL_86_85_S", "N>>NIL_84_88_S","N>>NIL_88_84_S",
                           "N>>NIL_85_86_S", "N>>NIL_86_85_S", "N>>NIL_86_8U_S", "N>>NIL_8U_86_S")
constraint_prices %>% 
  filter(scenario == "wtah_350_flat-p" & set %in% wtah_sips_constraints & 
           interval %in% c(ymd_hms("2025-11-25 12:15:00"), ymd_hms("2025-11-21 09:10:00"), ymd_hms("2025-11-20 13:25:00")) &
           slack_hist < 1) %>% 
  select(interval, set, slack, slack_hist, rhs, rhs_hist) %>% 
  mutate(diff = (rhs - rhs_hist)/1.135) %>% # 1.135 scale for N>>NIL_85_86_S
  View()

# ---- Figure 4 ----
scenarios <- unique(settlement$scenario)
scenarios <- setNames(scenarios, c("V.3", "V.2", "V.1", "W.1", "W.2"))
data <- settlement %>% 
  filter((scenario %in% scenarios[1:2] & region == "VIC1") |
           (scenario %in% scenarios[4:5] & region == "NSW1")) %>% 
  mutate(scenario2 = setNames(names(scenarios), scenarios)[scenario],
         settlement_saving = settlement_payment_hist - settlement_payment) %>% 
  arrange(scenario2, settlement_saving) %>% 
  group_by(scenario2) %>% 
  mutate(settlement_saving_sum = cumsum(settlement_saving)) %>% 
  arrange(scenario2, desc(settlement_saving)) %>% 
  mutate(hour = row_number()/12)

p <- data %>% 
  ggplot() +
  geom_bar(aes(x = hour, y = settlement_saving/1000000), stat="identity") +
  theme_bw() +
  facet_wrap(~scenario2, scales="free") +
  xlab("Hour") +
  ylab("Regional Benefit ($m)") +
  xlim(0, 20) +
  geom_vline(aes(xintercept = 2), linetype = 2)

totals <- data %>% 
  group_by(scenario) %>% 
  summarise(total = sum(settlement_saving)) %>% 
  ungroup()

data %>% 
  filter(hour > 2 & hour <= 2.1) %>% 
  left_join(totals, by = "scenario") %>% 
  select(scenario, hour, settlement_saving_sum, total) %>% 
  mutate(perc = 1 - settlement_saving_sum/total)
  
# ---- Methods: Data Preparation ----
# INTERVENTION == 1
dispatchprice %>% 
  filter(period != "NONSIPS") %>% 
  group_by(SETTLEMENTDATE) %>% 
  summarise(INTERVENTION = max(INTERVENTION)) %>%
  ungroup() %>% 
  summarise(intervention_hours = sum(INTERVENTION)/12,
            non_intervention_hours = sum(INTERVENTION == 0)/12)

# MARKETSUSPENDEDFLAG > 0
dispatchprice %>% 
  filter(period != "NONSIPS") %>% 
  group_by(SETTLEMENTDATE) %>% 
  summarise(MARKETSUSPENDEDFLAG = max(MARKETSUSPENDEDFLAG)) %>%
  ungroup() %>% 
  summarise(marketsuspended_hours = sum(MARKETSUSPENDEDFLAG > 0)/12,
            non_marketsuspended_hours = sum(MARKETSUSPENDEDFLAG == 0)/12)

# APCFLAG > 0
dispatchprice %>% 
  filter(period != "NONSIPS") %>% 
  group_by(SETTLEMENTDATE) %>% 
  summarise(APCFLAG = max(APCFLAG)) %>%
  ungroup() %>% 
  summarise(administeredpricing_hours = sum(APCFLAG > 0)/12,
            non_administeredpricing_hours = sum(APCFLAG == 0)/12)

# ---- Table 4 ----
energy_prices %>% 
  distinct(interval, region, `historic-p`, ROP) %>% 
  arrange(interval) %>% 
  mutate(diff = `historic-p`-ROP) %>% 
  group_by(region) %>% 
  summarise(mean = mean(diff),
            p1 = quantile(diff, 0.01),
            p5 = quantile(diff, 0.05),
            p25 = quantile(diff, 0.25),
            p50 = quantile(diff, 0.5),
            p75 = quantile(diff, 0.75),
            p95 = quantile(diff, 0.95),
            p99 = quantile(diff, 0.99)) %>% 
  ungroup() %>% 
  pivot_longer(cols = mean:p99, names_to = "field", values_to = "value") %>% 
  mutate(value = round(value, 2)) %>% 
  pivot_wider(names_from = region, values_from = value)

energy_prices %>% 
  distinct(interval, region, `historic-p`, ROP) %>% 
  nrow(.)/5/12

# ---- Table 5 ----
# VBB
settlement %>% 
  filter(scenario %in% c("vni_voltage_100-p", "vni_arbe_elml_relaxation-p", "vbb_sips_on-p")) %>% 
  distinct(interval, region, settlement_payment_hist, settlement_payment_actual) %>% 
  group_by(region) %>% 
  summarise(REGION_settlement_hist = sum(settlement_payment_hist), 
            REGION_settlement_actual = sum(settlement_payment_actual),
            REGION_perc = (REGION_settlement_hist-REGION_settlement_actual)/REGION_settlement_actual*100) %>% 
  ungroup %>% 
  mutate(NEM_hist = sum(REGION_settlement_hist),
         NEM_actual = sum(REGION_settlement_actual),
         NEM_perc = (NEM_hist-NEM_actual)/NEM_actual*100)

settlement %>% 
  filter(scenario %in% c("vni_voltage_100-p", "vni_arbe_elml_relaxation-p", "vbb_sips_on-p")) %>% 
  distinct(interval) %>% 
  nrow(.)/12

# WSB
settlement %>% 
  filter(scenario %in% c("wtah_sips_on-p", "wtah_350_flat-p")) %>% 
  distinct(interval, region, settlement_payment_hist, settlement_payment_actual) %>% 
  group_by(region) %>% 
  summarise(REGION_settlement_hist = sum(settlement_payment_hist), 
            REGION_settlement_actual = sum(settlement_payment_actual),
            REGION_perc = (REGION_settlement_hist-REGION_settlement_actual)/REGION_settlement_actual*100) %>% 
  ungroup %>% 
  mutate(NEM_hist = sum(REGION_settlement_hist),
         NEM_actual = sum(REGION_settlement_actual),
         NEM_perc = (NEM_hist-NEM_actual)/NEM_actual*100)

settlement %>% 
  filter(scenario %in% c("wtah_sips_on-p", "wtah_350_flat-p")) %>% 
  distinct(interval) %>% 
  nrow(.)/12

# ---- Figure 10 ----
data <- ic_flows %>% 
  filter(scenario %in% scenarios) %>% 
  mutate(direction = case_when(flow_hist < 0 | (link == "BLNKTAS" & flow_hist > 0) ~ "Import",
                               flow_hist > 0 ~ "Export"),
         interconnection = case_when(link %in% c("V-SA", "V-S-MNSP1") ~ "V-S",
                                     link %in% c("NSW1-QLD1", "N-Q-MNSP1") ~ "N-Q",
                                     link %in% c("BLNKTAS", "BLNKVIC") ~ "V-T",
                                     link == "VIC1-NSW1" ~ "V-N"),
         flow_hist = case_when(direction == "Import" & link != "BLNKTAS" ~ -1 * flow_hist,
                               TRUE ~ flow_hist),
         flow = case_when(direction =="Import" & link != "BLNKTAS" ~ -1 * flow,
                          TRUE ~ flow)) %>% 
  filter(!is.na(direction)) %>% 
  group_by(interval, scenario, interconnection, direction, period) %>% 
  summarise(flow = sum(flow),
            flow_hist = sum(flow_hist)) %>% 
  ungroup() %>% 
  pivot_longer(cols = c(flow, flow_hist), names_to = "series", values_to = "MW") %>% 
  mutate(series = case_when(series == "flow_hist" ~ "Base Case",
                            series == "flow" ~ setNames(names(scenarios), scenarios)[scenario]))

data %>% 
  filter(interconnection == "V-N") %>% 
  ggplot() +
  geom_boxplot(aes(x = MW, y = direction, fill = series), position = position_dodge(reverse = TRUE)) +
  theme_bw() +
  #facet_wrap(~interconnection) +
  theme(legend.position = c(0.98, 0.35), legend.justification = c("right", "top") , 
        legend.title = element_blank(), legend.text = element_text(size = 6)) +
  ylab(NULL) +
  xlab("VNI Flow (MW)") +
  scale_fill_discrete(palette = c("#ABB6C2", "grey45"))

data %>% 
  filter(interconnection == "V-N") %>% 
  group_by(direction, series) %>% 
  summarise(p50 = median(MW)) %>% 
  ungroup()
  
  