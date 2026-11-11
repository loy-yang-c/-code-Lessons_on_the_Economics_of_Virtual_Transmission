# ---- Packages ----
library(tidyverse)
library(arrow)

# ---- PARQUET FILE PATH ----
parquet_path = "../../Paper 1/2. NEMPY/results/"

# ---- .Parquet to .RDS ----
dispatchprice <- read_csv("./raw_data/dispatchprice.csv") %>%
  mutate(period = case_when(SETTLEMENTDATE >= ymd_hms("2021-11-29 04:00:00") & SETTLEMENTDATE < ymd_hms("2022-04-01 04:00:00") ~ "SIPS1",
                            SETTLEMENTDATE >= ymd_hms("2022-11-01 04:00:00") & SETTLEMENTDATE < ymd_hms("2023-04-01 04:00:00") ~ "SIPS2",
                            SETTLEMENTDATE >= ymd_hms("2023-11-01 04:00:00") & SETTLEMENTDATE < ymd_hms("2024-04-01 04:00:00") ~ "SIPS3",
                            SETTLEMENTDATE >= ymd_hms("2024-11-01 04:00:00") & SETTLEMENTDATE < ymd_hms("2025-04-01 04:00:00") ~ "SIPS4",
                            SETTLEMENTDATE >= ymd_hms("2025-07-31 04:00:00") ~ "SIPS5",
                            TRUE ~ "NONSIPS")) %>% 
  filter(INTERVENTION == 0)

totaldemand <- read_csv("./raw_data/dispatchregionsum.csv") %>% 
  filter(INTERVENTION == 0)
dispatchcasesolution <- read_csv("./raw_data/dispatchcasesolution.csv") %>% 
  filter(INTERVENTION == 0) %>% 
  select(SETTLEMENTDATE, TOTALOBJECTIVE) %>% 
  rename("interval"=SETTLEMENTDATE, "sys_cost_actual"=TOTALOBJECTIVE)
wsb_sips_avail <- read_csv("./raw_data/scrape_2025-07-31_2026-02-01.csv") %>% 
  select(-`...1`) %>% 
  mutate(interval = ymd_hms(interval))

# Simulation
scenarios = c("vbb_sips_on-p", "wtah_sips_on-p", "wtah_350_flat-p", "vni_voltage_100-p", "vni_arbe_elml_relaxation-p")
input_mapping = c("vni_contract_binding", "wsb_thermal_binding", "wsb_thermal_binding", "vni_voltage_binding", "arbe_elml_binding")

nempy_energy_prices <- tibble()
nempy_fcas_prices <- tibble()
nempy_constraint_prices <- tibble()
nempy_unit_dispatch <- tibble()
nempy_ic_flows <- tibble()
nempy_obj_value <- tibble()
binding_set <- c()
for (s in c(scenarios)) {
  
  # empty tibbles
  scenario_temp_energy <- tibble()
  scenario_temp_fcas <- tibble()
  scenario_temp_constraint <- tibble()
  scenario_temp_unit_dispatch <- tibble()
  scenario_temp_ic_flows <- tibble()
  scenario_temp_obj_value <- tibble()
  
  # import parquet files
  for (i in c("SIPS1", "SIPS2", "SIPS3", "SIPS4", "SIPS5")) {
    
    if (dir.exists(paste0(parquet_path, i, "/", s, "/energy_prices/"))) {
      temp <- open_dataset(paste0(parquet_path, i, "/", s, "/energy_prices/")) %>% 
        collect()
      scenario_temp_energy <- scenario_temp_energy %>% 
        rbind(temp)
      
      temp <- open_dataset(paste0(parquet_path, i, "/", s, "/fcas_prices/")) %>% 
        collect()
      scenario_temp_fcas <- scenario_temp_fcas %>% 
        rbind(temp)
      
      temp <- open_dataset(paste0(parquet_path, i, "/", s, "/constraint_prices/")) %>% 
        collect()
      scenario_temp_constraint <- scenario_temp_constraint %>% 
        rbind(temp)
      
      temp <- open_dataset(paste0(parquet_path, i, "/", s, "/unit_dispatch/")) %>% 
        collect()
      scenario_temp_unit_dispatch <- scenario_temp_unit_dispatch %>% 
        rbind(temp)
      
      temp <- open_dataset(paste0(parquet_path, i, "/", s, "/ic_flows/")) %>% 
        collect()
      scenario_temp_ic_flows <- scenario_temp_ic_flows %>% 
        rbind(temp)
      
      temp <- open_dataset(paste0(parquet_path, i, "/", s, "/obj_value/")) %>% 
        collect()
      scenario_temp_obj_value <- scenario_temp_obj_value %>% 
        rbind(temp)
    }
  }
  scenario_temp_energy <- scenario_temp_energy %>% 
    mutate(interval = ymd_hms(interval),
           scenario = s)
  nempy_energy_prices <- nempy_energy_prices %>% 
    rbind(scenario_temp_energy)
  
  scenario_temp_fcas <- scenario_temp_fcas %>% 
    mutate(interval = ymd_hms(interval),
           scenario = s)
  nempy_fcas_prices <- nempy_fcas_prices %>% 
    rbind(scenario_temp_fcas)
  
  scenario_temp_constraint <- scenario_temp_constraint %>% 
    mutate(interval = ymd_hms(interval),
           scenario = s)
  nempy_constraint_prices <- nempy_constraint_prices %>% 
    rbind(scenario_temp_constraint)
  
  scenario_temp_unit_dispatch <- scenario_temp_unit_dispatch %>% 
    mutate(interval = ymd_hms(interval),
           scenario = s)
  nempy_unit_dispatch <- nempy_unit_dispatch %>% 
    rbind(scenario_temp_unit_dispatch)
  
  scenario_temp_ic_flows <- scenario_temp_ic_flows %>% 
    mutate(interval = ymd_hms(interval),
           scenario = s)
  nempy_ic_flows <- nempy_ic_flows %>% 
    rbind(scenario_temp_ic_flows)
  
  scenario_temp_obj_value <- scenario_temp_obj_value %>% 
    mutate(interval = ymd_hms(interval),
           scenario = s)
  nempy_obj_value <- nempy_obj_value %>% 
    rbind(scenario_temp_obj_value)
  
  temp <- scenario_temp_energy %>% 
    .$interval %>% 
    unique()
  
  assign(paste0(s, "_flag"), temp)
  
  binding_set <- c(binding_set, temp) %>% 
    unique(.)
  
}

binding_set <- as.POSIXct(binding_set, tz="UTC", format = "%Y-%m-%d %H:%M:%S")

# empty tibbles
scenario_temp_energy <- tibble()
scenario_temp_fcas <- tibble()
scenario_temp_constraint <- tibble()
scenario_temp_unit_dispatch <- tibble()
scenario_temp_ic_flows <- tibble()
scenario_temp_obj_value <- tibble()

# import parquet files
for (i in c("SIPS1", "SIPS2", "SIPS3", "SIPS4", "SIPS5")) {
  
  if (dir.exists(paste0(parquet_path, i, "/", "historical-p_re-run", "/energy_prices/"))) {
    temp <- open_dataset(paste0(parquet_path, i, "/", "historical-p_re-run", "/energy_prices/")) %>% 
      collect() %>% 
      mutate(interval = ymd_hms(interval),
             scenario = "historical-p")
    scenario_temp_energy <- scenario_temp_energy %>% 
      rbind(temp)
    
    temp <- open_dataset(paste0(parquet_path, i, "/", "historical-p_re-run", "/fcas_prices/")) %>% 
      collect() %>% 
      mutate(interval = ymd_hms(interval),
             scenario = "historical-p")
    scenario_temp_fcas <- scenario_temp_fcas %>% 
      rbind(temp)
    
    temp <- open_dataset(paste0(parquet_path, i, "/", "historical-p_re-run", "/constraint_prices/")) %>% 
      collect() %>% 
      mutate(interval = ymd_hms(interval),
             scenario = "historical-p") %>% 
      filter(interval %in% binding_set)
    scenario_temp_constraint <- scenario_temp_constraint %>% 
      rbind(temp)
    
    cat("Finished constraint prices, period ", i)
    
    temp <- open_dataset(paste0(parquet_path, i, "/", "historical-p_re-run", "/unit_dispatch/")) %>% 
      collect() %>% 
      mutate(interval = ymd_hms(interval),
             scenario = "historical-p") %>% 
      filter(interval %in% binding_set)
    scenario_temp_unit_dispatch <- scenario_temp_unit_dispatch %>% 
      rbind(temp)
    
    cat("Finished unit_dispatch, period ", i)
    
    temp <- open_dataset(paste0(parquet_path, i, "/", "historical-p_re-run", "/ic_flows/")) %>% 
      collect() %>% 
      mutate(interval = ymd_hms(interval),
             scenario = "historical-p") %>% 
      filter(interval %in% binding_set)
    scenario_temp_ic_flows <- scenario_temp_ic_flows %>% 
      rbind(temp)
    
    temp <- open_dataset(paste0(parquet_path, i, "/", "historical-p_re-run", "/obj_value/")) %>% 
      collect() %>% 
      mutate(interval = ymd_hms(interval),
             scenario = "historical-p") %>% 
      filter(interval %in% binding_set)
    scenario_temp_obj_value <- scenario_temp_obj_value %>% 
      rbind(temp)
  }
}

# CVP parquet
cvp_by_constraint <- tibble()
cvp_by_type <- tibble()

for (i in c("SIPS1", "SIPS2","SIPS3", "SIPS4", "SIPS5")) {
  
  temp <- open_dataset(paste0(parquet_path, i, "/cvp/generic/")) %>% 
    collect() %>% 
    mutate(interval = ymd_hms(format(interval, format = "%Y-%m-%d %T"))) %>% 
    filter(interval %in% binding_set)
  
  cvp_by_constraint <- cvp_by_constraint %>% 
    rbind(temp)
  
  temp <- open_dataset(paste0(parquet_path, i, "/cvp/case/")) %>% 
    collect() %>% 
    mutate(interval = ymd_hms(format(interval, format = "%Y-%m-%d %T"))) %>% 
    filter(interval %in% binding_set)
  
  cvp_by_type <- cvp_by_type %>% 
    rbind(temp)
  
}

nempy_energy_prices <- nempy_energy_prices %>% 
  rbind(scenario_temp_energy)

nempy_fcas_prices <- nempy_fcas_prices %>% 
  rbind(scenario_temp_fcas)

nempy_constraint_prices <- nempy_constraint_prices %>% 
  rbind(scenario_temp_constraint)

nempy_unit_dispatch <- nempy_unit_dispatch %>% 
  rbind(scenario_temp_unit_dispatch)

nempy_ic_flows <- nempy_ic_flows %>% 
  rbind(scenario_temp_ic_flows)

nempy_obj_value <- nempy_obj_value %>% 
  rbind(scenario_temp_obj_value)

# market price caps, fcas service mapping, binding intervals and op demand
mpc <- c("SIPS1" = 15100, "SIPS2" = 15500, "SIPS3" = 16600, "SIPS4" = 17500, "SIPS5" = 20300)
fcas_service_mapping = c("RAISEREGROP" = "raise_reg", "LOWERREGROP" = "lower_reg",
                         "RAISE1SECROP" = "raise_1s", "RAISE6SECROP" = "raise_6s", 
                         "RAISE60SECROP" = "raise_60s", "RAISE5MINROP" = "raise_5min",
                         "LOWER1SECROP" = "lower_1s", "LOWER6SECROP" = "lower_6s", 
                         "LOWER60SECROP" = "lower_60s", "LOWER5MINROP" = "lower_5min")

# create coalesced datasets
intervals_removed <- dispatchprice %>% 
  filter(period != "NONSIPS") %>% 
  group_by(SETTLEMENTDATE) %>% 
  summarise(APCFLAG = max(APCFLAG),
            MARKETSUSPENDEDFLAG = max(MARKETSUSPENDEDFLAG)) %>% 
  ungroup %>% 
  # remove market suspended per 1. price_validation.R
  filter(MARKETSUSPENDEDFLAG > 0) %>% 
  .$SETTLEMENTDATE

# only allow wsb_sips_on to have entries when WSB_SIPS_AVAIL and PAIRED GENERATION > 0
wsb_sips_on_intervals <- wsb_sips_avail %>% 
  filter((spd_id == "WTAH_SIPS_AVAIL") | 
           (spd_id %in% c("METZSF1.NMTZ1M", "SAPHWF1.NSAP1S") & ems_id == "MW")) %>% 
  select(-ems_id) %>% 
  pivot_wider(names_from = spd_id, values_from = value) %>% 
  mutate(paired_gen = `METZSF1.NMTZ1M` + `SAPHWF1.NSAP1S`)

wtah_sips_on <- wsb_sips_on_intervals %>% 
  filter(WTAH_SIPS_AVAIL > 0 & paired_gen > 0) %>% 
  .$interval %>% 
  unique(.)

wtah_350_flat <- wsb_sips_on_intervals %>% 
  filter(paired_gen > 0) %>% 
  .$interval %>% 
  unique(.)

historic_energy_prices <- nempy_energy_prices %>% 
  select(interval, region, scenario, price) %>% 
  filter(scenario == "historical-p") %>% 
  distinct() %>% 
  select(-scenario) %>% 
  left_join(
    select(dispatchprice, SETTLEMENTDATE, REGIONID, period, ROP), 
    by = c("interval"="SETTLEMENTDATE", "region"="REGIONID")) %>% 
  filter(period != "NONSIPS" & !(interval %in% intervals_removed)) %>% 
  mutate(mpc = mpc[period],
         price = pmax(pmin(price, mpc), -1000),
         ROP = pmax(pmin(ROP, mpc), -1000)) %>% 
  select(-mpc) %>% 
  rename("historic-p" = price)

energy_prices <- nempy_energy_prices %>% 
  select(interval, region, scenario, price) %>%
  filter(scenario %in% scenarios) %>% 
  distinct() %>%
  left_join(
    historic_energy_prices, 
    by = c("interval", "region")) %>% 
  filter(period != "NONSIPS" & !(interval %in% intervals_removed)) %>% 
  mutate(mpc = mpc[period],
         price = pmax(pmin(price, mpc), -1000),
         # wsb_sips_on condition
         price = case_when(scenario == "wtah_sips_on-p" & !(interval %in% wtah_sips_on) ~ `historic-p`,
                           scenario == "wtah_350_flat-p" & !(interval %in% wtah_350_flat) ~ `historic-p`,
                           TRUE ~ price))%>% 
  select(-mpc)

historic_fcas_prices <- nempy_fcas_prices %>% 
  select(interval, region, service, scenario, price) %>% 
  filter(scenario == "historical-p") %>% 
  distinct() %>% 
  select(-scenario) %>% 
  left_join(
    mutate(
      pivot_longer(
        select(dispatchprice, SETTLEMENTDATE, REGIONID, period,
               names(fcas_service_mapping)),
        cols = names(fcas_service_mapping),
        names_to = "service",
        values_to = "ROP"),
      service = fcas_service_mapping[service]),
    by = c("interval"="SETTLEMENTDATE", "region"="REGIONID", "service"="service")) %>% 
  filter(period != "NONSIPS" & !(interval %in% intervals_removed)) %>% 
  mutate(mpc = mpc[period],
         price = pmax(pmin(price, mpc), -1000),
         ROP = pmax(pmin(ROP, mpc), -1000)) %>% 
  select(-mpc) %>% 
  rename("historic-p" = price)

fcas_prices <- nempy_fcas_prices %>% 
  select(interval, region, service, scenario, price) %>% 
  filter(scenario %in% scenarios) %>%
  distinct() %>% 
  left_join(
    historic_fcas_prices,
    by = c("interval", "region", "service")) %>% 
  filter(period != "NONSIPS" & !(interval %in% intervals_removed)) %>% 
  mutate(mpc = mpc[period],
         price = pmax(pmin(price, mpc), -1000),
         # wsb_sips_on condition
         price = case_when(scenario == "wtah_sips_on-p" & !(interval %in% wtah_sips_on) ~ `historic-p`,
                           scenario == "wtah_350_flat-p" & !(interval %in% wtah_350_flat) ~ `historic-p`,
                           TRUE ~ price)) %>% 
  select(-mpc)

historic_constraint_prices <- nempy_constraint_prices %>% 
  select(interval, set, type, rhs, slack, price, scenario) %>% 
  filter(scenario == "historical-p") %>% 
  select(-scenario) %>% 
  distinct() %>% 
  rename("rhs_hist" = rhs, "slack_hist" = slack, "price_hist" = price) %>% 
  mutate(period = case_when(interval >= ymd_hms("2021-11-29 04:00:00") & interval < ymd_hms("2022-04-01 04:00:00") ~ "SIPS1",
                            interval >= ymd_hms("2022-11-01 04:00:00") & interval < ymd_hms("2023-04-01 04:00:00") ~ "SIPS2",
                            interval >= ymd_hms("2023-11-01 04:00:00") & interval < ymd_hms("2024-04-01 04:00:00") ~ "SIPS3",
                            interval >= ymd_hms("2024-11-01 04:00:00") & interval < ymd_hms("2025-04-01 04:00:00") ~ "SIPS4",
                            interval >= ymd_hms("2025-07-31 04:00:00") ~ "SIPS5",
                            TRUE ~ "NONSIPS")) %>% 
  filter(period != "NONSIPS" & !(interval %in% intervals_removed))

constraint_prices <- nempy_constraint_prices %>% 
  select(interval, set, type, rhs, slack, price, scenario) %>% 
  filter(scenario %in% scenarios) %>%
  distinct() %>% 
  left_join(historic_constraint_prices,
            by = c("interval", "set", "type")) %>% 
  filter(period != "NONSIPS" & !(interval %in% intervals_removed)) %>% 
  # wsb_sips_on condition
  mutate(rhs = case_when(scenario == "wtah_sips_on-p" & !(interval %in% wtah_sips_on) ~ rhs_hist,
                         scenario == "wtah_350_flat-p" & !(interval %in% wtah_350_flat) ~ rhs_hist,
                         TRUE ~ rhs),
         slack = case_when(scenario == "wtah_sips_on-p" & !(interval %in% wtah_sips_on) ~ slack_hist,
                           scenario == "wtah_350_flat-p" & !(interval %in% wtah_350_flat) ~ slack_hist,
                           TRUE ~ slack),
         price = case_when(scenario == "wtah_sips_on-p" & !(interval %in% wtah_sips_on) ~ price_hist,
                           scenario == "wtah_350_flat-p" & !(interval %in% wtah_350_flat) ~ price_hist,
                           TRUE ~ price))

# add CVP penalty by constraint type
cvp_by_constraint <- cvp_by_constraint %>%
  select(interval, set, cost)

gc()

constraint_prices <- constraint_prices %>% 
  left_join(cvp_by_constraint, by = c("interval", "set")) %>% 
  mutate(violation = case_when(abs(price) == cost ~ 1,
                               TRUE ~ 0),
         violation_hist = case_when(abs(price_hist) == cost ~ 1,
                                    TRUE ~ 0),
         to_exclude = case_when(violation != violation_hist ~ 1,
                                TRUE ~ 0))

violations_by_scenario <- constraint_prices %>% 
  group_by(scenario, interval) %>% 
  summarise(violations = sum(violation),
            violations_hist = sum(violation_hist),
            exclude_interval = pmin(sum(to_exclude), 1)) %>% 
  ungroup() %>% 
  mutate(violation_interval = pmin(violations + violations_hist, 1))

# scenario violation rates
#violations_by_scenario %>% group_by(scenario) %>% summarise(scen = sum(violations > 0)/n(), hist = sum(violations_hist > 0)/n())
# historic violation rate by constraint
#constraint_prices %>% group_by(set) %>% summarise(rate = sum(violation_hist)/n()) %>% ungroup() %>% arrange(desc(rate)) %>% .[1:50,] %>% 
#  ggplot() + geom_bar(aes(y = reorder(set, rate), x = rate), stat="identity") + theme_minimal()
# scenario exclude rates
violations_by_scenario %>% group_by(scenario) %>% summarise(rate = sum(exclude_interval > 0)/n())


historic_unit_dispatch <- nempy_unit_dispatch %>% 
  select(interval, unit, dispatch_type, service, dispatch, scenario) %>% 
  filter(scenario == "historical-p") %>% 
  select(-scenario) %>% 
  distinct() %>% 
  rename("dispatch_hist" = dispatch) %>% 
  mutate(period = case_when(interval >= ymd_hms("2021-11-29 04:00:00") & interval < ymd_hms("2022-04-01 04:00:00") ~ "SIPS1",
                            interval >= ymd_hms("2022-11-01 04:00:00") & interval < ymd_hms("2023-04-01 04:00:00") ~ "SIPS2",
                            interval >= ymd_hms("2023-11-01 04:00:00") & interval < ymd_hms("2024-04-01 04:00:00") ~ "SIPS3",
                            interval >= ymd_hms("2024-11-01 04:00:00") & interval < ymd_hms("2025-04-01 04:00:00") ~ "SIPS4",
                            interval >= ymd_hms("2025-07-31 04:00:00") ~ "SIPS5",
                            TRUE ~ "NONSIPS")) %>% 
  filter(period != "NONSIPS" & !(interval %in% intervals_removed))

unit_dispatch <- nempy_unit_dispatch %>% 
  select(interval, unit, dispatch_type, service, dispatch, scenario) %>% 
  filter(scenario %in% scenarios) %>%
  distinct() %>% 
  left_join(historic_unit_dispatch,
            by = c("interval", "unit", "dispatch_type", "service")) %>% 
  filter(period != "NONSIPS" & !(interval %in% intervals_removed)) %>% 
  # wsb_sips_on condition
  mutate(rhs = case_when(scenario == "wtah_sips_on-p" & !(interval %in% wtah_sips_on) ~ dispatch_hist,
                         scenario == "wtah_350_flat-p" & !(interval %in% wtah_350_flat) ~ dispatch_hist,
                         TRUE ~ dispatch))

historic_ic_flows <- nempy_ic_flows %>% 
  select(interval, interconnector, link, flow, losses, scenario) %>% 
  filter(scenario == "historical-p") %>% 
  select(-scenario) %>% 
  distinct() %>% 
  rename("flow_hist" = flow, "losses_hist" = losses) %>% 
  mutate(period = case_when(interval >= ymd_hms("2021-11-29 04:00:00") & interval < ymd_hms("2022-04-01 04:00:00") ~ "SIPS1",
                            interval >= ymd_hms("2022-11-01 04:00:00") & interval < ymd_hms("2023-04-01 04:00:00") ~ "SIPS2",
                            interval >= ymd_hms("2023-11-01 04:00:00") & interval < ymd_hms("2024-04-01 04:00:00") ~ "SIPS3",
                            interval >= ymd_hms("2024-11-01 04:00:00") & interval < ymd_hms("2025-04-01 04:00:00") ~ "SIPS4",
                            interval >= ymd_hms("2025-07-31 04:00:00") ~ "SIPS5",
                            TRUE ~ "NONSIPS")) %>% 
  filter(period != "NONSIPS" & !(interval %in% intervals_removed))

ic_flows <- nempy_ic_flows %>% 
  select(interval, interconnector, link, flow, losses, scenario) %>% 
  filter(scenario %in% scenarios) %>%
  distinct() %>% 
  left_join(historic_ic_flows,
            by = c("interval", "interconnector", "link")) %>% 
  filter(period != "NONSIPS" & !(interval %in% intervals_removed)) %>% 
  # wsb_sips_on condition
  mutate(flow = case_when(scenario == "wtah_sips_on-p" & !(interval %in% wtah_sips_on) ~ flow_hist,
                          scenario == "wtah_350_flat-p" & !(interval %in% wtah_350_flat) ~ flow_hist,
                          TRUE ~ flow),
         losses = case_when(scenario == "wtah_sips_on-p" & !(interval %in% wtah_sips_on) ~ losses_hist,
                            scenario == "wtah_350_flat-p" & !(interval %in% wtah_350_flat) ~ losses_hist,
                            TRUE ~ losses))

historic_obj_value <- nempy_obj_value %>% 
  select(interval, sys_cost, scenario) %>% 
  filter(scenario == "historical-p") %>% 
  select(-scenario) %>% 
  distinct() %>% 
  rename("sys_cost_hist" = sys_cost) %>% 
  mutate(period = case_when(interval >= ymd_hms("2021-11-29 04:00:00") & interval < ymd_hms("2022-04-01 04:00:00") ~ "SIPS1",
                            interval >= ymd_hms("2022-11-01 04:00:00") & interval < ymd_hms("2023-04-01 04:00:00") ~ "SIPS2",
                            interval >= ymd_hms("2023-11-01 04:00:00") & interval < ymd_hms("2024-04-01 04:00:00") ~ "SIPS3",
                            interval >= ymd_hms("2024-11-01 04:00:00") & interval < ymd_hms("2025-04-01 04:00:00") ~ "SIPS4",
                            interval >= ymd_hms("2025-07-31 04:00:00") ~ "SIPS5",
                            TRUE ~ "NONSIPS")) %>% 
  filter(period != "NONSIPS" & !(interval %in% intervals_removed))

obj_value <- nempy_obj_value %>% 
  select(interval, sys_cost, scenario) %>% 
  filter(scenario %in% scenarios) %>% 
  distinct() %>% 
  left_join(historic_obj_value,
            by = c("interval")) %>% 
  filter(period != "NONSIPS" & !(interval %in% intervals_removed)) %>% 
  # wsb_sips_on condition
  mutate(sys_cost = case_when(scenario == "wtah_sips_on-p" & !(interval %in% wtah_sips_on) ~ sys_cost_hist,
                              scenario == "wtah_350_flat-p" & !(interval %in% wtah_350_flat) ~ sys_cost_hist,
                              TRUE ~ sys_cost)) %>% 
  # constraint violation intervals and calculate sys_cost differences
  left_join(violations_by_scenario, by = c("interval", "scenario")) %>%
  mutate(sys_cost_delta = sys_cost_hist - sys_cost)

# we retrospectively add some additional CVP exclusion criteria
obj_value <- obj_value %>% 
  left_join(dispatchcasesolution, by = c("interval"))

#A) if the constraint of interest itself is in violation, we exclude that interval.
# B) if the actual/modelled sys_cost error ratio exceeds 0.05, exclude 
scenarios <- unique(obj_value$scenario)

vbb_sips_constraints_on <- read_csv("./raw_data/genconid_VBB_SIPS_in_dispatch_frequency.csv")$CONSTRAINTID
wtah_sips_constraints <- c("DSNAP_N>>NIL_86_85_S", "N>>NIL_84_88_S","N>>NIL_88_84_S",
                           "N>>NIL_85_86_S", "N>>NIL_86_85_S", "N>>NIL_86_8U_S", "N>>NIL_8U_86_S") # from genericconstraintrhs_wsb.csv
arbe_elml_constraints <- c("V>>BAML1_ELML_BAML2", "V>>BAML2_ELML_BAML1", "V>>BDBU_ELML_BAML2", "V>>NIL_ELML_BAML2",
                           "V>>V_NIL_18", "V>>NIL_ARWB_KGBE", "V>>BDBU_ARWB_KGBE", "V>>NIL_WBBA_KGBE", "V>>V_NIL_9", "V>>BDBU_WBBA_KGBE") # from genericconstraintrhs_VIC.csv
lcr <- c("N^^V_NIL_1", "N^^V_DDSM1", "N^^V_SM_SCAP", "N^^V_MLNK_1", "N^^V_DPWG_1", "N^^V_DPWG_X5_1", "N^^V_CNYS_1",
         "N^^V_CNLT_1", "N^^V_CNCW_1", "N^^V_CTMN_1", "N^^V_LTYS_1", "N^^V_LTMS_1", "N^^V_LTUT_1", "N^^V_DTSS_1", 
         "N^^V_MSUT_1", "N^^V_BURC_1", "N^^V_BADP_1", "N^^V_2+5_1", "N^^V_2_6X_3J_1", "N^^V_3L+11_1", "N^^V_4+07_1", 
         "N^^V_JNWG_63_X5_1", "N^^V_MSDD1", "N^^V_LTWG_1", "N^^V_BUDP_1")
arwbba <- c("N^^V_NIL_ARWBBA", "N^^V_MLNK_ARWBBA")

constraint_prices <- constraint_prices %>% 
  mutate(coi = case_when(scenario == scenarios[1] & set %in% vbb_sips_constraints_on ~ 1,
                         scenario %in% scenarios[2:3] & set %in% wtah_sips_constraints ~ 1,
                         scenario == scenarios[4] & set %in% c(lcr, arwbba) ~ 1,
                         scenario == scenarios[5] & set %in% arbe_elml_constraints ~ 1,
                         TRUE ~ 0),
         to_exclude = case_when((violation + violation_hist > 0) & coi == 1 ~ 1,
                                TRUE ~ to_exclude))

violations_by_scenario <- constraint_prices %>% 
  group_by(scenario, interval) %>% 
  summarise(violations = sum(violation),
            violations_hist = sum(violation_hist),
            exclude_interval = pmin(sum(to_exclude), 1)) %>% 
  ungroup() %>% 
  mutate(violation_interval = pmin(violations + violations_hist, 1))

obj_value <- obj_value %>% 
  select(-violations, -violations_hist, -exclude_interval, -violation_interval) %>% 
  left_join(violations_by_scenario, by = c("interval", "scenario")) %>% 
  # manually remove this interval because of loss of load, which isn't picked by our CVP evaluation framework
  mutate(model_error_ratio = sys_cost_actual/sys_cost_hist - 4,
         exclude_interval = case_when(day(interval) == 13 & month(interval) == 2 & year(interval) == 2024 ~ 1,
                                      TRUE ~ exclude_interval))

saveRDS(list(energy_prices, fcas_prices, constraint_prices, ic_flows, unit_dispatch, obj_value), file = "./secondary_data/results.rds")
