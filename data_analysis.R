# ---- Packages ----
library(tidyverse)

# ---- Load and format data ----
wsb_sips_avail <- read_csv("./raw_data/scrape_2025-07-31_2026-02-01.csv") %>% 
  select(-`...1`) %>% 
  mutate(interval = ymd_hms(interval))
network_scrape <- read_csv("./raw_data/scrape_2021-11-29_2022-04-02.csv") %>% 
  rbind(read_csv("./raw_data/scrape_2022-11-01_2023-04-02.csv")) %>%
  rbind(read_csv("./raw_data/scrape_2023-11-01_2024-04-02.csv")) %>%
  rbind(read_csv("./raw_data/scrape_2024-11-01_2025-04-02.csv")) %>%
  rbind(read_csv("./raw_data/scrape_2025-07-31_2026-02-02.csv")) %>% 
  select(-`...1`) %>% 
  mutate(interval = ymd_hms(interval))
wsb_rrat <- read_csv("./raw_data/network_realtimerating_NRAT_2025-07-31_present.csv")
wsb_srat <- read_csv("./raw_data/network_staticrating_84_85_86_88.csv")
wsb_dict <- read_csv("./raw_data/network_rating_nrat.csv")

# there is no change in dictionary mapping over the period 31-07-2025
wsb_dict <- wsb_dict %>% 
  filter(VALIDTO == max(VALIDTO)) %>% 
  filter(ISDYNAMIC == 0)

spd_static_mapping = setNames(wsb_dict$RATINGLEVEL, wsb_dict$SPD_ID)

# same with static mapping
wsb_srat <- wsb_srat %>% 
  filter(VALIDTO == max(VALIDTO)) %>% 
  mutate(season = substr(APPLICATIONID, 1, 5),
         nch = nchar(APPLICATIONID),
         tod = substr(APPLICATIONID, nch - 2, nch - 2)) %>% 
  select(-nch)

wsb_rrat_to_append <- tibble(SETTLEMENTDATE = unique(wsb_rrat$SETTLEMENTDATE)) %>% 
  mutate(season = case_when(month(SETTLEMENTDATE) > 11 | month(SETTLEMENTDATE) <= 2 ~ "N_SUM",
                            6 <= month(SETTLEMENTDATE) & month(SETTLEMENTDATE) <= 8 ~ "N_WIN",
                            TRUE ~ "N_AUT"),
         tod = case_when(SETTLEMENTDATE < ymd_hms(paste0(substr(SETTLEMENTDATE, 1, 11), "18:10:00")) & 
                           SETTLEMENTDATE >= ymd_hms(paste0(substr(SETTLEMENTDATE, 1, 11), "6:10:00")) ~ "D",
                         TRUE ~ "N"))

substationid_spd_mapping = c("AR"="ARMIDALE", "TM"="TAMWORTH", "TW"="TAMWORTH")

# night and day rating are applied 6:10-6:05
for (i in names(spd_static_mapping)) {
  lookup <- wsb_srat %>% 
    filter(EQUIPMENTID == as.numeric(substr(i, nchar(i)-1, nchar(i))) & RATINGLEVEL == spd_static_mapping[i])
  
  temp = wsb_rrat_to_append %>% 
    mutate(SUBSTATIONID = substationid_spd_mapping[substr(i, 8, 9)]) %>% 
    left_join(select(lookup, SUBSTATIONID, season, tod, RATINGVALUE), by = c("SUBSTATIONID", "season", "tod"))
  
  wsb_rrat_to_append = wsb_rrat_to_append %>% 
    left_join(select(temp, SETTLEMENTDATE, RATINGVALUE), by = "SETTLEMENTDATE") %>% 
    rename(!!i := "RATINGVALUE")
  
}

wsb_rrat_to_append <- wsb_rrat_to_append %>% 
  select(-season, -tod) %>% 
  pivot_longer(cols = names(spd_static_mapping), names_to = "SPD_ID", values_to = "RATINGVALUE") %>% 
  mutate(type = substr(SPD_ID, 1, 6),
         line = substr(SPD_ID, 8, 100)) %>% 
  select(SETTLEMENTDATE, line, type, RATINGVALUE) %>% 
  mutate(rank = 2)

wsb_rrat <- wsb_rrat %>% 
  mutate(type = substr(SPD_ID, 1, 6),
         line = substr(SPD_ID, 8, 100)) %>% 
  select(-SPD_ID) %>% 
  mutate(rank = 1) %>% 
  rbind(wsb_rrat_to_append) %>% 
  group_by(SETTLEMENTDATE, line, type) %>% 
  filter(rank == min(rank)) %>% 
  ungroup() %>% 
  select(-rank) %>% 
  pivot_wider(names_from = type, values_from = RATINGVALUE)

# Add 5min constants (from genericconstraintrhs_WSB.csv)
wsb_rrat <- wsb_rrat %>% 
  mutate(NRAT5 = case_when(line == "TMLD84" & SETTLEMENTDATE >= ymd_hms("2025-09-22 00:00:00", tz = "UTC") ~ 1800,
                           line == "TMLD84"~ 1707,
                           line == "TWUL85" & SETTLEMENTDATE >= ymd_hms("2025-09-22 00:00:00", tz = "UTC") ~ 1579,
                           line == "TWUL85" ~ 1360,
                           line == "ARTW86" & SETTLEMENTDATE >= ymd_hms("2025-09-22 00:00:00", tz = "UTC") ~ 1260,
                           line == "ARTW86" ~ 1410,
                           line == "TMMU88" & SETTLEMENTDATE >= ymd_hms("2025-09-22 00:00:00", tz = "UTC") ~ 1800,
                           line == "TMMU88" ~ 1707)) 

wsb_line_ratings <- wsb_rrat %>% 
  pivot_wider(id_cols = SETTLEMENTDATE, names_from = line, values_from = c(NRATCN, NRAT15, NRAT5), names_sep = "_")

ic <- read_csv("./raw_data/dispatchinterconnectorres.csv") %>% 
  filter(INTERCONNECTORID == "VIC1-NSW1")

vbb_constraints_mapping <- read_csv("./secondary_data/genconid_VBB_SIPS_mapping.csv")
vbb_sips_constraints_on <- read_csv("./raw_data/genconid_VBB_SIPS_in_dispatch_frequency.csv")$CONSTRAINTID
vbb_sips_constraints_off <- vbb_constraints_mapping$CONSTRAINTID
vbb_corridor_mapping <- read_csv("./secondary_data/vbb_sips_constraint_groups.csv")
vni_import_constraints <- c(unique(ic$IMPORTGENCONID), "V>>BAML2_ELML_BAML1")
wtah_sips_constraints <- c("DSNAP_N>>NIL_86_85_S", "N>>NIL_84_88_S","N>>NIL_88_84_S",
                           "N>>NIL_85_86_S", "N>>NIL_86_85_S", "N>>NIL_86_8U_S", "N>>NIL_8U_86_S") # from genericconstraintrhs_wsb.csv
arbe_elml_constraints <- c("V>>BAML1_ELML_BAML2", "V>>BAML2_ELML_BAML1", "V>>BDBU_ELML_BAML2", "V>>NIL_ELML_BAML2",
                           "V>>V_NIL_18", "V>>NIL_ARWB_KGBE", "V>>BDBU_ARWB_KGBE", "V>>NIL_WBBA_KGBE", "V>>V_NIL_9", "V>>BDBU_WBBA_KGBE") # from genericconstraintrhs_VIC.csv
lcr <- c("N^^V_NIL_1", "N^^V_DDSM1", "N^^V_SM_SCAP", "N^^V_MLNK_1", "N^^V_DPWG_1", "N^^V_DPWG_X5_1", "N^^V_CNYS_1",
         "N^^V_CNLT_1", "N^^V_CNCW_1", "N^^V_CTMN_1", "N^^V_LTYS_1", "N^^V_LTMS_1", "N^^V_LTUT_1", "N^^V_DTSS_1", 
         "N^^V_MSUT_1", "N^^V_BURC_1", "N^^V_BADP_1", "N^^V_2+5_1", "N^^V_2_6X_3J_1", "N^^V_3L+11_1", "N^^V_4+07_1", 
         "N^^V_JNWG_63_X5_1", "N^^V_MSDD1", "N^^V_LTWG_1", "N^^V_BUDP_1")
arwbba <- c("N^^V_NIL_ARWBBA", "N^^V_MLNK_ARWBBA")

dispatchconstraint <- tibble()
for (i in c("20211208", "20220315", "20230427", "20240318", "20250216", "20250806", "20260318")) {
  temp <- read_csv(paste0("./raw_data/dispatchconstraint_", i, ".csv")) %>% 
    filter(CONSTRAINTID %in% c(vni_import_constraints, vbb_sips_constraints_on, vbb_sips_constraints_off, wtah_sips_constraints, arbe_elml_constraints,
                               lcr, arwbba) & INTERVENTION == 0)
  
  dispatchconstraint <- dispatchconstraint %>% 
    rbind(temp)
  
  rm(temp)
}

# ---- Table 2 ----
# Observed Values: WSB
wsb_sips_avail %>% 
  filter(((spd_id %in% c("WTAH_SIPS_ONSTATUS", 
                         "WTAH_SIPS_84_ON", "WTAH_SIPS_85_ON", "WTAH_SIPS_86_ON", "WTAH_SIPS_88_ON", "WTAH_SIPS_8U_ON", 
                         "METZSF_SIPS_STATUS", "SAPHWF1_SIPS_STATUS",
                         "WTAH_SIPS_AVAIL")) | (spd_id %in% c("METZSF1.NMTZ1M", "SAPHWF1.NSAP1S") & ems_id == "MW")) &
           (interval >= ymd_hms("2025-07-31 04:00:00") & interval < ymd_hms("2026-02-01 04:00:00")))%>%
  select(-ems_id) %>% 
  pivot_wider(names_from = spd_id, values_from = value) %>% 
  left_join(wsb_line_ratings, by = c("interval"="SETTLEMENTDATE")) %>% 
  # append contribution factors and construct expansion (from genericconstraintrhs_WSB.csv)
  mutate(METZ_CF_85_86 = 0.947,
         SAPH_CF_85_86 = 0.987,
         VT_CF_85_86 = 0.815,
         METZ_CF_86_85 = 0.959,
         SAPH_CF_86_85 = 1,
         VT_CF_86_85 = 0.819,
         METZ_CF_84_88 = 0.954,
         SAPH_CF_84_88 = 0.975,
         VT_CF_84_88 = 0.738,
         METZ_CF_88_84 = 0.954,
         SAPH_CF_88_84 = 0.975,
         VT_CF_88_84 = 0.738,
         METZ_CF_86_8U = 0.96,
         SAPH_CF_86_8U = 1,
         VT_CF_86_8U = 0.808,
         METZ_CF_8U_86 = 0.96,
         SAPH_CF_8U_86 = 1,
         VT_CF_8U_86 = 0.821) %>% 
  # remove non-SIP periods
  filter(!is.na(WTAH_SIPS_AVAIL)) %>% 
  mutate(`85/86_I` = NRAT5_TWUL85 - NRAT15_TWUL85,
         `86/85_I` = NRAT5_ARTW86 - NRAT15_ARTW86,
         `84/88_I` = NRAT5_TMLD84 - NRAT15_TMLD84,
         `88/84_I` = NRAT5_TMMU88 - NRAT15_TMMU88,
         `86/8U_I` = NRAT5_ARTW86 - NRAT15_ARTW86,
         `8U/86_I` = NRAT5_TWUL85 - NRAT15_TWUL85,
         III = METZSF1.NMTZ1M + SAPHWF1.NSAP1S,
         `85/86_epsilon` = NRAT15_TWUL85 - NRATCN_TWUL85,
         `86/85_epsilon` = NRAT15_ARTW86 - NRATCN_ARTW86,
         `84/88_epsilon` = NRAT15_TMLD84 - NRATCN_TMLD84,
         `88/84_epsilon` = NRAT15_TMMU88 - NRATCN_TMMU88,
         `86/8U_epsilon` = NRAT15_ARTW86 - NRATCN_ARTW86,
         `8U/86_epsilon` = NRAT15_TWUL85 - NRATCN_TWUL85,
         `85/86_gamma` = pmax(pmin(`85/86_I`, pmin(VT_CF_85_86 * WTAH_SIPS_AVAIL, METZ_CF_85_86 * METZSF1.NMTZ1M + SAPH_CF_85_86 * SAPHWF1.NSAP1S)-`85/86_epsilon`),0)*WTAH_SIPS_ONSTATUS*WTAH_SIPS_85_ON,
         `86/85_gamma` = pmax(pmin(`86/85_I`, pmin(VT_CF_86_85 * WTAH_SIPS_AVAIL, METZ_CF_86_85 * METZSF1.NMTZ1M + SAPH_CF_86_85 * SAPHWF1.NSAP1S)-`86/85_epsilon`),0)*WTAH_SIPS_ONSTATUS*WTAH_SIPS_86_ON,
         `84/88_gamma` = pmax(pmin(`84/88_I`, pmin(VT_CF_84_88 * WTAH_SIPS_AVAIL, METZ_CF_84_88 * METZSF1.NMTZ1M + SAPH_CF_84_88 * SAPHWF1.NSAP1S)-`84/88_epsilon`),0)*WTAH_SIPS_ONSTATUS*WTAH_SIPS_84_ON,
         `88/84_gamma` = pmax(pmin(`88/84_I`, pmin(VT_CF_88_84 * WTAH_SIPS_AVAIL, METZ_CF_88_84 * METZSF1.NMTZ1M + SAPH_CF_88_84 * SAPHWF1.NSAP1S)-`88/84_epsilon`),0)*WTAH_SIPS_ONSTATUS*WTAH_SIPS_88_ON,
         `86/8U_gamma` = pmax(pmin(`86/8U_I`, pmin(VT_CF_86_8U * WTAH_SIPS_AVAIL, METZ_CF_86_8U * METZSF1.NMTZ1M + SAPH_CF_86_8U * SAPHWF1.NSAP1S)-`86/8U_epsilon`),0)*WTAH_SIPS_ONSTATUS*WTAH_SIPS_86_ON,
         `8U/86_gamma` = pmax(pmin(`8U/86_I`, pmin(VT_CF_8U_86 * WTAH_SIPS_AVAIL, METZ_CF_8U_86 * METZSF1.NMTZ1M + SAPH_CF_8U_86 * SAPHWF1.NSAP1S)-`8U/86_epsilon`),0)*WTAH_SIPS_ONSTATUS*WTAH_SIPS_8U_ON) %>% 
  select(interval, `85/86_I`:`8U/86_gamma`) %>% 
  pivot_longer(cols = `85/86_I`:`8U/86_gamma`, names_to = "series", values_to = "value") %>% 
  group_by(series) %>% 
  summarise(min = min(value, na.rm = TRUE),
            max = max(value, na.rm = TRUE)) %>% 
  ungroup()

wsb_sips_avail %>% 
  filter(spd_id %in% c("WTAH_SIPS_AVAIL") &
           (interval >= ymd_hms("2025-07-31 04:00:00") & interval < ymd_hms("2026-02-01 04:00:00")) &
           value > 0) %>% 
  mutate(time_of_day = hour(interval) + ceiling(minute(interval)/30)/2) %>% 
  ggplot() +
  geom_boxplot(aes(x = as.factor(time_of_day), y = value)) + 
  theme_bw() +
  ylab("WSB SIPS Availability (MW)") +
  xlab("Time of Day") +
  scale_x_discrete(breaks = c(0, 4, 8, 12, 16, 20, 24))

# ---- Figure 2 ----
# Panel A
data <- wsb_sips_avail %>% 
  filter(((spd_id %in% c("WTAH_SIPS_ONSTATUS", 
                         "WTAH_SIPS_84_ON", "WTAH_SIPS_85_ON", "WTAH_SIPS_86_ON", "WTAH_SIPS_88_ON", "WTAH_SIPS_8U_ON", 
                         "METZSF_SIPS_STATUS", "SAPHWF1_SIPS_STATUS",
                         "WTAH_SIPS_AVAIL")) | (spd_id %in% c("METZSF1.NMTZ1M", "SAPHWF1.NSAP1S") & ems_id == "MW")) &
           (interval >= ymd_hms("2025-07-31 04:00:00") & interval < ymd_hms("2026-02-01 04:00:00")))%>%
  select(-ems_id) %>% 
  pivot_wider(names_from = spd_id, values_from = value) %>% 
  left_join(wsb_line_ratings, by = c("interval"="SETTLEMENTDATE")) %>% 
  #filter(WTAH_SIPS_AVAIL > 0 & (METZSF1.NMTZ1M + SAPHWF1.NSAP1S) > 0) %>% 
  # append contribution factors and construct expansion (from genericconstraintrhs_WSB.csv)
  mutate(METZ_CF_85_86 = 0.947,
         SAPH_CF_85_86 = 0.987,
         VT_CF_85_86 = 0.815,
         METZ_CF_86_85 = 0.959,
         SAPH_CF_86_85 = 1,
         VT_CF_86_85 = 0.819,
         METZ_CF_84_88 = 0.954,
         SAPH_CF_84_88 = 0.975,
         VT_CF_84_88 = 0.738,
         METZ_CF_88_84 = 0.954,
         SAPH_CF_88_84 = 0.975,
         VT_CF_88_84 = 0.738,
         METZ_CF_86_8U = 0.96,
         SAPH_CF_86_8U = 1,
         VT_CF_86_8U = 0.808,
         METZ_CF_8U_86 = 0.96,
         SAPH_CF_8U_86 = 1,
         VT_CF_8U_86 = 0.821,
         # expansion
         `85/86` = pmax(pmin(NRAT5_TWUL85 - NRAT15_TWUL85, NRATCN_TWUL85 - NRAT15_TWUL85 + VT_CF_85_86 * WTAH_SIPS_AVAIL, 
                             NRATCN_TWUL85 - NRAT15_TWUL85 + METZ_CF_85_86 * METZSF1.NMTZ1M + SAPH_CF_85_86 * SAPHWF1.NSAP1S), 0),
         `86/85` = pmax(pmin(NRAT5_ARTW86 - NRAT15_ARTW86, NRATCN_ARTW86 - NRAT15_ARTW86 + VT_CF_86_85 * WTAH_SIPS_AVAIL, 
                             NRATCN_ARTW86 - NRAT15_ARTW86 + METZ_CF_86_85 * METZSF1.NMTZ1M + SAPH_CF_86_85 * SAPHWF1.NSAP1S), 0),
         `84/88` = pmax(pmin(NRAT5_TMLD84 - NRAT15_TMLD84, NRATCN_TMLD84 - NRAT15_TMLD84 + VT_CF_84_88 * WTAH_SIPS_AVAIL, 
                             NRATCN_TMLD84 - NRAT15_TMLD84 + METZ_CF_84_88 * METZSF1.NMTZ1M + SAPH_CF_84_88 * SAPHWF1.NSAP1S), 0),
         `88/84` = pmax(pmin(NRAT5_TMMU88 - NRAT15_TMMU88, NRATCN_TMMU88 - NRAT15_TMMU88 + VT_CF_88_84 * WTAH_SIPS_AVAIL, 
                             NRATCN_TMMU88 - NRAT15_TMMU88 + METZ_CF_88_84 * METZSF1.NMTZ1M + SAPH_CF_88_84 * SAPHWF1.NSAP1S), 0),
         `86/8U` = pmax(pmin(NRAT5_ARTW86 - NRAT15_ARTW86, NRATCN_ARTW86 - NRAT15_ARTW86 + VT_CF_86_8U * WTAH_SIPS_AVAIL, 
                             NRATCN_ARTW86 - NRAT15_ARTW86 + METZ_CF_86_8U * METZSF1.NMTZ1M + SAPH_CF_86_8U * SAPHWF1.NSAP1S), 0),
         `8U/86` = pmax(pmin(NRAT5_TWUL85 - NRAT15_TWUL85, NRATCN_TWUL85 - NRAT15_TWUL85 + VT_CF_8U_86 * WTAH_SIPS_AVAIL, 
                             NRATCN_TWUL85 - NRAT15_TWUL85 + METZ_CF_8U_86 * METZSF1.NMTZ1M + SAPH_CF_8U_86 * SAPHWF1.NSAP1S), 0))
data %>% 
  select(interval, `85/86`:`8U/86`) %>% 
  pivot_longer(cols = `85/86`:`8U/86`, names_to = "constraint", values_to = "mw") %>% 
  ggplot() +
  geom_boxplot(aes(x = mw, y = constraint)) +
  theme_bw() +
  xlab("Expansion (MW)") +
  ylab("Monitored/N-1")

# Panel B
data <- wsb_sips_avail %>% 
  filter(((spd_id %in% c("WTAH_SIPS_ONSTATUS", 
                         "WTAH_SIPS_84_ON", "WTAH_SIPS_85_ON", "WTAH_SIPS_86_ON", "WTAH_SIPS_88_ON", "WTAH_SIPS_8U_ON", 
                         "METZSF_SIPS_STATUS", "SAPHWF1_SIPS_STATUS",
                         "WTAH_SIPS_AVAIL")) | (spd_id %in% c("METZSF1.NMTZ1M", "SAPHWF1.NSAP1S") & ems_id == "MW")) &
           (interval >= ymd_hms("2025-07-31 04:00:00") & interval < ymd_hms("2026-02-01 04:00:00")))%>%
  select(-ems_id) %>% 
  pivot_wider(names_from = spd_id, values_from = value) %>% 
  left_join(wsb_line_ratings, by = c("interval"="SETTLEMENTDATE")) %>% 
  #filter(WTAH_SIPS_AVAIL > 0 & (METZSF1.NMTZ1M + SAPHWF1.NSAP1S) > 0) %>% 
  # append contribution factors and construct expansion
  mutate(METZ_CF_85_86 = 0.947,
         SAPH_CF_85_86 = 0.987,
         VT_CF_85_86 = 0.815,
         METZ_CF_86_85 = 0.959,
         SAPH_CF_86_85 = 1,
         VT_CF_86_85 = 0.819,
         METZ_CF_84_88 = 0.954,
         SAPH_CF_84_88 = 0.975,
         VT_CF_84_88 = 0.738,
         METZ_CF_88_84 = 0.954,
         SAPH_CF_88_84 = 0.975,
         VT_CF_88_84 = 0.738,
         METZ_CF_86_8U = 0.96,
         SAPH_CF_86_8U = 1,
         VT_CF_86_8U = 0.808,
         METZ_CF_8U_86 = 0.96,
         SAPH_CF_8U_86 = 1,
         VT_CF_8U_86 = 0.821,
         # expansion
         `85/86` = pmax(pmin(NRAT5_TWUL85 - NRAT15_TWUL85, VT_CF_85_86 * WTAH_SIPS_AVAIL, 
                             METZ_CF_85_86 * METZSF1.NMTZ1M + SAPH_CF_85_86 * SAPHWF1.NSAP1S), 0),
         `86/85` = pmax(pmin(NRAT5_ARTW86 - NRAT15_ARTW86, VT_CF_86_85 * WTAH_SIPS_AVAIL, 
                             METZ_CF_86_85 * METZSF1.NMTZ1M + SAPH_CF_86_85 * SAPHWF1.NSAP1S), 0),
         `84/88` = pmax(pmin(NRAT5_TMLD84 - NRAT15_TMLD84, VT_CF_84_88 * WTAH_SIPS_AVAIL, 
                             METZ_CF_84_88 * METZSF1.NMTZ1M + SAPH_CF_84_88 * SAPHWF1.NSAP1S), 0),
         `88/84` = pmax(pmin(NRAT5_TMMU88 - NRAT15_TMMU88, VT_CF_88_84 * WTAH_SIPS_AVAIL, 
                             METZ_CF_88_84 * METZSF1.NMTZ1M + SAPH_CF_88_84 * SAPHWF1.NSAP1S), 0),
         `86/8U` = pmax(pmin(NRAT5_ARTW86 - NRAT15_ARTW86, VT_CF_86_8U * WTAH_SIPS_AVAIL, 
                             METZ_CF_86_8U * METZSF1.NMTZ1M + SAPH_CF_86_8U * SAPHWF1.NSAP1S), 0),
         `8U/86` = pmax(pmin(NRAT5_TWUL85 - NRAT15_TWUL85, VT_CF_8U_86 * WTAH_SIPS_AVAIL, 
                             METZ_CF_8U_86 * METZSF1.NMTZ1M + SAPH_CF_8U_86 * SAPHWF1.NSAP1S), 0))

data %>% 
  select(interval, `85/86`:`8U/86`) %>% 
  pivot_longer(cols = `85/86`:`8U/86`, names_to = "constraint", values_to = "mw") %>% 
  ggplot() +
  geom_boxplot(aes(x = mw, y = constraint)) +
  theme_bw() +
  xlab("Expansion (MW)") +
  ylab("Monitored/N-1")


# ---- Lesson 2: Statistics ----
# Number of WSB VT hours
wsb_sips_avail %>% 
  filter(spd_id %in% "WTAH_SIPS_AVAIL" & !is.na(value) &
           (interval >= ymd_hms("2025-07-31 04:00:00") & interval < ymd_hms("2026-02-01 04:00:00"))) %>% 
  nrow(.)/12

# Hours with WSB VT armed
wsb_sips_avail %>% 
  filter(spd_id %in% c("WTAH_SIPS_ONSTATUS", "WTAH_SIPS_84_ON", "WTAH_SIPS_85_ON", "WTAH_SIPS_86_ON", "WTAH_SIPS_88_ON", "WTAH_SIPS_8U_ON") &
           (interval >= ymd_hms("2025-07-31 04:00:00") & interval < ymd_hms("2026-02-01 04:00:00"))) %>% 
  group_by(spd_id) %>% 
  summarise(hrs_armed = sum(value == 1, na.rm = TRUE)/12) %>% 
  ungroup()

# Maximum WSB expansion when expansion is armed
wsb_sips_avail %>% 
  filter(((spd_id %in% c("WTAH_SIPS_ONSTATUS", 
                         "WTAH_SIPS_84_ON", "WTAH_SIPS_85_ON", "WTAH_SIPS_86_ON", "WTAH_SIPS_88_ON", "WTAH_SIPS_8U_ON", 
                         "METZSF_SIPS_STATUS", "SAPHWF1_SIPS_STATUS",
                         "WTAH_SIPS_AVAIL")) | (spd_id %in% c("METZSF1.NMTZ1M", "SAPHWF1.NSAP1S") & ems_id == "MW")) &
           (interval >= ymd_hms("2025-07-31 04:00:00") & interval < ymd_hms("2026-02-01 04:00:00"))) %>%
  select(-ems_id) %>% 
  pivot_wider(names_from = spd_id, values_from = value) %>% 
  left_join(wsb_line_ratings, by = c("interval"="SETTLEMENTDATE")) %>% 
  #filter(WTAH_SIPS_AVAIL > 0 & (METZSF1.NMTZ1M + SAPHWF1.NSAP1S) > 0) %>% 
  # append contribution factors and construct expansion
  mutate(METZ_CF_85_86 = 0.947,
         SAPH_CF_85_86 = 0.987,
         VT_CF_85_86 = 0.815,
         METZ_CF_86_85 = 0.959,
         SAPH_CF_86_85 = 1,
         VT_CF_86_85 = 0.819,
         METZ_CF_84_88 = 0.954,
         SAPH_CF_84_88 = 0.975,
         VT_CF_84_88 = 0.738,
         METZ_CF_88_84 = 0.954,
         SAPH_CF_88_84 = 0.975,
         VT_CF_88_84 = 0.738,
         METZ_CF_86_8U = 0.96,
         SAPH_CF_86_8U = 1,
         VT_CF_86_8U = 0.808,
         METZ_CF_8U_86 = 0.96,
         SAPH_CF_8U_86 = 1,
         VT_CF_8U_86 = 0.821,
         # expansion
         `85/86` = pmax(pmin(NRAT5_TWUL85 - NRAT15_TWUL85, NRATCN_TWUL85 - NRAT15_TWUL85 + VT_CF_85_86 * WTAH_SIPS_AVAIL, 
                             NRATCN_TWUL85 - NRAT15_TWUL85 + METZ_CF_85_86 * METZSF1.NMTZ1M + SAPH_CF_85_86 * SAPHWF1.NSAP1S), 0),
         `86/85` = pmax(pmin(NRAT5_ARTW86 - NRAT15_ARTW86, NRATCN_ARTW86 - NRAT15_ARTW86 + VT_CF_86_85 * WTAH_SIPS_AVAIL, 
                             NRATCN_ARTW86 - NRAT15_ARTW86 + METZ_CF_86_85 * METZSF1.NMTZ1M + SAPH_CF_86_85 * SAPHWF1.NSAP1S), 0),
         `84/88` = pmax(pmin(NRAT5_TMLD84 - NRAT15_TMLD84, NRATCN_TMLD84 - NRAT15_TMLD84 + VT_CF_84_88 * WTAH_SIPS_AVAIL, 
                             NRATCN_TMLD84 - NRAT15_TMLD84 + METZ_CF_84_88 * METZSF1.NMTZ1M + SAPH_CF_84_88 * SAPHWF1.NSAP1S), 0),
         `88/84` = pmax(pmin(NRAT5_TMMU88 - NRAT15_TMMU88, NRATCN_TMMU88 - NRAT15_TMMU88 + VT_CF_88_84 * WTAH_SIPS_AVAIL, 
                             NRATCN_TMMU88 - NRAT15_TMMU88 + METZ_CF_88_84 * METZSF1.NMTZ1M + SAPH_CF_88_84 * SAPHWF1.NSAP1S), 0),
         `86/8U` = pmax(pmin(NRAT5_ARTW86 - NRAT15_ARTW86, NRATCN_ARTW86 - NRAT15_ARTW86 + VT_CF_86_8U * WTAH_SIPS_AVAIL, 
                             NRATCN_ARTW86 - NRAT15_ARTW86 + METZ_CF_86_8U * METZSF1.NMTZ1M + SAPH_CF_86_8U * SAPHWF1.NSAP1S), 0),
         `8U/86` = pmax(pmin(NRAT5_TWUL85 - NRAT15_TWUL85, NRATCN_TWUL85 - NRAT15_TWUL85 + VT_CF_8U_86 * WTAH_SIPS_AVAIL, 
                             NRATCN_TWUL85 - NRAT15_TWUL85 + METZ_CF_8U_86 * METZSF1.NMTZ1M + SAPH_CF_8U_86 * SAPHWF1.NSAP1S), 0)) %>% 
  select(interval, `85/86`:`8U/86`) %>% 
  pivot_longer(cols = `85/86`:`8U/86`, names_to = "constraint", values_to = "mw") %>% 
  inner_join(
    filter(wsb_sips_avail, spd_id == "WTAH_SIPS_ONSTATUS" & value == 1),
    by = "interval") %>% 
  group_by(constraint) %>% 
  summarise(mw = max(mw, na.rm = TRUE)) %>% 
  ungroup()

# Time with positive expansion
wsb_sips_avail %>% 
  filter(((spd_id %in% c("WTAH_SIPS_ONSTATUS", 
                         "WTAH_SIPS_84_ON", "WTAH_SIPS_85_ON", "WTAH_SIPS_86_ON", "WTAH_SIPS_88_ON", "WTAH_SIPS_8U_ON", 
                         "METZSF_SIPS_STATUS", "SAPHWF1_SIPS_STATUS",
                         "WTAH_SIPS_AVAIL")) | (spd_id %in% c("METZSF1.NMTZ1M", "SAPHWF1.NSAP1S") & ems_id == "MW")) &
           (interval >= ymd_hms("2025-07-31 04:00:00") & interval < ymd_hms("2026-02-01 04:00:00")))%>%
  select(-ems_id) %>% 
  pivot_wider(names_from = spd_id, values_from = value) %>% 
  left_join(wsb_line_ratings, by = c("interval"="SETTLEMENTDATE")) %>% 
  # append contribution factors and construct expansion (from genericconstraintrhs_WSB.csv)
  mutate(METZ_CF_85_86 = 0.947,
         SAPH_CF_85_86 = 0.987,
         VT_CF_85_86 = 0.815,
         METZ_CF_86_85 = 0.959,
         SAPH_CF_86_85 = 1,
         VT_CF_86_85 = 0.819,
         METZ_CF_84_88 = 0.954,
         SAPH_CF_84_88 = 0.975,
         VT_CF_84_88 = 0.738,
         METZ_CF_88_84 = 0.954,
         SAPH_CF_88_84 = 0.975,
         VT_CF_88_84 = 0.738,
         METZ_CF_86_8U = 0.96,
         SAPH_CF_86_8U = 1,
         VT_CF_86_8U = 0.808,
         METZ_CF_8U_86 = 0.96,
         SAPH_CF_8U_86 = 1,
         VT_CF_8U_86 = 0.821) %>% 
  # remove non-SIP periods
  filter(!is.na(WTAH_SIPS_AVAIL)) %>% 
  mutate(`85/86_I` = NRAT5_TWUL85 - NRAT15_TWUL85,
         `86/85_I` = NRAT5_ARTW86 - NRAT15_ARTW86,
         `84/88_I` = NRAT5_TMLD84 - NRAT15_TMLD84,
         `88/84_I` = NRAT5_TMMU88 - NRAT15_TMMU88,
         `86/8U_I` = NRAT5_ARTW86 - NRAT15_ARTW86,
         `8U/86_I` = NRAT5_TWUL85 - NRAT15_TWUL85,
         III = METZSF1.NMTZ1M + SAPHWF1.NSAP1S,
         `85/86_epsilon` = NRAT15_TWUL85 - NRATCN_TWUL85,
         `86/85_epsilon` = NRAT15_ARTW86 - NRATCN_ARTW86,
         `84/88_epsilon` = NRAT15_TMLD84 - NRATCN_TMLD84,
         `88/84_epsilon` = NRAT15_TMMU88 - NRATCN_TMMU88,
         `86/8U_epsilon` = NRAT15_ARTW86 - NRATCN_ARTW86,
         `8U/86_epsilon` = NRAT15_TWUL85 - NRATCN_TWUL85,
         `85/86_gamma` = pmax(pmin(`85/86_I`, pmin(VT_CF_85_86 * WTAH_SIPS_AVAIL, METZ_CF_85_86 * METZSF1.NMTZ1M + SAPH_CF_85_86 * SAPHWF1.NSAP1S)-`85/86_epsilon`),0)*WTAH_SIPS_ONSTATUS*WTAH_SIPS_85_ON,
         `86/85_gamma` = pmax(pmin(`86/85_I`, pmin(VT_CF_86_85 * WTAH_SIPS_AVAIL, METZ_CF_86_85 * METZSF1.NMTZ1M + SAPH_CF_86_85 * SAPHWF1.NSAP1S)-`86/85_epsilon`),0)*WTAH_SIPS_ONSTATUS*WTAH_SIPS_86_ON,
         `84/88_gamma` = pmax(pmin(`84/88_I`, pmin(VT_CF_84_88 * WTAH_SIPS_AVAIL, METZ_CF_84_88 * METZSF1.NMTZ1M + SAPH_CF_84_88 * SAPHWF1.NSAP1S)-`84/88_epsilon`),0)*WTAH_SIPS_ONSTATUS*WTAH_SIPS_84_ON,
         `88/84_gamma` = pmax(pmin(`88/84_I`, pmin(VT_CF_88_84 * WTAH_SIPS_AVAIL, METZ_CF_88_84 * METZSF1.NMTZ1M + SAPH_CF_88_84 * SAPHWF1.NSAP1S)-`88/84_epsilon`),0)*WTAH_SIPS_ONSTATUS*WTAH_SIPS_88_ON,
         `86/8U_gamma` = pmax(pmin(`86/8U_I`, pmin(VT_CF_86_8U * WTAH_SIPS_AVAIL, METZ_CF_86_8U * METZSF1.NMTZ1M + SAPH_CF_86_8U * SAPHWF1.NSAP1S)-`86/8U_epsilon`),0)*WTAH_SIPS_ONSTATUS*WTAH_SIPS_86_ON,
         `8U/86_gamma` = pmax(pmin(`8U/86_I`, pmin(VT_CF_8U_86 * WTAH_SIPS_AVAIL, METZ_CF_8U_86 * METZSF1.NMTZ1M + SAPH_CF_8U_86 * SAPHWF1.NSAP1S)-`8U/86_epsilon`),0)*WTAH_SIPS_ONSTATUS*WTAH_SIPS_8U_ON) %>% 
  select(interval, `85/86_gamma`:`8U/86_gamma`) %>% 
  pivot_longer(cols = `85/86_gamma`:`8U/86_gamma`, names_to = "series", values_to = "value") %>% 
  group_by(interval) %>% 
  summarise(value = max(value, na.rm = TRUE)) %>% 
  ungroup() %>% 
  summarise(hrs = sum(value > 0, na.rm = TRUE)/12) %>% 
  ungroup()

# ---- Figure 3 ----
# Panel A
data <- ic %>% 
  filter((SETTLEMENTDATE >= ymd_hms("2021-11-29 04:00:00") & SETTLEMENTDATE < ymd_hms("2022-04-01 04:00:00")) |
           (SETTLEMENTDATE >= ymd_hms("2022-11-01 04:00:00") & SETTLEMENTDATE < ymd_hms("2023-04-01 04:00:00")) |
           (SETTLEMENTDATE >= ymd_hms("2023-11-01 04:00:00") & SETTLEMENTDATE < ymd_hms("2024-04-01 04:00:00")) |
           (SETTLEMENTDATE >= ymd_hms("2024-11-01 04:00:00") & SETTLEMENTDATE < ymd_hms("2025-04-01 04:00:00")) |
           (SETTLEMENTDATE >= ymd_hms("2025-11-01 04:00:00") & SETTLEMENTDATE < ymd_hms("2026-02-01 04:00:00"))) %>% 
  mutate(group = case_when(IMPORTGENCONID %in% c(vbb_sips_constraints_off, vbb_sips_constraints_on) ~ "VT Thermal Constraints",
                           substr(IMPORTGENCONID, 2, 2) == "^" ~ "Voltage Constraints",
                           substr(IMPORTGENCONID, 2, 2) == ":" ~ "Stability Constraints",
                           substr(IMPORTGENCONID, 2, 2) == ">" ~ "Other Thermal Constraints",
                           substr(IMPORTGENCONID, 1, 3) == "NRM" ~ "Negative Residue Management",
                           TRUE ~ "Other"))

data %>% 
  group_by(group) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  mutate(n2 = n/sum(n)*100) %>% 
  ggplot() +
  geom_bar(aes(y = reorder(group, n2), x = n2), stat="identity") +
  theme_bw() +
  ylab(NULL) +
  xlim(0, 100) +
  xlab(NULL)

# Panel B
data <- ic %>% 
  filter((SETTLEMENTDATE >= ymd_hms("2021-11-29 04:00:00") & SETTLEMENTDATE < ymd_hms("2022-04-01 04:00:00")) |
           (SETTLEMENTDATE >= ymd_hms("2022-11-01 04:00:00") & SETTLEMENTDATE < ymd_hms("2023-04-01 04:00:00")) |
           (SETTLEMENTDATE >= ymd_hms("2023-11-01 04:00:00") & SETTLEMENTDATE < ymd_hms("2024-04-01 04:00:00")) |
           (SETTLEMENTDATE >= ymd_hms("2024-11-01 04:00:00") & SETTLEMENTDATE < ymd_hms("2025-04-01 04:00:00")) |
           (SETTLEMENTDATE >= ymd_hms("2025-11-01 04:00:00") & SETTLEMENTDATE < ymd_hms("2026-02-01 04:00:00"))) %>% 
  filter(LOCAL_PRICE_ADJUSTMENT_IMPORT != 0) %>% 
  mutate(group = case_when(IMPORTGENCONID %in% c(vbb_sips_constraints_off, vbb_sips_constraints_on) ~ "VT Thermal Constraints",
                           substr(IMPORTGENCONID, 2, 2) == "^" ~ "Voltage Constraints",
                           substr(IMPORTGENCONID, 2, 2) == ":" ~ "Stability Constraints",
                           substr(IMPORTGENCONID, 2, 2) == ">" ~ "Other Thermal Constraints",
                           substr(IMPORTGENCONID, 1, 3) == "NRM" ~ "Negative Residue Management",
                           TRUE ~ "Other"))

data %>% 
  group_by(group) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  mutate(n2 = n/sum(n)*100) %>% 
  ggplot() +
  geom_bar(aes(y = reorder(group, n2), x = n2), stat="identity") +
  theme_bw() +
  ylab(NULL) +
  xlim(0, 100) +
  xlab("Percentage of Intervals (%)")

# ---- Lesson 3: Statistics ----
# VBB VT constraint binding hours
mapping1 = setNames(vbb_constraints_mapping$`SIPS EQUIVALENT`, vbb_constraints_mapping$CONSTRAINTID)
mapping2 = setNames(vbb_corridor_mapping$equiv_group, vbb_corridor_mapping$GENCONID)
dispatchconstraint %>% 
  filter(CONSTRAINTID %in% c(vbb_sips_constraints_off, vbb_sips_constraints_on) &
    ((SETTLEMENTDATE >= ymd_hms("2021-11-29 04:00:00") & SETTLEMENTDATE < ymd_hms("2022-04-01 04:00:00")) |
       (SETTLEMENTDATE >= ymd_hms("2022-11-01 04:00:00") & SETTLEMENTDATE < ymd_hms("2023-04-01 04:00:00")) |
       (SETTLEMENTDATE >= ymd_hms("2023-11-01 04:00:00") & SETTLEMENTDATE < ymd_hms("2024-04-01 04:00:00")) |
       (SETTLEMENTDATE >= ymd_hms("2024-11-01 04:00:00") & SETTLEMENTDATE < ymd_hms("2025-04-01 04:00:00")) |
       (SETTLEMENTDATE >= ymd_hms("2025-11-01 04:00:00") & SETTLEMENTDATE < ymd_hms("2026-02-01 04:00:00")))) %>% 
  mutate(group = mapping1[CONSTRAINTID],
         group = coalesce(group, CONSTRAINTID),
         group2 = mapping2[group],
         group3 = case_when(group2 == "G1a" ~ "DDTS-SMTS/DDTS-SMTS",
                            group2 == "G1b" ~ "DDTS-SMTS/DDTS-SMTS (existing EPS-TTS outage)",
                            group2 == "G2" ~ "EPS-TTS/DDTS-SMTS",
                            group2 %in% c("G3a", "G4", "G5a", "G6", "G7a") ~ "MSS-DDTS/MSS-DDTS",
                            group2 == "G3b" ~ "MSS-DDTS/MSS-DDTS (existing JINDERA-WAGGA330 outage)",
                            group2 %in% c("G5b", "G7b") ~ "MSS-DDTS/MSS-DDTS (existing TUMUT3-WAGGA330 outage)",
                            group2 == "G5c" ~ "MSDD/parallel line (existing JINDERA-WOTS outage)")) %>% 
  filter(MARGINALVALUE != 0) %>% 
  .$SETTLEMENTDATE %>% 
  unique() %>% 
  length()/12

# Percentage of time VNI was import limited
ic %>% 
  filter((SETTLEMENTDATE >= ymd_hms("2021-11-29 04:00:00") & SETTLEMENTDATE < ymd_hms("2022-04-01 04:00:00")) |
           (SETTLEMENTDATE >= ymd_hms("2022-11-01 04:00:00") & SETTLEMENTDATE < ymd_hms("2023-04-01 04:00:00")) |
           (SETTLEMENTDATE >= ymd_hms("2023-11-01 04:00:00") & SETTLEMENTDATE < ymd_hms("2024-04-01 04:00:00")) |
           (SETTLEMENTDATE >= ymd_hms("2024-11-01 04:00:00") & SETTLEMENTDATE < ymd_hms("2025-04-01 04:00:00")) |
           (SETTLEMENTDATE >= ymd_hms("2025-11-01 04:00:00") & SETTLEMENTDATE < ymd_hms("2026-02-01 04:00:00"))) %>% 
  # LOCALLY_CONSTRAINED_IMPORT == 1: import constrained under N-0 system state, LOCALLY_CONSTRAINED_IMPORT == 2: import constrained under N-1 system state
  summarise(percentage = sum(LOCALLY_CONSTRAINED_IMPORT > 0)/n()*100) %>% 
  ungroup()

# VNI voltage constraint binding frequency
dispatchconstraint %>% 
           filter(CONSTRAINTID %in% c(lcr, arwbba) & 
                    ((SETTLEMENTDATE >= ymd_hms("2021-11-29 04:00:00") & SETTLEMENTDATE < ymd_hms("2022-04-01 04:00:00")) |
                    (SETTLEMENTDATE >= ymd_hms("2022-11-01 04:00:00") & SETTLEMENTDATE < ymd_hms("2023-04-01 04:00:00")) |
                    (SETTLEMENTDATE >= ymd_hms("2023-11-01 04:00:00") & SETTLEMENTDATE < ymd_hms("2024-04-01 04:00:00")) |
                    (SETTLEMENTDATE >= ymd_hms("2024-11-01 04:00:00") & SETTLEMENTDATE < ymd_hms("2025-04-01 04:00:00")) |
                    (SETTLEMENTDATE >= ymd_hms("2025-11-01 04:00:00") & SETTLEMENTDATE < ymd_hms("2026-02-01 04:00:00")))) %>% 
  filter(MARGINALVALUE != 0) %>% 
  nrow(.)/12
  
# ---- Extended Data: Figure 5 ----
network_scrape %>% 
  filter(substr(spd_id, 1, 6) == "VRAT15") %>% 
  mutate(spd_id = substr(spd_id, nchar(spd_id)-4, 100)) %>% 
  pivot_wider(names_from = ems_id, values_from = value) %>% 
  filter(!is.na(`15MT`) & !is.na(LDSH)) %>% 
  mutate(spd_id = case_when(spd_id == "_EPTT" ~ "EPTT",
                            TRUE ~ spd_id),
         diff = LDSH - `15MT`) %>% 
  ggplot() +
  geom_boxplot(aes(x = diff, y = spd_id)) +
  xlab("MW") +
  ylab("Line") +
  xlim(-10, 10) +
  theme_minimal()

# ---- Extended Data: Figure 9 ----
data <- dispatchconstraint %>% 
  filter(CONSTRAINTID %in% c(wtah_sips_constraints) &
           (SETTLEMENTDATE >= ymd_hms("2025-07-31 04:00:00") & SETTLEMENTDATE < ymd_hms("2026-02-01 04:00:00"))) %>% 
  mutate(group = case_when(CONSTRAINTID %in% c("DSNAP_N>>NIL_85_86_S", "N>>NIL_85_86_S") ~ "85/86",
                           CONSTRAINTID %in% c("DSNAP_N>>NIL_86_85_S", "N>>NIL_86_85_S") ~ "86/85",
                           CONSTRAINTID == "N>>NIL_84_88_S" ~ "84/88",
                           CONSTRAINTID == "N>>NIL_88_84_S" ~ "88/84",
                           CONSTRAINTID == "N>>NIL_8U_86_S" ~ "8U/86",
                           CONSTRAINTID == "N>>NIL_86_8U_S" ~ "86/8U"))

data %>% 
  group_by(group) %>% 
  summarise(n = sum(MARGINALVALUE != 0)/12) %>% 
  ungroup() %>% 
  ggplot() +
  geom_bar(aes(y = group, x = n), stat="identity") +
  theme_bw() +
  ylab("Monitored/N-1") +
  xlab("Binding Hours")

# ---- Simulation Re-Dispatch
# v.1
export_intervals_SIPS1 <- dispatchconstraint %>% 
  filter(CONSTRAINTID %in% vbb_sips_constraints_on & MARGINALVALUE != 0 &
           (SETTLEMENTDATE >= ymd_hms("2021-11-29 04:00:00") & SETTLEMENTDATE < ymd_hms("2022-04-01 04:00:00"))) %>% 
  mutate(start = format(SETTLEMENTDATE, "%Y/%m/%d %H:%M:%S")) %>% 
  select(start)

export_intervals_SIPS2 <- dispatchconstraint %>% 
  filter(CONSTRAINTID %in% vbb_sips_constraints_on & MARGINALVALUE != 0 &
           (SETTLEMENTDATE >= ymd_hms("2022-11-01 04:00:00") & SETTLEMENTDATE < ymd_hms("2023-04-01 04:00:00"))) %>% 
  mutate(start = format(SETTLEMENTDATE, "%Y/%m/%d %H:%M:%S")) %>% 
  select(start)

export_intervals_SIPS3 <- dispatchconstraint %>% 
  filter(CONSTRAINTID %in% vbb_sips_constraints_on & MARGINALVALUE != 0 &
           (SETTLEMENTDATE >= ymd_hms("2023-11-01 04:00:00") & SETTLEMENTDATE < ymd_hms("2024-04-01 04:00:00"))) %>% 
  mutate(start = format(SETTLEMENTDATE, "%Y/%m/%d %H:%M:%S")) %>% 
  select(start)

export_intervals_SIPS4 <- dispatchconstraint %>% 
  filter(CONSTRAINTID %in% vbb_sips_constraints_on & MARGINALVALUE != 0 &
           (SETTLEMENTDATE >= ymd_hms("2024-11-01 04:00:00") & SETTLEMENTDATE < ymd_hms("2025-04-01 04:00:00"))) %>% 
  mutate(start = format(SETTLEMENTDATE, "%Y/%m/%d %H:%M:%S")) %>% 
  select(start)

export_intervals_SIPS5 <- dispatchconstraint %>% 
  filter(CONSTRAINTID %in% vbb_sips_constraints_on & MARGINALVALUE != 0 &
           (SETTLEMENTDATE >= ymd_hms("2025-11-01 04:00:00") & SETTLEMENTDATE < ymd_hms("2026-02-01 04:00:00"))) %>% 
  mutate(start = format(SETTLEMENTDATE, "%Y/%m/%d %H:%M:%S")) %>% 
  select(start)

vni_contract_superset <- export_intervals_SIPS1 %>% 
  rbind(export_intervals_SIPS2) %>% 
  rbind(export_intervals_SIPS3) %>% 
  rbind(export_intervals_SIPS4) %>% 
  rbind(export_intervals_SIPS5)

write_csv(export_intervals_SIPS1, "./secondary_data/vni_contract_binding_SIPS1.csv")
write_csv(export_intervals_SIPS2, "./secondary_data/vni_contract_binding_SIPS2.csv")
write_csv(export_intervals_SIPS3, "./secondary_data/vni_contract_binding_SIPS3.csv")
write_csv(export_intervals_SIPS4, "./secondary_data/vni_contract_binding_SIPS4.csv")
write_csv(export_intervals_SIPS5, "./secondary_data/vni_contract_binding_SIPS5.csv")

# v.2
export_intervals_SIPS1 <- dispatchconstraint %>% 
  filter(CONSTRAINTID %in% arbe_elml_constraints & MARGINALVALUE != 0 &
           (SETTLEMENTDATE >= ymd_hms("2021-11-29 04:00:00") & SETTLEMENTDATE < ymd_hms("2022-04-01 04:00:00"))) %>% 
  mutate(start = format(SETTLEMENTDATE, "%Y/%m/%d %H:%M:%S")) %>% 
  select(start)

export_intervals_SIPS2 <- dispatchconstraint %>% 
  filter(CONSTRAINTID %in% arbe_elml_constraints & MARGINALVALUE != 0 &
           (SETTLEMENTDATE >= ymd_hms("2022-11-01 04:00:00") & SETTLEMENTDATE < ymd_hms("2023-04-01 04:00:00"))) %>% 
  mutate(start = format(SETTLEMENTDATE, "%Y/%m/%d %H:%M:%S")) %>% 
  select(start)

export_intervals_SIPS3 <- dispatchconstraint %>% 
  filter(CONSTRAINTID %in% arbe_elml_constraints & MARGINALVALUE != 0 &
           (SETTLEMENTDATE >= ymd_hms("2023-11-01 04:00:00") & SETTLEMENTDATE < ymd_hms("2024-04-01 04:00:00"))) %>% 
  mutate(start = format(SETTLEMENTDATE, "%Y/%m/%d %H:%M:%S")) %>% 
  select(start)

export_intervals_SIPS4 <- dispatchconstraint %>% 
  filter(CONSTRAINTID %in% arbe_elml_constraints & MARGINALVALUE != 0 &
           (SETTLEMENTDATE >= ymd_hms("2024-11-01 04:00:00") & SETTLEMENTDATE < ymd_hms("2025-04-01 04:00:00"))) %>% 
  mutate(start = format(SETTLEMENTDATE, "%Y/%m/%d %H:%M:%S")) %>% 
  select(start)

export_intervals_SIPS5 <- dispatchconstraint %>% 
  filter(CONSTRAINTID %in% arbe_elml_constraints & MARGINALVALUE != 0 &
           (SETTLEMENTDATE >= ymd_hms("2025-11-01 04:00:00") & SETTLEMENTDATE < ymd_hms("2026-02-01 04:00:00"))) %>% 
  mutate(start = format(SETTLEMENTDATE, "%Y/%m/%d %H:%M:%S")) %>% 
  select(start)

arbe_elml_superset <- export_intervals_SIPS1 %>% 
  rbind(export_intervals_SIPS2) %>% 
  rbind(export_intervals_SIPS3) %>% 
  rbind(export_intervals_SIPS4) %>% 
  rbind(export_intervals_SIPS5)

write_csv(export_intervals_SIPS1, "./secondary_data/arbe_elml_binding_SIPS1.csv")
write_csv(export_intervals_SIPS2, "./secondary_data/arbe_elml_binding_SIPS2.csv")
write_csv(export_intervals_SIPS3, "./secondary_data/arbe_elml_binding_SIPS3.csv")
write_csv(export_intervals_SIPS4, "./secondary_data/arbe_elml_binding_SIPS4.csv")
write_csv(export_intervals_SIPS5, "./secondary_data/arbe_elml_binding_SIPS5.csv")

# v.3
export_intervals_SIPS1 <- dispatchconstraint %>% 
  filter(CONSTRAINTID %in% c(lcr, arwbba) & MARGINALVALUE != 0 &
           (SETTLEMENTDATE >= ymd_hms("2021-11-29 04:00:00") & SETTLEMENTDATE < ymd_hms("2022-04-01 04:00:00"))) %>% 
  mutate(start = format(SETTLEMENTDATE, "%Y/%m/%d %H:%M:%S")) %>% 
  select(start)

export_intervals_SIPS2 <- dispatchconstraint %>% 
  filter(CONSTRAINTID %in% c(lcr, arwbba) & MARGINALVALUE != 0 &
           (SETTLEMENTDATE >= ymd_hms("2022-11-01 04:00:00") & SETTLEMENTDATE < ymd_hms("2023-04-01 04:00:00"))) %>% 
  mutate(start = format(SETTLEMENTDATE, "%Y/%m/%d %H:%M:%S")) %>% 
  select(start)

export_intervals_SIPS3 <- dispatchconstraint %>% 
  filter(CONSTRAINTID %in% c(lcr, arwbba) & MARGINALVALUE != 0 &
           (SETTLEMENTDATE >= ymd_hms("2023-11-01 04:00:00") & SETTLEMENTDATE < ymd_hms("2024-04-01 04:00:00"))) %>% 
  mutate(start = format(SETTLEMENTDATE, "%Y/%m/%d %H:%M:%S")) %>% 
  select(start)

export_intervals_SIPS4 <- dispatchconstraint %>% 
  filter(CONSTRAINTID %in% c(lcr, arwbba) & MARGINALVALUE != 0 &
           (SETTLEMENTDATE >= ymd_hms("2024-11-01 04:00:00") & SETTLEMENTDATE < ymd_hms("2025-04-01 04:00:00"))) %>% 
  mutate(start = format(SETTLEMENTDATE, "%Y/%m/%d %H:%M:%S")) %>% 
  select(start)

export_intervals_SIPS5 <- dispatchconstraint %>% 
  filter(CONSTRAINTID %in% c(lcr, arwbba) & MARGINALVALUE != 0 &
           (SETTLEMENTDATE >= ymd_hms("2025-11-01 04:00:00") & SETTLEMENTDATE < ymd_hms("2026-02-01 04:00:00"))) %>% 
  mutate(start = format(SETTLEMENTDATE, "%Y/%m/%d %H:%M:%S")) %>% 
  select(start)

vni_voltage_superset <- export_intervals_SIPS1 %>% 
  rbind(export_intervals_SIPS2) %>% 
  rbind(export_intervals_SIPS3) %>% 
  rbind(export_intervals_SIPS4) %>% 
  rbind(export_intervals_SIPS5)

write_csv(export_intervals_SIPS1, "./secondary_data/vni_voltage_binding_SIPS1.csv")
write_csv(export_intervals_SIPS2, "./secondary_data/vni_voltage_binding_SIPS2.csv")
write_csv(export_intervals_SIPS3, "./secondary_data/vni_voltage_binding_SIPS3.csv")
write_csv(export_intervals_SIPS4, "./secondary_data/vni_voltage_binding_SIPS4.csv")
write_csv(export_intervals_SIPS5, "./secondary_data/vni_voltage_binding_SIPS5.csv")

# w.1 and w.2
export_intervals_SIPS5 <- dispatchconstraint %>% 
  filter(CONSTRAINTID %in% wtah_sips_constraints & MARGINALVALUE != 0 &
           (SETTLEMENTDATE >= ymd_hms("2025-07-31 04:00:00") & SETTLEMENTDATE < ymd_hms("2026-02-01 04:00:00"))) %>% 
  mutate(start = format(SETTLEMENTDATE, "%Y/%m/%d %H:%M:%S")) %>% 
  select(start)

wsb_thermal_superset <- export_intervals_SIPS5

write_csv(export_intervals_SIPS5, "./secondary_data/wsb_thermal_binding_SIPS5.csv")

# Full historic re-dispatch set
historical_superset <- vni_contract_superset %>% 
  rbind(arbe_elml_superset) %>% 
  rbind(vni_voltage_superset) %>% 
  rbind(wsb_thermal_superset) %>% 
  distinct() %>% 
  arrange(start)

export_intervals_SIPS1 <- historical_superset %>% 
  filter(start >= ymd_hms("2021/11/28 00:00:00") & start < ymd_hms("2022/04/01 04:00:00"))

export_intervals_SIPS2 <- historical_superset %>% 
  filter(start >= ymd_hms("2022/11/01 00:04:00") & start < ymd_hms("2023/04/01 04:00:00"))

export_intervals_SIPS3 <- historical_superset %>% 
  filter(start >= ymd_hms("2023/11/01 00:04:00") & start < ymd_hms("2024/04/01 04:00:00"))

export_intervals_SIPS4 <- historical_superset %>% 
  filter(start >= ymd_hms("2024/11/01 00:04:00") & start < ymd_hms("2025/04/01 04:00:00"))

export_intervals_SIPS5 <- historical_superset %>% 
  filter(start >= ymd_hms("2025/07/31 00:04:00") & start < ymd_hms("2026/02/01 00:04:00"))

write_csv(export_intervals_SIPS1, "./secondary_data/historical_coi_SIPS1.csv")
write_csv(export_intervals_SIPS2, "./secondary_data/historical_coi_SIPS2.csv")
write_csv(export_intervals_SIPS3, "./secondary_data/historical_coi_SIPS3.csv")
write_csv(export_intervals_SIPS4, "./secondary_data/historical_coi_SIPS4.csv")
write_csv(export_intervals_SIPS5, "./secondary_data/historical_coi_SIPS5.csv")
