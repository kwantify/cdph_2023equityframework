##############################################################
## Equity Framework Paper
## author: Ada Kwan
## other contributors: Jason Vargo, Chris Hoover
## created on: 30 June 2023
## edited on: 18 Nov 2024
##############################################################
## 1. SETUP ###########################################################
rm(list = ls())
options(stringsAsFactors = F)
pkgs <-c("dplyr","data.table","sf","tidyr","lubridate","dplyr","magrittr","viridis","tsibble",
         "flextable","glue","infer","rddtools","readr","ggplot2","grid","broom","ggthemes","plm","memisc",
         "stringr","tidycensus","ggiraph","patchwork","viridis", "SCtools", "RCurl", "gdata", "zoo", "stargazer", "writexl", "bit64")
lapply(pkgs,require,character.only=TRUE)

setwd("~/R/Equity Framework/outputs")

############################### SETUP - PLOT THEMES

scale_color_hpiq <- function(){
  scale_color_manual(name = "HPI Quartile:",
                     values = c("1" = "royalblue", "2" = "lightblue", "3" = "lightgreen", "4" = "darkgreen", "5" = "tomato1"),
                     labels = c("HPIQ1", "HPIQ2", "HPIQ3", "HPIQ4", "No HPI Quartile"))
}
scale_fill_hpiq <- function(){
  scale_fill_manual(name = "HPI Quartile:",
                    values = c("1" = "royalblue", "2" = "lightblue", "3" = "lightgreen", "4" = "darkgreen", "5" = "tomato1"),
                    labels = c("HPIQ1", "HPIQ2", "HPIQ3", "HPIQ4", "No HPI Quartile"))
}

scale_color_veq <- function(){
  scale_color_manual(name = "Vaccine Equity Quartile:",
                     values = c("1" = "royalblue", "2" = "lightblue", "3" = "lightgreen", "4" = "darkgreen", "5" = "tomato1"),
                     labels = c("VEM Q1", "VEM Q2", "VEM Q3", "VEM Q4", "No VEM Quartile"))
}
scale_fill_veq <- function(){
  scale_fill_manual(name = "Vaccine Equity Quartile:",
                    values = c("1" = "royalblue", "2" = "lightblue", "3" = "lightgreen", "4" = "darkgreen", "5" = "tomato1"),
                    labels = c("VEM Q1", "VEM Q2", "VEM Q3", "VEM Q4", "No VEM Quartile"))
}

theme_hpi <- function(){
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14))
}


############################### SETUP - FORMULAS, FUNCTIONS
# IRR calcs (JV)
irr_calc_cases <- function(df){
  
  PAF <- df[, COVID_OUTCOME := "cases"] %>%
    .[,.(outcome = sum(cases), 
         persons = sum(pop)),
      by = .(state_quantile, COVID_OUTCOME)] %>% 
    .[, rate := outcome/persons] %>%
    .[, rate_var := outcome/persons^2] %>%
    .[, `:=` (rateLL95 = rate - 1.96*sqrt(outcome/persons^2),
              rateUL95 = rate + 1.96*sqrt(outcome/persons^2))] 
  
  
  PAF <- PAF[, outcome_fraction := outcome/sum(outcome)]                  
  PAF <- PAF[, pop_fraction := persons/sum(persons)]
  PAF <- PAF[, 
             IRR := rate/{
               PAF[state_quantile == "Q4"]$rate
             }
  ]
  PAF <- PAF[, 
             IRD := rate-{
               PAF[state_quantile == "Q4"]$rate
             }
  ]
  
  PAF <- PAF[,`:=` 
             (varIRD = rate_var + {
               PAF[state_quantile == "Q4"]$rate_var
             },
               varlogirr = (rate_var/rate^2) + ({
                 PAF[state_quantile == "Q4"]$rate_var
               }/{
                 PAF[state_quantile == "Q4"]$rate
               }^2))]
  
  PAF <- PAF[,`:=` (IRDLL95 = IRD - 1.96*sqrt(varIRD),
                    IRDUL95 = IRD + 1.96*sqrt(varIRD),
                    IRRLL95 = exp(log(IRR) - 1.96*sqrt(varlogirr)),
                    IRRUL95 = exp(log(IRR) + 1.96*sqrt(varlogirr)))]           
  
  PAF <- PAF[,.(quartile = state_quantile,
                COVID_OUTCOME, 
                outcome, 
                persons, 
                outcome_fraction, 
                pop_fraction,
                rate = rate*100000,
                rateLL = rateLL95 *100000,
                rateUL = rateUL95 *100000, 
                IRD = IRD *100000, 
                IRDLL = IRDLL95*100000,
                IRDUL = IRDUL95 *100000, 
                IRR,
                IRRLL = IRRLL95, 
                IRRUL = IRRUL95
  )]
  
  return(PAF)
  
}


irr_calc_tests <- function(df){
  
  PAF <- df[, COVID_OUTCOME := "tests"] %>%
    .[,.(outcome = sum(tests), 
         persons = sum(pop)),
      by = .(state_quantile, COVID_OUTCOME)] %>% 
    .[, rate := outcome/persons] %>%
    .[, rate_var := outcome/persons^2] %>%
    .[, `:=` (rateLL95 = rate - 1.96*sqrt(outcome/persons^2),
              rateUL95 = rate + 1.96*sqrt(outcome/persons^2))] 
  
  
  PAF <- PAF[, outcome_fraction := outcome/sum(outcome)]                  
  PAF <- PAF[, pop_fraction := persons/sum(persons)]
  PAF <- PAF[, 
             IRR := rate/{
               PAF[state_quantile == "Q4"]$rate
             }
  ]
  PAF <- PAF[, 
             IRD := rate-{
               PAF[state_quantile == "Q4"]$rate
             }
  ]
  
  PAF <- PAF[,`:=` 
             (varIRD = rate_var + {
               PAF[state_quantile == "Q4"]$rate_var
             },
               varlogirr = (rate_var/rate^2) + ({
                 PAF[state_quantile == "Q4"]$rate_var
               }/{
                 PAF[state_quantile == "Q4"]$rate
               }^2))]
  
  PAF <- PAF[,`:=` (IRDLL95 = IRD - 1.96*sqrt(varIRD),
                    IRDUL95 = IRD + 1.96*sqrt(varIRD),
                    IRRLL95 = exp(log(IRR) - 1.96*sqrt(varlogirr)),
                    IRRUL95 = exp(log(IRR) + 1.96*sqrt(varlogirr)))]           
  
  PAF <- PAF[,.(quartile = state_quantile,
                COVID_OUTCOME, 
                outcome, 
                persons, 
                outcome_fraction, 
                pop_fraction,
                rate = rate*100000,
                rateLL = rateLL95 *100000,
                rateUL = rateUL95 *100000, 
                IRD = IRD *100000, 
                IRDLL = IRDLL95*100000,
                IRDUL = IRDUL95 *100000, 
                IRR,
                IRRLL = IRRLL95, 
                IRRUL = IRRUL95
  )]
  
  return(PAF)
  
}


irr_calc_deaths <- function(df){
  
  PAF <- df[, COVID_OUTCOME := "deaths"] %>%
    .[,.(outcome = sum(deaths), 
         persons = sum(pop)),
      by = .(state_quantile, COVID_OUTCOME)] %>% 
    .[, rate := outcome/persons] %>%
    .[, rate_var := outcome/persons^2] %>%
    .[, `:=` (rateLL95 = rate - 1.96*sqrt(outcome/persons^2),
              rateUL95 = rate + 1.96*sqrt(outcome/persons^2))] 
  
  
  PAF <- PAF[, outcome_fraction := outcome/sum(outcome)]                  
  PAF <- PAF[, pop_fraction := persons/sum(persons)]
  PAF <- PAF[, 
             IRR := rate/{
               PAF[state_quantile == "Q4"]$rate
             }
  ]
  PAF <- PAF[, 
             IRD := rate-{
               PAF[state_quantile == "Q4"]$rate
             }
  ]
  
  PAF <- PAF[,`:=` 
             (varIRD = rate_var + {
               PAF[state_quantile == "Q4"]$rate_var
             },
               varlogirr = (rate_var/rate^2) + ({
                 PAF[state_quantile == "Q4"]$rate_var
               }/{
                 PAF[state_quantile == "Q4"]$rate
               }^2))]
  
  PAF <- PAF[,`:=` (IRDLL95 = IRD - 1.96*sqrt(varIRD),
                    IRDUL95 = IRD + 1.96*sqrt(varIRD),
                    IRRLL95 = exp(log(IRR) - 1.96*sqrt(varlogirr)),
                    IRRUL95 = exp(log(IRR) + 1.96*sqrt(varlogirr)))]           
  
  PAF <- PAF[,.(quartile = state_quantile,
                COVID_OUTCOME, 
                outcome, 
                persons, 
                outcome_fraction, 
                pop_fraction,
                rate = rate*100000,
                rateLL = rateLL95 *100000,
                rateUL = rateUL95 *100000, 
                IRD = IRD *100000, 
                IRDLL = IRDLL95*100000,
                IRDUL = IRDUL95 *100000, 
                IRR,
                IRRLL = IRRLL95, 
                IRRUL = IRRUL95
  )]
  
  return(PAF)
  
}

make_table <- function(index) {
  table <-
    bind_rows(
      irr_calc_cases(cases_merge_tract[hpi_component == index]),
      irr_calc_deaths(cases_merge_tract[hpi_component == index]),
      irr_calc_tests(cases_merge_tract[hpi_component == index])
    ) %>%
    .[, index := index]
  return(table)
}



## 2. DATASETS ##############################################################
## NON-VACCINE DATASETS ##############################################################

##  HPI DATASETS 
hpi <- fread("//mnt/projects/connect-izb/resources/general/healthy_places_index_tract.csv") %>% 
  .[, geoid := paste0("0", geoid)] %>% 
  rename(hpi2score = hpi2_score)
hpi$geoid <- as.integer64.character(hpi$geoid)

hpi3 <- readxl::read_xlsx("//mnt/projects/connect-izb/resources/health_equity/hpi_3_complete_file.xlsx") %>% 
  mutate(geoid = paste0("0", GEO_ID),
         hpi_pctile = 100 * hpi_pctile)
hpi3$geoid <- as.integer64.character(hpi3$geoid)

hpi_main <- merge(y = hpi, x = hpi3[, c("geoid", "pop", "latino_pct", "white_pct", "black_pct", "asian_pct", "multiple_pct", "NativeAm_pct", "PacificIsl_pct", "other_pct")], 
                  by = "geoid", all.x = TRUE)

hpi_main <- hpi_main[order(hpi_main$geoid),]

hpi_main <- hpi_main %>%
  mutate(geoid = paste0("0",geoid))

hpi_main$hpi_score_round_trunc <-trunc(hpi_main$hpi2_pctile*100)/100
hpi_main$hpi_score_round <- ceiling(hpi_main$hpi_score_round_trunc)


## SURVEILLANCE LINELIST MASTER -> master, master_week

  master_raw <- as.data.frame(fread("//mnt/projects/connect-izb/rmd_generate/geocoding/linelist_geo_draft.tsv"))

  master <- master_raw %>% 
    filter(episode_date >= ymd(20200201) & episode_date <= ymd(20210630)) %>% #20210131
    # episode_date date_created lab_result_date onset_date %>% 
    mutate(county_fips = case_when(county == "Alameda" ~ 1,
                                   county == "Amador" ~ 005,
                                   county == "Butte" ~ 007,
                                   county == "Calaveras" ~ 009,
                                   county == "Colusa" ~ 011,
                                   county == "Contra Costa" ~ 013,
                                   county == "Del Norte" ~ 015,
                                   county == "El Dorado" ~ 017,
                                   county == "Fresno" ~ 019,
                                   county == "Glenn" ~ 021,
                                   county == "Humboldt" ~ 023,
                                   county == "Imperial" ~ 025,
                                   county == "Kern" ~ 029,
                                   county == "Kings" ~ 031,
                                   county == "Lake" ~ 033,
                                   county == "Lassen" ~ 035,
                                   county == "Los Angeles" ~ 037,
                                   county == "Madera" ~ 039,
                                   county == "Marin" ~ 041,
                                   county == "Mendocino" ~ 045,
                                   county == "Merced" ~ 047,
                                   county == "Monterey" ~ 053,
                                   county == "Napa" ~ 055,
                                   county == "Nevada" ~ 057,
                                   county == "Orange" ~ 059,
                                   county == "Placer" ~ 061,
                                   county == "Riverside" ~ 065,
                                   county == "Sacramento" ~ 067,
                                   county == "San Benito" ~ 069,
                                   county == "San Bernardino" ~ 071,
                                   county == "San Diego" ~ 073,
                                   county == "San Francisco" ~ 075,
                                   county == "San Joaquin" ~ 077,
                                   county == "San Luis Obispo" ~ 079,
                                   county == "San Mateo" ~ 081,
                                   county == "Santa Barbara" ~ 083,
                                   county == "Santa Clara" ~ 085,
                                   county == "Santa Cruz" ~ 087,
                                   county == "Shasta" ~ 089,
                                   county == "Siskiyou" ~ 093,
                                   county == "Solano" ~ 095,
                                   county == "Sonoma" ~ 097,
                                   county == "Stanislaus" ~ 099,
                                   county == "Sutter" ~ 101,
                                   county == "Tehama" ~ 103,
                                   county == "Tulare" ~ 107,
                                   county == "Tuolumne" ~ 109,
                                   county == "Ventura" ~ 111,
                                   county == "Yolo" ~ 113,
                                   county == "Yuba" ~ 115))  %>%
    ungroup()
    master$tract_fips <- master$census_tract
    master$geoid <- master$gc_census_tract_full
    master$case <- ifelse(master$ncov_result == "Positive", 1, 0)
    
    master <- master[order(master$geoid),]
rm(master_raw)

master_week <- master %>%
  filter(geoid < 7000000000) %>%
  mutate(geoid = paste0("0",geoid),
         date = as.Date(episode_date, format = "%b %Y"),
         wk = lubridate::isoweek(episode_date),
         mo = lubridate::month(episode_date),
         yr = lubridate::year(episode_date),
         yr_wks = strftime(episode_date, format = "%Yw%V"),
         yr_wk = yearweek(episode_date, week_start = getOption("lubridate.week.start", 1)),
         mo_yr = as.yearmon(episode_date, "%b %Y"),
         race = paste(race)) %>%
  group_by(geoid, race, yr_wks) %>%
  summarise(mo_yr = min(mo_yr),
            wk = min(wk),
            mo = min(mo), 
            yr = min(yr),
            number = n(),
            county = first(county),
            cases = sum(case, na.rm = T),
            hosp = ifelse(ever_hospitalized == "Y", 1, 0),
            icu = ifelse(icu == "Y", 1, 0),
            death = ifelse(death == "Y", 1, 0))  %>%
  ungroup()

master_week <- master_week %>% 
  distinct() %>% 
  filter(yr_wks !="2021w53") %>% 
  mutate(date = ceiling_date(ymd(paste(yr, "01", "01", sep = "-")) + 
                               (wk - 1) * 7 - 1, "week", week_start = 1)) %>% 
  relocate(date, .after = race)

  master_week <- merge(x = master_week, y = hpi_main, by = "geoid", all.x = TRUE)

  master_week <- master_week %>%
    mutate(case_rate = 100000 * cases/pop,
           hosp_rate = 100000 * hosp/pop,
           mort_rate = 100000 * death/pop)  %>%
    ungroup()

  master_week$hpi_score_round_trunc <-trunc(master_week$hpi2_pctile*100)/100
  master_week$hpi_score_round <- ceiling(master_week$hpi_score_round_trunc)
  
  master_week <- master_week %>% 
    mutate(hpi_octiles = case_when(hpi_score_round_trunc >= 0 & hpi_score_round_trunc <= 12.5 ~ 1,
                                   hpi_score_round_trunc > 12.5 & hpi_score_round_trunc <= 25 ~ 2,
                                   hpi_score_round_trunc > 25 & hpi_score_round_trunc <= 37.5 ~ 3,
                                   hpi_score_round_trunc > 37.5 & hpi_score_round_trunc <= 50 ~ 4,
                                   hpi_score_round_trunc > 50 & hpi_score_round_trunc <= 62.5 ~ 5,
                                   hpi_score_round_trunc > 62.5 & hpi_score_round_trunc <= 75 ~ 6,
                                   hpi_score_round_trunc > 75 & hpi_score_round_trunc <= 87.5 ~ 7,
                                   hpi_score_round_trunc > 87.5 & hpi_score_round_trunc <=100 ~ 8))
  master_week <- master_week %>% 
    mutate(hpi_quartiles = case_when(hpi_score_round_trunc >= 0 & hpi_score_round_trunc <= 25 ~ 1,
                                   hpi_score_round_trunc > 25 & hpi_score_round_trunc <= 50 ~ 2,
                                   hpi_score_round_trunc > 50 & hpi_score_round_trunc <= 75 ~ 3,
                                   hpi_score_round_trunc > 75 & hpi_score_round_trunc <= 100 ~ 4))


  rm(hpi, hpi3)

## VACCINE DATASETS ##############################################################
  
  zip_fin <- readRDS("//mnt/projects/connect-izb/resources/general/zip_master_fresh.rds")
  
  vem <- fread("//mnt/projects/epi/covid19/R/resources/progData/vem_allvariables_2021.csv") %>% 
    dplyr::select(zip, vem, vemquartile, county, totalpop) %>% 
    mutate(zip = as.character(zip))
  
  vem_octiles <- quantile(vem$vem, seq(0,1,by = 1/8))
  vem_20ths <- quantile(vem$vem, seq(0,1,by = 1/20))
  
  zip_fin <- zip_fin %>% 
    dplyr::select(-county) %>% 
    left_join(vem, by = "zip") %>% 
    mutate(vemq1 = if_else(vemquartile == 1, 1, 0),
           vemoctile = cut(vem, vem_octiles, label = F),
           vem20ths = cut(vem, vem_20ths, label = F))
  
  # County population from zip level dataset  
  cnty_pop <- vem %>% 
    filter(county!= "", !is.na(county)) %>% 
    group_by(county) %>% 
    summarise(pop = sum(totalpop))
  
  # Summarise to month
  monthly_zip_sum <- zip_fin %>% 
    filter(date >= ymd(20200201) & date <= ymd(20210630) & !is.na(vemquartile)) %>%
    #st_drop_geometry() %>% 
    filter(!is.na(vemquartile)) %>% 
    group_by(zip,moyr) %>% 
    summarise(date = first(date),
              county = first(county),
              region = first(region),
              pop = first(totalpop),
              vemq1 = mean(vemq1), # should be 0 or 1, but use this just to check
              vemquartile = first(vemquartile),
              vemoctile = first(vemoctile),
              vem20th = first(vem20ths),
              tests = sum(tests_new),
              vax = sum(vax),
              cases = sum(cases_new),
              hosps = sum(hosp),
              deaths = sum(deaths_new)) %>% 
    ungroup() %>% 
    mutate(testp100k = tests/pop*1e5,
           vaxp100k = vax/pop*1e5,
           casep100k = cases/pop*1e5,
           hospsp100k = hosps/pop*1e5,
           deathp100k = deaths/pop*1e5) %>% 
    arrange(zip, date)
  
  # Summarise to week
  weekly_zip_sum <- zip_fin %>% 
    filter(date >= ymd(20200201) & date <= ymd(20210630) & !is.na(vemquartile)) %>%
    mutate(wkyr = if_else(epiweek(date) > 50 & month(date) == 1, 
                          paste0(epiweek(date), "_", year(date)-1),
                          paste0(epiweek(date), "_", year(date)))) %>% 
    filter(!is.na(hpiquartile)) %>% 
    group_by(zip,wkyr) %>% 
    summarise(date = first(date),
              county = first(county),
              region = first(region),
              pop = first(totalpop),
              vemq1 = mean(vemq1), # should be 0 or 1, but use this just to check
              vemquartile = first(vemquartile),
              vemoctile = first(vemoctile),
              vem20th = first(vem20ths),
              vemscore = first(vem),
              tests = sum(tests_new),
              vax = sum(vax),
              cases = sum(cases_new),
              hosps = sum(hosp),
              deaths = sum(deaths_new)) %>% 
    ungroup() %>% 
    mutate(testp100k = tests/pop*1e5,
           vaxp100k = vax/pop*1e5,
           casep100k = cases/pop*1e5,
           hospsp100k = hosps/pop*1e5,
           deathp100k = deaths/pop*1e5) %>% 
    arrange(zip, date)
  
  # Summarise to week + vem quartiles
  veq_weekly_zip_sum <- weekly_zip_sum %>% 
    filter(date >= ymd(20200201) & date <= ymd(20210630) & !is.na(vemquartile)) %>%
    mutate(wkyr = if_else(epiweek(date) > 50 & month(date) == 1, 
                          paste0(epiweek(date), "_", year(date)-1),
                          paste0(epiweek(date), "_", year(date)))) %>% 
    group_by(vemquartile,wkyr) %>% 
    summarise(date = first(date),
              county = first(county),
              region = first(region),
              vem_pop = sum(pop),
              vemq1 = mean(vemq1), # should be 0 or 1, but use this just to check
              # vemquartile = first(vemquartile),
              vemoctile = first(vemoctile),
              vem20th = first(vem20th),
              vemscore = first(vem),
              tests = sum(tests),
              vax = sum(vax),
              cases = sum(cases),
              hosps = sum(hosps),
              deaths = sum(deaths)) %>% 
    ungroup() %>% 
    mutate(testp100k = tests/vem_pop*1e5,
           vaxp100k = vax/vem_pop*1e5,
           casep100k = cases/vem_pop*1e5,
           hospsp100k = hosps/vem_pop*1e5,
           deathp100k = deaths/vem_pop*1e5) %>% 
    arrange(vemquartile, date) %>% 
    group_by(vemquartile) %>% 
    mutate(cum_vax = cumsum(vax),
           cum_vaxp100k = (cumsum(vax))/vem_pop*1e5) %>% 
    ungroup() %>% 
    arrange(vemquartile, date)
  
  weekly_zip_sum$t_wk <- as.numeric(as.Date(as.character(weekly_zip_sum$date)) - ymd(20210131))/7
  veq_weekly_zip_sum$t_wk <- as.numeric(as.Date(as.character(veq_weekly_zip_sum$date)) - ymd(20210131))/7
  
  
  
################################################################
  
## 3. ANALYSIS ##############################################################
  
  # descriptives
  str(master)

##### TABLE 1 #####
  
  table(hpi_main)

##### FIG 2 #####
  ### reconstructing health equity metric of weekly test positivity, HPIQ1 and HPIQ4 and county over time
  zip_fin <- readRDS("//mnt/projects/connect-izb/resources/general/zip_master_fresh.rds")

  weekly_zip_sum <- zip_fin %>% 
  filter(date >= ymd(20200201) & date <= ymd(20210630)) %>% #20210131
  mutate(wkyr = if_else(epiweek(date) > 50 & month(date) == 1, 
                        paste0(epiweek(date), "_", year(date)-1),
                        paste0(epiweek(date), "_", year(date)))) %>% 
  filter(!is.na(hpiquartile)) %>% 
  group_by(zip,wkyr) %>% 
  summarise(date = first(date),
            hpiq1 = mean(hpiq1), # should be 0 or 1, but use this just to check
            hpiquartile = first(hpiquartile),
            hpioctile = first(hpioctile),
            hpipctile = first(hpipctile),
            county = first(county),
            region = first(region),
            pop = first(pop),
            tests = sum(tests_new),
            pos_tests = sum(pos_tests_new),
            vax = sum(vax),
            cases = sum(cases_new),
            hosps = sum(hosp),
            deaths = sum(deaths_new)) %>% 
  ungroup() %>% 
  mutate(testp100k = tests/pop*1e5,
         test_pos_rate = pos_tests/tests,
         vaxp100k = vax/pop*1e5,
         casep100k = cases/pop*1e5,
         hospsp100k = hosps/pop*1e5,
         deathp100k = deaths/pop*1e5) %>% 
  arrange(zip, date)

  weekly_zip_sum$t_wk <- as.numeric(as.Date(as.character(weekly_zip_sum$date)) - ymd(20210131))/7

  weekly_county <- weekly_zip_sum %>% 
    group_by(county,hpiquartile,wkyr) %>% 
    summarise(date = first(date),
              hpiq1 = mean(hpiq1), # should be 0 or 1, but use this just to check
              hpiquartile = first(hpiquartile),
              county = first(county),
              region = first(region),
              pop = sum(pop),
              tests = sum(tests),
              pos_tests = sum(pos_tests),
              vax = sum(vax),
              cases = sum(cases),
              hosps = sum(hosps),
              deaths = sum(deaths)) %>% 
    ungroup() %>% 
    mutate(testp100k = tests/pop*1e5,
           test_pos_rate = pos_tests/tests,
           vaxp100k = vax/pop*1e5,
           casep100k = cases/pop*1e5,
           hospsp100k = hosps/pop*1e5,
           deathp100k = deaths/pop*1e5) %>% 
    arrange(county, date)
  
  # plot themes
  scale_color_hpiq <- function(){
    scale_color_manual(name = "HPI Quartile:",
                       values = c("1" = "royalblue", "2" = "lightblue", "3" = "lightgreen", "4" = "darkgreen", "5" = "tomato1"),
                       labels = c("HPIQ1", "HPIQ2", "HPIQ3", "HPIQ4", "No HPI Quartile"))
  }
  scale_fill_hpiq <- function(){
    scale_fill_manual(name = "HPI Quartile:",
                      values = c("1" = "royalblue", "2" = "lightblue", "3" = "lightgreen", "4" = "darkgreen", "5" = "tomato1"),
                      labels = c("HPIQ1", "HPIQ2", "HPIQ3", "HPIQ4", "No HPI Quartile"))
  }
  theme_hpi <- function(){
    theme(axis.title = element_text(size = 14),
          axis.text = element_text(size = 12),
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 14))
  }


  ## 2023-0705 weekly test pos_alameda san bernardino
  weekly_county %>% 
    filter(date >= ymd(20200501) & date <= ymd(20210131)) %>% 
          # county == "Alameda"|
           # county == "Butte"|
           # county == "Contra Costa"|
           # county == "El Dorado"|
           # county == "Fresno"|
           # county == "Humboldt"|
           # county == "Imperial"|
           # county == "Kern"|
           # county == "Kings"|
           # county == "Los Angeles"|
           # county == "Madera"|
           # county == "Marin"|
           # county == "Merced"|
           # county == "Monterey"|
           # county == "Napa"|
           county == "Orange"|
           county == "Placer"|
           county == "Riverside"|
           county == "Sacramento"|
           county == "San Bernardino"|
            county == "San Diego"|
            county == "San Francisco"|
            county == "San Joaquin"|
            county == "San Luis Obispo"|
            county == "San Mateo"|
            county == "Santa Barbara"|
            county == "Santa Clara"|
            county == "Santa Cruz"|
            county == "Shasta"|
            county == "Solano"|
            county == "Sonoma"|
            county == "Stanislaus"|
            county == "Tulare"|
            county == "Ventura"|
            county == "Yolo") %>%
  ggplot(aes(y=test_pos_rate,x=date, group=as.factor(hpiquartile), color=as.factor(hpiquartile))) + 
    geom_line() + 
    facet_wrap(~ as.factor(county), ncol = 5, scales = "fixed") +
    theme(strip.background = element_rect(colour=NA, fill=NA),
          panel.border = element_rect(colour = "black", fill = NA)) +
    scale_color_hpiq() +
    scale_x_date(name = "", date_breaks = "months", date_labels = "%b %Y") +
    scale_y_continuous(name = "Weekly Test Positivity Rate (%)", labels = scales::percent) +
    #theme_hpi() +
    theme_bw() +
    theme(legend.position = "top",
        plot.title = element_text(hjust = 0.5)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    theme(text = element_text(size = 12))  +
    theme(axis.text = element_text(size = 8)) +
    ggtitle("Weekly Test Positivity Rate (%) by HPI Quartiles")



  weekly_county %>% 
    filter(county="Alameda") %>% 
    ggplot(aes(y=cases, x=date, fill=factor(hpi_quartiles), color=factor(hpi_quartiles))) +
    geom_bar(position = position_stack(reverse = TRUE), stat="identity") +
    scale_color_hpiq() +
    scale_fill_hpiq() +
    #scale_x_date(name = "", date_breaks = "weeks", date_labels = "%Y %W") +
    scale_x_date(name = "", date_breaks = "months", date_labels = "%b %Y") +
    scale_y_continuous(name = "Weekly Cases", labels = scales::comma) +
    theme_hpi() +
    theme_bw() +
    theme(legend.position = "top",
          plot.title = element_text(hjust = 0.5)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    theme(text = element_text(size = 12))  +
    theme(axis.text = element_text(size = 12)) +
    ggtitle("Weekly Cases")

  
  
  ##
  master_counties <- zip_fin %>%
    filter(geoid < 7000000000) %>%
    mutate(geoid = paste0("0",geoid),
           date = as.Date(episode_date, format = "%b %Y"),
           wk = lubridate::isoweek(episode_date),
           mo = lubridate::month(episode_date),
           yr = lubridate::year(episode_date),
           yr_wks = strftime(episode_date, format = "%Yw%V"),
           yr_wk = yearweek(episode_date, week_start = getOption("lubridate.week.start", 1)),
           mo_yr = as.yearmon(episode_date, "%b %Y"),
           race = paste(race)) %>%
    group_by(geoid, race, yr_wks) %>%
    summarise(mo_yr = min(mo_yr),
              wk = min(wk),
              mo = min(mo), 
              yr = min(yr),
              number = n(),
              county = first(county),
              tests = sum(test),
              cases = sum(case, na.rm = T),
              hosp = ifelse(ever_hospitalized == "Y", 1, 0),
              icu = ifelse(icu == "Y", 1, 0),
              death = ifelse(death == "Y", 1, 0))  %>%
    ungroup()
  
  
  master <- master_raw %>% 
    filter(episode_date >= ymd(20200201) & episode_date <= ymd(20210131)) %>% 
    # episode_date date_created lab_result_date onset_date %>% 
    mutate(county_fips = case_when(county == "Alameda" ~ 1,
                                   county == "Amador" ~ 005,
                                   county == "Butte" ~ 007,
                                   county == "Calaveras" ~ 009,
                                   county == "Colusa" ~ 011,
                                   county == "Contra Costa" ~ 013,
                                   county == "Del Norte" ~ 015,
                                   county == "El Dorado" ~ 017,
                                   county == "Fresno" ~ 019,
                                   county == "Glenn" ~ 021,
                                   county == "Humboldt" ~ 023,
                                   county == "Imperial" ~ 025,
                                   county == "Kern" ~ 029,
                                   county == "Kings" ~ 031,
                                   county == "Lake" ~ 033,
                                   county == "Lassen" ~ 035,
                                   county == "Los Angeles" ~ 037,
                                   county == "Madera" ~ 039,
                                   county == "Marin" ~ 041,
                                   county == "Mendocino" ~ 045,
                                   county == "Merced" ~ 047,
                                   county == "Monterey" ~ 053,
                                   county == "Napa" ~ 055,
                                   county == "Nevada" ~ 057,
                                   county == "Orange" ~ 059,
                                   county == "Placer" ~ 061,
                                   county == "Riverside" ~ 065,
                                   county == "Sacramento" ~ 067,
                                   county == "San Benito" ~ 069,
                                   county == "San Bernardino" ~ 071,
                                   county == "San Diego" ~ 073,
                                   county == "San Francisco" ~ 075,
                                   county == "San Joaquin" ~ 077,
                                   county == "San Luis Obispo" ~ 079,
                                   county == "San Mateo" ~ 081,
                                   county == "Santa Barbara" ~ 083,
                                   county == "Santa Clara" ~ 085,
                                   county == "Santa Cruz" ~ 087,
                                   county == "Shasta" ~ 089,
                                   county == "Siskiyou" ~ 093,
                                   county == "Solano" ~ 095,
                                   county == "Sonoma" ~ 097,
                                   county == "Stanislaus" ~ 099,
                                   county == "Sutter" ~ 101,
                                   county == "Tehama" ~ 103,
                                   county == "Tulare" ~ 107,
                                   county == "Tuolumne" ~ 109,
                                   county == "Ventura" ~ 111,
                                   county == "Yolo" ~ 113,
                                   county == "Yuba" ~ 115))  %>%
    ungroup()
  master$tract_fips <- master$census_tract
  master$geoid <- master$gc_census_tract_full
  master$case <- ifelse(master$ncov_result == "Positive", 1, 0)
  
  master_week <- master %>%
    filter(geoid < 7000000000) %>%
    mutate(geoid = paste0("0",geoid),
           date = as.Date(episode_date, format = "%b %Y"),
           wk = lubridate::isoweek(episode_date),
           mo = lubridate::month(episode_date),
           yr = lubridate::year(episode_date),
           yr_wks = strftime(episode_date, format = "%Yw%V"),
           yr_wk = yearweek(episode_date, week_start = getOption("lubridate.week.start", 1)),
           mo_yr = as.yearmon(episode_date, "%b %Y"),
           race = paste(race)) %>%
    group_by(geoid, race, yr_wks) %>%
    summarise(mo_yr = min(mo_yr),
              wk = min(wk),
              mo = min(mo), 
              yr = min(yr),
              number = n(),
              county = first(county),
              cases = sum(case, na.rm = T),
              hosp = ifelse(ever_hospitalized == "Y", 1, 0),
              icu = ifelse(icu == "Y", 1, 0),
              death = ifelse(death == "Y", 1, 0))  %>%
    ungroup()
  
  
  master_counties <- master_week %>%
    filter(county_size %in% c("large")) %>%
    filter()
  

##### TABLE 2A - DESCRIPTIVES BY HPI QUARTILES #####
  all_dat_raw <- fread("//mnt/projects/connect-izb/rmd_generate/geocoding/testing_case_tract_calc.tsv") %>% 
    .[, date := as.Date(date)] %>% 
    mutate(geoid = location)
  
  all_dat <- merge(x = all_dat_raw, y = hpi_main, by = "geoid", all.x = TRUE)
  # table(all_dat$county)
  # length(unique(all_dat$county))
  # length(unique(all_dat$geoid))
  
  dat_month <- all_dat %>% 
    filter(location < 7000000000 & date <= ymd(20210131) & date >= ymd(20200201)) %>% 
    mutate(mo = lubridate::month(date),
           yr = lubridate::year(date),
           geoid = paste0("0",location),
           mo_yr = paste0(mo,"_",yr)) %>% 
    group_by(geoid, mo_yr) %>% 
    summarise(date = first(date),
              hpi2_pctile = first(hpi2_pctile),
              hpi2_quartile = first(hpi2_quartile),
              hpi3_pctile = first(hpi3_pctile), 
              hpi3_quartile = first(hpi3_quartile),
              pop = first(pop),
              tests = sum(tests_new, na.rm=T),
              positives = sum(pos_tests_new, na.rm=T),
              cases = sum(cases_new, na.rm=T),
              deaths = sum(deaths_new, na.rm=T)) %>% 
    ungroup()
  
  dat_month_merge <- dat_month %>% 
    mutate(state_quantile = ntile(hpi2_pctile, 4),
           state_quantile = case_when(state_quantile == 1 ~ "Q1",
                                      state_quantile == 4 ~ "Q4",
                                      state_quantile == 2 ~ "Q2",
                                      state_quantile == 3 ~ "Q3")) %>% 
    filter(!is.na(state_quantile)) %>% 
    group_by(state_quantile, mo_yr) %>% 
    summarise(date = first(date),
              pop = sum(pop),
              n = n(),
              cases = sum(cases),
              tests = sum(tests),
              positives = sum(positives),
              deaths = sum(deaths)) %>% 
    mutate(rate = cases/(pop/100000),
           mort_rate = deaths/(pop/100000),
           test_rate = tests/(pop/100000),
           test_positivity = 100*positives/tests) %>% 
    arrange(state_quantile, date)
  
  DR <- all_dat %>%
    # .[county == "Los Angeles"] %>% ########## to run for a single county
    .[location < 7000000000 & date <= ymd(20210131) & date >= ymd(20200201), 
      .(pop = first(pop),
        tests = sum(tests_new, na.rm=T),
        positives = sum(pos_tests_new, na.rm=T),
        cases = sum(cases_new, na.rm=T),
        deaths = sum(deaths_new, na.rm=T)), 
      by = geoid]
  
  indices <- c("hpi2_pctile", "economic_pctile", "healthcareaccess_pctile", "education_pctile", "housing_pctile", "social_pctile", "neighborhood_pctile", "transportation_pctile", "pollution_pctile") 
  economic_indicators = c("abovepoverty_pctile","employed_pctile","income_pctile")
  healthcare_indicators = c("healthcareaccess_pctile")
  education_indicators = c("bachelorsed_pctile", "inhighschool_pctile", "inpreschool_pctile")
  housing_indicators = c("homeownership_pctile", "houserepair_pctile", "rentsevere_pctile", "ownsevere_pctile", "uncrowded_pctile")
  
  
  cases_merge <- merge(x = DR, y = hpi, by = "geoid", all.x = TRUE)
  
  adi <- fread("//mnt/projects/connect-izb/resources/general/ca_bg.txt")
  
  svi <- fread("//mnt/projects/connect-izb/resources/general/california_svi.csv") %>%
    .[, geoid := paste0("0", FIPS)] %>%
    .[, percentile := (rank(RPL_THEMES, na.last="keep") / sum(!is.na(RPL_THEMES)))*100] %>% 
    .[, .(geoid, svi_score = percentile)]
  
  adi$geoid <- as.integer64.character(adi$geoid)
  svi$geoid <- as.integer64.character(svi$geoid)
  
  cases_merge <- merge(cases_merge, svi, by = "geoid")
  
  # cases_merge <- merge(cases_merge, total_tests, by = "geoid")
  # names(cases_merge)
  cases_merge <- cases_merge %>% 
    dplyr::select(
      # ct10 = 
      geoid,
      pop,
      # social determinants
      `Healthy Places Index` = hpi2_pctile,
      `Income` = income,
      `SVI` = svi_score,
      `CES` = ces3_pctile,
      `crowding` = uncrowded,
      county = county_name, 
      city, 
      cases, 
      tests, 
      positives, 
      deaths,
      # total_tests,
      ct_wt_cnty) %>%
    melt(id = c("geoid","pop", "county","city", "cases", "tests","positives","deaths", "ct_wt_cnty"), variable.name = "hpi_component",value.name = "value") %>% replace_na(list(cases = 0))
  
  cases_merge_tract <- data.table(cases_merge)
  cases_merge_tract <- cases_merge_tract[,.(cases,
                                            tests,
                                            city,
                                            positives,
                                            deaths,
                                            pop,
                                            value = value,
                                            county), by = .(geoid, hpi_component)]
  
  cases_merge_tract <- cases_merge_tract[, `:=` (rate = cases/(pop/100000),
                                                 mort_rate = deaths/(pop/100000),
                                                 test_rate = tests/(pop/100000),
                                                 test_positivity = 100*positives/tests
  )]
  
  cases_merge_tract <- cases_merge_tract[,state_quantile := ntile(value,4), by =.(hpi_component)] %>% 
    .[, state_quantile := case_when(state_quantile == 1 ~ "Q1",
                                    state_quantile == 4 ~ "Q4",
                                    state_quantile == 2 ~ "Q2",
                                    state_quantile == 3 ~ "Q3")]
  
  cases_merge_tract <- cases_merge_tract[!is.na(state_quantile)]
  
  hpi_tab1_dat <- cases_merge_tract %>% 
    filter(hpi_component == "Healthy Places Index") %>% 
    group_by(state_quantile) %>% 
    summarise(n_ct = n(),
              pop = sum(pop),
              tests = sum(tests),
              positives = sum(positives),
              cases = sum(cases),
              deaths = sum(deaths),
              test_rate = tests/(pop/1e5),
              case_rate = cases/(pop/1e5),
              mort_rate = deaths/(pop/1e5),
              test_positivity = (positives/tests)*100) %>% 
    ungroup()
  
##### TABLE 2B - VACCINE DOSES BY VEM QUARTILES #####
  
  # Summarise to week + vem quartiles
  veq_weekly_zip_sum <- weekly_zip_sum %>% 
    filter(date >= ymd(20200201) & date <= ymd(20210630) & !is.na(vemquartile)) %>%
    mutate(wkyr = if_else(epiweek(date) > 50 & month(date) == 1, 
                          paste0(epiweek(date), "_", year(date)-1),
                          paste0(epiweek(date), "_", year(date)))) %>% 
    group_by(vemquartile,wkyr) %>% 
    summarise(date = first(date),
              county = first(county),
              region = first(region),
              vem_pop = sum(pop),
              vemq1 = mean(vemq1), # should be 0 or 1, but use this just to check
              # vemquartile = first(vemquartile),
              vemoctile = first(vemoctile),
              vem20th = first(vem20th),
              vemscore = first(vem),
              tests = sum(tests),
              vax = sum(vax),
              cases = sum(cases),
              hosps = sum(hosps),
              deaths = sum(deaths)) %>% 
    ungroup() %>% 
    mutate(testp100k = tests/vem_pop*1e5,
           vaxp100k = vax/vem_pop*1e5,
           casep100k = cases/vem_pop*1e5,
           hospsp100k = hosps/vem_pop*1e5,
           deathp100k = deaths/vem_pop*1e5) %>% 
    arrange(vemquartile, date) %>% 
    group_by(vemquartile) %>% 
    mutate(cum_vax = cumsum(vax),
           cum_vaxp100k = (cumsum(vax))/vem_pop*1e5) %>% 
    ungroup() %>% 
    arrange(vemquartile, date)
  
  # Summarise to week + vem quartiles
  t1_vax_stats <- veq_weekly_zip_sum %>% 
    filter(date >= ymd(20200201) & date <= ymd(20210630) & !is.na(vemquartile)) %>%
    group_by(vemquartile) %>% 
    summarise(vem_pop = max(vem_pop),
              vax = sum(vax)) %>% 
    ungroup() %>% 
    group_by(vemquartile) %>% 
    mutate(vaxp100k = vax/vem_pop*1e5) %>% 
    ungroup()
  
  
  
  library("openxlsx")
  
  write.xlsx(t1_vax_stats, 't1_vax_stats.xlsx')
  


##### FIG 2 #####

countywide_pos <- fread("//mnt/projects/connect-izb/rmd_generate/lane6_indicator_app/lane6_indicators_cumall_classified.csv") %>% 
  .[variable %in% c("test_pos_nopris_7day_total_7lag_v2","county_positivity_low_hpi", "county_positivity_high_hpi") & location_level == "county"] %>%
  dcast.data.table(location + date ~ variable, value.var = "value") %>%
  .[, date := as.Date(date)] %>%
  #.[date >= max(date)- 100] %>%
  .[date >= ymd(20200401) & date <= ymd(20211231)] %>%
  .[, county := location] 

ggplot(countywide_pos[!is.na(county_positivity_low_hpi)]) + 
  geom_line(aes(x = as.Date(date), y = county_positivity_high_hpi), alpha = 1, lwd = 0.5, color = "darkgreen") + 
  geom_line(aes(x = as.Date(date), y = county_positivity_low_hpi), alpha = 1, lwd = 0.5, color = "royalblue") + 
  geom_line(aes(x = as.Date(date), y = test_pos_nopris_7day_total_7lag_v2), alpha = 0.7, lwd = 0.5, color = "black") + # , linetype = "dashed"
  geom_hline(yintercept = 8, color = "purple", alpha = .7, lwd = 0.25)+
  geom_hline(yintercept = 5, color = "red", alpha = .7, lwd = 0.25)+
  geom_hline(yintercept = 2, color = "orange", alpha = .7, lwd = 0.25)+
  facet_wrap(~ county, scales = "free_y") + 
  ggtitle("Daily Test Positivity",subtitle = "Highest HPI quartile (green), lowest HPI quartile (blue), county overall (black), reference lines at y = 2 (yellow), 5 (red), 8 (purple)") +
  theme_bw() +
  scale_x_date(name = "", date_breaks = "2 month", date_labels = "%b %Y") +
  scale_y_continuous(name = "Daily Test Positivity Rate (%)") +
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1)) +
  theme(text = element_text(size = 12))  +
  theme(axis.text = element_text(size = 12)) +
  theme(strip.text.x = element_text(size = 10)) +
  theme(strip.background =element_rect(fill="white", color="white"))



##### FIG 3 #####

  # IRR figures for case, mortality, and test rates

  irr_cases_monthly <- bind_rows(lapply(unique(dat_month_merge$mo_yr),
                                        function(t){
                                          dt_in <- as.data.table(dat_month_merge %>% filter(mo_yr == t))
                                          irrs <- irr_calc_cases(dt_in) %>% 
                                            dplyr::select(quartile, COVID_OUTCOME, IRR, IRRLL, IRRUL) %>% 
                                            pivot_wider(names_from = COVID_OUTCOME, values_from = c(IRR, IRRLL, IRRUL), id_cols = quartile) %>% 
                                            mutate(mo_yr = t,
                                                   date = dt_in$date)
                                          
                                          return(irrs)
                                        }))
  
  irr_deaths_monthly <- bind_rows(lapply(unique(dat_month_merge$mo_yr),
                                         function(t){
                                           dt_in <- as.data.table(dat_month_merge %>% filter(mo_yr == t))
                                           irrs <- irr_calc_deaths(dt_in) %>% 
                                             dplyr::select(quartile, COVID_OUTCOME, IRR, IRRLL, IRRUL) %>% 
                                             pivot_wider(names_from = COVID_OUTCOME, values_from = c(IRR, IRRLL, IRRUL), id_cols = quartile) %>% 
                                             mutate(mo_yr = t,
                                                    date = dt_in$date)
                                           
                                           return(irrs)
                                         }))
  
  irr_tests_monthly <- bind_rows(lapply(unique(dat_month_merge$mo_yr),
                                        function(t){
                                          dt_in <- as.data.table(dat_month_merge %>% filter(mo_yr == t))
                                          irrs <- irr_calc_tests(dt_in) %>% 
                                            dplyr::select(quartile, COVID_OUTCOME, IRR, IRRLL, IRRUL) %>% 
                                            pivot_wider(names_from = COVID_OUTCOME, values_from = c(IRR, IRRLL, IRRUL), id_cols = quartile) %>% 
                                            mutate(mo_yr = t,
                                                   date = dt_in$date)
                                          
                                          return(irrs)
                                        }))
  
  irr_vax_monthly <- bind_rows(lapply(unique(veq_monthly_zip_sum$moyr),
                                      function(t){
                                        dt_in <- as.data.table(veq_monthly_zip_sum %>% filter(moyr == t))
                                        irrs <- irr_calc_tests(dt_in) %>% 
                                          dplyr::select(quartile, COVID_OUTCOME, IRR, IRRLL, IRRUL) %>% 
                                          pivot_wider(names_from = COVID_OUTCOME, values_from = c(IRR, IRRLL, IRRUL), id_cols = quartile) %>% 
                                          mutate(moyr = t,
                                                 date = dt_in$date)
                                        
                                        return(irrs)
                                      }))
  
  
  # Scale factor for plotting case irr and cases on same plot
  case_scale_fac = 1/800
  
  plot_cr_monthly <- ggplot() +
    geom_bar(data = dat_month_merge, 
             aes(x = date, y = rate, fill = state_quantile),
             position="dodge", stat="identity") +
    geom_line(data = irr_cases_monthly %>% filter(quartile != "Q4"),
              aes(x = date, y = IRR_cases*1/case_scale_fac, 
                  col = quartile),
              show.legend = FALSE) + 
    geom_ribbon(data = irr_cases_monthly %>% filter(quartile != "Q4"),
                aes(x = date, fill = quartile,
                    ymin = IRRLL_cases*1/case_scale_fac, ymax = IRRUL_cases*1/case_scale_fac),
                alpha = 0.3) +
    scale_color_hpiq() +
    scale_fill_hpiq() +
    scale_x_date(date_breaks = "1 month",
                 date_labels = "%b %y",
                 limits = c(ymd(20200301), ymd(20211231))) +
    scale_y_continuous(name = "Monthly Cases per 100k (bars)",
                       sec.axis = sec_axis(trans = ~.*case_scale_fac, name = "Case RR (lines)")) +
    theme_classic() +
    theme_hpi() +
    theme(legend.position = "none",
          axis.text.x = element_blank()) +
    geom_hline(yintercept = 1/case_scale_fac) +
    labs(x = "",  fill = "")
  
  
  mort_scale_fac = 1/10
  
  plot_mr_monthly <- ggplot() +
    geom_bar(data = dat_month_merge %>% filter(date >= ymd(20200301)),
             aes(x = date, y = mort_rate, fill = state_quantile),
             position="dodge", stat="identity") +
    geom_line(data = irr_deaths_monthly %>% filter(quartile != "Q4", date >= ymd(20200301)),
              aes(x = date, y = IRR_deaths*1/mort_scale_fac, col = quartile),
              show.legend = FALSE) + 
    geom_ribbon(data = irr_deaths_monthly %>% filter(quartile != "Q4", date >= ymd(20200301)),
                aes(x = date, fill = quartile,
                    ymin = IRRLL_deaths*1/mort_scale_fac, ymax = IRRUL_deaths*1/mort_scale_fac),
                alpha = 0.3) +
    scale_fill_hpiq() + 
    scale_color_hpiq() +
    scale_x_date(date_breaks = "1 month",
                 date_labels = "%b'%y",
                 limits = c(ymd(20200301), ymd(20211231))) +
    scale_y_continuous(name = "Monthly Deaths per 100k (bars)",
                       sec.axis = sec_axis(trans = ~.*mort_scale_fac, name = "Mortality RR (lines)")) +
    theme_classic() +
    theme_hpi() +
    theme(legend.position = "none",
          axis.text.x = element_blank()) +
    geom_hline(yintercept = 1/mort_scale_fac) +
    labs(x = "", y = "")
  
  
  test_scale_fac = 1/12000
  
  plot_tr_monthly <- ggplot() +
    geom_bar(data = dat_month_merge,
             aes(x = date, y = test_rate, fill = state_quantile),
             position="dodge", stat="identity") +
    geom_line(data = irr_tests_monthly %>% filter(quartile != "Q4", date > ymd(20200220)),
              aes(x = date, y = IRR_tests*1/test_scale_fac, col = quartile),
              show.legend = FALSE) + 
    geom_ribbon(data = irr_tests_monthly %>% filter(quartile != "Q4", date > ymd(20200220)),
                aes(x = date, fill = quartile,
                    ymin = IRRLL_tests*1/test_scale_fac, ymax = IRRUL_tests*1/test_scale_fac),
                alpha = 0.3) +
    scale_fill_hpiq() +
    scale_color_hpiq() +
    scale_x_date(date_breaks = "1 month",
                 date_labels = "%b'%y",
                 limits = c(ymd(20200301), ymd(20211231))) +
    scale_y_continuous(name = "Monthly Tests per 100k (bars)",
                       sec.axis = sec_axis(trans = ~.*test_scale_fac, name = "Test RR (lines)")) +
    theme_classic() +
    theme_hpi() +
    theme(legend.position = c(0.15,0.825),
          legend.text = element_text(size = 9),
          legend.title = element_blank(),
          legend.key.size = unit(0.3, "cm"),
          axis.text.x = element_text(angle = 45, hjust = 1)) +
    geom_hline(yintercept = 1/test_scale_fac) +
    labs(x = "", y = "")
  
  test_scale_fac = 1/12000
  
  plot_vax_monthly <- ggplot() +
    geom_bar(data = veq_monthly_zip_sum,
             aes(x = date, y = vaxp100k, fill = state_quantile),
             position="dodge", stat="identity") +
    geom_line(data = irr_vax_monthly %>% filter(quartile != 4, date > ymd(20201201)),
              aes(x = date, y = IRR_tests*1/test_scale_fac, col = quartile),
              show.legend = FALSE) + 
    geom_ribbon(data = irr_vax_monthly %>% filter(quartile != 4, date > ymd(20201201)),
                aes(x = date, fill = quartile,
                    ymin = IRRLL_tests*1/test_scale_fac, ymax = IRRUL_tests*1/test_scale_fac),
                alpha = 0.3) +
    scale_fill_hpiq() +
    scale_color_hpiq() +
    scale_x_date(date_breaks = "1 month",
                 date_labels = "%b'%y",
                 limits = c(ymd(20200301), ymd(20210630))) +
    scale_y_continuous(name = "Monthly Vaccines per 100k (bars)",
                       sec.axis = sec_axis(trans = ~.*test_scale_fac, name = "Vaccines RR (lines)")) +
    theme_classic() +
    theme_hpi()+
    theme(legend.position = c(0.15,0.825),
          legend.text = element_text(size = 9),
          legend.title = element_blank(),
          legend.key.size = unit(0.3, "cm"),
          axis.text.x = element_text(angle = 45, hjust = 1)) +
    geom_hline(yintercept = 1/test_scale_fac) +
    labs(x = "", y = "")
  
  
  
  
  plot <- plot_cr_monthly /  plot_mr_monthly / plot_tr_monthly 


##### FIG 4 - VACCINES #####
  
  # bars on y1, cumul vax on y2
  
  scale_color_veqf <- function(){
    scale_color_manual(name = "Vaccine Equity Quartile:",
                       values = c("1" = "#2171B5", "2" = "#9ECAE1", "3" = "#A6DBA0", "4" = "#008837", "5" = "tomato1"),
                       labels = c("VEM Q1", "VEM Q2", "VEM Q3", "VEM Q4", "No VEM Quartile"))
  }
  scale_fill_veqf <- function(){
    scale_fill_manual(name = "Vaccine Equity Quartile:",
                      values = c("1" = "#2171B5", "2" = "#9ECAE1", "3" = "#A6DBA0", "4" = "#008837", "5" = "tomato1"),
                      #values = c("1" = fade("royalblue",0.2), "2" = "lightblue", "3" = "lightgreen", "4" = "darkgreen", "5" = "tomato1"),
                      labels = c("VEM Q1", "VEM Q2", "VEM Q3", "VEM Q4", "No VEM Quartile"))
  }
  
  vax_scale_fac = 1000
  
  plot_vax <- ggplot() +
    geom_bar(data = veq_weekly_zip_sum %>% filter(date >= ymd(20201201)),
             aes(x = date, y = vaxp100k, fill = as.factor(vemquartile)), # alpha = 0.8),
             position="dodge", stat="identity") +
    geom_line(data = veq_weekly_zip_sum %>% filter(date >= ymd(20201201)),
              aes(x = date, y = cum_vax*1/vax_scale_fac, 
                  col = as.factor(vemquartile)), size = 1, #, linetype = "dashed",
              show.legend = FALSE) + 
    scale_color_veqf() +
    scale_fill_veqf() +
    scale_x_date(name = "", 
                 date_breaks = "1 month",
                 date_labels = "%b %y",
                 limits = c(ymd(20201201), ymd(20210630))) +
    scale_y_continuous(name = "Weekly Vaccine Doses per 100k (bars)", labels = scales::label_comma(),
                       sec.axis = sec_axis(trans = ~.*vax_scale_fac, name = "Cumul Vaccine Doses (lines)",
                                           labels = scales::label_comma())) +
    geom_vline(xintercept = as.numeric(as.Date("2021-03-02")), linetype = 2) +
    theme_classic() +
    #theme_hpi() +
    theme(legend.position = "top",
          plot.title = element_text(hjust = 0.5)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    theme(text = element_text(size = 14))  +
    theme(axis.text = element_text(size = 12))
  plot_vax
  
  
## SUPPLEMENTAL APPENDIX ##
  
  weekly_zip_sum <- zip_fin %>% 
    filter(date >= ymd(20200301)& date <= today()) %>% 
    mutate(wkyr = if_else(epiweek(date) > 50 & month(date) == 1, 
                          paste0(epiweek(date), "_", year(date)-1),
                          paste0(epiweek(date), "_", year(date)))) %>% 
    filter(!is.na(hpiquartile)) %>% 
    group_by(zip,wkyr) %>% 
    summarise(date = first(date),
              hpiq1 = mean(hpiq1), # should be 0 or 1, but use this just to check
              hpiquartile = first(hpiquartile),
              hpioctile = first(hpioctile),
              hpipctile = first(hpipctile),
              county = first(county),
              region = first(region),
              pop = first(pop),
              tests = sum(tests_new),
              vax = sum(vax),
              cases = sum(cases_new),
              hosps = sum(hosp),
              deaths = sum(deaths_new)) %>% 
    ungroup() %>% 
    mutate(testp100k = tests/pop*1e5,
           vaxp100k = vax/pop*1e5,
           casep100k = cases/pop*1e5,
           hospsp100k = hosps/pop*1e5,
           deathp100k = deaths/pop*1e5) %>% 
    arrange(zip, date)
  
  weekly_zip_sum$t_wk <- as.numeric(as.Date(as.character(weekly_zip_sum$date)) - ymd(20210131))/7
  
  ### Blueprint Equity Threshold  
  
  tract_data_hpi <- hpi_main %>% 
    group_by(county_name) %>% 
    summarise(county = first(county_name),
              county_fips = first(county_fips),
              n_tracts = n(),
              pop = sum(pop)) %>% 
    ungroup() %>% 
    mutate(large_county = case_when(
      pop > 106000 ~ 1,
      pop <=106000 ~ 0)) %>% 
    filter(large_county == 1) %>%
    ungroup()

  ##### S6 FIG - Weekly cases, hospitalizations, and deaths by HPI quartiles and race groups from case surveillance
  
  table(master$race)
  prop.table(table(master$race))
  
  table(master$race, master$congregate_settings)
  prop.table(table(master$race, master$congregate_settings))
  ftable(round(prop.table(table1), 3))
  
  table(master$race, master$race)
  
  
##### S3 TEXT, TABLE C #####
  # SOURCE: https://data.chhs.ca.gov/dataset/covid-19-blueprint-for-a-safer-economy
##### S6 FIG #####

  master_week$hpi_quartiles[is.na(master_week$hpi_quartiles)] <- 5
  
  master_week <- master_week %>% 
    mutate(race_num = case_when(
      race == "American Indian or Alaska Native" ~ 2,
      race == "Asian" ~ 3,
      race == "Black or African American" ~ 1,
      race == "Multiple Races" ~ 6,
      race == "Native Hawaiian or Other Pacific Islander" ~ 4,
      race == "Other" ~ 7,
      race == "Unknown" ~ 8,
      race == "White" ~ 5,
      TRUE ~ NA_real_))
  
  master_week <- master_week %>% 
    mutate(race_known = case_when(
      race == "American Indian or Alaska Native" ~ 1,
      race == "Asian" ~ 1,
      race == "Black or African American" ~ 1,
      race == "Multiple Races" ~ 1,
      race == "Native Hawaiian or Other Pacific Islander" ~ 1,
      race == "Other" ~ 2,
      race == "Unknown" ~ 3,
      race == "White" ~ 1,
      TRUE ~ NA_real_))
  
  master_week$race_num2 <- factor(master_week$race_num, levels = c("8", "7", "6", "5", "4", "3", "2", "1"))
  #master_week$race_known <- factor(master_week$race_known, levels = c("8", "7", "6", "1"))
  
  
  # Plot themes
  scale_color_hpiq <- function(){
    scale_color_manual(name = "HPI Quartile:",
                       values = c("1" = "royalblue", "2" = "lightblue", "3" = "lightgreen", "4" = "darkgreen", "5" = "tomato1"),
                       labels = c("HPIQ1", "HPIQ2", "HPIQ3", "HPIQ4", "No HPI Quartile"))
  }
  scale_fill_hpiq <- function(){
    scale_fill_manual(name = "HPI Quartile:",
                      values = c("1" = "royalblue", "2" = "lightblue", "3" = "lightgreen", "4" = "darkgreen", "5" = "tomato1"),
                      labels = c("HPIQ1", "HPIQ2", "HPIQ3", "HPIQ4", "No HPI Quartile"))
  }
  theme_hpi <- function(){
    theme(axis.title = element_text(size = 14),
          axis.text = element_text(size = 12),
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 14))
  }
  
  
  scale_color_re <- function(){
    scale_color_manual(name = "",
                       values = c("1" = "mediumorchid1", "2" = "mediumorchid2", "3" = "mediumorchid3", "4" = "mediumpurple3", "5" = "mediumpurple2", "6" = "mediumpurple1", "7" = "tomato4", "8" = "tomato1"),
                       labels = c("Black or African American", "American Indian or Alaska Native", "Asian", "Native Hawaiian or Other Pacific Islander", "White", "Multiple Races", "Other", "Unknown"))
  }
  scale_fill_re <- function(){
    scale_fill_manual(name = "",
                      values = c("1" = "mediumorchid1", "2" = "mediumorchid2", "3" = "mediumorchid3", "4" = "mediumpurple3", "5" = "mediumpurple2", "6" = "mediumpurple1", "7" = "tomato4", "8" = "tomato1"),
                      labels = c("Black or African American", "American Indian or Alaska Native", "Asian", "Native Hawaiian or Other Pacific Islander", "White", "Multiple Races", "Other", "Unknown"))
  }
  theme_re <- function(){
    theme(axis.title = element_text(size = 14),
          axis.text = element_text(size = 12),
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 14))
  }
  
  scale_color_re2 <- function(){
    scale_color_manual(name = "",
                       values = c("1" = "mediumorchid1", "2" = "mediumorchid2", "3" = "mediumorchid3", "4" = "mediumpurple3", "5" = "mediumpurple2", "6" = "mediumpurple1", "7" = "tomato4", "8" = "tomato1"),
                       labels = c("Unknown", "Other","Multiple Races", "White", "Native Hawaiian or Other Pacific Islander", "Asian", "American Indian or Alaska Native", "Black or African American"))
  }
  scale_fill_re2 <- function(){
    scale_fill_manual(name = "",
                      values = c("1" = "mediumorchid1", "2" = "mediumorchid2", "3" = "mediumorchid3", "4" = "mediumpurple3", "5" = "mediumpurple2", "6" = "mediumpurple1", "7" = "tomato4", "8" = "tomato1"),
                      labels = c("Unknown", "Other","Multiple Races", "White", "Native Hawaiian or Other Pacific Islander", "Asian", "American Indian or Alaska Native", "Black or African American"))
  }
  
  scale_color_rknown <- function(){
    scale_color_manual(name = "",
                       values = c("1" = "mediumpurple1", "2" = "tomato4", "3" = "tomato1"),
                       labels = c("Single or Multiple Race(s) Reported: Black or African American, American Indian or Alaska Native, Asian, Native Hawaiian or Other Pacific Islander, White, Multi-Race", "Other Race Reported", "Unknown"))
  }
  scale_fill_rknown <- function(){
    scale_fill_manual(name = "",
                      values = c("1" = "mediumpurple1", "2" = "tomato4", "3" = "tomato1"),
                      labels = c("Single or Multiple Race(s) Reported: Black or African American, American Indian or Alaska Native, Asian, Native Hawaiian or Other Pacific Islander, White, Multi-Race", "Other Race Reported", "Unknown"))
  }
  
  
  ## CASES BY HPI
  # S6 FIG PANEL A, left panel, stacked bar
  master_week %>% 
    ggplot(aes(y=cases, x=date, fill=factor(hpi_quartiles), color=factor(hpi_quartiles))) +
    geom_bar(position = position_stack(reverse = TRUE), stat="identity") +
    scale_color_hpiq() +
    scale_fill_hpiq() +
    #scale_x_date(name = "", date_breaks = "weeks", date_labels = "%Y %W") +
    scale_x_date(name = "", date_breaks = "months", date_labels = "%b %Y") +
    scale_y_continuous(name = "Weekly Cases", labels = scales::comma) +
    theme_hpi() +
    theme_bw() +
    theme(legend.position = "top",
          plot.title = element_text(hjust = 0.5)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    theme(text = element_text(size = 16))  +
    theme(axis.text = element_text(size = 14)) +
    ggtitle("Weekly Cases")
  
  # S6 FIG PANEL A, right panel, percent stacked
  master_week %>% 
    ggplot(aes(y=cases, x=date, fill=factor(hpi_quartiles), color=factor(hpi_quartiles))) +
    geom_bar(position=position_fill(reverse = TRUE), stat="identity") +
    scale_color_hpiq() +
    scale_fill_hpiq() +
    scale_x_date(name = "", date_breaks = "months", date_labels = "%b %Y") +
    scale_y_continuous(name = "Proportion of Weekly Cases", labels = scales::comma) +
    theme_hpi() +
    theme_bw() +
    theme(legend.position = "top",
          plot.title = element_text(hjust = 0.5)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    theme(text = element_text(size = 16))  +
    theme(axis.text = element_text(size = 14)) +
    ggtitle("Proportion of Weekly Cases")
  
  
  ## CASES BY RACE
  # S6 FIG PANEL B, left panel, stacked bar
  master_week %>% 
    ggplot(aes(y=cases, x=date, fill=factor(race_known), color=factor(race_known))) +
    geom_bar(position = "stack", stat="identity") +
    #geom_bar(position = position_stack(reverse = TRUE)
    scale_color_rknown() +
    scale_fill_rknown() +
    scale_x_date(name = "", date_breaks = "months", date_labels = "%b %Y") +
    scale_y_continuous(name = "Weekly Cases", labels = scales::comma) +
    theme_re() +
    theme_bw() +
    theme(legend.position = "top",
          plot.title = element_text(hjust = 0.5)) +
    guides(color = guide_legend(nrow = 3)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    theme(text = element_text(size = 12))  +
    theme(axis.text = element_text(size = 12)) +
    ggtitle("Weekly Cases")
  
  # S6 FIG PANEL B, right panel, percent stacked
  master_week %>% 
    ggplot(aes(y=cases, x=date, fill=factor(race_known), color=factor(race_known))) +
    geom_bar(position="fill", stat="identity") +
    scale_fill_rknown() +
    scale_color_rknown() +
    scale_x_date(name = "", date_breaks = "months", date_labels = "%b %Y") +
    scale_y_continuous(name = "Proportion of Weekly Cases", labels = scales::comma) +
    theme_re() +
    theme_bw() +
    theme(legend.position = "top",
          plot.title = element_text(hjust = 0.5)) +
    guides(fill = guide_legend(nrow = 3)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    theme(text = element_text(size = 12))  +
    theme(axis.text = element_text(size = 12)) +
    ggtitle("Proportion of Weekly Cases")
  
  
  
  ## HOSPITALIZATIONS BY HPI
  # S6 FIG PANEL C, left panel, stacked bar
  master_week %>% 
    ggplot(aes(y=hosp, x=date, fill=factor(hpi_quartiles), color=factor(hpi_quartiles))) +
    geom_bar(position = position_stack(reverse = TRUE), stat="identity") +
    scale_fill_hpiq() +
    scale_color_hpiq() +
    scale_x_date(name = "", date_breaks = "months", date_labels = "%b %Y") +
    scale_y_continuous(name = "Weekly Hospitalizations", labels = scales::comma) +
    theme_hpi() +
    theme_bw() +
    theme(legend.position = "top",
          plot.title = element_text(hjust = 0.5)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    theme(text = element_text(size = 16))  +
    theme(axis.text = element_text(size = 14)) +
    ggtitle("Weekly Hospitalizations")
  
  # S6 FIG PANEL C, right panel, percent stacked
  master_week %>% 
    ggplot(aes(y=hosp, x=date, fill=factor(hpi_quartiles), color=factor(hpi_quartiles))) +
    geom_bar(position=position_fill(reverse = TRUE), stat="identity") +
    scale_fill_hpiq() +
    scale_color_hpiq() +
    scale_x_date(name = "", date_breaks = "months", date_labels = "%b %Y") +
    scale_y_continuous(name = "Proportion of Weekly Hospitalizations", labels = scales::comma) +
    theme_hpi() +
    theme_bw() +
    theme(legend.position = "top",
          plot.title = element_text(hjust = 0.5)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    theme(text = element_text(size = 16))  +
    theme(axis.text = element_text(size = 14)) +
    ggtitle("Proportion of Weekly Hospitalizations")
  
  
  ## HOSPITALIZATIONS BY RACE
  # S6 FIG PANEL D, left panel, stacked bar
  master_week %>% 
    ggplot(aes(y=hosp, x=date, fill=factor(race_known), color=factor(race_known))) +
    geom_bar(position = "stack", stat="identity") +
    scale_color_rknown() +
    scale_fill_rknown() +
    scale_x_date(name = "", date_breaks = "months", date_labels = "%b %Y") +
    scale_y_continuous(name = "Weekly Hospitalizations", labels = scales::comma) +
    theme_re() +
    theme_bw() +
    theme(legend.position = "top",
          plot.title = element_text(hjust = 0.5)) +
    guides(color = guide_legend(nrow = 3)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    theme(text = element_text(size = 16))  +
    theme(axis.text = element_text(size = 14)) +
    ggtitle("Weekly Hospitalizations")
  
  # S6 FIG PANEL D, right panel, percent stacked
  master_week %>% 
    ggplot(aes(y=hosp, x=date, fill=factor(race_known), color=factor(race_known))) +
    geom_bar(position="fill", stat="identity") +
    scale_fill_rknown() +
    scale_color_rknown() +
    scale_x_date(name = "", date_breaks = "months", date_labels = "%b %Y") +
    scale_y_continuous(name = "Proportion of Weekly Hospitalizations", labels = scales::comma) +
    theme_re() +
    theme_bw() +
    theme(legend.position = "top",
          plot.title = element_text(hjust = 0.5)) +
    guides(fill = guide_legend(nrow = 3)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    theme(text = element_text(size = 16))  +
    theme(axis.text = element_text(size = 14)) +
    ggtitle("Proportion of Weekly Hospitalizations")
  
  
  ## DEATHS BY HPI
  # S6 FIG PANEL E, left panel, stacked bar - 2023-0711 e death_hpi
  master_week %>%
    ggplot(aes(y=death, x=date, fill=factor(hpi_quartiles), color=factor(hpi_quartiles))) +
    geom_bar(position = position_stack(reverse = TRUE), stat="identity") +
    scale_fill_hpiq() +
    scale_color_hpiq() +
    scale_x_date(name = "", date_breaks = "months", date_labels = "%b %Y") +
    scale_y_continuous(name = "Weekly Deaths", labels = scales::comma) +
    theme_hpi() +
    theme_bw() +
    theme(legend.position = "top",
          plot.title = element_text(hjust = 0.5)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    theme(text = element_text(size = 16))  +
    theme(axis.text = element_text(size = 14)) +
    ggtitle("Weekly Deaths")
  
  # S6 FIG PANEL E, left panel, percent stacked - 2023-0711 e death_hpi_prop
  master_week %>% 
    ggplot(aes(y=death, x=date, fill=factor(hpi_quartiles), color=factor(hpi_quartiles))) +
    geom_bar(position=position_fill(reverse = TRUE), stat="identity") +
    scale_fill_hpiq() +
    scale_color_hpiq() +
    scale_x_date(name = "", date_breaks = "months", date_labels = "%b %Y") +
    scale_y_continuous(name = "Proportion of Weekly Deaths", labels = scales::comma) +
    theme_hpi() +
    theme_bw() +
    theme(legend.position = "top",
          plot.title = element_text(hjust = 0.5)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    theme(text = element_text(size = 16))  +
    theme(axis.text = element_text(size = 14)) +
    ggtitle("Proportion of Weekly Deaths")
  
  ## DEATHS BY RACE
  # S6 FIG PANEL F, left panel, stacked bar - 2023-0711 f death_race_known
  master_week %>% 
    ggplot(aes(y=death, x=date, fill=factor(race_known), color=factor(race_known))) +
    geom_bar(position = "stack", stat="identity") +
    scale_color_rknown() +
    scale_fill_rknown() +
    scale_x_date(name = "", date_breaks = "months", date_labels = "%b %Y") +
    scale_y_continuous(name = "Weekly Deaths", labels = scales::comma) +
    theme_re() +
    theme_bw() +
    theme(legend.position = "top",
          plot.title = element_text(hjust = 0.5)) +
    guides(color = guide_legend(nrow = 3)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    theme(text = element_text(size = 12))  +
    theme(axis.text = element_text(size = 14)) +
    ggtitle("Weekly Deaths")
  
  # S6 FIG PANEL F, right panel, percent stacked - 2023-0711 f death_race_known_prop
  master_week %>% 
    ggplot(aes(y=death, x=date, fill=factor(race_known), color=factor(race_known))) +
    geom_bar(position="fill", stat="identity") +
    scale_fill_rknown() +
    scale_color_rknown() +
    scale_x_date(name = "", date_breaks = "months", date_labels = "%b %Y") +
    scale_y_continuous(name = "Proportion of Weekly Deaths", labels = scales::comma) +
    theme_re() +
    theme_bw() +
    theme(legend.position = "top",
          plot.title = element_text(hjust = 0.5)) +
    guides(fill = guide_legend(nrow = 3)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    theme(text = element_text(size = 12))  +
    theme(axis.text = element_text(size = 14)) +
    ggtitle("Proportion of Weekly Deaths")

  
##### S7 FIG #####
  # percent stacked
  master_week <- master_week[order(master_week$yr_wk, master_week$race),]
  
  ## HPI v2 r/e makeup
  #latino_pct white_pct black_pct asian_pct NativeAm_pct PacificIsl_pct multiple_pct other_pct
  #hpi2_pctile
  hpi_main_wide <- hpi_main[, c("geoid", "hpi_score_round", "latino_pct", "white_pct", "black_pct", "asian_pct", "NativeAm_pct", "PacificIsl_pct", "multiple_pct", "other_pct")]
  
  hpi_main_long <- hpi_main_wide %>% 
    gather(key="race_eth", value="Percentage", 3:10)
  hpi_main_long$Percentage <-   hpi_main_long$Percentage * 100
  
  hpi_main_long <- hpi_main_long %>%
    mutate(race_eth2 = case_when(
      race_eth == "asian_pct" ~ 3,
      race_eth == "black_pct" ~ 1,
      race_eth == "latino_pct" ~ 4,
      race_eth == "multiple_pct" ~ 7,
      race_eth == "NativeAm_pct" ~ 2,
      race_eth == "other_pct" ~ 8,
      race_eth == "PacificIsl_pct" ~ 5,
      race_eth == "white_pct" ~ 6,
      TRUE ~ NA_real_))
  
  #re_palette <- c("#d84446", "#004aa4", "#ee7822", "#82cedc", "#e7ba37", "#b94a0f", "#c23498")
  
  scale_color_re <- function(){
    scale_color_manual(name = "",
                       values = c("1" = "#d84446", "2" = "#004aa4", "3" = "#ee7822", "4" = "#82cedc", "5" = "#e7ba37", "6" = "#b94a0f", "7" = "#c23498"),
                       labels = c("Black or African American",
                                  "American Indian or Alaska Native",
                                  "Asian American",
                                  "Latino/a",
                                  "Native Hawaiian or Other Pacific Islander",
                                  "White",
                                  "Multi-Race",
                                  "Other"))
  }
  scale_fill_re <- function(){
    scale_fill_manual(name = "",
                      values = c("1" = "#d84446", "2" = "#004aa4", "3" = "#ee7822", "4" = "#82cedc", "5" = "#e7ba37", "6" = "#b94a0f", "7" = "#c23498"),
                      labels = c("Black or African American",
                                 "American Indian or Alaska Native",
                                 "Asian American",
                                 "Latino/a",
                                 "Native Hawaiian or Other Pacific Islander",
                                 "White",
                                 "Multi-Race",
                                 "Other"))
  }
  
  
  # Y-AXIS IS 0-100%
  hpi_main_long %>% 
    filter(race_eth2 != 8) %>% 
    ggplot(aes(x = hpi_score_round, y = Percentage, color = as.factor(race_eth2), group = as.factor(race_eth2)), size=2) + 
    geom_smooth(method = "loess", se = FALSE) + 
    scale_color_re() +
    scale_fill_re() +
    ylim(0,100) + #ylim(0,5)
    theme(axis.title.x = element_blank()) +
    theme_bw() +
    ylab("Average Percentage among Census Tracts") +
    xlab("HPI v2 Percentile") +
    theme(text = element_text(size = 20))  +
    theme(axis.text = element_text(size = 20)) +
    theme(
      legend.position = c(.95, .95),
      legend.justification = c("right", "top"),
      legend.box.just = "right",
      legend.margin = margin(3, 3, 3, 3)
    )        
  
  # Y-AXIS IS 0-5% FOR ZOOMED IN LOOK
  hpi_main_long %>% 
    filter(race_eth2 != 8) %>% 
    ggplot(aes(x = hpi_score_round, y = Percentage, color = as.factor(race_eth2), group = as.factor(race_eth2)), size=2) + 
    # ggplot(aes(x = hpi_score_round, y = Percentage, color = as.factor(race_eth2), group = as.factor(race_eth2)), size=2) + 
    geom_smooth(method = "loess", se = FALSE) + 
    scale_color_re() +
    scale_fill_re() +
    ylim(0,5) + #ylim(0,5)
    theme(axis.title.x = element_blank()) +
    theme_bw() +
    ylab("Average Percentage among Census Tracts") +
    xlab("HPI v2 Percentile") +
    theme(text = element_text(size = 20))  +
    theme(axis.text = element_text(size = 20)) +
    theme(
      legend.position = c(.95, .95),
      legend.justification = c("right", "top"),
      legend.box.just = "right",
      legend.margin = margin(3, 3, 3, 3)
    )
##### S8 TABLE #####
  
  ggplot() +
    geom_bar(data = dat_month_merge, 
             aes(x = date, y = rate, fill = state_quantile),
             position="dodge", stat="identity") +
    geom_line(data = irr_cases_monthly %>% filter(quartile != "Q4"),
              aes(x = date, y = IRR_cases*1/case_scale_fac, 
                  col = quartile),
              show.legend = FALSE) + 
    geom_ribbon(data = irr_cases_monthly %>% filter(quartile != "Q4"),
                aes(x = date, fill = quartile,
                    ymin = IRRLL_cases*1/case_scale_fac, ymax = IRRUL_cases*1/case_scale_fac),
                alpha = 0.3) +
    geom_vline(xintercept = ymd(20200830), lty = 2) +
    scale_color_hpiq() +
    scale_fill_hpiq() +
    scale_x_date(date_breaks = "1 month",
                 date_labels = "%b'%y",
                 limits = c(ymd(20200301), ymd(20210131))) +
    scale_y_continuous(name = "Cases per 100k",
                       sec.axis = sec_axis(trans = ~.*case_scale_fac, name = "Case RR")) +
    theme_classic() +
    theme_hpi() +
    theme(legend.position = c(0.075,0.825),
          legend.text = element_text(size = 9),
          legend.title = element_blank(),
          legend.key.size = unit(0.3, "cm"),
          axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(x = "",  fill = "")
  
  ggsave(here::here("plots/equity/blueprint_monthly_cases_rr.png"), 
         width = 6, height = 4, units = "in")
  
  ggplot() +
    geom_line(data = irr_cases_monthly %>% filter(quartile != "Q4"),
              aes(x = date, y = IRR_cases, 
                  col = quartile),
              show.legend = FALSE) + 
    geom_ribbon(data = irr_cases_monthly %>% filter(quartile != "Q4"),
                aes(x = date, fill = quartile,
                    ymin = IRRLL_cases, ymax = IRRUL_cases),
                alpha = 0.3) +
    geom_vline(xintercept = ymd(20200830), lty = 2) +
    geom_hline(yintercept = 1, lty = 3) +
    scale_color_hpiq() +
    scale_fill_hpiq() +
    scale_x_date(date_breaks = "1 month",
                 date_labels = "%b'%y",
                 limits = c(ymd(20200301), ymd(20210131))) +
    scale_y_continuous(name = "Cases RR") +
    theme_classic() +
    theme_hpi() +
    theme(legend.position = c(0.075,0.825),
          legend.text = element_text(size = 9),
          legend.title = element_blank(),
          legend.key.size = unit(0.3, "cm"),
          axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(x = "",  fill = "")
  
  ggsave(here::here("plots/equity/blueprint_monthly_cases_rr_only.png"), 
         width = 6, height = 4, units = "in")
  
  
  irr <- merge(irr_tests_monthly, irr_cases_monthly, 
               by = c("mo_yr", "quartile", "date"), all.x = TRUE)
  
  irr <- merge(irr, irr_deaths_monthly, 
               by = c("mo_yr", "quartile", "date"), all.x = TRUE)
  
  write_xlsx(irr,"irr_monthly.xlsx")
  
  
##### S9 TABLE #####
  # see stata do file