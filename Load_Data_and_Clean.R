# TidyTuesday exploration idea


# Based on Harper S, Palayew A The annual cannabis holiday and fatal traffic crashes 
# Injury Prevention Published Online First: 29 January 2019. 
# doi: 10.1136/injuryprev-2018-043068
# https://injuryprevention.bmj.com/content/early/2019/01/28/injuryprev-2018-043068 
# Data/code for paper at https://osf.io/qnrg6/


#### Load packages -------------------------------------------------------------
library(purrr)
library(haven)
library(tidyverse)
library(lubridate)


#### Acquire raw data ----------------------------------------------------------
download.file("https://osf.io/kj7ub/download", "~/Downloads/farsp/farsp.zip")
unzip("~/Downloads/farsp/farsp.zip", exdir = "~/Downloads/farsp")

dta_files = list.files(path = "~/Downloads/farsp", pattern = "*.dta", full.names = TRUE)
dta_files = setNames(dta_files, dta_files)

fars = map_df(dta_files, read_dta, .id = "id") 


#### Data wrangling ------------------------------------------------------------
# Used https://osf.io/drbge/ Stata code as a guide for cleaning

# All data
# This takes awhile... go get a coffee
all_accidents = fars %>%
    # What are state and county codes/look ups?
    select(id, state, county, month, day, hour, minute, st_case, per_no, veh_no,
           per_typ, age, sex, inj_sev, death_da, death_mo, death_yr, 
           death_hr, mod_year, death_mn, death_tm, lag_hrs, lag_mins) %>%
    # CAPS used to avoid conflict with lubridate
    rename(MONTH = month, DAY = day, HOUR = hour, MINUTE = minute) %>%
    mutate_at(vars(MONTH, DAY, HOUR, MINUTE), na_if, 99) %>%
    mutate(crashtime = HOUR * 100 + MINUTE,
           YEAR = as.numeric(gsub("\\D", "", id)) - 10000,
           DATE = as.Date(paste(YEAR, MONTH, DAY, sep = "-")),
           TIME = paste(HOUR, MINUTE, sep = ":"),
           TIMESTAMP = as.POSIXct(paste(DATE, TIME), format = "%Y-%m-%d %H:%M"), 
           e420 = case_when(
               MONTH == 4 & DAY == 20 & crashtime >= 1620 & crashtime <= 2359 ~ 1,
               TRUE ~ 0),
           e420_control = case_when(
               MONTH == 4 & (DAY == 20 | DAY == 27) & crashtime >= 1620 & crashtime < 2359 ~ 1,
               TRUE ~ 0),
           d420 = case_when(
               crashtime >= 1620 & crashtime <= 2359 ~ 1,
               TRUE ~ 0),
           sex = factor(case_when(
               sex == 2 ~ "F",
               sex == 1 ~ "M",
               sex >= 8 ~ NA_character_,
               TRUE ~ NA_character_)),
           Period = factor(case_when(
               YEAR < 2004  ~ "Remote (1992-2003)",
               YEAR >= 2004 ~ "Recent (2004-2016)",
               TRUE ~ NA_character_)),
           age_group = factor(case_when(
               age <= 20 ~ "<20y",
               age <= 30 ~ "21-30y",
               age <= 40 ~ "31-40y",
               age <= 50 ~ "41-50y",
               age <= 97 ~ "51-97y",
               age == 98 | age == 99 | age == 998 ~ NA_character_,
               is.na(age) ~ NA_character_,
               TRUE ~ NA_character_))
           ) %>%
        filter(per_typ == 1, 
           !is.na(MONTH),
           !is.na(DAY))


# Daily+Time Group
# This should match 420-data.dta observations at https://osf.io/ejz28/ 
# Verify: dta_orig = read_dta("https://osf.io/ejz28/download")
# arsenal::compare(daily_accidents_time_groups, dta_orig)
daily_accidents_time_groups = all_accidents %>%
    group_by(DATE, d420) %>%
    summarize(fatalities_count = n())


# Daily+Time Group final working data
# Only use data starting in 1992
daily_accidents_time_groups = all_accidents %>%
    filter(YEAR > 1991) %>%
    group_by(DATE, d420) %>%
    summarize(fatalities_count = n())


# Daily final working data
daily_accidents = all_accidents %>%
    filter(YEAR > 1991) %>%
    group_by(DATE) %>%
    summarize(fatalities_count = n())


#### Some plotting fun ---------------------------------------------------------

# Simple annual patterns in fatalities
p1 = ggplot(daily_accidents_time_groups, aes(DATE, fatalities_count)) +
    geom_line(size = 0.25) +
    labs(title = "Daily Fatality Count, 1992-2016",
         x = "Date",
         y = "Fatalities") +
    theme_bw()

p1

plotly::ggplotly(p1)


# Hack to get rid of Feb 29s and get day of year number w/o counting leap days
hack_daily_accidents = daily_accidents %>%
        filter(substr(DATE, 6, 10) != "02-29") %>%
        mutate(DOY = case_when(
            leap_year(DATE) ~ yday(as.Date(paste("1993", 
                month(DATE), day(DATE), sep = "-"))),
            !leap_year(DATE) ~ yday(DATE),
            TRUE ~ NA_real_))
           
# Plot mean fatalities and 95% CIs by day of year 
p2 = ggplot(hack_daily_accidents, aes(DOY, fatalities_count)) +
        geom_vline(xintercept = 110, linetype = "dashed") +
        annotate("text", label = "April 20", x = 115, y = 100, size = 3, hjust = 0) +
        stat_summary(geom = "errorbar", fun.data = "mean_cl_boot", 
            size = 0.25, color = "gray70") +
        stat_summary(geom = "point", fun.y = "mean") +
        labs(title = "Daily Mean Fatality Count, 1992-2016",
             caption = "error bars are 95% bootstrapped confidence intervals",
             x = "Day of Year",
             y = "Fatalities") +
        theme_bw() +
        theme(plot.caption = element_text(hjust = 0, face = "italic"))

p2

p3 = ggplot(hack_daily_accidents, aes(DOY, fatalities_count)) +
        geom_vline(xintercept = 110, linetype = "dashed") +
        stat_summary(geom = "errorbar", fun.data = "mean_cl_boot", 
            size = 0.25, color = "gray70") +
        xlim(90.5, 120.5) +
        stat_summary(geom = "point", fun.y = "mean") +
        labs(title = "Daily Mean Fatality Count in April, 1992-2016",
             caption = "error bars are 95% bootstrapped confidence intervals",
             x = "Day of Year (month of April)",
             y = "Fatalities") +
        theme_bw() +
        theme(plot.caption = element_text(hjust = 0, face = "italic"))

gridExtra::grid.arrange(p2, p3, ncol = 1)


# Time for some #tidytuesday?
