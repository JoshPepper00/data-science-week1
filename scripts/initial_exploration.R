# Week 1: Initial Data Exploration ====
# Author: Josh Pepper
# Date: 29/01/2026

# Load packages ====
library(tidyverse)
library(here)
library(naniar)
library(janitor)
library(skimr)
# Load data ====
mosquito_egg_raw <- read_csv(here("data", "mosquito_egg_data.csv"),
                             name_repair = janitor::make_clean_names)

# Basic overview ====
glimpse(mosquito_egg_raw)
summary(mosquito_egg_raw)
skim(mosquito_egg_raw)

# React table====
# view interactive table of data
view(mosquito_egg_raw)


# Counts by site and treatment====

mosquito_egg_raw |> 
  group_by(site, treatment) |> 
  summarise(n = n())

# Observations ====
# Your observations (add as comments below):
# - What biological system is this?
#   female mosquitos
# - What's being measured?
#   biometric data and egg hatching success at different sites with different doses
# - How many observations?
#   205 rows, 9 variables
# - Anything surprising?
#   Values for eggs_hatched when eggs_laid is 0
# - Any obvious problems?
#   Several NA values in body_mass_mg, eggs_laid, eggs_hatched, collector
#   Negative body_mass_mg values
#   Inconsistent underscore use and capitalization for site, treatment
#   Inconsistent spelling of collector names

# FIX 1: inconsistent underscore use and capitalization ====

# Show the problem:
mosquito_egg_raw |>  
  distinct(site)

mosquito_egg_raw |>  
  distinct(treatment)

# Fix it:
mosquito_egg_data_step1 <- mosquito_egg_raw |>
  # change the site labels to make them consistent
  mutate(site = case_when(
    site == "Site A" ~ "Site_A",
    site == "Site-A" ~ "Site_A",
    site == "site_a" ~ "Site_A",
    site == "Site B" ~ "Site_B",
    site == "site_b" ~ "Site_B",
    site == "Site-B" ~ "Site_B",
    site == "Site C" ~ "Site_C",
    site == "site_c" ~ "Site_C",
    site == "Site-C" ~ "Site_C",
    .default = as.character(site)
  )
  )

mosquito_egg_data_step1_next <- mosquito_egg_data_step1 |>
  # change the treatment labels to make them consistent
  mutate(treatment = case_when(
    treatment == "control" ~ "Control",
    treatment == "CONTROL" ~ "Control",
    treatment == "LOW_DOSE" ~ "Low_dose",
    treatment == "low_dose" ~ "Low_dose",
    treatment == "MEDIUM_DOSE" ~ "Medium_dose",
    treatment == "medium_dose" ~ "Medium_dose",
    treatment == "HIGH_DOSE" ~ "High_dose",
    treatment == "high_dose" ~ "High_dose",
    .default = as.character(treatment)
  )
  )
  
# Verify it worked:
mosquito_egg_data_step1_next |>  
  distinct(site) 

mosquito_egg_data_step1_next |>
  distinct(treatment)

# What changed and why it matters:
# [2-3 sentences explaining consequences]
# Has Has allowed me to consolidate my data - there are now three sites instead of 12 and 4 treatment 
# values instead of 12.
  
  
# FIX 2: [Issue description]  ====

# Show the problem:
# [Code]

# Fix it:
mosquito_egg_data_step2 <- mosquito_egg_data_step1_next |>
  filter(!if_any(c(eggs_laid, eggs_hatched), is.na))
  
# Verify it worked:
mosquito_egg_data_step3 <- mosquito_egg_data_step2 |>
  filter(if_any(eggs_laid, is.na) & if_any(eggs_hatched, is.na))

# What changed and why it matters:
# [2-3 sentences]
#