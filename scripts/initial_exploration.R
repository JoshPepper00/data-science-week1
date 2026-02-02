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