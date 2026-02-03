penguins_clean_names <- readRDS(url("https://github.com/UEABIO/5023B/raw/refs/heads/2026/files/penguins.RDS"))

library(tidyverse)

# 3  Strings================================

# Trim whitespace on either side of a string
str_trim(" Adelie Penguin (Pygoscelis adeliae) ")

# Or just from one side
str_trim("  Adelie Penguin (Pygoscelis adeliae)  ", side = "left")

# str_squish() removes leading, trailing, and extra internal 
# whitespace, leaving only single spaces between words
str_squish("  Adelie    Penguin   (Pygoscelis   adeliae)  ")

# Truncate: shorten long strings to a specific width — handy when making plots or reports
str_trunc("Adelie Penguin (Pygoscelis adeliae)", width = 18, side = "right")

# Split a string into smaller pieces based on a separator. (e.g. separating the 
# scientific name in parentheses)
str_split("Adelie Penguin (Pygoscelis adeliae)", " ")

# Concatenate: Join pieces of text into one string.
str_c("Adelie", "Penguin", sep = "_")

# Cleaning strings with dplyr====

# Print only unique character strings in this variable
penguins_clean_names |>  
  distinct(sex)

# use mutate and case_when 
# for a statement that conditionally changes 
# the names of the values in a variable
penguins_clean_names |> 
  mutate(species = case_when(
    species == "Adelie Penguin (Pygoscelis adeliae)" ~ "Adelie",
    species == "Gentoo penguin (Pygoscelis papua)" ~ "Gentoo",
    species == "Chinstrap penguin (Pygoscelis antarctica)" ~ "Chinstrap",
    .default = as.character(species)
  )
  )

# use mutate and if_else
# for a statement that conditionally changes 
# the names of the values in a variable
penguins_clean_names |> 
  mutate(sex = if_else(
    sex == "MALE", "Male", "Female"
  )
  )

# Convert all species names to uppercase 
penguins_clean_names |>  
  mutate(species = str_to_upper(species))

# Split columns: want simpler species names but that we would 
# like to keep the latin name information, but in a separate column. To do this we are using regex
penguins_clean_names_full <- penguins_clean_names |>
  mutate(species = case_when(
    species == "Adelie" ~ "Adelie Penguin (Pygoscelis adeliae)",
    species == "Gentoo" ~ "Gentoo penguin (Pygoscelis papua)",
    species == "Chinstrap" ~ "Chinstrap penguin (Pygoscelis antarctica)",
    .default = as.character(species)
  )
  )
penguins_clean_names_full |>
  separate(
    species,
    into = c("species", "full_latin_name"),
    sep = "(?=\\()"
  ) 

# Detect a pattern
str_detect("Genus specificus", "Genus")

#  filter the species names to only those containing the pattern “papua”
# 3 possible names in species column
penguins_clean_names_full |> distinct(species)

penguins_clean_names_full |>
  filter(str_detect(species, "papua")) |>
  select(species)

# remove match for Genus (followed by a whitespace)
str_remove("Genus specificus", pattern = "Genus ")

# use str_remove to strip away brackets
penguins_clean_names_full |> 
  separate(
    species,
    into = c("species", "full_latin_name"),
    sep = "(?=\\()" # regex pattern: split before the '('
  ) |> 
  mutate(full_latin_name = str_remove_all(full_latin_name, "[\\(\\)]"))

# 4  Duplicates================================
# check for whole duplicate 
# rows in the data
penguins_clean_names |> 
  filter(duplicated(across(everything())))
sum() 

# add a few dupes to our data
penguins_demo <- penguins_clean_names |> 
  slice(1:50) |> 
  bind_rows(slice(penguins_clean_names, c(1,5,10,15,30)))

penguins_demo |> 
  filter(duplicated(across(everything())))
  sum()

# Keep only unduplicated data with !
penguins_demo |> 
  filter(!duplicated(across(everything())))

# count the number of distinct values in an R data frame using one of the following methods
penguins_clean_names |> 
  summarise(
    n = n(),
    n_distinct(individual_id)
  )

# 5  Missing data================================

# The mechanism of missingness matters:
#   MCAR (Missing Completely At Random): safe to remove or ignore
#   MAR (Missing At Random): depends on observed variables
#   MNAR (Missing Not At Random): missingness related to the unobserved value itself
# Different mechanisms require different analytical approaches.
  
penguins_clean_names_full |> 
  group_by(species) |> 
  summarise(mean = mean(body_mass_g))

# Find Missing values
summary(penguins_clean_names_full)
#also
# library(skimr)
# skimr::skim(penguins_clean_names)    
# library(naniar)
# naniar::vis_miss(penguins_clean_names)
# naniar::gg_miss_upset(penguins_clean_names)

# return all rows with a missing variable
penguins_clean_names_full |> 
  filter(if_any(everything(), is.na)) |>
  select(culmen_length_mm, culmen_depth_mm, flipper_length_mm, 
         sex, delta_15_n_o_oo, delta_13_c_o_oo,comments,
         everything()) # reorder columns

# specify we just want to look at a specific columns NA
penguins_clean_names_full |> 
  filter(if_any(culmen_length_mm, is.na))  # reorder columns

# Remove missing values
# Drop all rows with any NA (drop_na()):  simple but can remove lots - the nuclear option
# Drop NAs in specific columns only.
# Keep rows, use na.rm = TRUE             inside summary functions (least destructive).

penguins_clean_names_full |> 
  drop_na()

#  It’s often better to drop NAs temporarily for specific analyses or calculations.

# might still lose useful information from other columns. If you overwrite the dataset permanently:
penguins_clean_names_full <- penguins_clean_names |> drop_na(body_mass_g)

# more cautious approach is to handle missing data within summary or calculation functions, 
# allows you to keep missing values in the dataset but ignore them only when performing calculations.
penguins_clean_names_full |> 
  group_by(species) |> 
  summarise(
    mean_body_mass = mean(body_mass_g, na.rm = T)
  )

# 6  Dates================================
library(lubridate)

#  reformat dates written in different ways to YYYY-MM-DD format.
date("2017-10-11T14:02:00")
dmy("11 October 2020")
mdy("10/11/2020")

# the following date format isn’t converted correctly when we try to convert it to a date.
df <- tibble(
  date = c("X2020.01.22",
           "X2020.01.22",
           "X2020.01.22",
           "X2020.01.22")
)

df |> 
  mutate(
    date = as_date(date)
  )

# use % to be more explicit about what information is in each part of our date column, 
# specifying where the 4-digit year (%Y), 2-digit month (%m) and 2 digit day (%d) are within each string.
df |> 
  mutate(
    date = as_date(date, format = "X%Y.%m.%d")
  )

# extract certain elements of a longer date-time value for summarising, filtering, or plotting data.
year("2017-11-28T14:02:00")
month("2017-11-28T14:02:00")
week("2017-11-28T14:02:00")
day("2017-11-28T14:02:00")

# Excel stores dates internally as serial numbers
# The janitor package has a handy function janitor::excel_numeric_to_date() to deal with this.

library(janitor)

excel_numeric_to_date(42370)

# use the mutate function from dplyr to create a new variable called date_egg_proper 
# based on the output of converting the characters in date_egg to date format
penguins_clean_names_full <- penguins_clean_names_full |>
  mutate(date_egg_proper = lubridate::dmy(date_egg))

# ??? Got warning/didn't work so try
penguins_clean_names_full <- penguins_clean_names_full |>
  mutate(date_egg_proper = lubridate::date(date_egg))

# Calculations with dates
penguins_clean_names_full |> 
  summarise(min_date=min(date_egg_proper),
            max_date=max(date_egg_proper))

penguins_clean_names_full <- penguins_clean_names_full |> 
  mutate(year = lubridate::year(date_egg_proper))

# 7  Check Data Consistency
# 7  Check Data Consistency================================

# Just because a value is an outlier doesn’t mean it’s wrong. 
# Similarly, a value within the normal range could still be an error if it’s inconsistent 
# with other measurements. 
# You need biological knowledge AND cross-variable checks to distinguish errors from legitimate variation.

# Check ranges of all numeric variables at once
penguins_clean_names_full |> 
  summarise(across(where(is.numeric), 
                   list(min = ~min(., na.rm = TRUE),
                        max = ~max(., na.rm = TRUE))))

# Check body mass range
penguins_clean_names_full |> 
  summarise(
    min_mass = min(body_mass_g, na.rm = TRUE),
    max_mass = max(body_mass_g, na.rm = TRUE)
  )

# Check for negative values (impossible for mass, length measurements)
penguins_clean_names_full |> 
  filter(if_any(c(body_mass_g, flipper_length_mm, 
                  culmen_length_mm, culmen_depth_mm), 
                ~ . < 0))

# Check for zero or negative values where zero doesn't make biological sense
penguins_clean_names_full |> 
  filter(body_mass_g <= 0)
# A body mass of exactly 0g is impossible - this likely represents a 
# missing measurement that was coded as 0 instead of NA.

# Species-specific checks

# Body mass ranges by species
penguins_clean_names_full |> 
  group_by(species) |> 
  summarise(
    min_mass = min(body_mass_g, na.rm = TRUE),
    max_mass = max(body_mass_g, na.rm = TRUE),
    mean_mass = mean(body_mass_g, na.rm = TRUE)
  )

# Find Adelie penguins with Gentoo-sized body mass
penguins_clean_names_full |> 
  filter(species == "Adelie Penguin (Pygoscelis adeliae)", body_mass_g > 4750)

# Visual inspection
penguins_clean_names_full |> 
  ggplot(aes(x = species, y = body_mass_g)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.3) +
  labs(
    x = "",
    y = "Body mass (g)"
  ) +
  theme_minimal()+
  coord_flip()

# Cross-variable checks: Expected correlations

# relationships between variables can reveal errors. Body size measurements should correlate positively

#| label: fig-mass-flipper
#| fig-cap: "Body mass should generally increase with flipper length within species. Points far from the trend may indicate measurement errors."

penguins_clean_names_full |> 
  ggplot(aes(x = flipper_length_mm, y = body_mass_g, color = species)) +
  geom_point(alpha = 0.6) +
  labs(
    x = "Flipper length (mm)",
    y = "Body mass (g)",
  ) +
  theme_minimal()

# Find penguins with large flippers but low body mass
penguins_clean_names_full |> 
  filter(flipper_length_mm > 210, body_mass_g < 3500) |> 
  select(species, sex, flipper_length_mm, body_mass_g, island)

# Find penguins with small flippers but high body mass
penguins_clean_names_full |> 
  filter(flipper_length_mm < 185, body_mass_g > 4500) |> 
  select(species, sex, flipper_length_mm, body_mass_g, island)

# Cross-variable checks: Biological impossibilities
# Males cannot lay eggs
penguins_clean_names_full |> 
  filter(sex == "MALE", !is.na(date_egg)) |> 
  select(species, sex, date_egg, body_mass_g, island)

# Cross-variable checks: Spatial consistency
# Location variables should match known species distributions

# Check which species appear on which islands
penguins_clean_names_full |> 
  count(species, island) |> 
  pivot_wider(names_from = island, values_from = n, values_fill = 0)

# Flagging suspicious values

# Create flags for different types of potential issues
penguins_flagged <- penguins_clean_names_full |> 
  mutate(
    # Single-variable flags
    flag_impossible = case_when(
      body_mass_g <= 0 ~ "negative_or_zero_mass",
      flipper_length_mm <= 0 ~ "negative_or_zero_flipper",
      TRUE ~ NA_character_
    ),
    flag_implausible = case_when(
      body_mass_g < 2000 ~ "suspiciously_light",
      body_mass_g > 7000 ~ "suspiciously_heavy",
      TRUE ~ NA_character_
    ),
    
    # Cross-variable flags
    flag_species_size = case_when(
      species == "Adelie" & body_mass_g > 5000 ~ "Adelie_too_heavy",
      species == "Gentoo" & body_mass_g < 4000 ~ "Gentoo_too_light",
      TRUE ~ NA_character_
    ),
    # Any flag present?
    any_flag = !is.na(flag_impossible) | !is.na(flag_implausible) | 
      !is.na(flag_species_size) 
  )

# Summarize flagged observations
penguins_flagged |> 
  summarise(
    n_impossible = sum(!is.na(flag_impossible)),
    n_implausible = sum(!is.na(flag_implausible)),
    n_species_size = sum(!is.na(flag_species_size)),
    total_flagged = sum(any_flag)
  )