# Amanda Fredrickson
# Github ID: mandyfreddy
# DAP2 Final Project
# Preliminary Analysis of Veteran Disability

## data.R

# One from the web automatically using APIs or web scraping
# All processing here in the code - including merging and reshaping
# Any auto data retrieval will have an option to toggle off accessing the web 
# if data is already downloaded

# 1. IPUMS data

# Libraries
library(dplyr)
library(readr)

# The path to your .zip file
zip_path <- "/Users/amandaharrison/Desktop/DAP2/final-project-fredrickson/data/usa_00014.csv.zip"

# Use read_csv to read the .zip file directly
ipums <- read_csv(zip_path, guess_max = 10000)

# Drop columns
ipums_cleaned <- ipums %>% select(-c(2:6, 8))

# Fix states
# Create a mapping of STATEFIP codes to state names based on the codebook provided
state_codes <- c(
  "01" = "Alabama", "02" = "Alaska", "04" = "Arizona", "05" = "Arkansas",
  "06" = "California", "08" = "Colorado", "09" = "Connecticut", "10" = "Delaware",
  "11" = "District of Columbia", "12" = "Florida", "13" = "Georgia", "15" = "Hawaii",
  "16" = "Idaho", "17" = "Illinois", "18" = "Indiana", "19" = "Iowa",
  "20" = "Kansas", "21" = "Kentucky", "22" = "Louisiana", "23" = "Maine",
  "24" = "Maryland", "25" = "Massachusetts", "26" = "Michigan", "27" = "Minnesota",
  "28" = "Mississippi", "29" = "Missouri", "30" = "Montana", "31" = "Nebraska",
  "32" = "Nevada", "33" = "New Hampshire", "34" = "New Jersey", "35" = "New Mexico",
  "36" = "New York", "37" = "North Carolina", "38" = "North Dakota", "39" = "Ohio",
  "40" = "Oklahoma", "41" = "Oregon", "42" = "Pennsylvania", "44" = "Rhode Island",
  "45" = "South Carolina", "46" = "South Dakota", "47" = "Tennessee", "48" = "Texas",
  "49" = "Utah", "50" = "Vermont", "51" = "Virginia", "53" = "Washington",
  "54" = "West Virginia", "55" = "Wisconsin", "56" = "Wyoming"
)

# Assuming your STATEFIP column is character and has leading zeros
# If it is numeric, convert it to a character with leading zeros first
ipums_cleaned$STATEFIP <- sprintf("%02d", as.integer(ipums_cleaned$STATEFIP))

# Create the STATENAME column by mapping the STATEFIP codes to state names
ipums_cleaned <- ipums_cleaned %>%
  mutate(STATENAME = state_codes[STATEFIP])

# 2. IPUMS CPS data

# Libraries
library(readr)
library(ipumsr)

# Load
ddi <-
  read_ipums_ddi("/Users/amandaharrison/Desktop/DAP2/final-project-fredrickson/data/cps_00003.xml")
ipums_cps <- read_ipums_micro(ddi)

# Clean
# Drop columns
ipums_cps_cleaned <- ipums_cps %>% select(-c(2:6, 10:13))

# States
ipums_cps_cleaned$STATEFIP <- sprintf("%02d", as.integer(ipums_cps_cleaned$STATEFIP))

# Create the STATENAME column by mapping the STATEFIP codes to state names
ipums_cps_cleaned <- ipums_cps_cleaned %>%
  mutate(STATENAME = state_codes[STATEFIP])

# Overall plot cleaning follows
# Cleaning for plot 1

# Cleaning for the plots (rating and age)
ipums_cleaned <- ipums_cleaned %>%
  mutate(
    AGE_GROUP = cut(AGE,
      breaks = c(18, 30, 40, 50, 60, 70, Inf),
      right = FALSE, labels = c("18-29", "30-39", "40-49", "50-59", "60-69", "70+")
    ),
    DISABILITY_SIMPLE = case_when(
      VETDISAB == 1 ~ "No rating",
      VETDISAB == 2 ~ "0%",
      VETDISAB %in% 3:5 ~ "1-60%",
      VETDISAB == 6 ~ "70%+",
      VETDISAB == 9 ~ "Not reported",
      TRUE ~ "N/A"
    )
  )

# Calculate the proportion in the labor force for each age group and veteran status
lfp_by_age_vet <- ipums_cleaned %>%
  group_by(AGE_GROUP, VETSTAT) %>%
  summarize(Proportion_In_LFP = mean(as.numeric(LABFORCE) == 2, na.rm = TRUE)) %>%
  ungroup()

wage_summary <- ipums_cleaned %>%
  group_by(AGE_GROUP, VETSTAT, DISABILITY_SIMPLE) %>%
  summarize(AVG_INCWAGE = mean(INCWAGE, na.rm = TRUE))

# Cleaning for plot 2

# Filter the data to include only veterans
veterans_data <- ipums_cleaned %>%
  filter(VETSTAT == 2) # Assuming '2' represents veterans

# Calculate the proportion in the labor force for each age group and disability rating
lfp_by_age_disability <- veterans_data %>%
  group_by(AGE_GROUP, DISABILITY_SIMPLE) %>%
  summarize(Proportion_In_LFP = mean(as.numeric(LABFORCE) == 2, na.rm = TRUE)) %>%
  ungroup()

lfp_by_age_disability_filtered <- lfp_by_age_disability %>% 
  filter(!is.na(AGE_GROUP))

# Cleaning for plot 3

library(ggplot2)
library(dplyr)
library(maps)

# First, calculate the average income for veterans and non-veterans by state
average_income_by_state <- ipums_cps_cleaned %>%
  filter(VETSTAT %in% c(1, 2)) %>% # Assuming 1 is non-veteran and 2 is veteran
  group_by(STATENAME) %>%
  summarize(
    Average_Income_Veterans = mean(HHINCOME[VETSTAT == 2], na.rm = TRUE),
    Average_Income_NonVeterans = mean(HHINCOME[VETSTAT == 1], na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(Veterans_Higher = Average_Income_Veterans > Average_Income_NonVeterans)

# Ensure the STATE column is lowercase for matching with the map data
average_income_by_state$STATENAME <- tolower(average_income_by_state$STATENAME)

# Get a map of the US states
states_map <- map_data("state")

# Merge the income data with the map data
map_data <- left_join(states_map, average_income_by_state, by = c("region" = "STATENAME"))

# 3. APIs or web scraping automatic data retrieval (for further text processing)

# need to do this

# Basic visualizations for my understanding and for the writeup

# Basic structure of the data
str(ipums_cleaned)
str(ipums_cps)

# Summary of the data
summary(ipums_cleaned)
summary(ipums_cps)

# First few rows of the data
head(ipums_cleaned)
head(ipums_cps)
