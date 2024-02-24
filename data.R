# Amanda Fredrickson
# Github ID: mandyfreddy
# DAP2 Final Project
# Preliminary Analysis of Veteran Disability

## data.R

# One from the web automatically using APIs or web scraping
# All processing here in the code - including merging and reshaping
# Any auto data retrieval will have an option to toggle off accessing the web if data is already donwloade

# 1. IPUMS data

# Libraries
library(dplyr)

# Load
ipums <-
  read.csv("/Users/amandaharrison/Desktop/DAP2/final-project-fredrickson/data/usa_00012.csv",
    stringsAsFactors = FALSE
  )

# Drop columns
ipums_cleaned <- ipums %>% select(-c(2:7, 9:12))

# Drop extra years
ipums_cleaned <- ipums_cleaned[!(ipums_cleaned$YEAR %in% c(2006, 2021, 2022)), ]
# Now it is just 2011 and 2016 data, it was a bit slow to load before

# Fix states
# Ensure the STATEFIP values are numeric
ipums_cleaned$STATEFIP <- as.numeric(as.character(ipums_cleaned$STATEFIP))

# Create a named vector with numeric state codes as names
state_codes <- c(
  "1" = "Alabama", "2" = "Alaska", "4" = "Arizona", "5" = "Arkansas",
  "6" = "California", "8" = "Colorado", "9" = "Connecticut", "10" = "Delaware",
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
  "54" = "West Virginia", "55" = "Wisconsin", "56" = "Wyoming",
  "61" = "Maine-New Hampshire-Vermont", "62" = "Massachusetts-Rhode Island",
  "63" = "Minnesota-Iowa-Missouri-Kansas-Nebraska-S.Dakota-N.Dakota",
  "64" = "Maryland-Delaware", "65" = "Montana-Idaho-Wyoming", "66" = "Utah-Nevada",
  "67" = "Arizona-New Mexico", "68" = "Alaska-Hawaii", "72" = "Puerto Rico",
  "97" = "Military/Mil. Reservation", "99" = "State not identified"
)

# Replace the numeric STATEFIP values with the corresponding state names
ipums_cleaned$STATEFIP <-
  sapply(ipums_cleaned$STATEFIP, function(x) state_codes[as.character(x)])

# 2. IPUMS CPS data

# Libraries
library(readr)
library(ipumsr)

# Load
ddi <-
  read_ipums_ddi("/Users/amandaharrison/Desktop/DAP2/final-project-fredrickson/data/cps_00002.xml")
ipums_cps <- read_ipums_micro(ddi)

# Clean
# Drop columns
ipums_cps_cleaned <- ipums_cps %>% select(-c(2, 3))

# Drop extra years
ipums_cps_cleaned <- ipums_cps_cleaned[!(ipums_cps_cleaned$YEAR %in%
  c(2019, 2020, 2021, 2024)), ]
# Now it is just 2022 and 2023 data to perhaps show trends differently

# Replace the numeric STATEFIP values with the corresponding state names
ipums_cps_cleaned$STATEFIP <- as.numeric(as.character(ipums_cps_cleaned$STATEFIP))

ipums_cps_cleaned$STATEFIP <-
  sapply(ipums_cps_cleaned$STATEFIP, function(x) state_codes[as.character(x)])

# Overall plot cleaning follows

# Cleaning for plot 1
# Omit
ipums_cleaned_no_na <- na.omit(ipums_cleaned)

# Cleaning for the plots (rating and age)
ipums_cleaned_no_na <- ipums_cleaned_no_na %>%
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

wage_summary <- ipums_cleaned_no_na %>%
  group_by(AGE_GROUP, VETSTAT, DISABILITY_SIMPLE) %>%
  summarize(AVG_INCWAGE = mean(INCWAGE, na.rm = TRUE))

# Cleaning for plot 2
# Filter the data to include only veterans
veterans_data <- ipums_cleaned_no_na %>%
  filter(VETSTAT == 2)  # Assuming '2' represents veterans

# Calculate the proportion in the labor force for each age group and disability rating
lfp_by_age_disability <- veterans_data %>%
  group_by(AGE_GROUP, DISABILITY_SIMPLE) %>%
  summarize(Proportion_In_LFP = mean(as.numeric(LABFORCE) == 2, na.rm = TRUE)) %>%
  ungroup()

# Calculate the proportion in the labor force for each age group and veteran status
lfp_by_age_vet <- ipums_cleaned_no_na %>%
  group_by(AGE_GROUP, VETSTAT) %>%
  summarize(Proportion_In_LFP = mean(as.numeric(LABFORCE) == 2, na.rm = TRUE)) %>%
  ungroup()

# Cleaning for plot 3

library(dplyr)
library(ggplot2)

# Convert VDISRATE to numeric and filter for veterans
veterans_data_cps <- ipums_cps_cleaned %>%
  mutate(VDISRATE = as.numeric(as.character(VDISRATE))) %>%
  filter(VETSTAT == 2)  # Assuming '2' represents veterans

# Create AGE_GROUP, ensuring that AGE is not NA
veterans_data_cps <- veterans_data_cps %>%
  filter(!is.na(AGE)) %>%
  mutate(
    AGE_GROUP = cut(AGE,
                    breaks = c(18, 30, 40, 50, 60, 70, Inf),
                    right = FALSE,
                    labels = c("18-29", "30-39", "40-49", "50-59", "60-69", "70+"))
  )

# Recode VDISRATE into DISABILITY_SIMPLE, excluding "Not in Universe"
veterans_data_cps <- veterans_data_cps %>%
  mutate(
    DISABILITY_SIMPLE = case_when(
      VDISRATE == 10 ~ "0%",
      VDISRATE %in% c(20, 21, 22, 23) ~ "1-29%",
      VDISRATE %in% c(29, 30, 31) ~ "30-40%",
      VDISRATE == 39 ~ "50-60%",
      VDISRATE %in% c(40, 41, 42) ~ "50+%",
      VDISRATE %in% c(43, 44, 45, 46, 47) ~ "70%+",
      VDISRATE == 56 ~ "100%",
      VDISRATE %in% c(996, 997, 998) ~ "No response",
      VDISRATE == 999 ~ "Not in Universe",
      TRUE ~ "Unknown"  # Catch-all for any other values
    )
  ) %>%
  filter(DISABILITY_SIMPLE != "Not in Universe" & DISABILITY_SIMPLE != "Unknown")

# Calculate the average family income for non-veterans by age group
non_vet_avg_income <- ipums_cps_cleaned %>%
  filter(VETSTAT == 1, !is.na(AGE)) %>%
  mutate(
    AGE_GROUP = cut(AGE,
                    breaks = c(18, 30, 40, 50, 60, 70, Inf),
                    right = FALSE,
                    labels = c("18-29", "30-39", "40-49", "50-59", "60-69", "70+"))
  ) %>%
  group_by(AGE_GROUP) %>%
  summarise(
    Avg_NonVet_FAMINC = mean(FAMINC, na.rm = TRUE),
    .groups = 'drop'
  )

# Calculate the average family income by age group and disability status for veterans
average_income_by_disability <- veterans_data_cps %>%
  group_by(AGE_GROUP, DISABILITY_SIMPLE) %>%
  summarise(
    Avg_FAMINC = mean(FAMINC, na.rm = TRUE),
    .groups = 'drop'
  )

# Generate the plot with the average non-veteran income line included
ggplot(average_income_by_disability, aes(x = DISABILITY_SIMPLE, y = Avg_FAMINC, fill = DISABILITY_SIMPLE)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_hline(data = non_vet_avg_income, aes(yintercept = Avg_NonVet_FAMINC), color = "red", size = 1) +
  facet_wrap(~ AGE_GROUP, scales = "free_y") +
  labs(
    title = "Average Family Income by Disability Rating and Age Group",
    x = "Disability Rating",
    y = "Average Family Income",
    fill = "Disability Rating"
  ) +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  ) +
  guides(fill = guide_legend(title = "Disability Rating"))

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

# Comparing income between veterans and non-veterans
aggregate(INCTOT ~ VETSTAT, data = ipums_cleaned, FUN = mean)

# Labor force participation by veteran status
table(ipums_cps$VETSTAT, ipums_cps$LABFORCE)

with(ipums_cleaned, table(STATEFIP, VETSTAT))
with(ipums_cps, table(LABFORCE, VDISRATE))
