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

# Path .zip file
zip_path <- "/Users/amandaharrison/Desktop/DAP2/final-project-fredrickson/data/usa_00014.csv.zip"

# Use read_csv to read the .zip file directly
ipums <- read_csv(zip_path, guess_max = 10000)

# Drop columns
ipums_cleaned <- ipums %>% select(-c(2:6, 8))

# Fix states
# Create a mapping of STATEFIP codes to state names
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

# Convert
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
  filter(VETSTAT == 2)

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

# Calculate the average income for veterans and non-veterans by state
average_income_by_state <- ipums_cps_cleaned %>%
  filter(VETSTAT %in% c(1, 2)) %>%
  group_by(STATENAME) %>%
  summarize(
    Average_Income_Veterans = mean(HHINCOME[VETSTAT == 2], na.rm = TRUE),
    Average_Income_NonVeterans = mean(HHINCOME[VETSTAT == 1], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(Veterans_Higher = Average_Income_Veterans > Average_Income_NonVeterans)

# Ensure the STATE column is lowercase for matching with the map data
average_income_by_state$STATENAME <- tolower(average_income_by_state$STATENAME)

# Get a map of the US states
states_map <- map_data("state")

# Merge the income data with the map data
map_data <- left_join(states_map, average_income_by_state,
  by = c("region" = "STATENAME")
)

# 3. Web scraping (including automatic data retrieval )

# Function to check if data is already downloaded and read in, otherwise scrape and save
scrape_and_save_content <- function(url, selector, file_name) {
  if (file.exists(file_name)) {
    message("Reading from local file: ", file_name)
    content <- read_lines(file_name)
  } else {
    message("Scraping content from web: ", url)
    webpage <- read_html(url)
    content <- webpage %>%
      html_element(selector) %>%
      html_text(trim = TRUE)
    write_lines(content, file_name)
  }
  return(content)
}

# Define articles and selectors
articles <- list(
  list(
    url = "https://americandisabilityactiongroup.com/are-va-benefits-being-reduced-if-you-earn-too-much/",
    selector = "#post-1896 > div > div",
    file_name = "article1.txt"
  ),
  list(
    url = "https://www.moaa.org/content/publications-and-media/news-articles/2023-news-articles/advocacy/the-va-has-no-plans-to-cut-off-wealthy-veterans.-heres-what-you-need-to-know/",
    selector = "body > div.with-toolbar > article > div > div > div.col-xl-8.news-article__col > div:nth-child(2)",
    file_name = "article2.txt"
  ),
  list(
    url = "https://www.washingtonpost.com/opinions/2023/04/03/veterans-affairs-disability-payments-overdue-update/",
    selector = "#__next > article > div.meteredContent.grid-center",
    file_name = "article3.txt"
  ),
  list(
    url = "https://hunterseven.org/vacuts/",
    selector = "body > div.elementor.elementor-2539.elementor-location-single.post-3199.post.type-post.status-publish.format-standard.has-post-thumbnail.hentry.category-news.tag-cbo.tag-disability.tag-military.tag-pact-act.tag-service-connection.tag-va-benefits.tag-veteran.tag-veteran-health > section.elementor-section.elementor-top-section.elementor-element.elementor-element-baeefbe.elementor-section-boxed.elementor-section-height-default.elementor-section-height-default > div",
    file_name = "article4.txt"
  ),
  list(
    url = "https://vabenefitattorneys.com/will-veteran-disability-benefits-be-cut-in-2024/",
    selector = "#post-4230 > div > div",
    file_name = "article5.txt"
  )
)

# Loop through articles to scrape or read from local files
content_list <- lapply(articles, function(article) {
  scrape_and_save_content(article$url, article$selector, article$file_name)
})

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
