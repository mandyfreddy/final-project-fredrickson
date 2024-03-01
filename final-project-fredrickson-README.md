# Preliminary Analysis of Veteran Disability
# Amanda Fredrickson
# Github ID: mandyfreddy

## Date Created: 20 February 2024
## Date Completed: 29 February 2024

## R Version: 4.3.1

## Required Packages
- dplyr 
- readr 
- ipumsr 
- ggplot2 
- rvest 
- shiny 
- tidytext 
- stringr
- maps 
- sf 

## Project Description
This project conducts a preliminary analysis of veteran disability, including economic outcomes and labor force participation. Various datasets from IPUMS and web scraping are used to perform the analysis.

## Data Source and Description
- The IPUMS data is sourced from [IPUMS USA](https://usa.ipums.org/usa/) and [IPUMS CPS](https://cps.ipums.org/cps/). These datasets provide comprehensive data on veterans' demographic and economic characteristics.
- The web scraping code retrieves articles from specified URLs for sentiment analysis and text processing.

## Data Files
- `usa_00014.csv.zip` - Contains IPUMS USA data, used in `data.R` for demographic analysis. Unzip manually or in R to access the .csv itself. 
- `cps_00003.xml` - Contains IPUMS CPS metadata, used in `data.R` for economic analysis.

## File Descriptions and Execution Order
1. `data.R`: Prepares and cleans the IPUMS datasets for analysis. The cleaned datasets are then saved in the specified data directory.
   - Modify `zip_path` and `data_dir` with the path to your local data storage location.
2. `model.R`: Runs regression models on the cleaned data to understand the impact of various factors on income.
   - Ensure the cleaned data from `data.R` is accessible at the specified path.
3. `textprocess.R`: Processes the scraped web content for sentiment analysis.
   - The `content_list` should be populated with the actual content if not scraping anew.
4. `staticplot.R`: Generates static plots from the cleaned data and saves them as images.
   - Modify file paths in `ggsave` to match your local directory for images.
5. `shinyapp.R`: An interactive Shiny app for exploring the data. It can be run locally.
   - No changes are needed unless you want to customize the app's functionality.

## Replication Instructions
To replicate this analysis, download the IPUMS datasets as described above and place them in your local `data` directory. Update all path variables (`zip_path`, `data_dir`, etc.) in the code to match your directory structure. Install and load all required R packages with the versions specified.

## Description of Plots
- 'plot_1.png' is a depiction of the proportion of labor force participation between veterans and non-veterans within different age groups.
- 'plot_2.png' is a bar chart that shows the proportion of veterans in the labor force across different age groups (18-29, 30-39, etc.), broken down by their disability rating (0, 1-60, 70+, No rating, Not reported). Each age group has five bars representing the proportion of labor force participation for each category of disability rating.
- 'plot_3.png' is a map of the United States is color-coded to show whether veterans or non-veterans earn more on average within each state.
- 'plot_4.png' is a sentiment analysis of text content of five articles using the bing lexicon. Most articles show a positive sentiment score, with one article, article 4, being very negative compared to the others
- 'plot_5.png' is a sentiment analysis plot showing the ratio of positive to negative words in each article, meant to depict the tone of each article around the topic. 

## Additional Notes
This project is a part of my DAP2 Final Project for analyzing veteran disability data. 
