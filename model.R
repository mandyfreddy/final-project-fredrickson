# Amanda Fredrickson
# Github ID: mandyfreddy
# DAP2 Final Project
# Preliminary Analysis of Veteran Disability

## model.R

library(dplyr)
library(ggplot2)

# Linear regression for ipums_cps_cleaned with HHINCOME
veteran_hhincome_model <- lm(HHINCOME ~ VETSTAT, data = ipums_cps_cleaned)
summary(veteran_hhincome_model)
