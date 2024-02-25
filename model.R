# Amanda Fredrickson
# Github ID: mandyfreddy
# DAP2 Final Project
# Preliminary Analysis of Veteran Disability

## model.R

library(dplyr)
library(ggplot2)

# Preliminary regression model of ipums_cleaned
model <- lm(INCTOT ~ AGE + LABFORCE + VETSTAT + AGE_GROUP + DISABILITY_SIMPLE, 
            data = ipums_cleaned)
summary(model)

# Linear regression with INCTOT as the dependent variable
model_2 <- lm(INCTOT ~ DISABILITY_SIMPLE + AGE, data = ipums_cleaned)
summary(model_2)

# Now let's do something simple with ipums_cps_cleaned

# Linear regression for ipums_cps_cleaned with HHINCOME
veteran_hhincome_model <- lm(HHINCOME ~ VETSTAT, data = ipums_cps_cleaned)
summary(veteran_hhincome_model)

# Linear regression for ipums_cps_cleaned with FAMINC
veteran_faminc_model <- lm(FAMINC ~ VETSTAT, data = ipums_cps_cleaned)
summary(veteran_faminc_model)
