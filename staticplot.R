# Amanda Fredrickson
# Github ID: mandyfreddy
# DAP2 Final Project
# Preliminary Analysis of Veteran Disability

# Two static ggplots

library(ggplot2)

# Plot 1
# Labor Force Participation by Age Group and Veteran Status
ggplot(data = lfp_by_age_vet, aes(x = AGE_GROUP, y = Proportion_In_LFP, 
                                  fill = as.factor(VETSTAT))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Labor Force Participation by Age Group and Veteran Status",
    x = "Age Group",
    y = "Proportion in Labor Force",
    fill = "Veteran Status"
  ) +
  scale_fill_manual(values = c("1" = "red", "2" = "blue"),
                    labels = c("Non-veteran", "Veteran")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Potential plot 2
# Plot for wage income
ggplot(data = ipums_cleaned_no_na, aes(x = AGE_GROUP, y = INCWAGE, 
                                       fill = as.factor(VETSTAT))) +
  geom_boxplot() +
  labs(
    title = "Wage Income by Age Group and Veteran Status",
    x = "Age Group",
    y = "Wage Income",
    fill = "Veteran Status"
  ) +
  scale_fill_manual(values = c("1" = "red", "2" = "blue"),
                    labels = c("Non-veteran", "Veteran")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Plot 2
# Labor force participation within veterans by age group and disability rating
# Disclaimer that I thought it was important to keep not reported
ggplot(data = lfp_by_age_disability, aes(x = AGE_GROUP, y = Proportion_In_LFP, 
                                         fill = DISABILITY_SIMPLE)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Labor Force Participation Within Veterans by Age Group and Disability Rating",
    x = "Age Group",
    y = "Proportion in Labor Force",
    fill = "Disability Rating"
  ) +
  scale_fill_brewer(palette = "Set1") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")

# Plot 3
# Short description
# Create a plot to compare the average family income for disability groups
ggplot(average_income_by_disability, aes(x = Disability_Group, y = Avg_FAMINC, 
                                         fill = Disability_Group)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_hline(data = average_income_non_disabled, aes(yintercept = Avg_FAMINC_Reference), 
             color = "blue", linetype = "dashed", size = 1) +
  geom_hline(data = average_income_non_veterans, aes(yintercept = Avg_FAMINC_NonVet), 
             color = "red", linetype = "dotted", size = 1) +
  facet_wrap(~ AGE_GROUP) +
  labs(
    title = "Average Family Income by Age Group and Disability Group",
    x = "Disability Group",
    y = "Average Family Income",
    fill = "Disability Group"
  ) +
  scale_fill_brewer(palette = "Set1") +
  theme_minimal() +
  theme(legend.position = "bottom")
