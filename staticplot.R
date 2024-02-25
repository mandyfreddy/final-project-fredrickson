# Amanda Fredrickson
# Github ID: mandyfreddy
# DAP2 Final Project
# Preliminary Analysis of Veteran Disability

# Two static ggplots

library(ggplot2)

# Plot 1
# Labor Force Participation by Age Group and Veteran Status
ggplot(data = lfp_by_age_vet, aes(
  x = AGE_GROUP, y = Proportion_In_LFP,
  fill = as.factor(VETSTAT)
)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Labor Force Participation by Age Group and Veteran Status",
    x = "Age Group",
    y = "Proportion in Labor Force",
    fill = "Veteran Status"
  ) +
  scale_fill_manual(
    values = c("1" = "red", "2" = "blue"),
    labels = c("Non-veteran", "Veteran")
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Plot 2
# Labor force participation within veterans by age group and disability rating
# Disclaimer that I thought it was important to keep not reported
ggplot(data = lfp_by_age_disability, aes(
  x = AGE_GROUP, y = Proportion_In_LFP,
  fill = DISABILITY_SIMPLE
)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Labor Force Participation Within Veterans by Age Group and Disability Rating",
    x = "Age Group",
    y = "Proportion in Labor Force",
    fill = "Disability Rating"
  ) +
  scale_fill_brewer(palette = "Set1") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )
