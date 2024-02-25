# Amanda Fredrickson
# Github ID: mandyfreddy
# DAP2 Final Project
# Preliminary Analysis of Veteran Disability

# staticplot.R

library(ggplot2)

# Plot 1
# Labor Force Participation by Age Group and Veteran Status
plot_1 <-ggplot(data = lfp_by_age_vet, aes(
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

ggsave(filename = 
         "/Users/amandaharrison/Desktop/DAP2/final-project-fredrickson/images/plot_1.png", 
       plot = plot_1, width = 10, height = 8, units = "in")

# Plot 2
# Labor force participation within veterans by age group and disability rating
# Disclaimer that I thought it was important to keep not reported

plot_2 <- ggplot(data = lfp_by_age_disability_filtered, aes(
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

ggsave(filename = 
         "/Users/amandaharrison/Desktop/DAP2/final-project-fredrickson/images/plot_2.png", 
       plot = plot_2, width = 10, height = 8, units = "in")

# Plot 3 because it was interesting

library(sf)
library(maps)

# Plot the map
plot_3 <- ggplot(map_data, aes(x = long, y = lat, group = group, fill = Veterans_Higher)) +
  geom_polygon() +
  scale_fill_manual(values = c("blue", "red"),
                    labels = c("Non-Veterans Earn More", "Veterans Earn More"),
                    name = "Income Comparison") +
  labs(title = "Do Veterans Earn More than Non-Veterans on Average by State?") +
  coord_fixed(1.3) +
  theme_void()

ggsave(filename = 
         "/Users/amandaharrison/Desktop/DAP2/final-project-fredrickson/images/plot_3.png", 
       plot = plot_3, width = 10, height = 8, units = "in")
