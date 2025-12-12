library(ggplot2)
library(dplyr)

descriptive_means_df <- descriptive_means_df %>%
  mutate(Gender_Label = factor(Gender, levels = 1:2, labels = c("Male", "Female")))

sugar_trend_plot <- ggplot(descriptive_means_df, 
                           aes(x = Time_Period, y = Mean_Sugar_Intake, 
                               group = Gender_Label, color = Gender_Label)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = Mean_Sugar_Intake - SE_Sugar, 
                    ymax = Mean_Sugar_Intake + SE_Sugar), 
                width = 0.2) +
  labs(
    title = "Weighted Mean Total Sugar Intake Over Time, by Gender",
    x = "Time Period (2000-2018)", # <-- ADDED/CLARIFIED X-AXIS LABEL
    y = "Mean Sugar Intake (grams)",
    color = "Gender"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1)) # <-- ROTATE X-TEXT

print(sugar_trend_plot)