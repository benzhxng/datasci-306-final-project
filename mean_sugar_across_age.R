library(survey)
library(dplyr)
library(ggplot2)

# Calculate Weighted Means for Sugar by Time and Age Group
descriptive_age_df <- svyby(
  formula = ~Sugar_Intake_g, 
  by = ~Time_Period + Age_Group, 
  design = nhanes_design_focused, 
  FUN = svymean, 
  na.rm = TRUE
) %>% 
  as.data.frame() %>%
  rename(
    Mean_Sugar_Intake = Sugar_Intake_g,
    SE_Sugar          = se,
  )

sugar_age_facet_plot <- ggplot(descriptive_age_df, 
                               aes(x = Time_Period, y = Mean_Sugar_Intake, group = 1)) +
  geom_line(color = "darkgreen", linewidth = 1) +
  geom_point(color = "darkgreen", size = 3) +
  geom_errorbar(aes(ymin = Mean_Sugar_Intake - SE_Sugar, 
                    ymax = Mean_Sugar_Intake + SE_Sugar), 
                width = 0.2, color = "gray50") +
  labs(
    title = "Weighted Mean Sugar Intake Trend Across Age Groups",
    subtitle = "Sugar Intake (grams) Over Three NHANES Time Periods",
    x = "Time Period",
    y = "Mean Sugar Intake (grams)"
  ) +
  facet_wrap(~Age_Group) +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10)) # <-- ROTATED X-TEXT

print(sugar_age_facet_plot)