library(survey)
library(dplyr)
library(ggplot2)


obesity_age_df <- svyby(
  formula = ~Obese, 
  by = ~Time_Period + Age_Group, 
  design = nhanes_design_focused, 
  FUN = svymean, 
  na.rm = TRUE
) %>% 
  as.data.frame() %>%
  # Rename columns
  rename(
    Prevalence_Obesity = Obese, 
    SE_Obesity         = se
  ) %>%
  # Convert prevalence from 0-1 to percentage for clearer plotting labels
  mutate(Prevalence_Percent = Prevalence_Obesity * 100)

# Print the table to inspect the means and ensure data exists
print("Weighted Obesity Prevalence by Age Group:")
print(head(obesity_age_df))

# --- Rerun the Plot ---
obesity_age_facet_plot <- ggplot(obesity_age_df, 
                                 aes(x = Time_Period, y = Prevalence_Percent, group = 1)) +
  geom_line(color = "firebrick4", linewidth = 1) +
  geom_point(color = "firebrick4", size = 3) +
  geom_errorbar(aes(ymin = Prevalence_Percent - (SE_Obesity * 100), 
                    ymax = Prevalence_Percent + (SE_Obesity * 100)), 
                width = 0.2, color = "gray50") +
  labs(
    title = "Weighted Obesity Prevalence Trend Across Age Groups",
    subtitle = "Prevalence (%) Over Three NHANES Time Periods",
    x = "Time Period",
    y = "Obesity Prevalence (%)"
  ) +
  facet_wrap(~Age_Group) +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10)) + 
  coord_cartesian(ylim = c(0, max(obesity_age_df$Prevalence_Percent, na.rm = TRUE) * 1.1))

print(obesity_age_facet_plot)