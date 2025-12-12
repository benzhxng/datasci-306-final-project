library(survey)
library(dplyr)

# Define the complex survey design using the clean data frame
nhanes_design_focused <- svydesign(
  id = ~PSU,      
  strata = ~Strata, 
  weights = ~MEC_Weight, 
  data = data_3_periods_age_sex,
  nest = TRUE
)

# 1. Calculate Weighted Means for Sugar and BMI by Time and Gender
descriptive_means_df <- svyby(
  formula = ~Sugar_Intake_g + BMI, 
  by = ~Time_Period + Gender, 
  design = nhanes_design_focused, 
  FUN = svymean, 
  na.rm = TRUE
) %>% 
  as.data.frame() %>%
  rename(
  Mean_Sugar_Intake = Sugar_Intake_g,
  SE_Sugar          = se.Sugar_Intake_g,
  Mean_BMI          = BMI,
  SE_BMI            = se.BMI
  )

# 2. Calculate Weighted Prevalence for Obesity by Time and Gender
descriptive_obesity_df <- svyby(
  formula = ~Obese, 
  by = ~Time_Period + Gender, 
  design = nhanes_design_focused, 
  FUN = svymean, 
  na.rm = TRUE
) %>% 
  as.data.frame() %>%
  rename(Obesity_Prevalence = Obese, SE_Obesity = se)


final_descriptive_data <- left_join(descriptive_means_df, descriptive_obesity_df, 
                                     by = c("Time_Period", "Gender"))

library(ggplot2)

# Create clearer labels for the Gender variable
final_descriptive_data <- final_descriptive_data %>%
  mutate(Gender_Label = factor(Gender, levels = 1:2, labels = c("Male", "Female")))

# Scatterplot: Mean Sugar vs. Mean BMI, points colored by gender and labeled by time
library(ggplot2)

sugar_bmi_scatterplot <- ggplot(final_descriptive_data, 
                                aes(x = Mean_Sugar_Intake, y = Mean_BMI, 
                                    color = Gender_Label)) +
  geom_point(aes(size = Obesity_Prevalence), alpha = 0.7) + 
  geom_path(aes(group = Gender_Label), arrow = arrow(length = unit(0.3, "cm"))) + 
  geom_text(aes(label = Time_Period), vjust = -1.5, size = 3) + 
  labs(
    title = "Weighted Mean Sugar Intake vs. Mean BMI Over Time",
    subtitle = "Points represent 3 Time Periods (Early -> Late)",
    x = "Weighted Mean Sugar Intake (grams)",
    y = "Weighted Mean BMI (kg/m²)",
    color = "Gender",
    size = "Obesity Prevalence"
  ) +
  theme_minimal(base_size = 14) +
  scale_color_brewer(palette = "Set1")

print(sugar_bmi_scatterplot)
