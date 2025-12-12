final_analysis_data <- final_analysis_data %>%
  mutate(NHANES_Cycle = as.factor(NHANES_Cycle))

# 2. Create the Numeric Cycle variable (1 through 10)
final_analysis_data <- final_analysis_data %>%
  mutate(Cycle_Numeric = as.numeric(NHANES_Cycle))

# 3. Create the Categorical Time_Period variable (3-point comparison)
final_analysis_data_3_periods <- final_analysis_data %>%
  mutate(
    Time_Period = case_when(
      # Early Period: First 3 cycles (2000-2006)
      Cycle_Numeric %in% 1:3 ~ "1_Early (00-06)",
      # Middle Period: Cycles 4 through 7 (2007-2014)
      Cycle_Numeric %in% 4:7 ~ "2_Middle (07-14)",
      # Late Period: Last 3 cycles (2015-2018)
      Cycle_Numeric %in% 8:10 ~ "3_Late (15-18)",
      TRUE ~ NA_character_
    ) %>% factor(levels = c("1_Early (00-06)", "2_Middle (07-14)", "3_Late (15-18)"))
  )

final_analysis_data_3_periods <- final_analysis_data_3_periods %>%
  rename(
    # --- Exposure & Outcome Variables ---
    Sugar_Intake_g = DRITSUG,     # Total Sugar Intake (grams)
    BMI = BMXBMI,                 # Body Mass Index (kg/m^2)
    Obese = Obesity_Indicator,    # Binary: 1=Obese (BMI >= 30), 0=Not Obese
    
    # --- Demographic Variables ---
    Gender = RIAGENDR,            # 1=Male, 2=Female
    
    # --- Survey Design Variables ---
    PSU = SDMVPSU,                # Primary Sampling Unit
    Strata = SDMVSTRA,            # Strata
    MEC_Weight = WTMEC2YR         # Examination Weight
  )