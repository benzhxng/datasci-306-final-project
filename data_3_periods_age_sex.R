library(dplyr)

data_3_periods_age_sex <- final_analysis_data_3_periods %>%
  select(
    # --- Survey Design (REQUIRED) ---
    PSU,                # SDMVPSU
    Strata,             # SDMVSTRA
    MEC_Weight,         # WTMEC2YR
    
    # --- Time Variables (REQUIRED) ---
    NHANES_Cycle,       # The original factor cycle (for reference/detailed plots)
    Cycle_Numeric,      # The numeric cycle (1-10)
    Time_Period,        # The new 3-category time variable (for modeling)
    
    # --- Outcome/Exposure Variables (REQUIRED) ---
    Sugar_Intake_g,     # DRITSUG (Primary exposure)
    BMI,                # BMXBMI
    Obese,              # Obesity_Indicator (Secondary outcome)
    
    # --- Demographic Variables (REQUIRED) ---
    Gender,             # RIAGENDR
    Age_Group,          # (Created during cleaning)
    
    # --- Other important variable (OPTIONAL, but useful) ---
    BMI_Category        # (Useful for descriptive tables)
  )

# Verify the result (it should show only 12 columns now)
print(paste("Number of columns remaining:", ncol(final_analysis_data_3_periods)))
head(final_analysis_data_3_periods)