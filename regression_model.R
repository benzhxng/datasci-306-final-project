# Time_Period:Gender and Time_Period:Age_Group test for differential trends.

sugar_time_model_age_sex <- svyglm(
  formula = Sugar_Intake_g ~ Time_Period + Gender + Age_Group + 
    Time_Period:Gender + Time_Period:Age_Group,
  design = nhanes_design_focused
)

print("--- MODEL 1: SUGAR TREND RESULTS ---")
summary(sugar_time_model_age_sex)

obesity_model <- svyglm(
  formula = Obese ~ Time_Period + Gender + Age_Group + Sugar_Intake_g,
  design = nhanes_design_focused,
  family = quasibinomial()
)

print("--- MODEL 2: OBESITY LOGISTIC REGRESSION RESULTS ---")
summary(obesity_model)