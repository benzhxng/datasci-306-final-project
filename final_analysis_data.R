library(dplyr)

final_analysis_data <- final_raw_data %>%
  mutate(
    DRITSUG_harmonized = coalesce(DRXTSUGR, DR1TSUGR),
    DRITKCAL_harmonized = coalesce(DRXTKCAL, DR1TKCAL) 
  ) %>%
  select(
    SEQN, NHANES_Cycle,
    RIDAGEYR, RIAGENDR, INDFMPIR,
    WTMEC2YR, SDMVPSU, SDMVSTRA,
    BMXBMI,
    DRITSUG = DRITSUG_harmonized,
    DRITKCAL = DRITKCAL_harmonized
  ) %>%

filter(RIDAGEYR >= 13) %>%
  filter(
    !is.na(DRITSUG), 
    !is.na(BMXBMI),
    !is.na(WTMEC2YR),
    !is.na(INDFMPIR)
  ) %>%
mutate(
  Obesity_Indicator = as.integer(BMXBMI >= 30),
  BMI_Category = case_when(
    BMXBMI < 18.5 ~ "Underweight",
    BMXBMI < 25 ~ "Healthy weight",
    BMXBMI < 30 ~ "Overweight",
    BMXBMI >= 30 ~ "Obese",
    TRUE ~ NA_character_
  ) %>% factor(levels = c("Underweight", "Healthy weight", "Overweight", "Obese")),

  Age_Group = case_when(
    RIDAGEYR >= 60 ~ "60+",
    RIDAGEYR >= 45 ~ "45-59",
    RIDAGEYR >= 30 ~ "30-44",
    RIDAGEYR >= 20 ~ "20-29",
    RIDAGEYR >= 13 ~ "13-19",
    TRUE ~ NA_character_
  ) %>% factor(levels = c("13-19", "20-29", "30-44", "45-59", "60+")),
  
  Income_Category = case_when(
    INDFMPIR < 1 ~ "1=Below Poverty",
    INDFMPIR < 2 ~ "2=Low Income",
    INDFMPIR < 3.5 ~ "3=Middle Income",
    TRUE ~ "4=High Income"
  ) %>% factor(levels = c("1=Below Poverty", "2=Low Income", "3=Middle Income", "4=High Income"))
)