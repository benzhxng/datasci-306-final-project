library(shiny)
library(ggplot2)
library(dplyr)
library(survey)
library(haven)
library(purrr)
library(tidyverse)

data_folder <- "nhanes_data/"

file_info <- data.frame(
  Cycle = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J"),
  DemoFile = c("DEMO.XPT", "DEMO_B.XPT", paste0("DEMO_", LETTERS[3:10], ".XPT")),
  DietFile = c("DRXTOT.XPT", "DRXTOT_B.XPT", paste0("DR1TOT_", LETTERS[3:10], ".XPT")),
  BMXFile = c("BMX.XPT", "BMX_B.XPT", paste0("BMX_", LETTERS[3:10], ".XPT"))
)

load_and_tag <- function(file_name, cycle_id) {
  path <- paste0(data_folder, file_name)
  data <- read_xpt(path) %>%
    mutate(NHANES_Cycle = cycle_id)
  return(data)
}

demo_df <- map2_df(file_info$DemoFile, file_info$Cycle, load_and_tag)
bmx_df <- map2_df(file_info$BMXFile, file_info$Cycle, load_and_tag)
diet_df <- map2_df(file_info$DietFile, file_info$Cycle, load_and_tag)

# Merge all raw data into final_raw_data
final_raw_data <- full_join(demo_df, bmx_df, by = c("SEQN", "NHANES_Cycle")) %>%
  full_join(diet_df, by = c("SEQN", "NHANES_Cycle"))

# --------------------------------------------------------------------
# END: DATA LOADING AND MERGING
# --------------------------------------------------------------------


# ====================================================================
# B. CLEANING AND ANALYSIS VARIABLE CREATION
# (Unchanged from our last refinement)
# ====================================================================

final_analysis_data <- final_raw_data %>%
  mutate(
    DRITSUG_harmonized = coalesce(DRXTSUGR, DR1TSUGR),
    DRITKCAL_harmonized = coalesce(DRXTKCAL, DR1TKCAL) 
  ) %>%
  select(
    SEQN, NHANES_Cycle, RIDAGEYR, RIAGENDR, INDFMPIR, 
    WTMEC2YR, SDMVPSU, SDMVSTRA, BMXBMI,
    DRITSUG = DRITSUG_harmonized,
    DRITKCAL = DRITKCAL_harmonized 
  ) %>%
  filter(RIDAGEYR >= 13) %>%
  filter(
    !is.na(DRITSUG), !is.na(BMXBMI), !is.na(WTMEC2YR),
    !is.na(INDFMPIR), !is.na(DRITKCAL)
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


# ====================================================================
# C. TIME PERIODS AND FINAL DATA FRAME
# (Unchanged from our last refinement)
# ====================================================================

cycle_mapping <- c(
  "A" = "1999-00", "B" = "2001-02", "C" = "2003-04", "D" = "2005-06",
  "E" = "2007-08", "F" = "2009-10", "G" = "2011-12", "H" = "2013-14",
  "I" = "2015-16", "J" = "2017-18"
)

final_analysis_data_with_time <- final_analysis_data %>%
  mutate(NHANES_Cycle = as.factor(NHANES_Cycle)) %>%
  mutate(Cycle_Numeric = as.numeric(NHANES_Cycle)) %>%
  mutate(
    Cycle_Label = factor(cycle_mapping[as.character(NHANES_Cycle)], levels = cycle_mapping),
    Year_Numeric = 1999 + (Cycle_Numeric - 1) * 2, 
    Time_Period = case_when(
      Cycle_Numeric %in% 1:3 ~ "1_Early (00-06)",
      Cycle_Numeric %in% 4:7 ~ "2_Middle (07-14)",
      Cycle_Numeric %in% 8:10 ~ "3_Late (15-18)",
      TRUE ~ NA_character_
    ) %>% factor(levels = c("1_Early (00-06)", "2_Middle (07-14)", "3_Late (15-18)"))
  )

data_3_periods_age_sex <- final_analysis_data_with_time %>%
  rename(
    Sugar_Intake_g = DRITSUG,             
    Total_Calories_kCal = DRITKCAL,       
    BMI = BMXBMI,                        
    Obese = Obesity_Indicator,           
    Gender = RIAGENDR,                   
    PIR = INDFMPIR,                       
    PSU = SDMVPSU,                       
    Strata = SDMVSTRA,                    
    MEC_Weight = WTMEC2YR                 
  ) %>%
  select(
    PSU, Strata, MEC_Weight,
    NHANES_Cycle, Cycle_Numeric, Cycle_Label, Time_Period, Year_Numeric, 
    Sugar_Intake_g, Total_Calories_kCal, BMI, Obese,
    Gender, RIDAGEYR, Age_Group, BMI_Category, 
    PIR, Income_Category
  )


# ====================================================================
# UI, Server, and shinyApp Call (No change from last version)
# ====================================================================

age_choices <- levels(data_3_periods_age_sex$Age_Group)
gender_choices <- c("Male" = 1, "Female" = 2)
income_choices <- levels(data_3_periods_age_sex$Income_Category) 

plot_choices <- c(
  "Sugar vs. BMI Scatterplot" = "scatter",
  "Sugar Intake Trend by Sex" = "sugar_trend_sex",
  "Sugar Intake Trend Across Age Groups" = "sugar_trend_age",
  "Obesity Prevalence Trend Across Age Groups" = "obesity_trend_age"
)

AGE_PLOT_TYPES <- c("sugar_trend_age", "obesity_trend_age")

ui <- fluidPage(
  titlePanel(h1("Interactive NHANES Sugar & Obesity Trends", style = "color: #2c3e50;")),
  
  sidebarLayout(
    sidebarPanel(
      h4("Interactive Filters", style = "color: #34495e;"),
      
      selectInput("plot_selector", "Select Plot to Display:",
                  choices = plot_choices,
                  selected = "scatter"),
      
      radioButtons("time_granularity", "Time Axis Granularity:",
                   choices = c("Aggregated (3 Periods)" = "aggregated",
                               "Detailed (Cycles)" = "detailed"),
                   selected = "detailed"),
      
      hr(),
      
      uiOutput("age_group_filter_ui"), 
      
      selectInput("gender_filter", "Select Sex:",
                  choices = c("All" = "All", gender_choices),
                  selected = "All"),
      
      selectInput("income_filter", "Select Income Category:",
                  choices = c("All" = "All", income_choices),
                  selected = "All")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Time Trends & Divergence",
                 h3("Selected Trend Visualization", style = "color: #34495e;"),
                 p("Use the filters in the sidebar to dynamically generate the visualization below."),
                 hr(),
                 plotOutput("selectedPlot", height = "500px")
        ),
        
        tabPanel("Statistical Models (Reactive)",
                 h3("Survey-Weighted Regression Analysis", style = "color: #34495e;"),
                 p("Regression models use the subset defined by filters. Complex survey design is applied."),
                 hr(),
                 fluidRow(
                   column(4, 
                          h4("Model 1: Sugar Trend", style = "color: #2980b9;"),
                          p("Sugar ~ Time Period + Gender + Age Group + Income Category"),
                          verbatimTextOutput("model1Summary")
                   ),
                   column(4,
                          h4("Model 2: Obesity Odds", style = "color: #e74c3c;"),
                          p("Obese ~ Time + Age + Sugar + Income Category (Logistic)"),
                          verbatimTextOutput("model2Summary")
                   ),
                   column(4,
                          h4("Model 3: Continuous Sugar Trend", style = "color: #27ae60;"),
                          p("Sugar ~ Year (Cont.) + Age (Num.) + Gender + PIR (Cont. Income)"),
                          verbatimTextOutput("model3Summary")
                   )
                 )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  AGE_PLOT_TYPES <- c("sugar_trend_age", "obesity_trend_age")
  
  error_plot <- function(message_text) {
    ggplot() +
      annotate("text", x = 0.5, y = 0.5, label = message_text, size = 5, color = "red") +
      theme_void()
  }
  
  output$age_group_filter_ui <- renderUI({
    if (input$plot_selector %in% AGE_PLOT_TYPES) {
      selectInput("age_filter", "Select Age Group(s):",
                  choices = levels(data_3_periods_age_sex$Age_Group),
                  multiple = TRUE,
                  selected = levels(data_3_periods_age_sex$Age_Group))
    } else {
      NULL
    }
  })
  
  reactive_data_filter <- reactive({
    data <- data_3_periods_age_sex
    
    if (input$gender_filter != "All") {
      data <- data %>% filter(Gender == as.numeric(input$gender_filter))
    }
    
    if (input$income_filter != "All") {
      data <- data %>% filter(Income_Category %in% input$income_filter)
    }
    
    if (input$plot_selector %in% AGE_PLOT_TYPES) {
      if (!is.null(input$age_filter) && length(input$age_filter) > 0) {
        data <- data %>% filter(Age_Group %in% input$age_filter)
      } else {
        return(NULL) 
      }
    }
    
    if (nrow(data) < 10) return(NULL)
    data
  })
  
  reactive_survey_design <- reactive({
    data <- reactive_data_filter()
    if (is.null(data)) return(NULL)
    data <- droplevels(data)
    
    tryCatch({
      # Use nest = TRUE to handle PSU/Strata identifiers
      svydesign(id = ~PSU, strata = ~Strata, weights = ~MEC_Weight, data = data, nest = TRUE)
    }, error = function(e) NULL)
  })
  
  get_time_var <- reactive({
    if(input$time_granularity == "detailed") "Cycle_Label" else "Time_Period"
  })
  
  reactive_gender_means <- reactive({
    design <- reactive_survey_design()
    req(design)
    time_col <- get_time_var()
    by_formula <- as.formula(paste("~", time_col, "+ Gender"))
    
    means_df <- svyby(formula = ~Sugar_Intake_g + BMI, by = by_formula, design = design, FUN = svymean, na.rm = TRUE)
    obese_df <- svyby(formula = ~Obese, by = by_formula, design = design, FUN = svymean, na.rm = TRUE)
    
    left_join(means_df, obese_df, by = c(time_col, "Gender")) %>%
      rename(Time_X_Axis = !!sym(time_col)) %>% 
      rename(Mean_Sugar_Intake = Sugar_Intake_g, SE_Sugar = se.Sugar_Intake_g,
             Mean_BMI = BMI, Obesity_Prevalence = Obese) %>%
      mutate(Gender_Label = factor(Gender, levels = 1:2, labels = c("Male", "Female")))
  })
  
  reactive_age_sugar_means <- reactive({
    design <- reactive_survey_design()
    req(design)
    time_col <- get_time_var()
    by_formula <- as.formula(paste("~", time_col, "+ Age_Group"))
    
    svyby(formula = ~Sugar_Intake_g, by = by_formula, design = design, FUN = svymean, na.rm = TRUE) %>%
      rename(Time_X_Axis = !!sym(time_col)) %>%
      rename(Mean_Sugar_Intake = Sugar_Intake_g, SE_Sugar = se)
  })
  
  reactive_age_obesity_prevalence <- reactive({
    design <- reactive_survey_design()
    req(design)
    time_col <- get_time_var()
    by_formula <- as.formula(paste("~", time_col, "+ Age_Group"))
    
    svyby(formula = ~Obese, by = by_formula, design = design, FUN = svymean, na.rm = TRUE) %>%
      rename(Time_X_Axis = !!sym(time_col)) %>%
      rename(Prevalence_Obesity = Obese, SE_Obesity = se) %>%
      mutate(Prevalence_Percent = Prevalence_Obesity * 100)
  })
  
  # --- MODELS ---
  reactive_model_1 <- reactive({
    d <- reactive_survey_design()
    req(d)
    svyglm(Sugar_Intake_g ~ Time_Period + factor(Gender) + Age_Group + Income_Category, design = d)
  })
  
  reactive_model_2 <- reactive({
    d <- reactive_survey_design()
    req(d)
    svyglm(Obese ~ Time_Period + factor(Gender) + Age_Group + Sugar_Intake_g + Income_Category, design = d, family = quasibinomial())
  })
  
  reactive_model_3 <- reactive({
    d <- reactive_survey_design()
    req(d)
    svyglm(Sugar_Intake_g ~ Year_Numeric + factor(Gender) + RIDAGEYR + PIR, design = d)
  })
  
  # --- OUTPUTS ---
  output$selectedPlot <- renderPlot({
    design <- reactive_survey_design()
    if (is.null(design)) return(error_plot("Filters too restrictive, Survey Design Failed, or Data Missing."))
    
    is_faceted_plot <- input$plot_selector %in% AGE_PLOT_TYPES && length(unique(design$variables$Age_Group)) > 1
    
    base_theme <- theme_minimal(base_size = 14) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    switch(input$plot_selector,
           "scatter" = {
             df <- reactive_gender_means()
             req(df)
             ggplot(df, aes(x = Mean_Sugar_Intake, y = Mean_BMI, color = Gender_Label)) +
               geom_point(aes(size = Obesity_Prevalence), alpha = 0.7) +
               geom_path(aes(group = Gender_Label), arrow = arrow(length = unit(0.2, "cm"))) +
               geom_text(aes(label = Time_X_Axis), vjust = -1.5, size = 3) +
               scale_size_area(name = "Obesity_Prevalence", max_size = 8) +
               labs(title = paste("Sugar vs. BMI:", ifelse(input$time_granularity=="detailed", "By Cycle", "By Period")),
                    subtitle = paste("Filtered by Income:", input$income_filter),
                    x = "Mean Sugar (g)", y = "Mean BMI", color = "Sex") + 
               base_theme
           },
           "sugar_trend_sex" = {
             df <- reactive_gender_means()
             req(df)
             ggplot(df, aes(x = Time_X_Axis, y = Mean_Sugar_Intake, group = Gender_Label, color = Gender_Label)) +
               geom_line(linewidth = 1) + geom_point(size = 3) +
               geom_errorbar(aes(ymin = Mean_Sugar_Intake - SE_Sugar, ymax = Mean_Sugar_Intake + SE_Sugar), width = 0.2) +
               labs(title = "Sugar Intake Trend by Sex", 
                    subtitle = paste("Filtered by Income:", input$income_filter),
                    x = "Time", y = "Sugar Intake (g)") + 
               base_theme + theme(legend.position = "bottom")
           },
           "sugar_trend_age" = {
             df <- reactive_age_sugar_means()
             req(df)
             p <- ggplot(df, aes(x = Time_X_Axis, y = Mean_Sugar_Intake, group = Age_Group)) +
               geom_line(aes(color = Age_Group), linewidth = 1) + geom_point(aes(color = Age_Group), size = 3) +
               geom_errorbar(aes(ymin = Mean_Sugar_Intake - SE_Sugar, ymax = Mean_Sugar_Intake + SE_Sugar), width = 0.2) +
               labs(title = "Sugar Intake Trend Across Age Groups", 
                    subtitle = paste("Filtered by Income and Sex:", input$income_filter, "/", input$gender_filter),
                    x = "Time", y = "Sugar Intake (g)") + 
               base_theme
             if (is_faceted_plot) p + facet_wrap(~Age_Group) else p
           },
           "obesity_trend_age" = {
             df <- reactive_age_obesity_prevalence()
             req(df)
             p <- ggplot(df, aes(x = Time_X_Axis, y = Prevalence_Percent, group = Age_Group)) +
               geom_line(aes(color = Age_Group), linewidth = 1) + geom_point(aes(color = Age_Group), size = 3) +
               geom_errorbar(aes(ymin = Prevalence_Percent - (SE_Obesity * 100), ymax = Prevalence_Percent + (SE_Obesity * 100)), width = 0.2) +
               labs(title = "Obesity Prevalence Trend Across Age Groups", 
                    subtitle = paste("Filtered by Income and Sex:", input$income_filter, "/", input$gender_filter),
                    x = "Time", y = "Obesity Prevalence (%)") + 
               base_theme + 
               coord_cartesian(ylim = c(0, max(df$Prevalence_Percent, na.rm=TRUE) * 1.15))
             
             if (is_faceted_plot) p + facet_wrap(~Age_Group) else p
           }
    )
  })
  
  output$model1Summary <- renderPrint({ summary(reactive_model_1()) })
  output$model2Summary <- renderPrint({ summary(reactive_model_2()) })
  output$model3Summary <- renderPrint({ summary(reactive_model_3()) })
}

shinyApp(ui = ui, server = server)