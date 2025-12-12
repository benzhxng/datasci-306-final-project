library(shiny)
library(ggplot2)
library(dplyr)
library(survey)
library(haven)
library(purrr)
library(tidyverse)

# --- MOCK DATA GENERATION ---
file_info <- data.frame(
  Cycle = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J")
)

if (!exists("final_raw_data")) {
  set.seed(42)
  n <- 1000
  final_raw_data <- tibble(
    SEQN = 1:n,
    NHANES_Cycle = sample(file_info$Cycle, n, replace = TRUE),
    RIDAGEYR = sample(13:80, n, replace = TRUE),
    RIAGENDR = sample(1:2, n, replace = TRUE),
    WTMEC2YR = runif(n, 100, 5000),
    SDMVPSU = sample(1:10, n, replace = TRUE),
    SDMVSTRA = sample(1:5, n, replace = TRUE),
    BMXBMI = runif(n, 15, 45),
    DRXTSUGR = runif(n, 50, 300),
    DR1TSUGR = runif(n, 50, 300),
    INDFMPIR = runif(n, 0.5, 5)
  )
}
# --- END MOCK DATA ---

# --- B. Clean and Create Analysis Variables ---
final_analysis_data <- final_raw_data %>%
  # Create the harmonized variable first
  mutate(
    DRITSUG_harmonized = coalesce(DRXTSUGR, DR1TSUGR)
  ) %>%
  select(
    SEQN, NHANES_Cycle, RIDAGEYR, RIAGENDR,
    WTMEC2YR, SDMVPSU, SDMVSTRA, BMXBMI,
    DRITSUG = DRITSUG_harmonized
  ) %>%
  filter(RIDAGEYR >= 13) %>%
  filter(!is.na(DRITSUG), !is.na(BMXBMI), !is.na(WTMEC2YR)) %>%
  mutate(
    Obesity_Indicator = as.integer(BMXBMI >= 30),
    Age_Group = case_when(
      RIDAGEYR >= 60 ~ "60+",
      RIDAGEYR >= 45 ~ "45-59",
      RIDAGEYR >= 30 ~ "30-44",
      RIDAGEYR >= 20 ~ "20-29",
      RIDAGEYR >= 13 ~ "13-19",
      TRUE ~ NA_character_
    ) %>% factor(levels = c("13-19", "20-29", "30-44", "45-59", "60+"))
  )

# --- C. Create Time Periods and Final Data Frame/Survey Design ---
cycle_mapping <- c(
  "A" = "1999-00", "B" = "2001-02", "C" = "2003-04", "D" = "2005-06",
  "E" = "2007-08", "F" = "2009-10", "G" = "2011-12", "H" = "2013-14",
  "I" = "2015-16", "J" = "2017-18"
)

data_3_periods_age_sex <- final_analysis_data %>%
  mutate(NHANES_Cycle = as.factor(NHANES_Cycle)) %>%
  mutate(Cycle_Numeric = as.numeric(NHANES_Cycle)) %>%
  mutate(
    # 1. Granular Label
    Cycle_Label = factor(cycle_mapping[as.character(NHANES_Cycle)], levels = cycle_mapping),
    
    # 2. Continuous Year (Start of cycle) for regression
    Year_Numeric = 1999 + (Cycle_Numeric - 1) * 2,
    
    # 3. Grouped Period
    Time_Period = case_when(
      Cycle_Numeric %in% 1:3 ~ "1_Early (00-06)",
      Cycle_Numeric %in% 4:7 ~ "2_Middle (07-14)",
      Cycle_Numeric %in% 8:10 ~ "3_Late (15-18)",
      TRUE ~ NA_character_
    ) %>% factor(levels = c("1_Early (00-06)", "2_Middle (07-14)", "3_Late (15-18)"))
  ) %>%
  rename(
    Sugar_Intake_g = DRITSUG, BMI = BMXBMI, Obese = Obesity_Indicator,
    Gender = RIAGENDR, PSU = SDMVPSU, Strata = SDMVSTRA, MEC_Weight = WTMEC2YR
    # Note: RIDAGEYR is strictly kept from final_analysis_data, no rename needed
  )

# UI Choices
age_choices <- levels(data_3_periods_age_sex$Age_Group)
gender_choices <- c("Male" = 1, "Female" = 2)

plot_choices <- c(
  "Sugar vs. BMI Scatterplot" = "scatter",
  "Sugar Intake Trend by Sex" = "sugar_trend_sex",
  "Sugar Intake Trend Across Age Groups" = "sugar_trend_age",
  "Obesity Prevalence Trend Across Age Groups" = "obesity_trend_age"
)

AGE_PLOT_TYPES <- c("sugar_trend_age", "obesity_trend_age")

# ====================================================================
# User Interface (ui) - UPDATED FOR MODEL ORDER
# ====================================================================
ui <- fluidPage(
  titlePanel(h1("Interactive NHANES Sugar & Obesity Trends", style = "color: #2c3e50;")),
  
  sidebarLayout(
    sidebarPanel(
      h4("Interactive Filters", style = "color: #34495e;"),
      
      selectInput("plot_selector", "Select Plot to Display:",
                  choices = plot_choices,
                  selected = "sugar_trend_age"),
      
      radioButtons("time_granularity", "Time Axis Granularity:",
                   choices = c("Aggregated (3 Periods)" = "aggregated",
                               "Detailed (Cycles)" = "detailed"),
                   selected = "aggregated"),
      
      hr(),
      
      uiOutput("age_group_filter_ui"),
      
      selectInput("gender_filter", "Select Sex:",
                  choices = c("All" = "All", gender_choices),
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
                   # --- MODEL 1 (FIRST) ---
                   column(4, 
                          h4("Model 1: Grouped Trends", style = "color: #2980b9;"),
                          p("Sugar ~ Time Period + Gender + Age Group"),
                          verbatimTextOutput("model1Summary")
                   ),
                   # --- MODEL 2 (SECOND) ---
                   column(4,
                          h4("Model 2: Obesity Odds", style = "color: #e74c3c;"),
                          p("Obese ~ Time + Age + Sugar"),
                          verbatimTextOutput("model2Summary")
                   ),
                   # --- MODEL 3 (THIRD - RENAME VARS) ---
                   column(4,
                          h4("Model 3: Continuous Linear", style = "color: #27ae60;"),
                          # Renamed for better context: Year_Numeric -> Year, factor(Gender) -> Gender, RIDAGEYR -> Age (Numeric)
                          p("Sugar ~ Year (Continuous) + Gender + Age (Numeric)"),
                          verbatimTextOutput("model3Summary")
                   )
                 )
        )
      )
    )
  )
)

# ====================================================================
# Server Function (server) - UNCHANGED
# ====================================================================
server <- function(input, output, session) {
  
  AGE_PLOT_TYPES <- c("sugar_trend_age", "obesity_trend_age")
  
  error_plot <- function(message_text) {
    ggplot() +
      annotate("text", x = 0.5, y = 0.5, label = message_text, size = 5, color = "red") +
      theme_void()
  }
  
  # --- 1. DYNAMIC UI ---
  output$age_group_filter_ui <- renderUI({
    if (input$plot_selector %in% AGE_PLOT_TYPES) {
      selectInput("age_filter", "Select Age Group(s):",
                  choices = levels(data_3_periods_age_sex$Age_Group),
                  multiple = TRUE)
    } else {
      NULL
    }
  })
  
  # --- 2. REACTIVE DATA FILTERING ---
  reactive_data_filter <- reactive({
    data <- data_3_periods_age_sex
    if (input$gender_filter != "All") {
      data <- data %>% filter(Gender == as.numeric(input$gender_filter))
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
  
  # --- 3. REACTIVE SURVEY DESIGN ---
  reactive_survey_design <- reactive({
    data <- reactive_data_filter()
    if (is.null(data)) return(NULL)
    data <- droplevels(data)
    
    tryCatch({
      svydesign(id = ~PSU, strata = ~Strata, weights = ~MEC_Weight, data = data, nest = TRUE)
    }, error = function(e) NULL)
  })
  
  # --- 4. REACTIVE CALCULATIONS (Plots) ---
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
  
  # --- 5. MODELS ---
  
  # Model 1: Original Grouped
  reactive_model_1 <- reactive({
    d <- reactive_survey_design()
    req(d)
    svyglm(Sugar_Intake_g ~ Time_Period + Gender + Age_Group, design = d)
  })
  
  # Model 2: Logistic Obesity
  reactive_model_2 <- reactive({
    d <- reactive_survey_design()
    req(d)
    svyglm(Obese ~ Time_Period + Gender + Age_Group + Sugar_Intake_g, design = d, family = quasibinomial())
  })
  
  # Model 3: Continuous/Ungrouped (NEW)
  # Regresses Sugar ~ Year (numeric) + Gender (Factor) + Age (numeric)
  reactive_model_3 <- reactive({
    d <- reactive_survey_design()
    req(d)
    svyglm(Sugar_Intake_g ~ Year_Numeric + factor(Gender) + RIDAGEYR, design = d)
  })
  
  # ====================================================================
  # OUTPUTS
  # ====================================================================
  output$selectedPlot <- renderPlot({
    design <- reactive_survey_design()
    if (is.null(design)) return(error_plot("Filters too restrictive or Survey Design Failed."))
    
    data_levels <- levels(design$variables$Age_Group)
    is_faceted_plot <- length(data_levels) > 1
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
               labs(title = paste("Sugar vs. BMI:", ifelse(input$time_granularity=="detailed", "By Cycle", "By Period")),
                    x = "Mean Sugar (g)", y = "Mean BMI", color = "Sex") + base_theme
           },
           "sugar_trend_sex" = {
             df <- reactive_gender_means()
             req(df)
             ggplot(df, aes(x = Time_X_Axis, y = Mean_Sugar_Intake, group = Gender_Label, color = Gender_Label)) +
               geom_line(linewidth = 1) + geom_point(size = 3) +
               geom_errorbar(aes(ymin = Mean_Sugar_Intake - SE_Sugar, ymax = Mean_Sugar_Intake + SE_Sugar), width = 0.2) +
               labs(title = "Sugar Intake Trend by Sex", x = "Time", y = "Sugar Intake (g)") + base_theme + theme(legend.position = "bottom")
           },
           "sugar_trend_age" = {
             df <- reactive_age_sugar_means()
             req(df)
             p <- ggplot(df, aes(x = Time_X_Axis, y = Mean_Sugar_Intake, group = 1)) +
               geom_line(color = "darkgreen", linewidth = 1) + geom_point(color = "darkgreen", size = 3) +
               geom_errorbar(aes(ymin = Mean_Sugar_Intake - SE_Sugar, ymax = Mean_Sugar_Intake + SE_Sugar), width = 0.2, color = "gray50") +
               labs(title = "Sugar Intake Trend Across Age Groups", x = "Time", y = "Sugar Intake (g)") + base_theme
             if (is_faceted_plot) p + facet_wrap(~Age_Group) else p
           },
           "obesity_trend_age" = {
             df <- reactive_age_obesity_prevalence()
             req(df)
             p <- ggplot(df, aes(x = Time_X_Axis, y = Prevalence_Percent, group = 1)) +
               geom_line(color = "firebrick4", linewidth = 1) + geom_point(color = "firebrick4", size = 3) +
               geom_errorbar(aes(ymin = Prevalence_Percent - (SE_Obesity * 100), ymax = Prevalence_Percent + (SE_Obesity * 100)), width = 0.2, color = "gray50") +
               labs(title = "Obesity Prevalence Trend Across Age Groups", x = "Time", y = "Obesity Prevalence (%)") + base_theme + 
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