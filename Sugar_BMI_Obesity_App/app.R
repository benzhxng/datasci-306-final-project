library(shiny)
library(ggplot2)
library(dplyr)
library(survey)
library(haven)
library(purrr)
library(tidyverse)

data_folder <- "/Users/joycechen/Desktop/datasci306 final project/datasci-306-final-project/final_project_clean/nhanes_data/"

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

# --- MOCK DATA FOR DEMO PURPOSES ---
# Define cycles and info for mock data generation
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
  final_raw_data <- final_raw_data %>% 
    mutate(DRITSUG_harmonized = coalesce(DRXTSUGR, DR1TSUGR))
}
# --- END MOCK DATA ---


# --- B. Clean and Create Analysis Variables ---
final_analysis_data <- final_raw_data %>%
  mutate(
    DRITSUG_harmonized = coalesce(DRXTSUGR, DR1TSUGR)
  ) %>%
  select(
    SEQN, NHANES_Cycle, RIDAGEYR, RIAGENDR,
    WTMEC2YR, SDMVPSU, SDMVSTRA, BMXBMI,
    DRITSUG = DRITSUG_harmonized
  ) %>%
  filter(RIDAGEYR >= 13) %>%
  filter(
    !is.na(DRITSUG), !is.na(BMXBMI), !is.na(WTMEC2YR)
  ) %>%
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
data_3_periods_age_sex <- final_analysis_data %>%
  mutate(NHANES_Cycle = as.factor(NHANES_Cycle)) %>%
  mutate(Cycle_Numeric = as.numeric(NHANES_Cycle)) %>%
  mutate(
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
  )

# Get variable choices for UI
age_choices <- levels(data_3_periods_age_sex$Age_Group)
gender_choices <- c("Male" = 1, "Female" = 2)

# Define plot choices for the new selector
plot_choices <- c(
  "Sugar vs. BMI Scatterplot" = "scatter",
  "Sugar Intake Trend by Sex" = "sugar_trend_sex",
  "Sugar Intake Trend Across Age Groups" = "sugar_trend_age",
  "Obesity Prevalence Trend Across Age Groups" = "obesity_trend_age"
)

# Define which plots need the Age Group filter to be visible
AGE_PLOT_TYPES <- c("sugar_trend_age", "obesity_trend_age")

# ====================================================================
# User Interface (ui)
# ====================================================================
ui <- fluidPage(
  titlePanel(h1("Interactive NHANES Sugar & Obesity Trends", style = "color: #2c3e50;")),
  
  sidebarLayout(
    # --- Sidebar Panel for Interactive Filters ---
    sidebarPanel(
      h4("Interactive Filters", style = "color: #34495e;"),
      
      # Plot Selector (Always Visible)
      selectInput("plot_selector", "Select Plot to Display:",
                  choices = plot_choices,
                  selected = "sugar_trend_age"), 
      hr(),
      
      # Filter 1: Age Group - RENDERED CONDITIONALLY 
      uiOutput("age_group_filter_ui"),
      
      # Filter 2: Sex - Always Visible
      selectInput("gender_filter", "Select Sex:",
                  choices = c("All" = "All", gender_choices),
                  selected = "All")
      
      # Time Period filter is now removed completely.
    ),
    
    # --- Main Panel for Visualizations ---
    mainPanel(
      tabsetPanel(
        
        # -----------------------------------------------------------
        # Tab 1: Descriptive Trends
        # -----------------------------------------------------------
        tabPanel("Time Trends & Divergence",
                 h3("Selected Trend Visualization", style = "color: #34495e;"),
                 p("Use the filters in the sidebar to dynamically generate the visualization below."),
                 hr(),
                 
                 # Single dynamic plot output
                 plotOutput("selectedPlot", height = "500px")
        ),
        
        # -----------------------------------------------------------
        # Tab 2: Regression Results
        # -----------------------------------------------------------
        tabPanel("Statistical Models (Reactive)",
                 h3("Survey-Weighted Regression Analysis", style = "color: #34495e;"),
                 p("Regression models are refit live using the subset of data defined by the sidebar filters. Note: Filtered subsets may cause 'Stratum/PSU' errors."),
                 hr(),
                 
                 fluidRow(
                   column(6, 
                          h4("Model 1: Sugar Trend (Linear Regression)", style = "color: #2980b9;"),
                          p("Outcome: Mean Sugar Intake (grams). Predictors: Time, Age, Gender, and Interactions."),
                          verbatimTextOutput("model1Summary")
                   ),
                   column(6,
                          h4("Model 2: Odds of Obesity (Logistic Regression)", style = "color: #e74c3c;"),
                          p("Outcome: Obese (Binary). Predictors: Time, Age, Gender, and Sugar Intake."),
                          verbatimTextOutput("model2Summary")
                   )
                 )
        )
      )
    )
  )
)


# ====================================================================
# Server Function (server) - All logic is now REACTIVE
# ====================================================================
server <- function(input, output, session) {
  
  # Define plot types that require the Age Group filter
  AGE_PLOT_TYPES <- c("sugar_trend_age", "obesity_trend_age")
  
  # --- 1. DYNAMIC UI OUTPUT for Age Group Filter ---
  output$age_group_filter_ui <- renderUI({
    # Only show this filter if an age-related plot is selected
    if (input$plot_selector %in% AGE_PLOT_TYPES) {
      
      # Get variable choices for UI (defined in static setup)
      age_choices <- levels(data_3_periods_age_sex$Age_Group)
      
      # Default to selecting ALL age groups so users see the faceted plot first
      selectInput("age_filter", "Select Age Group(s):",
                  choices = age_choices,
                  selected = age_choices, 
                  multiple = TRUE)
    } else {
      # Return NULL to hide the element
      NULL
    }
  })
  
  # Custom error message for plot outputs
  error_plot <- function(message_text = "ERROR: Filters are too restrictive. Relax selections to ensure at least two PSUs remain in each stratum.") {
    ggplot() + 
      annotate("text", x = 0.5, y = 0.5, 
               label = message_text, 
               size = 5, color = "red") + 
      theme_void()
  }
  
  # Custom error message for verbatim outputs
  error_verbatim <- "ERROR: Model failed. Filters are too restrictive for reliable complex survey analysis (Stratum/PSU error). Relax filters."
  
  
  # --- 2. REACTIVE DATA FILTERING (Returns filtered RAW data frame) ---
  reactive_data_filter <- reactive({
    
    data <- data_3_periods_age_sex
    
    # --- A. Apply Gender Filter (Always present) ---
    if (input$gender_filter != "All") {
      data <- data %>%
        filter(Gender == as.numeric(input$gender_filter))
    }
    
    # --- B. Apply Age Filter (Conditional & CRITICAL FIX) ---
    
    # Get the selected age groups, which might be NULL if the UI is hidden
    # Default to all age groups if the filter is hidden or hasn't loaded yet
    selected_ages <- if (!is.null(input$age_filter) && input$plot_selector %in% AGE_PLOT_TYPES) {
      input$age_filter
    } else {
      age_choices # Default to all groups for models/other plots
    }
    
    # We always filter the data to the selected age groups if the plot needs it
    if (input$plot_selector %in% AGE_PLOT_TYPES) {
      data <- data %>%
        filter(Age_Group %in% selected_ages)
    }
    
    # Check if any data remains
    if (nrow(data) < 10) { 
      return(NULL)
    }
    
    return(data)
  })
  
  # --- 3. REACTIVE SURVEY DESIGN (Returns svydesign object or NULL on error) ---
  reactive_survey_design <- reactive({
    data <- reactive_data_filter()
    req(data)
    
    # CRITICAL FIX: Drop unused factor levels in the data frame *before* creating the design.
    # This prevents svyby from calculating means for age groups that are filtered out.
    data <- droplevels(data)
    
    survey_design_result <- tryCatch({
      svydesign(
        id = ~PSU,
        strata = ~Strata,
        weights = ~MEC_Weight,
        data = data,
        nest = TRUE
      )
    }, error = function(e) {
      if (grepl("only one PSU at stage 1", conditionMessage(e))) {
        return(NULL)
      } else {
        if (nrow(data) < 2) { 
          return(NULL)
        }
        stop(e) 
      }
    })
    
    return(survey_design_result)
  })
  
  
  # --- 4. REACTIVE DESCRIPTIVE CALCULATIONS ---
  
  # R1. Gender-based means (Used for Scatter and Trend by Sex)
  reactive_gender_means <- reactive({
    design <- reactive_survey_design()
    req(design)
    
    means_df <- svyby(
      formula = ~Sugar_Intake_g + BMI, by = ~Time_Period + Gender,
      design = design, FUN = svymean, na.rm = TRUE
    ) %>% as.data.frame() %>%
      rename(Mean_Sugar_Intake = Sugar_Intake_g, SE_Sugar = se.Sugar_Intake_g,
             Mean_BMI = BMI, SE_BMI = se.BMI)
    
    obesity_df <- svyby(
      formula = ~Obese, by = ~Time_Period + Gender,
      design = design, FUN = svymean, na.rm = TRUE
    ) %>% as.data.frame() %>%
      rename(Obesity_Prevalence = Obese, SE_Obesity = se)
    
    left_join(means_df, obesity_df, by = c("Time_Period", "Gender")) %>%
      mutate(Gender_Label = factor(Gender, levels = 1:2, labels = c("Male", "Female")))
  })
  
  # R2. Age-based sugar means (Used for Sugar Facet Plot)
  reactive_age_sugar_means <- reactive({
    design <- reactive_survey_design()
    req(design)
    
    # No extra droplevels needed here since it's already done on the design data.
    svyby(
      formula = ~Sugar_Intake_g, by = ~Time_Period + Age_Group,
      design = design, FUN = svymean, na.rm = TRUE
    ) %>% as.data.frame() %>%
      rename(Mean_Sugar_Intake = Sugar_Intake_g, SE_Sugar = se)
  })
  
  # R3. Age-based obesity prevalence (Used for Obesity Facet Plot)
  reactive_age_obesity_prevalence <- reactive({
    design <- reactive_survey_design()
    req(design)
    
    # No extra droplevels needed here since it's already done on the design data.
    svyby(
      formula = ~Obese, by = ~Time_Period + Age_Group,
      design = design, FUN = svymean, na.rm = TRUE
    ) %>% as.data.frame() %>%
      rename(Prevalence_Obesity = Obese, SE_Obesity = se) %>%
      mutate(Prevalence_Percent = Prevalence_Obesity * 100)
  })
  
  
  # --- 5. REACTIVE MODEL FITTING ---
  
  # R4. Model 1: Sugar Trend 
  reactive_model_1 <- reactive({
    design <- reactive_survey_design()
    req(design)
    
    svyglm(
      formula = Sugar_Intake_g ~ Time_Period + Gender + Age_Group +
        Time_Period:Gender + Time_Period:Age_Group,
      design = design
    )
  })
  
  # R5. Model 2: Obesity Relationship 
  reactive_model_2 <- reactive({
    design <- reactive_survey_design()
    req(design)
    
    svyglm(
      formula = Obese ~ Time_Period + Gender + Age_Group + Sugar_Intake_g,
      design = design,
      family = quasibinomial()
    )
  })
  
  # ====================================================================
  # DYNAMIC PLOT OUTPUT
  # ====================================================================
  output$selectedPlot <- renderPlot({
    design <- reactive_survey_design()
    
    # 1. Handle Survey Design Error First
    if (is.null(design)) {
      return(error_plot())
    }
    
    # 2. Determine selected age groups and if faceting is needed
    selected_age_groups <- if (input$plot_selector %in% AGE_PLOT_TYPES && !is.null(input$age_filter)) {
      input$age_filter
    } else {
      age_choices # Use all groups for title default
    }
    
    # Check the *number of levels* remaining in the Age_Group column of the survey design data
    # This is the most reliable way to check for faceting after filtering.
    data_levels <- levels(design$variables$Age_Group)
    is_faceted_plot <- length(data_levels) > 1
    
    # 3. Render the selected plot based on the selector input
    switch(input$plot_selector,
           "scatter" = {
             df <- reactive_gender_means()
             req(df)
             
             ggplot(df,
                    aes(x = Mean_Sugar_Intake, y = Mean_BMI, color = Gender_Label)) +
               geom_point(aes(size = Obesity_Prevalence), alpha = 0.7) +
               geom_path(aes(group = Gender_Label), arrow = arrow(length = unit(0.3, "cm"))) +
               geom_text(aes(label = Time_Period), vjust = -1.5, size = 3) +
               labs(title = "Sugar Intake vs. BMI Scatterplot Over Time",
                    x = "Weighted Mean Sugar Intake (grams)", y = "Weighted Mean BMI (kg/m²)",
                    color = "Sex", size = "Obesity Prevalence") +
               theme_minimal(base_size = 14) + scale_color_brewer(palette = "Set1") +
               theme(legend.position = "bottom", legend.box = "horizontal")
           },
           
           "sugar_trend_sex" = {
             df <- reactive_gender_means()
             req(df)
             
             ggplot(df,
                    aes(x = Time_Period, y = Mean_Sugar_Intake, group = Gender_Label, color = Gender_Label)) +
               geom_line(linewidth = 1) + geom_point(size = 3) +
               geom_errorbar(aes(ymin = Mean_Sugar_Intake - SE_Sugar, ymax = Mean_Sugar_Intake + SE_Sugar),
                             width = 0.2) +
               labs(title = "Sugar Intake Trend by Sex Over Time",
                    x = "Time Period", y = "Mean Sugar Intake (grams)", color = "Sex") +
               theme_minimal(base_size = 14) +
               theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1))
           },
           
           "sugar_trend_age" = {
             df <- reactive_age_sugar_means()
             req(df)
             
             # Dynamic Plotting: Facet if multiple groups, single plot if one group
             main_title <- if (is_faceted_plot) {
               "Sugar Intake Trend Across Selected Age Groups"
             } else {
               # Use the single remaining level for the title
               paste("Sugar Intake Trend for Age Group:", data_levels[1])
             }
             
             p <- ggplot(df, aes(x = Time_Period, y = Mean_Sugar_Intake, group = 1)) +
               geom_line(color = "darkgreen", linewidth = 1) + geom_point(color = "darkgreen", size = 3) +
               geom_errorbar(aes(ymin = Mean_Sugar_Intake - SE_Sugar, ymax = Mean_Sugar_Intake + SE_Sugar),
                             width = 0.2, color = "gray50") +
               labs(title = main_title,
                    x = "Time Period", y = "Mean Sugar Intake (grams)") +
               theme_minimal(base_size = 14) +
               theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10))
             
             if (is_faceted_plot) {
               p + facet_wrap(~Age_Group)
             } else {
               p # Single plot is returned
             }
           },
           
           "obesity_trend_age" = {
             df <- reactive_age_obesity_prevalence()
             req(df)
             
             # Dynamic Plotting: Facet if multiple groups, single plot if one group
             main_title <- if (is_faceted_plot) {
               "Obesity Prevalence Trend Across Selected Age Groups"
             } else {
               # Use the single remaining level for the title
               paste("Obesity Prevalence Trend for Age Group:", data_levels[1])
             }
             
             p <- ggplot(df, aes(x = Time_Period, y = Prevalence_Percent, group = 1)) +
               geom_line(color = "firebrick4", linewidth = 1) + geom_point(color = "firebrick4", size = 3) +
               geom_errorbar(aes(ymin = Prevalence_Percent - (SE_Obesity * 100), ymax = Prevalence_Percent + (SE_Obesity * 100)),
                             width = 0.2, color = "gray50") +
               labs(title = main_title,
                    x = "Time Period", y = "Obesity Prevalence (%)") +
               theme_minimal(base_size = 14) +
               theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10)) +
               coord_cartesian(ylim = c(0, max(df$Prevalence_Percent, na.rm = TRUE) * 1.1))
             
             if (is_faceted_plot) {
               p + facet_wrap(~Age_Group)
             } else {
               p # Single plot is returned
             }
           },
           
           error_plot("Please select a plot type from the sidebar.")
    )
  })
  
  # OUTPUT 5: Regression Model 1 Summary
  output$model1Summary <- renderPrint({
    design <- reactive_survey_design()
    if (is.null(design)) return(error_verbatim)
    summary(reactive_model_1())
  })
  
  # OUTPUT 6: Regression Model 2 Summary
  output$model2Summary <- renderPrint({
    design <- reactive_survey_design()
    if (is.null(design)) return(error_verbatim)
    summary(reactive_model_2())
  })
}

# Run the application
shinyApp(ui = ui, server = server)