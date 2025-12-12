# datasci-306-final-project

Our Shiny App provides a fully functional user interactive interface that allows for exploration of sugar intake trends across demographic groups such as age and gender ranging from 2000 to 2018. The app draws real world data from the National Health and Nutrition Examination Survey (NHANES) dataset, enabling users to visualize how sugar intake patterns relate to BMI and obesity trends in the United States. 

Our app also integrates several high level components as follows:

1. Survey Design (PSU, strata, weight)
This is used to scale the sample data appropriately so that the estimates are unbiased and the results can be generalized to the US population.

2. Time Variables (NHANES cycles, time period)
The app breaks down the dataset into three main stages used for data analysis: Early (2000-2006), Middle (2007-2014), and Late (2015-2018) and uses the relevant NHANES cycles that correspond to each time period.

3. Response Variables (sugar intake, BMI, obesity prevalence)
These variables are measured as outcomes and allows the app to display the visualization of such trends.

4. Demographic Variables (Gender, Age Group)
The app integrates these demographic variables as filters, enbaling users to visually compare the difference of sugar intake, BMI, and obesity prevalence patterns within subgroups side by side.

5. Regression Models
The app displays accurate regression models that changes values based on the filters inputted by the user.

Our app includes four main plots that provide the visualization of time trends: 
1. Sugar vs BMI Scatterplot
2. Sugar Intake Trend by Sex
3. Sugar Intake Trend Across Age Groups
4. Obesity Prevalence Trend Across Age Groups

Additionally, our app incorporates dynamic filtering by allowing the user to input their desired gender and age group selection. Upon filtering, the app immediately responds by displaying the appropriate plot and regression models. The regression models give the general formula, the particular survey design, slope coefficients for intercept, time periods, age groups, and gender. 

How to Run the App Locally:

1. Install R and RStudio:

R (version 4.0 or later)
Link: https://cran.r-project.org/

RStudio:
Link: https://posit.co/download/rstudio-desktop/

2. Install Required Packages: 

Open R or RStudio and run the following command: 
```{r}
install.packages(c(
  "shiny", "ggplot2", "dplyr", "survey", 
  "haven", "purrr", "tidyverse"
))
```

3. Download the Repository:
   
Via Git, you can run the following command: 
```{r}
git clone https://github.com/<your-username>/<your-repo>.git
```
Then run:
```{r}
cd <your-repo>
```

This clones the repository. 

Via Zip File, click Code then Download ZIP on GitHub and extract the folder. 

4. Add the NHANES Data Files:
Once you are inside the project file, create a directory named nhanes_data/. The necessary supported file types are DEMO, BMX, DR1TOT or DRXTOT followed by a underscore and the letter (A-J) and must end with .xpt. 

This is what your folder should look like:
```{r}
project-folder/
├── app.R
├── README.md
└── nhanes_data/
    ├── DEMO_C.XPT
    ├── DR1TOT_C.XPT
    ├── BMX_C.XPT
    └── ... (other cycles)
```

5. Run the Shiny App from R/RStudio

First, set the working directory to the project folder created earlier by running the following command: setwd("path/to/project-folder")

Inside the project directory, run:
```{r}
shiny::runApp("app.R") 
```

or you can do this manually by opening app.R and clicking Run App in RStudio.

Troubleshooting:

If the following error message pops up on the Time Trends & Divergence:

```{r}
"No valid survey design for current filters. Select at least one Age Group (and/or relax filters) so that enough PSUs remain."
```

Input at least one filter for "Select Age Group(s):"

If the following error message pops up on the Statistical Models (Reactive) for either Model 1 or 2:

```{r}
[1] "ERROR: Model failed. Filters are too restrictive for reliable complex survey analysis (Stratum/PSU error). Relax filters."
```

Input at least one filter for "Select Age Group(s):"













