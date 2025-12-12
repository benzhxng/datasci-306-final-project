library(tidyverse)
library(haven)
library(dplyr) 
library(purrr) 
library(tidyverse)

data_folder <- "/Users/joycechen/Desktop/datasci306 final project/nhanes_data/"

file_info <- data.frame(
  Cycle = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J"),
  DemoFile = c("DEMO.XPT", "DEMO_B.XPT", paste0("DEMO_", LETTERS[3:10], ".XPT")),
  DietFile = c("DRXTOT.XPT", "DRXTOT_B.XPT", paste0("DR1TOT_", LETTERS[3:10], ".XPT")),
  BMXFile = c("BMX.XPT", "BMX_B.XPT", paste0("BMX_", LETTERS[3:10], ".XPT"))
)

load_and_tag <- function(file_name, cycle_id) {
  path <- paste0(data_folder, file_name)
  message(paste("Loading:", file_name))
  
  data <- read_xpt(path) %>%
    mutate(NHANES_Cycle = cycle_id) 
  return(data)
}

demo_df <- map2_df(file_info$DemoFile, file_info$Cycle, load_and_tag)

bmx_df <- map2_df(file_info$BMXFile, file_info$Cycle, load_and_tag)

diet_df <- map2_df(file_info$DietFile, file_info$Cycle, load_and_tag)

final_raw_data <- full_join(demo_df, bmx_df, by = c("SEQN", "NHANES_Cycle")) %>%
  full_join(diet_df, by = c("SEQN", "NHANES_Cycle"))
