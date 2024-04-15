library(tidyverse)

box_path = "/Users/michaelfive/Library/CloudStorage/Box-Box/WeCare/Data"

# dat_y <- read_csv(file.path(box_path, "cleaned/dat_youth_cleaned.csv"))
# dat_c <- read_csv(file.path(box_path, "cleaned/dat_caregiver_cleaned.csv"))

dat <- read_csv(file.path(box_path, "cleaned/dat_merged.csv")) |> 
  # Create a new column to categorize age group
  mutate(age_group = case_when(
    p_over_12 == 1 & p_over_18 != 1 ~ "12-17",
    p_over_18 == 1 | over_18 == 1 ~ "18+",
    TRUE ~ NA_character_  # Handles cases that do not fit either category
  )) |> 
  mutate(site_id = if_else(site_id == "H", "Harlem", "Kings County")) |> 
  mutate(treatment = if_else(treatment == 1, "Treatment", "Control"))


