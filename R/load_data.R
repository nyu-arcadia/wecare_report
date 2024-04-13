library(tidyverse)

box_path = "/Users/michaelfive/Library/CloudStorage/Box-Box/WeCare/Data"

# dat_y <- read_csv(file.path(box_path, "cleaned/dat_youth_cleaned.csv"))
# dat_c <- read_csv(file.path(box_path, "cleaned/dat_caregiver_cleaned.csv"))

dat <- read_csv(file.path(box_path, "cleaned/dat_merged.csv")) |> 
  filter(!is.na(site_id))

