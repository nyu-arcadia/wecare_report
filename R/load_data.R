library(tidyverse)

box_path = "/Users/michaelfive/Library/CloudStorage/Box-Box/WeCare/Data"

# dat_y <- read_csv(file.path(box_path, "cleaned/dat_youth_cleaned.csv"))
# dat_c <- read_csv(file.path(box_path, "cleaned/dat_caregiver_cleaned.csv"))

dat <- read_csv(file.path(box_path, "cleaned/dat_merged.csv")) |> 
  # Create a new column to categorize age group
  mutate(age_group = case_when(
    screen_age_group == 0 ~ "12-17",
    screen_age_group == 1 ~ "18+",
    TRUE ~ NA_character_  # Handles cases that do not fit either category
  )) |> 
  rowwise() |> 
  mutate(screen_race_eligible = if_any(c(screen_race_1, screen_race_2, screen_race_3, screen_race_4), 
                                     ~ .x == 1)) |> 
  mutate(completed_survey = if_else(
    any(!is.na(c_across(starts_with(c("giso_","cssrs_", "st_", "ets_",
                                      "acfs_", "barrier_", "atphs_", "soc_",
                                      "tam_", "pfcs_", "pvp_", "cde_",
                                      "hopeless_", "uppss_", "yrbsa_", "dus_",
                                      "se_", "pet_", "pil_", "cdef_",
                                      "ibelong_", "sc_", "joy_")) &
                        ends_with("_b")
                        ))),
    1, 0
  )) |> 
  ungroup() |>
  mutate(site_id = if_else(site_id == "H", "Harlem", "Kings County")) |> 
  mutate(treatment = if_else(treatment == 1, "Treatment", "Control")) |> 
  mutate(
    meet_minimal_risk = if_else(
      eligible_visited_ed == 1 | 
      eligible_hospitalized == 1 | 
      eligible_taken_med == 1 | 
      eligible_received_therapy == 1 | 
      eligible_sought_counseling == 1 | 
      (eligible_anxiety_1 + eligible_anxiety_2 >= 4) | 
      (eligible_depress_1 + eligible_depress_2 >= 4) | 
      (eligible_sch_connect_1 < 3 | eligible_sch_connect_2 < 3) | 
      eligible_self_harm > 0 | 
      eligible_sleep_problem_1 > 1,
      1, 0)
  )

# Manual fixes due to redcap entry error
# Check this box file for a complete list and description
# https://nyu.box.com/s/8vj7nrljzxsg7s8z68ilm80vgm2bey2m

dat <- dat |> 
  mutate(p_informed_consent_form_parent_complete = 
           if_else(p_wecare_id == "K-F0005-C-S", 
                   0, p_informed_consent_form_parent_complete)) |> 
  mutate(age_group = 
           if_else(wecare_id == "K-F0006-Y-S",
                   "18+", age_group)) |> 
  mutate(contact_form_youth_complete = 
           if_else(wecare_id == "K-F0006-Y-S",
                   2, contact_form_youth_complete)) |> 
  mutate(ps_signature = 
           if_else(wecare_id == "K-F0006-Y-S",
                   "signature_unavailable", ps_signature))


