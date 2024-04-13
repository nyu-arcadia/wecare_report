library(tidyverse)
# Functions to generate tables and figures

# -------- Table: Summary of Enrollment --------

t_enrollment <- dat |>
  # Create a new column to categorize age group
  mutate(age_group = case_when(
    p_over_12 == 1 & p_over_18 != 1 ~ "12-17",
    p_over_18 == 1 | over_18 == 1 ~ "18+",
    TRUE ~ NA_character_  # Handles cases that do not fit either category
  )) |>
  # Ensure grouping is done by both site_id and the new age_group column
  group_by(site_id, age_group) |>
  # Use summarise() to compute summaries specific to each group
  summarize(
    n_completed_contact_form = sum((age_group == "12-17" & p_contact_form_parent_complete == 2) | 
                                     (age_group == "18+" & contact_form_youth_complete == 2), na.rm = TRUE),
    n_passed_eligibility_screen = sum(eligibility_screen_complete == 2, na.rm = TRUE),
    n_passed_eligibility_survey = sum(eligibility_survey_complete == 2, na.rm = TRUE),
    n_consented = sum((age_group == "12-17" & p_informed_consent_form_parent_complete == 2) | 
                        (age_group == "18+" & informed_consent_form_youth_complete == 2), na.rm = TRUE),
    .groups = 'drop'  # Drop the grouping once summarization is done
  ) |> 
  # Ensure all combinations of site_id and age_group appear in the results
  complete(site_id = c("H", "K"), age_group = c("12-17", "18+"), fill = list(
    n_completed_contact_form = 0, 
    n_passed_eligibility_screen = 0,
    n_passed_eligibility_survey = 0,
    n_consented = 0
  )) |> 
  # Create percentage variables
  mutate(
    perc_consented_contact = if_else(n_completed_contact_form > 0, n_consented / n_completed_contact_form, NA_real_),
    perc_consented_eligible = if_else(n_passed_eligibility_survey > 0, n_consented / n_passed_eligibility_survey, NA_real_)
  ) |> 
  mutate(across(starts_with("perc"), ~ scales::percent(.x, accuracy = 1)))

# Rename
names(t_enrollment) <- c("Site", "Age Group",
               "Number of youth who completed the contact form",
               "Number of youth who passed the eligibility screen",
               "Number of youth who passed the eligibility survey",
               "Number of youth who consented",
               "Percent of youth eligible among those who completed the contact form",
               "Percent of youth eligible among those who passed the eligibility survey")

# Transpose
t_enrollment <- t_enrollment |> 
  t() |> 
  as.data.frame() |> 
  rownames_to_column()

# Create flextable
ft_enrollment <- flextable(t_enrollment) |> 
  width(width = 1) |>
  width(j = 1, width = 4) |>
  delete_part(part = "header") |>
  delete_rows(1:2) |> 
  add_header_row(values = c("", "12-17", "18+", "12-17", "18+"),
                 colwidths = c(1, 1, 1, 1, 1), top = TRUE) |> 
  hline_top(part = "header") |> 
  hline_bottom(part = "header") |> 
  add_header_row(values = c("", "Harlem", "King"),
                 colwidths = c(1, 2, 2), top = TRUE) |> 
  align(align = "center", part = "header") |> 
  align(align = "center", part = "body") |> 
  bold(bold = TRUE, part = "header")

# -------- Table: Number of participants enrolled during each one-week recruitment period --------

t_weekly_enrollment <- dat |>
  mutate(date_of_enrollment_all = coalesce(p_date_of_enrollment, date_of_enrollment)) |> 
  mutate(date_of_enrollment_range = get_week_range(date_of_enrollment_all)) |> 
  mutate(week = calculate_week_number(date_of_enrollment_all)) |> 
  # Create a new column to categorize age group
  mutate(age_group = case_when(
    p_over_12 == 1 & p_over_18 != 1 ~ "12-17",
    p_over_18 == 1 | over_18 == 1 ~ "18+",
    TRUE ~ NA_character_  # Handles cases that do not fit either category
  )) |>
  # Ensure grouping is done by both site_id and the new age_group column
  group_by(site_id, age_group, week, date_of_enrollment_range) |>
  # Use summarise() to compute summaries specific to each group
  summarize(
    n_consented = sum((age_group == "12-17" & p_informed_consent_form_parent_complete == 2) | 
                        (age_group == "18+" & informed_consent_form_youth_complete == 2), na.rm = TRUE),
    .groups = 'drop'  # Drop the grouping once summarization is done
  ) |> 
  # Ensure all combinations of site_id and age_group appear in the results
  complete(site_id = c("H", "K"), age_group = c("12-17", "18+"), fill = list(
    n_consented = 0
  )) |> 
  mutate(site_id = if_else(site_id == "K", "Kings County", "Harlem"))

# Rename
names(t_weekly_enrollment) <- c("Site", "Age Group",
                         "Week",
                         "Date Range of Enrollment",
                         "Number of youth who enrolled")

# Create flextable
ft_weekly_enrollment <- flextable(t_weekly_enrollment) |> 
  width(width = 1) |>
  width(j = 4, width = 2) |> 
  align(align = "center", part = "body") |> 
  align(align = "center", part = "header") |> 
  bold(bold = TRUE, part = "header")

# -------- Table: Treatment/Control Group Assignment --------

t_assignment <- dat |>
  # Create a new column to categorize age group
  mutate(age_group = case_when(
    p_over_12 == 1 & p_over_18 != 1 ~ "12-17",
    p_over_18 == 1 | over_18 == 1 ~ "18+",
    TRUE ~ NA_character_  # Handles cases that do not fit either category
  )) |>
  rowwise() %>%
  mutate(completed_survey = if_else(
    any(!is.na(select(., demo_grade_b:joy_youth_baseline_assessment_complete) %>%
                 select(-ends_with("_complete")))),
    1, 0
  )) %>%
  ungroup() |> 
  # Ensure grouping is done by both site_id and the new age_group column
  group_by(site_id, age_group, treatment) |>
  # Use summarise() to compute summaries specific to each group
  summarize(
    n_consented = sum((age_group == "12-17" & p_informed_consent_form_parent_complete == 2) | 
                        (age_group == "18+" & informed_consent_form_youth_complete == 2), na.rm = TRUE),
    n_completed_survey = sum(completed_survey, na.rm = T),
    n_completed_cassy = sum(cassy_results_complete == 2, na.rm = T),
    n_cassy_positive = sum(cassy_result == 1, na.rm = T),
    n_received_cfs = sum(swdf_received_cfs == 1, na.rm = T),
    .groups = 'drop'  # Drop the grouping once summarization is done
  ) |> 
  # Ensure all combinations of site_id and age_group appear in the results
  complete(site_id = c("H", "K"), fill = list(
    n_consented = 0,
    n_completed_survey = 0,
    n_completed_cassy = 0,
    n_cassy_positive = NA,
    n_received_cfs = NA
  )) |> 
  mutate(site_id = if_else(site_id == "K", "Kings County", "Harlem"))

# Rename
names(t_assignment) <- c("", "Age Group", "Treatment Condition",
                         "Number of youth who consented",
                         "Number of youth who completed baseline survey",
                         "Number of youth who completed CASSY",
                         "Number of youth who screened positive (> 0.05) on CASSY",
                         "Number of youth who received Connections for Safety"
                         )

# Transpose
t_assignment <- t_assignment |> 
  t() |> 
  as.data.frame() |> 
  rownames_to_column()

# Create flextable
ft_assignment <- flextable(t_assignment) |> 
  width(width = 1) |>
  width(j = 1, width = 4) |>
  delete_part(part = "header") |>
  hline_top(j = 1:ncol(t_assignment) , part = "body") |>
  hline(i = 1 , part = "body") |> 
  hline(i = 3 , part = "body") |>
  align(align = "center", part = "header") |> 
  align(align = "center", part = "body") |> 
  bold(i = 1, bold = TRUE, part = "body")


# -------- Table: Treatment/Control Group Assignment --------

