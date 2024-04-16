library(tidyverse)
library(flextable)
# Functions to generate tables and figures

# -------- Table 1: Summary of Enrollment --------

t_enrollment <- dat |>
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
  complete(site_id = c("Harlem", "Kings County"), age_group = c("12-17", "18+"), fill = list(
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
names(t_enrollment) <- c("", "Age Group",
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
  width(width = 1.5) |>
  width(j = 1, width = 4) |>
  delete_part(part = "header") |>
  hline_top(j = 1:ncol(t_enrollment) , part = "body") |>
  hline(i = 1:2 , part = "body") |> 
  align(j = 2:5, align = "center", part = "body") |> 
  bold(i = 1:2, bold = TRUE, part = "body") |> 
  merge_h(i = 1:2)

# -------- Table 2: Number of participants enrolled during each one-week recruitment period --------

t_weekly_enrollment <- dat |>
  mutate(date_of_enrollment_all = coalesce(p_date_of_enrollment, date_of_enrollment, screen_doe)) |>
  mutate(date_of_enrollment_range = get_week_range(date_of_enrollment_all))

t_weekly_enrollment <- t_weekly_enrollment |> 
  # Ensure grouping is done by both site_id and the new age_group column
  group_by(site_id, age_group, date_of_enrollment_range) |>
  # Use summarise() to compute summaries specific to each group
  summarize(
    n_consented = sum((age_group == "12-17" & p_informed_consent_form_parent_complete == 2) | 
                        (age_group == "18+" & informed_consent_form_youth_complete == 2), na.rm = TRUE),
    .groups = 'drop'  # Drop the grouping once summarization is done
  ) |> 
  # Ensure all combinations of site_id and age_group appear in the results
  complete(site_id = c("Harlem", "Kings County"), 
           age_group = c("12-17", "18+"), 
           date_of_enrollment_range = na.omit(unique(t_weekly_enrollment$date_of_enrollment_range)),
           fill = list(
    n_consented = 0
  ))

# Rename
names(t_weekly_enrollment) <- c("Site", "Age Group",
                                "Date Range of Enrollment",
                                "Number of youth who enrolled")

t_weekly_enrollment <- t_weekly_enrollment |> 
  pivot_wider(names_from = `Date Range of Enrollment`, values_from = `Number of youth who enrolled`)


# Transpose
t_weekly_enrollment <- t_weekly_enrollment |> 
  t() |> 
  as.data.frame() |> 
  rownames_to_column() |> 
  mutate(week = if_else(rowname %in% c("Site", "Age Group"),
                        NA,
                        rowname
                        ),
         .before = rowname) |> 
  mutate(week = calculate_week_number(week))

t_weekly_enrollment$rowname[1] <- ""
t_weekly_enrollment$week[2] <- "Week"
t_weekly_enrollment$rowname[2] <- "Date range of report"

# Create flextable
ft_weekly_enrollment <- flextable(t_weekly_enrollment) |>
  width(width = 1.5) |>
  width(j = 2, width = 2) |>
  delete_part(part = "header") |>
  hline_top(j = 1:ncol(t_weekly_enrollment) , part = "body") |>
  hline(i = 1:2, part = "body") |> 
  align(j = 1:6, align = "center", part = "body") |> 
  bold(i = 1:2, bold = TRUE, part = "body") |> 
  merge_h(i = 1)

# -------- Table 3: Treatment/Control Group Assignment --------

t_assignment <- dat %>%
  rowwise() |> 
  mutate(completed_survey = if_else(
    any(!is.na(c_across(demo_grade_b:joy_youth_baseline_assessment_complete))),
    1, 0
  )) |> 
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
  complete(site_id = c("Harlem", "Kings County"), 
           treatment = c("Treatment", "Control"),
           age_group = c("12-17", "18+"), fill = list(
    n_consented = 0,
    n_completed_survey = 0,
    n_completed_cassy = 0,
    n_cassy_positive = NA,
    n_received_cfs = NA
  )) |> 
  mutate(treatment = if_else(is.na(treatment), "Unassigned", treatment))

# Rename
names(t_assignment) <- c("", "Treatment Condition","Age Group",
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
  width(width = 1.1) |>
  width(j = 1, width = 1.5) |>
  delete_part(part = "header") |>
  hline_top(j = 1:ncol(t_assignment) , part = "body") |>
  hline(i = 1, part = "body") |> 
  hline(i = 3, part = "body") |>
  align(align = "center", part = "header") |> 
  align(align = "center", part = "body") |> 
  bold(i = 1:3, bold = TRUE, part = "body") |> 
  merge_h(i = 1:2)


# -------- Table 4: Triggered Safety Plan During Baseline --------

t_trigger_cfs <- dat |>
  rowwise() |>
  mutate(
    cssrs_positive = if_else(
      any(c_across(c(cssrs_3b_b, cssrs_4b_b, cssrs_5b_b, cssrs_6b_b, cssrs_7b_b)) == 1),
      1, 
      0
    )
  ) |>
  ungroup() |>  
  group_by(site_id) |> 
  summarize(
    n_disclosure_suicide = sum(disclose_suicide, na.rm = T),
    n_cssrs_positive = sum(cssrs_positive, na.rm = T),
    n_cassy_positive = sum(cassy_result, na.rm = T)
  ) |> 
  # Ensure all combinations of site_id and age_group appear in the results
  complete(site_id = c("Harlem", "Kings County"), fill = list(
    n_disclosure_suicide = NA,
    n_cssrs_positive = NA,
    n_cassy_positive = NA
  ))
  
# Rename
names(t_trigger_cfs) <- c("", 
                         "Number of youth who verbally disclosed suicide ideation",
                         "Number of youth who screened positive on CSSRS",
                         "Number of youth who screened positive on CASSY"
)

# Transpose
t_trigger_cfs <- t_trigger_cfs |> 
  t() |> 
  as.data.frame() |> 
  rownames_to_column()

# Create flextable
ft_trigger_cfs <- flextable(t_trigger_cfs) |> 
  width(width = 1.5) |>
  width(j = 1, width = 4) |>
  delete_part(part = "header") |>
  hline_top(j = 1:ncol(t_trigger_cfs) , part = "body") |>
  hline(i = 1 , part = "body") |> 
  align(align = "center", part = "header") |> 
  align(align = "center", part = "body") |> 
  bold(i = 1, bold = TRUE, part = "body")


# -------- Table 5: Demographics of Enrolled Subjects (DSMB Chart) --------
t_demo <- dat |> 
  group_by(site_id, treatment) |>
  summarize(
            # age
            age_12_17 = sum(age_group == "12-17", na.rm = T),
            age_18_over = sum(age_group == "18+", na.rm = T),
            
            # sex at birht
            sex_male = sum(screen_sex == 1, na.rm = T),
            sex_female = sum(screen_sex == 2, na.rm = T),
            
            # gender identity
            gender_male = sum(giso_gender_identity_b == 1, na.rm = T),
            gender_female = sum(giso_gender_identity_b == 2, na.rm = T),
            gender_nonbinary = sum(giso_gender_identity_b == 3, na.rm = T),
            gender_genderfluid = sum(giso_gender_identity_b == 4, na.rm = T),
            gender_genderqueer = sum(giso_gender_identity_b == 5, na.rm = T),
            gender_notsure = sum(giso_gender_identity_b == 66, na.rm = T),
            gender_notknow = sum(giso_gender_identity_b == 77, na.rm = T),
            gender_notidentify = sum(giso_gender_identity_b == 88, na.rm = T),
            gender_noanswer = sum(giso_gender_identity_b == 99, na.rm = T),
            
            # race
            race_black = sum(screen_race_1 == 1, na.rm = T),
            race_africanamerican = sum(screen_race_2 == 1, na.rm = T),
            race_african = sum(screen_race_3 == 1, na.rm = T),
            race_caribbeanwestindian = sum(screen_race_4 == 1, na.rm = T),
            race_native = sum(screen_race_5 == 1, na.rm = T),
            race_asian = sum(screen_race_6 == 1, na.rm = T),
            race_pacificislander = sum(screen_race_7 == 1, na.rm = T),
            race_white = sum(screen_race_8 == 1, na.rm = T),
            race_unknown = sum(screen_race_99 == 1, na.rm = T),
            
            # ethnicity
            ethnicity_latino = sum(screen_ethnicity == 1, na.rm = T),
            ethnicity_notlatino = sum(screen_ethnicity == 2, na.rm = T),
            ethnicity_unknown = sum(screen_ethnicity == 99, na.rm = T),
            
            .groups = 'drop'  # Drop the grouping once summarization is done
  ) |> 
  # Ensure all combinations of site_id and age_group appear in the results
  complete(site_id = c("Harlem", "Kings County"), treatment = c("Treatment", "Control"), fill = list(
    age_12_17 = NA,
    age_18_over = NA,
    sex_male = NA,
    sex_female = NA,
    sex_male = NA,
    sex_female = NA,
    gender_male = NA,
    gender_female = NA,
    gender_nonbinary = NA,
    gender_genderfluid = NA,
    gender_genderqueer = NA,
    gender_notsure = NA,
    gender_notknow = NA,
    gender_notidentify = NA, 
    gender_noanswer = NA,
    race_black = NA,
    race_africanamerican = NA,
    race_african = NA,
    race_caribbeanwestindian = NA,
    race_native = NA,
    race_asian = NA,
    race_pacificislander = NA,
    race_white = NA,
    race_unknown = NA,
    ethnicity_latino = NA,
    ethnicity_notlatino = NA,
    ethnicity_unknown = NA
  )) |> 
  filter(is.na(treatment) != T)

# add sum variables
t_demo <- t_demo |> 
  rowwise() |> 
  mutate(age_total = sum(c_across(starts_with("age"))),
         .after = age_18_over) |> 
  mutate(sex_total = sum(c_across(starts_with("sex"))),
         .after = sex_female) |> 
  mutate(gender_total = sum(c_across(starts_with("gender"))),
         .after = gender_noanswer) |> 
  # mutate(race_total = sum(c_across(starts_with("race"))),
  #        .after = race_unknown) |> 
  # the total count should be the same for each section (e.g., for race or gender)
  # the rowsum is not accurate because race is multiple selection
  mutate(race_total = gender_total, .after = race_unknown) |> 
  mutate(ethnicity_total = sum(c_across(starts_with("ethnicity"))),
         .after = ethnicity_unknown) |> 
  ungroup()

# Apply the transformation to each group
t_demo <- t_demo |>
  transform_columns("age", "age_total") |>
  transform_columns("sex", "sex_total") |>
  transform_columns("gender", "gender_total") |>
  transform_columns("race", "race_total") |>
  transform_columns("ethnicity", "ethnicity_total")

# Transpose
t_demo <- t_demo |> 
  t() |> 
  as.data.frame() |> 
  rownames_to_column()

# Rename
t_demo$rowname <- c(
  "", "Treatment",
  "12-17", "18-19", "Total",
  "Male", "Female", "Total",
  "Male", "Female", "Nonbinary", "Genderfluid", "Genderqueer",
  "I am not sure or questioning", "I don't know what this question means",
  "I do not identify as any of these options", "I do not want to answer", 
  "Total",
  "Black",
  "Black (African American)",
  "Black (African)",
  "Black (Caribbean/West Indian)",
  "American Indian/Alaskan Native",
  "Native Hawaiian Or Other Pacific Islander",
  "Asian",
  "White",
  "Unknown or not reported",
  "Total", 
  "Hispanic origin",
  "Non-Hispanic Origin",
  "Unknown or not reported",
  "Total"
)

t_demo <- t_demo |> 
  add_row(rowname = "Age", .after = 2) |> 
  add_row(rowname = "Sex assigned at birth", .after = 6) |> 
  add_row(rowname = "Gender", .after = 10) |> 
  add_row(rowname = "Race (multiple selection)", .after = 21) |> 
  add_row(rowname = "Ethnicity", .after = 32)

# Create flextable
ft_demo <- flextable(t_demo) |> 
  width(width = 1.5) |>
  width(j = 1, width = 3) |>
  delete_part(part = "header") |>
  hline_top(j = 1:ncol(t_demo) , part = "body") |>
  hline(i = c(1:2, 6, 10, 21, 32) , part = "body") |> 
  align(align = "center", part = "header") |> 
  align(j = 2:5, align = "center", part = "body") |> 
  bold(i = c(1:2, 3, 7, 11, 22, 33), bold = TRUE, part = "body") |> 
  italic(i = c(6, 10, 21, 32), italic = TRUE, part = "body") |> 
  merge_h(i = 1)

  
# -------- Table 6: Detailed Recruitment Information --------

t_recruitment <- dat |>
  rowwise() |>
  mutate(screen_race_eligible = if_any(c(screen_race_1, screen_race_2, screen_race_3, screen_race_4), 
                                       ~ .x == 1)) |>
  ungroup() |> 
  # Ensure grouping is done by both site_id and the new age_group column
  group_by(site_id) |>
  # Use summarise() to compute summaries specific to each group
  summarize(
    n_completed_contact_form = sum((age_group == "12-17" & p_contact_form_parent_complete == 2) | 
                                     (age_group == "18+" & contact_form_youth_complete == 2), na.rm = TRUE),
    n_passed_eligibility_screen = sum(eligibility_screen_complete == 2, na.rm = TRUE),
    n_passed_eligibility_survey = sum(eligibility_survey_complete == 2, na.rm = TRUE),
    n_consented = sum((age_group == "12-17" & p_informed_consent_form_parent_complete == 2) | 
                        (age_group == "18+" & informed_consent_form_youth_complete == 2), na.rm = TRUE),
    
    n_not_completed_contact_form = sum(!((age_group == "12-17" & p_contact_form_parent_complete == 2) | 
                                     (age_group == "18+" & contact_form_youth_complete == 2)), na.rm = TRUE),
    n_not_passed_eligibility_screen = sum(!(eligibility_screen_complete == 2), na.rm = TRUE),
    n_not_passed_eligibility_survey = sum(!(eligibility_survey_complete == 2), na.rm = TRUE),
    n_not_consented = sum(!((age_group == "12-17" & p_informed_consent_form_parent_complete == 2) | 
                        (age_group == "18+" & informed_consent_form_youth_complete == 2)), na.rm = TRUE),
    
    n_not_hear_more = sum(!((age_group == "12-17" & p_ps_hear_more == 1) | 
                              (age_group == "18+" & ps_hear_more == 1)), na.rm = TRUE),
    n_not_willing_to_contact = sum(!((age_group == "12-17" & p_ps_willing_to_contact == 1) | 
                                       (age_group == "18+" & ps_willing_to_contact == 1)), na.rm = TRUE),
    n_not_p_ps_signature = sum(age_group == "12-17" & is.na(p_ps_signature)),
    n_not_ps_signature = sum(age_group == "18+" & is.na(ps_signature)),
    n_not_screen_age = sum(!(screen_age >= 12 & screen_age <= 19)),
    n_not_screen_caregiver_present = sum(screen_age >= 12 & screen_age <= 17 & screen_caregiver_present == 0),
    n_not_screen_race_eligible = sum(!(screen_race_eligible == 1)),
    n_not_screen_race_eligible = sum(!(screen_race_eligible == 1)),
    n_not_screen_has_cell_phone = sum(!(screen_has_cell_phone == 1)),
    n_not_screen_speak_read_english = sum(!(screen_speak_read_english == 1)),
    n_seek_therapy = sum(screen_in_treatment == 1 & screen_seek_therapy == 1),
    n_screen_self_repeat = sum(screen_self_repeat == 1),
    n_screen_sibling_repeat = sum(screen_sibling_repeat == 2),
    n_meet_minimal_risk = sum(!(
        eligible_visited_ed == 1 | 
        eligible_hospitalized == 1 | 
        eligible_taken_med == 1 | 
        eligible_received_therapy == 1 | 
        eligible_sought_counseling == 1 | 
        (eligible_anxiety_1 + eligible_anxiety_2 >= 4) | 
        (eligible_depress_1 + eligible_depress_2 >= 4) | 
        (eligible_sch_connect_1 < 3 | eligible_sch_connect_2 < 3) | 
        eligible_self_harm > 0 | 
        eligible_sleep_problem_1 > 1)),
    n_total = n(),
    .groups = 'drop'  # Drop the grouping once summarization is done
  ) |> 
  # Ensure all combinations of site_id and age_group appear in the results
  complete(site_id = c("Harlem", "Kings County"), fill = list(
    n_completed_contact_form = 0, 
    n_passed_eligibility_screen = 0,
    n_passed_eligibility_survey = 0,
    n_consented = 0,
    n_not_completed_contact_form = 0, 
    n_not_passed_eligibility_screen = 0,
    n_not_passed_eligibility_survey = 0,
    n_not_consented = 0,
    n_not_hear_more = 0,
    n_not_willing_to_contact = 0,
    n_not_p_ps_signature = 0,
    n_not_ps_signature = 0,
    n_not_screen_age = 0,
    n_not_screen_caregiver_present = 0,
    n_not_screen_race_eligible = 0,
    n_not_screen_race_eligible = 0,
    n_not_screen_has_cell_phone = 0,
    n_not_screen_speak_read_english = 0,
    n_seek_therapy = 0,
    n_screen_self_repeat = 0,
    n_screen_sibling_repeat = 0,
    n_meet_minimal_risk = 0,
    n_total = 0
  )) 

# Transpose
t_recruitment <- t_recruitment |> 
  t() |> 
  as.data.frame() |> 
  rownames_to_column()

# Rename
t_recruitment$rowname <- c(
  "",
  "Number of youth who completed the contact form",
  "Number of youth who passed the eligibility screen",
  "Number of youth who passed the eligibility survey",
  "Number of families (youth and/or caregivers) who consented",
  
  "Number of youth who DID NOT complete the contact form",
  "Number of youth who DID NOT pass the eligibility screen",
  "Number of youth who DID NOT pass the eligibility survey",
  "Number of youth who DID NOT consent",
  
  "Number of youth who were NOT willing to hear more about the study",
  "Number of youth who were NOT willing to be contacted",
  "Number of parents (with youth aged 12-17) who DID NOT consent in the contact form",
  "Number of youth aged 18-19 who DID NOT consent in the contact form",
  
  "Number of youth who were either below 12 or above 19",
  "Number of youth aged 12-17 whose caregiver was NOT present",
  "Number of youth whose race was NOT Black",
  "Number of youth who DID NOT have a cellphone to receive texts",
  "Number of youth who were NOT able to speak or read in English",
  "Number of youth who has sought therapy in the past week",
  "Number of youth who were currently enrolled in WeCare",
  "Number of youth who had another youth family member in WeCare",
  "Number of youth who DID NOT the minimal risk criteria",
  "Total"
)

t_recruitment <- t_recruitment |> 
  add_row(rowname = "General Recruitment Information", .after = 1) |> 
  add_row(rowname = "Ineligible Based on the First Screen (Information from the Contact Form)", .after = 10) |> 
  add_row(rowname = "Ineligible Based on the Second Screen (Information from Youth Eligibility Screen and Survey)", .after = 15)

# Create flextable
ft_recruitment <- flextable(t_recruitment) |> 
  width(width = 1.5) |>
  width(j = 1, width = 6) |>
  delete_part(part = "header") |>
  hline_top(j = 1:ncol(t_recruitment) , part = "body") |>
  hline(i = c(1, 10, 15, 25) , part = "body") |> 
  align(align = "center", part = "header") |> 
  align(j = 2:3, align = "center", part = "body") |> 
  bold(i = c(1, 2, 11, 16), bold = TRUE, part = "body") |> 
  italic(i = 26, italic = TRUE, part = "body") |> 
  merge_h(i = 1)
