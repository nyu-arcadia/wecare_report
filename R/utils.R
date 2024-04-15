# Function to get week range from Monday to Sunday
get_week_range <- function(dates) {
  # Ensure the input is in Date format
  dates <- as.Date(dates)
  
  # Calculate the start of the week (Monday)
  week_starts <- dates - wday(dates, week_start = 1) + 1
  
  # Calculate the end of the week (Sunday)
  week_ends <- week_starts + 6
  
  # Format the week range as a string
  week_ranges <- paste(week_starts, "~", week_ends)
  
  return(week_ranges)
}

# Function to calculate week number
calculate_week_number <- function(dates,
                                  project_start_date = "2024-04-08") {
  
  dates <- gsub("(.*)( ~ )(.*)", "\\1", dates)
  
  # Calculate the difference in days from the project start date
  days_difference <- as.Date(dates) - as.Date(project_start_date)
  
  # Calculate which week the dates fall into
  week_number <- as.integer(days_difference) %/% 7 + 1
  
  return(week_number)
}

# Function to transform columns based on their prefix and total column
transform_columns <- function(df, prefix, total_col) {
  df %>%
    mutate(across(
      starts_with(prefix),
      ~ ifelse(is.na(.x), NA, paste0(.x, " (", round((.x / .data[[total_col]]) * 100, 2), "%)")),
      .names = "{.col}"
    ))
}
