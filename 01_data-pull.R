
# Script to pull in data --------------------------------------------------

pacman::p_load(tidyverse, googlesheets4, googledrive)

# Authorize access to Google Drive for googledrive and googlesheets4
googledrive::drive_auth()
gs4_auth(token = drive_token())

# Find the sheet named "workouts"
drive_find("workouts")

# Get drive information about "workouts"
wo <- drive_get("workouts")

# Get Sheets location for "workouts"
gs4_get(wo)

# Load "workouts" sheet from "workouts"
workouts <- read_sheet(wo, sheet = "workouts")

# Pull Random Workout

n_wo <- length(unique(workouts$workout_number))

workouts %>% 
  filter(workout_number == sample(1:n_wo, 1)) %>% 
  janitor::remove_empty("cols") %>% 
  rename_with(str_to_sentence)
  gt::gt(.) %>% 
  gt::cols_label(everything() ~ str_to_title) 
  gtExtras::gt_theme_538()


