
# Script to pull in data --------------------------------------------------

pacman::p_load(tidyverse, googlesheets4, googledrive)

# Authorize access to Google Drive for googledrive and googlesheets4
googledrive::drive_auth()

googlesheets4::gs4_auth(token = googledrive::drive_token())

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
  mutate(notes = snakecase::to_sentence_case(notes)) %>%
  mutate(
    # across(where(is.list), ~ na_if(., is.null)),
    across(where(is.list), as.character)
  ) %>%
  filter(workout_number == sample(1:n_wo, 1)) %>%
  mutate(across(where(is.character), ~na_if(., "NULL"))) %>% 
  janitor::remove_empty("cols") %>%
  select(-workout_number) %>% 
  rename_with(snakecase::to_title_case) %>% 
  gt::gt(.) %>%
  gt::sub_missing(everything(),
                  missing_text = "---") %>% 
  gtExtras::gt_theme_538()


