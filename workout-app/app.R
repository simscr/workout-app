
# library(shiny)
library(tidyverse)
library(gt)
library(gtExtras)
library(rio)
library(here)
library(reactable)

# options(
#   gargle_oauth_email = TRUE,
#   gargle_oauth_cache = "workout-app/.secrets"
# )
#
# googledrive::drive_auth(email = TRUE)
#
# googlesheets4::gs4_auth(token = googledrive::drive_token())
#
# sheet_id <- googledrive::drive_get("workouts")$id
#
# workouts <- googlesheets4::read_sheet(
#   ss = sheet_id,
#   sheet = "workouts"
# )
#
# rio::export(workouts, here::here("workout-app", "data", "workouts.rds"))

workouts <- rio::import(here::here("data", "workouts.rds"))  # add "workout-app" if running locally

n_wo <- length(unique(workouts$workout_number))

# ui ----------------------------------------------------------------------

# Define UI for application that draws a workout
ui <- navbarPage(theme = shinythemes::shinytheme("united"),
  "Workout Selector",
  tabPanel("Random Workout",

    # Sidebar with a random workout input and a select input
    sidebarLayout(
      sidebarPanel(
        actionButton("random_button", label = "Random Workout")
      ),

      # Show a plot of the generated workout
      mainPanel(
        "Here's your workout!",
        gt_output("workoutTable")
      )
    )
  ),
  tabPanel("All Workouts",
    mainPanel(reactable::reactableOutput("allTable"))
  )
)

# Define server logic required to draw a workout
server <- function(input, output) {
  random_number <- eventReactive(input$random_button, {
    sample(1:n_wo, 1)
  })

  workout_name <- eventReactive(input$name, valueExpr = "Death")


  # workout_choice <- eventReactive(input$random_button, {sample(1:n_wo)})

  output$workoutTable <- render_gt({
    workouts %>%
      mutate(notes = snakecase::to_sentence_case(notes)) %>%
      mutate(
        # across(where(is.list), ~ na_if(., is.null)),
        across(where(is.list), as.character)
      ) %>%
      filter(workout_number == random_number()) %>%
      mutate(across(where(is.character), ~ na_if(., "NULL"))) %>%
      janitor::remove_empty("cols") %>%
      select(-workout_number) %>%
      rename_with(snakecase::to_title_case) %>%
      gt::gt(.) %>%
      gt::sub_missing(everything(),
        missing_text = "---"
      ) %>%
      gtExtras::gt_theme_538()
  })

  output$allTable <- reactable::renderReactable({
    reactable::reactable(workouts, # groupBy = c("workout_number", "name")
                         filterable = TRUE,
                         highlight = TRUE)  
  })
}


# Run the application
shinyApp(ui = ui, server = server)
