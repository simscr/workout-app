
library(shiny)
library(tidyverse)
library(gt)
library(gtExtras)

googledrive::drive_auth(email = TRUE)

googlesheets4::gs4_auth(token = googledrive::drive_token())

sheet_id <- googledrive::drive_get("workouts")$id

workouts <- googlesheets4::read_sheet(
  ss = sheet_id,
  sheet = "workouts"
)

n_wo <- length(unique(workouts$workout_number))

wo_name <- "Death"


# functions ---------------------------------------------------------------

random_wo <- function() {
  workouts %>%
    mutate(notes = snakecase::to_sentence_case(notes)) %>%
    mutate(
      # across(where(is.list), ~ na_if(., is.null)),
      across(where(is.list), as.character)
    ) %>%
    filter(workout_number == sample(1:n_wo, 1)) %>%
    mutate(across(where(is.character), ~ na_if(., "NULL"))) %>%
    janitor::remove_empty("cols") %>%
    select(-workout_number) %>%
    rename_with(snakecase::to_title_case) %>%
    gt::gt(.) %>%
    gt::sub_missing(everything(),
      missing_text = "---"
    ) %>%
    gtExtras::gt_theme_538()
}


specific_wo <- function(term) {
  workouts %>%
    mutate(notes = snakecase::to_sentence_case(notes)) %>%
    mutate(
      # across(where(is.list), ~ na_if(., is.null)),
      across(where(is.list), as.character)
    ) %>%
    filter(name == term) %>%
    mutate(across(where(is.character), ~ na_if(., "NULL"))) %>%
    janitor::remove_empty("cols") %>%
    select(-workout_number) %>%
    rename_with(snakecase::to_title_case) %>%
    gt::gt(.) %>%
    gt::sub_missing(everything(),
      missing_text = "---"
    ) %>%
    gtExtras::gt_theme_538() %>%
    gt::cols_align(columns = 3, align = "center")
}


# ui ----------------------------------------------------------------------

# Define UI for application that draws a workout
ui <- fluidPage(

  # Application title
  titlePanel("Workout Selector"),

  # Sidebar with a random workout input and a select input
  sidebarLayout(
    sidebarPanel(
      "Selection Options",
      hr(), 
      actionButton("random_button", label = "Random Workout"),
      hr(),
      selectInput("name", label = "Search by Name", choices = unique(workouts %>%
        filter(!is.na(name)) %>%
        pluck("name")))
    ),

    # Show a plot of the generated workout
    mainPanel(
      "Here's your workout!",
      gt_output("workoutTable")
    )
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
  
    # workouts %>%
    #   mutate(notes = snakecase::to_sentence_case(notes)) %>%
    #   mutate(
    #     # across(where(is.list), ~ na_if(., is.null)),
    #     across(where(is.list), as.character)
    #   ) %>%
    #   filter(workout_name == workout_name()) %>%
    #   mutate(across(where(is.character), ~ na_if(., "NULL"))) %>%
    #   janitor::remove_empty("cols") %>%
    #   select(-workout_number) %>%
    #   rename_with(snakecase::to_title_case) %>%
    #   gt::gt(.) %>%
    #   gt::sub_missing(everything(),
    #                   missing_text = "---"
    #   ) %>%
    #   gtExtras::gt_theme_538()
    
  })
  
    
}


# Run the application
shinyApp(ui = ui, server = server)
