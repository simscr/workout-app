
library(shiny)
library(tidyverse)
library(gt)
library(gtExtras)

googledrive::drive_auth(email = TRUE)

googlesheets4::gs4_auth(token = googledrive::drive_token())

sheet_id <- googledrive::drive_get("workouts")$id

workouts <- googlesheets4::read_sheet(ss = sheet_id,
                       sheet = "workouts") 

n_wo <- length(unique(workouts$workout_number))

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

# Define UI for application that draws a histogram
ui <- fluidPage(

  # Application title
  titlePanel("Workout Selector"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      "Selection Options\n",
      actionButton("random_button", label = "Random Workout"),
      # numericInput(inputId = "rand_no", label = "Random Number", value = sample(1:n_wo, 1)),
      textInput("name", label = "Search by Name")
    ),

    # Show a plot of the generated distribution
    mainPanel(
      "Here's your workout!",
      gt_output("workoutTable")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  random_number <- eventReactive(input$random_button, {
    sample(1:n_wo, 1)
  })
  
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
}

# Run the application
shinyApp(ui = ui, server = server)
