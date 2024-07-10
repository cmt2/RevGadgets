# Load necessary libraries
library(shiny)
library(jsonlite)
library(dplyr)
library(knitr)
library(kableExtra)

# Function to read and parse JSON lines file
readAndParseJSON <- function(file) {
  # Function to safely parse each line of JSON
  parse_json_safe <- function(line) {
    tryCatch(
      fromJSON(line, simplifyVector = FALSE),
      error = function(e) {
        message("Error parsing line: ", line)
        NULL
      }
    )
  }
  
  # Read JSON lines file line by line
  json_lines <- readLines(file)
  
  # Parse each line of JSON data
  parsed_data <- lapply(json_lines, function(line) {
    parse_json_safe(line)
  })
  
  # Filter out any NULL values that failed to parse
  parsed_data <- compact(parsed_data)
  
  # Convert list columns to HTML formatted strings with divs
  parsed_data <- lapply(parsed_data, function(row) {
    row <- lapply(row, function(value) {
      if (is.list(value)) {
        # Create a div for each value
        paste(sapply(unlist(value), function(v) {
          paste0("<div style='border: 1px solid #000; padding: 2px; margin: 2px; display: inline-block;'>", v, "</div>")
        }), collapse = "")
      } else {
        value
      }
    })
    return(row)
  })
  
  # Combine all parsed JSON objects into a single data frame
  df <- bind_rows(parsed_data)
  
  return(df)
}

# UI
ui <- fluidPage(
  titlePanel("JSON Log Viewer"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Choose JSON Log File",
                accept = c(".json")),
      tags$hr(),
      checkboxInput("header", "Header", TRUE)
    ),
    mainPanel(
      tableOutput("contents")
    )
  )
)

# Server
server <- function(input, output) {
  output$contents <- renderTable({
    req(input$file1)
    file <- input$file1$datapath
    parsed_df <- readAndParseJSON(file)
    kable(parsed_df, format = "html", escape = FALSE) %>%
      kable_styling("striped", full_width = FALSE)
  }, sanitize.text.function = function(x) x)
}

# Run the application 
shinyApp(ui = ui, server = server)
