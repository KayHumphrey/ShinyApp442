# Load libraries
library(shiny)
library(dplyr)
library(ggplot2)
library(readr)
library(ggiraph) # For interactive ggplot tooltips
library(DT)
library(bslib)  # For Bootstrap themes

# Load the datasets
accidents <- read_csv("accident.csv")
weather <- read_csv("weather.csv")
drugs <- read_csv("drugs.csv")

# Define UI for application with a Bootstrap theme
ui <- fluidPage(
  theme = bs_theme(bootswatch = "flatly"), # Applying a stylish theme
  
  titlePanel("2021 US Traffic Accident Fatality Data"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Filter Options", icon("filter")),
      # Weather condition selection
      selectInput("weather", "Select Weather Condition:",
                  choices = unique(weather$WEATHERNAME),
                  selected = unique(weather$WEATHERNAME)[1]),
      # Grouping option
      radioButtons("xaxis", "Group Data By:",
                   choices = list("Hour of the Day" = "HOUR", "Day of the Week" = "DAY_WEEKNAME"),
                   selected = "HOUR"),
      # Checkbox for filtering by drug involvement
      checkboxInput("drugsOnly", "Show Only Accidents Involving Drugs", FALSE),
      br(),
      actionButton("reset", "Reset Filters", icon = icon("redo"), class = "btn-warning")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Plots",
                 girafeOutput("fatalityPlot"),
                 plotOutput("timeSeriesPlot")),
        tabPanel("Summary",
                 fluidRow(
                   column(4, div(class = "alert alert-info",
                                 h5("Total Accidents"),
                                 textOutput("totalAccidents"))),
                   column(4, div(class = "alert alert-danger",
                                 h5("Total Fatalities"),
                                 textOutput("totalFatalities"))),
                   column(4, div(class = "alert alert-warning",
                                 h5("Accidents Involving Drugs"),
                                 textOutput("drugInvolved")))
                 )),
        tabPanel("Data Table", DTOutput("accidentTable"))
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Reset filters
  observeEvent(input$reset, {
    updateSelectInput(session, "weather", selected = unique(weather$WEATHERNAME)[1])
    updateRadioButtons(session, "xaxis", selected = "HOUR")
    updateCheckboxInput(session, "drugsOnly", value = FALSE)
  })
  
  # Reactive filtering of weather, accidents, and drugs data
  filtered_data <- reactive({
    data <- weather %>%
      filter(WEATHERNAME == input$weather) %>%
      inner_join(accidents, by = "ST_CASE") %>%
      left_join(drugs, by = "ST_CASE") %>%
      filter(HOUR >= 0 & HOUR <= 23) # Filter hours to 0-23 range
    
    # Filter by drug involvement if checkbox is checked
    if (input$drugsOnly) {
      data <- data %>% filter(!is.na(DRUGSPECNAME))
    }
    data
  })
  
  # Render an interactive bar plot with color-coded fatalities and tooltips
  output$fatalityPlot <- renderGirafe({
    data <- filtered_data()
    
    # Set day of the week as ordered if selected
    if (input$xaxis == "DAY_WEEKNAME") {
      data$DAY_WEEKNAME <- factor(data$DAY_WEEKNAME,
                                  levels = c("Monday", "Tuesday", "Wednesday", "Thursday", 
                                             "Friday", "Saturday", "Sunday"))
    }
    
    # Plot with color-coded bars based on fatalities
    p <- ggplot(data, aes_string(x = input$xaxis, fill = "FATALS > 0")) +
      geom_bar_interactive(aes(tooltip = ..count..), position = "dodge") +
      scale_fill_manual(values = c("TRUE" = "lightgreen", "FALSE" = "red"), labels = c("Fatal", "Non-Fatal")) +
      labs(title = paste("Accidents During", input$weather, "Weather by", 
                         ifelse(input$xaxis == "HOUR", "Hour of Day", "Day of Week")),
           x = ifelse(input$xaxis == "HOUR", "Hour of Day", "Day of Week"),
           y = "Number of Accidents",
           fill = "Accident Severity") +
      theme_minimal() +
      theme(legend.position = "top") +
      if (input$xaxis == "HOUR") {
        scale_x_continuous(breaks = 0:23)
      } else {
        scale_x_discrete(drop = FALSE)
      }
    
    girafe(ggobj = p) # Wrap plot in girafe for interactivity
  })
  
  # Render a cumulative time series plot with smoother
  output$timeSeriesPlot <- renderPlot({
    # Precompute daily accident counts to make them available for smoothing
    data <- filtered_data() %>%
      mutate(Date = as.Date(paste(YEAR, MONTH, DAY, sep = "-"))) %>%
      count(Date)
    
    ggplot(data, aes(x = Date, y = n)) +
      geom_line(color = "green") +
      geom_smooth(color = "darkgreen", se = FALSE, method = "loess") +
      labs(title = paste("Accidents Over Time -", input$weather, "Weather"),
           x = "Date", y = "Number of Accidents") +
      theme_minimal()
  })
  
  # # Render the data table with accident details
  # output$accidentTable <- renderDT({
  #   data <- filtered_data() %>%
  #     select(ST_CASE, WEATHERNAME, HOUR, DAY_WEEKNAME, DRUGSPECNAME, FATALS)
  #   datatable(data, options = list(pageLength = 5))
  # })
  
  ##--------------
  # Render the data table with accident details
  output$accidentTable <- renderDT({
    # Extract the filtered data
    data <- filtered_data()
    
    # Dynamically select columns that exist in the filtered data
    available_columns <- c("ST_CASE", "WEATHERNAME", "HOUR", "DAY_WEEKNAME", "DRUGSPECNAME", "FATALS")
    selected_columns <- available_columns[available_columns %in% names(data)]
    
    # Display the data table with only the available columns
    datatable(data %>% select(all_of(selected_columns)), options = list(pageLength = 5))
  })
  
  ##--------------
  
  # Enhanced summary with conditional formatting for drug involvement
  output$totalAccidents <- renderText({
    data <- filtered_data()
    nrow(data)
  })
  
  output$totalFatalities <- renderText({
    data <- filtered_data()
    sum(data$FATALS, na.rm = TRUE)
  })
  
  output$drugInvolved <- renderText({
    data <- filtered_data()
    sum(!is.na(data$DRUGSPECNAME), na.rm = TRUE)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
