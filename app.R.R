#Increase file size limit to 30MB

options(shiny.maxRequestSize = 30*1024^2)

#Load libraries
library(shiny) # Library for shiny applications
library(dplyr) # Library for data manipulations
library(magrittr) # Library for pipe operator
library(ggplot2) # Library for creating graphs
library(shinythemes) # Library for different themes


# Define UI for application
ui <- fluidPage(
  
  #Choosing a different theme
  theme = shinytheme("united"),
  
  # Application title
  titlePanel("COURSE SHINY APP"),
  
  sidebarLayout(
    # Sidebar panel
    sidebarPanel(
      "This is the sidebar panel",
      
      # Input: A simple slider ----
      sliderInput(inputId = "year", label = "Year",
                  min = 2000,
                  max = 2019,
                  value = 2000, 
                  sep = ""),
      
      # Input: A simple drop down list  ----
      selectInput(inputId = "country", label = "Country",
                  choices = ""), # if it was a static expression --> c(unique(course_data$Country))
      
      selectInput(inputId = "city", label = "City",
                  choices = ""),  # if it was a static expression --> c(unique(course_data$City))
      
      # Input: A simple text input  ----
      textInput(inputId = "text_input", label = "Input text here:"),
      
      # Input: A simple radio button input  ----
      radioButtons(inputId = "temperature", label = "Temperature",
                   choices = list("Fahrenheit" = "F", "Celsius" = "C"),
                   selected = "F"),
      
      
      actionButton(inputId = "button", label = "Go!"),
      
      fileInput(inputId = "file", label = "Upload a file (RDS)",
                multiple = F,
                accept = c(".rds")),
      
      downloadButton(outputId = "download", label = "Download")
      
    ),
    
    # Main panel
    mainPanel(
      strong("This is the main panel"),
      hr(),
      
      textOutput(outputId = "text_output"),
      
      # Layout: Tabset with info, data, and plots tabs ----
      tabsetPanel(type = "tabs",
                  tabPanel(title = "Info",
                           h3("App description"),
                           p(
                           "This is the course shiny app.", br(), 
                           "It is created during the course exercises using the europe.rds data:", br(),
                           strong("Average daily temperatures (in Fahrenheit)"), "from cities around
                           Europe from 2000 to 2019"),
                           hr(),
                           
                           verbatimTextOutput("data_summary")
                  ),
                  tabPanel(title = "Data",
                           
                           dataTableOutput("data_table")
                  ),
                  tabPanel(title = "Plots",
                           fluidRow(
                             column(width = 12, plotOutput("lineplot"))
                           ),
                           fluidRow(
                             column(width = 6, plotOutput("boxplot")),
                             column(width = 6, plotOutput("lineplotF"))
                           )
                           
                           
                           
                  )
      )
    )))



# Define server side logic
server <- function(input, output, session) {
  
  
  # course_data <- readRDS("C:/Documents_NoBackup/R Shiny Kurs/Shiny app/Data/europe.rds") %>% # Load the course data
  #   mutate(AvgTemperatureC = round((AvgTemperatureF - 32)*5/9, 1)) # Create a new column with Avg Temperature in Celsius
  # 
  
  
  course_data <- eventReactive(input$file, {
    readRDS(input$file$datapath) %>% # Load the course data
         mutate(AvgTemperatureC = round((AvgTemperatureF - 32)*5/9, 1))
    
  })
  
  
  #Here we define the reactive data frames  --> there are two possibilities: a reactive expression and an eventReactive expression (with action button)
  #Important: if a data frame relies on a previous reactive data frame, then () must be behind the dataframe
  #Important: the input$Name is case sensitive, the same Name as defined in the ui (inputId = "Name") must be inserted
  
  country_df <- reactive({
    course_data() %>%
      filter(Year >= input$year) %>% # Subset the rows to keep data more than or equal to a year
      filter(Country == input$country) # Subset the rows to keep a specific country
  })
  
  #Here we create an event Reactive expression using the action button in the ui
  city_df <- eventReactive(input$button, {
    country_df() %>% 
      filter(City == input$city) %>% # Subset the rows for specific City
      filter(Year == input$year) # Subset the rows for specific Year
  })
  
  
  year_df <- eventReactive(input$button, {
    country_df() %>% 
      filter(City == input$city) %>% # Subset the rows for specific City
      filter(Year == input$year) %>%  # Subset the rows for specific Year
      group_by(Country, City, Year, Month) %>% 
      summarise(MinTempF = min(AvgTemperatureF),
                MeanTempF = round(mean(AvgTemperatureF), 1),
                MaxTempF = max(AvgTemperatureF),
                MinTempC = min(AvgTemperatureC),
                MeanTempC = round(mean(AvgTemperatureC), 1),
                MaxTempC = max(AvgTemperatureC)) %>% 
      ungroup()
    
  })
  
  
  
  #Here we create the outputs (e.g. text, plots, tables, models)    
  
  # Output: Render a text output  ----
  output$text_output <- renderText({
    paste("Your inputs are:", input$year, input$country, input$city, input$text_input, input$temperature)
  })
  
  # Output: Render a print output  ----
  output$data_summary <- renderPrint({
    summary(course_data())
  })
  
  # Output: Render a (dynamic) table output  ----
  output$data_table <- renderDataTable({
    city_df()
  })
  
  # Output: Render a plot output  ----
  output$lineplot <- renderPlot({
    ggplot(data = city_df()) +
      geom_line(mapping = aes(x = Date, y = AvgTemperatureF), size = 1) +
      ylab("Average daily temperatures (in Fahrenheit)")
  })
  
  # Output: Render a plot output  ----
  output$boxplot <- renderPlot({
    ggplot(data = country_df()) +
      geom_boxplot(mapping = aes(x = Month, y = AvgTemperatureF, group = Year))
  })
  
  # Output: Render a plot output  ----
  output$lineplotF <- renderPlot({
    ggplot(data = year_df()) +
      geom_line(mapping = aes(x = Month, y = MinTempF), size = 1, colour = "red", linetype = "dotted") +
      geom_line(mapping = aes(x = Month, y = MeanTempF), size = 1, colour = "black") +
      geom_line(mapping = aes(x = Month, y = MaxTempF), size = 1, colour = "red", linetype = "dotted") +
      scale_x_discrete(name = "", limits = month.abb) +
      ylab("Average daily temperatures (in Fahrenheit)")
  })
  
  # Output: Render a plot output  ----
  output$lineplotC <- renderPlot({
    ggplot(data = year_df()) +
      geom_line(mapping = aes(x = Month, y = MinTempC), size = 1, colour = "red", linetype = "dotted") +
      geom_line(mapping = aes(x = Month, y = MeanTempC), size = 1, colour = "black") +
      geom_line(mapping = aes(x = Month, y = MaxTempC), size = 1, colour = "red", linetype = "dotted") +
      scale_x_discrete(name = "", limits = month.abb) +
      ylab("Average daily temperatures (in Celsius)")
  })
  
  #Here we put in an observer to facilitate the choices in the Sidebar panel
  observe({
    
   new_choices <-  unique(course_data()$City[course_data()$Country == input$country])
   
   updateSelectInput(session, inputId = "city", choices = new_choices)
   
   })
  
  observe({
    new_choices <- unique(course_data()$Country)
    updateSelectInput(session, inputId = "country", choices = new_choices)
  })
  
  
  output$download <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv2(city_df(), file = file, row.names=FALSE)
    }
  )
  
  
  
  
}

# Run the application 

runApp(shinyApp(ui, server), launch.browser = TRUE)

#shinyApp(ui = ui, server = server)