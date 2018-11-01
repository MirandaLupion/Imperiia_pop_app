library(tidyverse)
library(readr)
library(shiny)
library(stringr)
library(rebus)

pop <- read_csv("VoennoStatObozrenie_Population_DistrictLevel_1846_DG23Aug2018 - Uezd Level GazIDs.csv") 

pop$TotalPop <- str_replace_all(pop$TotalPop, pattern = fixed(","), replacement = "") 
pop$Male <- str_replace_all(pop$Male, pattern = fixed(","), replacement = "") 
pop$Female <- str_replace_all(pop$Female, pattern = fixed(","), replacement = "") 
pop$City <- str_replace_all(pop$City, pattern = fixed(","), replacement = "") 
pop$Rural <- str_replace_all(pop$Rural, pattern = fixed(","), replacement = "") 
pop$TotalPop <- as.numeric(pop$TotalPop)
pop$Male <- as.numeric(pop$Male)
pop$Female <- as.numeric(pop$Female)
pop$City <- as.numeric(pop$City)
pop$Rural <- as.numeric(pop$Rural)
pop$GazID <- as.character(pop$GazID)
pop$District <- str_trim(pop$District)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Population Data from VSO"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        h3("Select the inputs"),
        selectInput(inputId = "y", #internal label 
                    label = "Select population to graph", #label that user sees
                    choices = c("Total population" = "TotalPop", 
                                "Male population" = "Male", 
                                "Female population" = "Female",
                                "Urban population" = "City",
                                "Rural population" = "Rural"), #vector of choices for user to pick from 
                    selected = "TotalPop"),
        
        selectizeInput(inputId = "d", #internal label
                       label = "Select districts to include", #label that user sees
                       choices = c(pop$District), #choose from this list 
                       multiple = TRUE, # can choose multiple 
                       options = list(maxItems = 5))), #can choose up to five
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("plot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  pop_subset <- reactive({
    req(input$d)
    filter(pop, District %in% input$d) })
  
 output$plot <- renderPlot({
  ggplot(data = pop_subset(), aes_string(x = "District", y = input$y, fill = "District")) + #plot year on x and value on y
          geom_col() + #color by region
          labs(x = "District", y = "Population") +
          scale_fill_discrete(name = "Districts")
})}

# Run the application 
shinyApp(ui = ui, server = server)

