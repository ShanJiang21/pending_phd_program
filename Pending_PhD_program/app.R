#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shiny)
library(dplyr, warn.conflicts = FALSE)
library(tidyverse)
library(plotly)
library(shinythemes)
#sessionInfo()

phd_data = read_csv(file = "./grad_program_pause_2021 - Sheet1.csv") %>% 
    janitor::clean_names() %>% 
    mutate(discipline = gsub(" Ph.D.*","",programs))


names(phd_data)[7] <- "link"
names(phd_data)[4] <- "cycle_status_2021"


## For plotly graphs
count_school = phd_data %>% 
    filter(cycle_status_2021 == c("Suspend","Downsize")) %>% 
    count(schools) %>% 
    mutate(schools = fct_reorder(schools, n))


ui <- fluidPage(
    # Application Title 
    titlePanel("Updated List of Programs NOT Accepting Applicants in 2020-2021 Cycle"),
    
    sidebarLayout( 
        sidebarPanel(
            # Input
            selectInput("schools", "Schools", choices = unique(phd_data$schools)),
            selectInput("discipline", "Discipline", choices = NULL),
            selectInput("programs", "Programs", choices = NULL)
        ),
        
        mainPanel(
            tabsetPanel(
                tabPanel("Schools", 
                         plotlyOutput('plot1', height = "750px", width = "900px"),
                         #textOutput("Colleges that are not accepting the Chart")
                ),
                tabPanel("Disciplines", 
                         plotlyOutput('plot2', height = "700px", width = "900px"),
                         #textOutput("Disciplines that are affected")
                ),
                tabPanel("Detailed Data of the program", 
                         tableOutput("data"))
                
            )
        )
    )
)

server <- function(input, output, session) {
    schools <- reactive({
        filter(phd_data, schools == input$schools)
    })
    observeEvent(schools(), {
        choices <- unique(schools()$discipline)
        updateSelectInput(session, "discipline", choices = choices) 
    })
    
    detaillist <- reactive({
        req(input$discipline)
        filter(schools(), discipline == input$discipline)
    })
    observeEvent(detaillist(), {
        choices <- unique(detaillist()$programs)
        updateSelectInput(session, "programs", choices = choices)
    })
    
    output$data <- renderTable({
        req(input$programs)
        detaillist() %>% 
            filter(programs == input$programs) %>% 
            select(link,funding,cycle_status_2021, greetings)})
    
    output$plot1 <- renderPlotly({
        plot_ly(data = count_school, 
                x = ~schools, 
                y = ~n, 
                color = ~schools, 
                type = "bar")
    })
    
    output$plot2 <- renderPlotly({
        phd_data %>% 
            count(discipline) %>% 
            #filter(n >1) %>% 
            mutate(discipline = fct_reorder(discipline, n)) %>% 
            plot_ly(x = ~discipline, y = ~n, color = ~discipline, type = "bar")
    })
    
}

# Create Shiny object
shinyApp(ui, server)

# Run the application 
shinyApp(ui = ui, server = server)
