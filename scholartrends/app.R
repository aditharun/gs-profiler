#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            textInput("user_input", "Enter text:"),
            actionButton("enter_button", "Enter")

    ),

        # Show a plot of the generated distribution
        mainPanel(
           verbatimTextOutput("output_text")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    validurl <- reactiveVal(NULL)

    observeEvent(input$enter_button, { 

        prefix_format <- startsWith(input$user_input, "https://scholar.google.com/citations")

        if (prefix_format){
            validurl(input$user_input)
        } else{
            validurl(NULL)
        }

    })

    getData <- reactive({

        url <- validurl()

        if (!is.null(url)){

            id <- gsub(".*user=", "", url)

            start_pos <- str_locate(stringi::stri_reverse(id), "[[:punct:]]+") %>% as_tibble() %>% dplyr::slice(1) %>% pull(start)

            if (!is.na(start_pos)){

                id <- substr(id, 1, nchar(id) - start_pos)

            }
        } else{

            id <- NULL
        }

        id

    })


    
    output$output_text <- renderPrint({

        id <- getData()


        if (!is.null(id)){
            
            system(paste0('python3 analysis.py --subjectid ', id))

            paste("URL processed: ", id)
        }

    })




}

# Run the application 
shinyApp(ui = ui, server = server)



















