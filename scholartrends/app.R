library(shiny)
library(tidyverse)
library(scholar)
library(scales)
library(shinydashboard)

source("process-gscholar.R")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Publication Trends"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(width = 12,
            textInput("user_input", "Google Scholar URL:"),
            actionButton("enter_button", "Enter")

    ),

        # Show a plot of the generated distribution

        #width = 12 for full width
        mainPanel(width = 12,
           tags$div(
            style = "font-size: 30px; text-align: center;",
            textOutput("intro")
          ),
           tags$br(), 
           tags$br(),
           plotOutput("ppy"),
           tags$br(),
           plotOutput("jc"),
           tags$br(),
           plotOutput("auth"),
           tags$br(),
           plotOutput("cpy"),
           tags$br(),
           plotOutput("jpy")
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

    sizing_theme <- theme(axis.text = element_text(size=12), axis.title=element_text(size=16), legend.text=element_text(size=14), legend.title=element_text(size=16), plot.title=element_text(size=18, hjust=0.5)) 

    panel_theme <- theme_bw() + theme(panel.grid.major.x = element_blank(), panel.grid.minor=element_blank())


    output$intro <- renderText({

        id <- getData()

        if (!is.null(id)){

            author <- get_profile(id)$name

            author

        }

    })

    output$cpy <- renderPlot({

        id <- getData()

        if (!is.null(id)){
            
            citations_per_year(id, sizing_theme, panel_theme) + theme(plot.title = element_text(size = 18), axis.title = element_text(size=16), axis.text.y = element_text(size = 12)) + ylab("# of citations") + theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10))

        }
    })

    output$ppy <- renderPlot({

        id <- getData()

        if (!is.null(id)){
            
            pubs_per_year(id, sizing_theme, panel_theme) + theme(plot.title = element_text(size = 18), axis.title = element_text(size=16), axis.text.y = element_text(size = 12)) + theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10))

        }
    })


    output$jc<- renderPlot({

        id <- getData()

        if (!is.null(id)){
            
            journal_counts(id, sizing_theme, panel_theme) + ggtitle("Most frequently published journals") + theme(plot.title = element_text(size = 18, hjust = 0.5), axis.title = element_text(size=16), axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 10)) 

        }
    })

    output$jpy <- renderPlot({

        id <- getData()

        if (!is.null(id)){
            
            journal_per_year(id, sizing_theme, panel_theme) + theme(plot.title = element_text(size = 18, hjust = 0.5), axis.title = element_text(size=16), axis.text = element_text(size = 12))

        }
    })

    output$auth <- renderPlot({

        id <- getData()

        if (!is.null(id)){
            
            auth_numbers(id, sizing_theme, panel_theme) + ggtitle("Total # of lead authorships") + theme(plot.title = element_text(size = 18, hjust = 0.5))

        }
    })
    

}

# Run the application 
shinyApp(ui = ui, server = server)



















