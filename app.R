library(shiny)
library(shinydashboard)

load("data/ngrams_and_profanities.RData", envir=.GlobalEnv)
source("nextword.R")

ui <- dashboardPage(title= "Coursera Data Science Capstone",
                 dashboardHeader(title = "Coursera Data Science Capstone - Final Project",
                   titleWidth = 450
                 ),
                 dashboardSidebar(disable = TRUE),

      dashboardBody( width=120,
                     img(src = "headers.png"),
                    div(style="padding-top:20px",""),
                 tabsetPanel(type = "tabs",
                            
                             tabPanel("Classic",
                                      br(),
                                      
                                      fluidRow(
                                        column(6,
                                               "Enter the word or partial phrase here",
                                               
                                               textInput("phrase", label = "", value = ""),
                                               tags$head(tags$style(type="text/css", "#phrase {width: 300px;}")),
                                        column(12,
                                               actionButton("goButton", "Click to Predict the next word"),
                                               br(), br()
                                        ),
                                        column(12,
                                               p(textOutput("stats")),
                                               h3("The suggested next word for your word or phrase is"),
                                               box(height = "50px",textOutput("nextword"))
                                        ))
                                        ,
                                        column(6,
                                               wellPanel(
                                                 #h4("How to use this?"),
                                                 h3("Overview of the Project"),
                                                 h5("
                                                  The purpose of the project is to create text-prediction application with R Shiny package that predicts words using a natural language processing model.
                                                 "),
                                                 h5("Given a word or phrase as input, the application will try to predict the next word, similar to the way most smart phone keyboards are implemented today using the technology of Swiftkey.
                                                 "),
                                                 
                                                 h5("This app provides a simple user interface to the next word prediction model. The app takes as input a phrase (multiple words) in a text box input and outputs a prediction of the next word.
                                                 "),
                                                  p("If you're in 'Classic' tab, click the button.
                                                   In 'Instant' tab, a word should appear by itself.")
                                                 
                                                 
                                                 )
                                        )
                                      )
                                      
                             ),
                             tabPanel("Instant",
                                      br(),
                                      "Enter the word or partial phrase here",
                                      textInput("phrase2", label = "", value = ""),
                                      tags$head(tags$style(type="text/css", "#phrase2 {width: 300px;}")),
                                      
                                      fluidRow(
                                        column(6,
                                            
                                            column(12,
                                                   p(textOutput("stats2")),
                                                   p("Predicted next word is..."),
                                                   box(height = "50px",(textOutput("nextword2")))
                                            )
                                        ),
                                        column(6,
                                               wellPanel(
                                                 #h4("How to use this?"),
                                                 h3("Overview of the Project"),
                                                 h5("
                                                    The purpose of the project is to create text-prediction application with R Shiny package that predicts words using a natural language processing model.
                                                    "),
                                                 h5("Given a word or phrase as input, the application will try to predict the next word, similar to the way most smart phone keyboards are implemented today using the technology of Swiftkey.
                                                    "),
                                                 
                                                 h5("This app provides a simple user interface to the next word prediction model. The app takes as input a phrase (multiple words) in a text box input and outputs a prediction of the next word.
                                                    "),
                                                 p("If you're in 'Classic' tab, click the button.
                                                   In 'Instant' tab, a word should appear by itself.")
                                                 
                                                 
                                                 )
                                                 )
                                      )
                             )
                 )
                 
      )

)

nextw <- function(phrase, lang, safemode) {
    return(StupidBackoff(phrase, removeProfanity=TRUE))
  
}

server <- function(input, output,session) {
  
  phraseGo <- eventReactive(input$goButton, {
    input$phrase
  })
  output$stats <- renderText({
    numword <- length(strsplit(input$phrase," ")[[1]])
    numchar <- nchar(input$phrase)
    paste("You've written -", input$phrase)
  })
  output$nextword <- renderText({
    result <- nextw(phraseGo(), input$lang, input$safemode)
    # result <- nextw(input$phrase, input$lang)
    paste0(result)
  })
  output$stats2 <- renderText({
    numword <- length(strsplit(input$phrase2," ")[[1]])
    numchar <- nchar(input$phrase2)
    paste("You've written -", input$phrase2)
  })
  output$nextword2 <- renderText({
    result <- nextw(input$phrase2, input$lang, input$safemode)
    if(length(input$phrase2)==0)
    {
      return(NULL)
    }
    paste0(result)
  })
  
}



shinyApp(ui = ui, server = server)

