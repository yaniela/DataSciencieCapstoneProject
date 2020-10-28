# Load libraries ---------------------------------------------------------------
library(shiny)
library(shinythemes)

library(knitr)
library(config)
library(rCharts)
library(leaflet)
library(plotly)
library(DT)


# Adjustments
h3.align <- 'center'

# Shiny UI ---------------------------------------------------------------------
shinyUI(
    navbarPage(
    title = span( "Next Word Prediction",  style="color:black" ),

    helpText("This app takes an input phrase (multiple words)  and outputs a prediction of the next word."),
    
    div(style = "margin-top:15px"),
    
    # Pick a bootstrap theme from https://rstudio.github.io/shinythemes/
    theme = shinytheme("spacelab"),
    
    # Analytics tab panel --------------------------------------------------        
    sidebarLayout(
        sidebarPanel(
            h2("Instructions"),
            "1. Enter a word or phrase in the text input.",
            div(style = "margin-top:8px"),
            "2. No need to press enter or submit.",
            div(style = "margin-top:8px"),
            "3. The top five predicted next word prints below.",
            div(style = "margin-top:8px"),
            "4. The table provide the predicted words, its history,
                       the Katz probability, the GoodTuring frequencies, and the GoodTuring probability for each word.",
            div(style = "margin-top:8px"),
            "5. A message appears in red color if there is no prediction because it is an unknown word. .",
            div(style = "margin-top:8px"),
            "5. The additional tab called: N-grams, show plots for all predicted words by 4,3,2,1 ngrams that match with the text input entered by the user.",
            
            
          ),
       
        
        mainPanel(
           
            tags$style(type="text/css",
                       ".shiny-output-error { visibility: hidden; }",
                       ".shiny-output-error:before { visibility: hidden; }"
            ),
            tabsetPanel(
            # Predict Tab-----------------------------------------
                         tabPanel(
                             p(icon("area-chart"), "Predict"),
                              fluidRow(
                                  tags$head(
                                      tags$style("h3{color: RoyalBlue}")),
                                      
                                  column(6, textInput("text", label = h3("Text input"), value = "")),
                                  
                        
                                   ),
                              hr(),
                            
                              fluidRow( span(textOutput("top_five_pred"), style="color:LimeGreen")),
                              div(style = "margin-top:15px"),
                             
                              fluidRow( textOutput("predictionOutput")),
                            
                              fluidRow( span(textOutput("predictionOutputError"), style="color:red")),
                              
                             hr(),
                             fluidRow( span(textOutput("TablePredict"), style="color:LimeGreen")),
                             tags$hr(style="border-color: white;"),
                             
                              fluidRow(DT::dataTableOutput(outputId="table")),
                             
                        
                         ),  # End Predict Tab

                         # N-grams Tab -----------------------------------------
                         tabPanel(
                             p(icon("bar-chart-o"), "N-grams"),
                             hr(),
                             div(style = "margin-top:20px"),
                             tags$style(HTML("
                                             #comentInputText {color: blue;}{ font-size: 0;}
                                             p > * {  font-size: 14px;  }  ")),
                             
                             
                             p(textOutput("comentInputText", inline = TRUE), textOutput("input", inline = TRUE)),
                             
                             hr(),
                             div(style = "margin-top:20px"),
                             tags$style(HTML("
                                             #comentNgrams {color: blue;}{ font-size: 0;}
                                             p > * {  font-size: 14px;  }  ")),
                             
                             p( textOutput("comentNgrams",inline = TRUE), textOutput("ngramstext", inline = TRUE)),
            
                             hr(),
                             
                             fluidRow(
                        
                                column(6, 
                                        p(plotlyOutput("TwoGram",  height = "400px"), align="center")),
                       
                                 column(6, 
                                       p(plotlyOutput("TriGram",  height = "400px"), align="center")),
                      
                                column(6, 
                                       p(plotlyOutput("QuadGram",  height = "400px", width  = "600px"), align="center")),
                        
                             )
                        
                        
                         ) # End N-grams Tab
                      
            )

        )
    )
    
) # End navbarPage
) # End shinyUI






