Sys.setlocale("LC_ALL", "es_ES.UTF-8") #to be sure that accents in text will be allowed in plots
library(shiny)
source('../credit.r')

# Define UI for application that plots random distributions 
shinyUI(fluidPage(h3(textOutput("titulo")),
  
  absolutePanel(
    top = 0, right = 0, width=80, 
	fluidRow(actionButton("eng", img(src='uk-icon.png'), style="margin:0;padding:0;"), actionButton("spa", img(src='Spain-icon.png'), style="margin:0;padding:0;"))
	),

  # Sidebar with a slider input for number of observations
  fluidRow(
   column(3,
    withMathJax(),
    uiOutput("parraf"), p(),
    sliderInput("aa", 
                textOutput("shape1"), 
                min = -2, 
                max = 2, step = 0.01, 
                value = 0),
    sliderInput("bb", 
                textOutput("shape2"), 
                min = -2, 
                max = 2, step = 0.01,
                value = 0),
    sliderInput("n", 
                uiOutput("ene"), 
                min = 1, 
                max = 40, 
                value = 10),
    uiOutput("pd"),
	credit1
   ),
   column(4,
    plotOutput("graf")
   ),
   column(4,
    plotOutput("graf2"),
    checkboxInput("zoom", "Zoom", FALSE)
   )
  )
))
