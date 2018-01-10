Sys.setlocale("LC_ALL", "es_ES.UTF-8") #to be sure that accents in text will be allowed in plots
library(shiny)
source('../credit.r')

shinyUI(pageWithSidebar(
  
  absolutePanel(
    top = 0, right = 0, width=80, 
	fluidRow(actionButton("eng", img(src='uk-icon.png'), style="margin:0;padding:0;"), actionButton("spa", img(src='Spain-icon.png'), style="margin:0;padding:0;"))
	),
	  
  sidebarPanel(h3(textOutput("titulo"), style="padding-top:0; padding-bottom:20px;"),
  
    uiOutput("parrafo"),

    sliderInput("malla", 
                textOutput("malla"), 
                min = 5, 
                max = 30, 
                value = 15),
    sliderInput("bom", 
                textOutput("bombas"), 
                min = 50, 
                max = 1000, step = 5, 
                value = 200),
    sliderInput("desv", 
                textOutput("dispe"), 
                min = 2, 
                max = 10, step = 0.25,
                value = 7),

	credit1,
	width = 3
  ),
  
  # Show a plot of the generated distribution
  mainPanel(    plotOutput("mapa"),
    flowLayout(
	  uiOutput("explicacion"),
	  tableOutput("comp")
  ))
))
