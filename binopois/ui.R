Sys.setlocale("LC_ALL", "es_ES.UTF-8") #to be sure that accents in text will be allowed in plots
library(shiny)
source('../credit.r')

# Define UI for application that plots random distributions 
shinyUI(pageWithSidebar(

  absolutePanel(
    top = 0, right = 0, width=80, 
	fluidRow(actionButton("eng", img(src='uk-icon.png'), style="margin:0;padding:0;"), actionButton("spa", img(src='Spain-icon.png'), style="margin:0;padding:0;"))
	),
	
  # Sidebar with a slider input for number of observations
  sidebarPanel(h3(textOutput("titulo"), style="padding-top:0; padding-bottom:20px;"),
    p(),
    p(textOutput("par1")), 
    uiOutput("par2"), p(),
    p(textOutput("par3")), 
    sliderInput("n", 
                uiOutput("ene"), 
                min = 10, 
                max = 200, 
                value = 100),
    sliderInput("p", 
                uiOutput("pe"), 
                min = 0.01, 
                max = 0.5, step = 0.01,
                value = 0.05),
	credit1,
	width = 3
  ),
  
  # Show a plot of the generated distribution
  mainPanel(p(),
    plotOutput("graf")
  )
))
