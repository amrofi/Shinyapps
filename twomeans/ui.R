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
    uiOutput("parr1"), p(), 
    uiOutput("parr2"), p(), 
    sliderInput("mu1", 
                textOutput("grA"), 
                min = 30, 
                max = 70, 
                value = 50, step=0.5),
    sliderInput("mu2", 
                textOutput("grB"), 
                min = 30, 
                max = 70, 
                value = 50, step=0.5),
    sliderInput("desv", 
                uiOutput("sig"), 
                min = 1, 
                max = 25, step = 0.25,
                value = 10),
	numericInput("obs", uiOutput("ene"), 10,
             min = 2, max = 100),
	uiOutput("HAlt"),

#	selectInput("lat", "Hipótesis alternativa", c(HTML("&mu;<sub>1</sub>&gt;&mu;<sub>2</sub>")=0,
#		HTML("&mu;<sub>1</sub>&lt;&mu;<sub>2</sub>")=1,HTML("&mu;<sub>1</sub>&ne;&mu;<sub>2</sub>")=2))
		
	credit1,
#    tags$a(href = "http://bioestadistica.upc.edu/", "(C) Bioestadística para No Estadísticos"),
	width = 4
  ),
  
  # Show a plot of the generated distribution
  mainPanel(p(),
    plotOutput("fig", height=580), p(),
	fluidRow(
		column(6,
			uiOutput("hypos")),
		column(6,
			wellPanel(uiOutput("res")))
	),
	width=7
  )
))
