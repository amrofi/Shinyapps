Sys.setlocale("LC_ALL", "es_ES.UTF-8") #to be sure that accents in text will be allowed in plots
library(shiny)
source('../credit.r')

# Define UI for slider demo application
shinyUI(fluidPage(h3(textOutput("titulo")),

  absolutePanel(
    top = 0, right = 0, width=80, 
	fluidRow(actionButton("eng", img(src='uk-icon.png'), style="margin:0;padding:0;"), actionButton("spa", img(src='Spain-icon.png'), style="margin:0;padding:0;"))
	),
  
	fluidRow(
	column(3, 
	  p(textOutput("par1")),
      sliderInput("delta", uiOutput("dife"), 
	              min = -4, max = 4, value = 0, step= 0.05, width='95%'),     
      sliderInput("sg", textOutput("stdev"), 
	              min = 0.1, max = 10, value = 2, step= 0.05, width='95%'),     
      sliderInput("corre", textOutput("corr"), 
	              min = -1, max = 1, value = 0.5, step= 0.02, width='95%'),     
	  p(textOutput("scala")),
	  plotOutput("escala", height=100, clickId='P'),
	  actionButton("reset", textOutput("borra")),p(),
	  helpText(textOutput("ojo")),
	  uiOutput("pot.butt"),
	  credit1
    ),
    column(8, 
	  plotOutput("values", height=550),
	  fluidRow(
	      column(3, tableOutput("data")),
		  column(3, htmlOutput("stat")),
		  column(6, htmlOutput("cons"), 
		            actionButton("mas", textOutput("ran")))
	  )
	)
    )
  )
)

