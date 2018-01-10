Sys.setlocale("LC_ALL", "es_ES.UTF-8") #to be sure that accents in text will be allowed in plots
library(shiny)
source('../credit.r')

shinyUI(fluidPage(h3(textOutput("titulo")),
  
  absolutePanel(
    top = 0, right = 0, width=80, 
	fluidRow(actionButton("eng", img(src='uk-icon.png'), style="margin:0;padding:0;"), actionButton("spa", img(src='Spain-icon.png'), style="margin:0;padding:0;"))
	),
  
  fluidRow(
     column(3,
        p(textOutput("parr")),
		tableOutput("DF"),
		actionButton("newr", textOutput("nrecta")),
		credit1
     ),

     column(4,
	   p(textOutput("marca")),
       plotOutput("fig", height=480, clickId='P'),
	   checkboxInput("go", textOutput("soluc"), value=FALSE),
       textOutput("txt")
     ),

     column(4,
	   p(textOutput("mapa")),
       plotOutput("cont", height=480),
       textOutput("txt2")
     )
  )
))


