Sys.setlocale("LC_ALL", "es_ES.UTF-8") #to be sure that accents in text will be allowed in plots

library(shiny)
source('../credit.r')

shinyUI(fluidPage(h3(textOutput("titulo")),
  
  absolutePanel(
    top = 0, right = 0, width=80, 
	fluidRow(actionButton("eng", img(src='uk-icon.png'), style="margin:0;padding:0;"), actionButton("spa", img(src='Spain-icon.png'), style="margin:0;padding:0;"))
	),
  tabsetPanel(
  tabPanel("Intro",
  fluidRow(
     column(5, 
       p(textOutput('intro1')),
       p(textOutput('intro2')),
	   p(textOutput('intro3')),
	   p(textOutput('intro4')),
	   p(uiOutput('alias'))
	 ), column(1),
	 column(6, img(src='omega.png'))
	)
  ),
  tabPanel(textOutput('play'),
  fluidRow(
     column(3, 
		   uiOutput('shapes'),
		   uiOutput('colors'),
		   sliderInput('ene', textOutput('dim'), min=5, max=25, value=20),
		   uiOutput('condi'),
		   uiOutput("percent")
		,
		credit1
     ),
     column(6,
       plotOutput("fig"),
	   checkboxInput("mess", textOutput('follon')),
	   br(),
	   uiOutput("dat")
     ),
	 column(3, br(), wellPanel(uiOutput("extra")))
  )
  )
  )
))


