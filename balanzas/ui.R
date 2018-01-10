Sys.setlocale("LC_ALL", "es_ES.UTF-8") #to be sure that accents in text will be allowed in plots
library(shiny)
source('../credit.r')

# Define UI for slider demo application
shinyUI(fluidPage(fluidPage(h3(textOutput("titulo")),

  absolutePanel(
    top = 0, right = 0, width=80, 
	fluidRow(actionButton("eng", img(src='uk-icon.png'), style="margin:0;padding:0;"), actionButton("spa", img(src='Spain-icon.png'), style="margin:0;padding:0;"))
	),

	fluidRow(withMathJax(),
	column(3, br(),
	       uiOutput("par1"), p(),
		   uiOutput("par2"), p(),
		   sliderInput("ene", textOutput("num"), 
	              min = 3, max = 9, value = 4, width='95%'),
		   p(textOutput("pesa")),
		   credit1
		),
	column(4, 
		   plotOutput("pesadas", height=600, , clickId='P'),
		   actionButton("calc", textOutput("calcula")),
		   actionButton("reset", textOutput("borra")),
		   actionButton("set", textOutput("marca"))
		),
	column(3,
	       tableOutput("orig"),
		   plotOutput("barp", height=250),
		   h4(textOutput('dets')),
		   uiOutput('txt1'),
		   uiOutput('txt2')
		)
	)
  )
))
