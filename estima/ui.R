Sys.setlocale("LC_ALL", "es_ES.UTF-8") #to be sure that accents in text will be allowed in plots

library(shiny)
source('../credit.r')

# Define UI for slider demo application
shinyUI(fluidPage(h3(textOutput("titulo")),
  
  absolutePanel(
    top = 0, right = 0, width=80, 
	fluidRow(actionButton("eng", img(src='uk-icon.png'), style="margin:0;padding:0;"), actionButton("spa", img(src='Spain-icon.png'), style="margin:0;padding:0;"))
	),
  
  withMathJax(),
  
  tabsetPanel(type = "tabs", 
	tabPanel(textOutput("tab1"), 
		fluidRow(
		 column(3, br(),
		    uiOutput("txt1"),
			p(textOutput("txt2")),
			uiOutput("txt3"), p(),
			uiOutput('viento'),
			sliderInput("wind", textOutput("vmax"), min=0.5, max=20, value=10),
		    numericInput("ene", textOutput("enelab"), 20, min=10, max=75),
			credit1
		),
		column(3,
		   plotOutput("dist", height=270),
		   htmlOutput("statW")
		),
		column(5,
		   plotOutput("estim", height=480),
		   fluidRow( 
		        column(6, p(), p(textOutput("stat"))),
		        column(6, htmlOutput("statT"))
		   )
		)
	  )
	),
	tabPanel(textOutput("tab2"), 
		fluidRow(
		 column(3, br(),
		   uiOutput("txt4"), p(),
		   p(textOutput("txt5")),
		   uiOutput("txt6"), p(),
		   p(textOutput("txt7"))
		 ),
		 column(8,
		   plotOutput("tipos", height=160),
		   withMathJax(),
		   fluidRow( 
		        column(4, p(uiOutput('propA'))),
		        column(4, p(uiOutput('propB'))),
		        column(4, p(uiOutput('propC')))
		   )
         )            
	 )
  ))
))
			