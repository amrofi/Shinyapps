Sys.setlocale("LC_ALL", "es_ES.UTF-8") #to be sure that accents in text will be allowed in plots

library(shiny)
source('../credit.r')

shinyUI(fluidPage(h3(textOutput("titulo")),
  
  absolutePanel(
    top = 0, right = 0, width=80, 
	fluidRow(actionButton("eng", img(src='uk-icon.png'), style="margin:0;padding:0;"), actionButton("spa", img(src='Spain-icon.png'), style="margin:0;padding:0;"))
	),

	fluidRow(
    column(5,
             p(textOutput("txt1")),
			 p(textOutput("txt2")),
			 p(textOutput("txt3")),
			 p(textOutput("txt4")),
			 numericInput("n", textOutput("ene"), min = 10, max = 120, value = 50),
             sliderInput("p", uiOutput('propi'), min = 0.05, max = 0.95, step = 0.01, value = 0.5),
             sliderInput("X", textOutput("simpa"), min = -1, max = 1, step = 0.1, value = 0),
             sliderInput("Co", textOutput("conf"), min = 80, max = 99, value = 95),
             selectInput("method", textOutput("proc"), c('prop.test'=1, 'binom.test'=2)),
			 credit1
	),
	column(3, 
	         uiOutput("explan"),
			 plotOutput("tree"),
			 htmlOutput("tab"),
			 p(textOutput("analis"))
	),
	column(4, plotOutput("prop"), htmlOutput("sum"))
	)
	)
)
