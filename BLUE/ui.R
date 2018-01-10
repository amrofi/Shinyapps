Sys.setlocale("LC_ALL", "es_ES.UTF-8") #to be sure that accents in text will be allowed in plots
library(shiny)
source('../credit.r')

shinyUI(fluidPage(h3(textOutput("titulo")),
  
  absolutePanel(
    top = 0, right = 0, width=80, 
	fluidRow(actionButton("eng", img(src='uk-icon.png'), style="margin:0;padding:0;"), actionButton("spa", img(src='Spain-icon.png'), style="margin:0;padding:0;"))
	),
  
  withMathJax(),
  tabsetPanel(type = "tabs", 

	tabPanel(textOutput('tab1'), 
    fluidRow(
	 column(3,
	    uiOutput('txt1'), p(),
		textOutput('txt2'), p(),
        hr(),
        checkboxInput("rectas", textOutput('super')),
		checkboxInput("show", textOutput('mues'), value=FALSE),
		uiOutput('condpan'),
		credit1
	 ),
     column(3,
        textOutput('txt3'), p(),
        plotOutput("fig", clickId='P'),
		actionButton("reset", textOutput('borra'))
     ),

     column(3,
	   textOutput('txt4'), p(),
       plotOutput("fig2"),
	   p(),
	   plotOutput("resum")
     ),
	 
     column(3,
	   textOutput('txt5'), p(),
       plotOutput("fig3", clickId='Q'), p(), br(),
	   uiOutput('txt6'), p(),
	   uiOutput('txt7'), p()
     )
    )
  ),
	tabPanel(textOutput('tab2'), 
    fluidRow(
     column(3, p(),
	     sliderInput("bet1", uiOutput('beta1'), min = -2, max = 2, value = 0, step=0.05),
         sliderInput("bet0", uiOutput('beta0'), min = -5, max = 5, value = 0, step=0.1),
         numericInput("obs", textOutput('ene'), 20, min = 10, max = 50),
		 selectInput("nor", uiOutput('epsi'), c('Normal'=1, 'Laplace'=2)),
		 uiOutput('conpan2'),
		 actionButton("go", textOutput('simular'))
	 ),
	 column(9,
	   plotOutput("simus"),
	   fluidRow(
	      column(8,
		    uiOutput('sim.1'), p(),
			uiOutput('sim.2'), p(),
			p(textOutput('sim.3')),
			p(textOutput('sim.4'))
		  ),
		  column(4,
		    htmlOutput("stdev"),
			uiOutput("sigm")
		  )
		)
	 )
	)
  )
 )
))

