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
			column(3, 
			  p(textOutput("par1")),
			  uiOutput("par2"),
			  p(textOutput("par3")),
			  plotOutput("data", height=475, clickId='P'),
			  actionButton("reset", textOutput("borrar")), p(),
			  uiOutput("summ"),
			  credit1
			),
			column(5, 
			  plotOutput("values", height=590, clickId='S'),
			  uiOutput("Type"),
			  uiOutput("Rdos")
			),
			column(4, 
			  uiOutput("par4"), p(),
			  uiOutput("sucu"), p(), 
			  uiOutput("par5"), p(), 
			  uiOutput("par6"), p(), 
			  p(textOutput("par7")),
			  uiOutput("par8"), p(), 
			  uiOutput("tabla")
			)
	   )
  ),
	tabPanel(textOutput("tab2"), 
	fluidRow(
	column(3, 
	    p(textOutput("par9")),
		uiOutput("par10"), p(),
		uiOutput("par11"), p(),
		p(textOutput("par12")), p(),
		htmlOutput("rollo")
	),
	column(9,
	    flowLayout(
		     checkboxInput("iguales", textOutput("igual"), value=TRUE),
				conditionalPanel(condition = "input.iguales==1",
					sliderInput("mu", HTML("&mu;<sub>i</sub>: i=1,2,3"), min=7, max=18, value=12, step=0.1)),
				conditionalPanel(condition = "input.iguales!=1",
					sliderInput("mu1", HTML("&mu;<sub>1</sub>"), min=7, max=18, value=10, step=0.1),
					sliderInput("mu2", HTML("&mu;<sub>2</sub>"), min=7, max=18, value=12, step=0.1),
					sliderInput("mu3", HTML("&mu;<sub>3</sub>"), min=7, max=18, value=14, step=0.1)),
			 sliderInput("desv", uiOutput("sigma"), min = 0.1, max = 15, step = 0.1, value = 5),
			 numericInput("obs", textOutput("tamano"), 30, min = 10, max = 100),
		     actionButton("go", textOutput("simula"))
		),
	    plotOutput("graf")
	)
   )
  )
 )

))

