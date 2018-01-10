Sys.setlocale("LC_ALL", "es_ES.UTF-8") #to be sure that accents in text will be allowed in plots
library(shiny)
source('../credit.r')

# Define UI for slider demo application
shinyUI(fluidPage(h3(textOutput("titulo")),
  tags$head(
    tags$style(HTML('#atras{background-color:aquamarine}')),
    tags$style(HTML('#tous{background-color:darkorange}'))
  ),

  absolutePanel(
    top = 0, right = 0, width=80, 
	fluidRow(actionButton("eng", img(src='uk-icon.png'), style="margin:0;padding:0;"), actionButton("spa", img(src='Spain-icon.png'), style="margin:0;padding:0;"))
	),
    withMathJax(),
	
  tabsetPanel(type = "tabs", 
	tabPanel(textOutput("tab1"), 	
		fluidRow(
			column(6, 
			  p(), uiOutput('txt1'),
			  uiOutput('cab2'), uiOutput('txt2'),
			  uiOutput('cab3'), textOutput('txt3'),
			  uiOutput('cab4'), textOutput('txt4'),
			  credit1
			),
			column(6, 
			  p(), div(textOutput('ejem1')), img(src = "popu1.png"),
			  div(textOutput('ejem2')), img(src = "select.png"),
			  div(textOutput('ejem3')), img(src = "popu2.png"),
			  div(textOutput('ejem4')), img(src = "popu3.png")
			)
		)
  ),
	tabPanel(textOutput("tab2"), 
	fluidRow(
	  column(7,
	     plotOutput('map', click='clic', brush = brushOpts(id='zoom', resetOnNew = TRUE), height=500),
	     splitLayout( actionButton('atras', textOutput("back")), textOutput('count'), cellWidths = c('17%', '83%') ), p(),
		 uiOutput('controls')
	  ),
	  column(5,
	     plotOutput('escala'),
		 uiOutput('tipochart'),
		 uiOutput('CI')
	  )
   )
  ),
	tabPanel(textOutput("tab3"),  # tableOutput('resu'),
		plotOutput('summa', width='80%'),
		fluidRow( column(7, uiOutput('explic') ) )
  )
 )
))

