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
		 column(6, br(),
		  p(textOutput('txt1')),
		  p(uiOutput('txt2')),
		  withMathJax(
		    '$$ \\rho_{X,Y} = \\sum_{i \\in I} \\sum_{j \\in J} \\frac{(x_i - \\mu_X)}{\\sigma_X} \\frac{(y_j - \\mu_Y)}{\\sigma_Y} P_{X,Y}(x_i,y_j) $$'),
		  p(textOutput('txt3')),
		  p(uiOutput('txt4')),
		  credit1
		))
	  ),
	  tabPanel(textOutput('tab2'), 
		  fluidRow(
			column(width = 2,
			  p(),
			  sliderInput("enex", textOutput('ptosX'), 
						  min = 2, max = 9, value = 5, width='100%'),
			  sliderInput("eney", textOutput('ptosY'), 
						  min = 2, max = 9, value = 5, width='100%'),
			  uiOutput('velo'),
#			  radioButtons("speed", '', choices=c('slow'=1,'medium'=2,'fast'=3), inline=TRUE, selected=2),
			  actionButton("indi", textOutput('Indi')),
			  checkboxInput("probs", textOutput('mostP')),
			  p(),
			  uiOutput('figuras', style="font-size: 80%;"),
			  p(),
			  fileInput("file1", textOutput("choose")),
			  uiOutput("datafile", style="font-size: 80%;")
			),
			column(width = 5,
			  plotOutput("plot1", height = 240, click = 'Mx_1', dblclick = 'Mx_2'),
			  plotOutput("plot2", height = 360, click = 'Cjta_1', dblclick = 'Cjta_2', brush = brushOpts(id='Cjta_rm', resetOnNew = TRUE)),
			  br(), div(textOutput('mus'), style="font-style: italic;")
			),
			column(width = 5,
			  plotOutput("salpica", height = 240),
			  plotOutput("plot4", height = 360, click = 'My_1', dblclick = 'My_2')
			)
		  )
	  ),
	  tabPanel(textOutput('tab3'), 
		  fluidRow(
			column(width = 3,
			  p(),
			  textOutput('txt5'), p(),
			  radioButtons("por", textOutput('by'), choices=c('X', 'Y')),
			  #uiOutput("condPanel")
			  div(textOutput('tomando'), style="font-weight: bold;"),
			  actionButton("menos", "", icon=icon("arrow-left")),
			  span(textOutput("numpg", inline=TRUE), style="font-size: 20px;"),
			  actionButton("mas", "", icon=icon("arrow-right"))
			),
			column(width = 5,
			  plotOutput("leyenda", height = 120),
			  plotOutput("condis", height = 360)
			)
		)
	 )
	)
)
)
