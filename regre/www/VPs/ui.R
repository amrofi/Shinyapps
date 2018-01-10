Sys.setlocale("LC_ALL", "es_ES.UTF-8") #to be sure that accents in text will be allowed in plots

library(shiny)
source('../credit.r')

# Define UI for application that plots random distributions 
shinyUI(fluidPage(h3(textOutput("titulo")),
  
  tags$head(includeScript("google-analytics.js")),
  absolutePanel(
    top = 0, right = 0, width=80, 
	fluidRow(actionButton("eng", img(src='uk-icon.png'), style="margin:0;padding:0;"), actionButton("spa", img(src='Spain-icon.png'), style="margin:0;padding:0;"))
	),
  
  	withMathJax(),
    tabsetPanel(type = "tabs", 
       tabPanel(textOutput("tab1"), 
		  sidebarPanel(
			sliderInput("pre", textOutput("prev"), 
							min = 0, max = 100, 
							value = 10),
			sliderInput("sens", textOutput("sens"), 
							min = 0, max = 100, 
							value = 50),
			sliderInput("esp", textOutput("espe"), 
							min = 0, max = 100, 
							value = 50),
			uiOutput("Var"),

			tags$a(href = "http://bioestadistica.upc.edu/", 
					tags$img(src = "http://www-eio.upc.es/teaching/best/Logos/logo_bne_tiny.png",  style="width:60px;height:60px;border:0")),
			p(),
			tags$em("(C) 2015. Bioestadística para No Estadísticos"),
			credit1,
			width = 3
		 ),
			  
		  mainPanel(
				plotOutput("distPlot",height = "500px"),
				withMathJax(),
				uiOutput("frmla")
		 )
	  ),
       tabPanel(textOutput("tab2"), 
       fluidRow(column(1), column(4, p(),
		     p(uiOutput("defpr")),
			 p(textOutput("defsn")),
			 p(textOutput("defsp")),
			 p(textOutput("defpos")),
			 p(textOutput("defneg")),
			 p(),
			 tags$a(href = "http://bioestadistica.upc.edu/", 
					tags$img(src = "http://www-eio.upc.es/teaching/best/Logos/logo_bne_tiny.png",  style="width:60px;height:60px;border:0")),
			 p(),
			 tags$em("(C) 2015. Bioestadística para No Estadísticos")
		 ))
	  )
	)
))
