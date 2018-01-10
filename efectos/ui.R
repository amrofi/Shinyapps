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
  
  tabsetPanel(type = "tabs", 
	tabPanel(textOutput("tab1"), 
  
	  sidebarPanel(
		p(textOutput("txt1")),
		tags$ol(tags$li(textOutput("ite1")),
			tags$li(textOutput("ite2")),
			tags$li(textOutput("ite3"))),
			p(textOutput("txt2")),
		sliderInput("pre", textOutput("expe"), min = 0, max = 100, value = 75),
		sliderInput("sens", textOutput("pote"), min = 0, max = 100, value = 80),
		sliderInput("esp", "1-α", min = 0, max = 100, value = 95),
		
		tags$a(href = "http://bioestadistica.upc.edu/", 
			tags$img(src = "http://www-eio.upc.es/teaching/best/Logos/logo_bne_tiny.png",  style="width:60px;height:60px;border:0")), p(),
		tags$em("(C) 2015. Bioestadística para No Estadísticos"),
		credit1,
		width = 4
	  ),
	  
	  # Show a plot of the generated distribution
	  mainPanel(
		plotOutput("distPlot",height = "800px")
	  )
	),
	tabPanel(textOutput("tab3"), 
	  sidebarPanel(
		uiOutput('XX'),
		checkboxInput('anima', textOutput("grAnim")),
		width = 4
	  ),
	  
	  # Show a plot of the curve
	  mainPanel(
		plotOutput("cred"),
		uiOutput('regla')
	  )
	),
	tabPanel(textOutput("tab2"), 
		fluidRow(	column(1), column(6, 
			p(textOutput('intro')),
			uiOutput('ioann'),
			p(), HTML('<em>Ioannidis JP. Why most published research findings are false. Plos Med. 2005; 2(8):e124.</em>')
		))
	)
  )
))
