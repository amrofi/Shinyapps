Sys.setlocale("LC_ALL", "es_ES.UTF-8") #to be sure that accents in text will be allowed in plots

library(shiny)
source('../credit.r')

# Define UI for slider demo application
shinyUI(fluidPage(h3(textOutput("titulo")),
  
  absolutePanel(
    top = 0, right = 0, width=80, 
	fluidRow(actionButton("eng", img(src='uk-icon.png'), style="margin:0;padding:0;"), actionButton("spa", img(src='Spain-icon.png'), style="margin:0;padding:0;"))
	),
  
  tabsetPanel(type = "tabs", 
	tabPanel(textOutput("tab1"), 
		fluidRow(
		 column(3, br(),
		    p(textOutput("par1")),
			p(textOutput("par2")),
		    uiOutput("par3"),
			credit1
		 ),
		 column(4,
		   plotOutput("fig", clickId='P'),
		   actionButton("reset", textOutput("null"))
		 ),
		 column(3, br(),
		   p(textOutput("par4")),
		   htmlOutput("tab")
		 )
		)
	),
	tabPanel(textOutput("tab2"), 
		fluidRow(
		 column(3, br(),
           p(textOutput("par5")), 
		   p(textOutput("par6")),
		   p(textOutput("par7")),
		   actionButton("lanza", textOutput("roll"))
		 ),
		 column(4,
		   plotOutput("freq"),
		   actionButton("cero", textOutput("zero"))
		 )
	  )
	),
	tabPanel(textOutput("tab3"), 
		fluidRow(
		 column(3, br(),
           p(textOutput("par8")), 
		   p(textOutput("par9"))
		 ),
		 column(4,
		  plotOutput("medias")
		 )
	  )
	)	
 )
))


 