Sys.setlocale("LC_ALL", "es_ES.UTF-8") #to be sure that accents in text will be allowed in plots

library(shiny)
source('../credit.r')

shinyUI(fluidPage(h3(textOutput("titulo")),
  
  absolutePanel(
    top = 0, right = 0, width=80, 
	fluidRow(actionButton("eng", img(src='uk-icon.png'), style="margin:0;padding:0;"), actionButton("spa", img(src='Spain-icon.png'), style="margin:0;padding:0;"))
	),
  
  fluidRow(
     column(3,
	   p(textOutput("par1")),
	   p(textOutput("par2")),
	   hr(),
		tabsetPanel(type = "tabs",
                  tabPanel(textOutput('scal'), uiOutput("barra")),
                  tabPanel(textOutput('text'), uiOutput("texto")),
				  tabPanel(textOutput('cust'), uiOutput("person"))
		),	   

		credit1
     ),

     column(6,
       plotOutput("fig"),
	   h4(uiOutput("txt"))
     ),

     column(3,
	   p(textOutput("par3")),
	   p(),
       selectInput("cond", textOutput("condi"), c("A"=0, "B"=1)),
       plotOutput("graf")
     )

  )
))


