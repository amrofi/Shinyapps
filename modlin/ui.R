Sys.setlocale("LC_ALL", "es_ES.UTF-8") #to be sure that accents in text will be allowed in plots

library(shiny)
source('../credit.r')

shinyUI(pageWithSidebar(

  absolutePanel(
    top = 0, right = 0, width=80, 
	fluidRow(actionButton("eng", img(src='uk-icon.png'), style="margin:0;padding:0;"), actionButton("spa", img(src='Spain-icon.png'), style="margin:0;padding:0;"))
	),

  sidebarPanel(
    h3(textOutput("titulo")),
    p(textOutput("side1")), 
    p(textOutput("side2")), 
    sliderInput("bet1", 
                uiOutput('pend'),
                min = -2, 
                max = 2, 
                value = 0, step=0.05),
    sliderInput("bet0", 
                uiOutput('cons'), 
                min = -5, 
                max = 5, 
                value = 0, step=0.1),
    sliderInput("desv", 
                uiOutput('sig'), 
                min = 0.1, 
                max = 5, step = 0.05,
                value = 1),
    numericInput("obs", textOutput('ene'), 20,
             min = 10, max = 50),
		
    uiOutput('Premises'),
	uiOutput('prem.CTRL'),
    credit1,
	width = 3
  ),
  
  # Show a plot of the generated distribution
  mainPanel(
      tabsetPanel(type = "tabs", 
	     tabPanel(textOutput("tab1"), 
            fluidRow( 
		        plotOutput("fig", height=640), br(),
				p(textOutput('T1.1')),
				uiOutput('T1.2')
			)),
		 tabPanel(textOutput("tab2"), 
            fluidRow( 
				h3(textOutput('wtf')),
				column(3,
				  p(uiOutput('T2')),
				  p(textOutput('t2.a')),
				  p(textOutput('t2.b')),
				  p(textOutput('t2.c')),
				  p(textOutput('t2.d'))
				),
				column(9,
				  plotOutput("fig2", height=540))
	        )),
		 tabPanel(textOutput("tab3"), 
            fluidRow( htmlOutput("alfas"),
			       style='line-height: 1.7;'
         ))
  ))
))
