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
		 column(4, br(),
		    p(textOutput("txt1")),
			br(),
			htmlOutput("rango"), br(),
			sliderInput("delta", HTML('&delta;'), width='95%',
                min = -0.5, max = 0.5, value = 0, step=0.005),
			sliderInput("alfa", HTML('&alpha;'), width='95%',
                min = -4, max = 4, value = 1, step=0.025),
			sliderInput("beta", HTML('&beta;'), width='95%', 
                min = -4, max = 4, value = 1, step=0.025),
			sliderInput("gamma", HTML('&gamma;'), width='95%', 
                min = -4, max = 4, value = 1, step=0.025),
			uiOutput('Tune'),
			helpText(uiOutput("ayuda")),
			credit1
		 ),
		 column(4,
		   plotOutput("fig"),
		   htmlOutput("integ")
		 ),
		 column(4,
		   plotOutput("figdis")
		 )  
		)
	),
	tabPanel(textOutput("tab2"), 
		fluidRow(
		 column(2, br(),
           htmlOutput("momen"), br(),
		   numericInput("ene", textOutput("ene"), 250, min=100, max=1000, step=25),
		   actionButton("sim", textOutput("mues"))
		 ),
		 column(4,
		  plotOutput("sampl", height=500), br(),
		  htmlOutput("resumen")
		 )
	  )
	),
	tabPanel(textOutput("tab3"), 
		fluidRow(
		 column(3, br(),
           p(textOutput("txt2")),
		   uiOutput("txt3"),p(),
           p(textOutput("txt4")),
           p(textOutput("txt5"))
		 ),
		 column(5,
		  plotOutput("mumo", clickId='P', height=410), br(),
		  actionButton("go", textOutput("estim")),
		  uiOutput("value")
		 ),
		 column(3,
		  h4(textOutput("info")),
		  helpText(uiOutput("txt6")),
		  helpText("$$\\prod_{i=1}^n f(X; \\Theta)$$"),
		  helpText(textOutput("txt7")),
		  helpText("$$\\sum_{i=1}^n log(f(X; \\Theta))$$"),
		  helpText(textOutput("txt8"))
		 )
	  )
	)	
 )
))


 