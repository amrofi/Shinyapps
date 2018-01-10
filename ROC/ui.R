Sys.setlocale("LC_ALL", "es_ES.UTF-8") #to be sure that accents in text will be allowed in plots
library(shiny)

shinyUI(fluidPage(h3(textOutput("titulo")),

  absolutePanel(
    top = 0, right = 0, width=80, 
	fluidRow(actionButton("eng", img(src='uk-icon.png'), style="margin:0;padding:0;"), actionButton("spa", img(src='Spain-icon.png'), style="margin:0;padding:0;"))
	),
  withMathJax(),

  tabsetPanel(type = "tabs", 
	tabPanel(textOutput('norm'), 
       fluidRow(column(3, p(),
	        p(textOutput('par1')), p(),
            sliderInput("difm", textOutput('difme'), min=0, max=10, value=5, step=0.05),
			helpText(textOutput('par2')),
            sliderInput("x", textOutput('umb'), min=0, max=1, value=0.5, step=0.005),
			helpText(textOutput('par3')), p(),
			tags$a(href='https://www.youtube.com/watch?t=2&v=fsgDD0pNkZ0', textOutput('vid')), p(),
			tags$a(href = "http://bioestadistica.upc.edu/", 
					tags$img(src = "http://www-eio.upc.es/teaching/best/Logos/logo_bne_tiny.png",  style="width:60px;height:60px;border:0")),
			p(),
			tags$em("(C) 2015. Bioestadística para No Estadísticos")
           ),
		   column(5,
		    plotOutput("fig")
		   ),
		   column(4,
		    plotOutput("roc")
		   )
	 )	  ),
	tabPanel(textOutput('disk'), 
       fluidRow(column(3, p(),
	         p(textOutput('par4')), p(),
			 sliderInput("k", textOutput('num'), min=5, max=25, value=10),
	         p(textOutput('par5')),
			 p(uiOutput('par6')),
			 sliderInput("sor", HTML('<em>OR</em>'), min=1, max=3, value=1.0, step=0.01),
             p(uiOutput('orcum')),
			tags$a(href = "http://bioestadistica.upc.edu/", 
					tags$img(src = "http://www-eio.upc.es/teaching/best/Logos/logo_bne_tiny.png",  style="width:60px;height:60px;border:0")),
			p(),
			tags$em("(C) 2015. Bioestadística para No Estadísticos")
	       ),
		   column(5,
		     plotOutput("disc", clickId='P'),
			 tableOutput("tab1")
		   ),
		   column(4,
		    plotOutput("roc2")
		   )
	 )	  )
	 )
))
