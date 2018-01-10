Sys.setlocale("LC_ALL", "es_ES.UTF-8") #to be sure that accents in text will be allowed in plots

library(shiny)
source('../credit.r')

# Define UI for slider demo application
shinyUI(fluidPage(h3(textOutput("titulo")),
  
  absolutePanel(
    top = 0, right = 0, width=80, 
	fluidRow(actionButton("eng", img(src='uk-icon.png'), style="margin:0;padding:0;"), actionButton("spa", img(src='Spain-icon.png'), style="margin:0;padding:0;"))
	),

	flowLayout(
      # Base
      sliderInput("base", textOutput("base.t"), min = 0, max = 1, value = 0.5, step= 0.02),     
      # Z
      sliderInput("z", textOutput("z.t"), min = 0, max = 1, value = 0.5, step= 0.02),     
      # X
      sliderInput("x", textOutput("x.t"), min = 0, max = 1, value = 0.5, step= 0.02),     
      # Y
      sliderInput("y", textOutput("y.t"), min = 0, max = 1, value = 0.5, step= 0.02),
      # XZ
      sliderInput("xz", textOutput("xz.t"), min = 0, max = 1, value = 0.5, step= 0.02),     
      # YZ
      sliderInput("yz", textOutput("yz.t"), min = 0, max = 1, value = 0.5, step= 0.02),     
      # XY
      sliderInput("xy", textOutput("xy.t"), min = 0, max = 1, value = 0.5, step= 0.02),     
      # XYZ
      sliderInput("xyz", textOutput("xyz.t"), min = 0, max = 1, value = 0.5, step= 0.02),
	  uiOutput("par1"), 
	  uiOutput("par2"),  
	  uiOutput("par3"), 
	  cellArgs = list(width="100%")
    ),
    
    # Show
    fluidRow(column(8, plotOutput("values")),
             column(2, p('Z=Z+'), 
			           tableOutput("tab1"), 
					   textOutput("OR1"), br(),
					   p('X-Y'), 
					   tableOutput("tabxy"), 
					   textOutput("ORxy"),
					   uiOutput("IC")),
             column(2, p('Z=Z-'), 
			           tableOutput("tab2"), 
					   textOutput("OR2"))
    ),
	credit1
  )
)

