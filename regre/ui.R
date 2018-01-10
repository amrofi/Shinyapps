Sys.setlocale("LC_ALL", "es_ES.UTF-8") #to be sure that accents in text will be allowed in plots

options(rgl.useNULL = TRUE)

library(shiny)
library(rgl)
source('../credit.r')
library(htmlwidgets)
library(jsonlite)

rglwgtctrl <- function(inputId, value="", nrows, ncols) {
  # This code includes the javascript that we need and defines the html
  tagList(
    singleton(tags$head(tags$script(src = "rglwidgetaux.js"))),
    tags$div(id = inputId,class = "rglWidgetAux",as.character(value))
  )
}

shinyUI(fluidPage(
  
  rglwgtctrl('ctrlplot3d'),

  absolutePanel(
    top = 0, right = 0, width=80, 
	fluidRow(actionButton("eng", img(src='uk-icon.png'), style="margin:0;padding:0;"), actionButton("spa", img(src='Spain-icon.png'), style="margin:0;padding:0;"))
	),
  
  fluidRow(h3(textOutput("titulo")),
	column(3, 
	   p(textOutput("txt1")),
	   p(textOutput("txt2")),
	   uiOutput('txt3'), hr(),
       sliderInput("bet1", uiOutput("pend"), 
                min = -1, max = 1, 
                value = 0, step=0.025, width='95%'),
       sliderInput("bet0", uiOutput("indep"), 
                min = -5, max = 5, 
                value = 0, step=0.1, width='95%'),
       sliderInput("desv", uiOutput("desv"), 
                min = 0.01, max = 2, step = 0.01,
                value = 0.1, width='95%'),
       sliderInput("ranX", textOutput("rango"), 
                min = -10, max = 10, step = 0.1,
                value = c(-3,3), width='95%'),
	   credit1
    ),
    column(6,
		 rglwidgetOutput("fig", height=550, width='100%'),
		 wellPanel(uiOutput('ecuaciones'))
    ),
	column(3,
		uiOutput('ensenar'),
		#textOutput('sel')
		tableOutput('losdatos')
	)
  )
))
