
library(shiny)
source('../credit.r')

# Define UI for slider demo application
shinyUI(fluidPage(
  
  #  Application title
  titlePanel("Hangman"),
  
  fluidRow(
     column(6,
	   p("Guess the word"),
       plotOutput("fig", height=350, width=720, clickId='P')
     )
  ),
  credit1
))


 