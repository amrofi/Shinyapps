library(shiny)

.L = read.table("etiquetas binopois.txt", header=FALSE, sep='\t', quote="", row.names=1, colClasses=rep("character", 3))

# Define server logic required to generate and plot a random distribution
shinyServer(function(input, output) {

  .E <- reactiveValues(d = 1)  #active language and default

  tr <- function(stringId) {
        .L[stringId, .E$d]
  }

  observeEvent(input$eng, {    .E$d=1   })
  observeEvent(input$spa, {    .E$d=2   })
  
  output$titulo = renderText( { tr("titulo") } )
  output$par1 = renderText( { tr("par1") } )
  output$par2 = renderUI( { HTML(tr("par2")) } )  
  output$par3 = renderText( { tr("par3") } )
  output$ene = renderUI( { HTML(tr("ene")) } )  
  output$pe = renderUI( { HTML(tr("pe")) } )  


  output$graf <- renderPlot({
    
	n = input$n
    p = input$p
    l = n*p
    K = qbinom(c(0.0001, 0.9999), n, p)
    k = seq(K[1], K[2])
    L =c(paste('B(',n,',',p,')', sep=''),paste('P(',l,')',sep=''))
  par(lend=1, mfrow=c(1,2), mar=c(2,2,0.5,0.1))
  plot(k, dbinom(k, n, p), t='h', xlab='', ylab='', lwd=8, col='orange')
  points(k, dpois(k, l), t='h', lwd=2, col='blue')
  legend("topright", leg=L, col=c('orange', 'blue'), lwd=c(8,2))
  plot(k, pbinom(k, n, p), t='s', xlab='', ylab='', lwd=7, col='orange', ylim=0:1)
  points(k, ppois(k, l), t='s', lwd=2, col='blue')

})

})
