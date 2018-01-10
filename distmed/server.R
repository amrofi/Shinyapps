library(shiny)

.L = read.table("etiquetas distmed.txt", header=FALSE, sep='\t', quote="", row.names=1, colClasses=rep("character", 3))

# Define server logic required to generate and plot a random distribution
shinyServer(function(input, output) {

  .E <- reactiveValues(d = 1)  #active language and default

  tr <- function(stringId) {
        .L[stringId, .E$d]
  }

  observeEvent(input$eng, {    .E$d=1   })
  observeEvent(input$spa, {    .E$d=2   })
  
  output$titulo = renderText( { tr("titulo") } )
  output$parraf = renderUI( { HTML(tr("parraf")) } )  
  output$ene = renderUI( { HTML(tr("ene")) } )  
  output$shape1 = renderText( { tr("shape1") } )  
  output$shape2 = renderText( { tr("shape2") } )  
  output$media = renderText( { tr("media") } )  
  output$sd = renderText( { tr("sd") } )  
  output$pd = renderUI( { 
      withMathJax(helpText(tr("pd"))) 
  } )  

  v = reactiveValues(a = 0, b = 0, n = 10, m = c())

  recalc = function() {
    T = rbeta(10000*v$n, v$a, v$b)
    v$m = apply(array(T, dim=c(10000,v$n)), 1, mean)
  }
  observeEvent( input$aa, {
	v$a = exp(input$aa)
	recalc()
  })
  observeEvent( input$bb, {
	v$b = exp(input$bb)
	recalc()
  })
  observeEvent( input$n, {
	v$n = input$n
	recalc()
  })

  output$graf <- renderPlot({
    
    a = v$a
    b = v$b
    x = seq(0,1, len=21)
# The mean is a/(a+b) and the variance is ab/((a+b)^2 (a+b+1)).
    y = pbeta(x, a, b)
    dy = diff(y)
    mu = a/(a+b)
    sg2 = a*b/((a+b)^2 *(a+b+1))
    hd = bquote(paste(mu==.(round(mu, 3)), ';  ', sigma==.(round(sqrt(sg2), 4)), sep=''))
    par(mar=c(2,1,1.5,0.5))
    plot(c(0,1), c(0,max(dy)), t='n', xlab='',ylab='')
    mtext(hd)
    rect(x[1:20], rep(0,20), x[2:21], dy, border=NA, col='grey')
})

  output$graf2 <- renderPlot({
    if (length(v$m) >0) {
      m = v$m
      mm = round(mean(m), 3)
      ss = round(sd(m), 4)
      par(mar=c(2,2,1.5,0.5))
      if (input$zoom) {
        rng = range(m)
      } else {
        rng=c(0,1)
      }
      hist(m, xlim=rng, main='')
      mtext(paste(tr("media"), mm, tr("sd"), ss, sep=''))
    }
  })

})
