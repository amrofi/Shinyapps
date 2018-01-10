library(shiny)


.L = read.table("etiquetas twomeans.txt", header=FALSE, sep='\t', quote="", row.names=1, colClasses=rep("character", 3))

shinyServer(function(input, output) {

  .E <- reactiveValues(d = 1)  #active language and default

  tr <- function(stringId) {
        .L[stringId, .E$d]
  }

  SEL = 1
  makeReactiveBinding("SEL")
  observeEvent(input$eng, {    .E$d=1   })
  observeEvent(input$spa, {    .E$d=2   })
  observeEvent(input$lat, {    
     t = input$lat
     SEL <<- ifelse(t==tr("H1.gt"), 1, 
				ifelse(t==tr("H1.lt"), 2, 3))
	 })

  output$titulo = renderText( { tr("titulo") } )
  output$parr1 = renderUI( { HTML(tr("parr1")) } )  
  output$parr2 = renderUI( { HTML(tr("parr2")) } )  
  output$sig = renderUI( { HTML(tr("sig")) } )  
  output$ene = renderUI( { HTML(tr("ene")) } )  
  output$grA = renderText( { tr("grA") } )  
  output$grB = renderText( { tr("grB") } )  
  output$HAlt = renderUI( { 
	   sltd = switch(SEL, tr("H1.gt"), tr("H1.lt"), tr("H1.neq"))
		selectInput("lat", tr("H_uno"),   c(tr("H1.gt"),tr("H1.lt"),tr("H1.neq")),
			selected = sltd)
  })

  M = 1000

  datasetInput <- reactive({
    ##--Inputs
    n <- input$obs
    sg <- input$desv
    mu1 <- input$mu1
    mu2 <- input$mu2
    Y1 = matrix(rnorm(n*M, mu1, sg), ncol=n)
    Y2 = matrix(rnorm(n*M, mu2, sg), ncol=n)
    x1 = apply(Y1, 1, mean)
    s1 = apply(Y1, 1, sd)
    x2 = apply(Y2, 1, mean)
    s2 = apply(Y2, 1, sd)
    V = (s1^2+s2^2)/2

    list(x1=x1,x2=x2,V=V,n=n, par=c(mu1, mu2, sg))
  })
  
  azul = rgb(0,0,1,0.4)
  rojo = rgb(1,0,0,0.4)
  output$fig <- renderPlot({
    
    Q = datasetInput(); x1=Q$x1; x2=Q$x2; V=Q$V; n=Q$n; mu1=Q$par[1]; mu2=Q$par[2]; sg=Q$par[3]

    layout(matrix(c(1,1,2,3), ncol=2, byrow=TRUE), heights=c(4,6))
    u1 = seq(-3, 3, len=60)*sg+mu1
    u2 = u1+mu2-mu1
    par(mar=c(2,2,0.3, 0.3))
    plot(range(c(u1,u2)), c(0,dnorm(mu1, mu1, sg)), t='n', xlab='', ylab='')
    lines(u1, dnorm(u1, mu1, sg), col='purple')
    lines(u2, dnorm(u2, mu2, sg), col='green')
    legend("topright", lwd=2, col=c('purple', 'green'), leg=c("YA","YB"), cex=1.75)

    t = (x1-x2)/sqrt(2*V/n)
    par(mar=c(5,2,1.8, 0.3))
    H=hist(t, sub=tr("t_val"), main='', xlab='')
    if (SEL == 1) {   # (input$lat==tr("H1.gt")) {
        d = qt(0.95, 2*n-2)
        ax = d; ay = 0; bx = max(H$breaks); by=75 
        cl = ifelse(t>d, rojo, azul)
    } else if (SEL == 2) {   # (input$lat==tr("H1.lt")) {
        d = qt(0.05, 2*n-2)
        ax = d; ay = 0; bx = min(H$breaks); by=75 
        cl = ifelse(t<d, rojo, azul)
    } else {
        d = qt(0.975, 2*n-2)
        cl = ifelse(abs(t)>d, rojo, azul)
        ax = c(-d, d); ay = c(0,0); bx = range(H$breaks); by=c(75, 75) 
    }
    rect(ax, ay, bx, by, col=rojo, border=NA)

    par(mar=c(4,4.3, 1.3, 0.3))
    plot(sqrt(2*V/n), x1-x2, col=cl, pch=19,xlab='', ylab='')
    title(xlab=expression(s ~ sqrt(2/n)))
    title(ylab=expression(bar(y[A])-bar(y[B])))

  })
  
  output$hypos = renderUI({
  })

  output$res = renderUI({
	Q = datasetInput(); x1=Q$x1; x2=Q$x2; V=Q$V; n=Q$n; mu1=Q$par[1]; mu2=Q$par[2]; sg=Q$par[3]

	txt0 = sprintf("&mu;<sub>A</sub> = %g; &mu;<sub>B</sub> = %g", mu1, mu2)
	if (mu1==mu2) {
		txt0 = paste(txt0, tr('H.si'))
	} else {
		txt0 = paste(txt0, tr('H.no'))
	}
    t = (x1-x2)/sqrt(2*V/n)
    if (SEL == 1) {   # (input$lat==tr("H1.gt")) {
        d = qt(0.95, 2*n-2)
        cl = ifelse(t>d, 0, 1)
    } else if (SEL == 2) {   # (input$lat==tr("H1.lt")) {
        d = qt(0.05, 2*n-2)
        cl = ifelse(t<d, 0, 1)
    } else {
        d = qt(0.975, 2*n-2)
        cl = ifelse(abs(t)>d, 0, 1)
    }
	if (mu1==mu2) {  # H0
		txt = sprintf(tr('tipo1'), (M-sum(cl))/M)
	} else {  # H1
		txt = sprintf(tr('tipo2'), sum(cl)/M)
	}
	HTML(paste('<b>',txt0, '<p><br>', txt, '</b>'))
  })
  
})
