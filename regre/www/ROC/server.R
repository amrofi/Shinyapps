library(shiny)
library(nleqslv)

.L = read.table("etiquetas ROC.txt", header=FALSE, sep='\t', quote="", row.names=1, colClasses=rep("character", 3))

equat = function(X, a, or) {
  l = length(X)
  y = array(NA, dim=l)
  for (j in 1:(l-1)) {
    y[j] = sum(a[1:j]) * sum(X[(j+1):l]) /(sum(X[1:j]) * sum(a[(j+1):l])) - or
  }
  y[l] = sum(X) - 1
  y
}

jacob = function(X, a, or) {
  l = length(X)
  D = array(NA, dim=c(l,l))
  for (j in 1:(l-1)) {
    cof = sum(a[1:j])/sum(a[(j+1):l])
    num = -sum(X[(j+1):l])
    den = sum(X[1:j])
    d1 = cof/den
    for (i in (j+1):l) {
      D[j, i] = d1
    }
    d2 = d1*num/den
    for (i in 1:j) {
      D[j, i] = d2
    }
  }
  D[l,] = rep(1, l)
  D
}

update_p = function(p, x, k) {
   y = (1 - x)/(1-p[k])
   p[-k] = y*p[-k]
   p[k] = x
   p
}

shinyServer(function(input, output) {

 .E <- reactiveValues(d = 1)  #active language and default

  tr <- function(stringId) {
        .L[stringId, .E$d]
  }

  observeEvent(input$eng, {    .E$d=1   })
  observeEvent(input$spa, {    .E$d=2   })
    
  output$titulo = renderText( { tr("titulo") } )
  output$par1 = renderText( { tr("par1") } )
  output$par2 = renderText( { tr("par2") } )
  output$par3 = renderText( { tr("par3") } )
  output$par4 = renderText( { tr("par4") } )
  output$par5 = renderText( { tr("par5") } )
  output$norm = renderText( { tr("norm") } )
  output$disk = renderText( { tr("disk") } )
  output$par6 = renderUI( { HTML(tr("par6")) } )
  output$orcum = renderUI( { withMathJax(HTML(tr("orcum"))) } )
  output$difme = renderText( { tr("difme") } )
  output$umb = renderText( { tr("umb") } )
  output$num = renderText( { tr("num") } )


  mu1 = 0
  sd1 = 2.5
  sd2 = sd1
  xmin <- mu1-2.5*sd1

  output$fig <- renderPlot({
      mu2 = input$difm	  
	  xmax <- mu2+2.5*sd2
      ymax0 <- max(dnorm(mu1,mu1,sd1),dnorm(mu2,mu2,sd2))
      ymax <- 1.1*ymax0
	  xline = xmin + input$x * (xmax-xmin)

      par(mar=c(4.5,1,4,1.5),las=1)
	  plot(NA,xlim=c(xmin,xmax),ylim=c(0,ymax),xaxt="n",yaxt="n",xlab="",ylab="",font.lab=2,cex.lab=2)
	  axis(1,at=c(xmin,xmax),labels=expression(symbol("\055"),"+"),tick=FALSE,cex.axis=2.5,font=2,adj=0.5,mgp=c(3,2,0))
	  axis(1,at=(mu1+mu2)/2,labels=tr('resul'),tick=FALSE,cex.axis=1.25,font=2,adj=0.5,mgp=c(3,2,0))
	  curve(dnorm(x,mu1,sd1),add=TRUE,col="blue",lwd=2)
	  curve(dnorm(x,mu2,sd2),add=TRUE,col="red",lwd=2)
	  abline(v=xline)
	  text(mu1,1.07*ymax0,tr('sanos'),cex=1.3,adj=0.5,col="blue", pos=2)
	  text(mu2,1.07*ymax0,tr('enfer'),cex=1.3,adj=0.5,col="red", pos=4)
	  xaux <- seq(xline,xmax,0.01)
	  vx1 <- c(xmax,xline,xaux)
	  vy1 <- c(0,0,dnorm(xaux,mu1,sd1))
	  polygon(vx1,vy1,col=rgb(red=0,green=0,blue=255,alpha=50,maxColorValue=255),border=NA)
	  xaux <- seq(xline,xmax,0.01)
	  vx2 <- c(xmax,xline,xaux)
	  vy2 <- c(0,0,dnorm(xaux,mu2,sd2))
	  polygon(vx2,vy2,col=rgb(red=255,green=0,blue=0,alpha=50,maxColorValue=255),border=NA)
  })
  
  output$roc <- renderPlot({
      mu2 = input$difm	  
	  xmax <- mu2+2.5*sd2
	  pun = xmin + input$x * (xmax-xmin)
      xline = seq(xmin, xmax, len=150)
	  xROC <- pnorm(xline,mu1,sd1,lower.tail=FALSE)
	  yROC <- pnorm(xline,mu2,sd2,lower.tail=FALSE)
	  xROC = c(1, xROC, 0)
	  yROC = c(1, yROC, 0)
	  xx <- pnorm(pun,mu1,sd1,lower.tail=FALSE)
	  yy <- pnorm(pun,mu2,sd2,lower.tail=FALSE)
	  par(mar=c(4.5,4,4,1.5),las=1)
	  plot(xROC,yROC,type="l",xlim=c(0.035,0.965),ylim=c(0.035,0.965),lwd=2,col="green",xlab="",ylab="")
	  axis(3,at=seq(0,1,0.2),labels=seq(1,0,-0.2))
	  mtext(paste("P(-|S) =", tr('espe')),3,at=0.5,line=2.5,font=2,col=1)
	  mtext(paste0("P(+|S) = 1-", tr('espe')), 1,at=0.5,line=2.5,font=2,col=rgb(red=0,green=0,blue=255,alpha=150,maxColorValue=255))
	  mtext(paste("P(+|E) =", tr('sensi')),2,at=0.5,line=3,las=0,font=2,col=rgb(red=255,green=0,blue=0,alpha=150,maxColorValue=255))
	  abline(0,1,lty=2)
	  segments(xx,-0.5,xx,yy,col="blue",lwd=3)
	  segments(-0.5,yy,xx,yy,col="red",lwd=3)
	  abc = sum(dnorm(xline,mu1,sd1)*pnorm(xline,mu2,sd2,lower.tail=FALSE))*(xmax-xmin)/150
      text(0.5, 0.1, paste0("AUC = ", round(abc, 3)), cex=2) 
  })

    v <- reactiveValues(K = 10, ps=rep(1/10, 10), pe=rep(1/10, 10), or=1.0)    # probabilidades sanos/enfermos
    observeEvent(input$k, { 
	              kk = as.numeric(input$k)
                  if (kk != v$K) {
                      v$K = kk
					  v$ps = rep(1/kk, kk)   # reset
					  Sol = nleqslv(x = v$ps, equat, jacob, v$ps, v$or)
					  v$pe = Sol$x
				  }
	} )
	observeEvent(input$sor, { 
	              v$or = as.numeric(input$sor)
				  Sol = nleqslv(x = v$ps, equat, jacob, v$ps, v$or)
                  v$pe = Sol$x
	} )
  observeEvent(input$P, {
		  a = input$P$x
		  b = input$P$y
		  if (b>0) {
		      if (abs(round(a)-a)<0.3) {
			      a = round(a)
				  p = update_p(v$ps, b, a)
				  v$ps = p
			  }
		  } else {
		      if (abs(round(a)-a)<0.3) {
			      a = round(a)
				  p = update_p(v$pe, -b, a)
				  v$pe = p
			  }
		  } 
	} )

  output$disc <- renderPlot({
		rany = c(-max(v$pe)*1.2, max(v$ps)*1.2)
		ptos = sort(unique(c(pretty(c(0,v$ps),n=5), pretty(c(0,-v$pe),n=5))))
		par(mar=c(2,3,1,1))
		plot(NA, xlim=c(1,v$K), ylim=rany, xlab='', ylab='', axes=FALSE)
		lines(1:v$K, v$ps, t='h', lwd=3, col='blue')
		lines(1:v$K, -v$pe, t='h', lwd=3, col='red')
		axis(2, at=ptos, lab=abs(ptos), las=1)
		abline(h=0)
		mtext(tr('sanos'), 3, at=v$K/2, col='blue', font=2)
		mtext(tr('enfer'), 1, at=v$K/2, col='red', font=2)
  })
  
  output$tab1 = renderTable({
        x=data.frame(Sanos = v$ps, Enfermos = v$pe)
		names(x) = c(tr('sanos'), tr('enfer'))
		x
  }, digits=c(0,3,3))
  
  output$roc2 <- renderPlot({
      acux = c(1, 1 - cumsum(v$ps))
      acuy = c(1, 1 - cumsum(v$pe))
	  par(mar=c(4.5,4,4,1.5),las=1)
	  plot(NA,type="n",xlim=c(0.035,0.965),ylim=c(0.035,0.965),xlab="",ylab="")
        points(acux, acuy, pch=18, col='green', cex=2)
	  axis(3,at=seq(0,1,0.2),labels=seq(1,0,-0.2))
	  mtext(paste("P(-|S) =", tr('espe')),3,at=0.5,line=2.5,font=2,col=1)
	  mtext(paste0("P(+|S) = 1-", tr('espe')),1,at=0.5,line=2.5,font=2,col=rgb(red=0,green=0,blue=255,alpha=150,maxColorValue=255))
	  mtext(paste("P(+|E) =", tr('sensi')),2,at=0.5,line=3,las=0,font=2,col=rgb(red=255,green=0,blue=0,alpha=150,maxColorValue=255))
	  abline(0,1,lty=2)
	  abc = sum(v$ps * (acuy[1:v$K]+acuy[2:(v$K+1)])/2 )
      text(0.5, 0.1, paste0("AUC = ", round(abc, 3)), cex=2) 
  })
})