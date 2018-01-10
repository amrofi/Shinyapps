.L = read.table("etiquetas pares.txt", header=FALSE, sep='\t', quote="", row.names=1, colClasses=rep("character", 3))

library(shiny)
library(mvtnorm)

# Define server logic for slider examples
shinyServer(function(input, output) {

  SEL = 1
  makeReactiveBinding("SEL")
 .E <- reactiveValues(d = 1)  #active language and default

  tr <- function(stringId) {
        .L[stringId, .E$d]
  }

  observeEvent(input$eng, {    .E$d=1   })
  observeEvent(input$spa, {    .E$d=2   })
  observeEvent(input$pot, {    
		SEL <<- ifelse(input$pot==tr('AB'), 1, 2)
  } )
  
  output$titulo = renderText( { tr("titulo") } )
  output$par1 = renderText( { tr("par1") } )
  output$dife = renderUI( { HTML(tr("dife")) } )
  output$stdev = renderText( { tr("stdev") } )
  output$corr = renderText( { tr("corr") } )
  output$scala = renderText( { tr("scala") } )
  output$borra = renderText( { tr("borra") } )
  output$ojo = renderText( { tr("ojo") } )
  output$AB = renderText( { tr("AB") } )
  output$SR = renderText( { tr("SR") } )
  output$ran = renderText( { tr("ran") } )
  output$view = renderText( { tr("view") } )
  output$pot.butt = renderUI( {
         radioButtons("pot", tr("view"), c(tr('AB'),tr('SR')), 
			selected=ifelse(SEL==1, tr('AB'), tr('SR')))
  } )

	mu1 = 12

  v <- reactiveValues(A=c(), B=c(), sigma=2)  # ver ui.R, default 2
  observeEvent(input$reset, {    # 
          v$A=c(); v$B=c()
  })
  regenerar = function(A) {
     if (length(A)>0) {
	      mu2 = mu1 + input$delta
		  sg = input$sg
		  vr = sg^2
		  cova = input$corre*vr
		  bet1 = cova/vr
		  bet0 = mu2-bet1*mu1
		  rnorm(length(A), bet0+bet1*A, sg*sqrt(1-input$corre^2))
	 } else { c()}
  }
  observeEvent(input$sg, {    # hay que reescalar primero A
		  v$A = input$sg/v$sigma*(v$A-mu1) + mu1
          v$B=regenerar(v$A)
		  v$sigma = input$sg
  })
  observeEvent(input$corre, {    # 
          v$B=regenerar(v$A)
  })
  observeEvent(input$delta, {    # 
          v$B=regenerar(v$A)
  })
  
  observeEvent(input$mas, {    # 
          n = 24
		  mu2 = mu1 + input$delta
		  sg = input$sg
		  vr = sg^2
		  cova = input$corre*vr
		  Sigma = matrix(c(vr, cova, cova, vr), ncol=2)
		  x <- rmvnorm(n=n, mean=c(mu1,mu2), sigma=Sigma)
		  v$A = x[,1]
		  v$B = x[,2]
  })
  
  observeEvent(input$P, {    # 
          v$A = c(v$A, input$P$x)
		  mu2 = mu1 + input$delta
		  sg = input$sg
		  vr = sg^2
		  cova = input$corre*vr
		  bet1 = cova/vr
		  bet0 = mu2-bet1*mu1
		  v$B = c(v$B, rnorm(1, bet0+bet1*input$P$x, sg*sqrt(1-input$corre^2)))
  })

	datasetInput <- reactive({
	mu2 = mu1 + input$delta
	sg = input$sg
	vr = sg^2
	cova = input$corre*vr
	n=10
	Sigma = matrix(c(vr, cova, cova, vr), ncol=2)
	x <- rmvnorm(n=n, mean=c(mu1,mu2), sigma=Sigma)
	x
  })

  output$escala <- renderPlot({
	sg = input$sg
    rngx = qnorm(c(0.001, 0.999), mu1, sg)
	par(mar=c(4,0,0,0))
	plot(NA, xlim=rngx, ylim=c(0,1), xlab='A', ylab='', axes=FALSE)
    axis(1)
    if (length(v$A)>0) {
	  last = v$A[length(v$A)]
      rug(v$A, lwd=2, ticksize=0.5)
	  lines(c(last, last), c(0, 0.9), lwd=2, col='red')
	}
  })
  
  # Show the values 

  output$values <- renderPlot({
  if (length(v$A)>0) {
    x = cbind(v$A, v$B)
	pos = as.numeric(x[,2]>x[,1])
	D = x[,2]-x[,1]
	sg = input$sg
    rngx = qnorm(c(0.001, 0.999), mu1, sg)
	rng = range(c(x, rngx))
	Y = c(x[,1],x[,2])
	n = dim(x)[1]
	G = c(rep('A', n), rep('B', n))
	Pos = c(pos,pos)
	H = factor(paste(G, Pos), levels=c('A 0', 'B 0', 'A 1', 'B 1'), ordered=TRUE,
		 labels=c('A','B','A','B'))
	rd = rank(x[,1])

	layout(matrix(c(1,2,3,4), 2, 2, byrow = FALSE), widths=c(3,2), heights=c(4,3))
	palette(c('royalblue2', 'orangered1'))    # (baja, sube)
	par(mar=c(3.9, 3.9, 0.5, 1))
    if (SEL==2) {    # 'SR'
		plot(x[,1], rd, xlim=rng, pch=16, xlab=tr('resp'), ylab=tr('suj'))
		points(x[,2], rd, col='olivedrab')
		arrows(x0=x[,1], x1=x[,2], y0=rd, col=pos+1, len=0.12, lty=2)
		points(x[n,1], rd[n], pch=13, cex=1.5)
		legend("topleft", pch=c(16, 1), col=c('black', 'olivedrab'), leg=c('A','B'))
    } else {
		plot(x, xlim=rng, ylim=rng, pch=16, xlab='A', ylab='B')
		abline(a=0, b=1, col='grey', lty=2)
		segments(x0=x[,1], y0=x[,2], y1=x[,1], col=pos+1, lty=2)
		points(x[n,1], x[n,2], pch=13, cex=1.5)
	}
	stripchart(Y~H, las=1, ylim=c(0.5,4.5), xlab='', xlim=rng)
	segments(x0=x[,1], x1=x[,2], y0=rep(1+2*pos,n), y1=rep(2+2*pos,n), col=Pos+1)
	points(x[n,1], 1+2*pos[n], pch=13, cex=1.5)
	mtext(c(tr('up'), tr('down')), 4, at=c(3.5, 1.5))

	par(mar=c(3.9, 2.5, 0.5, 1))
	gi=hist(D, main='', xlab='Dif. B-A', ylab='')
	lines(c(0,max(gi$bre)), c(0, 0), lwd=3, col=2)
	lines(c(0,min(gi$bre)), c(0, 0), lwd=3, col=1)
	plot(NA, xlim=range(gi$breaks), ylim=c(1,n), xlab='', ylab='', axes=FALSE)
	axis(1)
	segments(x0=rep(0, n), x1=D, y0=rank(D), col=pos+1, lwd=2)
	arrows(x0=0, x1=D[n], y0=rank(D)[n], col=pos[n]+1, lwd=2, len=0.15)
	}
  })

  output$data = renderTable({
      x = cbind(v$A, v$B)
      if (length(v$A)>0) colnames(x) = c('A','B')
	  x
  }, digits=c(0,2,2)
  )
  
  output$stat <- renderUI({
    if (length(v$A)>0) {
	  n = length(v$A)
	  mA = mean(v$A)
	  mB = mean(v$B)
	  D = v$B - v$A
	  mD = mean(D)
	  if (length(v$A)>1) {
	     sdD = sd(D) 
	     IC = t.test(v$B, v$A, paired=TRUE)$conf.int
		 lb = round(IC[1], 2)
	     ub = round(IC[2], 2)
		 cr = cor(v$A, v$B)
	  } else {
	     sdD= NA
	     lb = NA
		 ub = NA
		 cr = NA
	  }
	  txt = paste0('<h3>', tr('indi'), '</h3>')
	  txt = paste0(txt, tr("num"), n, "<br>")
	  txt = paste0(txt, tr("meA"), round(mA, 2), "<br>")
	  txt = paste0(txt, tr("meB"), round(mB, 2), "<br>")
	  txt = paste0(txt, tr("meD"), round(mD, 2), "<br>")
	  txt = paste0(txt, tr('sdD'), round(sdD, 2), "<br>")
	  txt = paste0(txt, tr('cole'), round(cr, 3), "<br>")
	  txt = paste0(txt, tr('ic'), lb, ", ", ub, ")<br>")
	  HTML(txt)
	}
  })
  
  output$cons <- renderUI({
	  txt = paste0('<h3>', tr('cabec'), '</h3>')
	  txt = paste0(txt, '<ul><li>', tr('pto1'), '</li>')
	  txt = paste0(txt, '<li>', tr('pto2'), '</li>')
	  txt = paste0(txt, '<li>', tr('pto3'), '</li>')
	  txt = paste0(txt, '<li>', tr('pto4'), '</li>')
	  HTML(txt)
  })
})
