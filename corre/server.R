library(shiny)
.L = read.table("etiquetas corre.txt", header=FALSE, sep='\t', quote="", row.names=1, colClasses=rep("character", 3))

mibarplot = function(t, horiz=FALSE, brdr=NULL, colr=NA, offset=0, add=FALSE, dlt = 0.2) {
     
     if (horiz) {
	     rx = c(0, max(t))
		 ry = 1:length(t)
		 if (!add) {
		    plot(NULL, t='n', axes=FALSE, xlab='', ylab='', xlim=rx, ylim=c(-1/4,1/4)+range(ry))
		    axis(1)
		 }
		 rect(0, ry-dlt+offset, t, ry+dlt+offset, brdr, col=colr)
	 } else {
	     ry = c(0, max(t))
		 rx = 1:length(t)
		 if (!add) {
		    plot(NULL, t='n', axes=FALSE, xlab='', ylab='', ylim=ry, xlim=c(-1/4,1/4)+range(rx))
		    axis(2)
		 }
		 rect(rx-dlt+offset, 0, rx+dlt+offset, t, border=brdr, col=colr)
	 }
}
girar = function(Ob, ang) {
   M = matrix(c(cos(ang), sin(ang), -sin(ang), cos(ang)), ncol=2)
   P = matrix(c(Ob$x, Ob$y), ncol=2)
   Q = P %*% M
   list(x=Q[,1], y=Q[,2])
}

shinyServer(function(input, output, session) {

  SEL = 2
  makeReactiveBinding("SEL")  
 .E <- reactiveValues(d = 1)  #active language and default

  tr <- function(stringId) {
        .L[stringId, .E$d]
  }

  observeEvent(input$eng, {    .E$d=1   })
  observeEvent(input$spa, {    .E$d=2   })
    
  output$titulo = renderText( { tr("titulo") } )
  output$tab1 = renderText( { tr("tab1") } )
  output$tab2 = renderText( { tr("tab2") } )
  output$tab3 = renderText( { tr("tab3") } )
  output$ptosX = renderText( { tr("ptosX") } )
  output$ptosY = renderText( { tr("ptosY") } )
  output$mostP = renderText( { tr("mostP") } )
  output$Indi = renderText( { tr("Indi") } )
  output$mus = renderText( { tr("mus") } )
  output$by = renderText( { tr("by") } )
  output$choose = renderText( { tr("choose") } )
  output$datafile = renderUI( { HTML(tr("datafile")) } )
  output$tomando = renderText( { tr("tomando") } )
  output$txt1 = renderText( { tr("txt1") } )
  output$txt2 = renderUI( { HTML(tr("txt2")) } )
  output$txt3 = renderText( { tr("txt3") } )
  output$txt4 = renderUI( { HTML(tr("txt4")) } )
  output$figuras= renderUI( { HTML(tr("figuras")) } )
  output$txt5 = renderText( { tr("txt5") } )
  output$tomando = renderText( { paste(' ', tr("tomando"), ' ') } )
  output$velo = renderUI(
  	radioButtons("speed", '', choices=c(tr('vel1'),tr('vel2'),tr('vel3')), inline=TRUE, 
	   selected=ifelse(SEL==1, tr('vel1'), ifelse(SEL==2, tr('vel2'), tr('vel3'))))
  )

  C = reactiveValues(cjta = NA, X=NA, Y=NA, rho=NA, num=1, por='X', upload=FALSE)

  observeEvent( input$file1, {
	  f = read.table(input$file1$datapath, header=FALSE, sep=c(' ', '\t'))
	  d = dim(f)
	  if (min(f) < 0 | abs(sum(f)-1)>0.00001) return()
	  if (min(d) >= 2 & max(d) <= 9) {
		updateSliderInput(session, "enex", value = d[2])
		updateSliderInput(session, "eney", value = d[1])
		C$cjta = as.matrix(f)
		C$X = apply(f, 1, sum)
		C$Y = apply(f, 2, sum)
		C$upload=TRUE
	  }
  })
  
  observeEvent(input$por, {
	  C$por = input$por
	  C$num = min(C$num, ifelse(input$por == 'X', input$enex, input$eney))
  })
  observeEvent(input$mas, {
      top = ifelse(C$por == 'X', input$enex, input$eney)
	  if (C$num < top) {C$num = C$num + 1} else {C$num = top}
  })
  observeEvent(input$menos, {
      top = ifelse(C$por == 'X', input$enex, input$eney)
	  if (C$num > 1) C$num = min(top, C$num - 1)
  })
  output$numpg = renderText( C$num )
  
  observeEvent(input$speed, { 
      x = input$speed
	  SEL <<- ifelse(x==tr('vel1'), 1, ifelse(x==tr('vel2'), 2, 3))
  })

  margin = c(1.05, 1.15, 1.25)
  cjt = c(1.1, 1.25, 1.4)      # cambios escala
  
  marginales = function() {
      C$X = apply(C$cjta, 1, sum)
	  C$Y = apply(C$cjta, 2, sum)
	  n = dim(C$cjta)
	  x = 1:n[1]
	  y = 1:n[2]
	  mux = sum(x*C$X)
	  muy = sum(y*C$Y)
	  XX = (x-mux) %*% t(y-muy)
	  vx = sum(x*x*C$X) - mux*mux
	  vy = sum(y*y*C$Y) - muy*muy
	  C$rho = sum(XX*C$cjta)/sqrt(vx*vy)
	  C$upload=FALSE
  }

  observeEvent(input$indi, { 
      X = apply(C$cjta, 1, sum)
	  Y = apply(C$cjta, 2, sum)
      C$cjta = X %*% t(Y)
	  marginales()
  })
  observeEvent(input$enex | input$eney, { 
      C$num = 1
      nx = input$enex
      ny = input$eney
	  u = rexp(nx*ny)
	  u = u/sum(u)
      if (!C$upload) C$cjta = array(u, dim=c(nx,ny))
	  marginales()
  })
  observeEvent(input$Mx_1, { 
    #fac = switch(as.numeric(input$speed), 1.1, 1.25, 1.4)
	fac = margin[as.numeric(SEL)]
    xx = input$Mx_1$x
    n = dim(C$cjta)[1]
    if (abs(xx-round(xx))<0.2) i = round(xx)
    if (exists("i")) {
        if (i>0 & i<=n) {
	        C$cjta[i,] = C$cjta[i,] / fac
            C$cjta = C$cjta/sum(C$cjta)
			marginales()
    }}
  })
  observeEvent(input$Mx_2, { 
	fac = margin[as.numeric(SEL)]
    xx = input$Mx_2$x
    n = dim(C$cjta)[1]
    if (abs(xx-round(xx))<0.2) i = round(xx)
    if (exists("i")) {
        if (i>0 & i<=n) {
	        C$cjta[i,] = C$cjta[i,] * fac
            C$cjta = C$cjta/sum(C$cjta)
			marginales()
    }}
  })
  observeEvent(input$My_1, { 
	fac = margin[as.numeric(SEL)]
    yy = input$My_1$y
    n = dim(C$cjta)[2]
    if (abs(yy-round(yy))<0.2) j = round(yy)
    if (exists("j")) {
        if (j>0 & j<=n) {
	        C$cjta[,j] = C$cjta[,j] / fac
            C$cjta = C$cjta/sum(C$cjta)
			marginales()
    }}
  })
  observeEvent(input$My_2, { 
	fac = margin[as.numeric(SEL)]
    yy = input$My_2$y
    n = dim(C$cjta)[2]
    if (abs(yy-round(yy))<0.2) j = round(yy)
    if (exists("j")) {
        if (j>0 & j<=n) {
	        C$cjta[,j] = C$cjta[,j] * fac
            C$cjta = C$cjta/sum(C$cjta)
			marginales()
    }}
  })
  observeEvent(input$Cjta_1, { 
	fac = cjt[as.numeric(SEL)]
    xx = input$Cjta_1$x
    yy = input$Cjta_1$y
    n = dim(C$cjta)
    if (abs(xx-round(xx))<0.2) i = round(xx)
    if (abs(yy-round(yy))<0.2) j = round(yy)
    if (exists("i") & exists("j")) {
        if (i>0 & i<=n[1] & j>0 & j<=n[2]) {
	        C$cjta[i,j] = C$cjta[i,j] / fac
            C$cjta = C$cjta/sum(C$cjta)
			marginales()
    }}
  })
  observeEvent(input$Cjta_2, { 
	fac = cjt[as.numeric(SEL)]
    xx = input$Cjta_2$x
    yy = input$Cjta_2$y
    n = dim(C$cjta)
    if (abs(xx-round(xx))<0.2) i = round(xx)
    if (abs(yy-round(yy))<0.2) j = round(yy)
    if (exists("i") & exists("j")) {
        if (i>0 & i<=n[1] & j>0 & j<=n[2]) {
		    if (C$cjta[i,j]==0) {
			    C$cjta[i,j] = min(C$cjta[C$cjta>0])
			} else {
	            C$cjta[i,j] = C$cjta[i,j] * fac
            }
			C$cjta = C$cjta/sum(C$cjta)
			marginales()
    }}
  })
  observeEvent(input$Cjta_rm, { 
    n = dim(C$cjta)
    pts = data.frame(expand.grid(1:n[1], 1:n[2]))
	res <- brushedPoints(pts, input$Cjta_rm, 'Var1', 'Var2')
	k = dim(res)[1]
	if (k>0) {
	    aux = C$cjta
	    RS = n[1]*(res$Var2-1) + res$Var1
		aux[RS] = 0
		if (max(aux) > 0) {
			C$cjta = aux/sum(aux)
			marginales()
		}
	}
  })
  
  output$plot1 <- renderPlot({
      if (anyNA(C$cjta)) return()
      par(mar=c(0.5,2,1,0))
	  mibarplot(C$X, brdr=NA, colr='cyan2')
  })
  output$plot2 <- renderPlot({
      if (anyNA(C$cjta)) return()
      par(mar=c(2,2,0,0))
	  n = dim(C$cjta)
	  x = 1:n[1]
	  y = 1:n[2]
	  g = expand.grid(x, y)
	  if (input$probs) {
	      Pr = round(C$cjta, 3)
		  #plot(NULL, xlim=c(-1/4,1/4)+range(x), ylim=c(-1/4,1/4)+range(y),axes=FALSE)  
          symbols(g$Var1, g$Var2, circle=sqrt(as.vector(C$cjta)), fg=NA, bg='darkolivegreen1', inches=5/n[1]/n[2], 
	           xlim=c(-1/4,1/4)+range(x), ylim=c(-1/4,1/4)+range(y))
		  text(g$Var1, g$Var2, lab=Pr)
	  } else {
          symbols(g$Var1, g$Var2, circle=sqrt(as.vector(C$cjta)), fg=NA, bg='forestgreen', inches=5/n[1]/n[2], 
	           xlim=c(-1/4,1/4)+range(x), ylim=c(-1/4,1/4)+range(y))
	  }
	  axis(1, at=x, lab=x)
	  axis(2, at=y, lab=y)
	  mux = sum(x * C$X)
	  abline(v=mux, lty=2, col='red')
	  muy = sum(y * C$Y)
	  abline(h=muy, lty=2, col='red')
  })
  
fl=list(x=c(0,0.5,0,-0.5),y=c(-1,0,8.5,0))
a = seq(0,1,len=45)*pi
a2 = seq(0,1,by=0.05)*pi
a3 = seq(1,-1,by=-0.1)
a22 = a2[1:20]+0.025*pi
  
  output$salpica <- renderPlot({
	par(mar=c(0.1,0.1,0,0), bg='royalblue1', fg='white')
	wid = session$clientData$output_salpica_width
	hei = session$clientData$output_salpica_heigth
	if (wid > 450) {
	    XL = (wid-450)*21/450*c(-1/2,1/2) + c(-10.5, 10.5); YL = c(-1, 10.5)
	} else {
	    XL = c(-10.5, 10.5); dy = (450-wid)*11.5/21; YL = dy*11.5/240*c(-1/2,1/2) + c(-1, 10.5)
	}
	plot(NULL, xlim=XL, ylim=YL, axes=FALSE, xlab='', ylab='')
	lines(9.5*cos(a), 9.5*sin(a))
	lines(8.5*cos(a), 8.5*sin(a))
	segments(9.2*cos(a2), 9.2*sin(a2), 8.8*cos(a2), 8.8*sin(a2), lwd=3)
	segments(9.2*cos(a22), 9.2*sin(a22), 8.8*cos(a22), 8.8*sin(a22), lwd=1)
	text(10.3*cos(a2), 10.1*sin(a2), a3, cex=1.2)
	ang = C$rho * pi/2
	F = girar(fl, ang)
	polygon(F$x, F$y, border=NA, col='navajowhite') # col='firebrick2')
  })
  
  output$plot4 <- renderPlot({
      if (anyNA(C$cjta)) return()
      par(mar=c(2,0,0,0.5))
	  mibarplot(C$Y, horiz=TRUE, brdr=NA, colr='goldenrod1')
  })

  output$leyenda <- renderPlot({
      if (anyNA(C$cjta)) return()
      par(mar=c(0,0,0,0))
	  plot(NULL, xlim=c(0,1), ylim=c(0,1), xlab='', ylab='', axes=FALSE)
      var = C$por
#	  num = input$val  # condic X==num si P_X(num) > 0
	  num = C$num
	  if (var=='X') {
		  if (C$X[num]>0) {
			legend("center", legend = c(expression(paste(P[Y](y))), bquote(paste(P[Y~"|"~X== .(num)](y)))),
				fill = c('goldenrod1', 'forestgreen'), box.col='white', cex=1.5, border='white')
		  }
	  } else {
		  if (C$Y[num]>0) {
			legend("center", legend = c(expression(paste(P[X](x))), bquote(paste(P[X~"|"~Y== .(num)](x)))),
				fill = c('cyan2', 'forestgreen'), box.col='white', cex=1.5, border='white')
		  }
	  }
  })

  output$condis <- renderPlot({
      if (anyNA(C$cjta)) return()
      par(mar=c(3.2,2,1,0))
      var = C$por
#	  num = input$val  # condic X==num si P_X(num) > 0
	  num = C$num
	  if (var=='X') {
		  if (C$X[num]>0) {
		      T = C$cjta[num,]/C$X[num]; n = length(T)
			  if (max(C$Y) > max(T)) {
	            mibarplot(C$Y, horiz=TRUE, brdr=NA, colr='goldenrod1', offset=0.18, dlt=0.15)
			    mibarplot(T, horiz=TRUE, brdr=NA, colr='forestgreen', offset=-0.18, add=TRUE, dlt=0.15)
			  } else {
			    mibarplot(T, horiz=TRUE, brdr=NA, colr='forestgreen', offset=-0.18, dlt=0.15)
	            mibarplot(C$Y, horiz=TRUE, brdr=NA, colr='goldenrod1', offset=0.18, add=TRUE, dlt=0.15)
			  }
			  axis(2, at=1:n)
		  }
	  } else {
		  if (C$Y[num]>0) {
		      T = C$cjta[,num]/C$Y[num]; n = length(T)
			  if (max(C$X) > max(T)) {
	            mibarplot(C$X, horiz=FALSE, brdr=NA, colr='cyan2', offset=0.18, dlt=0.15)
			    mibarplot(T, horiz=FALSE, brdr=NA, colr='forestgreen', offset=-0.18, add=TRUE, dlt=0.15)
			  } else {
			    mibarplot(T, horiz=FALSE, brdr=NA, colr='forestgreen', offset=-0.18, dlt=0.15)
	            mibarplot(C$X, horiz=FALSE, brdr=NA, colr='cyan2', offset=0.18, add=TRUE, dlt=0.15)
			  }
				axis(1, at=1:n)
		  }
	  }
  })
  #output$borrados <- renderTable({ if (!is.na(C$res)) C$res })

})