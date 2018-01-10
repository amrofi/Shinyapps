library(shiny)
library(rootSolve)
library(alabama)

source("max likelihood.R")

.L = read.table("etiquetas vac.txt", header=FALSE, sep='\t', quote="", row.names=1, comment.char = "", colClasses=rep("character", 3))

shinyServer(function(input, output, session) {

  SEL = 0
  makeReactiveBinding("SEL")  
  .E <- reactiveValues(d = 1)  #active language and default

  tr <- function(stringId) {
        .L[stringId, .E$d]
  }

  observeEvent(input$eng, {    .E$d=1   })
  observeEvent(input$spa, {    .E$d=2   })

  output$titulo = renderText({ tr('titulo') })
  output$tab1 = renderText({ tr('tab1') })
  output$tab2 = renderText({ tr('tab2') })
  output$tab3 = renderText({ tr('tab3') })
  output$txt1 = renderText({ tr('txt1') })
  output$txt2 = renderText({ tr('txt2') })
  output$txt3 = renderUI({ HTML(tr('txt3')) })
  output$txt4 = renderText({ tr('txt4') })
  output$txt5 = renderText({ tr('txt5') })
  output$txt6 = renderUI({ HTML(tr('txt6')) })
  output$txt7 = renderText({ tr('txt7') })
  output$txt8 = renderText({ tr('txt8') })
  
  output$Tune = renderUI({
	selectInput("micro", tr("fino"), c(tr('sele'), tr('alffa'), 'beta', 'gamma'),
		selected = ifelse(SEL==0, tr('sele'), ifelse(SEL==1, tr('alffa'),
				   ifelse(SEL==2, 'beta', 'gamma'))))
  })
  output$ayuda = renderUI({ HTML(tr('ayuda')) })
  output$estim = renderText({ tr('estim') })
  output$info = renderText({ tr('info') })
  output$ene = renderText({ tr('ene') })
  output$mues = renderText({ tr('mues') })
  
  A = sample( 2:10, 1)    # el extremo izquierdo es 1/A
  ax = 1/A
  bx = sample(c(2, 2.5, 3, 3.5, 4, 4.5), 1)

  v <- reactiveValues(d=NA, p=NA, q=NA, r=NA, ok=NA)
  ft <-reactiveValues(z=0)
  data <-reactiveValues(u=NA)
  
  observeEvent(input$delta, { v$d = input$delta })
  observeEvent(input$alfa, { v$p = input$alfa })
  observeEvent(input$beta, { v$q = input$beta })
  observeEvent(input$gamma, { v$r = input$gamma })
  
  observeEvent(input$micro, {
      sig = input$micro
      SEL <<- ifelse(sig==tr('sele'), 0, 
				ifelse(sig==tr('alffa'), 1,
					ifelse(sig=='beta', 2, 3)))
	  if (SEL != ft$z) {     # mirar si había uno diferente y apagar
	     if (ft$z==1) {
		     updateSliderInput(session, "alfa", min = -4, max = 4, step=0.025, value=v$p)
		 } else if (ft$z==2) {
		     updateSliderInput(session, "beta", min = -4, max = 4, step=0.025, value=v$q)
		 } else if (ft$z==3) {
		     updateSliderInput(session, "gamma", min = -4, max = 4, step=0.025, value=v$r)
		 }              # Ahora encender el nuevo
	     if (SEL==1) {
		     updateSliderInput(session, "alfa", min = v$p-0.05, max = v$p+0.05, step=0.0005, value=v$p)
		 } else if (SEL==2) {
		     updateSliderInput(session, "beta", min = v$q-0.05, max = v$q+0.05, step=0.0005, value=v$q)
		 } else if (SEL==3) {
		     updateSliderInput(session, "gamma", min = v$r-0.05, max = v$r+0.05, step=0.0005, value=v$r)
		 } 
	  }

	  ft$z = SEL
  })

  fun = function(x) { v$d + 0.1 * (v$p * x + v$q * sqrt(x) + v$r / sqrt(x)) }
  fun2 = function(x) { 0.1 * (v$p + 0.5*v$q / sqrt(x) - 0.5*v$r / sqrt(x) / x) }
  FunX = function(x) { 1/60*sqrt(x)*(3*v$p*x^1.5+4*v$q*x+12*v$r) - 1/60*sqrt(ax)*(3*v$p*ax^1.5+4*v$q*ax+12*v$r) + v$d*(x-ax)}
  xfun = function(x) { x*fun(x) }
  xxfun = function(x) { x*x*fun(x) }

  output$fig <- renderPlot({  
     xtr = uniroot.all(fun2, c(ax,bx))
	 f = fun(c(ax, xtr, bx))
	 rng = c(min(0,min(f)), max(0, max(f)))
     dx = (bx-ax)/300
     rts = uniroot.all(fun, c(ax, bx))
	 pts = c(ax, rts, bx)
     par(mar=c(4,4,2,0.1))
     curve(fun, ax, bx, ylab=tr('dens'), xlab='x', type='n', ylim=rng)
	 title(main=expression(delta + 1/10(alpha*x + beta*sqrt(x) + gamma/sqrt(x))))
	 for (i in 1:(length(pts)-1)) {
	    y = fun((pts[i]+pts[i+1])/2)
		fil = ifelse(y>0, rgb(0,0,1,0.4), rgb(1,0,0,0.2))
		X = seq(pts[i], pts[i+1], by=dx)
		Y = fun(X)
		polygon(c(pts[i],X,pts[i+1]), c(0,Y,0), col=fil, border=NA)
		lines(X,Y,lwd=2)
	 }
  })

  output$figdis <- renderPlot({  
	 y = FunX(bx)
     clr = ifelse(abs(y-1)<1e-4, 'black', 'red')
     par(mar=c(4,4,2,0.1), lwd=2)
     curve(FunX, ax, bx, ylab=tr('cum'), xlab='x', xlim=c(0, 1.2*bx), col=clr)	 
	 lines(c(0, ax), c(0,0), col=clr)
	 lines(c(bx, 1.2*bx), c(y,y), col=clr)
  })
  
  output$rango <- renderUI({  
     HTML(sprintf(tr('determ'), A, bx))
  })
  
  output$integ <- renderUI({
     txt = ''
     xtr = uniroot.all(fun2, c(ax,bx))
	 pts = fun(c(ax, xtr, bx))
	 if (min(pts) < (-1e-4)) {
	     txt=tr('noneg')
	 }
     #g = integrate(fun, ax, bx)
	 g = FunX(bx)
	 v$ok = (txt=='' & abs(g-1)<1e-4)
     HTML(sprintf(tr('area'), round(g,4)))
  })
  
  output$momen <- renderUI({
    if (v$ok) {
      espe = integrate(xfun, ax, bx)$value
      var = integrate(xxfun, ax, bx)$value - espe^2
      HTML(sprintf(tr('indic'), round(espe, 4), round(var, 4), round(sqrt(var), 4)))
    } else {
      HTML(tr('notyet'))
	}
  })
  
  nsim <- eventReactive(input$sim, {
    input$ene
  })
  
  output$sampl <- renderPlot({  
    M = nsim()
    if (v$ok) {
		g = function(x,u) { FunX(x)-u }
		U = runif(M)
		for ( i in 1:M) {
		   U[i] = uniroot(g, c(ax, bx), U[i])$root
		}
		data$u = U
		med = mean(U)
		std = sd(U)
        xtr = uniroot.all(fun2, c(ax,bx))
	    pts = fun(c(ax, xtr, bx))
		v = 1.2*max(pts)
        mu = integrate(xfun, ax, bx)$value
		par(mar=c(4,3,0.5,0.5))
		hist(U, freq=FALSE, main='', xlab='X', bre=seq(ax,bx, len=15), ylim=c(-v/5,v))
        curve(fun, ax, bx, col='brown', add=TRUE)
  	    arrows(mu, -v/9, mu, 0, lwd=3, col='brown', angle=20)
	    text(mu, -v/6, expression(mu), col='brown', cex=1.6, pos=ifelse(med<mu, 4, 2))
  	    arrows(med, -v/9, med, 0, lwd=3, col='purple', angle=20)
		text(med, -v/6, expression(bar(x)), col='purple', cex=1.6, pos=ifelse(med<mu, 2, 4))
   }
  })
  
  output$resumen <- renderUI({
    if (v$ok & !is.na(data$u)) {
        sm = capture.output(summary(data$u))
		ds = sd(data$u)
		HTML(sprintf(tr('indi2'), paste(sm, collapse='<br>'), round(ds, 4)))
   }
  })
  
  K = 24                 # numero de bins para la muestra introducida por el usuario
  Xran = c(1/3, 4)       # fijamos el rango de la X para la muestra
  H <- reactiveValues(tab=array(5, dim=K),         # frecuencia inicial
#					  pcero=NA,					  # starting point
					  ans=NA,					# resultado de la estimación
                      alt=7)                 # altura inicial del grafico; ylim es c(0, 1.15alt); (+) y (-) estan en 1.1alt
  observeEvent(input$P, {
		  a = input$P$x
		  b = input$P$y
          if (abs(b - 1.1*H$alt) < (0.035*H$alt)) {   # un zoom?
		      if (abs(a - (Xran[1]+0.7)) < 0.1) {     #   +
			      H$alt = 0.8*H$alt
			  } else if (abs(a - (Xran[1]+1)) < 0.1) {#   -
			      H$alt = 1.25*H$alt
			  }
		  } else if (b > 0 & b < H$alt & a > Xran[1] & a < Xran[2]) {
		      i = floor((a-Xran[1])*K/diff(Xran))+1
			  H$tab[i] = round(b)
			  # si tocamos datos, borramos estimación anterior
			  H$ans = NA
		  }
  })

#  observeEvent(input$startp, {
#        a = as.numeric(strsplit(input$startp, " ")[[1]])
#		if (is.numeric(a) & length(a)==4) {
#		    H$pcero = a
#		} else {
#		    H$pcero = NA
#		}
#  })
  
  observeEvent(input$go, {
        if (sum(H$tab)>0) {
		    xx = (0.5+0:(K-1))*diff(Xran)/K + Xran[1]
		    datos = data.frame(a=xx, b=sqrt(xx), c=1/sqrt(xx), m=H$tab)
			bounds = cbind(c(-3, -1000, -1000, -1000), c(3, 1000, 1000, 1000))
			var2 = list(limi=Xran, datos=datos, bounds=bounds)
#			if (is.na(H$pcero)) {
			     P0 = c(1/diff(Xran), 0, 0, 0)
#			} else {
#				 P0 = H$pcero
#			}
			H$ans = auglag(par=P0, fn=fn, gr=gr, heq=heq, heq.jac=heq.jac, hin=hin, hin.jac=hin.jac, control.outer=list(trace=FALSE), var=var2)
		}
	})
		
  output$mumo <- renderPlot({  
	Y = c(0, 1.15*H$alt)
	par(mar=c(3,3,0.8,0.2))
	plot(Xran, Y, t='n', xlab='', ylab='')
	yz = 1.1*H$alt
	text(Xran[1]+0.3, yz, 'zoom:',cex=1.5)
	text(Xran[1]+0.7, yz, 'O',cex=2)
	text(Xran[1]+1, yz, 'O',cex=2)
	text(Xran[1]+0.7, yz, '+',cex=2)
	text(Xran[1]+1, yz, '-',cex=2)
	x = seq(Xran[1], Xran[2], len=25)
	y = H$tab
	y[y>H$alt] = H$alt
	rect(x[1:24], 0, x[2:25], y, col='grey', border='white')
	text(Xran[2], yz, sprintf(tr('size'), sum(H$tab)), cex=1.25, pos=2, col='darkgreen')
	if (!is.na(H$ans)) {
	   if ( (H$ans)[[4]]==0 ) {  # convergence
	      x2 = seq(Xran[1], Xran[2], len=100)
		  sol = (H$ans)[[1]]
		  y2 = ( sol[1] + 0.1 * (sol[2] * x2 + sol[3] * sqrt(x2) + sol[4] / sqrt(x2)) )*diff(Xran)/K*sum(H$tab)
		  lines(x2, y2)
	   }
	}
  })	  

    output$value <- renderUI({ 
	    c = (H$ans)[[1]]    # estimaciones
		if (!is.na(c)) {
		    withMathJax(
                sprintf("$$\\hat \\alpha = %.03f$$ $$\\hat \\beta = %.03f$$ $$\\hat \\gamma = %.03f$$ $$\\hat \\delta = %.03f$$",
				        c[2], c[3], c[4], c[1])
		    )
		}
	})
})
