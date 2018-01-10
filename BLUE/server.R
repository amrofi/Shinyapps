library(shiny)
library(L1pack)

sq.error = function(b, X, Y) {
   y = b[1]+b[2]*X
   sum((y-Y)^2)
}
M = 1024
.L = read.table("etiquetas blue.txt", header=FALSE, sep='\t', quote="", row.names=1, comment.char = "", colClasses=rep("character", 3))

shinyServer(function(input, output) {

  SEL = list(ti=1, sh=NA)
  makeReactiveBinding("SEL")
  .E <- reactiveValues(d = 1)  #active language and default

  tr <- function(stringId) {
        .L[stringId, .E$d]
  }

  observeEvent(input$eng, {    .E$d=1   })
  observeEvent(input$spa, {    .E$d=2   })
  observeEvent(input$tipo, {    
     t = input$tipo
     SEL$ti <<- ifelse(t==tr("areas"), 1, 2)
	})
  observeEvent(input$show, {    
     SEL$sh <<- ifelse(input$show, 1, 2)
	})
    
  output$titulo = renderText( { tr("titulo") } )
  output$tab1 = renderText( { tr("tab1") } )
  output$tab2 = renderText( { tr("tab2") } )
  output$txt1 = renderUI( { withMathJax( tr("txt1") )} )
  output$txt2 = renderText( { tr("txt2") } )
  output$txt3 = renderText( { tr("txt3") } )
  output$txt4 = renderText( { tr("txt4") } )
  output$txt5 = renderText( { tr("txt5") } )
  output$txt6 = renderUI( { HTML(tr("txt6")) } )
  output$txt7 = renderUI( { HTML(tr("txt7")) } )
  output$borra = renderText( { tr("borra") } )
  output$super = renderText( { tr("super") } )
  output$mues = renderText( { tr("mues") } )
  output$beta1 = renderUI( { HTML(tr("beta1")) } )
  output$beta0 = renderUI( { HTML(tr("beta0")) } )
  output$ene = renderText( { tr("ene") } )
  output$simular = renderText( { tr("simular") } )
  output$sim.1 = renderUI( { HTML(tr("sim.1")) } )
  output$sim.2 = renderUI( { HTML(tr("sim.2")) } )
  output$sim.3 = renderText( { tr("sim.3") } )
  output$sim.4 = renderText( { tr("sim.4") } )
  output$conpan2 = renderUI({
	if (input$nor==1) {
		sliderInput("desv", HTML(tr('sig')), min = 0.1, max = 5, step = 0.025, value = 1)
	} else {wellPanel(
		sliderInput("esc", HTML(tr('esca')), min = 0.1, max = 5, step = 0.025, value = 1),
		HTML(tr('lapla'))
		)
	}
  })

  output$condpan = renderUI({
    if (input$show) {
        radioButtons("tipo", '', c(tr("areas"), tr("long")), 
	             selected=ifelse(SEL$ti==1, tr("areas"), tr("long")))
	}
  })

  
  XLim = c(-5,5)
  YLim = c(-5,5)
  v <- reactiveValues(L2 = NA, L1 = NA, rl = NA, px = c(), py = c())
	
  observeEvent(input$reset, {    # borrar la lista
          v$px = c()
		  v$py = c()
		  v$L2 = NA
		  v$L1 = NA
  })		  

  observeEvent(input$P, {    # añadir un punto a la lista
          v$px = c(v$px, input$P$x)
		  v$py = c(v$py, input$P$y)
		  if (length(v$px)>2) {
		     v$L2 = lm(v$py ~ v$px)
			 v$L1 = l1fit(v$px, v$py)
          }
  })
  
  observeEvent(input$Q, {    # trazar una recta
		  a = input$Q$x
		  b = input$Q$y  
          if (is.na(v$rl)) { # primer punto
		      v$rl = list(a=a,b=b)
		  } else if (is.list(v$rl)) {  #  ahora llega el segundo punto
		      re = array(NA, 2)
              re[2] = (b - v$rl$b)/(a - v$rl$a)
              re[1] = v$rl$b - re[2]*v$rl$a
              v$rl = re
		  } else {        # sería otra vez el primer punto, eliminando la recta anterior
		      v$rl = list(a=a,b=b)
		  }
  })

  output$fig <- renderPlot({
	       par(mar=c(1,1,1,1), col.axis='white', lwd=2)
	       plot(v$px, v$py, pch=16, xlim=XLim, ylim=YLim, main='A')
		   if (!is.na(v$L2)) {
		       abline(v$L2, col='black')
		   }
		   if (input$rectas & length(v$px)>2) {
			   abline(v$L1, col='pink')
			   if (is.array(v$rl)) abline(a=v$rl[1], b=v$rl[2], col='lightblue')
		   }
		   if (SEL$sh==1 & !is.na(v$L2)) {
			   e = v$L2$resid
			   if (SEL$ti==1) {
			       rect(v$px, v$py, v$px-e, v$py-e, border=NA, col=rgb(0.6,0.6,0.6,0.3))
			   } else {
			       segments(x0=v$px, y0=v$py, y1=v$py-e, col='grey')
			   }
		   }
  })
  
  output$fig2 <- renderPlot({
	       par(mar=c(1,1,1,1), col.axis='white', lwd=2)
	       plot(v$px, v$py, pch=16, xlim=XLim, ylim=YLim, main='B')
		   if (!is.na(v$L1)) {
		       abline(v$L1, col='red')
		   }
		   if (input$rectas & length(v$px)>2) {
			   abline(v$L2, col='grey')
			   if (is.array(v$rl)) abline(a=v$rl[1], b=v$rl[2], col='lightblue')
		   }
		   if (SEL$sh==1 & !is.na(v$L1)) {
			   e = v$L1$resid
			   if (SEL$ti==1) {
			       rect(v$px, v$py, v$px-e, v$py-e, border=NA, col=rgb(0.6,0.6,0.6,0.3))
			   } else {
			       segments(x0=v$px, y0=v$py, y1=v$py-e, col='grey')
			   }
		   }
  })
  
  output$fig3 <- renderPlot({
	       par(mar=c(1,1,1,1), col.axis='white', lwd=2)
	       plot(v$px, v$py, pch=16, xlim=XLim, ylim=YLim, main='C')
		   if (is.list(v$rl)) {
		       points(v$rl$a, v$rl$b, pch=19, col='green2')
		   } else if (!is.na(v$rl)) {
		       abline(a=v$rl[1], b=v$rl[2], col='blue')
		   }
		   if (input$rectas & length(v$px)>2) {
			   abline(v$L1, col='pink')
			   abline(v$L2, col='grey')
		   }
		   if (length(v$px)>2 & SEL$sh==1 & is.array(v$rl)) {
			   e = v$py - (v$rl[1]+v$rl[2]*v$px)
			   if (SEL$ti==1) {
			       rect(v$px, v$py, v$px-e, v$py-e, border=NA, col=rgb(0.6,0.6,0.6,0.3))
			   } else {
			       segments(x0=v$px, y0=v$py, y1=v$py-e, col='grey')
			   }
		   }
  })

  output$resum <- renderPlot({
     if (length(v$px)>2) {
	    if (is.array(v$rl)) {
		    e = v$py - (v$rl[1]+v$rl[2]*v$px)
		} else e=NA
		D = c(sum(v$L2$res^2), sum(v$L1$res^2), sum(e^2))
		E = c(sum(abs(v$L2$res)), sum(abs(v$L1$res)), sum(abs(e)))
		lado = sqrt(D/max(D, na.rm=TRUE))*0.6
		par(mar=c(0,0,0,0)+0.1)
		plot(0:1,0:1, t='n',xlab='',ylab='',axes=FALSE)
		rect(0,0,1,1, border='grey')
		rect(0,0,lado,lado,border=NA,col=rgb(0.35,0.17,0.57,0.3))
		text(0.63, 0.15, paste0("A: ", round(D[1],3)), pos=4)
		text(0.63, 0.1, paste0("B: ", round(D[2],3)), pos=4)
		text(0.63, 0.05, paste0("C: ", round(D[3],3)), pos=4)
		segments(x0=0,x1=E/max(E, na.rm=TRUE)*0.7,y0=c(0.85,0.8,0.75),lwd=4,col=rgb(0.17,0.57,0.35, 0.343))
		text(0.73, 0.85, paste0("A: ", round(E[1],3)), pos=4)
		text(0.73, 0.8, paste0("B: ", round(E[2],3)), pos=4)
		text(0.73, 0.75, paste0("C: ", round(E[3],3)), pos=4)
	 }
  })
  
    datasetInput <- eventReactive(input$go, {
     n = input$obs
     bet0 = input$bet0
     bet1 = input$bet1
	 eps = input$nor
	 if (eps==1) {
         sg = input$desv
		 E = rnorm(M*n, 0, sg)
	 } else {
	     sc = input$esc
		 E = rexp(M*n, 1/sc) - rexp(M*n, 1/sc)
	 }
	 X = seq(1, 10, len=n)
     Y = matrix(rep(bet0+bet1*X, M) + E, ncol=n, byrow=TRUE)
     mx = mean(X)
     sx = sd(X)
	 my = apply(Y, 1, mean)
	 sy = apply(Y, 1, sd)
	 rxy = apply(Y, 1, cor, X)
	 b1 = rxy*sy/sx
	 b0 = my - b1*mx
	 vabs = array(NA, dim=c(M, 2))    # coefs. de regresion L1
	 for (i in 1:M) {
	     modl1 = l1fit(X, Y[i,])
		 vabs[i,] = modl1$coef
	 }
	 # s = sqrt((n-1)/(n-2)*sy^2*(1-rxy^2))
     list(b0=b0, b1=b1, X=X, n=n, bet0=bet0, bet1=bet1, vabs=vabs, dis=eps, pam=ifelse(eps==1, sg, sc))
  })
  
  output$simus <- renderPlot({
	   data = datasetInput();
	   X = data$X;  bet0 = data$bet0;  bet1 = data$bet1;  b1 = data$b1; b0 = data$b0; vabs = data$vabs
	   par(mar=c(4,3,1.9,0.5), mfrow=c(1,3), cex.main=1.8)
	   x = range(X)
	   y = range(b0 + b1*x)
	   plot(x,y, t='n', xlab='X', ylab='Y')
	   rug(X)
	   segments(x[1], b0+b1*x[1], x[2], b0+b1*x[2], col=rgb(0.3,0.3,0.3,0.2))
	   abline(a=bet0, b=bet1, lwd=2, col='white')
	   j2 = density(b1)
	   j1 = density(vabs[,2])
	   hist(b1, border='white', ylim=c(0, 1.05*max(c(j2$y, j1$y))), main=tr('lineal'))
	   polygon(j2)
	   polygon(j1, border='red')
	   abline(v=bet1, col='blue')
	   k2 = density(b0)
	   k1 = density(vabs[,1])
	   hist(b0, border='white', ylim=c(0, 1.05*max(c(k2$y, k1$y))), main=tr('cept'))
	   polygon(k2)
	   polygon(k1, border='red')
	   abline(v=bet0, col='blue')
	   legend("topright", col=c('black', 'red'), lwd=2, leg=c(tr("mco"), tr("mda")))
  })

  output$stdev <- renderUI({
	   data = datasetInput();
	   b1 = data$b1; b0 = data$b0; vabs = data$vabs
       txt = "<style>table, th, td { border: 1px solid black; border-collapse: collapse; } th, td {text-align:center; padding:5px}</style>"
	   txt = paste0(txt, "<p/><table style='width:100%'><tr><th></th><th>sd b<sub>1</sub></th><th>sd b<sub>0</sub></th></tr>")
       txt = paste0(txt, "<tr><th>", tr('mco'), "</th><td>", round(sd(b1),3), "</td><td>", round(sd(b0),3), "</td></tr>")
	   txt = paste0(txt, "<tr><th>", tr('mda'), "</th><td>", round(sd(vabs[,2]),3), "</td><td>", round(sd(vabs[,1]),3), "</td></tr></table>")
       HTML(txt)
  })
  output$sigm <- renderUI({
	   data = datasetInput();
	   if (data$dis==1) {  # distribución Normal
	       X = data$X;  sg = data$pam;  n = data$n
	   } else {  # recalcular sigma para Laplace
	       X = data$X;  sg = 1.414213562373*data$pam;  n = data$n
	   }
	   mx = mean(X)
	   ssx = sum((X-mx)^2)
       sg.1 = sg/sqrt(ssx)
	   sg.0 = sg*sqrt(1/n + mx^2/ssx)
	   var.2 = '$$ \\sigma_{b1} = \\sqrt{\\frac {\\sigma^2}{\\sum{(X_i-\\bar{X})^2}}} = %g $$'
	   var.3 = '$$ \\sigma_{b0} = \\sqrt{\\sigma^2 \\left( \\frac{1}{n} + \\frac {\\bar{X}^2}{\\sum{(X_i-\\bar{X})^2}} \\right)} = %g $$'
       withMathJax(
		      helpText(tr('var.1')),
              sprintf(var.2, round(sg.1, 3)),
			  sprintf(var.3, round(sg.0, 3))
	   )
  })
})
  