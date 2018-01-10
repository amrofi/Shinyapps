.L = read.table("etiquetas a-ojo.txt", header=FALSE, sep='\t', quote="", row.names=1, colClasses=rep("character", 3))

library(shiny)

sq.error = function(b, X, Y) {
   y = b[1]+b[2]*X
   sum((y-Y)^2)
}

shinyServer(function(input, output) {

  .E <- reactiveValues(d = 1)  #active language and default

  tr <- function(stringId) {
        .L[stringId, .E$d]
  }

  observeEvent(input$eng, {    .E$d=1   })
  observeEvent(input$spa, {    .E$d=2   })
    
  output$titulo = renderText( { tr("titulo") } )
  output$parr = renderText( { tr("parr") } ) 
  output$soluc = renderText( { tr("soluc") } ) 
  output$marca = renderText( { tr("marca") } )
  output$mapa = renderText( { tr("mapa") } )
  output$nrecta = renderText( { tr("nrecta") } )
  
  v = reactiveValues(X=NA, Y=NA, Coef=NULL, mse=NA, u=NULL, v=NULL, w=NULL, L=NULL, Pto=matrix(NA, ncol=2, nrow=2), Rec=NULL, sqe=NA)
  
  NuevaNube = function() {
   n = sample(20:35, 1)
   x0 = runif(1, -10, 10)
   x1 = x0 + rexp(1, 0.1)
   Sg = runif(1, 0.1, 1)
   X = seq(x0, x1, len=n)
   bet0 = runif(1, -4, 4)
   bet1 = runif(1, -1, 1)
   Y = bet0 + bet1*X + rnorm(n, me=0, sd=Sg)
   P = lm(Y~X)
   Coef = P$coef
   mse = sum(P$resid^2)
   sm = summary(P)
   t0 = 2.8*sm$coef[1,2]
   t1 = 2.8*sm$coef[2,2]
   u = seq(Coef[1]-t0, Coef[1]+t0, len=25)
   vv = seq(Coef[2]-t1, Coef[2]+t1, len=25)
   w = array(NA, dim=c(25, 25))
   b = c(NA, NA)
   for (i in 1:25) {
      b[1] = u[i]
      for (j in 1:25) {
         b[2] = vv[j]
         w[i,j] = sq.error(b,X,Y)
      }
   }
   L=pretty(w, 8)
   if (min(L)<mse) {
       L[1] = ceiling(0.98*mse+0.02*L[2])
   }	
   v$X = X; v$Y = Y; v$Coef = Coef; v$mse = mse; v$u = u; v$v = vv; v$w = w; v$L = L
  }
  
Myenv = reactiveValues(df = as.data.frame(matrix(NA, ncol=3, nrow=0)))
NuevaNube()

  observeEvent( input$P, {
	 if (is.na(v$Pto[1,1])) {
		v$Pto[1,] = c(input$P$x, input$P$y)
		return()
	 } else {  # 2nd point
	    v$Pto[2,] = c(input$P$x, input$P$y)
		re = c(NA, NA)
		re[2] = diff(v$Pto[,2])/diff(v$Pto[,1])
        re[1] = v$Pto[1,2] - re[2]*v$Pto[1,1]
		v$Rec = re
		v$sqe = sq.error(re, v$X, v$Y)
		Z = c(re, v$sqe)
	    Myenv$df = rbind(Myenv$df, Z)
		v$Pto[1,1] = NA
	 }
  })
  
  observeEvent( input$newr, { 
      NuevaNube() 
	  Myenv$df = as.data.frame(matrix(NA, ncol=3, nrow=0))
	  v$Pto=matrix(NA, ncol=2, nrow=2) 
	  v$Rec=NULL 
	  v$sqe=NA
  })

  output$txt <- renderText({
	b0 = v$Coef[1]
	b1 = v$Coef[2]  
	if (input$go) {
      sgn = ifelse(b1<0, '', '+')	   
	   return(sprintf(tr("opt"), round(b0,3), sgn, round(b1,3)))
	}  
    re=v$Rec
    if (!is.null(re[1])){
       sgn = ifelse(re[2]<0, '', '+')
	   re = round(re, 3)
       sprintf(tr("traz"), re[1], sgn, re[2])
    } else {''}
  })

  output$txt2 <- renderText({
     if (input$go) {
       return(paste(tr("suma"), round(v$mse, 3)))
	 }  
     re=v$Rec
     if (!is.null(re[1])){
       paste(tr("suma2"), round(v$sqe, 2))
     } else {''}
  })

  output$fig <- renderPlot({

     par(mar=c(5,4,0.5,0.5))
     plot(v$X, v$Y, xlab='X', ylab='Y')
	 if (input$go) {  # nos piden que dibujemos la solución
         abline(a=v$Coef[1], b=v$Coef[2], col='green', lwd=2)
		 return()
	 }
     if (!is.na(v$Pto[1,1])) {  # tenemos solo el primer punto
         points(v$Pto[1,1], v$Pto[1,2], pch=19, col='red')
     } else {
	     if (!is.null(v$Rec[1])) abline(a=v$Rec[1], b=v$Rec[2], col='red')  # tenemos la recta del usuario
     }
  })

  output$DF <- renderTable({
     df=Myenv$df
	 names(df) = c(tr('T0'),tr('T1'),tr('SC'))
	 s = order(df[,3])
	 g = dim(df)[1]
	 if (g==0) {
	   df
	 } else {
	   df[s[1:min(g,10)],]
	 }
  })
  
    output$cont <- renderPlot({  # contour plot

    par(mar=c(5,4,0.5,0.5))
    contour(v$u,v$v,v$w, levels=unique(v$L), xlab=expression(b[0]), ylab=expression(b[1]), labcex=0.8)
	if (input$go) {
	     b0 = v$Coef[1]
		 b1 = v$Coef[2]
         color = 'green'
	} else {
      if (!is.null(v$Rec[1])) {
	     b0 = v$Rec[1]
		 b1 = v$Rec[2]
         color = 'blue'
      }
	}
	if (exists("b0")) {
        points(b0, b1, pch=24, col=color, cex=1.3)
        points(b0, b1, pch=25, col=color, cex=1.3)
	}
  })


})
