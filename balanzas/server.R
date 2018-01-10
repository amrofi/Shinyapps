.L = read.table("etiquetas balanzas.txt", header=FALSE, sep='\t', quote="", row.names=1, colClasses=rep("character", 3))
library(shiny)
library(png)

path = '.'
z = readPNG(paste0(path, "/851pumpkin_normal.png"))
z2 = readPNG(paste0(path, "/851pumpkin_claro.png"))

# Define server logic for slider examples
shinyServer(function(input, output) {

.E <- reactiveValues(d = 1)  #active language and default

  tr <- function(stringId) {
        .L[stringId, .E$d]
  }

  observeEvent(input$eng, {    .E$d=1   })
  observeEvent(input$spa, {    .E$d=2   })
  
  output$titulo = renderText( { tr("titulo") } )
  output$par1 = renderUI( { HTML(tr("par1")) } )  
  output$par2 = renderUI( { HTML(tr("par2")) } )  
  output$num = renderText( { tr("num") } )
  output$pesa = renderText( { tr("pesa") } )
  output$calcula = renderText( { tr("calcula") } )
  output$borra = renderText( { tr("borra") } )
  output$marca = renderText( { tr("marca") } )
  output$dets = renderText( { tr("dets") } )
  output$txt1 = renderUI( { withMathJax(tr("txt1")) } )
  output$txt2 = renderUI( { withMathJax(tr("txt2")) } )


	TamPag = 20
	maxY = (TamPag-1)*10/TamPag
	stepY = 10/TamPag
	stepX = stepY

  V <- reactiveValues(n=4, page=1, sel=c(), X=NA, C=NA, cal=NA, xal=NA, sol=NA)

  settings = function() {
		  n = V$n
		  W = expand.grid(seq(1, 7.5, len=n), seq(0, maxY, by=stepY))
		  V$C = data.frame(x0=W$Var1, y0=9.5-W$Var2)    # coordenadas, no depende de la pagina
		  V$X = setdiff(1:(2^n-1), 2^(0:(n-1)))       # combinaciones posibles (mas de 1 calab)
		  V$sel = rep(0, length(V$X))
  }
  
  observeEvent(input$calc, {    # 
		  n = V$n
		  d = sum(V$sel)
		  A = matrix(NA, ncol=n, nrow=n+d)
		  if (d>0) {
		    A[1:n, 1:n] = diag(n)
			D = which(V$sel==1)
		    Q = matrix(intToBits(V$X[D]), ncol=32, byrow=TRUE)
		    q2 = as.numeric(Q[1:d, 1:n])
			A[(n+1):(n+d), 1:n] = matrix(q2, ncol=n)
			B = c(V$cal, round(matrix(q2, ncol=n) %*% V$xal, -2))
			V$sol = solve(t(A)%*%A, t(A)%*%B)
		  } else {
		    V$sol = V$cal
		  }
  })

  observeEvent(input$ene, {    # 
          V$n=input$ene
		  V$xal = round(runif(V$n, 2000, 5000))  # peso exacto calabazas
		  V$cal = round(V$xal, -2)               # peso medido
		  settings()
		  V$page = 1
		  V$sol = NA
  })
  
  observeEvent(input$set, {    # 
		  V$sel = rep(1, length(V$X))
  })
  
  observeEvent(input$reset, {    # 
		  V$sel = rep(0, length(V$X))
  })

  observeEvent(input$P, {    # 
          if (is.na(V$X)) settings()
          x = input$P$x
		  y = input$P$y
		  m = length(V$X)
		  last.p = ceiling((m-1)/TamPag)
		  if (y>10 & x<0.25 & V$page>1) {    #back
		      V$page = V$page-1
		  } else if (y>10 & x>8.8 & x<9.5 & V$page<last.p) {    #forward
		      V$page = V$page+1
		  } else if (x<0.25 & y<9.75) {    # select/unselect
		      p = ifelse(V$page<last.p, TamPag, 1+(m-1)%%TamPag)
              u = (9.75-y)/stepY + 1
			  q = round(u)
			  if (q <=p & abs(u-q)<0.5) {
			      i = (V$page-1)*TamPag + q
				  V$sel[i] = 1-V$sel[i]
			  }
		  }
	})
	
  output$pesadas <- renderPlot({
          if (is.na(V$X)) settings()
		  n = V$n
		  m = length(V$X)
		  ini.p = 1+TamPag*(V$page-1)
		  fin.p = min(m, TamPag*V$page)
		  last.p = ceiling((m-1)/TamPag)
		  p = fin.p-ini.p+1
		  Q = matrix(intToBits(V$X[ini.p:fin.p]), nrow=32)
		  q2 = as.numeric(as.vector(Q[1:n, 1:p]))
		  par(mar=c(0.5, 0.5, 0.5, 0.5))
		  plot(NA, xlim=c(0,10), ylim=c(0, 10), xlab='', ylab='', axes=FALSE)
		  c = subset(V$C[1:(n*p),], q2==1)
		  x0 = c$x0
		  y0 = c$y0
		  x1 = x0+stepX
		  y1 = y0+stepY
		  rasterImage(z, x0, y0, x1, y1)
		  c = subset(V$C[1:(n*p),], q2==0)
		  x0 = c$x0
		  y0 = c$y0
		  x1 = x0+stepX
		  y1 = y0+stepY
		  rasterImage(z2, x0, y0, x1, y1)
		  text(V$C$x0[1:n]+stepX/2, 10, 1:n, pos=3, cex=1.5)
		  y = unique(V$C[1:(n*p), "y0"])+0.125
		  rect(0, y, 0.25, y+0.25)
		  points(-0.1, 10.3, pch=25, bg=ifelse(V$page>1,'black','grey'), cex=2)
		  points(8.9, 10.2, pch=24, bg=ifelse(V$page<last.p,'black','grey'), cex=2)
		  text(9.7, 10.2, paste0(V$page, '/', last.p))
	      mrk = which(V$sel[ini.p:fin.p]==1)
		  K = length(mrk)
		  if (K>0) {
		      points(rep(0.125, K), y[mrk]+0.125, pch=4, cex=1.5)
			  pesos = V$xal %*% matrix(as.numeric(Q[1:n, mrk]), ncol=K)
			  text(rep(9.2, K), y[mrk]+0.125, paste0("= ", round(pesos,-2)))
		  }
  })
    
  output$orig = renderTable({
       df=data.frame(peso=V$xal, medido=V$cal, estimado=round(V$sol))
	   names(df) = c(tr('peso'), tr('medido'), tr('estim'))
	   df
  }, digits=c(0,0,0,0))
  
  output$barp = renderPlot({
       Err = c(sum(abs(V$xal-V$cal)), sum(abs(V$xal-V$sol)))
	   par(mar=c(4,2.1, 2, 0))
	   barplot(Err, names=c(tr('medido'), tr('estim')), space=0.8,
	           col='lightblue', main=tr('desvio'))
  })
})
