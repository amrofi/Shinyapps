library(shiny)

parterect3 = function(Q, Z) { # devuelve la lista de subrectangulos
   n = length(Z)
   if (n<=1) return(Q)

   # Partir Z en 2:
   sz = sum(Z)
   zz = abs(cumsum(Z) - sz/2)
   k = which.min(zz)
   if (k<n) {
      Z1 = Z[1:k]; Z2 = Z[(k+1):n]
   } else {
      Z1 = Z[1:(k-1)]; Z2 = Z[k:n]
   }
   pct = sum(Z1)/sz
   x0 = Q[1]; y0 = Q[2]; x1 = Q[3]; y1 = Q[4]
   if (x1-x0 > y1-y0) {
           z = pct*x1 + (1-pct)*x0
	     return(rbind(parterect3(c(x0,y0,z,y1), Z1),
                        parterect3(c(z,y0,x1,y1), Z2)))
   } else {
           z = pct*y1 + (1-pct)*y0
	     return(rbind(parterect3(c(x0,y0,x1,z), Z1),
                        parterect3(c(x0,z,x1,y1), Z2)))
   }
}

Explotar = function(Q, M) { # Q: exterior, M: lista de subrect
   CG = c((Q[1]+Q[3])/2, (Q[2]+Q[4])/2)
   m = dim(M)[1]
   R = array(NA, dim=dim(M))
   for (i in 1:m) {
      if (M[i,1]<CG[1] & M[i,3]>CG[1] & M[i,2]<CG[2] & M[i,4]>CG[2]) {
	    R[i,] = M[i,]
	} else {
	    cg = c((M[i,1]+M[i,3])/2, (M[i,2]+M[i,4])/2)
	    X = sqrt(sum((CG-cg)^2))
	    R[i,] = M[i,] + c((cg-CG)*X/6, (cg-CG)*X/6)
	}
   }
   R
}

.L = read.table("etiquetas anova.txt", header=FALSE, sep='\t', quote="", row.names=1, colClasses=rep("character", 3))

# Define server logic for slider examples
shinyServer(function(input, output, session) {

  SEL = ''
  .E <- reactiveValues(d = 1)  #active language and default

  tr <- function(stringId) {
        .L[stringId, .E$d]
  }

  observeEvent(input$eng, {    .E$d=1   })
  observeEvent(input$spa, {    .E$d=2   })
  observeEvent(input$tipo, {    
     t = input$tipo
     SEL <<- ifelse(t==tr("pie1"), 1, 2)
	})
    
  output$titulo = renderText( { tr("titulo") } )
  output$tab1 = renderText( { tr("tab1") } ) 
  output$tab2 = renderText( { tr("tab2") } ) 
  output$par1 = renderText( { tr("par1") } )
  output$par2 = renderUI( { HTML(tr("par2")) } )
  output$par3 = renderText( { tr("par3") } )
  output$borrar = renderText( { tr("borrar") } )
  output$simula = renderText( { tr("simula") } )
  output$tamano = renderText( { tr("tamano") } )
  output$igual = renderText( { tr("igual") } )
  output$sigma = renderUI( { HTML(tr("sigma")) } )
  output$Type = renderUI({
    radioButtons("tipo", tr("pastel"), c(tr("pie1"), tr("pie2")), inline=TRUE, 
	             selected=ifelse(SEL==1, tr("pie1"), tr("pie2")))
  })
  output$par4 = renderUI( { HTML(tr("par4")) } )
  output$sucu = renderUI( { withMathJax(HTML(
	   paste0(tr("sucu1"), '= \\sum_j \\sum_i (y_{i,j} - \\bar{Y})^2 \\) <br>', tr("sucu2"), '= \\sum_j \\sum_i (y_{i,j} - \\bar{y}_j)^2 \\) <br>', tr("sucu3"), '= \\sum_j n_j (\\bar{y}_j - \\bar{Y})^2 \\)'))) 
  } )
  output$par5 = renderUI( { 
    withMathJax(HTML(tr("par5")))
  } )
  output$par6 = renderUI( { 
    withMathJax(p(tr("par6")))
  } )
  output$par7 = renderText( { tr("par7") } )
  output$par8 = renderUI( { 
    withMathJax(HTML(tr("par8")))
  } )
  output$par9 = renderText( { tr("par9") } )
  output$par10 = renderUI( { HTML(tr("par10")) } )
  output$par11 = renderUI( { HTML(tr("par11")) } )
  output$par12 = renderText( { tr("par12") } )

  
  palette(c(hsv(0.04, 0.95, 0.95), hsv(0.09, 0.95, 0.95), hsv(0.14, 0.95, 0.95), 
          hsv(0.65, 0.90, 0.90), hsv(0.58, 0.90, 0.90), hsv(0.5, 0.90, 0.90),
          hsv(0.0, 0.0, 0.3), hsv(0.0, 0.0, 0.48), hsv(0.0, 0.0, 0.66)))

	YLim = c(5,20)
  v <- reactiveValues(Y=c(), G=c(), noise=c(), pinta=FALSE, mark=NA, a=NA, b=NA)  # 
  
  observeEvent(input$P, {    # 
    x = input$P$x
	rx = round(x)
	if (abs(x-rx)<0.2 & rx >=1 & rx <=3) {
        v$Y = c(v$Y, input$P$y)
		v$G = c(v$G, rx)
		v$noise = c(v$noise, runif(1, -0.15, 0.15))
        u = table(factor(v$G, levels=1:3))
	    if (min(u)>0 & max(u)>1) { # hay datos en todas las categorias y más de 1 en alguna
	        v$pinta = TRUE
		}
		v$mark = NA
		v$a = NA; v$b = NA
	}
  })
  observeEvent(input$S, {    # sobre los rectangulos
    v$a = input$S$x
    v$b = input$S$y
  })
  observeEvent(input$reset, {    # 
	v$Y=c()
	v$G=c()
	v$noise=c()
	v$pinta = FALSE
	v$mark = NA
  })
  datasetInput <- reactive({
  if (length(v$Y)>0) { # algo hay
	 if (v$pinta) { # hay datos en todas las categorias y más de 1 en alguna
		mod = lm(v$Y~as.factor(v$G))
		scr = by(mod$res^2, v$G, sum)
		N = as.vector(table(v$G))
		y = mean(v$Y)
		med = by(v$Y, v$G, mean)
		sce = N*(med-y)^2
		resq = mod$res^2
		list(Z=c(resq, sce), scr=scr, sce=sce)
    }}
  })

  output$data <- renderPlot({
  if (length(v$Y)>0) {
    med = by(v$Y, factor(v$G, levels=1:3), mean)
	par(mar=c(2.2, 4, 0.1, 0.1), las=1)
	n = length(v$Y)
	plot(v$G+v$noise, v$Y, bg=v$G+6, pch=22, cex=1.5, xlab='', ylab='Y', axes=FALSE, xlim=c(0.5, 3.5), ylim=YLim)
	axis(2)
	axis(1, at=1:3, lab=1:3)
	segments(1:3-0.5, med, 1:3+0.5, med, col=7:9, lwd=2, lty=2)
	abline(h=mean(v$Y), col='black', lwd=2, lty=1)
	abline(v=1:3, col=grey(0.6), lty=3)
	if (!is.na(v$mark)) {
	   if (v$mark<=n) {  # es una observación de SCR
	       k = v$G[v$mark]
	       arrows(k-0.2, v$Y[v$mark], k-0.2, med[k], angle=20, len=0.1, col='purple', lwd=2, code=3)
	   } else {  # es una observación de SCE
	       k = v$mark-n
	       arrows(k+0.35, mean(v$Y), k+0.35, med[k], angle=20, len=0.1, col='green', lwd=2, code=3)
	   }
	}
  } else {
	par(mar=c(2.2, 4, 0.1, 0.1), las=1)
	plot(NA, xlab='', ylab='Y', axes=FALSE, xlim=c(0.5, 3.5), ylim=YLim)
	axis(2)
	axis(1, at=1:3, lab=1:3)
	abline(v=1:3, col=grey(0.6), lty=3)
  }
  })
  
  # Show the values 

  output$values <- renderPlot({
  if (v$pinta) {
    datos = datasetInput()
    if (input$tipo %in% .L["pie1",]) {
		Q = c(0,0,1,1)
		Z = datos$Z;
		M = parterect3(Q, Z)
		P = Explotar(Q, M)
		par(mar=c(0, 0, 0, 0))
		plot(NA, xlim=range(P[,c(1,3)]), ylim=range(P[,c(2,4)]), xlab='', ylab='', axes=FALSE)
		rect(P[,1], P[,2], P[,3], P[,4], border=NA, col=c(v$G, 4:6))
		if (!is.na(v$a)) {
			hit = P[,1]<v$a & P[,2]<v$b & v$a<P[,3] & v$b<P[,4]
			H = which(hit)
			if (length(H)==1) {
				v$mark = H
				n = length(v$Y)
				f = ifelse(H<=n, v$G[H], H-n+3)
				rect(P[H,1], P[H,2], P[H,3], P[H,4], col=f)
			}
		}
	} else {
		x = c(datos$sce, datos$scr)
		pie(x, col=c(4:6, 1:3))
	}
  }
  })

  output$Rdos <- renderUI({
  if (v$pinta) {
     datos = datasetInput()
	 R2 = round(100*sum(datos$sce)/(sum(datos$sce)+sum(datos$scr)), 1)
	 sprintf(tr("coefR2"), R2)
   }
  })

  output$summ <- renderUI({
  if (length(v$Y)>0) {
		y = mean(v$Y)
		med = by(v$Y, factor(v$G, levels=1:3), mean)
		withMathJax(paste0('\\( \\quad \\bar{Y} \\) = ', round(y, 1)),
		            paste0('\\( \\quad \\bar{y}_1 \\) = ', round(med[1], 1), 
					       '\\( \\quad \\bar{y}_2 \\) = ', round(med[2], 1), 
						   '\\( \\quad \\bar{y}_3 \\) = ', round(med[3], 1) ))
   }
  })

  output$tabla <- renderUI({
  if (v$pinta) {
      n = length(v$Y)
      datos = datasetInput()
	  sce = round(sum(datos$sce), 1); cme = round(sum(datos$sce)/2, 2)
	  scr = round(sum(datos$scr), 1); cmr = round(sum(datos$scr)/(n-3), 2)
	  sct = round(sum(c(datos$sce, datos$scr)), 1)
	  F = round((sce/2)/(scr/(n-3)), 2)
      css = "<style>table, th, td { border: 1px solid black; border-collapse: collapse; } th, td {text-align:center; padding:5px}</style>"
      txt = paste0('<table><tr><th></th><th>', tr("SS"), '</th><th>', tr("DF"), '</th><th>', tr("MS"), '</th><th>', tr("ratio"), '</th></tr>')
	  txt = paste0(txt, '<tr><td>', tr("explic"), '</td><td>',sce,'</td><td>2</td><td>',cme,'</td><td>',F,'</td></tr>')
	  txt = paste0(txt, '<tr><td>', tr("residu"), '</td><td>',scr,'</td><td>',n-3,'</td><td>',cmr,'</td></tr>')
	  txt = paste0(txt, '<tr><td>', tr("total"), '</td><td>',sct,'</td><td>',n-1,'</td></tr></table>')
	  HTML(paste0(css,txt))
	}
  })


  M = 1000
  w <- reactiveValues(esti = NA, rej = NA, sg=NA)
  
  observeEvent(input$go, {    # 
    progress <- shiny::Progress$new(session, min=1, max=M)
	on.exit(progress$close())
	progress$set(message = 'Calculation in progress', detail = 'This may take a while...')
	if (input$iguales) {
	    Mu = rep(input$mu, 3)
	} else {
	    Mu = c(input$mu1, input$mu2, input$mu3)
	}
	w$sg = input$desv
	N = input$obs
	esti = array(NA, dim=c(M, 2))
	reject = array(NA, dim=M)
	for ( i in 1:M ) {
	   if (i%%100==0) progress$set(value = i)
	   X = sample(1:3, N, repl=TRUE)
	   Y = rnorm(N)*w$sg + Mu[X]
	   mod = lm(Y ~ as.factor(X))
	   SS = summary.aov(mod)
	   esti[i,] = SS[[1]]["Mean Sq"][,1]
	   reject[i] = SS[[1]]$Pr[1]<0.05
	}
	w$esti = esti
	w$rej = reject
  })
  
  output$graf = renderPlot({
     if (!is.na(w$esti)) {
	    verde = 'forestgreen'
		lila = 'purple'
		naran = 'orange'
	    N = input$obs
		layout(matrix(c(1,2,3,3), ncol=2))
		XLim = c(0, quantile(w$esti, 0.995))
		bre = pretty(XLim, 15)
		par(mar=c(3.0, 2.8, 1, 0.5))
		hist(w$esti[,1], main=tr("mainE"), xlim=XLim, bre = c(bre, max(w$esti)))
		abline(v=w$sg^2, col=lila, lwd=2, lty=2)
		abline(v=mean(w$esti[,1]), col=verde)
		hist(w$esti[,2], main=tr("mainR"), xlim=XLim, bre=bre, freq=FALSE)
		abline(v=w$sg^2, col=lila, lwd=2, lty=2)
		abline(v=mean(w$esti[,2]), col=verde)
		F = w$esti[,1]/w$esti[,2]
		FLim = c(0, quantile(F, 0.995))
		hh=hist(F, bre = c(pretty(FLim, 15), max(F)), plot=FALSE)
		xx = max(hh$dens)
		hist(F, main=tr("ratio"), xlim=FLim, bre = c(pretty(FLim, 15), max(F)), ylim=c(-0.1,1)*xx, axes=FALSE)
		axis(1)
		axis(2, at=pretty(c(0,xx)))
		f = seq(0, FLim[2], len=100)
		fd = df(f, 2, N-3)
		lines(f, fd, col=naran, lwd=2)
		lines(rep(N/(N-2),2),c(0,-0.1*xx), col=lila, lwd=3, lty=2)
		lines(rep(mean(F),2),c(0,-0.1*xx), col=verde, lwd=2)
		legend("topright", leg=c(tr("expec"), tr("media")), 
				lwd=c(2,1), lty=c(2,1), col=c(lila, verde))
		legend("right", leg=paste0("F(2,", N-3, ")"), lwd=2, col=naran)

	 }
  })
  
  output$rollo <- renderUI({
     txt = ''
     if (!is.na(w$esti)) {
		Hcero = !input$iguales & var(c(input$mu1, input$mu2, input$mu3))==0
		if (input$iguales | Hcero) {
		    txt = sprintf(tr("roll01"), input$obs-3)
			fails = table(w$rej)[2]
			alfa = round(fails/M*100, 1)
			txt = paste0(txt, sprintf(tr("roll02"), fails, M, alfa))
		} else {
			txt = sprintf(tr("rollx1"), input$obs-3)
			accep = table(w$rej)[1]
			beta = round(accep/M*100, 1)
			txt = paste0(txt, sprintf(tr("rollx2"), accep, M, beta))
		}
	} 
	withMathJax(HTML(txt))
  })
})
