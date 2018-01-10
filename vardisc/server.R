library(shiny)

.L = read.table("etiquetas vardisc.txt", header=FALSE, sep='\t', quote="", row.names=1, colClasses=rep("character", 3))

die = factor(1:6, levels=1:6)
n.d = 30

faces = list(c(1/2,1/2), c(1/3,2/3,2/3,1/3), c(1/4,1/2,3/4,1/4,1/2,3/4),
    c(1/3,2/3,2/3,1/3,1/3,2/3,1/3,2/3), c(1/4,1/4,3/4,3/4,1/2,1/4,3/4,1/4,3/4,1/2),
    c(1/4,1/2,3/4,1/4,1/2,3/4,1/3,1/3,1/3,2/3,2/3,2/3))

caradado = function(ox, oy, lx, ly, z) {
  D = faces[[z]]
  a = D[1:z]*lx + ox
  b = D[(z+1):(2*z)]*ly + oy
  list(a = a, b = b)
}

update_p = function(p, b, k) {
   nz = which(p > 0)
   if (length(nz)==1 & nz[1]==k) return(p)
   x = max(0, b)
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
  output$tab1 = renderText( { tr("tab1") } )
  output$tab2 = renderText( { tr("tab2") } )
  output$tab3 = renderText( { tr("tab3") } )
  output$par1 = renderText( { tr("par1") } )
  output$par2 = renderText( { tr("par2") } )
  output$par3 = renderUI( { HTML(tr("par3")) } )  
  output$par4 = renderText( { tr("par4") } )  
  output$par5 = renderText( { tr("par5") } )  
  output$par6 = renderText( { tr("par6") } )  
  output$par7 = renderText( { tr("par7") } )  
  output$par8 = renderText( { tr("par8") } )  
  output$par9 = renderText( { tr("par9") } )  
  output$null = renderText( { tr("null") } )  
  output$roll = renderText( { tr("roll") } )  
  output$zero = renderText( { tr("zero") } )  
  output$lnza = renderText( { tr("lnza") } )  
  output$cumm = renderText( { tr("cumm") } )  
  
  v <- reactiveValues(p=array(1/6, dim=6))  # probabilidades
  f <- reactiveValues(data=array(0, dim=6), # frecuencias observadas
					  m = c(),              # las medias
					  s = NULL)             # los n ultimos

  observeEvent(input$reset, {
    v$p <-rep(1/6, 6)
    f$data <-rep(0, 6)
	f$s = NULL
	f$m = c()
  })
 
  observeEvent(input$cero, {
    f$data <-rep(0, 6)
	f$s = NULL
	f$m = c()
  })
 
  observeEvent(input$lanza, {
      f$s = sample(die, n.d, replace=TRUE, prob=v$p)
      f$data = f$data + table(f$s)
	  f$m = c(f$m, sum(as.numeric(f$s)))
  })
 
  observeEvent(input$P, {
		  a = input$P$x
		  b = input$P$y
		  if (abs(round(a)-a)<0.3 & b<1) {
			 a = round(a)
			 p = update_p(v$p, b, a)
			 v$p <- p
			 f$data <-rep(0, 6)   # reinit datos
			 f$s = NULL
			 f$m = c()
		  }
  })
  
  output$fig <- renderPlot({  
	  max.y = min(1, 1.2*max(v$p))	     
	  par(mar=c(2.5,3.9,0.5, 0.5))
	  plot(1:6, v$p, ylim=c(-max.y/5, max.y), t='h', xlab='', ylab='prob.', lwd=2, col='blue2')
	  abline(h=0)
      mu = sum(v$p*(1:6))
	  arrows(mu, -max.y/9, mu, 0, lwd=3, col='brown', angle=20)
	  text(mu, -max.y/6, expression(mu), col='brown', cex=1.6)
  })

  output$freq <- renderPlot({  
      par(mfrow=c(2,1))
	  par(mar=c(0,0,0,0))
	  plot(c(0, 9), c(0, 5), t='n', xlab='', ylab='', axes=FALSE)
      if (!is.null(f$s)) {
		x = 1; dx = 0.7; sx = 0.1
		y = 1; dy = 0.9; sy = 0.12
		s = as.numeric(f$s)
		for (i in 1:n.d) {
			rect(x,y,x+dx,y+dy, col=1+s[i])
			d = caradado(x,y,dx,dy,s[i])
			points(d$a, d$b, pch=19)
			x = x+	dx+sx
			if (i%%10==0) {
			   x = 1
			   y = y+dy+sy
			}
		}
	  }
	  par(mar=c(3,3,0,0))
      barplot(f$data, col=2:7)
  })
  
  output$medias <- renderPlot({  
      if (length(f$m)==0) {
	      return()
	  }
	  y = cumsum(f$m)
	  x = n.d*(1:length(f$m))
	  y = y/x
	  mu = sum(v$p*(1:6))
	  YLim = range(c(mu, y))
	  par(mar=c(4,4,0.8,0.1))
	  plot(x,y,t='b',xlab=tr("lnza"),ylab=tr("cumm"), ylim=YLim, bty='n')
	  abline(h=mu, col='grey', lwd=2)
  })

  output$tab <- renderUI({  
      txt = '<table style="width:100%"><tr><th>x<sub>i</sub></th><th>P(x<sub>i</sub>)</th>'
	  txt = paste0(txt, '<th>P(x<sub>i</sub>) x<sub>i</sub></th><th>P(x<sub>i</sub>) x<sub>i</sub><sup>2</sup></th></tr>')
	  for (i in 1:6) {
		 txt = paste0(txt, '<tr><td>',i,'</td><td>',round(v$p[i],4),'</td><td>',round(v$p[i]*i,4),'</td><td>',round(v$p[i]*i*i,4),'</td></tr>')
	  }
	  mu = sum(v$p*(1:6))
	  ssq = sum(v$p*(1:6)^2)
	  txt = paste0(txt, '<tr><td><em>', tr("suma"), '</em></td><td>1</td><td>',round(mu,3),'</td><td>',round(ssq,3),'</td></tr>')
	  txt = paste0(txt, '</table><br>')
	  txt = paste0(txt, '&mu; = ', round(mu, 3),'<br>')
	  txt = paste0(txt, '&sigma;<sup>2</sup> = ', round(ssq, 3),' - ',round(mu,3),'<sup>2</sup> = ',round(ssq-mu^2,3),'<br>')
	  txt = paste0(txt, '&sigma; = ', round(sqrt(ssq-mu^2),3),'<br>')
      HTML(paste('<br>', txt, '<br>'))	  
  })
  
})
