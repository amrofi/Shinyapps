.L = read.table("etiquetas mnas.txt", header=FALSE, sep='\t', quote="", row.names=1, colClasses=rep("character", 3))

badsample = function(n, p=0.5, r=c(p,p)) {
   u = runif(n)
   s = array(NA, n)
   s[1] = u[1] < p
   for (i in 2:n) {
      s[i] = ifelse(s[i-1], u[i] < r[1], u[i] < r[2])
   }
   as.numeric(s)
}

shinyServer(function(input, output) {
  
  .E <- reactiveValues(d = 1)  #active language and default

  tr <- function(stringId) {
        .L[stringId, .E$d]
  }

  observeEvent(input$eng, {    .E$d=1   })
  observeEvent(input$spa, {    .E$d=2   })
    
  output$titulo = renderText( { tr("titulo") } )
  output$txt1 = renderText( { tr("txt1") } )
  output$txt2 = renderText( { tr("txt2") } )
  output$txt3 = renderText( { tr("txt3") } )
  output$txt4 = renderText( { tr("txt4") } )
  output$ene = renderText( { tr("ene") } )
  output$propi = renderUI( { HTML(tr("propi")) } )
  output$explan = renderUI( { HTML(tr("explan")) } )
  output$simpa = renderText( { tr("simpa") } )
  output$conf = renderText( { tr("conf") } )
  output$proc = renderText( { tr("proc") } )
  output$analis = renderText( { tr("analis") } )

  M=1000
  datasetInput <- reactive({
	n=input$n
	p=input$p
	x=input$X
	co=input$Co/100
	if (as.numeric(input$method)==1) {
	    FUN=prop.test
	} else {
	    FUN=binom.test
	}
	b = max(0, 2-1/p)
	p1 = ifelse(x>0, p+x*(1-p), p+x*(p-b))
	p2 = p*(1-p1)/(1-p)
	A = array(NA, dim=c(M,n))
	CI.ok = 0
    T = array(0, c(2,2))
	for (i in 1:M) {
	  A[i,] = badsample(n, p, c(p1, p2))
	  z = FUN(sum(A[i,]), n, conf=co)
      T = T + table(factor(A[i,1:(n-1)],levels=0:1), factor(A[i,2:n],levels=0:1))
	  if (z$conf.int[1]<p & z$conf.int[2]>p) CI.ok = CI.ok+1
	}
    list(A=A, T=T, CI.ok=CI.ok, r=c(p1, p2), n=n, p=p, co=co)
  })

  output$tree <- renderPlot({
    data = datasetInput();   r = data$r;   p = data$p
	p1 = r[1]
	p2 = r[2]
	par(lwd=6, mar=c(0,0,0,0))
	plot(c(0,0.8),c(0.0,0.87), t='n', axes=FALSE,xlab='',ylab='')
	lines(c(0,1/3),c(1/2,0.7), col=rgb(1-p,1-p,0.75))
	lines(c(0,1/3),c(1/2,0.3), col=rgb(p,p,0.75))
	lines(c(0.4,2.2/3),c(0.7,0.87), col=rgb(1-p1,1-p1,0.75))
	lines(c(0.4,2.2/3),c(0.7,0.53), col=rgb(p1,p1,0.75))
	lines(c(0.4,2.2/3),c(0.3,0.47), col=rgb(1-p2,1-p2,0.75))
	lines(c(0.4,2.2/3),c(0.3,0.13), col=rgb(p2,p2,0.75))
	text(0.367, 0.7, 1, cex=1.5)
	text(0.367, 0.3, 0, cex=1.5)
	text(0.76, 0.87, 1, cex=1.5)
	text(0.76, 0.53, 0, cex=1.5)
	text(0.76, 0.47, 1, cex=1.5)
	text(0.76, 0.13, 0, cex=1.5)
	text(0.21,0.05,tr('respu0'))
	text(0.61,0.05,tr('respu1'))
  })
  
  output$prop <- renderPlot({
	par(mar=c(4,2,0.5,0.5))
    data = datasetInput();   A = data$A;   n = data$n;   p = data$p
	medias = apply(A, 1, mean)
	v = range(medias)
	w = seq(v[1], v[2], len=75)
	y = dnorm(w, p, sqrt(p*(1-p)/n))
	H = hist(medias, plot=FALSE)
	tp = max(H$density, y)
	hist(medias, main='', freq=FALSE, xlab=tr('prop'), ylab='', ylim=c(0,tp))
	lines(w,y, col=rgb(0.65,0.65,0.65), lwd=2)
  })
  
  output$tab <- renderUI({
    data = datasetInput();   T = data$T;
	N = sum(T)
	t = round(100*T/N,1)
	txt = paste0('<table style="width:100%"><tr>   <td></td><td></td><th colspan="2">', tr('res1'), '</th> </tr>')
	txt = paste0(txt, '<tr><td></td><td></td><td>1</td><td>0</td> </tr>')
	txt = paste0(txt, '<tr><th rowspan="2">', tr('res0'), '</th><td>1&nbsp;</td><td>',
	             T[2,2],'<br>',t[2,2],'%</td><td>',T[2,1],'<br>',t[2,1],'%</td> </tr>')
	txt = paste0(txt, '<tr>   <td>0&nbsp;</td><td>',T[1,2],'<br>',t[1,2],'%</td><td>',
	             T[1,1],'<br>',t[1,1],'%</td> </tr></table>')  
    HTML(paste('<br>', txt, '<br>'))
	})
	
  output$sum <- renderUI({
    data = datasetInput();   n = data$n;  p = data$p;  CI.ok = data$CI.ok;   A = data$A; co = data$co
	medias = apply(A, 1, mean)
	txt = sprintf(tr('tam'), M, n)
	txt = paste0(txt, sprintf(tr('favo'), M))
	txt = paste0(txt, sprintf(tr('prome'), round(mean(medias),3), p))
	txt = paste0(txt, sprintf(tr('desv'), round(sd(medias),3), round(sqrt(p*(1-p)/n),4)))
	txt = paste0(txt, sprintf(tr('inter'), CI.ok, round(100*CI.ok/M, 1)))
	txt = paste0(txt, sprintf(tr('nomi'), 100*co, '%.'))
    HTML(paste('<br>', txt, '<br>'))
	})
})


