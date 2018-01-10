library(shiny)

.L = read.table("etiquetas polling.txt", header=FALSE, sep='\t', quote="", row.names=1, colClasses=rep("character", 3))

# Define server logic for slider examples
shinyServer(function(input, output, session) {

  SEL=1
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
  output$back = renderText( { tr("back") } ) 
  output$Ene = renderText( { tr("Ene") } ) 
  output$news = renderText( { tr("news") } ) 
  output$txt1 = renderUI( { HTML(tr("txt1")) } ) 
  output$txt2 = renderUI( { HTML(tr("txt2")) } ) 
  output$txt3 = renderText( { tr("txt3") } ) 
  output$txt4 = renderText( { tr("txt4") } ) 
  output$cab2 = renderUI({ HTML(tr('cab2')) })
  output$cab3 = renderUI({ HTML(tr('cab3')) })
  output$cab4 = renderUI({ HTML(tr('cab4')) })
  output$ejem1 = renderText( { tr("ejem1") } ) 
  output$ejem2 = renderText( { tr("ejem2") } ) 
  output$ejem3 = renderText( { tr("ejem3") } ) 
  output$ejem4 = renderText( { tr("ejem4") } ) 
  output$tipochart = renderUI( {
	radioButtons("Info", tr('queinfo'), c(tr('info1'), tr('info2')),
	             selected=ifelse(SEL==1, tr('info1'), tr('info2')), width='100%')
  })
  observeEvent(input$Info, {    
     t = input$Info
     SEL <<- ifelse(t==tr("info1"), 1, 2)
	})
  
  pila = list()
  makeReactiveBinding("pila")  

  load('pop.Rdata')
  # load('lista.Rdata')
  I = dim(pop)[1]
  A = range(pop$x, na.rm=TRUE)  
  B = range(pop$y, na.rm=TRUE) 
  pila = list(xx = A, yy = B, link = NULL)
	R = 51
	R2 = R %/% 2 + 1
	mapa = pop[1:I,]
	xl = range(mapa$x)*1.001
	yl = range(mapa$y)*1.001
	xc = seq(xl[1], xl[2], len=R)
	yc = seq(yl[1], yl[2], len=R)
	xc2 = seq(xl[1], xl[2], len=R2)
	yc2 = seq(yl[1], yl[2], len=R2)
	mapa$X = cut(mapa$x, xc)
	mapa$Y = cut(mapa$y, yc)
	tbl = table(mapa$X, mapa$Y)         ## density plot can be prepared at start
	TB = matrix(array(tbl), nrow=R-1)
  true.prop = sum(pop$v, na.rm=TRUE)/I

  Rsu = data.frame(n=NULL, p=NULL, IC.l=NULL, IC.r=NULL, ok=NULL, system=NULL)

  fill_resul = function(sys) {
		mst = pop[PTS,]
		x = sum(mst$v)
		test = prop.test(x = x, n = length(PTS))
		new = data.frame(n = length(PTS), p = test$estimate,
			IC.l = test$conf.int[1], IC.r = test$conf.int[2],
			ok = true.prop > test$conf.int[1] & true.prop < test$conf.int[2], system = sys)
		Rsu <<- rbind(Rsu, new)
		sam.finish <<-TRUE
	}
  
  observeEvent( input$atras, {  # Going back (clicking in the topleft button)
		if (is.null(pila)) return()
		if (is.null(pila$link)) return()
		pila <<- pila$link
	}
  )
  observeEvent( input$newSam, {  # reset survey
		sam.finish <<- FALSE
		PTS <<- c()
		pila <<- list(xx = A, yy = B, link = NULL)
  })

  output$controls = renderUI({
	if (is.null(PTS)) {
		splitLayout(
			sliderInput('ene', tr("Ene"), min=25, max=250, step=25, value=50),
			span(''),
			actionButton('auto', tr('Autom')),
			cellWidths = c('30%', '5%', '65%')
		)
	} else {
		splitLayout(
			actionButton('newSam', tr('news')),
			if (!sam.finish) {
				XL = pila$xx
				YL = pila$yy
				aux = subset(pop, x > XL[1] & x < XL[2] & y > YL[1] & y < YL[2])
				tam = dim(aux)[1]
				nlim = 10 + input$ene*0.1
				if (tam > 0 & tam <= nlim) {
				  span(
					actionButton('tous', tr('Tous')),
					HTML(tr('modera'))
				)}
			},
			cellWidths = c('30%', '70%'))
	}
  })
  
  clickable = FALSE            # individuals are (de)selectable because there are less than 100 in the chart
  makeReactiveBinding('clickable')
  sam.finish = FALSE
  makeReactiveBinding('sam.finish')
  PTS = c()
  makeReactiveBinding("PTS")
  
  observeEvent( input$clic, { 
		if (!clickable | sam.finish) return()
		pt = input$clic
		if (is.null(pt)) return()
		scal = c(diff(pila$xx), diff(pila$yy))/150
		aux = subset(pop, abs(x-pt$x)< scal[1] & abs(y-pt$y)< scal[2])
		rw = rownames(aux)
		en = is.element(rw, PTS)
		PTS <<- union(PTS, rw[which(en==FALSE)])
		PTS <<- setdiff(PTS, rw[which(en==TRUE)])
		if (length(PTS) >= input$ene) fill_resul(FALSE)
	}
  )
  observeEvent( input$tous, { 
	XL = pila$xx
	YL = pila$yy
	aux = subset(pop, x > XL[1] & x < XL[2] & y > YL[1] & y < YL[2])
	rw = rownames(aux)
	PTS <<- union(PTS, rw)
	if (length(PTS) >= input$ene) fill_resul(FALSE)
  })
  
  observeEvent(input$zoom, {
    brush <- input$zoom
    if (!is.null(brush)) {
      rangex <- c(brush$xmin, brush$xmax)
      rangey <- c(brush$ymin, brush$ymax)
      pila <<- list(xx = rangex, yy = rangey, link = pila)
    } 
  })
  
  observeEvent( input$auto, { 
	PTS <<- sample(1:I, input$ene)
	fill_resul(TRUE)
  } )
  
  output$count = renderText( { 
	XL = pila$xx
	YL = pila$yy
	aux = subset(pop, x > XL[1] & x < XL[2] & y > YL[1] & y < YL[2])
	tam = dim(aux)[1]
	c3 = ifelse(sam.finish, tr('cont3'), '')
      sprintf(paste(tr("contar"), '; ', tr('cont2'), '. ', c3, sep=''), tam, length(PTS))
  } )
  
  output$map = renderPlot( {    # Individuals
	XL = pila$xx
	YL = pila$yy
	aux = subset(pop, x > XL[1] & x < XL[2] & y > YL[1] & y < YL[2])
	tam = dim(aux)[1]
	if (tam==0) {
	   par(mar=c(0.1,0.1,0.1,0.1))
	   plot(NULL, xlim=pila$xx, ylim=pila$yy, xlab='', ylab='', axes=FALSE)
	   box(which='outer', col='coral2')
	   return()
	}
	cara = ifelse(tam < 100, 21, '.')
	clickable <<- tam < 100
	sz = ifelse(tam < 300, 2, 1)
	par(mar=c(0.1,0.1,0.1,0.1))
	plot(aux$x, aux$y, pch=cara, cex=sz, xlab='', ylab='', axes=FALSE)
	intsct = intersect(rownames(aux), PTS)
	if (!is.null(intsct) & clickable) points(aux[intsct, 'x'], aux[intsct, 'y'], pch=19, cex=sz)
	box(which='outer', col='coral2')
  })  

  output$escala = renderPlot( {     # other maps
	 if (SEL == 1) {						# density
		par(mar=c(0.1,0.1,0.1,0.1))
		image(TB, col=gray(seq(1, 0, len=12)), axes=FALSE)
		box(which='outer', col='coral')

		if (is.null(pila)) return()
		if (is.null(pila$link)) return()
		U = ( pila$xx - A[1] )/diff(A)
		W = ( pila$yy - B[1] )/diff(B)
		rect(U[1], W[1], U[2], W[2], border='blueviolet', lwd=2)
	 } else {								# counts
		if (is.null(PTS)) return()
		enq = pop[PTS, ]
		enq$X = cut(enq$x, xc2)
		enq$Y = cut(enq$y, yc2)
		tb = table(enq$Y, enq$X)
		NZ = which(tb!=0)
		xZ = ((NZ-1)%/%(R2-1)+1/2)/(R2-1)
		yZ = ((NZ-1)%%(R2-1)+1/2)/(R2-1)
		if (sam.finish) {
			tb2 = table(enq$Y, enq$X, enq$v)
			labe = paste(tb2[,,2], tb, sep='/')
		} else {
			labe = tb
		}
		par(mar=c(0.1,0.1,0.1,0.1))
		plot(NULL, xlim=0:1, ylim=0:1, axes=FALSE, xlab='', ylab='')
		text(xZ, yZ, labe[NZ])
		if (sam.finish) {
			legend('bottom', legend=tr('lgnd'), box.col='white')
		} else {
			legend('bottom', legend=tr('lgnd0'), box.col='white')
		}
		box(which='outer', col='coral')
	 }
  })
  
  output$CI = renderUI({
	if (!sam.finish) return()
	i = dim(Rsu)[1]
	if (i==0) return()
	estim = Rsu[i, 'p']
	icon = Rsu[i, c('IC.l', 'IC.r')]
	TXT = sprintf( paste(tr('estima'), '<br>', sep=''), round(estim, 3) )
	TXT = sprintf( paste(TXT, tr('confin'), '<br>', sep=''), 
			round(icon[1], 3), round(icon[2],3) )
	conc = ifelse(Rsu[i, 'ok'], tr('bien'), tr('mal')) 
	TXT = paste( TXT, conc)
	HTML(TXT)
  })

  output$resu = renderTable( { sam.finish; Rsu } )
  output$summa = renderPlot( {     # the CI
	sam.finish;
	i = dim(Rsu)[1]
	K = max(i, 5)
	if (i==0) return()
	par(mar=c(4.5,0.1,0.1,0.1))
	plot(NULL, xlim=c(-0.6,1.15), ylim=c(0.5,K+1), axes=FALSE, xlab='', ylab='')
	text(-0.55, K+1, tr('p'), cex=1.35)
	text(-0.35, K+1, tr('n'), cex=1.35)
	text(-0.15, K+1, tr('IC'), cex=1.35)
	for (j in 1:i) {
		text(-0.55, K-j+1, round(Rsu[j, 'p'], 2))
		text(-0.35, K-j+1, Rsu[j, 'n'])
		text(-0.15, K-j+1, sprintf('[%s; %s]', round(Rsu[j, 'IC.l'], 2), round(Rsu[j, 'IC.r'], 2)))
	}
	bien = ifelse(Rsu[,'ok'], 'green2', 'brown')
	tiplin = ifelse(Rsu[,'system'], 1, 2)
	points(Rsu[,'p'], K-1:i+1, pch=23, cex=2, bg=bien)
	segments(Rsu[, 'IC.l'], K-1:i+1, Rsu[, 'IC.r'], K-1:i+1, lwd=2, col=bien, lty=tiplin)
	axis(1, at=seq(0,1,by=0.2))
	mtext(text=tr('subtit'), side=1, at=0.5, line=3)
	legend("topright", legend=c(tr('manu'), tr('atoma'), tr('good'), tr('bad')), col=c('black', 'black', 'green2', 'brown'), lty=c(2,1,1,1), lwd=2)
  })

  output$explic = renderUI({
     txt = paste('<h4>', tr('guia'), '</h4>', tr('intro'), '<br>')
	 txt = paste(txt, '<ul> <li>', tr('rombo'), '</li>')
	 txt = paste(txt, '<li>', tr('linea'), '</li>')
	 txt = paste(txt, '<li>', tr('lin.tip'), '</li>')
	 txt = paste(txt, '<li>', tr('lin.col'), '</li>')
	 txt = paste(txt, '</ul>')
	 HTML(txt)
  })
 })
