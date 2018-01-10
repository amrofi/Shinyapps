.L = read.table("etiquetas regre.txt", header=FALSE, sep='\t', quote="", row.names=1, colClasses=rep("character", 3))

recalc = function(input, data) {
     be1 = input$bet1
     be0 = input$bet0
     sg = input$desv
	 rang.X = input$ranX
	 data$resp = array(rnorm(K*N), dim=c(K,N))
	 data$Xs = rang.X[1] + c(0,0.2,0.5,0.8,1)*(diff(rang.X))
	 data$pams = list(bet1=be1, bet0=be0, sg=sg)
	 data$resp = data$resp*sg + be0 + be1*matrix(rep(data$Xs[2:4],N), ncol=N)
}

signo = function(m){
   if (m<0) {
      ' '
   } else {
      ' + '
   }
}
N = 5
K = 3

library(shiny)
library(rgl)
library(htmlwidgets)
library(jsonlite)

shinyServer(function(input, output, session) {

  .E <- reactiveValues(d = 1)  #active language and default
  SEL = 31 # SEL = 1:5
  makeReactiveBinding("SEL")  

  tr <- function(stringId) {
        .L[stringId, .E$d]
  }
  observeEvent(input$eng, {    .E$d=1   })
  observeEvent(input$spa, {    .E$d=2   })

  output$titulo = renderText({ tr('titulo') })
  output$txt1 = renderText({ tr('txt1') })
  output$txt2 = renderText({ tr('txt2') })
  output$txt3 = renderUI({ HTML(tr('txt3')) })
  output$pend = renderUI({ HTML(tr('pend')) })
  output$indep = renderUI({ HTML(tr('indep')) })
  output$desv = renderUI({ HTML(tr('desv')) })
  output$ensenar = renderUI({
    grupo = c()
	if (bitwAnd(SEL,1)) grupo = c(grupo, tr('recta'))  # (1 %in% SEL)
	if (bitwAnd(SEL,2)) grupo = c(grupo, tr('condi'))  # (2 %in% SEL)
	if (bitwAnd(SEL,4)) grupo = c(grupo, tr('obse'))   # (3 %in% SEL)
	if (bitwAnd(SEL,8)) grupo = c(grupo, tr('ajus'))   # (4 %in% SEL)
	if (bitwAnd(SEL,16)) grupo = c(grupo, tr('tabla')) # (5 %in% SEL)
    checkboxGroupInput("chk", tr('mostrar'), c(tr('recta'), tr('condi'), tr('obse'), tr('ajus'), tr('tabla')), 
	    selected = grupo)
  })
  output$rango = renderText({ tr('rango') })
  output$ang = renderText({ tr('ang') })
  output$ang2 = renderText({ tr('ang2') })

  observeEvent(c(input$chk, input$bet1, input$bet0, input$desv, input$ranX), {
    # tell our rglWidgetAux to query the plot3d for its par3d

    session$sendInputMessage("ctrlplot3d",list("cmd"="getpar3d","rglwidgetId"="fig"))
  }, ignoreNULL = FALSE)

  MatUs = reactive({
    jsonpar3d <- input$ctrlplot3d
    if (jsonlite::validate(jsonpar3d)){
      par3dout <- fromJSON(jsonpar3d)
      umat <- matrix(unlist(par3dout$userMatrix),4,4) # make list into matrix
    }
    return(list(umat=umat, zoom=par3dout$zoom))
  })
  
  observeEvent(input$chk, { 
		x = input$chk
		if (is.null(x)) {SEL <<- 31; return() }
	    Z = 0
		if (tr('recta') %in% x) Z = bitwOr(Z, 1)
		if (tr('condi') %in% x) Z = bitwOr(Z, 2)
		if (tr('obse') %in% x) Z = bitwOr(Z, 4)
		if (tr('ajus') %in% x) Z = bitwOr(Z, 8)
		if (tr('tabla') %in% x) Z = bitwOr(Z, 16)
		SEL <<- Z
	}, ignoreNULL = FALSE)

  data = reactiveValues(resp=NA, Xs=NA, pams=NA)

  observeEvent(input$bet1, {    recalc(input, data)   })
  observeEvent(input$bet0, {    recalc(input, data)   })
  observeEvent(input$desv, {    recalc(input, data)   })
  observeEvent(input$ranX, {    recalc(input, data)   })

  scenegen <- reactive({
    try(rgl.close(), silent = TRUE)
	if (!is.null(input$ctrlplot3d)) {
	    pars <- MatUs()
		par3d(userMatrix = pars$umat, zoom = pars$zoom)
	}
	isolate({
		if (anyNA(data$resp)) return()
		rang.X = data$Xs[c(1,5)]
		xx = data$Xs[2:4]
		recta = data$pams$bet0 + data$pams$bet1*rang.X
		sg = data$pams$sg
		mod = lm(array(data$resp) ~ rep(xx,N))
		rang.Y = range(c(recta, data$resp))
		W = expand.grid(rang.X, rang.Y)
		P = array(0, dim=c(2,2))
		xtr=qnorm(c(0.005, 0.995), me=0, sd=sg)
		y = seq(xtr[1], xtr[2], len=30)
		kof = 0.7/dnorm(0,0,sg)
		d = dnorm(y, me=0, sd=sg)*kof
		
		plot3d(W$Var1, W$Var2, P, xlab='X', ylab='Y', zlab='', zlim=c(0,1), t='n', axes=FALSE)
		axes3d(c('x', 'y'))
		grid3d(side=c('x+','y+','z'), col='grey')

		if (bitwAnd(SEL,1)) {
			lines3d(rang.X, recta, c(0,0), lwd=2, col='red')
		}

		for (i in 1:K) {
		   x = xx[i]
		   yh = data$pams$bet0 + data$pams$bet1*x
		   if (bitwAnd(SEL,2)) {
			   lines3d(rep(x,30), yh+y, d, col='orangered')
			   polygon3d(rep(x,30), yh+y, d, coords=2:3, col='yellow', alpha=0.25)
			   lines3d(c(x,x), c(1,1)*yh, c(0,dnorm(yh, yh, sg)*kof), col='orange')
		   }
		   if (bitwAnd(SEL,4)) {
			   points3d(rep(x,N), data$resp[i,], rep(0,N), col='forestgreen', size=8)
			   #pch3d(rep(x,N), data$resp[i,], rep(0,N), pch=19, col='forestgreen', cex=2)
		   }
		}
		if (bitwAnd(SEL,8)) {
			lines3d(rang.X, mod$coef[1]+mod$coef[2]*rang.X, c(0,0), lwd=2, col='green', lty=2)
		}
	 })
     scene1 <- scene3d()
     return(scene1)
  })
  
  output$fig <- renderRglwidget({ 
     rglwidget(scenegen()) 
  })
  
  output$ecuaciones = renderUI({
    if (anyNA(data$resp)) return()
	mod = lm(array(data$resp) ~ rep(data$Xs[2:4],N))
	css = "<style>table, td { border: 0px; border-collapse: collapse; } td {text-align:left; padding:2px}</style>"
	stri = sprintf(tr('ecua'), round(mod$coef[1],3), signo(mod$coef[2]), round(mod$coef[2],3))
	txt = paste('<table><tr>', stri, '</tr><tr>')
	stri = sprintf(tr('espe'), data$pams$bet0, signo(data$pams$bet1), data$pams$bet1)
	HTML(paste(css, '<em>', txt, stri, '</tr></table></em>'))
  })
  
  output$losdatos = renderTable({
	if (anyNA(data$resp) | bitwAnd(SEL,16)==0) return()
	xx = round(rep(data$Xs[2:4], each=N), 2)
	yy = round(as.vector(t(data$resp)), 2)
	ta = cbind(xx,yy)
	colnames(ta) = c('X', 'Y')
	ta
  })
  
  output$sel = renderText( SEL )
  
})  
