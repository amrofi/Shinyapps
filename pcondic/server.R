
library(shiny)

circulo = function(cx, cy, r, ini=0, fin=2*pi) {
        n=50
	 a = seq(ini, fin, len=n)
        X = array(cx+r*cos(a), dim=n)
        Y = array(cy+r*sin(a), dim=n)
        list(x=X, y=Y)
}

curva = function(t, V) {
        R = V[1]; desp = V[2]
        sqrt(R^2 - (t-desp)^2)
}
.L = read.table("etiquetas condic.txt", header=FALSE, sep='\t', quote="", row.names=1, colClasses=rep("character", 3))

# Define a server for the Shiny app
shinyServer(function(input, output) {

  .E <- reactiveValues(d = 1)  #active language and default

  tr <- function(stringId) {
        .L[stringId, .E$d]
  }

  observeEvent(input$eng, {    .E$d=1   })
  observeEvent(input$spa, {    .E$d=2   })
    
  output$titulo = renderText( { tr("titulo") } )
  output$par1 = renderText( { tr("par1") } )
  output$par2 = renderText( { tr("par2") } )
  output$par3 = renderText( { tr("par3") } )
  output$text = renderText( { tr("text") } )
  output$scal = renderText( { tr("scal") } )
  output$cust = renderText( { tr("cust") } )
  output$centros = renderText( { tr("centros") } )
  output$radioA = renderText( { tr("radioA") } )
  output$radioB = renderText( { tr("radioB") } )
  output$condi = renderText( { tr("condi") } )
  
data = reactiveValues(x = c(3,6), Ra = 1.5, Rb = 1.5, A = 'A', B = 'B')

observeEvent(input$x, {data$x = input$x} )
observeEvent(input$x.2, {
	T = input$x.2
	T = strsplit(T, '[ ]+')
	Z = as.numeric(unlist(T))
	if (!anyNA(Z)) {
		ok = length(Z)==2
		if (ok) ok = Z[1] >= 0 & Z[1] < Z[2] & Z[2] <= 10
		if (ok) data$x = Z
	}
 } )
 
observeEvent(input$Ra, {data$Ra = input$Ra} )
observeEvent(input$Ra.2, {
	T = input$Ra.2
	T = strsplit(T, '[ ]+')
	Z = as.numeric(unlist(T))[1]
	if (!is.na(Z)) {
		ok = Z >= 0.1 & Z <= 8
		if (ok) data$Ra = Z
	}
 } )

observeEvent(input$Rb, {data$Rb = input$Rb} )
observeEvent(input$Rb.2, {
	T = input$Rb.2
	T = strsplit(T, '[ ]+')
	Z = as.numeric(unlist(T))[1]
	if (!is.na(Z)) {
		ok = Z >= 0.1 & Z <= 8
		if (ok) data$Rb = Z
	}
 } )
 
observeEvent(input$Abis, {data$A = substr(input$Abis, 1, min(20, nchar(input$Abis)))} )
observeEvent(input$Bbis, {data$B = substr(input$Bbis, 1, min(20, nchar(input$Bbis)))} )
 
proceso <- reactive({
	Y=0  # mejor no tocarlo...
	xA = data$x[1]
	xB = data$x[2]
	Ra = data$Ra
	Rb = data$Rb

	RangX = c(min(xA-Ra, xB-Rb), max(xA+Ra, xB+Rb))
	RangY = diff(RangX)*c(-0.5,0.5)
	S = diff(RangX)*diff(RangY)
	if (xA+Ra > xB+Rb) {
	   C = pi*Rb^2
	} else if (xA-Ra > xB-Rb) {
	   C = pi*Ra^2
	} else if (xA+Ra < xB-Rb) {
	   C = 0
	} else {
	  x = (Ra^2-Rb^2-xA^2+xB^2)/(xB-xA)/2
	  P1 = 2*integrate(curva, lower=x, upper=xA+Ra, V=c(Ra, xA))$value
	  P2 = 2*integrate(curva, lower=xB-Rb, upper=x, V=c(Rb, xB))$value
	  C = P1+P2
	}
	pros = c(pi*Ra^2/S, pi*Rb^2/S, C/S, C/(pi*Rb^2), C/(pi*Ra^2))
	list(pros=pros, x=c(xA, xB), R=c(Ra, Rb), ra=c(RangX, RangY), Y=Y)
})

output$texto = renderUI({
	wellPanel(
		textInput('x.2', tr('centros'), value=paste(data$x, collapse=' ')),
		textInput('Ra.2', tr('radioA'), value=data$Ra),
		textInput('Rb.2', tr('radioB'), value=data$Rb)
	)
})

output$barra = renderUI({
	wellPanel(
		sliderInput('x', tr('centros'), value=data$x, min=0, max=10, step=0.025),
		sliderInput("Ra", tr("radioA"), min = 0.1, max = 8, value = data$Ra, step= 0.02),
		sliderInput("Rb", tr("radioB"), min = 0.1, max = 8, value = data$Rb, step= 0.02)
	)
})

output$person = renderUI({
	wellPanel(
		tr('labels'),
		textInput('Abis', "", value=data$A, placeholder='A'),
		textInput('Bbis', "", value=data$B, placeholder='B')
	)
})

output$fig <- renderPlot({
    Z = proceso(); xA=Z$x[1]; xB=Z$x[2]; Ra=Z$R[1]; Rb=Z$R[2]; Y=Z$Y; RangX=Z$ra[1:2]; RangY=Z$ra[3:4]
	colA = NA
	colB = NA
	ColInt = 'pink'
	L1 = circulo(xA,Y,Ra)
	L2 = circulo(xB,Y,Rb)
       par(mar=c(0.1, 0.1, 0.1, 0.1))
	plot(RangX, RangY, t='n', axes=FALSE, xlab='', ylab='')
	rect(RangX[1], RangY[1], RangX[2], RangY[2], border='grey')
	if (xA+Ra > xB+Rb) {
	   colB = ColInt
	} else if (xA-Ra > xB-Rb) {
	   colA = ColInt
	} else if (xA+Ra < xB-Rb) {
	   y=0
	} else {
	  x = (Ra^2-Rb^2-xA^2+xB^2)/(xB-xA)/2
	  y = sqrt(Ra^2-(x-xA)^2)
	  alfB = atan2(y,x-xB)
	  alfA = atan2(y,x-xA)
	  arcoA = circulo(xA, Y, Ra, ini=-alfA, fin=alfA)
	  polygon(arcoA$x, arcoA$y, border=NA, col=ColInt)
	  arcoB = circulo(xB, Y, Rb, ini=alfB, fin=2*pi-alfB)
	  polygon(arcoB$x, arcoB$y, border=NA, col=ColInt)
	}
	polygon(L1$x, L1$y, col=colA)
	polygon(L2$x, L2$y, col=colB)
})

output$txt <- renderUI({ 
    Z = proceso(); p.A = Z$pros[1]; p.B = Z$pros[2]; p.AyB = Z$pros[3]; p.AB = Z$pros[4]; p.BA = Z$pros[5]
    HTML(paste("P(", data$A, ") =", round(p.A,3), "<p />P(", data$B, ") =", round(p.B,3), "<p />P(", data$A, "&cap; ", data$B, ") =", round(p.AyB,3), "<p />P(", data$A, " | ", data$B, ") =", round(p.AB,3), "<p />P(", data$B, " | ", data$A, ") =", round(p.BA,3)))
})

output$graf <- renderPlot({
    Z = proceso(); p.A = Z$pros[1]; p.B = Z$pros[2]; p.AyB = Z$pros[3]; p.AB = Z$pros[4]; p.BA = Z$pros[5]
    if (input$cond==0) {
	#nombres=c("P(B)", "P(B|A)")
	lab = c(sprintf("P(%s)", data$B), sprintf("P(%s | %s)", data$B, data$A))
	p1 = p.B
	p2 = p.BA
    } else {
	#nombres=c("P(A)", "P(A|B)")
	lab = c(sprintf("P(%s)", data$A), sprintf("P(%s | %s)", data$A, data$B))
	p1 = p.A
	p2 = p.AB
    }
	nombres=c('','')
    par(mar=c(2.1, 0.9, 0.1, 1.1))
    barplot(c(p1, p2), space=0.6, names=nombres, horiz=TRUE, las=1, xlim=0:1, ylim=c(0,3),
            col=c(rgb(0,0,1), rgb(0,0,1,0.3)))
	text(c(0.0,0.0), c(0.3, 1.9), lab, pos=4, cex=1.4)
}, height=200)

})

