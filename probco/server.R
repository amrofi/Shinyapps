library(shiny)


.L = read.table("etiquetas probco.txt", header=FALSE, sep='\t', quote="", row.names=1, colClasses=rep("character", 3))

# Define a server for the Shiny app
shinyServer(function(input, output) {

  .E <- reactiveValues(d = 1)  #active language and default

  tr <- function(stringId) {
        .L[stringId, .E$d]
  }

  observeEvent(input$eng, {    .E$d=1   })
  observeEvent(input$spa, {    .E$d=2   })
    
  output$titulo = renderText( { tr("titulo") } )
  output$dim = renderText( { tr("dim") } )
  output$play = renderText( { tr("play") } )
  output$intro1 = renderText( { tr("intro1") } )
  output$intro2 = renderText( { tr("intro2") } )
  output$intro3 = renderText( { tr("intro3") } )
  output$intro4 = renderText( { tr("intro4") } )
  output$follon = renderText( { tr("follon") } )
  
vals = reactiveValues(shapeVal = c(0,1), colVal = c(0,1), porVal = 0)
  
  output$shapes = renderUI( {
	checkboxGroupInput('shape', tr('show'), choiceNames=c(tr('cua'), tr('tri')), choiceValues=0:1, inline=TRUE, sel=vals$shapeVal)
  })
  output$colors = renderUI( {
	checkboxGroupInput('color', "", choiceNames=c(tr('gold'), tr('blue')), choiceValues=0:1, inline=TRUE, sel=vals$colVal)
  })
  output$condi = renderUI( {
   radioButtons("por", tr('segun'), choiceNames=c(tr('forma'), tr('color')), choiceValues=0:1, inline=TRUE, sel=vals$porVal)
  })
  
data = reactiveValues(n=20, P = c(0.5, 0.5, 0.5, # P(cuadrado), P(gold | cuad), P(gold | tri)
							0.5, 0.5, 0.5),  # P(gold), P(cuad | gold), P(cuad | blue)
							Q = rep(0.25, 4))

Alias = reactiveValues(cua='man', tri='woman', gol='non-smoker', blu='smoker')

proceso = reactive({
	#p = c(data$P[1]*data$P[2], (1-data$P[1])*data$P[3], data$P[1]*(1-data$P[2]), (1-data$P[1])*(1-data$P[3]))
	p = data$Q
	N = data$n*data$n
	n = round(N*p)
	Y = -1:1
	W = expand.grid(Y,Y,Y,Y)
	V = t(n + t(W))
	neg = which(V<0) %% dim(V)[1]
	if (length(neg)>0) V = V[-neg,]
	su = apply(V,1,sum)
	U = V[su==N,]
	R = t(t(U/N)-p)
	X = apply(R, 1, var)
	mi = which.min(X); if (length(mi)>1) mi = mi[1]
	ms = U[mi,1:4] 
	u = sample(c(rep(0, ms[1]),rep(1, ms[2]),rep(2, ms[3]),rep(3, ms[4])))
	data$v = u %% 2
	data$w = u %/% 2
	list(n = data$n, form = data$v, col = data$w)
})

observeEvent(input$ene, {	data$n = input$ene	})
observeEvent(input$shape, { vals$shapeVal = input$shape })
observeEvent(input$color, { vals$colVal = input$color })
observeEvent(input$por, { vals$porVal = input$por })

observeEvent(input$p1, { g = as.numeric(input$p1); if (g<=0 | g>=1) return(); data$P[1] = g; data$P[4:6] = updatebayes(data$P[1:3]); data$Q = conjunta(data$P) })
observeEvent(input$p2, { g = as.numeric(input$p2); if (g<=0 | g>=1) return(); data$P[2] = g; data$P[4:6] = updatebayes(data$P[1:3]); data$Q = conjunta(data$P) })
observeEvent(input$p3, { g = as.numeric(input$p3); if (g<=0 | g>=1) return(); data$P[3] = g; data$P[4:6] = updatebayes(data$P[1:3]); data$Q = conjunta(data$P) })
observeEvent(input$p4, { g = as.numeric(input$p4); if (g<=0 | g>=1) return(); data$P[4] = g; data$P[1:3] = updatebayes(data$P[4:6]); data$Q = conjunta(data$P) })
observeEvent(input$p5, { g = as.numeric(input$p5); if (g<=0 | g>=1) return(); data$P[5] = g; data$P[1:3] = updatebayes(data$P[4:6]); data$Q = conjunta(data$P) })
observeEvent(input$p6, { g = as.numeric(input$p6); if (g<=0 | g>=1) return(); data$P[6] = g; data$P[1:3] = updatebayes(data$P[4:6]); data$Q = conjunta(data$P) })

updatebayes = function(x) {
	r1 = x[2]*x[1] + x[3]*(1-x[1])
	r2 = x[2]*x[1]/r1  
	r3 = x[1]*(1-x[2])/(1-r1)  
	c(r1, r2, r3)
}

conjunta = function(P) {
	o = which(!is.finite(P))
	if (length(o)==0) return(c(P[1]*P[2], (1-P[1])*P[3], P[1]*(1-P[2]), (1-P[1])*(1-P[3])))
	if (o == 5) return(c(0, 0, P[1]*(1-P[2]), (1-P[1])*(1-P[3])))  # P(sq) = 0
	if (o == 6) return(c(P[1]*P[2], (1-P[1])*P[3], 0, 0))  # P(tr) = 0
	if (o == 2) return(c(0, (1-P[1])*P[3], 0, (1-P[1])*(1-P[3])))  # P(G) = 0
	if (o == 3) return(c(P[1]*P[2], 0, P[1]*(1-P[2]), 0))  # P(B) = 0
}

ordenar = function(v0, w0, por) {
	z0 = 2*v0+w0
	v = factor(v0, levels=0:1); w = factor(w0, levels=0:1)
	T = table(v, w)
	if (por == '0') {
		sum1 = apply(T, 1, sum)
		P = sum1/sum(sum1)
		Pc = diag(1/sum1) %*% T
		x = t(matrix(c(0, 0, Pc[,1]), ncol=2, byrow=TRUE))
		y = c(1, P[2])
		X = array(NA, dim=length(v0))
		Y = array(NA, dim=length(v0))
		z = 0
		for (i in 1:2) {
		  for (j in 1:2) {
			n = T[i,j]
			if (n>0) {
			  E = Pc[i,j]/P[i]
			  A = 1:n
			  B = ceiling(n/A)
			  e = which.min((A/B-E)^2)
			  if (length(e) > 1) e = e[1]
			  F = c(A[e], B[e])
			  X[z0 == z] = x[i,j] + Pc[i,j]*(0.5+(A-1) %% F[1])/F[1]
			  Y[z0 == z] = y[i] - P[i]*(0.5+(A-1) %/% F[1])/F[2]
			}
			z = z+1
		} }
	} else {
		sum1 = apply(T, 2, sum)
		P = sum1/sum(sum1)
		Pc = T %*% diag(1/sum1)
		x = c(0, P[1])
		y = matrix(c(1, 1, Pc[2,]), ncol=2, byrow=TRUE)
		X = array(NA, dim=length(v0))
		Y = array(NA, dim=length(v0))
		z = 0
		for (i in 1:2) {
		  for (j in 1:2) {
			n = T[i,j]
			if (n>0) {
			  E = P[j]/Pc[i,j]
			  A = 1:n
			  B = ceiling(n/A)
			  e = which.min((A/B-E)^2)
			  if (length(e) > 1) e = e[1]
			  F = c(A[e], B[e])
			  X[z0 == z] = x[j] + P[j]*(0.5+(A-1) %% F[1])/F[1]
			  Y[z0 == z] = y[i,j] - Pc[i,j]*(0.5+(A-1) %/% F[1])/F[2]
			}
			z = z+1
		} }
	}
	list(x=X, y=Y)
}

observeEvent(input$al.cua, {Alias$cua = substr(input$al.cua, 1, min(20, nchar(input$al.cua)))} )
observeEvent(input$al.tri, {Alias$tri = substr(input$al.tri, 1, min(20, nchar(input$al.tri)))} )
observeEvent(input$al.gold, {Alias$gol = substr(input$al.gold, 1, min(20, nchar(input$al.gold)))} )
observeEvent(input$al.blue, {Alias$blu = substr(input$al.blue, 1, min(20, nchar(input$al.blue)))} )

output$alias = renderUI({
  tagList(
	tags$head(tags$style(".X { padding-left: 6px ; margin-left: 10px; margin-bottom: 5px; font-size: 14px}")),
	tags$table(tags$tr(
		tags$td(tr('CUA'), class="X"), tags$td(tags$input(id = "al.cua", type="text", value=Alias$cua, class="X"))),
		tags$tr(
		tags$td(tr('TRI'), class="X"), tags$td(tags$input(id = "al.tri", type="text", value=Alias$tri, class="X"))),
		tags$tr(
		tags$td(tr('GOLD'), class="X"), tags$td(tags$input(id = "al.gold", type="text", value=Alias$gol, class="X"))),
		tags$tr(
		tags$td(tr('BLUE'), class="X"), tags$td(tags$input(id = "al.blue", type="text", value=Alias$blu, class="X"))), class="X"
	)
  )
})

output$percent = renderUI({
	if (is.null(input$por)) return()
	if (input$por == "0") {
	  div(
		numericInput("p1", tr('P_C'), round(data$P[1],6), 0, 1, step=0.01, width='80%'),
		numericInput("p2", tr('PD_C'), round(data$P[2], 6), 0, 1, step=0.01, width='60%'),
		numericInput("p3", tr('PD_T'), round(data$P[3], 6), 0, 1, step=0.01, width='60%')
	  )
	} else {
	  div(
		numericInput("p4", tr('P_D'), round(data$P[4], 6), 0, 1, step=0.01, width='80%'),
		numericInput("p5", tr('PC_D'), round(data$P[5],6), 0, 1, step=0.01, width='60%'),
		numericInput("p6", tr('PC_A'), round(data$P[6],6), 0, 1, step=0.01, width='60%') 
	  )
	}
})

output$fig <- renderPlot({
    Z = proceso(); n = Z$n; v0 = Z$form; w0 = Z$col

	if (input$mess) {
		coor = ordenar(v0, w0, input$por)
		coor.x = coor$x
		coor.y = coor$y
	} else {
		x = seq(0.5/n, 1-0.5/n, len=n)
		y = x
		z = expand.grid(x,y)
		coor.x = z$Var1 
		coor.y = z$Var2
	}
	v = array(NA, dim=n*n)
	w = array(NA, dim=n*n)
	v[v0==0] = 15
	v[v0==1] = 17
	w[w0==0] = 'gold'
	w[w0==1] = 'lightblue'
	
	par(mar=c(1,0,0,0))
	plot(NULL, xlim=c(0, 1), ylim=c(0, 1), axes=FALSE, xlab='', ylab='')
	sel = which(v0 %in% input$shape & w0 %in% input$color)
	points(coor.x[sel], coor.y[sel], pch=v[sel], cex=40/input$ene, col=w[sel])
})

output$dat <- renderUI({ 
    Z = proceso(); n = Z$n; v0 = Z$form; w0 = Z$col
    css = "<style>table#TA { border: 1px solid black; border-collapse: collapse; } .CE {border: 1px solid black; border-collapse: collapse; text-align:center; padding:5px; font-size: 135%}</style>"
	clr = array('#D6D773', dim=9)
    if (length(input$shape)==2 & length(input$color)==2) {
		clr[1:9] = 'black'
	} else if (length(input$shape)==2 & length(input$color)==1) {
		i = as.numeric(input$color)
		clr[c(1,4,7)+i] = 'black'
    } else if (length(input$shape)==1 & length(input$color)==2) {
		i = ifelse(as.numeric(input$shape)==0, 0, 3)
		clr[c(1,2,3)+i] = 'black'
    } else if (length(input$shape)==1 & length(input$color)==1) {
        i = as.numeric(input$shape)*3 + as.numeric(input$color)+1
        clr[i] = 'black'
    }

	v = factor(v0, levels=0:1); w = factor(w0, levels=0:1)
	T = table(v, w)
	tab = paste("<table id='TA'><tr><td /><td class='CE' style='background-color:gold;'>", Alias$gol, "</td>",
				"<td class='CE' style='background-color:lightblue;'>", Alias$blu, "</td><td /></tr>")
	tab = paste(tab, "<tr><td class='CE'>", Alias$cua, "</td><td class='CE' style='background-color:gold; color:", clr[1], "'>", T[1,1], "</td>")
	tab = paste(tab, "<td class='CE' style='background-color:lightblue; color:", clr[2], "'>", T[1,2], "</td>")
	tab = paste(tab, "<td class='CE' style='vertical-align:bottom; color:", clr[3], "'>", sum(T[1,]), "<img src='cuadr.png'></td></tr>")
	tab = paste(tab, "<tr><td class='CE'>", Alias$tri, "</td><td class='CE' style='background-color:gold; color:", clr[4], "'>", T[2,1], "</td>")
	tab = paste(tab, "<td class='CE' style='background-color:lightblue; color:", clr[5], "'>", T[2,2], "</td>")
	tab = paste(tab, "<td class='CE' style='vertical-align:bottom; color:", clr[6], "'>", sum(T[2,]), "<img src='trian.png'></td></tr>")
	tab = paste(tab, "<tr><td /><td class='CE' style='color:", clr[7], "'>", sum(T[,1]), "</td><td class='CE' style='color:", clr[8], "'>", sum(T[,2]), "</td><td class='CE' style='color:", clr[9], "'>", sum(T), "</td></tr></table>")
	HTML(paste(css,tab))
})

output$extra <- renderUI({ 
    Z = proceso(); n = Z$n; v0 = Z$form; w0 = Z$col
    css = "<style> .nota {padding:5px; font-size: 120%}</style>"
	shs = length(input$shape)
	cls = length(input$color)
	if (shs==0) return(HTML(paste(css, '<div class="nota">', tr('noform'), '</div>')))
	if (cls==0) return(HTML(paste(css, '<div class="nota">', tr('nocols'), '</div>')))
	v = factor(v0, levels=0:1); w = factor(w0, levels=0:1)
	T = table(v, w)
	if (shs != cls) {
		if (shs==1) {   # 2 colors, 1 shape
			if (input$shape == '0') {
			  if (sum(T[1,])>0) {
				outp = sprintf(tr('form1'), T[1,1], T[1,2], 100*T[1,1]/sum(T[1,]), 100*T[1,2]/sum(T[1,]))
				For = sprintf(tr('form11'), Alias$gol, Alias$cua, T[1,1]/sum(T[1,]), 
					Alias$blu, Alias$cua, T[1,2]/sum(T[1,]))
			  } else {
			    outp = sprintf(tr('form0'), tr('cua'))
				For = ''
			  }
			} else {
			  if (sum(T[2,])>0) {
				outp = sprintf(tr('form2'), T[2,1], T[2,2], 100*T[2,1]/sum(T[2,]), 100*T[2,2]/sum(T[2,]))
				For = sprintf(tr('form11'), Alias$gol, Alias$tri, T[2,1]/sum(T[2,]), 
					Alias$blu, Alias$tri, T[2,2]/sum(T[2,]))
			  } else {
			    outp = sprintf(tr('form0'), tr('tri'))
				For = ''
			  }
			}
		} else {     # 2 shapes, 1 color
			if (input$color == '0') {
			  if (sum(T[,1])>0) {
				outp = sprintf(tr('form3'), T[1,1], T[2,1], 100*T[1,1]/sum(T[,1]), 100*T[2,1]/sum(T[,1]))
				For = sprintf(tr('form11'), Alias$cua, Alias$gol, T[1,1]/sum(T[,1]), 
					Alias$tri, Alias$gol, T[2,1]/sum(T[,1]))
			  } else {
			    outp = sprintf(tr('form00'), tr('golden'))
				For = ''
			  }
			} else {
			  if (sum(T[,2])>0) {
				outp = sprintf(tr('form4'), T[1,2], T[2,2], 100*T[1,2]/sum(T[,2]), 100*T[2,2]/sum(T[,2]))
				For = sprintf(tr('form11'), Alias$cua, Alias$blu, T[1,2]/sum(T[,2]), 
					Alias$tri, Alias$blu, T[2,2]/sum(T[,2]))
			  } else {
			    outp = sprintf(tr('form00'), tr('blues'))
				For = ''
			  }
			}
		}
	} else if (shs == 2) {   # 2 colors, 2 shapes
		outp = tr('form6')
		For = ''
	} else {   # 1 color 1 shape
		outp = tr('form5')
		For = ''
	}
	return(HTML(paste(css, '<div class="nota">', outp, '</div><div class="nota">', For, '</div>')))
})

})

