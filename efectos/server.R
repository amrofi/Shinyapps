.L = read.table("etiquetas efectos2.txt", header=FALSE, sep='\t', quote="", row.names=1, colClasses=rep("character", 3))

library(shiny)

# Define server logic required to generate and plot a random distribution
shinyServer(function(input, output) {
  
  .E <- reactiveValues(d = 2)  #active language and default
  SEL = 1
  makeReactiveBinding("SEL")  

  tr <- function(stringId) {
        .L[stringId, .E$d]
  }

  observeEvent(input$eng, {    .E$d=1   })
  observeEvent(input$spa, {    .E$d=2   })
    
  output$titulo = renderText( { tr("titulo") } )
  output$tab1 = renderText( { tr("tab1") } )
  output$tab2 = renderText( { tr("tab2") } )
  output$tab3 = renderText( { tr("tab3") } )
  output$txt1 = renderText( { tr("txt1") } )
  output$txt2 = renderText( { tr("txt2") } )
  output$ite1 = renderText( { tr("ite1") } )
  output$ite2 = renderText( { tr("ite2") } )
  output$ite3 = renderText( { tr("ite3") } )
  output$expe = renderText( { tr("expe") } )
  output$pote = renderText( { tr("pote") } )
  output$intro = renderText( { tr("intro") } )
  output$grAnim = renderText( { tr("grAnim") } )
  output$XX = renderUI(
  		radioButtons("equis", tr('fun'), choices=c(tr('expe0'), tr('pote')),
			selected=ifelse(SEL==1, tr('expe0'), tr('pote')))
  )
  
  observeEvent(input$equis, { 
      x = input$equis
	  SEL <<- ifelse(x==tr('expe0'), 1, 2)
  })


  output$distPlot <- renderPlot({
    
    ##--Inputs
    pre <- input$pre
    esp <- input$esp
    sens <- input$sens
    
    
    ##--Colores
    #co.en <- rgb(1,0,0,0.7)
    #co.ep <- rgb(0,1,0,0.7)
    #co.sn <- rgb(0,1,0,0.3)
    #co.sp <- rgb(1,0,0,0.3)
    co.en <- "skyblue2"
    co.ep <- "skyblue2"
    co.sn <- "plum2"
    co.sp <- "plum2"
    
    ##--Valores
    vaen <- pre*(100-sens)/10000
    vaep <- pre*sens/10000
    vasn <- (100-pre)*esp/10000
    vasp <- (100-pre)*(100-esp)/10000
    
    ##--Indicadores
    VPP <- vaep/(vaep+vasp)
    VPN <- vasn/(vasn+vaen)
    
    ## grafico
    par(mfrow=c(2,1))
    
    plot(NA,xlim=c(0,100),ylim=c(0,100),bty="n",
         xlab="",ylab="",xaxt="n",yaxt="n",xaxs="i",yaxs="i")
    #rect(-10,-10,pre,100-sens,col=co.en,border=NA) # Enfermos negativos
    #rect(-10,100-sens,pre,110,col=co.ep,border=NA) # Enfermos positivos
    #rect(pre,-10,110,esp,col=co.sn,border=NA) # Sanos negativos
    #rect(pre,esp,110,110,col=co.sp,border=NA) # Sanos positivos
    rect(-10,-10,pre,100-sens,col=co.en,border=NA,density=15,angle=45,lwd=2) # Enfermos negativos
    rect(-10,100-sens,pre,110,col=co.ep,border=NA) # Enfermos positivos
    rect(pre,-10,110,esp,col=co.sn,border=NA) # Sanos negativos
    rect(pre,esp,110,110,col=co.sp,border=NA,density=15,angle=135,lwd=2) # Sanos positivos

    par(xpd=TRUE)
    eti <- c(tr('resig'), tr('renosig'),"","", tr('sinsig'), tr('sinnosig'))
#    legend(50,123,eti,ncol=3,col=c(co.ep,co.en,0,0,co.sp,co.sn),pch=15,xjust=0.5,cex=1.5,pt.cex=1.8,bty="n")
#legend(50,123,eti[c(1,2,5,6)],ncol=2,col=c(co.ep,co.en,co.sp,co.sn),pch=15,xjust=0.5,cex=1.5,pt.cex=1.8,bty="n")
    legend(50,123,eti[c(1,2,5,6)],ncol=2,fill=c(co.ep,co.en,co.sp,co.sn),xjust=0.5,cex=1.6,pt.cex=3,bty="n",density=c(200,20,20,200),angle=c(0,45,135,0),border=NA,pt.lwd=3)
    
par(xpd=FALSE)
    
	par(mar=c(0,4.1,0,2.1),mai=c(2,1,0,0))
      plot(NA,xlim=c(0,100),ylim=c(0,100),bty="n",
           xlab="",ylab="",xaxt="n",yaxt="n",xaxs="i",yaxs="i")
    par(xpd=TRUE)      
    text(-5,88,bquote(paste(P,.(tr('efcondsig'))==frac(P(.(tr('efr'))~intersect("Signif.")),P("Signif.")),""==frac(.(vaep),.(vaep+vasp)),""==.(round(VPP,3)))),adj=0,cex=1.8,font=3)
    par(xpd=FALSE)
      h <- 45  # Altura de la línea
      x <- 40  # Centro de la línea
      p <- 0.26 # Proporción del tamaño original de los rectángulos
      sep <- 3 # Separación de los rectángulos de la línea
      
      #text(0,h,bquote(paste(VPP=="")),adj=0,cex=2,font=3)
      text(x,h-10,"+",cex=4,font=2)
      rect(x-(p*pre)/2,h+sep,x+(p*pre)/2,h+sens*p+sep,col=co.ep,border='grey')
      rect(x-(p*pre)-5,h-sens*p-sep,x-5,h-sep,col=co.ep,border='grey')
      rect(x+5,h-(100-esp)*p-sep,x+5+(100-pre)*p,h-sep,col=co.sp,border='grey',density=15,angle=135,lwd=2)
      segments(10,h,70,h,lwd=4)
})
  
  output$ioann = renderUI({
	txt = paste('<table><tbody><tr><th>',tr('tipo'),'</th><th>',tr('prob'),'</th></tr><tr><td>',tr('ma'),'</td><td>67%</td></tr><tr><td>',tr('eca'),'</td><td>50%</td></tr>')
	txt = paste(txt, '<tr><td>',tr('ma2'),'</td><td>25%</td></tr><tr><td>',tr('obs1'),'</td><td>10%</td></tr><tr><td>',tr('obs2'),'</td><td>0.1%</td></tr></tbody></table>')
      css = "<style>table, th, td { border: 1px solid black; border-collapse: collapse; } th, td {text-align:center
; padding:5px}</style>"
	HTML(paste(css, txt))
  })

  output$regla = renderUI({
	if (input$anima) {
		if (SEL == 1) { # en función de Expectativa
			sliderInput("sens2", tr("pote"), min = 0, max = 100, value = 0, step=2,
							animate=animationOptions(interval=300))
		} else {
			sliderInput("pre2", tr("expe"), min = 0, max = 100, value = 0, step=2,
							animate=animationOptions(interval=300))
		}
	}
  })
  
  output$cred <- renderPlot({
    sens2 = ifelse(is.null(input$sens2), 0, input$sens2)
    pre2 = ifelse(is.null(input$pre2), 0, input$pre2)
	esp2 = input$esp
	
	par(mar=c(5,4,1,1))
	ylab = tr('credi')
	if (SEL == 1) { # Expectativa
	   X = 0:100
	   pt = ifelse(input$anima, sens2, input$sens)
	   Y = pt*X / (pt*X + (100-esp2)*(100-X))
	   pie = sprintf("%s = %d%%    1-α = %d%%", tr('pote'), pt, esp2)
	   plot(X, Y, t='l', xlab=paste(tr('expe0'),'(%)'), ylab=ylab, sub=pie,
			lwd=2, col='dodgerblue3', ylim=c(0,1))
	   if (input$anima==0) {
			x = input$pre
			y = input$sens*x / (input$sens*x + (100-esp2)*(100-x))
			lines(c(x, x, 0), c(0, y, y), lty=2)
	  }
	} else {
	   X = 0:100
	   xp = ifelse(input$anima, pre2, input$pre)
	   Y = xp*X / (xp*X + (100-xp)*(100-esp2))
	   pie = sprintf("%s = %d%%    1-α = %d%%", tr('expe0'), xp, esp2)
	   plot(X, Y, t='l', xlab=paste(tr('pote'),'(%)'), ylab=ylab, sub=pie,
			lwd=2, col='dodgerblue3', ylim=c(0,1))
	   if (input$anima==0) {
			x = input$sens
			y = input$pre*x / (input$pre*x + (100-esp2)*(100-xp))
			lines(c(x, x, 0), c(0, y, y), lty=2)
	  }
	}
  })  
})