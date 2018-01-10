.L = read.table("etiquetas vps.txt", header=FALSE, sep='\t', quote="", row.names=1, colClasses=rep("character", 3))

library(shiny)

# Define server logic required to generate and plot a random distribution
shinyServer(function(input, output) {
  
  SEL = 0
  makeReactiveBinding("SEL")  
  .E <- reactiveValues(d = 2)  #active language and default

  tr <- function(stringId) {
        .L[stringId, .E$d]
  }

  observeEvent(input$eng, {    .E$d=1   })
  observeEvent(input$spa, {    .E$d=2   })
    
  output$titulo = renderText( { tr("titulo") } )
  output$tab1 = renderText( { tr("tab1") } )
  output$tab2 = renderText( { tr("tab2") } )
  output$prev = renderText( { tr("prev") } )
  output$sens = renderText( { tr("sens") } )
  output$espe = renderText( { tr("espe") } )
  output$defpr = renderUI( { HTML(tr("defpr")) } )
  output$defsn = renderText( { tr("defsn") } )
  output$defsp = renderText( { tr("defsp") } )
  output$defpos = renderText( { tr("defpos") } )
  output$defneg = renderText( { tr("defneg") } )

  output$Var = renderUI( {
	selectInput("var", label = tr('choose'),
					choices = c(tr('pos'), tr('neg')),
					selected = ifelse(SEL==0, tr('pos'), tr('neg')), width="200px")
  } )
  
  observeEvent(input$var, { SEL <<- ifelse(input$var==tr('pos'), 0, 1) })
  
  output$distPlot <- renderPlot({
    
    ##--Inputs
    pre <- input$pre
    esp <- input$esp
    sens <- input$sens    
    
    ##--Colores
    co.en <- "red"
    co.ep <- "red"
    co.sn <- rgb(0,1,0,0.5)#rgb(155,198,105,255,max=255)
    co.sp <- rgb(0,1,0,0.5)#rgb(155,198,105,255,max=255)
    
    ## grafico
    layout(matrix(c(1,2), nrow=2), heights=c(4,1))
    
    par(mar=c(0,0,4.5,0))
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
	eti <- c(paste(tr('enfs'),tr('poss')), paste(tr('enfs'),tr('negs')), "", "",
			paste(tr('sanos'),tr('poss')), paste(tr('sanos'),tr('negs')))
    legend(50,120,eti,ncol=3,fill=c(co.ep,co.en,0,0,co.sp,co.sn),xjust=0.5,cex=1.5,pt.cex=3,bty="n",density=c(200,20,-1,-1,20,200),angle=c(0,45,-1,-1,135,0),border=NA,pt.lwd=3)
    par(xpd=FALSE)    
	h <- 45  # Altura de la línea
	x <- 40  # Centro de la línea
    p <- 0.3 # Proporción del tamaño original de los rectángulos
    sep <- 4 # Separación de los rectángulos de la línea
    par(mar=c(0,0.1,0,0.1))
    plot(NA,xlim=c(0,100),ylim=c(0,100),bty="n",
           xlab="",ylab="",xaxt="n",yaxt="n",xaxs="i",yaxs="i")
    if (SEL == 0){
      text(0,h,bquote(paste(.(tr('vpp'))=="")),adj=0,cex=2,font=3)
      text(x,h-10,"+",cex=2,font=2)
      rect(x-(p*pre)/2,h+sep,x+(p*pre)/2,h+sens*p+sep,col=co.ep,border=NA)
      rect(x-(p*pre)-5,h-sens*p-sep,x-5,h-sep,col=co.ep,border=NA)
      rect(x+5,h-(100-esp)*p-sep,x+5+(100-pre)*p,h-sep,col=co.sp,border=NA,density=15,angle=135,lwd=2)
      segments(12,h,70,h,lwd=2)
	} else {
      text(0,h,bquote(paste(.(tr('vpn'))=="")),adj=0,cex=2,font=3)
      text(x,h-10,"+",cex=2,font=2)
	  rect(x-p*(100-pre)/2, h+sep, x+p*(100-pre)/2, h+esp*p+sep, col=co.sn, border=NA)
      rect(x-(p*pre)-5,h-(100-sens)*p-sep,x-5,h-sep,col=co.en, border=NA,density=15,angle=45,lwd=2)
      rect(x+5, h-esp*p-sep, x+5+(100-pre)*p, h-sep, col=co.sn, border=NA)
      segments(12,h,70,h,lwd=2)
	}
})
  
  output$frmla = renderUI( {
    ##--Inputs
    pre <- input$pre
    esp <- input$esp
    sens <- input$sens    
    
    ##--Valores
    vaen <- pre*(100-sens)/10000
    vaep <- pre*sens/10000
    vasn <- (100-pre)*esp/10000
    vasp <- (100-pre)*(100-esp)/10000
    
    ##--Indicadores
    VPP <- vaep/(vaep+vasp)
    VPN <- vasn/(vasn+vaen)
  	withMathJax(
      if (SEL == 0){
		paste0("$$", tr('vpp'), "=P(", tr('enfs'), "|", tr('poss'), ")= \\frac{P(", tr('enfs'), "\\cap ", tr('poss'), ")}{P(", tr('poss'), ")} = \\frac{", vaep,"}{", vaep+vasp, "} =", round(VPP,3), " $$")
      } else {
		paste0("$$", tr('vpn'), "=P(", tr('sanos'), "|", tr('negs'), ")= \\frac{P(", tr('sanos'), "\\cap ", tr('negs'), ")}{P(", tr('negs'), ")} = \\frac{", vasn,"}{", vaen+vasn, "} =", round(VPN,3), " $$")
	  }
	)
  })
})