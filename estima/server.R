library(shiny)

.L = read.table("etiquetas estima.txt", header=FALSE, sep='\t', quote="", row.names=1, comment.char = "", colClasses=rep("character", 3))

shinyServer(function(input, output, session) {

  SEL = 0
  makeReactiveBinding("SEL")  
  .E <- reactiveValues(d = 1)  #active language and default

  tr <- function(stringId) {
        .L[stringId, .E$d]
  }

  observeEvent(input$eng, {    .E$d=1   })
  observeEvent(input$spa, {    .E$d=2   })

  output$titulo = renderText({ tr('titulo') })
  output$tab1 = renderText({ tr('tab1') })
  output$tab2 = renderText({ tr('tab2') })
  output$txt1 = renderUI({ HTML(tr('txt1')) })
  output$txt2 = renderText({ tr('txt2') })
  output$txt3 = renderUI({ HTML(tr('txt3')) })
  output$txt4 = renderUI({ HTML(tr('txt4')) })
  output$txt5 = renderText({ tr('txt5') })
  output$txt6 = renderUI({ HTML(tr('txt6')) })
  output$txt7 = renderText({ tr('txt7') })
  output$vmax = renderText({ tr('vmax') })
  output$enelab = renderText({ tr('enelab') })
  output$stat = renderText({ tr('stat') })
  output$viento = renderUI({
		selectInput("tipo", tr('prof'), c(tr('unif'), tr('deb'), tr('fuer')),
		   selected = ifelse(SEL==0, tr('unif'), ifelse(SEL==1, tr('deb'), tr('fuer'))))
	})
  
  M = 1000
  v <- reactiveValues(T=NA, n=NA)
  observeEvent(input$wind, { v$T = as.numeric(input$wind) })
  observeEvent(input$ene, { v$n = as.numeric(input$ene) })
  observeEvent(input$tipo, { 
      x = input$tipo
	  SEL <<- ifelse(x==tr('unif'), 0, ifelse(x==tr('deb'), 1, 2))
  })

  f0 = function(x) rep(1/v$T, length(x))
  f1 = function(x) 2/v$T*(1-x/v$T)
  f2 = function(x) 2*x/v$T^2  

  datasetInput <- reactive({
	   #input$tipo
       opc = SEL + 1
       corr_media = switch(opc, 1/2, 1/3, 2/3)
       corr_mediana = switch(opc, 1/2, 1-1/sqrt(2), 1/sqrt(2))
	   corr_stdev = switch(opc, sqrt(12), sqrt(18), sqrt(18))
       u = array(runif(M * v$n), dim=c(M, v$n))
	   if (SEL==0) {
	       U = v$T * u
	   } else {
	       U = v$T * sqrt(u)
		   if (SEL == 1) U = v$T - U
	   }
	   S1 = apply(U, 1, mean) / corr_media
	   S2 = apply(U, 1, median) / corr_mediana
	   S3 = apply(U, 1, sd) * corr_stdev
       S4 = apply(U, 1, max)
	   list(s1=S1, s2=S2, s3=S3, s4=S4)
  })
  
  output$dist <- renderPlot({  
      F = switch(SEL+1, f0, f1, f2)
	  par(mar=c(4,4,0.4,0.4))
      curve(F, 0, v$T, ylim=c(0, 2/v$T), xlab=tr('velo'), ylab=tr('fd'))
  })
  
  output$statW <- renderUI({
       opc = SEL + 1
       corr_media = switch(opc, 1/2, 1/3, 2/3)
       corr_mediana = switch(opc, 1/2, 1-1/sqrt(2), 1/sqrt(2))
	   corr_stdev = switch(opc, sqrt(12), sqrt(18), sqrt(18))
	   txt = paste0('<h3>', tr('indic'), '</h3><ul style="list-style-type:none">')
       txt = paste0(txt, '<li>', tr('Max'), v$T, '</li>')
       txt = paste0(txt, '<li>', tr('espe'), ': ', round(v$T * corr_media, 3), '</li>')
       txt = paste0(txt, '<li>', tr('medi'), ': ', round(v$T * corr_mediana, 3), '</li>')
       txt = paste0(txt, '<li>', tr('Sd'), round(v$T / corr_stdev, 3), '</li></ul>')
	   txt = paste0(txt, '<br><br><h4>', tr('emm'), '</h4>')
	   txt = paste0(txt, '<ul style="list-style-type:none"><li>', tr('oI'), '</li><li>', tr('oII'), '</li>')
	   txt = paste0(txt, '<li>', tr('oIII'), '</li><li>', tr('oIV'), '</li></ul>')
	   HTML(txt)
  })

  output$estim <- renderPlot({  
       data = datasetInput()
	   S1 = data$s1; S2	= data$s2; S3 = data$s3; S4 = data$s4
	   aa = min(c(S1,S2,S3,S4))
	   bb = max(c(S1,S2,S3,S4))
       par(mar=c(3,3,1.2,0.2), mfrow=c(2,2))
       hist(S1, main='I', xlim=c(aa,bb), col='lightblue')
       hist(S2, main='II', xlim=c(aa,bb), col='lightblue')	   
       hist(S3, main='III', xlim=c(aa,bb), col='lightblue')
       hist(S4, main='IV', xlim=c(aa,bb), col='lightblue')	
  })

  output$statT <- renderUI({
       data = datasetInput()
	   S1 = data$s1; S2	= data$s2; S3 = data$s3; S4 = data$s4
	   txt = '<head><style>table, th, td {border: 1px solid black;  border-collapse: collapse;} th, td {padding: 5px;}</style></head>'
	   txt = paste0(txt, '<table><tr><th></th><th>', tr('prom'), '</th><th>', tr('dsv'), '</th></tr>')
	   Z = c('I', 'II', 'III', 'IV')
	   for (i in 1:4) {
	      S = get(paste0('S',i))
	      txt = paste0(txt, '<tr><th>', Z[i], '</th><td>', round(mean(S), 3), '</td><td>', round(sd(S), 3), '</td></tr>')
	   }
	   txt = paste0(txt, '</table>')
	   HTML(txt)
  })

  output$tipos <- renderPlot({  
      tiks = c(0,0.5,1)*v$T
	  par(mfrow=c(1,3), mar=c(2,2,1.4,0.4))
      curve(f0, 0, v$T, ylim=c(0, 2/v$T), xlab='', ylab='', axes=FALSE, main=tr('unif'))
      axis(1,at=tiks,lab=c('0', 'T/2', 'T')); #axis(2)
      curve(f1, 0, v$T, ylim=c(0, 2/v$T), xlab='', ylab='', axes=FALSE, main=tr('deb'))
      axis(1,at=tiks,lab=c('0', 'T/2', 'T')); #axis(2)
      curve(f2, 0, v$T, ylim=c(0, 2/v$T), xlab='', ylab='', axes=FALSE, main=tr('fuer'))
      axis(1,at=tiks,lab=c('0', 'T/2', 'T')); #axis(2)
  })
    
  output$propA <- renderUI({
	withMathJax(
		paste0(tr('fdens'),': $$f_{V1}(x) = \\frac {1}{T}, \\quad 0 \\lt x \\lt T$$', tr('espe'),': $$\\mu_{V1} = \\frac {T}{2}$$', tr('vari'),': $$\\sigma^2_{V1} = \\frac {T^2}{12}$$', tr('medi'),': $$Me = \\frac {T}{2}$$ $$\\hat T_I = 2 \\,\\bar x $$ $$\\hat T_{II} = 2 \\,me $$ $$\\hat T_{III} = \\sqrt{12} \\,s $$ $$\\hat T_{IV} = max$$')
	)
	})
  output$propB <- renderUI({
	withMathJax(
		paste0(tr('fdens'),': $$f_{V2}(x) = \\frac {2-2x}{T^2}, \\quad 0 \\lt x \\lt T$$', tr('espe'),': $$\\mu_{V2} = \\frac {T}{3}$$', tr('vari'),': $$\\sigma^2_{V2} = \\frac {T^2}{18}$$', tr('medi'),': $$Me = \\left(1-1/\\sqrt{2} \\right) T $$ $$\\hat T_I = 3 \\,\\bar x $$ $$\\hat T_{II} = \\frac{me}{1-1/\\sqrt{2}} $$ $$\\hat T_{III} = \\sqrt{18} \\,s $$ $$\\hat T_{IV} = max$$')
	)
	})
  output$propC <- renderUI({
	withMathJax(
		paste0(tr('fdens'),': $$f_{V3}(x) = \\frac {2x}{T^2}, \\quad 0 \\lt x \\lt T$$', tr('espe'),': $$\\mu_{V3} = \\frac {2T}{3}$$', tr('vari'),': $$\\sigma^2_{V3} = \\frac {T^2}{18}$$', tr('medi'),': $$Me = 1/\\sqrt{2} T $$ $$\\hat T_I = \\frac{3}{2}\\bar x $$ $$\\hat T_{II} = \\sqrt{2}\\,me $$ $$\\hat T_{III} = \\sqrt{18} \\,s $$ $$\\hat T_{IV} = max$$')
	)
	})
})
	   