.L = read.table("etiquetas modlin.txt", header=FALSE, sep='\t', quote="", row.names=1, colClasses=rep("character", 3))

M = 1000

# Estimates
simu.lm = function(X, Z, E, n) {
       mx = mean(X)
       sx = sd(X)
	Y = matrix(rep(Z, M) + E, ncol=n, byrow=TRUE)
	my = apply(Y, 1, mean)
	sy = apply(Y, 1, sd)
	rxy = apply(Y, 1, cor, X)
	b1 = rxy*sy/sx
	b0 = my - b1*mx
	s = sqrt((n-1)/(n-2)*sy^2*(1-rxy^2))
    list(b0=b0, b1=b1, s=s)
}

# para ajustar una curva de segundo grado (c,d,f) a una recta (a,b), entre u y v
ffun = function(a,b,c,d,f,u,v) {
-1/3*u^3*(2*f*(c-a)+(b-d)^2)+1/3*v^3*(2*f*(c-a)+(b-d)^2)-u^2*(a-c)*(b-d)+v^2*(a-c)*(b-d)-u*(a-c)^2+v*(a-c)^2+1/2*f*u^4*(b-d)+1/2*f*v^4*(d-b)-1/5*f^2*u^5+(f^2*v^5)/5
}

Myenv = new.env()
Myenv$a = NA
Myenv$b = NA
Myenv$f = NA
Myenv$u = 1
Myenv$v = 10


mifun = function(x) {
  ffun(Myenv$a, Myenv$b, x[1], x[2], Myenv$f, Myenv$u, Myenv$v)
}

curvita = function(fun, pars) {
    if (fun=='N') {
       x = seq(pars[3], pars[4], len=75)
       y = dnorm(x, pars[1], pars[2])
    } else if (fun=='t') {
       x = seq(pars[2], pars[3], len=75)
       y = dt(x, pars[1])
    } else if (fun=='X') { # en realidad no representaremos la X2, sino la S2
       K = pars[1]/pars[2]  # (n-2)/sigma2
       x = seq(K*pars[3], K*pars[4], len=75)   #  l < s2 < u
       y = dchisq(x, pars[1])
       x = x/K
       y = K*y
    }
    lines(x, y, col='purple')
}

library(shiny)

# Define server logic required to generate and plot a random distribution
shinyServer(function(input, output) {

  .E <- reactiveValues(d = 1)  #active language and default

  tr <- function(stringId) {
        .L[stringId, .E$d]
  }
  SEL = list(prem=1, qua=0.2, nor=1, rho=0.5)

  observeEvent(input$eng, {    .E$d=1   })
  observeEvent(input$spa, {    .E$d=2   })
  observeEvent(input$prem, {    
     t = input$prem
     SEL$prem <<- ifelse(t==tr("all"), 1, 
				ifelse(t==tr("homo"), 2, 
					ifelse(t==tr("line"), 3, 
						ifelse(t==tr("norma"), 4, 5)
			)))
	 })
  observeEvent(input$qua, {   SEL$qua <<- input$qua    }  )    
  observeEvent(input$rho, {   SEL$rho <<- input$rho    }  )    
  observeEvent(input$nor, {    
     t = input$nor
     SEL$nor <<- ifelse(t==tr("unif"), 1, 
				ifelse(t==tr("expo"), 2, 3))
	 })
    
  output$titulo = renderText( { tr("titulo") } )
  output$tab1 = renderText( { tr("tab1") } ) 
  output$tab2 = renderText( { tr("tab2") } ) 
  output$tab3 = renderText( { tr("tab3") } ) 
  output$side1 = renderText( { tr("side1") } ) 
  output$side2 = renderText( { tr("side2") } ) 
  output$pend = renderUI( { HTML(tr("pend")) } )
  output$cons = renderUI( { HTML(tr("cons")) } )
  output$sig = renderUI( { HTML(tr("sig")) } )
  output$ene = renderText( { tr("ene") } ) 
  output$T1.1 = renderText( { tr("T1.1") } ) 
  output$T1.2 = renderUI( { HTML(tr("T1.2")) } )
  output$T2 = renderUI( { HTML(tr("T2")) } ) 
  output$wtf = renderText( { tr("wtf") } ) 
  output$t2.a = renderText( { tr("t2.a") } ) 
  output$t2.b = renderText( { tr("t2.b") } ) 
  output$t2.c = renderText( { tr("t2.c") } ) 
  output$t2.d = renderText( { tr("t2.d") } ) 
  output$Premises = renderUI( {
	   sltd = switch(SEL$prem,
			tr("all"), tr("homo"), tr("line"), tr("norma"), tr("indd")
		) 
       radioButtons("prem", tr('prem'), c(tr('all'), tr('homo'),
             tr('line'), tr('norma'), tr('indd')), selected = sltd)
  } )
  output$prem.CTRL = renderUI( {
     prem = input$prem
	 if (is.null(prem)) return()
     if (prem == tr('line')) {
		sliderInput("qua", tr('nl'), min=0.01, max=0.3, step=0.005, value=SEL$qua)
	 } else if (prem == tr('norma')) {
	    sltd = switch(SEL$nor, tr("unif"), tr("expo"), tr("ush"))
		selectInput("nor", HTML(tr('epsi')), c(tr('unif'), tr('expo'), tr('ush')),
				selected = sltd)
	 } else if (prem == tr('indd')) {
		sliderInput("rho", tr('depp'), min=-0.9, max=0.9, step=0.05, value=SEL$rho)
	 }
  } )
			 
  datasetInput <- reactive({
     n = input$obs
     bet0 = input$bet0
     bet1 = input$bet1
     sg = input$desv
     prem = input$prem
	 if (is.null(prem)) return()
     X = seq(1, 10, len=n)
     mx = mean(X)
     sx = sd(X)
     sxx = (n-1)*var(X)
     if (prem==tr('indd')) if (is.null(input$rho)) return()
     if (prem==tr('line')) if (is.null(input$qua)) return()
     if (prem==tr('norma')) if (is.null(input$nor)) return()

     if (prem==tr('line')) {  # Sin linealidad
        Myenv$f = input$qua
        Myenv$a = bet0
        Myenv$b = bet1
        Q = nlm(mifun, c(0,0))
        Z = Q$est[1] + Q$est[2]*X + Myenv$f*X^2
     } else {
        Z = bet0+bet1*X
     }
     if (prem==tr('indd')) {  # Sin independencia
        E = c()
        sh = sg*sqrt(1-input$rho^2)
        for (i in 1:M) {
          E = c(E, arima.sim(n=n,model=list(ar=input$rho),sd=sh))
        }
     } else if (prem==tr('norma')) {  # Sin normalidad
        u = input$nor
        if (u==tr('unif')) { # uniforme; (b-a)/sq(12) = sg; 2b/sq(12); b/sq(3); b = sq(3)sg
	    E = runif(M*n,-1,1)*sqrt(3)*sg
        } else if (u==tr('expo')) { # exponencial; ha de tener media 0
	    E = rexp(M*n, 1/sg)-sg
        } else if (u==tr('ush')) { # forma de U; usaremos una beta escalada
	    E = sg*sqrt(6)*(rbeta(M*n, 0.25, 0.25)-0.5)
        }
     } else if (prem==tr('homo')) {  # Sin homocedasticidad
		we = sqrt(seq(2/(n+1), 2*n/(n+1), len=n))
		E = rnorm(M*n, 0, sg) * rep(we, M)
     } else {	 # prem == 0: todo OK 
        E = rnorm(M*n, 0, sg)
     }
     M1 = simu.lm(X, Z, E, n)
     sb1 = M1$s/sx/sqrt(n-1)
     t1 = (M1$b1 - bet1)/sb1
   # de paso, calculamos los alfas empiricos
        tcr = qt(0.975, n-2)
        Tb = cut(abs(t1), c(0, tcr, 7))
        alfa1 = (table(Tb)/M) [2]
     sb0 = M1$s*sqrt(1/n + mx^2/sxx)
     t0 = (M1$b0 - bet0)/sb0
        Tb = cut(abs(t0), c(0, tcr, 7))
        alfa0 = (table(Tb)/M) [2]

     list(n=n, bet0=bet0, bet1=bet1, sg=sg, X=X, M1=M1, Y = Z[1:n] + E[1:n],
          t1=t1, t0=t0, alfa=c(alfa1, alfa0))
  })

  output$fig <- renderPlot({
   #input$Update

   data = datasetInput();
   if (is.null(data)) return()
   if (data$n==0) return()
   X = data$X;  M1 = data$M1;  bet0 = data$bet0;  bet1 = data$bet1;  n = data$n;  sg = data$sg; t1 = data$t1; t0 = data$t0

   mx = mean(X)
   sx = sd(X)
   sxx = (n-1)*var(X)
   x = range(X)
   y = range(M1$b0 + M1$b1*x)
   
   par(mar=c(4,3,1.2,0.5), mfrow=c(2,3), cex.main=1.5)
   plot(x,y, t='n', xlab='X', ylab='Y')
   segments(x[1],M1$b0+M1$b1*x[1], x[2],M1$b0+M1$b1*x[2], col=rgb(0.3,0.3,0.3,0.2))
   abline(a=bet0, b=bet1, lwd=2, col='white')
   hist(M1$b1, xlab='', main=bquote(b[1]), bre=20, freq=FALSE)
   curvita('N', c(bet1, sg*sqrt(1/sxx), min(M1$b1), max(M1$b1)))
   hist(M1$b0, xlab='', main=bquote(b[0]), bre=20, freq=FALSE)
   curvita('N', c(bet0, sg*sqrt(1/n + mx^2/sxx), min(M1$b0), max(M1$b0)))
   par(mar=c(2,3,3,0.5))
   hist(M1$s^2, xlab='', main=bquote(s^2), bre=20, freq=FALSE)
   curvita('X', c(n-2, sg^2, min(M1$s)^2, max(M1$s)^2))

   hist(t1, xlab='', main=bquote(t : beta[1]== .(bet1)), bre=20, freq=FALSE)
   curvita('t', c(n-2, min(t1), max(t1)))

   hist(t0, xlab='', main=bquote(t : beta[0]== .(bet0)), bre=20, freq=FALSE)
   curvita('t', c(n-2, min(t0), max(t0)))

  })

  output$fig2 <- renderPlot({

   data = datasetInput();
   X = data$X;  Y = data$Y;  n = data$n

   mode = lm(Y ~ X)

   par(mar=c(4, 4, 2.2, 0.5), mfrow=c(2,2))
   plot(X, Y, pch=19)
   plot(mode,c(2,1))
   plot (1:n, rstandard(mode), type="l", ylab='')
   title(main='Standardized residuals in sequence', font.main=1)
  })

  output$alfas <- renderUI({

   data = datasetInput();
   prem = input$prem
   bet0 = data$bet0;  bet1 = data$bet1;  alfa1 = data$alfa[1]; alfa0 = data$alfa[2]

   txt = tr('t3.1')
   txt = paste(txt, sprintf(tr('t3.2'), bet1, alfa1))
   txt = paste(txt, sprintf(tr('t3.3'), bet0, alfa0))
   txt = paste(txt, tr('t3.4'))

   if (prem==tr('homo')) {
      txt2 = tr('Hetero')
   } else if (prem==tr('line')) {
      txt2 = tr('NoLin')
   } else if (prem==tr('norma')) {
      txt2 = tr('Nono')
        u = input$nor
        if (u==tr('unif')) { # uniforme; 
	    txt2 = paste(txt2, tr('modun'))
        } else if (u==tr('expo')) { # exponencial;
	    txt2 = paste(txt2, tr('modex'))
        } else if (u==tr('ush')) { # forma de U; usaremos una beta escalada
	    txt2 = paste(txt2, tr('modU'))
        }
	  txt2 = paste(txt2, tr('Nono2'))
   } else if (prem==tr('indd')) {
      txt2 = tr('NoInd')
   } else {
      txt2 = tr('todoOK')
   }

   HTML(paste(txt, '<br><b>', txt2, '</b><br>'))

  })

})
