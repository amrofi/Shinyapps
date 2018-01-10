library(shiny)

.L = read.table("etiquetas bombas.txt", header=FALSE, sep='\t', quote="", row.names=1, colClasses=rep("character", 3))

# Define server logic required to generate and plot a random distribution
shinyServer(function(input, output) {

  .E <- reactiveValues(d = 1)  #active language and default

  tr <- function(stringId) {
        .L[stringId, .E$d]
  }

  observeEvent(input$eng, {    .E$d=1   })
  observeEvent(input$spa, {    .E$d=2   })
  
  output$titulo = renderText( { tr("titulo") } )
  output$parrafo = renderText( { tr("parrafo") } )
  output$malla = renderText( { tr("malla") } )
  output$bombas = renderText( { tr("bombas") } )
  output$dispe = renderText( { tr("dispe") } )
  output$espe = renderText( { tr("espe") } )
  output$obse = renderText( { tr("obse") } )
  output$bocel = renderText( { tr("bocel") } )

  
  datasetInput <- reactive({
    ##--Inputs
    #N <- input$malla
    n <- input$bom
    sg <- input$desv^2
    X = 100
    cx=X/2; cy=X/2
    nn = n/(pnorm(X,cx,sg)-pnorm(0,cx,sg))/(pnorm(X,cy,sg)-pnorm(0,cy,sg))
    nn = round(1.2*nn)
    u = rnorm(nn, cx, sg)
    v = rnorm(nn, cx, sg)
    t = which(u>0 & u<X & v>0 & v<X)
	reemplazo = length(t) < n   # es dificil pero por si acaso...
    t2 = sample(t, n, replace=reemplazo)
    u = u[t2]
    v = v[t2]

    #list(N=N,X=X,u=u,v=v)
    list(X=X,u=u,v=v)
  })
  
  output$mapa <- renderPlot({
    
  Q = datasetInput()
  X = Q$X
  N <- input$malla   # N = Q$N
  u = Q$u
  v = Q$v
  x = seq(0, X, len=N+1)
  U = cut(u, x)
  V = cut(v, x)
  W = array(table(U,V), dim=N*N)
  lam = length(u)/N/N
  b = as.numeric(names(table(W)))
  obs = as.numeric(table(W))
  pr = dpois(b, lam)*N*N
  layout(matrix(c(1,2),nrow=1), wid=c(3,2))
  par(mar=c(1,1,0,0))
  plot(c(0,X), c(0,X), t='n', axes=F, xlab='', ylab='')
  x = seq(0, X, len=N+1)
  abline(h=x, col='grey', lty=2)
  abline(v=x, col='grey', lty=2)
  text(u,v,lab='*', col='red', cex=1.5)
  par(mar=c(2.5,2.5,0.5,0.5), lend=1)
  plot(b, pr, t='h', lwd=11, col='grey', ylim=c(0,max(obs, pr)))
  points(b, obs, t='h', col='red', lwd=2)
  legend("topright", leg=c(tr("espe"), tr("obse")), lwd=c(11,3), col=c("grey","red"))

})

  output$comp <- renderTable({

  Q = datasetInput()
  X = Q$X
  N <- input$malla    # N = Q$N
  u = Q$u
  v = Q$v
  x = seq(0, X, len=N+1)
  U = cut(u, x)
  V = cut(v, x)
  W = array(table(U,V), dim=N*N)
  lam = length(u)/N/N
  b = as.numeric(names(table(W)))
  pr = dpois(b, lam)*N*N
  DF=data.frame(b, as.numeric(table(W)), round(pr,2))
  names(DF)=c(tr("bocel"), tr("obse"), tr("espe"))
  DF
#d=(as.numeric(table(W))-pr)^2/pr
#sum(d[which(pr>1)])

}, digits=c(0,0,0,2))
  
  output$explicacion <- renderUI({
	  N = input$malla
	  n = input$bom
	  mn = round(n/N/N, 3)
      HTML(sprintf(tr("expla"), mn, n, N*N, mn))
  })
})