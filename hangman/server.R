library(shiny)

mot = function(str) {
      paste(str, collapse='')
}
max.F = 10
a = seq(0, 6.28, len=36)
cabeza = array(c(8+0.25*cos(a), 5+0.5*sin(a)), dim=c(36,2))
partes.x = list(cabeza[,1], c(8,8), c(7.2,8.8), c(8,7.4,7.2), c(8,8.6,8.8), c(5,10), c(6,6), c(6,9), c(6,7.5), c(8,8))
partes.y = list(cabeza[,2], c(4.5,3), c(4,4), c(3,1.5,1.6), c(3,1.5,1.6), c(1,1), c(1,7), c(7,7), c(5.5,7), c(7,5.5))

W = scan("words2.txt", what="character")

shinyServer(function(input, output) {

  Myenv = new.env()
  word = sample(W, 1)
  len = nchar(word)
  letras = unlist(strsplit(word, split=NULL))
  Myenv$txt = rep('_', len)
  Myenv$fails = 0
  Myenv$clr = rep('black', 26)
  Myenv$fin = FALSE

  output$fig <- renderPlot({  
  if (Myenv$fin) {
    plot(c(-1,10),c(0,7.5),t='n',axes=FALSE,xlab='',ylab='')
    text(1, 6, "Reload the page", cex=2)
    text(1, 4, "to play again", cex=2)
    return()
  }
  
  u = NA; v = NA
  k  = 0  
  if (length(unlist(input$P)) > 0) {
     u = round((input$P$x -(-1))/0.6)
     v = round((input$P$y -(2.5))/(-0.9))
     if (u>=0 & u<9 & v>=0 & v<3) {
	    k = 1 + 9*v + u
		if (k>26) k = 0
     }
  }
  I = ifelse(k>0, LETTERS[k], NA)
  w = which(letras == I)
  v = length(w)
  if (k>0) {
    if (v==0) {
      Myenv$clr[k] = 'red'
      Myenv$fails = Myenv$fails + 1
    } else {
      Myenv$clr[k] = 'grey'
      Myenv$txt[w] = I
  }}
  msg = ''
  d = length(which(Myenv$txt=='_'))
  if (d==0) {Myenv$fin=TRUE; msg = 'You win'}
  if (Myenv$fails>=max.F) {Myenv$fin=TRUE; msg = 'You lose'}
  
  par(mar=c(1,1,1,1))
  plot(c(-1,10),c(0,7.5),t='n',axes=FALSE,xlab='',ylab='')
  text(-1, 6.5, mot(Myenv$txt), pos=4, cex=3, vfont=c("serif","bold"))
  i = 1
  while (i <= Myenv$fails) {
     lines(partes.x[[i]], partes.y[[i]]); i=i+1
  }
  i = 0:25
  x = (i %% 9)*0.6 -1
  y = 2.5-0.9*(i %/% 9)
  text(x,y, LETTERS, col=Myenv$clr, cex=1.8)	
  rect(x-0.25, y-0.375, x+0.25, y+0.375)
  
  text(0.5, 4.5, msg, cex=4, vfont=c("serif","bold"), col='blue')
  })
})
