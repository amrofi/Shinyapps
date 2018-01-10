.L = read.table("etiquetas xyz.txt", header=FALSE, sep='\t', quote="", row.names=1, colClasses=rep("character", 3))

library(shiny)

ORfuncion = function(x) {
  estim = x[1,1]*x[2,2]/x[1,2]/x[2,1];
  stderr=sqrt(1/x[1,1]+1/x[2,2]+1/x[1,2]+1/x[2,1]);
  lb = exp(log(estim)-1.96*stderr)
  ub = exp(log(estim)+1.96*stderr)
  return(c(lb,estim,ub));
}

# Define server logic for slider examples
shinyServer(function(input, output) {

  .E <- reactiveValues(d = 1)  #active language and default

  tr <- function(stringId) {
        .L[stringId, .E$d]
  }

  observeEvent(input$eng, {    .E$d=1   })
  observeEvent(input$spa, {    .E$d=2   })
    
  output$titulo = renderText( { tr("titulo") } )
  output$base.t = renderText( { tr("base.t") } )
  output$z.t = renderText( { tr("z.t") } )
  output$x.t = renderText( { tr("x.t") } )
  output$y.t = renderText( { tr("y.t") } )
  output$xz.t = renderText( { tr("xz.t") } )
  output$yz.t = renderText( { tr("yz.t") } )
  output$xy.t = renderText( { tr("xy.t") } )
  output$xyz.t = renderText( { tr("xyz.t") } )
  output$par1 = renderUI( { HTML(tr("par1")) } )
  output$par2 = renderUI( { HTML(tr("par2")) } )
  output$par3 = renderUI( { HTML(tr("par3")) } )
  output$IC = renderUI( { HTML(tr("IC")) } )

# Orden  de los parámetros:
# [U, Z, Y, X, YZ, XZ, XY, XYZ]
# Parámetro de confusión: XZ
# Parámetro de modificación del efecto: XYZ
f1=c(1,  1,  1,  1,  1,  1,  1,  1)
f2=c(1,  1,  1, -1,  1, -1, -1, -1) 
f3=c(1,  1, -1,  1, -1,  1, -1, -1) 
f4=c(1,  1, -1, -1, -1, -1,  1,  1) 
f5=c(1, -1,  1,  1, -1, -1,  1, -1) 
f6=c(1, -1,  1, -1, -1,  1, -1,  1) 
f7=c(1, -1, -1,  1,  1, -1, -1,  1) 
f8=c(1, -1, -1, -1,  1,  1,  1, -1)
M=rbind(f1,f2,f3,f4,f5,f6,f7,f8)
  
  datasetInput <- reactive({

R = c(input$base, input$z, input$y, input$x, input$yz, input$xz, input$xy, input$xyz)
B = c(6.0, 4, 4, 4, 0.8, 0.8, 0.8, 1.2)
A = c(1.4,-2,-2,-2,-0.4,-0.4,-0.4,-0.6)
Par = R*B+A						
X = Par[1:8]
logcof = M%*%X
Efec = round(exp(logcof),0)
Tabla=array(Efec,dim=c(2,2,2),dimnames=list(X=c("X+", "X-"),
          Y=c("Y+", "Y-"), Z=c("Z+","Z-")))
Tabla
  })

  # Show the values 

  output$values <- renderPlot({
    T = datasetInput()
    par(mfrow=c(1,2), mar=c(1,1,1.5,0))
    mosaicplot(~Z+X+Y, data=T, shade=c(1,2), main=tr("cova"), cex=1, off=2)
    mosaicplot(~Y+X, data=T, shade=c(1,2), main=tr("jun"), cex=1, off=2)
  })

  output$tab1 <- renderTable({datasetInput()[,,1]}, digits=0)
  output$tab2 <- renderTable({datasetInput()[,,2]}, digits=0)
  output$tabxy <- renderTable({T = datasetInput(); T[,,1] + T[,,2]}, 
                              digits=0)

  output$OR1 <- renderText({t = datasetInput()[,,1]; a = round(ORfuncion(t), 2); paste('OR = ', a[2], '; ', tr("ci"), ' = [', a[1], ', ', a[3], ']', sep='')})
  output$OR2 <- renderText({t = datasetInput()[,,2]; a = round(ORfuncion(t), 2); paste('OR = ', a[2], '; ', tr("ci"), ' = [', a[1], ', ', a[3], ']', sep='')})
  output$ORxy <- renderText({T = datasetInput(); 
							  t = T[,,1] + T[,,2]; 
							  a = round(ORfuncion(t), 2); 
							  paste('OR = ', a[2], '; ', tr("ci"), ' = [', a[1], ', ', a[3], ']', sep='')
				}) 
})
