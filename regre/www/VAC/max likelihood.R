library(alabama)

fn <- function(x, var, ...) {
       datos = var$datos[var$datos$m>0,]
	   y = (log(x[1] + (datos$a*x[2] + datos$b*x[3] + datos$c*x[4])/10))* datos$m
	   -sum(y)
	}

gr <- function(x, var, ...) {
    datos = var$datos
	g <- rep(0, 4)
    u = -(datos$m / (x[1] + 0.1*(datos$a*x[2] + datos$b*x[3] + datos$c*x[4])))
	g[1] = sum(u)
	g[2] = sum(datos$a*u)/10
	g[3] = sum(datos$b*u)/10
	g[4] = sum(datos$c*u)/10
	g
}

 # FunX = function(x) { 1/60*sqrt(x)*(3*v$p*x^1.5+4*v$q*x+12*v$r) - 1/60*sqrt(ax)*(3*v$p*ax^1.5+4*v$q*ax+12*v$r) + v$d*(x-ax)}

heq <- function(x, var, ...) {
    limi = var$limi
	h <- rep(0, 1)
	ax = limi[1]
	z = limi[2]
	h[1] <- 1/60*sqrt(z)*(3*x[2]*z^1.5+4*x[3]*z+12*x[4]) - 1/60*sqrt(ax)*(3*x[2]*ax^1.5+4*x[3]*ax+12*x[4]) + x[1]*(z-ax) - 1
	h
}
heq.jac <- function(x, var, ...) {
    limi = var$limi
	j <- matrix(0, 1, length(x))
	ax = limi[1]
	z = limi[2]
	j[1, ] <- c(z-ax, 1/20*(z*z-ax*ax), 1/15*(z^1.5-ax^1.5), 1/5*(sqrt(z)-sqrt(ax)))
	j
}
hin <- function(x, var, ...) {
	bounds = var$bounds
	lim = var$limi; limx = sqrt(lim)
    datos = rbind(var$datos, c(lim[1], limx[1], 1/limx[1], NA), c(lim[2], limx[2], 1/limx[2], NA))
    m = 8+dim(datos)[1]
	h <- rep(0, m)
	h[1:4] <- x[1:4]-bounds[,1]
	h[5:8] <- bounds[,2]-x[1:4]
	h[9:m] <- x[1] + (x[2]*datos$a + x[3]*datos$b + x[4]*datos$c)/10 - 1e-5
	h
}
hin.jac <- function(x, var, ...) {
	bounds = var$bounds
	lim = var$limi; limx = sqrt(lim)
    datos = rbind(var$datos, c(lim[1], limx[1], 1/limx[1], NA), c(lim[2], limx[2], 1/limx[2], NA))
    m = 8+dim(datos)[1]
	j <- matrix(0, m, length(x))
	j[1:4,1:4] <- diag(rep(1,4))
	j[5:8,1:4] <- -diag(rep(1,4))
	j[9:m, 1] <- 1
	j[9:m, 2] <- 0.1*datos$a
	j[9:m, 3] <- 0.1*datos$b
	j[9:m, 4] <- 0.1*datos$c
	j
}
