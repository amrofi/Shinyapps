A = 10
M = 102000
MM = round(1.2*M)

populate = function(W) {
    x0 = W$x; y0 = W$y; r0 = W$r; N = W$N
    W$S = list()
    u = runif(A)
    n = round(N * u/sum(u))    #  sum n puede ser != N
    i = 0
    for (k in 1:A) {
      if (n[k] > 15) {
        aux = r0*r0; r = sqrt(runif(1, aux/6, aux))
        a = runif(1, 0, 2*pi)
        x = x0+r*cos(a)
        y = y0+r*sin(a)
        q = r0*sqrt(n[k]/N)   
        link = list(x=x, y=y, r=q, N=n[k]) 
        i = i+1
        W$S[[i]] = populate(link)
      } else if (n[k]>0) {
        J = (I+1):(I+n[k])
        i = i+1
        W$S[[i]] = J
        pop$x[J] <<- rnorm(n[k], W$x, W$r)
        pop$y[J] <<- rnorm(n[k], W$y, W$r)
        I <<- I+n[k]
      }
    }
    W
}

pop = data.frame(x=array(NA, dim=MM), y=array(NA, dim=MM), v=array(NA, dim=MM))
I = 0
Z = list( x=0, y=0, r=10, N=M )
Q = populate(Z)

# asignar un voto a cada individuo, recorriendo todo el árbol

voto_disc = function(W) {
    p = W$P
    m = length(W$S)
    for (i in 1:m) {
       J = W$S[[i]]
       if (class(J)!='list') {
          pop$v[J] <<- rbinom(length(J), 1, p)
       } else {
          y = rnorm(1, p, p*(1-p))
          J$P = ifelse(y<0, 0, ifelse(y>1, 1, y))
          W$S[[i]] = voto_disc(J)
       }
    }
    W
}

Q$P = runif(1, 0.1, .9)
D = voto_disc(Q)
pop = pop[1:I,]
save(pop, file='ShinyApps/polling/pop.Rdata')
# no hace falta guardar nada más
