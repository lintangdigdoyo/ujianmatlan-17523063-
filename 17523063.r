#No1

xj <- 50:54 # dapet dri nilai x di tabel
yj <- c(40,46,44,55,49) # dapet dari nilai y di tabel
n <- 5

spl <- matrix(c(n,sum(xj),
                sum(xj),sum(xj^2)),2,2,TRUE)

jwb <- c(sum(yj),sum(yj*xj))

solve(spl)%*%jwb

#No2 
y <- function(x){
  hasil <- (-93.6 +( 2.7*x))
}
print (y(55))

#No3
xi <- 0:4 
yi <- c(1,2.25,3.75,4.25,5.65) 

data <- data.frame(cbind(xi,yi))
poly.calc(xi,yi)


#No5
f <- function(xi,yi){
  hasil<- 1-0.0791667*xi+2.19375*xi^2-0.9958333*xi^3+0.13125*xi^4
  return(hasil)
}
plot(xi,yi)
curve(f, add = TRUE)

##########

fx <- function(x) {
  return((a+b)/2)
}

bi <- function(a, b) {
  
  #initialize arbritrary large relative error
  re <- 3
  
  #compute the first pn
  pn <- (a + b) / 2
  
  
  
  while(re >= 0.0001) {
    
    
    #print to monitor: a, b, pn, f(pn), and re
    print(paste(a, b, pn, fx(pn), re, sep=" "))
    
    #previous p
    p <- pn
    
    #bisection step to halve interval
    if (sign(fx(p)) == sign(fx(a))) {
      a <- p
    } else {
      b <- p
    }
    
    
    #current p
    pn <- (a + b) / 2
    
    #current relative error
    re <- abs(pn-p) / abs(pn)
  }
  
  #print the final solution
  print(paste(a, b, pn, fx(pn), re, sep=" "))
}

####


#No11
library(pracma)

f <- function(x){
  hasil <- x^2 - 6
  return(hasil)
}

trapzfun(f,0,1)


#No12
j <- function(x){
  hasil <- (x^3 + 4*(x^2) -10)
  return(hasil)
}

trapzfun(j,1,2)

#####
h <- 0.1
x <- seq(0,1,by=h)
f <- function(x){
  return(x^2)
}

f0 <- f(x[1])
fi <- sapply(x[2:10], f)
fn <- f(x[length(x)])

trap <- function(f0,fi,fn,h){
L <- h/2 * (f0 + 2 * sum(fi)-1 +fn)
  return(L)
}
trap(f0,fi,fn,h) 




