library(polynom)
library(PolynomF)
x=c(14.65, 14.71, 14.6, 14.8, 15.2, 15.6, 15.7, 17.0, 17.6, 17.52, 17.3, 16.8, 15.4, 14.83, 14.4, 14.5, 15.0, 15.1, 15.0, 14.9, 14.6, 14.3, 14.0, 13.9, 13.8, 13.5, 13.1, 13.0, 13.3, 13.2, 13.1, 12.9, 12.4, 11.9, 11.7, 11.6, 11.3, 10.9, 10.7, 10.6, 10.6, 10.1, 9.7, 9.4, 9.3, 9.6, 9.9, 10.1, 10.2, 10.3, 9.10, 8.6, 7.5, 7.0, 6.7, 6.6, 7.70, 8.00, 8.10, 8.40,9.20, 9.30, 10, 10.2, 10.3, 10.0, 9.50)                                                                                                       
y=c(14.7, 14.33, 13.4, 12.33, 11.0, 10.5, 10.22, 8.20, 7.10, 6.70, 6.60, 6.80, 8.30, 8.80, 9.30, 8.80, 6.30, 5.50, 5.00, 4.70, 4.60, 4.50, 4.90, 5.40, 5.80, 6.90, 8.20, 7.60, 5.80, 4.50, 4.30, 3.90, 4.20, 5.70, 7.00, 7.90, 8.20, 7.30, 6.70, 5.50, 5.10, 4.60, 4.7, 5.0, 5.5, 7.2, 7.8, 8.60, 9.40, 10.0, 10.7, 9.9, 9.0, 9.1, 9.3, 9.7, 11.7, 12.3, 12.5, 13.0,13.91, 14.9, 16, 16.4, 16.8, 10.7, 11.0)     
length(x)
plot(x,y, pch=19, cex=0.5, col = "red", asp=1,xlab="X", ylab="Y", main="Mano ")
#puntos
x2=c(14.65, 15.2, 17.0, 17.52, 16.8, 14.4, 15.0, 14.6, 13.9, 13.1, 13.3, 
     12.9, 11.9, 11.3, 10.6, 10.1, 9.3, 10.0, 8.6, 7.0, 6.6, 8.10, 10.3)
y2=c(14.7, 11.0, 8.20, 6.70, 6.80, 9.30, 6.30, 4.60, 5.40, 8.20, 5.80,
     3.90, 5.70, 8.20, 5.50, 4.60, 5.50, 10.7, 9.9, 9.1, 9.7, 12.5, 16.8)
length(x2)
par(new=T)
plot(x2,y2, pch=19, cex=0.9, col = "Black", asp=1,xlab="X", ylab="Y", main="Mano ")

cubicSpline<- function(x,y){
  a=rep(y)
  n=length(x)
  h<-(c(x,0)-c(0,x))[2:n]
  alph<-(3/c(1,h,1,1)*(c(a,1,1) - c(1,a,1)) - 3/c(1,1,h,1)*(c(1,a,1)-c(1,1,a)))[3:n]
  A <- c(1,rep(0,times=n-1))
  for (i in 1:(n-2)) {
    A <- rbind(A,c( rep(0,times=i-1) , c(h[i],2*(h[i]+h[i+1]),h[i+1]) , rep(0,times=n-i-2) ) )
  }
  A <- rbind(A,c(rep(0,times=n-1),1))
  b <- c(0,alph,0)
  c <- solve(A, b)
  b <- ((c(a,0) - c(0,a))/c(1,h,1) - c(1,h,1)/3*(c(c,0) + 2*c(0,c)))[2:n]
  d <- ((c(c,0) - c(0,c))/(3*c(1,h,1)))[2:n]
  ans = rbind(a[1:n-1],b,c[1:n-1],d)
}

draw<-function(x,y){
  t = 1:length(x)
  sx = cubicSpline(t,x)
  sy = cubicSpline(t,y)
  for (i in 1:(length(t)-1)) {
    dat<- data.frame(t=seq(t[i],t[i+1], by=0.1) )
    fx <- function(x) (sx[1,i] + sx[2,i]*(x-t[i]) + sx[3,i]*(x-t[i])^2 + sx[4,i]*(x-t[i])^3)
    fy <- function(x) (sy[1,i] + sy[2,i]*(x-t[i]) + sy[3,i]*(x-t[i])^2 + sy[4,i]*(x-t[i])^3)
    dat$y=fy(dat$t)
    dat$x=fx(dat$t)
    points(dat$x,dat$y,type='l', col='purple',lwd=3)
  }
}
draw2<-function(x,y){
  t = 1:length(x)
  sx = cubicSpline(t,x)
  sy = cubicSpline(t,y)
  for (i in 1:(length(t)-1)) {
    dat<- data.frame(t=seq(t[i],t[i+1], by=0.1) )
    fx <- function(x) (sx[1,i] + sx[2,i]*(x-t[i]) + sx[3,i]*(x-t[i])^2 + sx[4,i]*(x-t[i])^3)
    fy <- function(x) (sy[1,i] + sy[2,i]*(x-t[i]) + sy[3,i]*(x-t[i])^2 + sy[4,i]*(x-t[i])^3)
    dat$y=fy(dat$t)
    dat$x=fx(dat$t)
    points(dat$x,dat$y,type="l", col='orange')
  }
}
draw(x2,y2)
draw2(x,y)