"
Tarea 1: Raices

1. Metodo de Biseccion 


"

rm(list=ls())
Fx <- function(x) exp(x) - x*pi

biseccion <- function(a,b) 
{
x<-seq(a,b,0.01)
plot(x,Fx(x),type="l",col="red")
abline(h=0,col="blue")


x <- (a+b)/2
i <- 0

while (Fx(x) != 0 ) 
{


   error<-abs(a-b)/2
   if(error >= 1.e-8)
     if (Fx(x)*Fx(a) < 0) b <- x 
       else {a <- x}
    else {break}
  
   x<-(a+b)/2
      
   text(x,0,i,cex=0.8,col="blue")
     
   i<-i+1
     
   cat("I=",i,"\tX=",x,"\tE=",error,"\n")

}

}
biseccion(-3,1)




- Grafica Metodo biseccion 


![metodo_biseccion](https://user-images.githubusercontent.com/46997659/52247924-e5bde900-28b9-11e9-9980-5f0a6189a55b.png)







