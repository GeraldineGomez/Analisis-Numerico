
Tarea 1: Raíces

1. Método de Bisección 


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


- Gráfica Método bisección 


![metodo_biseccion](https://user-images.githubusercontent.com/46997659/52247924-e5bde900-28b9-11e9-9980-5f0a6189a55b.png)



2. Método del Punto Fijo


             rm(list=ls())
             Fx <- function(x) exp(x) - x*pi
             Gx <- function(x) exp(x) / pi

             puntoFijo <- function(a,b) 
             {

             x<-(a+b)/2
             i<-0

             while (Gx(x) != x ) 
             {    
                 
                 error<-abs(a-b)/2
                  
                  if(error > 1.e-8)
                     if (Gx(x) < x) b <- x 
                     else {a <- x}
                  else {break}  

                  x<-(a+b)/2
                  
                  i<-i+1
                  cat("I=",i,"\tG(x) =",Gx(x),"\tX=",signif(x, digits = 8),"\tE=",error,"\n")
             }


             }

             puntoFijo(-3,1)


3. Aplicaciones

En La dinámica de fluidos computacional, es necesario utilizar métodos de discretización, para resolver problemas de flujos multifásicos, reactivos o turbulentos, para lo cual en la mayoria de ocasiones es usado el método de volumenes finitos, en donde se intercambia el dominio continuo por un dominio discreto, este conjunto de volúmenes de control es utilizado para representar el dominio original, generando un ecuación de conservación para cada dominio de control.

La forma algebraica de la ecuación, es resuelta en cada uno de los volúmenes de control, generando un sistema de ecuaciones que debe ser resuelto numéricamente. El número de volúmenes de control a ser usado para el análisis debe ser establecido por medio del estudio de convergencia de malla, o sea, se analiza el mismo caso con mallas de refinamientos diferentes y se comparan los resultados. Cuando el resultado entre dos refinamientos de malla no sufre una gran alteración, se dice que la convergencia de malla fue alcanzada.

- Malla de Dominios de Control

 ![dominio](https://user-images.githubusercontent.com/46997659/52270826-0c0e7380-2910-11e9-9a4d-6b953ebb98c5.jpg)






