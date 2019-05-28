#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
if (interactive()) {
  
ui <- fluidPage(
   
   # Application title
   titlePanel("Temperatura cadaverica"),
   
   # Sidebar with a slider input for number of bins 
     sidebarLayout(
      sidebarPanel(
        radioButtons("Grados",
                     "Unidad de medida:",
                     choices=list("Celcius"=1,"Fahrenheit"=2,"Kelvin"=3),
                     selected=1),
        sliderInput("M",
                    "Temperatura ambiente:",
                    min = 0,
                    max = 30,
                    value = 15,
                    step=0.1),
         sliderInput("T0",
                     "Temperatura inicial:",
                     min = 1,
                     max = 50,
                     value = 25,
                     step=0.1),
         sliderInput("T1",
                     "Temperatura final:",
                     min = 1,
                     max = 50,
                     value = 20,
                     step = 0.1),
         sliderInput("t0", 
                    "Tiempo inicial:", 
                    min = 0,
                    max = 24,
                    value = 9,
                    step = 0.5),
        sliderInput("t1", 
                    "Tiempo final:", 
                    min = 0,
                    max = 24,
                    value = 12,
                    step = 0.5),
        radioButtons("Seleccion",
                     "Tipo de muerte:",
                     choices=list("Hipertermia"=1,"Normal"=2,"Hipotermia"=3),
                     selected=2),
        actionButton("graf", "Graficar")
      ),
      # Show a plot of the generated distribution
      mainPanel(
         
        plotOutput("distPlot"),
        textOutput("Total")
      )
   )
)

server <- function(input, output, session) {
  observe({
  v <- reactiveValues(doPlot = FALSE)
  
  observeEvent(input$graf, {
    # 0 will be coerced to FALSE
    # 1+ will be coerced to TRUE
    v$doPlot <- input$graf
  })
  if(input$Grados==1){
    updateSliderInput(session, "M",
                      min=0,
                      max=40,
                      value=20,
                      step=0.1)
    updateSliderInput(session, "T0",
                      min=0,
                      max=50,
                      value=30,
                      step=0.1)
    updateSliderInput(session, "T1",
                      min=0,
                      max=50,
                      value=25,
                      step=0.1)
  }
  if(input$Grados==2){
  updateSliderInput(session, "M",
                    min=32,
                    max=104,
                    value=68,
                    step=0.1)
  updateSliderInput(session, "T0",
                      min=32,
                      max=122,
                      value=86,
                      step=0.1)
  updateSliderInput(session, "T1",
                    min=32,
                    max=122,
                    value=77,
                    step=0.1)
  }
  if(input$Grados==3){
    updateSliderInput(session, "M",
                      min=273.15,
                      max=313.15,
                      value=293.15,
                      step=1)
    updateSliderInput(session, "T0",
                      min=273.15,
                      max=323.15,
                      value=303.15,
                      step=1)
    updateSliderInput(session, "T1",
                      min=273.15,
                      max=323.15,
                      value=298.15,
                      step=1)
  }
  output$distPlot <- renderPlot({
      if(v$doPlot==FALSE) return()
    
      tem<-input$t1-input$t0
      if(input$Grados==1){
        if(input$Seleccion==2){
          normal=37
          if(input$T0>normal){
            return()
          }
        }
        if(input$Seleccion==1){
          normal=42
          if(input$T0>normal){
            return()
          }
        }
        if(input$Seleccion==3){
          normal=16
          if(input$T0>normal){
            return()
          }
        }
      }
      if(input$Grados==2){
        if(input$Seleccion==2){
          normal=98.6
          if(input$T0>normal){
            return()
          }
        }
        if(input$Seleccion==1){
          normal=107.6
          if(input$T0>normal){
            return()
          }
        }
        if(input$Seleccion==3){
          normal=60.8
          if(input$T0>normal){
            return()
          }
        }
      }
      if(input$Grados==3){
        if(input$Seleccion==2){
          normal=310.15
          if(input$T0>normal){
            return()
          }
        }
        if(input$Seleccion==1){
          normal=315.15
          if(input$T0>normal){
            return()
          }
        }
        if(input$Seleccion==3){
          normal=289.15
          if(input$T0>normal){
            return()
          }
        }
      }
      Graficar(0,tem,input$M,normal,input$T0,input$T1)
  })
  output$Total<-renderText({
    if(v$doPlot==FALSE) return()
    
    if(input$Grados==1){
      if(input$Seleccion==2){
        normal=37
        if(input$T0>normal){
          return()
        }
      }
      if(input$Seleccion==1){
        normal=42
        if(input$T0>normal){
          return()
        }
      }
      if(input$Seleccion==3){
        normal=16
        if(input$T0>normal){
          return()
        }
      }
    }
    if(input$Grados==2){
      if(input$Seleccion==2){
        normal=98.6
        if(input$T0>normal){
          return()
        }
      }
      if(input$Seleccion==1){
        normal=107.6
        if(input$T0>normal){
          return()
        }
      }
      if(input$Seleccion==3){
        normal=60.8
        if(input$T0>normal){
          return()
        }
      }
    }
    if(input$Grados==3){
      if(input$Seleccion==2){
        normal=310.15
        if(input$T0>normal){
          return()
        }
      }
      if(input$Seleccion==1){
        normal=315.15
        if(input$T0>normal){
          return()
        }
      }
      if(input$Seleccion==3){
        normal=289.15
        if(input$T0>normal){
          return()
        }
      }
    }
    tem<-input$t1-input$t0
    tem2<-CalcularTiempo(0,tem,input$M,normal,input$T0,input$T1)
    options(digits=4)
    hora=input$t0-tem2
    horam=floor(hora)
    minum=round(hora,2)-horam
    minu=(minum)*60
    paste("La hora de muerte de la persona aproximadamente fue a las ",horam," horas con ",floor(minu)," minutos.")
  })
  })
}

Graficar<-function(t0,t1,M,normal,T0,T1){
  fp<-function(temp,t){
    s=(temp-M)/exp(t)
    return(s)
  }
  
  fp2<-function(temp,t,h){
    k=-log((temp-M)/h)/t
    return(k)
  }
  
  fp3<-function(k,temp,parms){
    s=k*(M-temp)
    return(list(s))
  }
  
  tiempo<-function(h,k){
    tem=log((normal-M)/h)/log(exp(k))
    return(tem)
  }
  
  
  h<-fp(T0,t0)
  h
  
  k<-fp2(T1,t1,h)
  k
  
  tem<-tiempo(h,k)
  tem
  tis=seq(0,3,0.01)
  require(deSolve)
  sol=ode(c(k,T0),tis,fp3,parms=NULL,method="rk4")
  sol
  puntos=c(normal,sol[,3])
  puntos
  tem2=-tem
  puntost=c(tem2,tis)
  puntost
  require(splines)
  suavizado.datos<-smooth.spline(puntos~puntost,cv=TRUE)
  plot(puntost,suavizado.datos$y,xlab="tiempo (horas)",ylab="temperatura",main="Enfriamiento",col="white")
  par(new=T)
  lines(puntost,suavizado.datos$y,xlab="tiempo (horas)",ylab="temperatura",main="Enfriamiento",col="blue")
}

CalcularTiempo<-function(t0,t1,M,normal,T0,T1){
  fp<-function(temp,t){
    s=(temp-M)/exp(t)
    return(s)
  }
  
  fp2<-function(temp,t,h){
    k=-log((temp-M)/h)/t
    return(k)
  }
  
  fp3<-function(k,temp,parms){
    s=k*(M-temp)
    return(list(s))
  }
  
  tiempo<-function(h,k){
    tem=log((normal-M)/h)/log(exp(k))
    return(tem)
  }
  h<-fp(T0,t0)
  
  k<-fp2(T1,t1,h)
  
  tem<-tiempo(h,k)
  return(tem)
}

# Run the application 
shinyApp(ui, server)

}