#--------------------------------------------
#  Tareas Estadistica Computacional 2016
#
#  Programa Server
#
#  Autor: Javier Quiroz
#--------------------------------------------
library(shiny)
library(dplyr)
library(plotly)
library(ggplot2)
library(markdown)

shinyServer(function(input, output) {
  

  ###  Tarea 1
  set.seed(20161212)
  
  funcion_inversa <- function(u, lambda) {
    return(-log(1-u)/lambda)
  }
  
  
  
  output$trendPlot <- renderPlotly({

    X <- entradas()
    X2 <- seq(0,max(X),(max(X)-0)/input$simula)
    funcion <- function(x) input$lambda * exp(- (x * input$lambda)) * input$simula/(input$lambda*10)
    aplicada <- sapply(X2, funcion)
    plot_ly(x=X,type="histogram", opacity=0.4, name = "Función inversa") %>%
    add_trace(x=X2, y=aplicada, type="bar", opacity=1, name = "PDF")
  })
  
  entradas <- reactive({
    nsim <- input$simula
    lambda <- input$lambda
    U <- runif(nsim)
    X <-  funcion_inversa(U, lambda)
  })
  
  
  y_calculada <- reactive({
    Y <- rexp(input$simula,rate=input$lambda)
  })
  
  
  output$text1 <- renderPrint({    
    X <- entradas()
    Y <- y_calculada()
    KS <- ks.test(X, Y)
    print(KS)
  })
  
  
  
  
  ### Tarea 2 Integracion MonteCarlo
  
  fun  <- reactive({
    texto <- paste("aux <- function(x) ",input$funcion)
    eval(parse(text = texto))
    aux
  })
  
  
  params_entrada <- reactive({
    a     <- input$a
    b     <- input$b
    N     <- input$simul
    f     <- fun()
    alphas <- input$alphas
    return(list(a,b,N,f,alphas))
  })
  
  
  calcula_integral <- reactive({
    c(a,b,N,f,alphas) := params_entrada()
    int <- integracion_mc(f,a,b,N)
    int
  })
  
  
  integracion_mc <- function(f,a,b,N){
    
    simulacion <- runif(N,a,b)
    funcion_aplicada <- f(simulacion)
    minimo <- min(funcion_aplicada)
    maximo <- max(funcion_aplicada)
    y_calculada<- runif(N,min(minimo,0),max(maximo,0))
    dist_fun <- sapply(funcion_aplicada,al_cero)
    dist_com <- sapply(y_calculada,al_cero)
    bajo_la_curva <- sum(dist_fun >= dist_com & (sign(y_calculada)==1 & sign(funcion_aplicada)==1))
    sobre_la_curva <- sum(dist_fun >= dist_com & (sign(y_calculada)==-1 & sign(funcion_aplicada)==-1))
    (bajo_la_curva - sobre_la_curva)/N * abs(max(maximo,0)-min(minimo,0))*abs(b-a)
  }
  
  
  
  al_cero <- function(x){dist(c(x,0))}
  
  
  
  confianza <- function(Numero){
    c(a,b,N,f,alphas) := params_entrada()
    uniforme <- runif(Numero, min = a, max = b)
    aplicada <- (b-a)*f(uniforme)
    media <- mean(aplicada)
    desvia <- qnorm(alphas/2, lower.tail = F) * sd(aplicada)/sqrt(Numero)
    minimo <- media - desvia
    maximo <- media + desvia
    lista <- list(media,minimo,maximo,Numero)
    df <- data.frame(lista)
    names(df) <- paste(c("media","minimo","maximo","Numero"))
    df
  }
  
  
  
  Intervalos <- reactive({
    c(a,b,N,f,alphas) := params_entrada()
    repeticiones <- seq(10,N,30)
    sapply(repeticiones,confianza, simplify = FALSE) %>%
      bind_rows()
  })
  
  output$text2 <- renderText({
    
    integral <- round(calcula_integral(), 5)
    as.character(integral)
  })
  
  output$grafica_funcion <- renderPlot({
    c(a,b,N,f,alphas) := params_entrada()
    x <- seq(a,b,(b-a)/N)
    y <- f(x)
    df <- data.frame(x,y)
    
    ggplot(df,aes(x,y))+
      geom_line(colour="#990000",size=1.5)+
      #geom_area(fill="#06b9C1",alpha=0.3)+
      geom_area(fill="#E69F00",alpha=0.3)+
      ylab('f(x)')
  })
  
  output$grafica_confianza <- renderPlot({
    c(a,b,N,f,alphas) := params_entrada()
    ggplot(Intervalos(), aes(x = Numero, y = media)) + 
      geom_ribbon(aes(ymin = minimo, ymax = maximo), 
                  alpha =0.4, fill = '#E69F00') + 
      geom_line(color = 'black', size = 0.6) + 
      ylab('Integral calculada MC') + 
      xlab("Simulaciones") #+ 
     # ggtitle("Intervalos de Confianza")
  })
  #  Funciones de soporte comun
  ':=' <- function(lhs, rhs) {
    frame <- parent.frame()
    lhs <- as.list(substitute(lhs))
    if (length(lhs) > 1)
      lhs <- lhs[-1]
    if (length(lhs) == 1) {
      do.call(`=`, list(lhs[[1]], rhs), envir=frame)
      return(invisible(NULL)) 
    }
    if (is.function(rhs) || is(rhs, 'formula'))
      rhs <- list(rhs)
    if (length(lhs) > length(rhs))
      rhs <- c(rhs, rep(list(NULL), length(lhs) - length(rhs)))
    for (i in 1:length(lhs))
      do.call(`=`, list(lhs[[i]], rhs[[i]]), envir=frame)
    return(invisible(NULL)) 
  }
  
  
  ####  tarea 3 Revolucion MCMC
  output$t3_texto = renderText( "Texto entregado en clase"  )
  
  ###   Tarea 4 Datos a usar en regresion MCMC
  output$t4_table = renderTable({
    df = read.csv("./cancer_limpios.csv")
    head(df)
  })

  ### Tarea 5 Regresion con MCMC
  output$t5_texto = renderText( "En construcción"  )
  
})

