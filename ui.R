#--------------------------------------------
#  Tareas Estadistica Computacional 2016
#
#  Interface de Usuario
#
#  Autor: Javier Quiroz
#--------------------------------------------
shinyUI(fluidPage(
  
  # Application title
  titlePanel(h3( "Estadística Computacional -- Tareas Javier Quiroz"), br() ) ,
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      conditionalPanel ( condition = "input.tabselected==1", 
                         sliderInput("simula", "Selecciona # de simulaciones:", min = 10, max = 1000, value = 350),
                         numericInput("lambda", "Parámetro lambda:", value = 0.5)  ) ,

      conditionalPanel ( condition = "input.tabselected==2",                    
                         textInput("funcion", "Función a Integrar:", value = "sin(x)"),
                         numericInput("simul", "Simulaciones:", value = 10000, min = 10,  max = 100000),
                         numericInput("a", "Límite inferior:", value = -2), 
                         numericInput("b", "Límite superior:", value = 2),
                         numericInput("alphas", "Significancia alpha :", value = 0.05) ),
      

      conditionalPanel ( condition = "input.tabselected==3", h4("Lectura de articulo MCMC Revolution") ),
      conditionalPanel ( condition = "input.tabselected==4", h4("Base de datos cáncer de mama, primeras 6 observaciones") ),
      conditionalPanel ( condition = "input.tabselected==5", h4("Regresion lineal con MCMC") )
    ),
    # Show a plot of the generated distribution
    mainPanel(
     tabsetPanel(type = "tab",
                 tabPanel("Tarea1 - Funcion Inversa", value = 1,
                          h3("Plot:"),
                          plotlyOutput("trendPlot"),
                          h2("Prueba Kolmogorov-Smirnov :"),
                          textOutput("text1")
                 ),
                 tabPanel("Tarea2 - Integracion MonteCarlo", value = 2,
                          h3("Función de Integración"),
                          plotOutput("grafica_funcion"),
                          h3("Resultado de la Integración por MonteCarlo"),
                          textOutput("text2"),
                          h3("Intervalos de confianza"),
                          plotOutput("grafica_confianza")
                  ),
                 tabPanel("Tarea3 - MCMC Revolution",  value = 3, verbatimTextOutput("t3_texto") ),
                 tabPanel("Tarea4 - Cancer DB",  value = 4, tableOutput("t4_table" )) ,
                 tabPanel("Tarea5 - MCMC", value = 5,  verbatimTextOutput("t5_texto")),
                 id = "tabselected"
                  )
     
  
     
   
    )
  )
))

