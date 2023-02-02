# App de Graficas de Productividad usando R y Shiny
# https://github.com/deepanshu88/summaryBox

# Importar librerias
library(shiny)
library(shinythemes)
library(data.table)
library(RCurl)
library(readr)
library(dplyr)
library(e1071)
library(mlbench)
library(readxl)
library("stopwords")
library(tidyverse)
library(tidytext)
library(tm)
library(SnowballC)
library("wordcloud")
library("RColorBrewer")
library(summaryBox)
library(r2r)
library(udunits2)
library(r2d3)
library(jsonlite)


#Cargar script de nube de palabras
source("nubeDePalabras.R")

#Cargar script de red de coocurrencias
source("redDeCoocurrencias.R")

#Cargar script de torta de productividad por areas
source("tortaDeAreas.R")

#Cargar script de barras para articulos por tipo de revista
source("barrasRevistas.R")

#Cargar script de nube de palabras
source("dataparaGraficos.R")



####################################
# Interfaz de usuario              #
####################################

ui <- fluidPage(
  #Archivo de Estilos Ubicado en Carpeta WWW
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "style.css")),
  
  #Version de D3 que Shiny reconoce
  #tags$script(src='//d3js.org/d3.v3.min.js'),
  
  #Tema a utilizar
  theme = shinytheme("flatly"),

  navbarPage(
    
    #Titulo de la barra de navegacion
    "Productividad",
    
    tabPanel(
      "Información General",
      br(),
      
      fluidRow(
        summaryBox3("Total de Productos", totalProductos, width = 12, icon = "fas fa-chart-pie", style = "primary")     
                
      ),
      br(),
      fluidRow(
        summaryBox3("Generación de Nuevo Conocimiento (GNC)", nGNC, width = 6, style = "primary"),        
        summaryBox3("Desarrollo Tecnológico e Innovación (DTI)", nDTI, width = 6, style = "primary"),        
      ),
      br(),br(),
      fluidRow(
               
        summaryBox3("Apropiación Social de Conocimiento (ASC)", nASC, width = 6, style = "primary"),        
        summaryBox3("Formación Recurso Humano (FRH)", nFRH, width = 6, style = "primary"),        
      ),
      hr(),
      #Fila
      fluidRow(
        
        #Espacio para grafica de GNC Articulos
        column(
          
          #Tamaño
          4,
          plotOutput("GNCBarras")
          
        ),
        
        #Espacio para grafica de GNC Libros
        column(
          
          #Tamaño
          4,
          plotOutput("GNCLibros")
          
        ),
        
        #Espacio para grafica de DTI Software
        column(
          
          #Tamaño
          4,
          plotOutput("DTISoftware")
          
        )
        
        
      ),
      hr(),
      fluidRow(
        
        #Espacio para grafica de DTI Software
        column(
          
          #Tamaño
          6,
          plotOutput("ASCLibros")
          
        ), 
        #Espacio para grafica de FRH Trabajos
        column(
          
          #Tamaño
          6,
          plotOutput("FRHTrabajos")
          
        )
        
      ),
      hr(), hr(),
      
      
      
    ),
    
    #Pestaña de graficas
    tabPanel(
      
      #Titulo de la pestaña
      "Análisis de Relaciones",
      
      #Panel lateral izquierdo
      sidebarPanel(
        
        #Selector de grupo
        selectInput("grupo", label = "Grupo de Investigación:", 
                    choices = c("",
                      "GIIRA",
                      "LAMIC",
                      "IDEAS",
                      "INTECSE",
                      "ARCOSES",
                      "ARQUISOFT",
                      "COMPLEXUD",
                      "DIMSI",
                      "GCEM",
                      "GEFEM",
                      "GEIT",
                      "GESDATOS",
                      "GESETIC",
                      "GICOECOL",
                      "GICOGE",
                      "GISE 3",
                      "GITEM",
                      "GITUD",
                      "GRECO",
                      "INTERNET INTELIGENTE",
                      "INVID",
                      "LASER",
                      "LIDER",
                      "LIFAE",
                      "MULTIMEDIA INTERACTIVA",
                      "NIDE",
                      "THRISCUD",
                      "VIRTUS",
                      "GICALYT"), 
                    selected = ""),
        
      ),
      
      #Panel principal de graficacion
      mainPanel(
        
        #Espacio para red de coocurrencia
        h3("Red de Co-ocurrencia en Títulos (GNC - DTI - ASC - FRH)"),
        plotOutput("redDeCoocurrencia"),
        hr(),
        
        #Fila para graficas de torta y nube de palabras
        fluidRow(
          
          #Columna para la nube de palabras
          column(
            6,
            #Espacio para la nube de palabras
            h3("Nube de palabras en Títulos (GNC - DTI - ASC - FRH)"),
            plotOutput("nubeDePalabras"),
            ),
          
          #Columna para la torta de productividad
          column(
            6,
            #Espacio para la torta de productividad
            h3("Productividad academica por área"),
            plotOutput("tortaDeAreasArticulos"),
          ),
        ),
        hr(),
        
        #Fila para graficas de barras de revistas
        fluidRow(
          
          #Espacio para grafica de revistas nacionales
          column(
            #Tamaño
            6,
            h4("Publicaciones (GNC) en revistas nacionales (Publindex)"),
            plotOutput("barrasRevNac")
            
          ),
          
          #Espacio para grafica de revistas internacionales
          column(
            #Tamaño
            6,
            h4("Publicaciones (GNC) en revistas internacionales (SJR/JCR)"),
            plotOutput("barrasRevInt")
          )
        ),
        
        #Fila para las primeras 2 tortas (Articulos, Libros)
        fluidRow(
          
          #Espacio para grafica de Articulos
          column(
            #Tamaño
            6,
            h4("GNC - Publicaciones en revistas cientificas"),
            plotOutput("tortaGrupoArt")
            
          ),
          
          #Espacio para grafica de revistas internacionales
          column(
            #Tamaño
            6,
            h4("GNC - Libros de Investigación"),
            plotOutput("tortaGrupoLib")
          ), 
          
           
        ), hr(),
        
        #Fila para las siguientes 2 tortas (Sofrware, Capitulos)
        
        fluidRow(
          #Espacio para grafica de software
          column(
            #Tamaño
            6,
            h4("DTI - Software"),
            plotOutput("tortaGrupoSof")
          ),
          
          #Espacio para grafica de software
          column(
            #Tamaño
            6,
            h4("ASC - Capítulos de libros en eventos científicos"),
            plotOutput("tortaGrupoCap")
          ),
          
          #Fila para las siguiente torta (Trabajos)
          
          fluidRow(
            #Espacio para grafica de software
            column(
              #Tamaño
              12,
              h4("FRH - Proyectos de Grado, Tesis de Maestría y Tesis de Doctorado"),
              plotOutput("tortaGrupoTra")
            ),
          ),
        ),
      ) #Cierre del panel principal
      ),
    #Pestaña de graficas de forma general
    tabPanel(
      #Titulo de la pestaña
      "Análisis de Relaciones",
      # sidebarPanel(
      #   # selectInput(inputId = "condicionGrafico",
      #   #             label = 'Graficos',
      #   #             choices = c("")),
      #   
      #   # Show a plot of the generated distribution
      # ),
      mainPanel(
          column(
            12,
            #Espacio para la nube de palabras
            h3("Articulos Publicados en Paises"),
            plotlyOutput("Paises"),
          )
        )
      
      
    ) #Cierre pestaña de graficas
  ), #Cierre de la barra de navegacion
) #Cierre de la UI

####################################
# Servidor                         #
####################################

server <- function(input, output, session) {
  
  #1ra pestaña
  output$GNCBarras <- renderPlot({
    #Llamar al script correspondiente
    generarBarraArticulosDash()
  })
  
  output$GNCLibros <- renderPlot({
    #Llamar al script correspondiente
    generarBarraLibrosDash()
  })
  
  output$DTISoftware <- renderPlot({
    #Llamar al script correspondiente
    generarBarraSoftwareDash()
  })
  
  output$ASCLibros <- renderPlot({
    #Llamar al script correspondiente
    generarBarraCapitulosDash()
  })
  
  output$FRHTrabajos <- renderPlot({
    #Llamar al script correspondiente
    generarBarraTrabajosDash()
  })
  
  #FRHTrabajos
  #2da pestaña
  output$redDeCoocurrencia <- renderPlot({
    #Llamar al script correspondiente
    generarRedDeCoocurrenciasPorGrupo(input$grupo)
  })
  
  output$nubeDePalabras <- renderPlot({
    #Llamar al script correspondiente
    generarNubeDePalabrasPorGrupo(input$grupo)
  })
  
  output$tortaDeAreasArticulos <- renderPlot({
    #Llamar al script correspondiente
    generarTortaPorAreaArticulos(input$grupo)
  })
  
  output$tortaDeAreasArticulos <- renderPlot({
    #Llamar al script correspondiente
    generarTortaPorAreaArticulos(input$grupo)
  })
  
  output$barrasRevNac <- renderPlot({
    #Llamar al script correspondiente
    generarBarraArticulosRevista(input$grupo, 10)
  })
  
  output$barrasRevInt <- renderPlot({
    #Llamar al script correspondiente
    generarBarraArticulosRevista(input$grupo, 11)
  })
  
  output$tortaGrupoArt <- renderPlot({
    #Llamar al script correspondiente
    generarTortaPorGrupoArticulos(input$grupo)
  })
  
  output$tortaGrupoLib <- renderPlot({
    #Llamar al script correspondiente
    generarTortaPorGrupoLibros(input$grupo)
  })
  
  output$tortaGrupoSof <- renderPlot({
    #Llamar al script correspondiente
    generarTortaPorGrupoSoftware(input$grupo)
  })
  
  output$tortaGrupoCap <- renderPlot({
    #Llamar al script correspondiente
    generarTortaPorGrupoCapitulos(input$grupo)
  })
  
  output$tortaGrupoTra <- renderPlot({
    #Llamar al script correspondiente
    generarTortaPorGrupoTrabajos(input$grupo)
  })
  
  #Tercera Pestaña
  output$Paises <- renderPlotly(
    generarGraficaPaises()
  )
}



####################################
# Crear la App. en Shiny           #
####################################
shinyApp(ui = ui, server = server)