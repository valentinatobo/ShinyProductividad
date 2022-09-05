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

#Cargar script de nube de palabras
source("nubeDePalabras.R")

#Cargar script de red de coocurrencias
source("redDeCoocurrencias.R")

#Cargar script de torta de productividad por areas
source("tortaDeAreas.R")

#Cargar script de barras para articulos por tipo de revista
source("barrasRevistas.R")

####################################
# Interfaz de usuario              #
####################################

ui <- fluidPage(
  
  #Tema a utilizar
  theme = shinytheme("flatly"),
  
  navbarPage(
    
    #Titulo de la barra de navegacion
    "Productividad",
    
    tabPanel(
      "Información General",
      br(),
      
      fluidRow(
        summaryBox3("Total de Productos", "3.499", width = 12, icon = "fas fa-chart-pie", style = "primary")     
                
      ),
      br(),
      fluidRow(
        summaryBox3("Generación de Nuevo Conocimiento (GNC)", "2.127", width = 6, style = "primary"),        
        summaryBox3("Desarrollo Tecnológico e Innovación (DTI)", "134", width = 6, style = "primary"),        
      ),
      br(),br(),
      fluidRow(
               
        summaryBox3("Apropiación Social de Conocimiento (ASC)", "179", width = 6, style = "primary"),        
        summaryBox3("Formación Recurso Humano (FRH)", "1.059", width = 6, style = "primary"),        
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
                    choices = list(
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
                    selected = "GIIRA"),
        
      ),
      
      #Panel principal de graficacion
      mainPanel(
        
        #Espacio para red de coocurrencia
        h3("Red de Co-ocurrencia en títulos (Toda la prod. académica)"),
        plotOutput("redDeCoocurrencia"),
        hr(),
        
        #Fila para graficas de torta y nube de palabras
        fluidRow(
          
          #Columna para la nube de palabras
          column(
            6,
            #Espacio para la nube de palabras
            h3("Nube de palabras (Títulos de toda la prod. académica)"),
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
            h4("Articulos pub. en revistas nacionales (Publindex)"),
            plotOutput("barrasRevNac")
            
          ),
          
          #Espacio para grafica de revistas internacionales
          column(
            #Tamaño
            6,
            h4("Articulos pub. en revistas internacionales (SJR/JCR)"),
            plotOutput("barrasRevInt")
          )
        ),
      ) #Cierre del panel principal
      
      
      ) #Cierre pestaña de graficas
    ), #Cierre de la barra de navegacion
) #Cierre de la UI

####################################
# Servidor                         #
####################################

server <- function(input, output) {
  
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
  output$nubeDePalabras <- renderPlot({
    #Llamar al script correspondiente
    generarNubeDePalabrasPorGrupo(input$grupo)
  })
  
  output$redDeCoocurrencia <- renderPlot({
    #Llamar al script correspondiente
    generarRedDeCoocurrenciasPorGrupo(input$grupo)
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
  
}

####################################
# Crear la App. en Shiny           #
####################################
shinyApp(ui = ui, server = server)