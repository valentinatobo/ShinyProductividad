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
      "Análisis General",
      
      #mainPanel(
        fluidRow(
          #Espacio para Grafico Cartograma
          h4("Países donde más se publican"),
          plotlyOutput("Paises"),
          hr(),
        ),
        
        fluidRow(
          column(
            6,
            #Grafico de Revistas (Dividir en nacional E internacional)
            h4("Revistas donde más se publica"),
            plotlyOutput("Revistas"),
          ),
          
          #Columna para la torta de productividad
          column(
            6,
            h4("Revistas con mejores cuartiles (Q1 & Q2)"),
            plotlyOutput("RevistasCuartil"),
          ),
        ),
        hr(),
      
        fluidRow(
          column(
            #Tamaño
            6,
            h4("Áreas de investigación donde más se publica"),
            plotlyOutput("areasPlus"),
          ),
          
          column(
            #Tamaño
            6,
            h4("Áreas de investigación donde más se publica en revisatas Q1 & Q2"),
            plotlyOutput("areaCuartilPlus")
          )
        ),
        
        fluidRow(
          
          #Espacio para grafica 
          column(
            #Tamaño
            12,
            h4("Países con mayor índice de cuartil de revistas (Q1 & Q2)"),
            plotlyOutput("paisCuartil")
            
          )
        ),
      fluidRow(
        column(
          #Tamaño
          12,
          h4("Países donde más se publica según Área de Investigación"),
          plotlyOutput("areaPais")
        ),
      ), hr()
      #), #Cierre del panel principal (Este Panel se usa cuando tendremos Lateral, de resto es ideal no usarlo)
    ),
    tabPanel(
      #Titulo de la Pestaña
      "Analisis de Grupos",
      #Grupos donde mas se publica por Pais
      fluidRow(
        column(
          12,
          h4("Grupos donde mas se Publica por Pais"),
          plotlyOutput("gruposPais"),
        ),
      ),
      #Grupos donde mas Se publica Por Revista
      fluidRow(
        column(
          12,
          h4("Grupos donde mas se Publica por Revista"),
          plotlyOutput("gruposRevista"),
        ),
      ),
      #Grupos publican mejores cuartiles
      fluidRow(
        column(
          12,
          h4("Grupos que publican en Mejores Cuartiles"),
          plotlyOutput("gruposCuartil"),
        ),
      ),
      #Grupos publican por área de investigación
      fluidRow(
        column(
          12,
          h4("publicaciones de grupos Por area de Investigación"),
          plotlyOutput("gruposArea"),
        ),
      ),
      #Grupos con áreas investigación por cuartil
      fluidRow(
        column(
          12,
          h4("Grupos con Areas de Investigación por Cuartil"),
          plotlyOutput("gruposArCuar"),
        ),
      ),
      #Grupos en países con mayor índice cuartil
      fluidRow(
        column(
          12,
          h4("Grupos en paises con mayor indice de Cuartil"),
          plotlyOutput("gruposPaisCuar"),
        ),
      ),
    ),#Cierre Cuarta Pestaña
   ), #Cierre de la barra de navegacion
) #Cierre de la UI

####################################
# Servidor                         #
####################################

server <- function(input, output, session) {
  #-----------------------------------------------------------------------------
  #1ra pestaña
  #Principal
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
  
  #----------------------------------------------------------------------------
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
  
  #-----------------------------------------------------------------------------
  #Tercera Pestaña
  output$Paises <- renderPlotly({
    #Cartograma
    generarGraficaPaises()
  })
  output$Revistas <- renderPlotly({
    #Funcion de revistas
    generarGraficaRevistas()
  })
  output$RevistasCuartil <- renderPlotly({
    #Funcion de revistas
    generarGraficamejoresCuartiles()
  })
  output$areasPlus <- renderPlotly({
    #Funcion de revistas
    generarGraficaAreasdeInves()
  })
  output$areaCuartilPlus <- renderPlotly({
    #Funcion de revistas
    generarGraficasAreasCuartil()
  })
  output$paisCuartil <- renderPlotly({
    #Funcion de revistas
    generarGraficaPaisesCuartil()
  })
  output$areaPais <- renderPlotly({
    #Funcion de revistas
    generarGraficaPaisesArea()
  })
  
  #-----------------------------------------------------------------------------
  #Cuarta Pestaña GRUPOS
  output$gruposPais <- renderPlotly({
    generarGraficagruposPais()
  })
  output$gruposRevista<- renderPlotly({
    generarGraficaGrupoRevista()  
  })
  output$gruposCuartil<- renderPlotly({
    generarGraficaGrupoCuartil()
  })
  output$gruposArea<- ({
    
  })
  output$gruposArCuar<- ({
    
  })
  output$gruposPaisCuar<- ({
    
  })
}



####################################
# Crear la App. en Shiny           #
####################################
shinyApp(ui = ui, server = server)