########################
# Graficacion con r2D3 #
########################

# Librerias
# instalar los paquetes necesarios
# install.packages("sf")
# install.packages("cartogram")
# install.packages("readxl")
# install.package("dplyr")
# install.packages("ggplot2")
# install.packages("tidyr")
# install.packages("tmap")

# activar las librerías instaladas y demas
library(cartogram)
library(sf)
library(readxl)
library(dplyr)
library(ggplot2)
library(plotly)
library(tidyr)
library(tmap)
library(r2d3)
library(Hmisc)
########################
#     Data Global      #
########################
#-------------------------------------------------------------------------------
# Importación de la Data de Articulos Shiny
Data <- read_excel("Consolidado_Productividad_gráficas_2000_2022.xlsx",
                  sheet = 'Datos de Articulos Shiny')
# Datas de Geometrias
mundo <- st_read("countries.geo.json")

#Data para los Cartogramas en ISO 3166-1 alpha-3
df <- read.csv("https://raw.githubusercontent.com/plotly/datasets/master/2014_world_gdp_with_codes.csv")
df$GDP..BILLIONS. <- NULL

#Cantidad Total de Articulos
Articulos <- read_excel("Consolidado_Productividad_gráficas_2000_2022.xlsx",
                        sheet = 'Datos de Articulos Shiny')
NArticulos <- dim(Articulos)[1]
#Cantidad Total de Eventos Cientificos
Eventos <- read_excel("Consolidado_Productividad_gráficas_2000_2022.xlsx",
                      sheet = 'Eventos científicos')
NEventos <- dim(Eventos)[1]
#Cantidad de Trabajos Dirigidos
Trabajos <- read_excel("Consolidado_Productividad_gráficas_2000_2022.xlsx",
                       sheet = 'Trabajo dirigidos ')
NTrabajos <- dim(Trabajos)[1]
#Cantidad Total de Libros
Libros <- read_excel("Consolidado_Productividad_gráficas_2000_2022.xlsx",
                     sheet = 'Libros')
NLibros <- dim(Libros)[1]
#Cantidad Total de Software
Software <- read_excel("Consolidado_Productividad_gráficas_2000_2022.xlsx",
                       sheet = 'Software')
NSoftware <- dim(Software)[1]

#Cantidad de Capitulos de Libros
CapLibros <- read_excel("Consolidado_Productividad_gráficas_2000_2022.xlsx",
                        sheet = 'Capitulos de Libro')
NCapLibros <- dim(CapLibros)[1]

totalProductos <- sum(NArticulos, NEventos, NTrabajos, NLibros, NSoftware, NCapLibros)
nGNC <- sum(NArticulos, NLibros)
nDTI <- sum(NSoftware)
nASC <- sum(NCapLibros)
nFRH <- sum(NTrabajos)

pais <- "Pais"
issn <- "ISSN"
revista <- "Nombre de revista"
cuartil <- "SJR/JCR"
Area1 <- "Area de Investigación 1"
Area2 <- "Area de Investigación 2"
#-------------------------------------------------------------------------------

#Paises donde mas Se Publica
generarGraficaPaises <- function(){
  Paises  <- (Data[, pais])
  # Se agrupan los paises
  PaiPubA <- split(Paises, Paises$Pais)
  PaiPubB <- cbind(names(PaiPubA))
  dataPaiPub <- NULL
  for (i in PaiPubA) {
    dataPaiPub <- c(dataPaiPub, nrow(i))
  }
  PaiPubB <- cbind(PaiPubB, dataPaiPub)## Matriz de articulos por pais
  PaiPubB <- as.data.frame(PaiPubB)
  colnames(PaiPubB) <- c('Pais','Articulos')
  #
  articulosPais <- left_join(df, PaiPubB, by=c('COUNTRY'="Pais"))
  #Grafica
  #plot(mundo$geometry) # Grafico En Blanco
  #Grafico Simple
  l <- list(color = toRGB("grey"), width = 0.5)
  
  g <- list(
    showframe = TRUE,
    showcoastlines = TRUE,
    projection = list(type = 'Mercator')
  )
  fig <- plot_geo(articulosPais, type='choropleth', locations=articulosPais$CODE, z=articulosPais$Articulos, text=articulosPais$COUNTRY, colorscale="Purples")
  
  fig <- fig %>% colorbar(fig, title = "Numero de Articulos")
  fig <- fig %>% colorbar(title = 'Densidad de Articulos')
  fig <- fig %>% layout(
    title = 'Articulos Publicados<br> Por País',
    geo = g
  )
  fig
}
#-------------------------------------------------------------------------------
#Revistas donde mas se Publica
generarGraficaRevistas <- function(){
  RevPlus  <- (Data[,c(issn, revista)])
  #Separación de Elementos
  RevPlusA <- split(RevPlus, RevPlus$ISSN)
  #Muestra De Cantidades
  RevPlusB <- table(RevPlus)
  #Grafica
}
#-------------------------------------------------------------------------------
#Revistas con mejores Cuartiles
generarGraficamejoresCuartiles <- function(){
  RevCuar  <- Data[,c(revista, issn, cuartil)]
  #se Validan los duplicados en la selección
  #RevCuar2 <- duplicated(RevCuar)
  #Se retiran los valores duplicados
  #RevCuar3 <- unique(RevCuar)
  #Cambiamos la Columna de Cuartil por un tipo clasificador
  RevCuar$`SJR/JCR` <- factor(RevCuar$`SJR/JCR`, levels = c("Q1", "Q2", "Q3", "Q4", "NI"))
  
  #Revistas con Cuartil 1 y 2
  RevMay <- na.exclude(RevCuar[RevCuar$`SJR/JCR` == "Q1" | RevCuar$`SJR/JCR` == "Q2",])
  #Grafica
}


#-------------------------------------------------------------------------------

#Áreas de investigación donde más se publica

ArPlus1 <- Data[, Area1]
ArPlus2 <- Data[, Area2]
#Agrupación de Las Areas
ArPlus1 <- split(ArPlus1, ArPlus1$`Area de Investigación 1`)
ArPlus2 <- split(ArPlus2, ArPlus2$`Area de Investigación 2`)

#Grafica

#-------------------------------------------------------------------------------
#Áreas de investigación donde más se publica en revistas de mayor cuartil

#Tabla Con los Datos Necesarios
ArCuar1 <- Data[, c(Area1, issn, cuartil)]
ArCuar2 <- Data[, c(Area2, issn, cuartil)]

#Cambiamos la Columna de Cuartil por un tipo clasificador
ArCuar1$`SJR/JCR` <- factor(ArCuar1$`SJR/JCR`, levels = c("Q1", "Q2", "Q3", "Q4", "NI"))
ArCuar2$`SJR/JCR` <- factor(ArCuar2$`SJR/JCR`, levels = c("Q1", "Q2", "Q3", "Q4", "NI"))

#Revistas con Cuartil 1 y 2
ArCuarMay1 <- na.exclude(ArCuar1[ArCuar1$`SJR/JCR` == "Q1" | ArCuar1$`SJR/JCR` == "Q2",])
ArCuarMay2 <- na.exclude(ArCuar2[ArCuar2$`SJR/JCR` == "Q1" | ArCuar2$`SJR/JCR` == "Q2",])
#Agrupación de Las Areas
ArCuarMay1 <- split(ArCuarMay1, ArCuarMay1$`Area de Investigación 1`)
ArCuarMay2 <- split(ArCuarMay2, ArCuarMay2$`Area de Investigación 2`)

#Contrucción de la matriz en que se encontraran los Datos para Graficar
matriz <- cbind(names(ArCuarMay1))
publicaciones <- NULL
for (i in ArCuarMay1) {
  publicaciones <- c(publicaciones, nrow(i))
}
matriz <- cbind(matriz, publicaciones)## Matriz para Graficar

#Grafica

#-------------------------------------------------------------------------------
#Países con mayor índice de cuartil de revistas
PMCR <- Data[, c(pais, cuartil)]
#Separar mejores Cuartiles
#Cambiamos la Columna de Cuartil por un tipo clasificador
PMCR$`SJR/JCR` <- factor(PMCR$`SJR/JCR`, levels = c("Q1", "Q2", "Q3", "Q4", "NI"))
#Revistas con Cuartil 1 y 2
PMCR <- na.exclude(PMCR[PMCR$`SJR/JCR` == "Q1" | PMCR$`SJR/JCR` == "Q2",])
#Separar por Paises
PMCR <- split(PMCR, PMCR$Pais)
#Construir Tabla para Grafico
MPMCR <- cbind(names(PMCR))
publicaciones <- NULL
for (i in PMCR) {
  publicaciones <- c(publicaciones, nrow(i))
}
MPMCR <- cbind(MPMCR, publicaciones) ## Matriz para Graficar


#Grafica

#-------------------------------------------------------------------------------
#Países donde se publica con mayores proporciones de áreas de investigación

#Grafica

#-------------------------------------------------------------------------------
#Relación de últimos años 5 por área de investigación

#Grafica

#-------------------------------------------------------------------------------


### Pruebas de Graficos
