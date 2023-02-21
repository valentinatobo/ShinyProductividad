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
########################
#     Librerias        #
########################
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
library(packcircles)
library(viridisLite)
library(viridis)
library(ggiraph)
########################
#     Data Global      #
########################

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
  fig <- fig %>% colorbar(title = 'Densidad de Articulos')
  fig <- fig %>% layout(
    title = 'Densidad de Articulos',
    geo = g
  )
  fig
}
#-------------------------------------------------------------------------------
#Revistas donde mas se Publica
generarGraficaRevistas <- function(){
  RevPlus  <- (Data[,c(issn, revista)])
  #Limpiando Las cadenas
  # for (i in 1:length(RevPlus[[1]])) {
  #   a <- NULL
  #   a <- RevPlus[[1]][i]
  #   a <- gsub(" ", "", a)
  #   RevPlus[[1]][i] <- a
  # }
  RevPlusA <- split(RevPlus, RevPlus$ISSN)
  RevPlusB <- cbind(names(RevPlusA))
  RevPubli <- NULL
  for (i in RevPlusA) {
    RevPubli <- c(RevPubli, nrow(i))
  }
  nrev <- NULL
  for (i in RevPlusA) {
    a <- table(i[2])
    valor_mas_frecuente <- names(a)[which.max(a)]
    nrev <- c(nrev, valor_mas_frecuente)
  }
  RevPlusB <-  as.data.frame(cbind(RevPlusB, RevPubli, nrev))## Matriz de articulos por pais
  #Grafica
  

  # # Add a column with the text you want to display for each bubble:
  # data$text <- paste("name: ",data$V1, "\n", "value:", data$RevPubli, "\n", "You can add a story here!")
  # 
  # # Generate the layout
  # packing <- circleProgressiveLayout(RevPlusB$RevPubli, sizetype='area')
  # data <- cbind(data, packing)
  # dat.gg <- circleLayoutVertices(packing, npoints=298662)
  # 
  # # Make the plot with a few differences compared to the static version:
  # p <- ggplot() + 
  #   geom_polygon_interactive(data = dat.gg, aes(x, y, group = id, fill=id, tooltip = data$V1, data_id = id), colour = "black", alpha = 0.6) +
  #   scale_fill_viridis() +
  #   geom_text(data = data, aes(x, y, label = gsub("Group_", "", group)), size=2, color="black") +
  #   theme_void() + 
  #   theme(legend.position="none", plot.margin=unit(c(0,0,0,0),"cm") ) + 
  #   coord_equal()
  # 
  # # Turn it interactive
  # widg <- ggiraph(ggobj = p, width_svg = 7, height_svg = 7)
  
  revs <- plot_ly(
    x = RevPlusB$nrev,
    y = as.numeric(RevPlusB$RevPubli),
    name = "Publicaciones por Revisa",
    type = "bar"
  )
  revs
}
#-------------------------------------------------------------------------------
#Revistas con mejores Cuartiles
generarGraficamejoresCuartiles <- function(){
  RevCuar  <- Data[,c(revista, issn, cuartil)]

  #Cambiamos la Columna de Cuartil por un tipo clasificador
  RevCuar$`SJR/JCR` <- factor(RevCuar$`SJR/JCR`, levels = c("Q1", "Q2", "Q3", "Q4", "NI"))
  
  #Revistas con Cuartil 1 y 2
  RevMay <- na.exclude(RevCuar[RevCuar$`SJR/JCR` == "Q1" | RevCuar$`SJR/JCR` == "Q2",])
  RevMayA <- split(RevMay, RevMay$ISSN)
  RevMayB <- cbind(names(RevMayA))
  RevMayCuarts <- NULL
  for (i in RevMayA) {
    RevMayCuarts <- c(RevMayCuarts, nrow(i))
  }
  nrevCuart <- NULL
  for (i in RevMayA) {
    a <- table(i[1])
    valor_mas_frecuente_cuart <- names(a)[which.max(a)]
    nrevCuart <- c(nrevCuart, valor_mas_frecuente_cuart)
  }
  RevMayB <-  as.data.frame(cbind(RevMayB, RevMayCuarts, nrevCuart))## Matriz de articulos por pais
  
  #Grafica
  revsCuar <- plot_ly(
    x = RevMayB$RevMayCuarts,
    y = as.numeric(RevMayB$nrevCuart),
    name = "Revistas Con Mejor Cuartil",
    type = "bar"
  )
  revsCuar
}
#-------------------------------------------------------------------------------
#Áreas de investigación donde más se publica
generarGraficaAreasdeInves <- function(){
  ArPlus1 <- Data[, Area1]
  ArPlus2 <- Data[, Area2]
  #Agrupación de Las Areas
  ArPlus1 <- split(ArPlus1, ArPlus1$`Area de Investigación 1`)
  ArPlus2 <- split(ArPlus2, ArPlus2$`Area de Investigación 2`)
  #Contrucción de la matriz en que se encontraran los Datos para Graficar
  matrizTotal <- cbind(names(ArPlus1))
  publicacionesTotal <- NULL
  for (i in ArPlus1) {
    publicacionesTotal <- c(publicacionesTotal, nrow(i))
  }
  publicacionesTotal2 <- c(16, 10, 30, 0, 90, 16, 0, 57)
  # for (i in ArPlus2) {
  #   publicacionesTotal2 <- c(publicacionesTotal2, nrow(i))
  # }
  
  matrizTotal <- as.data.frame(cbind(matrizTotal, publicacionesTotal, publicacionesTotal2))## Matriz para Graficar
  
  #Grafica
  areaPlus <- plot_ly(
    x = matrizTotal$V1,
    y = as.numeric(matrizTotal$publicacionesTotal),
    type = 'bar',
    name = 'Publicaciones por Area 1',
    marker = list(color = 'rgb(158,202,225)',
    line = list(color = 'rgb(8,48,107)', width = 1.5))
  )
  areaPlus <- areaPlus %>% add_trace(
    y = as.numeric(matrizTotal$publicacionesTotal2),
    name = 'Publicaciones por Area 2',
    marker = list(color = 'rgb(58,200,225)',
    line = list(color = 'rgb(8,48,107)', width = 1.5))
  )
  areaPlus <- areaPlus %>% layout(yaxis = list(title = 'Count'), barmode = 'group')
  areaPlus
}
#-------------------------------------------------------------------------------
#Áreas de investigación donde más se publica en revistas de mayor cuartil
generarGraficasAreasCuartil <- function(){
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
  matriz <- as.data.frame(cbind(matriz, publicaciones))## Matriz para Graficar
  
  #Grafica
  areacuarPlus <- plot_ly(
    x = matriz$V1,
    y = as.numeric(matriz$publicaciones),
    name = "Areas Donde Mas se Publica con mayor Cuartil",
    type = "bar"
  )
  areacuarPlus
}
#-------------------------------------------------------------------------------
#Países con mayor índice de cuartil de revistas
generarGraficaPaisesCuartil <- function(){
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
  MPMCR <- as.data.frame(cbind(MPMCR, publicaciones)) ## Matriz para Graficar
  
  
  #Grafica
  paisCuart <- plot_ly(
    x = MPMCR$V1,
    y = as.numeric(MPMCR$publicaciones),
    name = "Paises Con mas Publicaciones",
    type = "bar",
    marker = list(color = 'rgb(58,200,225)',
    line = list(color = 'rgb(8,48,107)', width = 1.5))
  )
  paisCuart
}
#-------------------------------------------------------------------------------
#Países donde se publica con mayores proporciones de áreas de investigación
generarGraficaPaisesArea <- function(){
  PPA1 <- Data[, c(pais, Area1)]
  PPA2 <- Data[, c(pais, Area2)]
  sepPPA1 <- split(PPA1, PPA1$Pais)
  sepPPA2 <- split(PPA2, PPA2$Pais)
  #Grafica
  paisArea <- plot_ly(
    x = c("A1", "A2", "A3", "A4", "A5"),
    y = c(5,3,2,4,5),
    name = "Areas Donde Mas se Publica",
    type = "bar"
  )
  paisArea
}
#-------------------------------------------------------------------------------


### Pruebas de Graficos
