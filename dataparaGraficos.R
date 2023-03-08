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
grupo <- "Grupo"
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
    x = RevMayB$nrevCuart,
    y = as.numeric(RevMayB$RevMayCuarts),
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
  areas1 <- names(split(PPA1, PPA1$`Area de Investigación 1`))
  sepPPA2 <- split(PPA2, PPA2$Pais)
  areas2 <- names(split(PPA2, PPA2$`Area de Investigación 2`))
  sepPPA1A <- cbind(names(sepPPA1))
  sepPPA2A <- cbind(names(sepPPA2))
  dataPPA1 <- NULL
  #Calculando los totales
  for (i in sepPPA1) {
    dataPPA1 <-as.numeric((c(dataPPA1, nrow(i))))
  }
  dataPPA1 <- as.data.frame(cbind(sepPPA1A, dataPPA1))
  dataPPA1$dataPPA1 <- as.numeric(dataPPA1$dataPPA1)
  colnames(dataPPA1)[1] <- "Pais"
  colnames(dataPPA1)[2] <- "Total"
  #Agregamos las Columnas para cada una de las Areas
  for(i in areas1){
    col <- NULL
    col <- data.frame(matrix(nrow = nrow(dataPPA1), ncol = 1))
    names(col) <- i
    dataPPA1 <- cbind(dataPPA1, col)
  }
  #se hace el conteo por cada area para cada Pais
  for(i in sepPPA1){
    areas <- NULL
    #Primero separar por Areas
    areas <- as.data.frame(i)
    areas <- split(areas, areas$`Area de Investigación 1`)
    #Tercero Contar las Filas
    for (j in areas) {
      fila <- NULL
      fila <- i[[1]][[1]][[1]]
      fila <- which(dataPPA1$Pais == fila)
      if (nrow(j)>0) {
        dataPPA1[fila, j[[2]][1]] <- as.numeric(nrow(j))
      }
    }
  }
  

  #Esta linea Quita todos los totales en los que los totales de publicaciones sea menor de 10
  dataPPA1 <- subset(dataPPA1, Total>=10) #Esta Matriz es con la que se debe Graficar
  #Grafica
  
  grafPaisArea <- plot_ly(
    x = dataPPA1$Pais,
    y = as.numeric(dataPPA1$`Energía y Potencia`),
    type = 'bar',
    name = colnames(dataPPA1)[3],
    marker = list(color = 'rgb(158,202,225)',
                  line = list(color = 'rgb(8,48,107)', width = 1.5))
  )
  grafPaisArea <- grafPaisArea %>% add_trace(
    y = as.numeric(dataPPA1$`Geomática, Ciencias de la Tierra y Gestión de Territorio`),
    name = colnames(dataPPA1)[4],
    marker = list(color = 'rgb(58,200,225)',
                  line = list(color = 'rgb(8,48,107)', width = 1.5))
  )
  cols <- ncol(dataPPA1)
  for (co in 5:cols) {
    nombre = colnames(dataPPA1)[co]
    colorg <- paste('rgb(',sample(1:225,1) ,',',sample(1:225,1),',',sample(1:225,1),')')
    grafPaisArea <- grafPaisArea %>% add_trace(
          y = as.numeric(dataPPA1[,co]),
          name = nombre,
          marker = list(color = colorg,
                        line = list(color = 'rgb(8,48,107)', width = 1.5))
        )
  }
  grafPaisArea <- grafPaisArea %>% layout(yaxis = list(title = 'Count'), barmode = 'group')
  grafPaisArea
  
}
#-------------------------------------------------------------------------------
#Grupos donde mas publican por Pais
generarGraficagruposPais <- function(){
  grupPais <- Data[, c(grupo, pais)]
  vecGrup <- as.data.frame(names(split(grupPais, grupPais$Grupo)))
  names(vecGrup) <- "Grupo"
  vecPais <-  names(split(grupPais, grupPais$Pais))
  grupPais <- split(grupPais, grupPais$Grupo)
  #Agregamos las Columnas de los Paises, y una extra del Total
  Total <- NULL
  Total <- data.frame(matrix(nrow = nrow(vecGrup), ncol = 1))
  names(Total) <- "Total"
  vecGrup <- cbind(vecGrup, Total)
  for (i in vecPais) {
    col <- NULL
    col <- data.frame(matrix(nrow = nrow(vecGrup), ncol = 1))
    names(col) <- i
    vecGrup <- cbind(vecGrup, col)
  }
  for (j in grupPais) {
    total <- NULL
    total <- nrow(j)
    filagrup <- NULL
    filagrup <- j[[1]][1]
    filagrup <- which(vecGrup$Grupo == filagrup)
    colum <- "Total"
    PaisesTotal <- as.data.frame(j[2])
    PaisesTotal <- split(PaisesTotal, PaisesTotal$Pais)
    if (nrow(j)>0) {
      vecGrup[filagrup, colum] <- as.numeric(total)
    }
    for (k in PaisesTotal) {
      totPais <- NULL
      colum <- NULL
      totPais <- nrow(k)
      colum <- k[[1]][1]
      if (nrow(j)>0) {
        vecGrup[filagrup, colum] <- as.numeric(totPais)
      }
    }
  }
  
  #Ahora se Filtraran los Grupos en los que halla menos de X publicaiones
  x <- 100
  vecGrup <- subset(vecGrup, Total>=x) #Esta Matriz es con la que se debe Graficar
  
  #Grafico
  fig <- plot_ly(vecGrup, x = vecGrup$Grupo,
                 y = vecGrup$Total,
                 name = "Total",
                 type = 'scatter',
                 mode = 'lines',
                 line = list(shape = "spline"))
  nc <- ncol(vecGrup)
  for (c in 3:nc) {
    nom <- colnames(vecGrup)[c]
    fig <- fig %>% add_trace(y = vecGrup[, c],
                             name = nom,
                             connectgaps = TRUE,
                             line = list(shape = "spline"))
  }
  
  
  fig
}
#-------------------------------------------------------------------------------
#Grupos donde mas publican Por Revistas
generarGraficaGrupoRevista <- function(){
  grupRevistas <- Data[, c(grupo, revista)]
  vecRevs <- as.data.frame(names(split(grupRevistas, grupRevistas$`Nombre de revista`)))
  names(vecRevs) <- "Revistas"
  vecGrup <-  names(split(grupRevistas, grupRevistas$Grupo))
  grupRevs <- split(grupRevistas, grupRevistas$`Nombre de revista`)
  
  #Agregamos las Columnas de los Paises, y una extra del Total
  Total <- NULL
  Total <- data.frame(matrix(nrow = nrow(vecRevs), ncol = 1))
  names(Total) <- "Total"
  vecRevs <- cbind(vecRevs, Total)
  for (i in vecGrup) {
    col <- NULL
    col <- data.frame(matrix(nrow = nrow(vecRevs), ncol = 1))
    names(col) <- i
    vecRevs <- cbind(vecRevs, col)
  }
  for (j in grupRevs) {
    total <- NULL
    total <- nrow(j)
    filaRev <- NULL
    filaRev <- j[[2]]
    filaRev <- which(vecRevs$Revistas == filaRev)
    colum <- "Total"
    grupTotal <- as.data.frame(j[1])
    grupTotal <- split(grupTotal, grupTotal$Grupo)
    if (nrow(j)>0) {
      vecRevs[filaRev, colum] <- as.numeric(total)
    }
    for (k in grupTotal) {
      totRevs <- NULL
      colum <- NULL
      totRevs <- nrow(k)
      colum <- k[[1]][1]
      if (nrow(j)>0) {
        vecRevs[filaRev, colum] <- as.numeric(totRevs)
      }
    }
  }
  #Ahora se Filtraran los Grupos en los que halla menos de X publicaiones
  x <- 10
  vecRevs <- subset(vecRevs, Total>=x) #Esta Matriz es con la que se debe Graficar
  
  #Grafico
  figgr <- plot_ly(vecRevs, x = abbreviate(vecRevs$Revistas, 15),
                 y = vecRevs$ARCOSES,
                 name = 'ARCOSES') %>% add_bars() %>% hide_legend()
  n <- ncol(vecRevs)-3
  
  for (r in 4:n) {
    names <-  colnames(vecRevs)[r]
    figgr <- figgr %>% add_trace(y = vecRevs[, r], name = names)
  }
  
  figgr <- figgr %>% layout(barmode = 'stack')
  figgr 
}

#-------------------------------------------------------------------------------
#Grupos que Publican en Q1 y Q2
generarGraficaGrupoCuartil <- function(){
  grupQuar <- Data[, c(grupo, cuartil)]
  #Cambiamos la Columna de Cuartil por un tipo clasificador
  grupQuar$`SJR/JCR` <- factor(grupQuar$`SJR/JCR`, levels = c("Q1", "Q2", "Q3", "Q4", "NI"))
  #Revistas con Cuartil 1 y 2
  grupQuar <- na.exclude(grupQuar[grupQuar$`SJR/JCR` == "Q1" | grupQuar$`SJR/JCR` == "Q2",])
  #Separar por Paises
  grupQuar <- split(grupQuar, grupQuar$Grupo)
  #Construir Tabla para Grafico
  MayGrups <- cbind(names(grupQuar))
  publicaciones <- NULL
  for (i in grupQuar) {
    publicaciones <- c(publicaciones, nrow(i))
  }
  MayGrups <- as.data.frame(cbind(MayGrups, publicaciones)) ## Matriz para Graficar
  figgq <- plot_ly(MayGrups, x = MayGrups$V1,
                 y = MayGrups$publicaciones,
                 color = MayGrups$V1) %>% add_bars() %>% hide_legend()
  figgq
  
}