######################
# Barras para revistas #
######################

# Librerias
library(readr)
library(dplyr)
library(e1071)
library(mlbench)

#Text mining packages
library(tm)
library(SnowballC)
library("wordcloud")
library("RColorBrewer")

#Original packages
library(readxl)
library("stopwords")
library(tidyverse)
library(tidytext)


#Funcion para generar la grafica de barras por productos en revistas Internacionales (NA-Q1)
generarBarraArticulosRevista <- function(grupoSelec, tipoRevista){
  
  #Tipo de revista se refiere a la columna en el excel, 10 para nacional 11 para int.
  
  #Ruta del archivo Excel
  ruta_fuente <- "datosFuenteExcel.xlsx"
  
  #Importar grupos y titulos de artículos
  datos_Articulos <- read_excel(ruta_fuente,
                                sheet = 'Articulos',
                                range = 'A1:K1991')
  
  #Filtrar para el grupo seleccionado
  datos_ArticulosFiltrados <- subset(datos_Articulos, Grupo == grupoSelec)
  
  #Solo tomar la columna de la categoria
  revistaInternacional <- select(datos_ArticulosFiltrados, tipoRevista)
  
  tabladeFrec <- table(revistaInternacional)
  
  barplot (tabladeFrec,
           xlab = "Cuantil de la revista",
           ylab = "Cantidad de articulos",
           col = brewer.pal(6, "Set1")
           )
  
}

rotate_x <- function(data, labels_vec, rot_angle) {
  plt <- barplot(data, col='steelblue', xaxt="n")
  text(plt, par("usr")[3], labels = labels_vec, srt = rot_angle, adj = c(1.1,1.1), xpd = TRUE, cex=0.8) 
}

generarBarraArticulosDash <- function(){
  #Ruta del archivo Excel
  ruta_fuente <- "datosFuenteExcel.xlsx"
  
  #Importar grupos y titulos de artículos
  datos_Articulos <- read_excel(ruta_fuente,
                                sheet = 'Articulos',
                                range = 'A1:C1991')
  
  #Solo tomar la columna de la categoria
  articulos <- select(datos_Articulos, 3)
  
  tabladeFrec <- table(articulos)
  labelsTablaDeFrec = names(tabladeFrec)
  
  #Resumir labels
  labelsTablaDeFrec[2] = "Geomática..."
  labelsTablaDeFrec[3] = "Informática..."
  labelsTablaDeFrec[5] = "Organización..."
  labelsTablaDeFrec[7] = "TIC"
  
  #rotate_x(tabladeFrec, row.names(tabladeFrec), 45)

  par(mar=c(5,4,6,2))
  plt <- barplot (tabladeFrec,
           main = "GNC - Publicaciones en revistas cientificas",
           ylab = "Cantidad de articulos",
           col = brewer.pal(6, "Set1"),
           xaxt="n"
  )
  text(plt, par("usr")[3], labels = labelsTablaDeFrec, srt = 30, adj = c(1.1,1.1), xpd = TRUE, cex=0.8)
  
}

generarBarraLibrosDash <- function(){
  #Ruta del archivo Excel
  ruta_fuente <- "datosFuenteExcel.xlsx"
  
  #Importar grupos y titulos de artículos
  datos_Articulos <- read_excel(ruta_fuente,
                                sheet = 'Libros',
                                range = 'A1:C138')
  
  #Solo tomar la columna de la categoria
  articulos <- select(datos_Articulos, 3)
  
  tabladeFrec <- table(articulos)
  labelsTablaDeFrec = names(tabladeFrec)
  
  #Resumir labels
  #labelsTablaDeFrec[2] = "Geomática..."
  #labelsTablaDeFrec[3] = "Informática..."
  #labelsTablaDeFrec[5] = "Organización..."
  #labelsTablaDeFrec[7] = "TIC"
  
  #rotate_x(tabladeFrec, row.names(tabladeFrec), 45)
  
  par(mar=c(5,4,6,2))
  plt <- barplot (tabladeFrec,
                  main = "GNC - Libros de Investigación",
                  ylab = "Cantidad de Libros",
                  col = brewer.pal(6, "Set1"),
                  xaxt="n"
  )
  text(plt, par("usr")[3], labels = labelsTablaDeFrec, srt = 30, adj = c(1.1,1.1), xpd = TRUE, cex=0.8)
  
}

generarBarraSoftwareDash <- function(){
  #Ruta del archivo Excel
  ruta_fuente <- "datosFuenteExcel.xlsx"
  
  #Importar grupos y titulos de artículos
  datos_Articulos <- read_excel(ruta_fuente,
                                sheet = 'Software',
                                range = 'A1:C135')
  
  #Solo tomar la columna de la categoria
  articulos <- select(datos_Articulos, 3)
  
  tabladeFrec <- table(articulos)
  labelsTablaDeFrec = names(tabladeFrec)
  
  #Resumir labels
  #labelsTablaDeFrec[2] = "Geomática..."
  #labelsTablaDeFrec[3] = "Informática..."
  #labelsTablaDeFrec[5] = "Organización..."
  #labelsTablaDeFrec[7] = "TIC"
  
  #rotate_x(tabladeFrec, row.names(tabladeFrec), 45)
  
  par(mar=c(5,4,6,2))
  plt <- barplot (tabladeFrec,
                  main = "DTI - Software",
                  ylab = "Software Desarrollado",
                  col = brewer.pal(6, "Set1"),
                  xaxt="n"
  )
  text(plt, par("usr")[3], labels = labelsTablaDeFrec, srt = 30, adj = c(1.1,1.1), xpd = TRUE, cex=0.8)
  
}

generarBarraCapitulosDash <- function(){
  #Ruta del archivo Excel
  ruta_fuente <- "datosFuenteExcel.xlsx"
  
  #Importar grupos y titulos de artículos
  datos_Articulos <- read_excel(ruta_fuente,
                                sheet = 'Capitulos',
                                range = 'A1:C180')
  
  #Solo tomar la columna de la categoria
  articulos <- select(datos_Articulos, 3)
  
  tabladeFrec <- table(articulos)
  labelsTablaDeFrec = names(tabladeFrec)
  
  #Resumir labels
  #labelsTablaDeFrec[2] = "Geomática..."
  #labelsTablaDeFrec[3] = "Informática..."
  #labelsTablaDeFrec[5] = "Organización..."
  #labelsTablaDeFrec[7] = "TIC"
  
  #rotate_x(tabladeFrec, row.names(tabladeFrec), 45)
  
  par(mar=c(5,4,6,2))
  plt <- barplot (tabladeFrec,
                  main = "ASC - Capítulos de libros en eventos científicos",
                  ylab = "Capítulos de Libros",
                  col = brewer.pal(6, "Set1"),
                  xaxt="n"
  )
  text(plt, par("usr")[3], labels = labelsTablaDeFrec, srt = 30, adj = c(1.1,1.1), xpd = TRUE, cex=0.8)
  
}

generarBarraTrabajosDash <- function(){
  #Ruta del archivo Excel
  ruta_fuente <- "datosFuenteExcel.xlsx"
  
  #Importar grupos y titulos de artículos
  datos_Articulos <- read_excel(ruta_fuente,
                                sheet = 'Trabajos',
                                range = 'A1:C1060')
  
  #Solo tomar la columna de la categoria
  articulos <- select(datos_Articulos, 3)
  
  tabladeFrec <- table(articulos)
  labelsTablaDeFrec = names(tabladeFrec)
  
  #Resumir labels
  #labelsTablaDeFrec[2] = "Geomática..."
  #labelsTablaDeFrec[3] = "Informática..."
  #labelsTablaDeFrec[5] = "Organización..."
  #labelsTablaDeFrec[7] = "TIC"
  
  #rotate_x(tabladeFrec, row.names(tabladeFrec), 45)
  
  par(mar=c(5,4,7,2))
  plt <- barplot (tabladeFrec,
                  main = "FRH - Proyectos de Grado, Tesis de Maestría y Tesis de Doctorado",
                  ylab = "Producto FRH",
                  col = brewer.pal(6, "Set1"),
                  xaxt="n"
  )
  text(plt, par("usr")[3], labels = labelsTablaDeFrec, srt = 30, adj = c(1.1,1.1), xpd = TRUE, cex=0.8)
  
}

