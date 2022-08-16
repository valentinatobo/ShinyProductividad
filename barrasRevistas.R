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



