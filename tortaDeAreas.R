######################
# Grupo por producto #
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



#Funcion para generar la grafica de torta por productos en articulos
generarTortaPorAreaArticulos <- function(grupoSelec){
  #Para probar el script
  #grupoSelec = "GIIRA"
  
  #Ruta del archivo Excel
  ruta_fuente <- "datosFuenteExcel.xlsx"
  
  #Importar grupos y titulos solo de articulos
  datos_Articulos <- read_excel(ruta_fuente,
                                sheet = 'Articulos',
                                range = 'A1:C1991')
  
  #Filtrar para el grupo seleccionado
  datos_ArticulosFiltrados <- subset(datos_Articulos, Grupo == grupoSelec)
  
  #Solo tomar la columna de la categoria
  titulos <- select(datos_ArticulosFiltrados, 3)
  
  #Tabla de frecuencia para cada opcion
  tabladeFrec <- table(titulos)
  
  #
  
  #Graficar la torta
  etiquetas <- paste0(names(tabladeFrec), " = ", round(100 * tabladeFrec/sum(tabladeFrec), 2), "%")
  
  #Grafica con porcentajes
  #pie (tabladeFrec, labels = etiquetas)
  
  #Grafica comun y corriente
  pie(tabladeFrec)
  
  
}



