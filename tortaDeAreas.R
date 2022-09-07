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
library(ggplot2)
library(r2r)

#Hash para las abreviaturas


#Funcion para generar la grafica de torta por productos en articulos
generarTortaPorAreaArticulos <- function(grupoSelec){
  
  m <- hashmap()
  insert(m, "E y P", "Energía y Potencia")
  insert(m, "G; C T y G T", "Geomática; Ciencias de la Tierra y Gestión de Territorio")
  insert(m, "I y P D", "Informática y Procesamiento de Datos")
  insert(m, "I C", "Inteligencia Computacional")
  insert(m, "O; P y L", "Organización; Producción y Logística")
  insert(m, "TIC", "Tecnología; Información y Comunicaciones")
  insert(m, "Otra", "Otra")
  
  #Para probar el script
  #grupoSelec = "GIIRA"
  
  #Ruta del archivo Excel
  ruta_fuente <- "datosFuenteExcel.xlsx"
  
  #Importar grupos y titulos de artículos
  datos_Articulos <- read_excel(ruta_fuente,
                                sheet = 'Articulos',
                                range = 'A1:C1991')
  
  #Importar grupos y titulos de Libros
  datos_Libros <- read_excel(ruta_fuente,
                             sheet = 'Libros',
                             range = 'A1:C138')
  
  #Importar grupos y titulos de Capitulos de Libros
  datos_Capitulos <- read_excel(ruta_fuente,
                                sheet = 'Capitulos',
                                range = 'A1:C180')
  
  #Importar grupos y titulos de Software
  datos_Software <- read_excel(ruta_fuente,
                               sheet = 'Software',
                               range = 'A1:C135')
  
  #Importar grupos y titulos de Trabajos dirigidos
  datos_Trabajos <- read_excel(ruta_fuente,
                               sheet = 'Trabajos',
                               range = 'A1:C1060')
  
  #Filtrar para el grupo seleccionado (grupoSelec)
  datos_ArticulosFiltrados <- subset(datos_Articulos, Grupo == grupoSelec)
  datos_LibrosFiltrados <- subset(datos_Libros, Grupo == grupoSelec)
  datos_CapitulosFiltrados <- subset(datos_Capitulos, Grupo == grupoSelec)
  datos_SoftwareFiltrados <- subset(datos_Software, Grupo == grupoSelec)
  datos_TrabajosFiltrados <- subset(datos_Trabajos, Grupo == grupoSelec)
  
  #Concatenar los datos en un unico dataframe
  mezclaDos = merge(x = datos_ArticulosFiltrados, y = datos_LibrosFiltrados, all = TRUE)
  mezclaTres = merge(x = mezclaDos, y = datos_CapitulosFiltrados, all = TRUE)
  mezclaCuatro = merge(x = mezclaTres, y = datos_SoftwareFiltrados, all = TRUE)
  datos_Filtrados = merge(x = mezclaDos, y = datos_TrabajosFiltrados, all = TRUE)
  
  #Filtrar para el grupo seleccionado
  datos_ArticulosFiltrados <- subset(datos_Filtrados, Grupo == grupoSelec)
  
  #Solo tomar la columna de la categoria
  titulos <- select(datos_ArticulosFiltrados, 3)
  
  #Tabla de frecuencia para cada opcion
  tabladeFrec <- table(titulos)
  
  names = names(tabladeFrec)
  namesTrad = names(tabladeFrec)
  trad = "H"
  #traducir
  
  "for(i in 1:length(names)){
    if(query(m, names[i] is.null())){
      namesTrad[i]
    } else {
      namesTrad[i] = query(m, names[i])
    }
  }"
  
  #Graficar la torta
  etiquetas <- paste0(names(tabladeFrec), " (", round(100 * tabladeFrec/sum(tabladeFrec), 2), "%)")
  
  #etiquetasLeg <- paste0(names(tabladeFrec), ": ", namesTrad)
  
  #Grafica con porcentajes
  pie (tabladeFrec, labels = etiquetas, col = brewer.pal(length(etiquetas), "Set1"))
  #legend("topright", etiquetasLeg, cex = 0.55, fill = brewer.pal(length(etiquetas), "Set1"))
  
  
  #Grafica comun y corriente
  #pie(tabladeFrec, col = )
  
  
}

generarTortaPorGrupoArticulos <- function(grupoSelec){
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
  
  names = names(tabladeFrec)
  namesTrad = names(tabladeFrec)
  trad = "H"
  #traducir
  
  "for(i in 1:length(names)){
    if(query(m, names[i] is.null())){
      namesTrad[i]
    } else {
      namesTrad[i] = query(m, names[i])
    }
  }"
  
  #Graficar la torta
  etiquetas <- paste0(names(tabladeFrec), " (", round(100 * tabladeFrec/sum(tabladeFrec), 2), "%)")
  
  #etiquetasLeg <- paste0(names(tabladeFrec), ": ", namesTrad)
  
  #Grafica con porcentajes
  pie (tabladeFrec, labels = etiquetas, col = brewer.pal(length(etiquetas), "Set1"), edges = length(etiquetas))
  #legend("topright", etiquetasLeg, cex = 0.55, fill = brewer.pal(length(etiquetas), "Set1"))
  
  
  #Grafica comun y corriente
  #pie(tabladeFrec, col = )
  
  
}

generarTortaPorGrupoLibros <- function(grupoSelec){
  #Para probar el script
  #grupoSelec = "GIIRA"
  
  #Ruta del archivo Excel
  ruta_fuente <- "datosFuenteExcel.xlsx"
  
  #Importar grupos y titulos solo de articulos
  datos_Articulos <- read_excel(ruta_fuente,
                                sheet = 'Libros',
                                range = 'A1:C138')
  
  #Filtrar para el grupo seleccionado
  datos_ArticulosFiltrados <- subset(datos_Articulos, Grupo == grupoSelec)
  
  #Solo tomar la columna de la categoria
  titulos <- select(datos_ArticulosFiltrados, 3)
  
  #Tabla de frecuencia para cada opcion
  tabladeFrec <- table(titulos)
  
  names = names(tabladeFrec)
  namesTrad = names(tabladeFrec)
  trad = "H"
  #traducir
  
  "for(i in 1:length(names)){
    if(query(m, names[i] is.null())){
      namesTrad[i]
    } else {
      namesTrad[i] = query(m, names[i])
    }
  }"
  
  #Graficar la torta
  etiquetas <- paste0(names(tabladeFrec), " (", round(100 * tabladeFrec/sum(tabladeFrec), 2), "%)")
  
  #etiquetasLeg <- paste0(names(tabladeFrec), ": ", namesTrad)
  
  #Grafica con porcentajes
  pie (tabladeFrec, labels = etiquetas, col = brewer.pal(length(etiquetas), "Set1"))
  #legend("topright", etiquetasLeg, cex = 0.55, fill = brewer.pal(length(etiquetas), "Set1"))
  
  
  #Grafica comun y corriente
  #pie(tabladeFrec, col = )
  
  
}

generarTortaPorGrupoSoftware <- function(grupoSelec){
  #Para probar el script
  #grupoSelec = "GIIRA"
  
  #Ruta del archivo Excel
  ruta_fuente <- "datosFuenteExcel.xlsx"
  
  #Importar grupos y titulos solo de articulos
  datos_Articulos <- read_excel(ruta_fuente,
                                sheet = 'Software',
                                range = 'A1:C135')
  
  #Filtrar para el grupo seleccionado
  datos_ArticulosFiltrados <- subset(datos_Articulos, Grupo == grupoSelec)
  
  #Solo tomar la columna de la categoria
  titulos <- select(datos_ArticulosFiltrados, 3)
  
  #Tabla de frecuencia para cada opcion
  tabladeFrec <- table(titulos)
  
  names = names(tabladeFrec)
  namesTrad = names(tabladeFrec)
  trad = "H"
  #traducir
  
  "for(i in 1:length(names)){
    if(query(m, names[i] is.null())){
      namesTrad[i]
    } else {
      namesTrad[i] = query(m, names[i])
    }
  }"
  
  #Graficar la torta
  etiquetas <- paste0(names(tabladeFrec), " (", round(100 * tabladeFrec/sum(tabladeFrec), 2), "%)")
  
  #etiquetasLeg <- paste0(names(tabladeFrec), ": ", namesTrad)
  
  #Grafica con porcentajes
  pie (tabladeFrec, labels = etiquetas, col = brewer.pal(length(etiquetas), "Set1"))
  #legend("topright", etiquetasLeg, cex = 0.55, fill = brewer.pal(length(etiquetas), "Set1"))
  
  
  #Grafica comun y corriente
  #pie(tabladeFrec, col = )
  
  
}

generarTortaPorGrupoCapitulos <- function(grupoSelec){
  #Para probar el script
  #grupoSelec = "GIIRA"
  
  #Ruta del archivo Excel
  ruta_fuente <- "datosFuenteExcel.xlsx"
  
  #Importar grupos y titulos solo de articulos
  datos_Articulos <- read_excel(ruta_fuente,
                                sheet = 'Capitulos',
                                range = 'A1:C180')
  
  #Filtrar para el grupo seleccionado
  datos_ArticulosFiltrados <- subset(datos_Articulos, Grupo == grupoSelec)
  
  #Solo tomar la columna de la categoria
  titulos <- select(datos_ArticulosFiltrados, 3)
  
  #Tabla de frecuencia para cada opcion
  tabladeFrec <- table(titulos)
  
  names = names(tabladeFrec)
  namesTrad = names(tabladeFrec)
  trad = "H"
  #traducir
  
  "for(i in 1:length(names)){
    if(query(m, names[i] is.null())){
      namesTrad[i]
    } else {
      namesTrad[i] = query(m, names[i])
    }
  }"
  
  #Graficar la torta
  etiquetas <- paste0(names(tabladeFrec), " (", round(100 * tabladeFrec/sum(tabladeFrec), 2), "%)")
  
  #etiquetasLeg <- paste0(names(tabladeFrec), ": ", namesTrad)
  
  #Grafica con porcentajes
  pie (tabladeFrec, labels = etiquetas, col = brewer.pal(length(etiquetas), "Set1"))
  #legend("topright", etiquetasLeg, cex = 0.55, fill = brewer.pal(length(etiquetas), "Set1"))
  
  
  #Grafica comun y corriente
  #pie(tabladeFrec, col = )
  
  
}

generarTortaPorGrupoTrabajos <- function(grupoSelec){
  #Para probar el script
  #grupoSelec = "GIIRA"
  
  #Ruta del archivo Excel
  ruta_fuente <- "datosFuenteExcel.xlsx"
  
  #Importar grupos y titulos solo de articulos
  datos_Articulos <- read_excel(ruta_fuente,
                                sheet = 'Trabajos',
                                range = 'A1:C1060')
  
  #Filtrar para el grupo seleccionado
  datos_ArticulosFiltrados <- subset(datos_Articulos, Grupo == grupoSelec)
  
  #Solo tomar la columna de la categoria
  titulos <- select(datos_ArticulosFiltrados, 3)
  
  #Tabla de frecuencia para cada opcion
  tabladeFrec <- table(titulos)
  
  names = names(tabladeFrec)
  namesTrad = names(tabladeFrec)
  trad = "H"
  #traducir
  
  "for(i in 1:length(names)){
    if(query(m, names[i] is.null())){
      namesTrad[i]
    } else {
      namesTrad[i] = query(m, names[i])
    }
  }"
  
  #Graficar la torta
  etiquetas <- paste0(names(tabladeFrec), " (", round(100 * tabladeFrec/sum(tabladeFrec), 2), "%)")
  
  #etiquetasLeg <- paste0(names(tabladeFrec), ": ", namesTrad)
  
  #Grafica con porcentajes
  pie (tabladeFrec, labels = etiquetas, col = brewer.pal(length(etiquetas), "Set1"))
  #legend("topright", etiquetasLeg, cex = 0.55, fill = brewer.pal(length(etiquetas), "Set1"))
  
  
  #Grafica comun y corriente
  #pie(tabladeFrec, col = )
  
  
}

