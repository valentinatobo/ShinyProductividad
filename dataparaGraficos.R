########################
# Graficacion con r2D3 #
########################

# Librerias

#Paquetes para el Analisis
library(readxl)
library(r2d3)
library(Hmisc)
# Importación de la Data de Articulos Shiny
Data <- read_excel("Consolidado_Productividad_gráficas_2000_2022.xlsx",
                  sheet = 'Datos de Articulos Shiny')
#-------------------------------------------------------------------------------

#Paises donde mas Se Publica
atributo1 <- "País"
RevFil  <- (Data[,c(1,7,8)])
a <- table(RevFil)

#Grafica

#-------------------------------------------------------------------------------
#Revistas donde mas se Publica

atributo2 <- "ISSN"
RevFil  <- (Data[,atributo2])
b <- table(RevFil)
#Grafica

#-------------------------------------------------------------------------------

#Revistas con mejores Cuartiles
atributo3 <- "Nombre de revista"
atributo4 <- "ISSN"
atributo5 <- "SJR/JCR"
#Revistas con Sus Cuartiles
RevCuar  <- Data[,c(atributo3, atributo4, atributo5)]
#se Validan los duplicados en la selección
#RevCuar2 <- duplicated(RevCuar)
#Se retiran los valores duplicados
#RevCuar3 <- unique(RevCuar)
#Cambiamos la Columna de Cuartil por un tipo clasificador
RevCuar$`SJR/JCR` <- factor(RevCuar$`SJR/JCR`, levels = c("Q1", "Q2", "Q3", "Q4", "NI"))

#Revistas con Cuartil 1 y 2
RevMay <- na.exclude(RevCuar[RevCuar$`SJR/JCR` == "Q1" | RevCuar$`SJR/JCR` == "Q2",])
#Grafica

#-------------------------------------------------------------------------------

#Áreas de investigación donde más se publica


#Grafica

#-------------------------------------------------------------------------------
#Áreas de investigación donde más se publica en revistas de mayor cuartil

#Grafica

#-------------------------------------------------------------------------------
#Países con mayor índice de cuartil de revistas

#Grafica

#-------------------------------------------------------------------------------
#Países donde se publica con mayores proporciones de áreas de investigación

#Grafica

#-------------------------------------------------------------------------------
#Relación de últimos años 5 por área de investigación

#Grafica

#-------------------------------------------------------------------------------