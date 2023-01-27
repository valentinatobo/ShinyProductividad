########################
#  Red de Coocurrencia #
########################

#Librerias a importar
library(tm)
library(tidytext)
library(dplyr)
library(igraph)
library(tidyr)
library(ggraph)

#Cargar el excel
library(readxl)

#Función para generar una red de co ocurrencias en títulos
generarRedDeCoocurrenciasPorGrupo <- function(grupoSelec) {
  
  #Ruta del archivo Excel
  ruta_fuente <- "datosFuenteExcel.xlsx"
  
  #Importar grupos y titulos de artículos
  datos_Articulos <- read_excel(ruta_fuente,
                                sheet = 'Articulos',
                                range = 'A1:B1991')
  
  #Importar grupos y titulos de Libros
  datos_Libros <- read_excel(ruta_fuente,
                             sheet = 'Libros',
                             range = 'A1:B138')
  
  #Importar grupos y titulos de Capitulos de Libros
  datos_Capitulos <- read_excel(ruta_fuente,
                                sheet = 'Capitulos',
                                range = 'A1:B180')
  
  #Importar grupos y titulos de Software
  datos_Software <- read_excel(ruta_fuente,
                               sheet = 'Software',
                               range = 'A1:B135')
  
  #Importar grupos y titulos de Trabajos dirigidos
  datos_Trabajos <- read_excel(ruta_fuente,
                               sheet = 'Trabajos',
                               range = 'A1:B1060')
  
  #Filtrar para el grupo seleccionado
  datos_ArticulosFiltrados <- subset(datos_Articulos, Grupo == grupoSelec)
  datos_LibrosFiltrados <- subset(datos_Libros, Grupo == grupoSelec)
  datos_CapitulosFiltrados <- subset(datos_Capitulos, Grupo == grupoSelec)
  datos_SoftwareFiltrados <- subset(datos_Software, Grupo == grupoSelec)
  datos_TrabajosFiltrados <- subset(datos_Trabajos, Grupo == grupoSelec)
  
  #Mezclar los datos en un unico dataframe
  mezclaDos = merge(x = datos_ArticulosFiltrados, y = datos_LibrosFiltrados, all = TRUE)
  mezclaTres = merge(x = mezclaDos, y = datos_CapitulosFiltrados, all = TRUE)
  mezclaCuatro = merge(x = mezclaTres, y = datos_SoftwareFiltrados, all = TRUE)
  datos_Filtrados = merge(x = mezclaDos, y = datos_TrabajosFiltrados, all = TRUE)
  
  #Solo tomar la columna de los titulos
  titulos <- datos_Filtrados[,2]
  
  #Unificar el texto en corpus para limpiarlo
  corpus = Corpus(VectorSource(titulos))
  
  #Evitar advertencias al limpiar el documento
  suppressWarnings({
    
    #Todo a minusculas
    corpus = tm_map(corpus, PlainTextDocument)
    corpus = tm_map(corpus, tolower)
    #Remover puntuacion
    corpus = tm_map(corpus, removePunctuation)
    #Eliminar numeros
    corpus = tm_map(corpus, removeNumbers)
    #Remover "stopwords" como is, is, at, etc.
    corpus = tm_map(corpus, removeWords, c("cloth", stopwords("english")))
    corpus = tm_map(corpus, removeWords, c("cloth", stopwords("spanish")))
    # Eliminar espacios en blanco
    corpus = tm_map(corpus, stripWhitespace)
    
  })
  
  TitulosDataFrame<-data.frame(text = sapply(corpus, as.character), stringsAsFactors = FALSE)
  
  #Ver el corpus
  #corpus[[1]][1] 
  
  New_bigrams <- TitulosDataFrame%>%unnest_tokens(bigram, text, token = "ngrams", n=2)
  New_bigrams
  
  New_bigrams %>% count(bigram, sort = TRUE)
  
  #Separate bigrams and remove stop words
  bigrams_separated <- New_bigrams %>% separate(bigram, c("word1", "word2"), sep = " ")
  
  bigrams_filtered <- bigrams_separated %>%
    filter(!word1 %in% stop_words$word) %>%
    filter(!word2 %in% stop_words$word)
  
  bigrams_counts <- bigrams_filtered %>% count(word1, word2, sort = TRUE)
  bigrams_counts
  
  bigrams_filtered %>%
    filter(word1 == "redes") %>%
    count(word2, sort=TRUE)
  
  
  bigram_graph <- bigrams_counts %>% 
    filter(n > 2) %>%
    graph_from_data_frame()
  
  bigram_graph
  
  set.seed(2017)
  ggraph(bigram_graph, layout = "fr") + 
   geom_edge_link() +
    geom_node_point() +
    geom_node_text(aes(label = name), vjust = 1, hjust = 1)
}