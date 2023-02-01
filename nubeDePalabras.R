#####################
#  Nube de palabras #
#####################

#Fuente: https://www.pluralsight.com/guides/visualization-text-data-using-word-cloud-r


#Función para generar la nube de palabras por grupo
generarNubeDePalabrasPorGrupo <- function(grupoSelec) {
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
    
    #Solo tomar la columna de los titulos
    titulos <- datos_Filtrados[,2]
    
    #Unificar el texto en un corpus para limpiarlo
    corpus = Corpus(VectorSource(titulos))
    
    #Desactivar los warnings de limpiar el archivo
    suppressWarnings({
      
      #Pasar todo el texto a minusculas
      corpus = tm_map(corpus, PlainTextDocument)
      corpus = tm_map(corpus, tolower)
      
      #Remover signos de puntuación
      corpus = tm_map(corpus, removePunctuation)
      
      #Eliminar numeros
      corpus = tm_map(corpus, removeNumbers)
      
      #Remover "stopwords" como is, is, at, en español e ingles.
      corpus = tm_map(corpus, removeWords, c("cloth", stopwords("english")))
      # corpus = tm_map(corpus, removeWords, c("cloth", stopwords("spanish")))
      
      # Eliminar espacios en blanco
      corpus = tm_map(corpus, stripWhitespace)
      
    })
    
    
    
    #Ver el corpus
    #corpus[[1]][1] 
    
    #Crear una matriz de terminos del corpus para obtener la frecuencia de palabras
    DTM <- TermDocumentMatrix(corpus)
    mat <- as.matrix(DTM)
    f <- sort(rowSums(mat),decreasing=TRUE)
    dat <- data.frame(word = names(f),freq=f)
    
    #Ver las 5 palabras mas usadas
    #head(dat, 5)
    
    #Crear la nube de palabras
    set.seed(100)
    wordcloud(words = dat$word, freq = dat$freq, scale=c(2,1), max.words = 30, random.order=FALSE, colors = brewer.pal(8, "Dark2"))
    
 
  
}

