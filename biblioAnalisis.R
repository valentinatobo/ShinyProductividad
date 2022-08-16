library("bibliometrix")
library("ggplot2")
library("stringr")
library("networkD3")
library("webshot")

corpus <- "Articulos.bib"
# corpus <- "MIE-dissertations-2016-2020.bib"
# corpus <- "MIS-dissertations-2012-2020.bib"
# corpus <- "MIS-dissertations-2016-2020.bib"

## for bibliometrix under v3.0.0 ##

# M <- convert2df(D, dbsource="scopus", format="bibtex")

## for bibliometrix v3.0.0 and up ##


corpus <- "datosFuenteExcel.xlsx"
D <- readFiles(corpus) 
M <- convert2df(D, dbsource="scopus", format="excel")
results <- biblioAnalysis(M)
summary(results, k=20, pause=FALSE)




net <- biblioNetwork(MAT, analysis="co-ocurrences", network="keywords", sep=";")
g <- networkPlot(net, n=10, type="fruchterman", Title="", labelsize=1, alpha=1,
                 cluster="louvain", edgesize=5, edges.min=1, verbose=T, weighted=T,
                 label.cex=F, label.n=10, size.cex=F, remove.isolates=T,
                 vos.path="VOSviewer.jar") 


# net2VOSviewer(g)
dev.print(png,filename="coocurrence-keywords.png", width=600, height=600);
dev.off()