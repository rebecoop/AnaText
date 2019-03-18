#Establecer directorio de trabajo
setwd("~/Rebecoop/Programacion humanidades/Análisis de texto y estilometría con R/AnaText/Arturo Perez/")

#Instalar librerias para descargar y manejar páginas HTML
install.packages("RCurl")
install.packages("XML")

#Llamar a las librerías instaladas
library("RCurl")
library("XML")
library("rvest")

#Establecemos la página índice del autor mediante un pequeño arreglo (para conseguir el índice completo lo he introducido en un archivo txt)
tabla_url <- data.frame(url = character())
tabla_url <- readLines("Direcciones.txt")


#Para leer esta página web y guardarla como un vetor
FUENTE <- getURL(tabla_url, encoding="UTF8")

#Para poder leer el fichero. Aquí buscaremos los html que nos lleven a los artículos
ANALIZADO <- htmlParse(FUENTE)

#Recuperamos la url de cada artículo y los etiquetamos según título, y fecha.
TITULO <- trimws(xpathSApply(ANALIZADO, "//main//h2//a",xmlValue))
DIRECCION <- xpathSApply(ANALIZADO, "//main//h2/a/@href")
FECHA <- (gsub(".*/(\\d{4})(\\d{2})(\\d{2}).*", "\\1\\2\\3", DIRECCION))

#Guardamos la información en una única matriz con tres vectores
DATOS <- cbind(TITULO, FECHA, DIRECCION)

#Para leer cuántos datos tenemos para descargar y guardar los artículos como ficheros
nrow(DATOS)


for (i in 1:nrow(DATOS)) {
  url <- DATOS[i, 3]
  original <- getURL(url, encoding="UTF-8")
  orig_analizado <- htmlParse(original)
  titulo <- xpathSApply(orig_analizado, "//h1",xmlValue) 
  texto <- xpathSApply(orig_analizado, "//section[@class='post-content']/p", xmlValue)
  metaDatos <- paste(titulo, (gsub(".*/(\\d{4})(\\d{2})(\\d{2})/.*",
                                   "\\1/\\2/\\3", url)), url, sep = "\t") 
  texto <- c(metaDatos, texto)
  fichero_salida <- paste("APR_", DATOS[i, 2], ".txt", sep="") 
  writeLines(texto, fichero_salida)
}

