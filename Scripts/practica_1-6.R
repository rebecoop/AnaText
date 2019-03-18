#Etiquetado morfológico o postagguer
# koRpus - herramienta dependiente de treetagguer 
  #https://cran.r-project.org/web/packages/koRpus/index.html

#coreNLP > Stanford Core NLP
#cleanNLP > Stanford Core NLP
#spaCy > depende de Python

#Treebanks > UPennTreebank
# http://www.iula.upf.edu/recurs01_tbk_uk.htm
#Paquete udpipe > es el que usaremos un poco aquí y más sencillo.

setwd("~/Rebecoop/Programacion humanidades/Análisis de texto y estilometría con R/AnaText")
install.packages("udpipe")
library(udpipe)
library(tidyverse)

udpipe_download_model(language = "spanish-gsd") 
udpipe_download_model(language = "spanish-ancora")
modelo_ancora <- udpipe_load_model(file = 'spanish-ancora-ud-2.3-181115.udpipe')
texto <- "Se me permitirá que antes de referir el gran suceso de que fui testigo, diga algunas palabras sobre mi infancia, explicando por qué extraña manera me llevaron los azares de la vida a presenciar la terrible catástrofe de nuestra marina."

#Para hacer en analisis una vez tenemos el modelo descargado y cargado en la consola y el objeto a analizar
analisis <- udpipe_annotate(modelo_ancora, texto)

#Para transformar esos datos en una tabla
analisis <- as_tibble(analisis)
#Para ver cual es la clase con mas palabras
analisis %>%
  count(upos, sort = T)
#para ponerlo en una tabla
analisis %>%
  count(upos, sort = T) %>%
  mutate (upos = reorder(upos, n)) %>%
  ggplot(aes(upos, n)) +
  geom_col() +
  coord_flip()
analisis %>%
  filter(upos == "NOUN") %>%
  count(token, sort = T)

