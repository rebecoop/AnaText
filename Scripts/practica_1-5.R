setwd("~/Rebecoop/Programacion humanidades/Análisis de texto y estilometría con R/AnaText")
# Carga las librerías
library(tidyverse)
library(tidytext)

# Ahora cargará todos los ficheros de los mensajes
ficheros <- list.files(path ="datos/mensajes", pattern = "\\d+")
anno <- gsub("\\.txt", "", ficheros, perl = T)
mensajes <- tibble(anno = character(),
                   parrafo = numeric(),
                   texto = character())
for (i in 1:length(ficheros)){
  discurso <- readLines(paste("datos/mensajes",
                              ficheros[i],
                              sep = "/"))
  temporal <- tibble(anno = anno[i],
                     parrafo = seq_along(discurso),
                     texto = discurso)
  mensajes <- bind_rows(mensajes, temporal)
}

# Regenera la tabla general con todas las palabras
mensajes_palabras <- mensajes %>%
  unnest_tokens(palabra, texto)

# Crea la tabla con todas las palabras y calcula frecuencias
mensajes_frecuencias <- mensajes_palabras %>%
  count(palabra, sort = T) %>%
  mutate(relativa = n / sum(n))

# Borra objetos que no sirven y que son temporales
rm(temporal,discurso,i)

#Conseguir listas de palabras vacías
vacias <- get_stopwords("es")
vacias <- vacias %>%
  rename(palabra = word)

#Borramos de nuestro corpus las palabras vacías
mensajes_vaciado <- mensajes_palabras %>%
  anti_join(vacias)
