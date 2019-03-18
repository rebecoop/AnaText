setwd("~/Rebecoop/Programacion humanidades/Análisis de texto y estilometría con R/AnaText")
install.packages("udpipe")
library(udpipe)
library(tidyverse)

udpipe_download_model(language = "spanish-ancora")
modelo_ancora <- udpipe_load_model(file = 'spanish-ancora-ud-2.3-181115.udpipe')
#Para leer el archivo desde un sitio local
Pazos_Ulloa <- read_lines("https://raw.githubusercontent.com/7PartidasDigital/AnaText/master/datos/pazos_ulloa.txt", skip = 3, locale = default_locale())
#Expresiones regulares para separar los guiones - mediante espacios y otras cuestiones
Pazos_Ulloa <- gsub("[-–—]", " — ", Pazos_Ulloa) 
Pazos_Ulloa <- gsub(" ([\\.,;:])", "\\1", Pazos_Ulloa) 
Pazos_Ulloa <- gsub(" {2,10}", " ", Pazos_Ulloa) 
Pazos_Ulloa <- gsub("^ ", "", Pazos_Ulloa)
PU_analisis <- udpipe_annotate(modelo_ancora, Pazos_Ulloa)
PU_analisis <- as_tibble(PU_analisis)
#Gráfico que muestre las clases de palabras ordenadas por frecuencia de aparición
PU_analisis %>%
  drop_na(upos) %>%
  count(upos, sort = T) %>% 
  mutate(upos = reorder(upos, n)) %>% 
  ggplot(aes(upos, n)) + 
  geom_col(fill = "darkgreen") + 
  coord_flip()
#Grafico que muestre los sustantivos más frecuentes
PU_analisis %>%
  filter(upos == "NOUN") %>% 
  count(token, sort = T) %>% 
  mutate(token = reorder(token, n)) %>% 
  top_n(30) %>%
  ggplot(aes(token, n)) + 
  geom_col(fill = "darkgreen") + 
  coord_flip()


#Grafico que muestre los verbos más frecuentes
PU_analisis %>%
  filter(upos == "VERB"| upos == "AUX") %>% 
  count(lemma, sort = T) %>% 
  mutate(lemma = reorder(lemma, n)) %>% 
  top_n(30) %>%
  ggplot(aes(lemma, n)) + 
  geom_col(fill = "darkgreen") + 
  coord_flip()



