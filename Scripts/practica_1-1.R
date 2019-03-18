#Llamamos a las librerías
library("tidyverse")
library("tidytext")

#Creamos una nueva variable que lee los datos del discurso de 1975 y los almacena según parrafo y texto
discurso <- readLines("datos/mensajes/1975.txt")

#Creamos una nueva tabla a partir del discurso con las dos variables y catorce filas con los datos correspondientes
mensaje <- tibble(parrafo = seq_along(discurso), texto = discurso)


#Extraemos las palabras token (estarán repetidas) del mensaje anterior y mantenemos el parrafo del que provienen
mensaje_palabras <- mensaje %>%
  unnest_tokens(palabra, texto)

#Extraemos los enunciados del mensaje anterior y mantenemos los párrafos, además, contamos el número de palabras por oración.
mensaje_enunciados <- mensaje %>%
  unnest_tokens(oraciones, 
                texto, 
                token = "sentences") %>%
  mutate(NumPal = str_count(oraciones, "\\w+"))

#Calculamos las palabras tipo y su frecuencia total y relativa (en una nueva columna hecha con mutate) en el texto de 1975
mensaje_frecuencias <- mensaje_palabras %>%
  count (palabra, sort = T) %>%
  mutate(relativa = n / sum(n))

#Pintamos en una gráfica los datos del número de palabras por enunciado y su media y mediana
ggplot(mensaje_enunciados, aes(1:nrow(mensaje_enunciados), NumPal)) +
  geom_bar(stat = 'identity') +
  geom_hline(yintercept = mean(mensaje_enunciados$NumPal), linetype = "dashed",
             colour = "red",
             size = 0.4) +
  geom_hline(yintercept = median(mensaje_enunciados$NumPal), linetype = "longdash",
             colour = "blue",
             size = 0.4) +
  labs(x = "Número de oración") +
  ggtitle("Número de palabras por oración", subtitle = "Mensaje de Navidad de 1975")
