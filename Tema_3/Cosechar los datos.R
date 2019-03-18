discurso <- readLines("datos/mensajes/1975.txt")
discurso
mensaje <- data_frame(parrafo = seq_along(discurso), texto = discurso)
mensajes_palabras <- mensaje %>%
  unnest_tokens(palabra, texto)
mensaje %>%
  unnest_tokens(palabra, texto, to_lower = FALSE)
oraciones <- mensaje %>%
  unnest_tokens(oraciones, texto, token= "sentences", to_lower = FALSE)
mensajes_palabras %>%
  count(palabra, sort = T)
mensaje_frecuencias <- mensajes_palabras %>%
  count(palabra, sort = T) %>%
  mutate(relativa = n / sum(n))
mensajes_enunciados <- mensaje %>%
  unnest_tokens(oraciones, texto, token = "sentences") %>%
  mutate(NumPal = str_count(oraciones, "\\w+"))
ggplot(mensajes_enunciados, aes(1:nrow(mensajes_enunciados), NumPal)) +
  geom_bar(stat = 'identity')
ggplot(mensajes_enunciados, aes(1:nrow(mensajes_enunciados), NumPal)) +
  geom_bar(stat = 'identity')+
  geom_hline(yintercept = mean(mensajes_enunciados$NumPal), linetype = "dashed", colour = "red", size = 0.4)+
  geom_hline(yintercept = median(mensajes_enunciados$NumPal), linetype = "dashed", colour = "blue", size = 0.4)+
  labs(x ="Número de oración")+
  labs(y = "Número de palabras por oración")+
  ggtitle("Número de palabras por oración", subtitle = "Mensaje de Navidad de 1975")
