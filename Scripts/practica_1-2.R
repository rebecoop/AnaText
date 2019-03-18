# Establece el directorio. No lo olvides.
# Tiene que ser la carpeta AnaText

# Carga las librerías
library(tidyverse)
library(tidytext)

#Carga los ficheros de los mensajes de navidad que terminen en .txt, serán 44
ficheros <- list.files(path = "datos/mensajes/", pattern = "*.txt")

#Extrae el dato del año que usaremos como variable para filtrar gracias al título de cada fichero que mantendrá asociado al mismo
anno <- gsub("\\.txt", "", ficheros, perl = T)

#Crea una tabla vacía con tres variables que tienen valores de letra o número
mensajes <- data_frame(anno = character(), 
                       parrafo = numeric(), 
                       texto = character())

#Hacemos un bucle que va a rellenar la tabla anterior
  #Recorrera la longitud total de los ficheros, en cada uno leerá los datos y extraerá año, párrafo y texto, lo meterá en temporal y luego de ahí lo trasladará a mensajes
for (i in 1:length(ficheros)){
  discurso <- readLines(paste("datos/mensajes/", ficheros[i], sep = "/"))
  temporal <- data_frame(anno = anno[i], parrafo = seq_along(discurso), texto = discurso)
  mensajes <- bind_rows(mensajes, temporal)
}
#Extraemos las palabras token de los mensajes 
mensajes_palabras <- mensajes %>%
  unnest_tokens(palabra, texto)

#Extraemos las palabras tipo y su frecuencia total + relativa de las token
mensajes_frecuencias <- mensajes_palabras %>%
  count(palabra, sort = T) %>%
  mutate(relativa = n / sum(n))

#Miramos los valores generales de las palabras tipo
summary(mensajes_palabras)

#Extraemos la media y mediana desde la frecuencia total de las palabras tipo
media <- mean (mensajes_frecuencias$n)
mediana <- median(mensajes_frecuencias$n)

#Agrupamos las frecuencias de las palabras tipo según año
frecuencias_anno <- mensajes_palabras %>%
  group_by(anno) %>%
  count(palabra, sort = T) %>%
  mutate(relativa = n / sum(n)) %>%
  ungroup()

#Filtramos de los datos anteriores un solo año
palabras_1992 <- frecuencias_anno %>%
  filter(anno == "1992")

#Calculamos el total de palabras token sumando la columna de la frecuencia
sum(palabras_1992$n)

#Calculamos el total del valor relativo de palabras tipo (tiene que dar 1)
sum(palabras_1992$relativa)

#Hacemos un gráfico que representa el numero de palabras por año
mensajes_palabras %>%
  group_by(anno) %>%
  count() %>%
  ggplot() +
  geom_bar(aes(anno, n),
           stat = 'identity',
           fill = "lightblue") +
  theme(legend.position = 'none',
        axis.text.x = element_text(angle=45, hjust =1)) +
  labs(x = "Año", y = "Número de palabras") +
  ggtitle("Mensajes de Navidad 1975-2017",
          subtitle = "Número de palabras en cada mensaje")
          
