#######################   Reinicio para Mensajes de Navidad    ###########################
#             Este fichero contiene el script para el reinicio del entorno               #
#          dentro del curso Análisis Automático de Textos y Estilomtería con R           #
#          del Laboratorio de Innovación en Humanidades Digitales (LinhdUNED)            #
#                                       edición 2019                                     #

# Proyecto 7PartidasDigital "Edición crítica digital de las Siete Partidas de Alfonso X" #
#       Proyecto financiado por el MINECO, referencia FFI2016-75014-P AEI-FEDER, EU      #
#              Universidad de Valladolid -- IP José Manuel Fradejas Rueda                #
#                            https://7partidas.hypotheses.org/                           #
#                            https://github.com/7PartidasDigital                         #
#                        Este material se distribuye con una licencia                    #
#                                            MIT                                         #
#                                         v. 1.0.0                                       #

# Establece el directorio. No lo olvides.
# Tiene que ser la carpeta AnaText

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

# Crear una lista de palabras vacías que puedan usarse para eliminar las nuestras desde tidyverse y cambia el nombre de la columna de word a palabra para que pueda comparar con lo nuestro
vacias_basic <- get_stopwords("es")
vacias <- vacias %>%
  rename(palabra = word)

#Crea una lista de palabras vacías más completas desde el repertorio de la asignatura
vacias <- 
  as_tibble(read.delim("https://raw.githubusercontent.com/7PartidasDigital/AnaText/master/datos/vacias.txt",
                       header = TRUE,
                       quote = '"',
                       encoding = "UTF-8",
                       stringsAsFactors = F))

#Vacíamos con la lista básica nuestra lista de mensajes del rey
mensajes_vaciado <- mensajes_palabras %>%
  anti_join(vacias)


#Miramos el número de palabras-tipo tras vaciarlas de palabras vacías
mensajes_vaciado %>%
  count(palabra, sort = T)

#Hacemos un gráfico representando las palabras-tipo llenas de significado más frecuentes
mensajes_vaciado %>%
  count(palabra, sort = T) %>%
  filter(n > 65) %>%
  mutate(palabra = reorder(palabra, n)) %>% 
  ggplot(aes(x = palabra, y = n, fill = palabra)) + 
  geom_bar(stat="identity") +
  theme_minimal() +
  theme(legend.position = "none") +
  ylab("Número de veces que aparecen") + xlab(NULL) +
  labs(
    title = "Mensajes de Navidad",
    subtitle = "1975-2018",
    caption = "Data from @Rebeca Ripa") +
  coord_flip()

#Empezamos a comparar por rey. Juan Carlos - 1975-2014. Felipe VI 2014-2018
#Agrupamos la variable año cambiándola por rey
mensajes_vaciado <- mensajes_vaciado %>%
  mutate(rey = anno) %>%
  mutate(rey = str_replace(rey, "201[45678]", "Felipe VI")) %>%
  mutate(rey = str_replace(rey, "\\d+", "Juan Carlos I"))

#Vemos una tabla con las diez palabras más frecuentes por cada rey (p.17)
mensajes_vaciado%>%
  group_by(rey) %>%
  count(palabra, sort = T) %>%
  top_n(10)
