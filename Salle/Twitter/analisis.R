
##Análisis

library(lubridate)
library(dplyr)
library(quanteda)
library(dplyr)
library(ggplot2)
library(tidyr)
library(plotly)
library(htmlwidgets)
library(lubridate)


base=openxlsx::read.xlsx("Salle/Twitter/base_final.xlsx")
base <- base %>%
  mutate(
    Fecha = ymd_hms(Time),
    Anio = year(Fecha),
    Mes = month(Fecha),
    Dia = day(Fecha),
    Fecha_corta = as.Date(Fecha)
  )

# Guardo para Fede
# openxlsx::write.xlsx(base,"Salle/Twitter/base_final_fecha.xlsx")


##Evolución de posteos por tipo y año ------------

evo_tipo <- base %>%
  group_by(Anio, comentario) %>%
  summarise(conteo_posteos = n(), .groups = "drop") %>%
  mutate(tipo_posteo = ifelse(comentario == 1, "Comentario", "Posteo original"))%>%
  mutate(tipo_posteo = factor(tipo_posteo, levels=c("Posteo original","Comentario")))%>%
  mutate(Anio=as.factor(Anio))%>%
  filter(Anio!=2025)


#openxlsx::write.xlsx(evo_tipo,"Salle/Twitter/Intermedias/evo_tipo.xlsx")

# Crear gráfico en ggplot
grafico_tipo <- ggplot(evo_tipo, aes(x = Anio, y = conteo_posteos, 
                                     color = tipo_posteo, 
                                     group = tipo_posteo,
                                     text = paste0("Tipo: ", tipo_posteo, "<br>",
                                                   "Año: ", Anio, "<br>",
                                                   "Cantidad: ", conteo_posteos))) +
  geom_line(size = 1) + 
  geom_point(size = 2) +  
  scale_color_manual(values = c("Comentario" = "#e31a1c", "Posteo original" = "#1f78b4")) +  
  labs(title = "Evolución de posteos por tipo y año",
       x = "Año",
       y = "Cantidad de posteos",
       color = "Tipo de Posteo") +
  theme_minimal(base_size = 14) +  
  theme(legend.position = "bottom",  
        plot.title = element_text(hjust = 0.5, face = "bold"),  
        axis.text.x = element_text(angle = 45, hjust = 1))  

# Convertir a gráfico interactivo con ggplotly
grafico_interactivo_tipo <- ggplotly(grafico_tipo, tooltip = "text") %>%
  layout(legend = list(orientation = "h",  
                       x = 0.5,  
                       y = -0.2,  
                       xanchor = "center", 
                       yanchor = "top",
                       title = list(text = "")))  

grafico_interactivo_tipo

saveWidget(grafico_interactivo_tipo, "Salle/Twitter/plots/grafico_tipo.html", selfcontained = TRUE)




##Posteos de cada categoría por año (al menos una mención)----------

# Definir las categorías y sus palabras clave
diccionario <- dictionary(list(
  "Orden global y política" = c("2030", "agenda", "masón", "masonería", "judío", 
                                "corporatocracia", "casta", "davos", "israel", 
                                "kissinger", "narco", "plutocracia", "plutócrata", 
                                "plutocr", "clepto", "george soros", "agenda soros", 
                                "bill gates", "corporación"),
  "Ambiente" = c("agua", "ambiente", "clima", "climático", "upm", 
                 "hidrógeno verde", "forestal"),
  "Pandemia" = c("pandemia", "covid", "vacuna", "plandemia"),
  "Sistema judicial" = c("abogacía", "abogado", "constitución", "constitucional", 
                         "corrupción", "corrupto", "coimero", "coima", "fiscal", 
                         "justicia"),
  "Género" = c("ideología de género", "homosexual*","feminis*"),
  "Otros" = c("comunismo",
              "capitalis*",
              "libertad",
              "vazquez","vázquez","tabaré",
              "medios de comunicación" )
  ))

# Crear corpus y tokens
corpus_tweets <- corpus(base, text_field = "Text")
tokens_tweets <- tokens(corpus_tweets, remove_punct = TRUE) %>%
  tokens_tolower()

# Crear el dfm y aplicar el diccionario de categorías
dfm_tweets <- dfm(tokens_tweets)
dfm_categorias <- dfm_lookup(dfm_tweets, dictionary = diccionario)

# Agregar el doc_id (identificador único para cada tweet)
dfm_categorias <- dfm_categorias %>%
  convert(to = "data.frame") %>%
  mutate(doc_id = row_number())  # Añadimos una columna doc_id con números secuenciales


baseunida <- dfm_categorias %>%
  pivot_longer(cols = -doc_id, names_to = "categoria", values_to = "mencionada") %>%
  filter(mencionada > 0) %>%
  left_join(base %>%
              mutate(doc_id = row_number())%>%
              mutate(Anio = as.factor(Anio))%>%
              select(doc_id, Anio,Text), by = c("doc_id" = "doc_id")) 



# Unir con la base original para obtener el año
conteos_categorias <- dfm_categorias %>%
  pivot_longer(cols = -doc_id, names_to = "categoria", values_to = "mencionada") %>%
  filter(mencionada > 0) %>%
  left_join(base %>%
              mutate(doc_id = row_number())%>%
              mutate(Anio = as.factor(Anio))%>%
              select(doc_id, Anio,Text), by = c("doc_id" = "doc_id")) %>%
  group_by(Anio, categoria) %>%
  summarise(conteo_posteos = n(), .groups = "drop")%>%
  filter(Anio!=2025)

openxlsx::write.xlsx(conteos_categorias,"Salle/Twitter/Intermedias/conteos_categorias.xlsx")


# Definir una paleta de colores personalizada para las categorías
colores_categoria <- c(
  "Orden global y política" = "#1f78b4",
  "Ambiente" = "#33a02c",
  "Pandemia" = "#e31a1c",
  "Sistema judicial" = "#ff7f00",
  "Género" ="#ead1dc",
  "Otros" = "#f3f3f3"
)

# Crear el gráfico con ggplot
grafico <- ggplot(conteos_categorias, aes(x = Anio, y = conteo_posteos, color = categoria, group = categoria, 
                                          text = paste0("Categoría: ", categoria, "<br>",
                                                        "Año: ", Anio, "<br>",
                                                        "Posteos: ", conteo_posteos))) +
  geom_line(size = 1) + 
  geom_point(size = 2) +  # Agrega puntos para resaltar valores
  scale_color_manual(values = colores_categoria) + # Aplicar colores definidos
  labs(title = "Evolución de posteos por categoría y año",
       x = "Año",
       y = "Cantidad de posteos",
       color = "Categoría") +
  theme_minimal(base_size = 14) +  # Ajustar tamaño de fuente
  theme(legend.position = "bottom",  # Ubicar la leyenda abajo
        plot.title = element_text(hjust = 0.5, face = "bold"), # Centrar y resaltar título
        axis.text.x = element_text(angle = 45, hjust = 1))  # Rotar etiquetas del eje X

# Convertir a gráfico interactivo con ggplotly y personalizar leyenda y tooltip
grafico_interactivo <- ggplotly(grafico, tooltip = "text") %>%
  layout(legend = list(orientation = "h",  # Horizontal
                       x = 0.5,  # Centrar en X
                       y = -0.2,  # Ubicar debajo del gráfico
                       xanchor = "center", 
                       yanchor = "top",
                       title = list(text = "")))  # Eliminar título de la leyenda



saveWidget(grafico_interactivo, "Salle/Twitter/plots/evo_cat.html", selfcontained = TRUE)



##Evolución de conteo de términos según categorías por año ------


corpus_tweets <- corpus(base, text_field = "Text")
tokens_tweets <- tokens(corpus_tweets, remove_punct = TRUE)
tokens_tweets <- tokens_tolower(tokens_tweets)
dfm_tweets <- dfm(tokens_tweets)
dfm_categorias <- dfm_lookup(dfm_tweets, dictionary = diccionario)
dfm_anual <- dfm_group(dfm_categorias, groups = base$Anio)
conteos <- convert(dfm_anual, to = "data.frame")

conteos_tidy <- conteos %>%
  pivot_longer(
    cols = -doc_id, # Todas las columnas excepto 'document' se convierten en valores largos
    names_to = "categoria", # Nombre de las columnas largas
    values_to = "conteo"    # Nombre de la columna con los valores
  )

conteos_tidy <- conteos_tidy %>%
  rename(Anio = doc_id) %>%  # Asumiendo que doc_id representa el año
  filter(Anio != 2025)  # Excluir 2025 si es necesario


openxlsx::write.xlsx(conteos_tidy,"Salle/Twitter/Intermedias/conteos_terminos.xlsx")


# Crear el gráfico con ggplot
grafico_terminos <- ggplot(conteos_tidy, aes(x = Anio, y = conteo, color = categoria, group = categoria,
                                             text = paste0("Categoría: ", categoria, "<br>",
                                                           "Año: ", Anio, "<br>",
                                                           "Términos mencionados: ", conteo))) +
  geom_line(size = 1) + 
  geom_point(size = 2) +  
  scale_color_manual(values = colores_categoria) +  
  labs(title = "Evolución de menciones de términos por categoría y año",
       x = "Año",
       y = "Cantidad de menciones",
       color = "Categoría") +
  theme_minimal(base_size = 14) +  
  theme(legend.position = "bottom",  
        plot.title = element_text(hjust = 0.5, face = "bold"),  
        axis.text.x = element_text(angle = 45, hjust = 1))  

# Convertir a gráfico interactivo con ggplotly
grafico_interactivo_terminos <- ggplotly(grafico_terminos, tooltip = "text") %>%
  layout(legend = list(orientation = "h",  
                       x = 0.5,  
                       y = -0.2,  
                       xanchor = "center", 
                       yanchor = "top",
                       title = list(text = "")))  


saveWidget(grafico_interactivo_terminos, "Salle/Twitter/plots/evo_terminos.html", selfcontained = TRUE)


### Mapas de calor de términos por categoría-----------


##Orden global y política


diccionario_orden <- dictionary(list(
  "Orden global y política" = c("agenda 2030", "masón*", "masonería", "judío*", 
                                "corporatocracia", "casta", "davos", "israel*", 
                                "kissinger", "narco*", "plutocracia", "plutócrata", 
                                "plutocr**", "clepto*", "george soros", "agenda soros", 
                                "bill gates", "corporación*")
))



corpus_tweets <- corpus(base, text_field = "Text")
tokens_tweets <- tokens(corpus_tweets, remove_punct = TRUE)
tokens_tweets <- tokens_tolower(tokens_tweets)
dfm_tweets <- dfm(tokens_tweets)
dfm_tweets <- dfm_group(dfm_tweets, groups = docvars(dfm_tweets, "Anio"))


terminos_diccionario <- unlist(diccionario_orden)
dfm_filtrado <- dfm_select(dfm_tweets, pattern = terminos_diccionario)

data_dfm <- convert(dfm_filtrado, to = "data.frame") %>%
  pivot_longer(cols = -doc_id, names_to = "Termino", values_to = "Frecuencia") %>%
  filter(Frecuencia != 0)

##tengo todos los términos que dice 

##me quedo con los 10 que más se dicen

# Calcular la frecuencia promedio por término a lo largo de todos los años
promedio_frecuencia <- data_dfm %>%
  group_by(Termino) %>%
  summarise(Promedio_Frecuencia = mean(Frecuencia)) %>%
  arrange(desc(Promedio_Frecuencia))

# Seleccionar los 10 términos más mencionados en promedio
top_10_terminos <- head(promedio_frecuencia, 10)



data_dfm_top10_orden <- data_dfm %>%
  filter(Termino%in%top_10_terminos$Termino)%>%
  rename(Anio = doc_id) %>%
  filter(Anio!=2025)%>%
  mutate(Termino = factor(Termino, levels = rev(top_10_terminos$Termino)))%>%
  mutate(Categoria = "Orden global y política")

plot_orden = ggplot(data_dfm_top10_orden, aes(x = Anio, y = Termino, fill = Frecuencia)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "#1f78b4", name = "Frecuencia") +
  labs(title = "Frecuencia de términos más mencionados por año - Orden global y política", x = "Año", y = "Términos") +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(size = 12, face = "bold"),
    legend.position = "none")
    

plot_orden= ggplotly(plot_orden)


##Ambiente


diccionario_ambiente <- dictionary(list(
  "Ambiente" = c("agua", "ambiente", "clima", "climático*", "upm", 
                 "hidrógeno verde", "forestal")))



corpus_tweets <- corpus(base, text_field = "Text")
tokens_tweets <- tokens(corpus_tweets, remove_punct = TRUE)
tokens_tweets <- tokens_tolower(tokens_tweets)
dfm_tweets <- dfm(tokens_tweets)
dfm_tweets <- dfm_group(dfm_tweets, groups = docvars(dfm_tweets, "Anio"))


terminos_diccionario <- unlist(diccionario_ambiente)
dfm_filtrado <- dfm_select(dfm_tweets, pattern = terminos_diccionario)

data_dfm <- convert(dfm_filtrado, to = "data.frame") %>%
  pivot_longer(cols = -doc_id, names_to = "Termino", values_to = "Frecuencia") %>%
  filter(Frecuencia != 0)

##tengo todos los términos que dice 

##me quedo con los 10 que más se dicen

# Calcular la frecuencia promedio por término a lo largo de todos los años
promedio_frecuencia <- data_dfm %>%
  group_by(Termino) %>%
  summarise(Promedio_Frecuencia = mean(Frecuencia)) %>%
  arrange(desc(Promedio_Frecuencia))

# Seleccionar los 10 términos más mencionados en promedio
top_10_terminos <- head(promedio_frecuencia, 10)



data_dfm_top10_ambiente <- data_dfm %>%
  filter(Termino%in%top_10_terminos$Termino)%>%
  rename(Anio = doc_id) %>%
  filter(Anio!=2025)%>%
  mutate(Termino = factor(Termino, levels = rev(top_10_terminos$Termino)))%>%
  mutate(Categoria = "Ambiente")

plot_ambiente = ggplot(data_dfm_top10_ambiente, aes(x = Anio, y = Termino, fill = Frecuencia)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "#33a02c", name = "Frecuencia") +
  labs(title = "Frecuencia de términos más mencionados por año - Ambiente", x = "Año", y = "Términos") +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(size = 12, face = "bold"),legend.position = "none")


plot_ambiente = ggplotly(plot_ambiente) 



##Pandemia


diccionario_pandemia <- dictionary(list(
  "Pandemia" = c("pandemia", "covid", "vacun*", "plandemia")))



corpus_tweets <- corpus(base, text_field = "Text")
tokens_tweets <- tokens(corpus_tweets, remove_punct = TRUE)
tokens_tweets <- tokens_tolower(tokens_tweets)
dfm_tweets <- dfm(tokens_tweets)
dfm_tweets <- dfm_group(dfm_tweets, groups = docvars(dfm_tweets, "Anio"))


terminos_diccionario <- unlist(diccionario_pandemia)
dfm_filtrado <- dfm_select(dfm_tweets, pattern = terminos_diccionario)

data_dfm <- convert(dfm_filtrado, to = "data.frame") %>%
  pivot_longer(cols = -doc_id, names_to = "Termino", values_to = "Frecuencia") %>%
  filter(Frecuencia != 0)

##tengo todos los términos que dice 

##me quedo con los 10 que más se dicen

# Calcular la frecuencia promedio por término a lo largo de todos los años
promedio_frecuencia <- data_dfm %>%
  group_by(Termino) %>%
  summarise(Promedio_Frecuencia = mean(Frecuencia)) %>%
  arrange(desc(Promedio_Frecuencia))

# Seleccionar los 10 términos más mencionados en promedio
top_10_terminos <- head(promedio_frecuencia, 10)



data_dfm_top10_pandemia <- data_dfm %>%
  filter(Termino%in%top_10_terminos$Termino)%>%
  rename(Anio = doc_id) %>%
  filter(Anio!=2025)%>%
  mutate(Termino = factor(Termino, levels = rev(top_10_terminos$Termino)))%>%
  mutate(Categoria = "Pandemia")

plot_pandemia = ggplot(data_dfm_top10_pandemia, aes(x = Anio, y = Termino, fill = Frecuencia)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "#e31a1c", name = "Frecuencia") +
  labs(title = "Frecuencia de términos más mencionados por año - Pandemia", x = "Año", y = "Términos") +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(size = 12, face = "bold"),legend.position = "none")


plot_pandemia= ggplotly(plot_pandemia) 


##Justicia


diccionario_justicia <- dictionary(list(
  "Sistema judicial" = c("abogacía", "abogado*", "constitución*", "constitucional*", 
                         "corrupción", "corrupto*", "coimero*", "coima*", "fiscal*", 
                         "justicia")))



corpus_tweets <- corpus(base, text_field = "Text")
tokens_tweets <- tokens(corpus_tweets, remove_punct = TRUE)
tokens_tweets <- tokens_tolower(tokens_tweets)
dfm_tweets <- dfm(tokens_tweets)
dfm_tweets <- dfm_group(dfm_tweets, groups = docvars(dfm_tweets, "Anio"))


terminos_diccionario <- unlist(diccionario_justicia)
dfm_filtrado <- dfm_select(dfm_tweets, pattern = terminos_diccionario)

data_dfm <- convert(dfm_filtrado, to = "data.frame") %>%
  pivot_longer(cols = -doc_id, names_to = "Termino", values_to = "Frecuencia") %>%
  filter(Frecuencia != 0)

##tengo todos los términos que dice 

##me quedo con los 10 que más se dicen

# Calcular la frecuencia promedio por término a lo largo de todos los años
promedio_frecuencia <- data_dfm %>%
  group_by(Termino) %>%
  summarise(Promedio_Frecuencia = mean(Frecuencia)) %>%
  arrange(desc(Promedio_Frecuencia))

# Seleccionar los 10 términos más mencionados en promedio
top_10_terminos <- head(promedio_frecuencia, 10)



data_dfm_top10_justicia <- data_dfm %>%
  filter(Termino%in%top_10_terminos$Termino)%>%
  rename(Anio = doc_id) %>%
  filter(Anio!=2025)%>%
  mutate(Termino = factor(Termino, levels = rev(top_10_terminos$Termino)))%>%
  mutate(Categoria = "Sistema Judicial")



plot_justicia = ggplot(data_dfm_top10_justicia, aes(x = Anio, y = Termino, fill = Frecuencia)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "#ff7f00", name = "Frecuencia") +
  labs(title = "Frecuencia de términos más mencionados por año - Sistema Judicial", x = "Año", y = "Términos") +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(size = 12, face = "bold"),legend.position = "none")


plot_justicia = ggplotly(plot_justicia) 


##Género


diccionario_genero <- dictionary(list(
  "Género" = c("ideología de género", "homosexual*","feminis*","género")))



corpus_tweets <- corpus(base, text_field = "Text")
tokens_tweets <- tokens(corpus_tweets, remove_punct = TRUE)
tokens_tweets <- tokens_tolower(tokens_tweets)
dfm_tweets <- dfm(tokens_tweets)
dfm_tweets <- dfm_group(dfm_tweets, groups = docvars(dfm_tweets, "Anio"))


terminos_diccionario <- unlist(diccionario_genero)
dfm_filtrado <- dfm_select(dfm_tweets, pattern = terminos_diccionario)

data_dfm <- convert(dfm_filtrado, to = "data.frame") %>%
  pivot_longer(cols = -doc_id, names_to = "Termino", values_to = "Frecuencia") %>%
  filter(Frecuencia != 0)

##tengo todos los términos que dice 

##me quedo con los 10 que más se dicen

# Calcular la frecuencia promedio por término a lo largo de todos los años
promedio_frecuencia <- data_dfm %>%
  group_by(Termino) %>%
  summarise(Promedio_Frecuencia = mean(Frecuencia)) %>%
  arrange(desc(Promedio_Frecuencia))

# Seleccionar los 10 términos más mencionados en promedio
top_10_terminos <- head(promedio_frecuencia, 10)


data_dfm_top10_genero <- data_dfm %>%
  filter(Termino%in%top_10_terminos$Termino)%>%
  rename(Anio = doc_id) %>%
  filter(Anio!=2025)%>%
  mutate(Termino = factor(Termino, levels = rev(top_10_terminos$Termino)))%>%
  mutate(Categoria = "Género")



plot_genero = ggplot(data_dfm_top10_genero, aes(x = Anio, y = Termino, fill = Frecuencia)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "#ead1dc", name = "Frecuencia") +
  labs(title = "Frecuencia de términos más mencionados por año - Género", x = "Año", y = "Términos") +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(size = 12, face = "bold"),legend.position = "none")


plot_genero = ggplotly(plot_genero) 




##Otros


diccionario_otros <- dictionary(list(
  "Género" = c("comunismo",
                 "capitalis*",
                 "libertad",
                 "vazquez","vázquez","tabaré",
                 "medios de comunicación" )))



corpus_tweets <- corpus(base, text_field = "Text")
tokens_tweets <- tokens(corpus_tweets, remove_punct = TRUE)
tokens_tweets <- tokens_tolower(tokens_tweets)
dfm_tweets <- dfm(tokens_tweets)
dfm_tweets <- dfm_group(dfm_tweets, groups = docvars(dfm_tweets, "Anio"))


terminos_diccionario <- unlist(diccionario_otros)
dfm_filtrado <- dfm_select(dfm_tweets, pattern = terminos_diccionario)

data_dfm <- convert(dfm_filtrado, to = "data.frame") %>%
  pivot_longer(cols = -doc_id, names_to = "Termino", values_to = "Frecuencia") %>%
  filter(Frecuencia != 0)

##tengo todos los términos que dice 

##me quedo con los 10 que más se dicen

# Calcular la frecuencia promedio por término a lo largo de todos los años
promedio_frecuencia <- data_dfm %>%
  group_by(Termino) %>%
  summarise(Promedio_Frecuencia = mean(Frecuencia)) %>%
  arrange(desc(Promedio_Frecuencia))

# Seleccionar los 10 términos más mencionados en promedio
top_10_terminos <- head(promedio_frecuencia, 10)


data_dfm_top10_otros <- data_dfm %>%
  filter(Termino%in%top_10_terminos$Termino)%>%
  rename(Anio = doc_id) %>%
  filter(Anio!=2025)%>%
  mutate(Termino = factor(Termino, levels = rev(top_10_terminos$Termino)))%>%
  mutate(Categoria = "Otros")



plot_otros = ggplot(data_dfm_top10_otros, aes(x = Anio, y = Termino, fill = Frecuencia)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "#f3f3f3", name = "Frecuencia") +
  labs(title = "Frecuencia de términos más mencionados por año - Otros", x = "Año", y = "Términos") +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(size = 12, face = "bold"),legend.position = "none")


plot_otros = ggplotly(plot_otros) 



##uno todo





library(plotly)

fig <- subplot(plot_orden, plot_ambiente, 
               plot_pandemia, plot_justicia, 
               plot_genero, plot_otros,
               nrows = 3, titleY = FALSE, titleX = FALSE, margin = 0.1)

# Anotaciones para los títulos de cada gráfico en la grilla 3x2
annotations = list( 
  list( x = 0.25, y = 1.0, text = "Orden global y política", xref = "paper", yref = "paper", 
        xanchor = "center", yanchor = "bottom", showarrow = FALSE, font = list(size = 12)),  
  list( x = 0.75, y = 1.0, text = "Ambiente", xref = "paper", yref = "paper", 
        xanchor = "center", yanchor = "bottom", showarrow = FALSE, font = list(size = 12)),  
  list( x = 0.25, y = 0.65, text = "Pandemia", xref = "paper", yref = "paper", 
        xanchor = "center", yanchor = "bottom", showarrow = FALSE, font = list(size = 12)),
  list( x = 0.75, y = 0.65, text = "Sistema Judicial", xref = "paper", yref = "paper", 
        xanchor = "center", yanchor = "bottom", showarrow = FALSE, font = list(size = 12)),
  list( x = 0.25, y = 0.3, text = "Género", xref = "paper", yref = "paper", 
        xanchor = "center", yanchor = "bottom", showarrow = FALSE, font = list(size = 12)),
  list( x = 0.75, y = 0.3, text = "Otros", xref = "paper", yref = "paper", 
        xanchor = "center", yanchor = "bottom", showarrow = FALSE, font = list(size = 12))
)

# Aplicar layout en una sola llamada para evitar sobreescribir configuraciones
fig <- fig %>% layout(
  title = 'Frecuencia de términos más mencionados por categoría y año',
  margin = list(t = 100),
  annotations = annotations,
  xaxis = list(tickfont = list(size = 10)),  
  xaxis2 = list(tickfont = list(size = 10)),  
  xaxis3 = list(tickfont = list(size = 10)),  
  xaxis4 = list(tickfont = list(size = 10)),  
  xaxis5 = list(tickfont = list(size = 10)),  
  xaxis6 = list(tickfont = list(size = 10)),  
  yaxis = list(tickfont = list(size = 10)),  
  yaxis2 = list(tickfont = list(size = 10)),  
  yaxis3 = list(tickfont = list(size = 10)),  
  yaxis4 = list(tickfont = list(size = 10)),  
  yaxis5 = list(tickfont = list(size = 10)),  
  yaxis6 = list(tickfont = list(size = 10))  
)

fig
saveWidget(fig, "Salle/Twitter/plots/heatmap_terminos.html", selfcontained = TRUE)


# library(ggplot2)
# library(plotly)
# library(dplyr)
# 
# # Unir los datos de los gráficos en un solo dataframe
# data_combined <- bind_rows(
#   data_dfm_top10_orden,
#   data_dfm_top10_pandemia,
#  data_dfm_top10_ambiente,
#   data_dfm_top10_genero,
#   data_dfm_top10_otros
# )
# 
# openxlsx::write.xlsx(data_combined,"Salle/Twitter/Intermedias/data_heatmap.xlsx")



#Análisis de interacciones


library(dplyr)
library(stringr)
library(tidyr)

# Filtrar los posteos que son respuestas a otros usuarios
respuestas <- base %>% filter(comentario==1)

# Extraer menciones en el texto con el símbolo @
respuestas <- respuestas %>%
  mutate(menciones = str_extract_all(Text, "@\\w+")) %>%  # Extrae menciones
  unnest(menciones) %>%  # Desanidar la lista
  mutate(menciones = str_remove(menciones, "@"))  # Eliminar el símbolo @



top_cuentas <- respuestas %>%
  count(menciones, sort = TRUE) %>%
  top_n(15, n)  # Seleccionar las 50 más mencionadas

interacciones_por_mes <- respuestas %>%
  filter(menciones%in%top_cuentas$menciones)%>%
  mutate(fecha = as.Date(Fecha_corta)) %>%
  mutate(mes = format(fecha, "%Y-%m")) %>%
  group_by(mes) %>%
  count(menciones) 



##Evolución de 10 cuentas principales

# Crear las cuentas más mencionadas
top_cuentas <- respuestas %>%
  count(menciones, sort = TRUE) %>%
  top_n(10, n)  # Selecciona las 15 más mencionadas

interacciones_por_periodo <- respuestas %>%
  filter(menciones %in% top_cuentas$menciones) %>%
  mutate(fecha = as.Date(Fecha_corta),
         periodo = floor_date(fecha, unit = "1 year")) %>%
  group_by(periodo, menciones) %>%
  summarise(conteo = n(), .groups = "drop")


#openxlsx::write.xlsx(interacciones_por_periodo,"Salle/Twitter/Intermedias/interacciones_por_periodo.xlsx")



# Crear gráfico con ggplot
grafico <- ggplot(interacciones_por_periodo, aes(x = periodo, y = conteo, 
                                                 color = menciones, group = menciones, 
                                                 text = paste0("Cuenta: ", menciones, "<br>",
                                                               "Periodo: ", format(periodo, "%b %Y"), "<br>",
                                                               "Interacciones: ", conteo))) +
  geom_line(size = 1) + 
  geom_point(size = 2) +  
  #scale_color_manual(values = colores_cuentas) +  
  labs(title = "Evolución de interacción con 10 cuentas más mencionadas",
       x = "Periodo (4 meses)",
       y = "Cantidad de interacciones",
       color = "Cuenta Mencionada") +
  theme_minimal(base_size = 14) +  
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, face = "bold"),  
        axis.text.x = element_text(angle = 45, hjust = 1),
          axis.title.x = element_blank())  

# Convertir a gráfico interactivo con ggplotly
grafico_interactivo <- ggplotly(grafico, tooltip = "text") %>%
  layout(legend = list(orientation = "h",  
                       x = 0.5,  
                       y = -0.2,  
                       xanchor = "center", 
                       yanchor = "top",
                       title = list(text = "")))  

grafico_interactivo
saveWidget(grafico_interactivo, "Salle/Twitter/plots/evo_interacciones.html", selfcontained = TRUE)



###menciones sobre parlamento







# Definir las categorías y sus palabras clave
diccionario <- dictionary(list(
  "Parlamento" = c("parlamento","diputado*","leyes","proyecto de ley","nicole","casta",
                                "derogar",
                                "propuesta",
                                "denunciar",
                                "proponer",
                                "comisión",
                                "comisión de salud")))

# Crear corpus y tokens
corpus_tweets <- corpus(base, text_field = "Text")
tokens_tweets <- tokens(corpus_tweets, remove_punct = TRUE) %>%
  tokens_tolower()

# Crear el dfm y aplicar el diccionario de categorías
dfm_tweets <- dfm(tokens_tweets)
dfm_categorias <- dfm_lookup(dfm_tweets, dictionary = diccionario)

# Agregar el doc_id (identificador único para cada tweet)
dfm_categorias <- dfm_categorias %>%
  convert(to = "data.frame") %>%
  mutate(doc_id = row_number())  # Añadimos una columna doc_id con números secuenciales


baseunida <- dfm_categorias %>%
  pivot_longer(cols = -doc_id, names_to = "categoria", values_to = "mencionada") %>%
  filter(mencionada > 0) %>%
  left_join(base %>%
              mutate(doc_id = row_number())%>%
              mutate(Anio = as.factor(Anio))%>%
              select(doc_id, Anio,Text), by = c("doc_id" = "doc_id")) 



# Unir con la base original para obtener el año
conteos_categorias <- dfm_categorias %>%
  pivot_longer(cols = -doc_id, names_to = "categoria", values_to = "mencionada") %>%
  filter(mencionada > 0) %>%
  left_join(base %>%
              mutate(doc_id = row_number())%>%
              mutate(Anio = as.factor(Anio))%>%
              select(doc_id, Anio,Text), by = c("doc_id" = "doc_id")) %>%
  group_by(Anio, categoria) %>%
  summarise(conteo_posteos = n(), .groups = "drop") %>%
  filter(Anio!=2025)

openxlsx::write.xlsx(conteos_categorias,"Salle/Twitter/Intermedias/conteos_categorias.xlsx")


# Definir una paleta de colores personalizada para las categorías
colores_categoria <- c(
  "Parlamento" = "#1f78b4"
)

# Crear el gráfico con ggplot
grafico <- ggplot(conteos_categorias, aes(x = Anio, y = conteo_posteos, color = categoria, group = categoria, 
                                          text = paste0("Categoría: ", categoria, "<br>",
                                                        "Año: ", Anio, "<br>",
                                                        "Posteos: ", conteo_posteos))) +
  geom_line(size = 1) + 
  geom_point(size = 2) +  # Agrega puntos para resaltar valores
  scale_color_manual(values = colores_categoria) + # Aplicar colores definidos
  labs(title = "Evolución de posteos sobre parlamento por año",
       x = "Año",
       y = "Cantidad de posteos",
       color = "Categoría") +
  theme_minimal(base_size = 14) +  # Ajustar tamaño de fuente
  theme(legend.position = "bottom",  # Ubicar la leyenda abajo
        plot.title = element_text(hjust = 0.5, face = "bold"), # Centrar y resaltar título
        axis.text.x = element_text(angle = 45, hjust = 1))  # Rotar etiquetas del eje X

# Convertir a gráfico interactivo con ggplotly y personalizar leyenda y tooltip
grafico_interactivo <- ggplotly(grafico, tooltip = "text") %>%
  layout(legend = list(orientation = "h",  # Horizontal
                       x = 0.5,  # Centrar en X
                       y = -0.2,  # Ubicar debajo del gráfico
                       xanchor = "center", 
                       yanchor = "top",
                       title = list(text = "")))  # Eliminar título de la leyenda



saveWidget(grafico_interactivo, "Salle/Twitter/plots/evo_parlamento.html", selfcontained = TRUE)




corpus_tweets <- corpus(base, text_field = "Text")
tokens_tweets <- tokens(corpus_tweets, remove_punct = TRUE)
tokens_tweets <- tokens_tolower(tokens_tweets)
dfm_tweets <- dfm(tokens_tweets)
dfm_categorias <- dfm_lookup(dfm_tweets, dictionary = diccionario)
dfm_anual <- dfm_group(dfm_categorias, groups = base$Anio)
conteos <- convert(dfm_anual, to = "data.frame")

conteos_tidy <- conteos %>%
  pivot_longer(
    cols = -doc_id, # Todas las columnas excepto 'document' se convierten en valores largos
    names_to = "categoria", # Nombre de las columnas largas
    values_to = "conteo"    # Nombre de la columna con los valores
  )

conteos_tidy <- conteos_tidy %>%
  rename(Anio = doc_id) %>%  # Asumiendo que doc_id representa el año
  filter(Anio != 2025)  # Excluir 2025 si es necesario


openxlsx::write.xlsx(conteos_tidy,"Salle/Twitter/Intermedias/conteos_terminos_parlamento.xlsx")


# Crear el gráfico con ggplot
grafico_terminos <- ggplot(conteos_tidy, aes(x = Anio, y = conteo, color = categoria, group = categoria,
                                             text = paste0("Categoría: ", categoria, "<br>",
                                                           "Año: ", Anio, "<br>",
                                                           "Términos mencionados: ", conteo))) +
  geom_line(size = 1) + 
  geom_point(size = 2) +  
  scale_color_manual(values = colores_categoria) +  
  labs(title = "Evolución de menciones de términos sobre parlamento por año",
       x = "Año",
       y = "Cantidad de menciones",
       color = "Categoría") +
  theme_minimal(base_size = 14) +  
  theme(legend.position = "bottom",  
        plot.title = element_text(hjust = 0.5, face = "bold"),  
        axis.text.x = element_text(angle = 45, hjust = 1))  

# Convertir a gráfico interactivo con ggplotly
grafico_interactivo_terminos <- ggplotly(grafico_terminos, tooltip = "text") %>%
  layout(legend = list(orientation = "h",  
                       x = 0.5,  
                       y = -0.2,  
                       xanchor = "center", 
                       yanchor = "top",
                       title = list(text = "")))  


saveWidget(grafico_interactivo_terminos, "Salle/Twitter/plots/evo_terminos_parlamento.html", selfcontained = TRUE)



##Hatmap- parlameto -------------


corpus_tweets <- corpus(base, text_field = "Text")
tokens_tweets <- tokens(corpus_tweets, remove_punct = TRUE)
tokens_tweets <- tokens_tolower(tokens_tweets)
dfm_tweets <- dfm(tokens_tweets)
dfm_tweets <- dfm_group(dfm_tweets, groups = docvars(dfm_tweets, "Anio"))


terminos_diccionario <- unlist(diccionario)
dfm_filtrado <- dfm_select(dfm_tweets, pattern = terminos_diccionario)

data_dfm <- convert(dfm_filtrado, to = "data.frame") %>%
  pivot_longer(cols = -doc_id, names_to = "Termino", values_to = "Frecuencia") %>%
  filter(Frecuencia != 0)

##tengo todos los términos que dice 

##me quedo con los 10 que más se dicen

# Calcular la frecuencia promedio por término a lo largo de todos los años
promedio_frecuencia <- data_dfm %>%
  group_by(Termino) %>%
  summarise(Promedio_Frecuencia = mean(Frecuencia)) %>%
  arrange(desc(Promedio_Frecuencia))

# Seleccionar los 10 términos más mencionados en promedio
top_10_terminos <- head(promedio_frecuencia, 10)



data_dfm_top10_parlamento <- data_dfm %>%
  filter(Termino%in%top_10_terminos$Termino)%>%
  rename(Anio = doc_id) %>%
  filter(Anio!=2025)%>%
  mutate(Termino = factor(Termino, levels = rev(top_10_terminos$Termino)))%>%
  mutate(Categoria = "Parlamento")

openxlsx::write.xlsx(data_dfm_top10_parlamento,"Salle/Twitter/Intermedias/data_heatmap_parlamento.xlsx")


plot_parlamento = ggplot(data_dfm_top10_parlamento, aes(x = Anio, y = Termino, fill = Frecuencia)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "#1f78b4", name = "Frecuencia") +
  labs(title = "Frecuencia de términos más mencionados por año - Parlemento", x = "Año", y = "Términos") +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(size = 12, face = "bold"),legend.position = "none")


plot_parlamento = ggplotly(plot_parlamento) 


saveWidget(plot_parlamento, "Salle/Twitter/plots/heatmap_terminos_parlamento.html", selfcontained = TRUE)

