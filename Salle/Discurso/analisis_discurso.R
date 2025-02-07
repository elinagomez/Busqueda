
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


base=openxlsx::read.xlsx("Salle/Discurso/discursos_completo_metadata.xlsx")

##Evolución de conteo de términos según categorías por año ------


diccionario <- dictionary(list(
  "Orden global y política" = c("2030", "agenda", "masón*", "masonería", "judío*", 
                                "corporatocracia", "casta", "davos", "israel*", 
                                "kissinger", "narco*", "plutocracia", "plutócrata", 
                                "plutocr**", "clepto*", "george soros", "agenda soros", 
                                "bill gates", "corporación*"),
  "Ambiente" = c("agua", "ambiente", "clima", "climático*", "upm", 
                 "hidrógeno verde", "forestal"),
  "Pandemia" = c("pandemia", "covid", "vacun*", "plandemia"),
  "Sistema judicial" = c("abogacía", "abogado*", "constitución*", "constitucional*", 
                         "corrupción", "corrupto*", "coimero*", "coima*", "fiscal*", 
                         "justicia")
))


# Crear el corpus y tokens
corpus_discursos <- corpus(base, text_field = "contenido")
tokens_discursos <- tokens(corpus_discursos, remove_punct = TRUE)
tokens_discursos <- tokens_tolower(tokens_discursos)

# Crear matriz de frecuencia de términos
dfm_discursos <- dfm(tokens_discursos)
dfm_categorias <- dfm_lookup(dfm_discursos, dictionary = diccionario)

# Agrupar por año
dfm_anual <- dfm_group(dfm_categorias, groups = base$año)
conteos <- convert(dfm_anual, to = "data.frame")

# Reestructurar en formato largo
conteos_tidy <- conteos %>%
  pivot_longer(
    cols = -doc_id, # Todas las columnas excepto 'doc_id' se convierten en valores largos
    names_to = "categoria", 
    values_to = "conteo"
  ) %>%
  rename(Anio = doc_id) 

# Calcular el total de palabras por año
dfm_tokens_anual <- dfm_group(dfm_discursos, groups = base$año)
total_palabras_anual <- rowSums(dfm_tokens_anual)

# Convertir a data frame
total_palabras_df <- data.frame(Anio = names(total_palabras_anual),
                                total_palabras = total_palabras_anual)

# Unir con los conteos de términos y calcular frecuencia por 1,000 palabras
conteos_tidy <- conteos_tidy %>%
  left_join(total_palabras_df, by = "Anio") %>%
  mutate(conteo_por_1000 = (conteo / total_palabras) * 1000)  # Normalización


#openxlsx::write.xlsx(conteos_tidy,"Salle/Discurso/Intermedias/conteos_terminos_discursos.xlsx")


# Definir colores para cada categoría
colores_categoria <- c(
  "Orden global y política" = "#1f78b4",
  "Ambiente" = "#33a02c",
  "Pandemia" = "#e31a1c",
  "Sistema judicial" = "#ff7f00"
)

# Crear el gráfico con ggplot
grafico_terminos <- ggplot(conteos_tidy, aes(x = Anio, y = round(conteo_por_1000,1), color = categoria, group = categoria,
                                             text = paste0("Categoría: ", categoria, "<br>",
                                                           "Año: ", Anio, "<br>",
                                                           "Menciones (cada 1000): ", round(conteo_por_1000,1)))) +
  geom_line(size = 1) + 
  geom_point(size = 2) +  
  scale_color_manual(values = colores_categoria) +  
  labs(title = "Evolución de menciones de términos (cada 1000) según categorías por año",
       x = "Año",
       y = "Cantidad de menciones",
       color = "Categoría") +
  theme_minimal(base_size = 14) +  
  theme(legend.position = "bottom",  
        plot.title = element_text(hjust = 0.5, face = "bold",size = 8),  
        axis.text.x = element_text(angle = 45, hjust = 1))  

# Convertir a gráfico interactivo con ggplotly
grafico_interactivo_terminos <- ggplotly(grafico_terminos, tooltip = "text") %>%
  layout(
    title = list(
      text = "Evolución de menciones de términos (cada 1000) según categorías por año",
      font = list(size = 15, face = "bold")  # Ajusta el tamaño y negrita del título
    ),
    legend = list(
      orientation = "h",
      x = 0.5,
      y = -0.2,
      xanchor = "center",
      yanchor = "top",
      title = list(text = "")
    )
  )


#saveWidget(grafico_interactivo_terminos, "Salle/Discurso/plots/evo_cat_discursos.html", selfcontained = TRUE)


### Mapas de calor de términos por categoría-----------


##Orden global y política

# Diccionario de términos
diccionario_orden <- dictionary(list(
  "Orden global y política" = c("agenda 2030", "masón*", "masonería", "judío*", 
                                "corporatocracia", "casta", "davos", "israel*", 
                                "kissinger", "narco*", "plutocracia", "plutócrata", 
                                "plutocr**", "clepto*", "george soros", "agenda soros", 
                                "bill gates", "corporación*")
))

corpus_tweets <- corpus(base, text_field = "contenido")
tokens_tweets <- tokens(corpus_tweets, remove_punct = TRUE)
tokens_tweets <- tokens_tolower(tokens_tweets)

dfm_tweets <- dfm(tokens_tweets)
dfm_tweets <- dfm_group(dfm_tweets, groups = docvars(dfm_tweets, "año"))

terminos_diccionario <- unlist(diccionario_orden)
dfm_filtrado <- dfm_select(dfm_tweets, pattern = terminos_diccionario)

data_dfm <- convert(dfm_filtrado, to = "data.frame") %>%
  pivot_longer(cols = -doc_id, names_to = "Termino", values_to = "Frecuencia") %>%
  filter(Frecuencia != 0) %>%
  rename(Anio = doc_id)

dfm_tokens_anual <- dfm_group(dfm_tweets, groups = docvars(dfm_tweets, "año"))
total_palabras_anual <- rowSums(dfm_tokens_anual)

total_palabras_df <- data.frame(Anio = names(total_palabras_anual),
                                total_palabras = total_palabras_anual)

data_dfm <- data_dfm %>%
  left_join(total_palabras_df, by = "Anio") %>%
  mutate(Frecuencia_normalizada = (Frecuencia / total_palabras) * 1000)

top_10_terminos <- data_dfm %>%
  group_by(Termino) %>%
  summarise(Promedio_Frecuencia = mean(Frecuencia_normalizada, na.rm = TRUE)) %>%
  arrange(desc(Promedio_Frecuencia)) %>%
  slice_head(n = 10)

data_dfm_top10_orden <- data_dfm %>%
  filter(Termino %in% top_10_terminos$Termino) %>%
  filter(Anio != 2025) %>%
  mutate(Termino = factor(Termino, levels = rev(top_10_terminos$Termino))) %>%
  mutate(Categoria = "Orden global y política")%>%
  mutate(Frecuencia_normalizada = round(Frecuencia_normalizada, 1))

plot_orden = ggplot(data_dfm_top10_orden, aes(x = Anio, y = Termino, fill = Frecuencia_normalizada)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "#1f78b4", name = "Frecuencia") +
  labs(title = "Frecuencia de términos más mencionados por año - Orden", x = "Año", y = "Términos") +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(size = 12, face = "bold"),legend.position = "none")

plot_orden <- ggplotly(plot_orden)


##Ambiente


diccionario_ambiente <- dictionary(list(
  "Ambiente" = c("agua", "ambiente", "clima", "climático*", "upm", 
                 "hidrógeno verde", "forestal")))

corpus_tweets <- corpus(base, text_field = "contenido")
tokens_tweets <- tokens(corpus_tweets, remove_punct = TRUE)
tokens_tweets <- tokens_tolower(tokens_tweets)

dfm_tweets <- dfm(tokens_tweets)
dfm_tweets <- dfm_group(dfm_tweets, groups = docvars(dfm_tweets, "año"))

terminos_diccionario <- unlist(diccionario_ambiente)
dfm_filtrado <- dfm_select(dfm_tweets, pattern = terminos_diccionario)

data_dfm <- convert(dfm_filtrado, to = "data.frame") %>%
  pivot_longer(cols = -doc_id, names_to = "Termino", values_to = "Frecuencia") %>%
  filter(Frecuencia != 0) %>%
  rename(Anio = doc_id)

dfm_tokens_anual <- dfm_group(dfm_tweets, groups = docvars(dfm_tweets, "año"))
total_palabras_anual <- rowSums(dfm_tokens_anual)

total_palabras_df <- data.frame(Anio = names(total_palabras_anual),
                                total_palabras = total_palabras_anual)

data_dfm <- data_dfm %>%
  left_join(total_palabras_df, by = "Anio") %>%
  mutate(Frecuencia_normalizada = (Frecuencia / total_palabras) * 1000)

top_10_terminos <- data_dfm %>%
  group_by(Termino) %>%
  summarise(Promedio_Frecuencia = mean(Frecuencia_normalizada, na.rm = TRUE)) %>%
  arrange(desc(Promedio_Frecuencia)) %>%
  slice_head(n = 10)

data_dfm_top10_ambiente <- data_dfm %>%
  filter(Termino %in% top_10_terminos$Termino) %>%
  filter(Anio != 2025) %>%
  mutate(Termino = factor(Termino, levels = rev(top_10_terminos$Termino))) %>%
  mutate(Categoria = "Ambiente")%>%
  mutate(Frecuencia_normalizada = round(Frecuencia_normalizada, 1))

plot_ambiente = ggplot(data_dfm_top10_ambiente, aes(x = Anio, y = Termino, fill = Frecuencia_normalizada)) +
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



corpus_tweets <- corpus(base, text_field = "contenido")
tokens_tweets <- tokens(corpus_tweets, remove_punct = TRUE)
tokens_tweets <- tokens_tolower(tokens_tweets)

dfm_tweets <- dfm(tokens_tweets)
dfm_tweets <- dfm_group(dfm_tweets, groups = docvars(dfm_tweets, "año"))

terminos_diccionario <- unlist(diccionario_pandemia)
dfm_filtrado <- dfm_select(dfm_tweets, pattern = terminos_diccionario)

data_dfm <- convert(dfm_filtrado, to = "data.frame") %>%
  pivot_longer(cols = -doc_id, names_to = "Termino", values_to = "Frecuencia") %>%
  filter(Frecuencia != 0) %>%
  rename(Anio = doc_id)

dfm_tokens_anual <- dfm_group(dfm_tweets, groups = docvars(dfm_tweets, "año"))
total_palabras_anual <- rowSums(dfm_tokens_anual)

total_palabras_df <- data.frame(Anio = names(total_palabras_anual),
                                total_palabras = total_palabras_anual)

data_dfm <- data_dfm %>%
  left_join(total_palabras_df, by = "Anio") %>%
  mutate(Frecuencia_normalizada = (Frecuencia / total_palabras) * 1000)

top_10_terminos <- data_dfm %>%
  group_by(Termino) %>%
  summarise(Promedio_Frecuencia = mean(Frecuencia_normalizada, na.rm = TRUE)) %>%
  arrange(desc(Promedio_Frecuencia)) %>%
  slice_head(n = 10)

data_dfm_top10_pandemia <- data_dfm %>%
  filter(Termino %in% top_10_terminos$Termino) %>%
  filter(Anio != 2025) %>%
  mutate(Termino = factor(Termino, levels = rev(top_10_terminos$Termino))) %>%
  mutate(Categoria = "Pandemia")%>%
  mutate(Frecuencia_normalizada = round(Frecuencia_normalizada, 1))

plot_pandemia = ggplot(data_dfm_top10_pandemia, aes(x = Anio, y = Termino, fill = Frecuencia_normalizada)) +
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




corpus_tweets <- corpus(base, text_field = "contenido")
tokens_tweets <- tokens(corpus_tweets, remove_punct = TRUE)
tokens_tweets <- tokens_tolower(tokens_tweets)

dfm_tweets <- dfm(tokens_tweets)
dfm_tweets <- dfm_group(dfm_tweets, groups = docvars(dfm_tweets, "año"))

terminos_diccionario <- unlist(diccionario_justicia)
dfm_filtrado <- dfm_select(dfm_tweets, pattern = terminos_diccionario)

data_dfm <- convert(dfm_filtrado, to = "data.frame") %>%
  pivot_longer(cols = -doc_id, names_to = "Termino", values_to = "Frecuencia") %>%
  filter(Frecuencia != 0) %>%
  rename(Anio = doc_id)

dfm_tokens_anual <- dfm_group(dfm_tweets, groups = docvars(dfm_tweets, "año"))
total_palabras_anual <- rowSums(dfm_tokens_anual)

total_palabras_df <- data.frame(Anio = names(total_palabras_anual),
                                total_palabras = total_palabras_anual)

data_dfm <- data_dfm %>%
  left_join(total_palabras_df, by = "Anio") %>%
  mutate(Frecuencia_normalizada = (Frecuencia / total_palabras) * 1000)

top_10_terminos <- data_dfm %>%
  group_by(Termino) %>%
  summarise(Promedio_Frecuencia = mean(Frecuencia_normalizada, na.rm = TRUE)) %>%
  arrange(desc(Promedio_Frecuencia)) %>%
  slice_head(n = 10)

data_dfm_top10_justicia <- data_dfm %>%
  filter(Termino %in% top_10_terminos$Termino) %>%
  filter(Anio != 2025) %>%
  mutate(Termino = factor(Termino, levels = rev(top_10_terminos$Termino))) %>%
  mutate(Categoria = "Sistema Judicial")%>%
  mutate(Frecuencia_normalizada = round(Frecuencia_normalizada, 1))

plot_justicia <- ggplot(data_dfm_top10_justicia, aes(x = Anio, y = Termino, fill = Frecuencia_normalizada)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "#ff7f00", name = "Frecuencia") +
  labs(title = "Frecuencia de términos más mencionados por año - Sistema Judicial", x = "Año", y = "Términos") +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(size = 12, face = "bold"),legend.position = "none")


plot_justicia = ggplotly(plot_justicia) 


fig <- subplot(plot_orden, plot_ambiente,
              plot_pandemia, plot_justicia, nrows = 2, titleY = FALSE, titleX = FALSE, margin = 0.1 )

fig <- fig %>% layout(
  title = 'Frecuencia relativa de términos (cada 1000) más mencionados en discursos por categoría y año',
  margin = list(t = 100),font = list(size = 11, face = "bold") 
)


annotations = list( 
  
  list( 
    
    x = 0.2,  
    
    y = 1.0,  
    
    text = "Orden global y política",  
    
    xref = "paper",  
    
    yref = "paper",  
    
    xanchor = "center",  
    
    yanchor = "bottom",  
    
    showarrow = FALSE 
    
  ),  
  
  list( 
    
    x = 0.8,  
    
    y = 1,  
    
    text = "Ambiente",  
    
    xref = "paper",  
    
    yref = "paper",  
    
    xanchor = "center",  
    
    yanchor = "bottom",  
    
    showarrow = FALSE 
    
  ),  
  
  list( 
    
    x = 0.2,  
    
    y = 0.4,  
    
    text = "Pandemia",  
    
    xref = "paper",  
    
    yref = "paper",  
    
    xanchor = "center",  
    
    yanchor = "bottom",  
    
    showarrow = FALSE 
    
  ),
  
  list( 
    
    x = 0.8,  
    
    y = 0.4,  
    
    text = "Sistema Judicial",  
    
    xref = "paper",  
    
    yref = "paper",  
    
    xanchor = "center",  
    
    yanchor = "bottom",  
    
    showarrow = FALSE 
    
  ))


fig <- fig %>%layout(annotations = annotations) 

fig

#saveWidget(fig, "Salle/Discurso/plots/heatmap_terminos_discursos.html", selfcontained = TRUE)


data_combined <- bind_rows(
  mutate(data_dfm_top10_orden),
  mutate(data_dfm_top10_pandemia),
  mutate(data_dfm_top10_ambiente),
  mutate(data_dfm_top10_justicia)
)

#openxlsx::write.xlsx(data_combined,"Salle/Discurso/Intermedias/data_heatmap_discursos.xlsx")

