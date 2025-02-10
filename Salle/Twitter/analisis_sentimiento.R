


library(readr)
library(dplyr)
library(ggplot2)
library(plotly)
library(htmlwidgets)


salle_sent <- read_csv("Salle/Twitter/salle_sent.csv")



evo_sent <- salle_sent %>%
  filter(Anio!=2025)%>%
  group_by(Anio, sentimiento) %>%
  summarise(conteo_posteos = n(), .groups = "drop") %>%
  group_by(Anio) %>%
  mutate(proporcion = round(conteo_posteos / sum(conteo_posteos)*100,1)) %>%
  mutate(Anio = as.factor(Anio))


#openxlsx::write.xlsx(evo_sent,"Salle/Twitter/Intermedias/evo_sent.xlsx")



grafico_sent <- ggplot(evo_sent, aes(x = Anio, y = proporcion, 
                                     color = sentimiento, 
                                     group = sentimiento,
                                     text = paste0("Sentimiento: ", sentimiento, "<br>",
                                                   "Año: ", Anio, "<br>",
                                                   "Porcentaje: ", paste(proporcion),"%"))) +
  geom_line(size = 1) + 
  geom_point(size = 2) +  
  labs(title = "Evolución de posteos por sentimiento y año") +
  theme_minimal(base_size = 14) +  
  theme(legend.position = "bottom",  
        plot.title = element_text(hjust = 0.5, face = "bold"),  
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_blank())  

grafico_interactivo_sent <- ggplotly(grafico_sent, tooltip = "text") %>%
  layout(legend = list(orientation = "h",  
                       x = 0.5,  
                       y = -0.2,  
                       xanchor = "center", 
                       yanchor = "top",
                       title = list(text = "")))  

grafico_interactivo_sent

saveWidget(grafico_interactivo_sent, "Salle/Twitter/plots/evo_sent.html", selfcontained = TRUE)





