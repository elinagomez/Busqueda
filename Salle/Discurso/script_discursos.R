# Instalar paquetes necesarios

library(tidyverse)
library(officer)

# Definir la ruta principal


ruta_principal <- "C:/Users/elina/OneDrive/Escritorio/Draper/Salle/Discurso/Salle"

# Función para procesar un archivo .docx
procesar_docx <- function(ruta_archivo) {
  # Leer el documento
  doc <- read_docx(ruta_archivo)
  
  # Extraer texto plano
  texto <- doc %>% 
    docx_summary() %>% 
    filter(content_type == "paragraph") %>% 
    pull(text) %>% 
    paste(collapse = "\n")
  
  # Extraer tablas y convertirlas en texto
  tablas <- tryCatch({
    doc %>%
      docx_summary() %>%
      filter(content_type == "table cell") %>%
      group_by(doc_index, table_id, row_id) %>%
      summarize(row_text = paste(text, collapse = " | "), .groups = "drop") %>%
      group_by(table_id) %>%
      summarize(tabla = paste(row_text, collapse = "\n"), .groups = "drop") %>%
      pull(tabla) %>%
      paste(collapse = "\n\n")
  }, error = function(e) {
    "" # Si no hay tablas, retorna vacío
  })
  
  # Combinar texto y tablas
  paste(texto, tablas, sep = "\n\n")
}

# Listar todas las subcarpetas de años
subcarpetas_año <- list.dirs(ruta_principal, full.names = TRUE, recursive = FALSE)

# Crear una lista para almacenar los datos
datos <- list()

# Iterar sobre las carpetas de años
for (subcarpeta_año in subcarpetas_año) {
  # Extraer el año de la subcarpeta
  año <- basename(subcarpeta_año)
  
  # Listar las carpetas de meses dentro del año
  subcarpetas_mes <- list.dirs(subcarpeta_año, full.names = TRUE, recursive = FALSE)
  
  # Iterar sobre las carpetas de meses
  for (subcarpeta_mes in subcarpetas_mes) {
    # Extraer el mes de la carpeta (ejemplo: 04-24 -> mes = 04)
    mes <- str_extract(basename(subcarpeta_mes), "^\\d{2}")
    
    # Listar los archivos docx en la carpeta del mes
    archivos <- list.files(subcarpeta_mes, pattern = "\\.docx$", full.names = TRUE)
    
    # Leer cada archivo
    for (archivo in archivos) {
      # Extraer el día del nombre del archivo (ejemplo: 1_4.docx -> día = 1)
      dia <- str_extract(basename(archivo), "^\\d+")
      
      # Procesar el contenido del archivo
      contenido <- procesar_docx(archivo)
      
      # Agregar la información al listado de datos
      datos <- append(datos, list(tibble(año = año, mes = mes, día = dia, contenido = contenido)))
    }
  }
}

# Consolidar todo en un único data frame
corpus <- bind_rows(datos)

#openxlsx::write.xlsx(corpus,"C:/Users/elina/OneDrive/Escritorio/Draper/Salle/Discurso/discursos_completo.xlsx")


##le pego la metadata

library(readxl)
library(dplyr)

# Leer el archivo de metadata (ajusta la ruta si es necesario)
ruta_metadata <- "C:/Users/elina/OneDrive/Escritorio/Draper/Salle/Discurso/METADATA{.xlsx" # Cambia esto por la ruta real del archivo
metadata <- openxlsx::read.xlsx(ruta_metadata,detectDates = TRUE)

# Extraer año y mes de la fecha completa en la metadata
metadata <- metadata %>%
  mutate(
    año = format(as.Date(Fecha, "%Y-%m-%d"), "%Y"), # Ajustar el formato si es necesario
    mes = format(as.Date(Fecha, "%Y-%m-%d"), "%m"),
    día = format(as.Date(Fecha, "%Y-%m-%d"), "%d")
  ) %>% distinct(año, mes,día, .keep_all = TRUE)

resultado <- corpus %>%
  left_join(metadata, by = c("año", "mes", "día"))


#openxlsx::write.xlsx(resultado,"C:/Users/elina/OneDrive/Escritorio/Draper/Salle/Discurso/discursos_completo_metadata.xlsx")

