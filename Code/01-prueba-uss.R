# Solemne Universidad San Sebastian


### Cargar Librerias----

library(haven)
library(dplyr)
library(tidyr)
library(stringr)
library(labelled)
library(data.table)
library(readxl)
library(readr)
library(purrr)
library(haven)
library(dplyr)
library(tidyr)
library(stringr)
library(labelled)
library(data.table)
library(ggplot2)
library(shinydashboard)
library(shiny)
library(shinythemes)
library(DT)
library(maps)
library(mapdata)
library(leaflet)
library(rnaturalearth)
library(sf)
library(plotly)
library(officer)
library(flextable)
library(viridis)
library(here)
library(glue)
library(colourpicker)
library(wbstats)
library(htmlwidgets)
library(bs4Dash)
library(countrycode)
library(bslib)
library(lubridate)
library(scales)
library(patchwork)
library(zoo)
library(scales)
library(purrr)
library(forcats)
library(readxl)
library(stringi)  
library(readxl)


#Directorios---- AQUI CAMBIAR!  


data_path_in <- "C:/Users/wb631166/OneDrive - WBG/Desktop/Prueba USS"

data_path_out <- "C:/Users/wb631166/OneDrive - WBG/Desktop/Prueba USS/Outputs"

#Cargar base de datos ----

computadores <- read_csv(
  file = file.path(data_path_in, "Recurso para resolución de solemne (estudiantes)_A.csv")
)

#View data prices

View(computadores)
head(computadores)
n_distinct(computadores) #281
nrow(computadores) # 339 observations #2 variables 
glimpse(computadores)

#Varianbles
#precio: precio del computador, en miles de pesos
#dd: capacidad del disco duro, en gigabytes (GB)

#Introduccion ----

RUT <- 13572743   # sin dígito verificador ni puntos

set.seed(RUT)
datos <- computadores[sample(nrow(computadores), 70), ]

rm(computadores)  # elimina los datos originales

#Pregunta 1 ----

resumen_precios <- datos %>% 
  summarise(
    precio_promedio_miles = mean(precio, na.rm = TRUE),
    precio_min_miles      = min(precio, na.rm = TRUE),
    precio_max_miles      = max(precio, na.rm = TRUE),
    
    # mismos valores pero en pesos
    precio_promedio_pesos = precio_promedio_miles * 1000,
    precio_min_pesos      = precio_min_miles * 1000,
    precio_max_pesos      = precio_max_miles * 1000
  )

resumen_precios

#Tabla exportable 

tabla_resumen <- resumen_precios %>%
  transmute(
    `Precio promedio (pesos)` = round(precio_promedio_pesos, 0),
    `Precio mínimo (pesos)`  = round(precio_min_pesos, 0),
    `Precio máximo (pesos)`  = round(precio_max_pesos, 0)
  )

# Tabla exportable con flextable

tabla_flex <- flextable(tabla_resumen) %>%
  theme_vanilla() %>%
  autofit()

tabla_flex  

# Exportar a Word

read_docx() |>
  body_add_par("Resumen de precios de computadores (muestra de 70)", style = "heading 1") |>
  body_add_flextable(tabla_flex) |>
  print(target = file.path(data_path_out, "tabla_resumen_precios.docx"))

#Interpretacion:

#El precio promedio es de $1.345.903 pesos chilenos.
#Esto indica que, en promedio, los computadores de la muestra tienen un valor cercano a 1,35 millones de pesos.
#El precio más bajo corresponde a $905.000 pesos.
#Esto representa el computador más económico incluido en la muestra.
#El precio más alto es de $2.195.000 pesos.
#Es decir, el computador más costoso cuesta un poco más de 2,19 millones de pesos.
#En conjunto, estos valores muestran que existe una variación importante en los precios de los computadores dentro de la muestra, con diferencias superiores al millón de pesos entre el valor mínimo y el máximo.

#Pregunta 2----

histogram <- ggplot(datos, aes(x = precio)) +
  geom_histogram(
    bins = 20,
    color = "white",
    fill = "steelblue"
  ) +
  labs(
    title = NULL,
    x = "Precio (miles de pesos)",
    y = "Frecuencia"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),      # quita todas las grillas
    axis.text = element_text(color = "black"),  # labels en negro
    axis.title = element_text(color = "black")  # títulos en negro
  )

histogram 

#Save plot 

ggsave(file.path(data_path_out, "Histogram.png"), histogram, width = 15, height = 6, dpi = 300)

#Interpretacion 

#El histograma muestra que los precios de los computadores en la muestra no siguen una distribución normal como se ve en la figura.
#La mayor parte de los computadores tiene precios entre $1.000.000 y $1.500.000. 
#Por otro lado tenemos un cantidad pequeña de computadores con precios mucho mas altos sobre los 2 millones en la cola hacia la derecha. 
#Como sabemos en una distribucion normal los datos se concentran alrededor del promdeio y tienen una forma de campana. 
#En esta imagen tenemos mucho mas valores por debajo del promedios, es decir en la cola de la izquierda. 

#Pregunta 3----

sd_dd <- sd(datos$dd, na.rm = TRUE)
sd_dd

#Interpretacion
#La desviacion estandar de la capacidad del disco duro de nuestra muestra es de 137.04 gb. 
#Esto indica que en promcedio la capacidad de un disco duro de un computadore varia en 137.04 gb respecto al promedio de la muestra.
#Esta variacion es relativamente alta lo que indica que en la muestra hay computadores con capacidades muy distintas entre sí: algunos con discos duros pequeños y otros con discos duros mucho mayores.

table(datos$dd)

#Pregunta 4----



