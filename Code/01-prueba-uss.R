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
library(ggplot2)


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

#Calcular varianza

cov_pc <- cov(datos$precio, datos$dd, use = "complete.obs")
cov_pc #18654.71

#Interpretacion

#Tenemos una covarianza positiva de 18654.71 entre el precio y la capacidad del disco duro.
#Esto indica que a medida que aumenta la capacidad del disco duro, también tiende a aumentar el precio del computador.

#Calcular correlacion 

cor_pc <- cor(datos$precio, datos$dd, use = "complete.obs")
cor_pc #0.5376049

#Interpretacion 
#Una correlacion de casi 0.54 es una relacion positiva entre los discos duros y el precio del computador. 
#Eso si esta relacion no es perfecta, ya que una correlacion perfecta seria 1. Por lo que tampoco significa causalidad.


#Grafico 

graf_correlacion <- ggplot(datos, aes(x = dd, y = precio)) +
  geom_point(color = "steelblue", size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(
    title = NULL,
    x = "Capacidad del disco duro (GB)",
    y = "Precio (miles de pesos)"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_text(color = "black"),
    axis.title = element_text(color = "black")
  )

graf_correlacion

#Save plot 

ggsave(file.path(data_path_out, "Correlacion.png"), graf_correlacion, width = 15, height = 6, dpi = 300)

#Interpretacion grafico

#El gráfico de dispersión muestra una relación positiva entre la capacidad del disco duro y el precio del computador.
#La línea roja (ajuste lineal) tiene inclinación hacia arriba, confirmando que mayores capacidades de disco duro están asociadas a precios más altos.
#pero tenemos dispersion en los puntos, lo que indica que aunque hay una tendencia general, no todos los computadores siguen esta relación de manera estricta.

#Pregunta 5----

#No, el precio y la capacidad del disco duro no son independientes estadísticamente.
#Como sabemos dos variables son independientes si la covarianza entre ellas es cero.
#En este caso, la covarianza entre el precio y la capacidad del disco duro es 18654.71, que es un valor positivo y diferente de cero.
#Además, la correlación entre las dos variables es aproximadamente 0.54, lo que indica una relación positiva moderada entre ellas.

#Pregunta 6----

#precio como variable dependiente y dd como variable independiente

#precioi=β0+β1*ddi+ui

#modelo <- lm(precio ~ dd, data = datos)

modelo <- lm(precio ~ dd, data = datos)

summary(modelo)

#Table export 

tabla_regresion <- tibble(
  Coeficiente = c("(Intercept)", "dd"),
  Estimacion  = c(983.7568, 0.9933),
  `Error Est.` = c(73.5207, 0.1889),
  `t value`   = c(13.381, 5.258),
  `Pr(>|t|)`  = c("< 2e-16", "1.59e-06")
)

tabla_flex_reg <- flextable(tabla_regresion) %>%
  theme_vanilla() %>%
  autofit() %>%
  bold(part = "header") %>%
  align(align = "center", part = "all") %>%
  set_caption("Resultados de la regresión del precio sobre la capacidad del disco duro")

read_docx() |>
  body_add_par("Tabla: Resultados de la regresión", style = "heading 1") |>
  body_add_flextable(tabla_flex_reg) |>
  print(target = file.path(data_path_out, "tabla_regresion_precios.docx"))

#Pregunta 7----


#Interpretacion

#Por cada 1 GB adicional de capacidad del disco duro, el precio del computador aumenta en promedio 0.993 miles de pesos, es decir, aproximadamente $993 pesos.
#La relación es positiva y estadísticamente significativa al 1% (p-value = 1.59e-06).
#R² = 0.289 Esto significa que aproximadamente el 28.9% de la variación en el precio se explica por la capacidad del disco duro.


#Pregunta 8----

#Calcular residuos 

residuos <- resid(modelo)

#Sumar residuos 

suma_residuos <- sum(residuos)
suma_residuos #4.249934e-13

#Interpretacion 

#La suma de los residuos es prácticamente cero (4.25e-13), lo que indica que el modelo de regresión lineal ajusta bien los datos en términos de minimizar la suma de los errores.
#Esto se debe a que, en una regresión estimada por Mínimos Cuadrados Ordinarios (MCO) con intercepto, la suma de los residuos es exactamente cero por construcción matemática del método.

#Pregunta 8----

#Calcular valore predichos 

valores_predichos <- predict(modelo)

#Calcular el promedio de los valores predichos 

promedio_predichos <- mean(valores_predichos)
promedio_predichos #[1] 1345.903

#Calcular el promedio del precio real 

promedio_precio <- mean(datos$precio)
promedio_precio #1345.903

#Interpretacion 

#Ambos valores son iguales. 
#El promedio de los valores predichos por el modelo de regresión lineal es igual al promedio del precio real de los computadores en la muestra.
#en una regresión estimada por Mínimos Cuadrados Ordinarios (MCO) que incluye un intercepto, el promedio de las predicciones siempre es igual al promedio de la variable dependiente.
#Por eso ambos promedios son exactamente iguales (1345.903).


#Pregunta 9----

grafico_regresion <- ggplot(datos, aes(x = dd, y = precio)) +
  geom_point(color = "steelblue", size = 3, alpha = 0.7) +        # puntos
  geom_smooth(method = "lm", color = "red", se = FALSE, size = 1.2) + # línea de regresión
  labs(
    title = NULL,
    x = "Capacidad del disco duro (GB)",
    y = "Precio (miles de pesos)"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_text(color = "black"),
    axis.title = element_text(color = "black")
  )

grafico_regresion

#Save plot 

ggsave(file.path(data_path_out, "Grafico_reg.png"), grafico_regresion, width = 15, height = 6, dpi = 300)

#Interpretacion:
#Los puntos azules representan cada computador
#La línea roja es la recta de regresión estimada por MCO. 
#Como la pendiente es positiva, se ve claramente que a mayor capacidad del disco duro, mayor precio promedio.


#Pregunta 10----

# Identificamos las observaciones con menor y mayor precio

punto_min <- datos %>% slice_min(precio, n = 1)
punto_max <- datos %>% slice_max(precio, n = 1)

puntos_extremos <- bind_rows(
  mutate(punto_min, tipo = "Mínimo precio"),
  mutate(punto_max, tipo = "Máximo precio")
)

# Gráfico 

grafico_regresion_ext <- ggplot(datos, aes(x = dd, y = precio)) +
  geom_point(color = "steelblue", size = 3, alpha = 0.7) +                # puntos normales
  geom_smooth(method = "lm", color = "red", se = FALSE, size = 1.2) +     # línea de regresión
  geom_point(
    data = puntos_extremos,
    aes(x = dd, y = precio, shape = tipo),
    color = "black",
    fill = "orange",
    size = 4,
    stroke = 1
  ) +
  labs(
    title = "Relación entre precio y capacidad del disco duro",
    x = "Capacidad del disco duro (GB)",
    y = "Precio (miles de pesos)",
    shape = NULL
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_text(color = "black"),
    axis.title = element_text(color = "black")
  )

grafico_regresion_ext

#Save plot

ggsave(file.path(data_path_out, "Grafico_reg_2.png"), grafico_regresion_ext, width = 15, height = 6, dpi = 300)

#Todos los puntos normales en azul.
#Dos puntos especiales (mínimo y máximo precio) en negro y distinta forma (leyenda “Mínimo precio” / “Máximo precio”).


#Pregunta 11----

# Puntos extremos 

punto_min <- datos %>% slice_min(precio, n = 1)
punto_max <- datos %>% slice_max(precio, n = 1)

puntos_extremos <- bind_rows(
  mutate(punto_min, tipo = "Mínimo precio"),
  mutate(punto_max, tipo = "Máximo precio")
)

# Agregar valores predichos y residuales
puntos_extremos <- puntos_extremos %>%
  mutate(
    precio_predicho = predict(modelo, newdata = .),
    residual        = precio - precio_predicho
  )

puntos_extremos

#Grafico 

grafico_regresion_ext2 <- ggplot(datos, aes(x = dd, y = precio)) +
  # puntos normales
  geom_point(color = "steelblue", size = 3, alpha = 0.7) +
  
  # línea de regresión
  geom_smooth(method = "lm", color = "red", se = FALSE, size = 1.2) +
  
  # puntos observados extremos (mínimo y máximo precio)
  geom_point(
    data = puntos_extremos,
    aes(x = dd, y = precio, shape = tipo),
    color = "black",
    fill  = "orange",
    size  = 4,
    stroke = 1
  ) +
  
 
  geom_point(
    data = puntos_extremos,
    aes(x = dd, y = precio_predicho),
    color = "green",
    size  = 3
  ) +
  
  # segmentos verticales (residuales: observado - predicho)
  geom_segment(
    data = puntos_extremos,
    aes(
      x = dd, xend = dd,
      y = precio_predicho, yend = precio
    ),
    linetype = "dashed",
    color = "purple",
    linewidth = 0.8
  ) +
  
  labs(
    title = NULL,
    x = "Capacidad del disco duro (GB)",
    y = "Precio (miles de pesos)",
    shape = NULL
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text  = element_text(color = "black"),
    axis.title = element_text(color = "black")
  )

grafico_regresion_ext2

#Save plot

ggsave(file.path(data_path_out, "Grafico_reg_3.png"), grafico_regresion_ext2, width = 15, height = 6, dpi = 300)

#El punto negro: precio observado del computador (mínimo o máximo).
#El punto verde: precio predicho por el modelo para ese mismo dd
#La línea punteada morada: el residual (diferencia entre observado y predicho).


#Pregunta 12----

#El modelo de regresion por MCO estima B cero y B uno de una regresion lineal buscando la recta que mejor se ajusta a los datos que tenemos. 
#En términos formales, MCO elige los valores de los coeficientes que minimizan la suma de los residuos al cuadrado. 
#Los puntos azules son los valores observados de precio y capacidad del disco duro.
#La línea roja es la recta ajustada por MCO. 
#Los puntos verdes en la línea son los valores predichos para cada observación.
#Las líneas verticales punteadas representan los residuales (la distancia entre cada punto observado y la recta).
#MCO elige la recta que hace que estas distancias (residuales) sean lo más pequeñas posible en promedio, pero ponderadas de forma cuadrática.

