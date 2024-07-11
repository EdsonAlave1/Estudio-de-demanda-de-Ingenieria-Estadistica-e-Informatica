# Cargar paquetes necesarios
library(readxl)   # Para leer archivos Excel
library(dplyr)    # Para manipulación de datos
library(tidyr)    # Para separar filas en columnas
library(ggplot2)  # Para visualización gráfica

# Cargar datos desde Excel (asegúrate de tener el paquete readxl instalado)
cuestionario <- read_excel("test.xlsx")

# Convertir variables relevantes a factor si es necesario
cuestionario$`Provincia de Procedencia` <- as.factor(cuestionario$`Provincia de Procedencia`)
cuestionario$`Distrito de Procedencia` <- as.factor(cuestionario$`Distrito de Procedencia`)

# Convertir respuestas de Likert a valores numéricos con etiquetas
#cuestionario <- cuestionario %>%
#  mutate(across(starts_with("3.1"), ~ recode(., "Ninguna" = 1, "Baja" = 2, "Media" = 3, "Alta" = 4, "Muy alta" = 5, .default = NA_real_), .names = "{col}")) %>%
#  mutate(across(starts_with("3.2"), ~ recode(., "Muy mala" = 1, "Mala" = 2, "Regular" = 3, "Buena" = 4, "Muy buena" = 5, .default = NA_real_), .names = "{col}")) %>%
#  mutate(across(starts_with("3.3"), ~ recode(., "Muy mala" = 1, "Mala" = 2, "Regular" = 3, "Buena" = 4, "Muy buena" = 5, .default = NA_real_), .names = "{col}")) %>%
#  mutate(across(starts_with("3.4"), ~ recode(., "Nada importante" = 1, "Poco Importante" = 2, "Neutral" = 3, "Importante" = 4, "Muy Importante" = 5, .default = NA_real_), .names = "{col}")) %>%
#  mutate(across(starts_with("3.5"), ~ recode(., "Nada importante" = 1, "Poco Importante" = 2, "Neutral" = 3, "Importante" = 4, "Muy Importante" = 5, .default = NA_real_), .names = "{col}")) %>%
#  mutate(across(starts_with("3.6"), ~ recode(., "Nada importante" = 1, "Poco Importante" = 2, "Neutral" = 3, "Importante" = 4, "Muy Importante" = 5, .default = NA_real_), .names = "{col}")) %>%
#  mutate(across(starts_with("3.7"), ~ recode(., "Nada importante" = 1, "Poco Importante" = 2, "Neutral" = 3, "Importante" = 4, "Muy Importante" = 5, .default = NA_real_), .names = "{col}")) %>%
#  mutate(across(starts_with("3.8"), ~ recode(., "si" = 1, "no" = 2, .default = NA_real_), .names = "{col}"))

### Análisis Descriptivo ###

## Características demográficas de los encuestados

### Edad
summary(cuestionario$edad)

### Género
table(cuestionario$Genero)

### Región de Procedencia
table(cuestionario$`Region de Procedencia`)

### Provincia de Procedencia
table(cuestionario$`Provincia de Procedencia`)

### Distrito de Procedencia
table(cuestionario$`Distrito de Procedencia`)

## Distribución de áreas de interés y razones para elegir la carrera

# Separar respuestas múltiples en filas individuales y contar frecuencias
areas_interes <- cuestionario %>%
  separate_rows(`2.1`, sep = ", ") %>%
  count(`2.1`)

# Renombrar la columna generada por `count` a `n` para la visualización
names(areas_interes)[names(areas_interes) == "n"] <- "Frecuencia"

# Visualización de áreas de interés
ggplot(areas_interes, aes(x = `2.1`, y = Frecuencia)) +
  geom_bar(stat = "identity") +
  labs(x = "areas de Interes", y = "Frecuencia") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

library(dplyr)

# Seleccionar columnas que empiezan con "3."
razones <- cuestionario %>%dplyr::select(starts_with("3"))

razones_summary <- sapply(razones, function(x) table(x))

str(razones_summary)
# Visualización de razones para elegir la carrera (ejemplo)
# Acceder a la tabla correspondiente a '3.1' dentro de razones_summary
table_3.1 <- razones_summary$`3.1`

# Convertir a dataframe para facilitar la visualización
df_3.1 <- as.data.frame(table_3.1)

# Renombrar columnas
names(df_3.1) <- c("Valor", "Frecuencia")

# Visualización con ggplot2
library(ggplot2)
ggplot(df_3.1, aes(x = Valor, y = Frecuencia)) +
  geom_bar(stat = "identity") +
  labs(x = "Razones para Elegir la Carrera", y = "Frecuencia")
## Fuentes de información utilizadas por los estudiantes

### Fuentes de Información

fuentes_info <- cuestionario %>%
  separate_rows(`p2.3`, sep = ", ") %>%
  count(`p2.3`)

# Renombrar la columna generada por `count` a `n` para la visualización
names(fuentes_info)[names(fuentes_info) == "n"] <- "Frecuencia"


# Visualización de fuentes de información
ggplot(fuentes_info, aes(x = `p2.3`, y = Frecuencia)) +
  geom_bar(stat = "identity") +
  labs(x = "Fuentes de Informacion", y = "Frecuencia") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## Influencia de padres y profesores

### Influencia de Padres y Profesores
influencia_padres <- table(cuestionario$`3.6`)

# Visualización de influencia de padres y profesores
barplot(influencia_padres, xlab = "Influencia de Padres y Profesores", ylab = "Frecuencia")

## Accesibilidad de la información sobre la carrera

### Accesibilidad de la Información
accesibilidad_info <- table(cuestionario$`3.7`)

# Visualización de accesibilidad de la información
barplot(accesibilidad_info, xlab = "Accesibilidad de la Informacion", ylab = "Frecuencia")

## Expectativas sobre capacidad de pago y salariales

### Expectativas de Capacidad de Pago
expectativas_pago <- table(cuestionario$`3.4`)

# Visualización de expectativas de capacidad de pago
barplot(expectativas_pago, xlab = "Expectativas de Capacidad de Pago", ylab = "Frecuencia")

### Expectativas Salariales
expectativas_salario <- table(cuestionario$`3.5`)

# Visualización de expectativas salariales
barplot(expectativas_salario, xlab = "Expectativas Salariales", ylab = "Frecuencia")

## Importancia del prestigio de la universidad y ubicación geográfica

### Importancia del Prestigio de la Universidad
importancia_prestigio <- table(cuestionario$`3.8`)

# Visualización de importancia del prestigio de la universidad
barplot(importancia_prestigio, xlab = "Importancia del Prestigio de la Universidad", ylab = "Frecuencia")

## Percepción sobre oportunidades de empleo y remuneración al egresar
##################
### Percepción sobre Oportunidades de Empleo
percepcion_empleo <- table(cuestionario$`3.2`)

# Visualización de percepción sobre oportunidades de empleo
barplot(percepcion_empleo, xlab = "Percepcion sobre Oportunidades de Empleo", ylab = "Frecuencia")

### Percepción sobre Remuneración al Egresar
percepcion_remuneracion <- table(cuestionario$`3.3`)

# Visualización de percepción sobre remuneración al egresar
barplot(percepcion_remuneracion, xlab = "Percepcion sobre Remuneracion al Egresar", ylab = "Frecuencia")

