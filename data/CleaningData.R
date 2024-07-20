# Cargar los paquetes necesarios
library(readr)
library(dplyr)

# Leer el archivo CSV
df <- read_csv("ESS9e03_2-subset.csv")

# Mostrar las primeras filas del data frame
head(df)

# Mostrar la estructura del data frame
str(df)

# Ver un resumen de los datos
summary(df)

# Obtener el tipo de variable de cada columna
variable_types <- sapply(df, class)

# Mostrar el tipo de variable de cada columna
print(variable_types)

##### DEPURACIÓN DE LOS DATOS
##### Como procesos de depuración de la BD, vamos a llevar a cabo las siguientes acciones:


# 1. Conversión de las variables gndr, bthcld y atncrse a categórica/factor (están como numéricas).
# 2. Eliminación de sujetos con al menos un valor faltante.
# 3. Detección de outliers.
# 4. Inversión de la variable 'happy'
# 5. Variable eisced: exclusión de sus valores 0 y 55, agrupación de categorías y conversión en factor
# 6. Descarte de las variables no consideradas relevantes para la tare y reordenación de columnas.



##### 1. Conversión de las variables gndr, bthcld y atncrse a categórica/factor (están como numéricas)

## Convertir gndr a factor
df <- df %>%
  mutate(gndr = factor(gndr, levels = c(1, 2), labels = c("Hombre", "Mujer")))

# Mostrar la estructura del data frame para verificar la conversión
str(df)

# Verificar los niveles de la variable gndr
levels(df$gndr)

# Verificar la conversión con un resumen
summary(df$gndr)

## Convertir bthcld a factor
df <- df %>%
  mutate(bthcld = factor(bthcld, levels = c(1, 2, 7, 8, 9), labels = c("Sí", "No", "Refusal", "Don't know", "No answer"))) %>%
  filter(!bthcld %in% c("Refusal", "Don't know", "No answer"))  # Eliminar valores de falta de respuesta

# Mostrar la estructura del data frame para verificar la conversión
str(df)

# Verificar los niveles de la variable bthcld
levels(df$bthcld)

# Verificar la conversión con un resumen
summary(df$bthcld)

## Convertir atncrse a factor
df <- df %>%
  mutate(atncrse = factor(atncrse, levels = c(1, 2, 7, 8, 9), labels = c("Sí", "No", "Refusal", "Don't know", "No answer"))) %>%
  filter(!atncrse %in% c("Refusal", "Don't know", "No answer"))

# Mostrar la estructura del data frame para verificar la conversión
str(df)

# Verificar los niveles de la variable atncrse
levels(df$atncrse)

# Verificar la conversión con un resumen
summary(df$atncrse)


##### 2. Eliminación de sujetos con al menos un valor faltante.

# Definir los códigos de valores faltantes para cada variable
missing_values_list <- list(
  happy = c(77, 88, 99),
  gndr = c(9),
  agea = c(999),
  impdiff = c(7, 8, 9),
  impenv = c(7, 8, 9),
  impfree = c(7, 8, 9),
  impfun = c(7, 8, 9),
  imprich = c(7, 8, 9),
  impsafe = c(7, 8, 9),
  imptrad = c(7, 8, 9),
  ipadvnt = c(7, 8, 9),
  ipbhprp = c(7, 8, 9),
  ipcrtiv = c(7, 8, 9),
  ipeqopt = c(7, 8, 9),
  ipfrule = c(7, 8, 9),
  ipgdtim = c(7, 8, 9),
  iphlppl = c(7, 8, 9),
  iplylfr = c(7, 8, 9),
  ipmodst = c(7, 8, 9),
  iprspot = c(7, 8, 9),
  ipshabt = c(7, 8, 9),
  ipstrgv = c(7, 8, 9),
  ipsuces = c(7, 8, 9),
  ipudrst = c(7, 8, 9),
  bthcld = c(7, 8, 9),
  atncrse = c(7, 8, 9),
  eisced = c(0, 55, 77, 88, 99)
)

# Función para contar valores faltantes específicos
count_missing_values <- function(variable, missing_values) {
  sum(df[[variable]] %in% missing_values)
}

# Contar los valores faltantes para cada variable y guardarlos en un data frame
missing_counts <- sapply(names(missing_values_list), function(var) {
  count_missing_values(var, missing_values_list[[var]])
})

# Convertir los resultados en un data frame
missing_counts_df <- data.frame(variable = names(missing_counts), missing_count = missing_counts)

# Mostrar los resultados
print(missing_counts_df)

# Función para identificar filas con valores faltantes específicos
has_missing_values <- function(row, missing_values_list) {
  for (col in names(missing_values_list)) {
    if (row[[col]] %in% missing_values_list[[col]]) {
      return(TRUE)
    }
  }
  return(FALSE)
}

# Aplicar la función a cada fila del DataFrame
df$has_missing <- apply(df, 1, has_missing_values, missing_values_list)

# Número total de sujetos
total_subjects <- nrow(df)

# Número de sujetos sin ningún valor faltante
subjects_without_missing <- nrow(df[!df$has_missing, ])

# Porcentaje de sujetos sin valores faltantes
percentage_without_missing <- (subjects_without_missing / total_subjects) * 100

# Imprimir los resultados
cat("Total de sujetos:", total_subjects, "\n")
cat("Sujetos sin valores faltantes:", subjects_without_missing, "\n")
cat("Porcentaje de sujetos sin valores faltantes:", round(percentage_without_missing, 2), "%\n")

# Calcular el número total de sujetos y el número de sujetos sin valores faltantes por país
summary_by_country <- df %>%
  group_by(cntry) %>%
  summarise(
    total_subjects = n(),
    subjects_without_missing = sum(!has_missing),
    percentage_without_missing = (sum(!has_missing) / n()) * 100
  )

# Imprimir el resumen por país
print(summary_by_country)

# Calcular el porcentaje de sujetos restantes para cada variable tras eliminar sujetos con valores faltantes específicos
percentage_by_variable <- sapply(names(missing_values_list), function(var) {
  remaining_for_var <- nrow(df[!df[[var]] %in% missing_values_list[[var]], ])
  (remaining_for_var / total_subjects) * 100
})

# Convertir los resultados en un data frame
percentage_by_variable_df <- data.frame(variable = names(percentage_by_variable), 
                                        percentage_remaining = percentage_by_variable)

# Mostrar los resultados
print(percentage_by_variable_df)

# Crear una base de datos filtrada, eliminando los sujetos con al menos un valor faltante
df_filtered <- df[!df$has_missing, ]

# Eliminar la columna 'has_missing' de la base de datos filtrada
df_filtered <- df_filtered %>% select(-has_missing)

# Mostrar las primeras filas de la base de datos filtrada
head(df_filtered)

# Mostrar la estructura de la base de datos filtrada
str(df_filtered)

# Ver un resumen de los datos
summary(df_filtered)


##### 3. Detección de outliers.

# Función para identificar outliers
identify_outliers <- function(variable) {
  Q1 <- quantile(df_filtered[[variable]], 0.25, na.rm = TRUE)
  Q3 <- quantile(df_filtered[[variable]], 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  outliers <- df_filtered[[variable]][df_filtered[[variable]] < lower_bound | df_filtered[[variable]] > upper_bound]
  return(outliers)
}

# Identificar outliers para cada variable numérica
numerical_vars <- names(df_filtered)[sapply(df_filtered, is.numeric)]
outliers_list <- lapply(numerical_vars, identify_outliers)

# Crear un data frame con los resultados
outliers_df <- data.frame(variable = numerical_vars, outliers = sapply(outliers_list, length))

# Calcular el porcentaje de outliers
outliers_df <- outliers_df %>%
  mutate(percentage_outliers = (outliers / nrow(df_filtered)) * 100)

# Mostrar los resultados
print(outliers_df)

# Crear los boxplots y guardarlos en una lista
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
if (!requireNamespace("e1071", quietly = TRUE)) install.packages("e1071")
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
if (!requireNamespace("gridExtra", quietly = TRUE)) install.packages("gridExtra")
if (!requireNamespace("nortest", quietly = TRUE)) install.packages("nortest")

library(dplyr)
library(e1071)
library(ggplot2)
library(gridExtra)
library(nortest)
boxplots <- lapply(numerical_vars, function(var) {
  ggplot(df_filtered, aes_string(y = var)) +
    geom_boxplot(outlier.colour = "red", outlier.shape = 8, outlier.size = 2) +
    labs(title = paste("Boxplot de", var), y = var) +
    theme_minimal()
})

# Guardar el plot combinado en un archivo PDF
pdf("boxplots_combined.pdf", width = 14, height = 10)

# Combinar los boxplots en varias páginas si es necesario
for (i in seq(1, length(boxplots), by = 6)) {
  print(do.call(grid.arrange, c(boxplots[i:min(i+5, length(boxplots))], ncol = 2)))
}

dev.off()

print("Los boxplots han sido guardados en 'boxplots_combined.pdf'")


##### 4. Inversión de la variable 'happy'

### Detección de OUTLIERS en la BD filtrada

#### Invertir la variable 'happy' en la BD filtrada
df_filtered <- df_filtered %>%
  mutate(happy = max(happy) + 1 - happy)

# Ver un resumen de los datos
summary(df_filtered)


##### 5. Variable eisced: exclusión de sus valores 0 y 55, agrupación de categorías y conversión en factor

### Filtrar la base de datos para excluir los valores 0 y 55 en eisced y agrupar categorías
df_filtered_eisced <- df_filtered %>%
  mutate(eisced_grouped = case_when(
    eisced %in% 1:4 ~ "ISCED 1-4",
    eisced == 5 ~ "ISCED 5",
    eisced == 6 ~ "ISCED 6",
    eisced == 7 ~ "ISCED 7"
  ))

# Convertir 'eisced_grouped' en factor
df_filtered_eisced <- df_filtered_eisced %>%
  mutate(eisced_grouped = factor(eisced_grouped, levels = c("ISCED 1-4", "ISCED 5", "ISCED 6", "ISCED 7")))

# Mostrar la estructura del data frame para verificar la conversión
str(df_filtered_eisced)

# Verificar los niveles de la variable 'eisced_grouped'
levels(df_filtered_eisced$eisced_grouped)

# Verificar la conversión con un resumen
summary(df_filtered_eisced$eisced_grouped)

# Ver un resumen de los datos
summary(df_filtered)

# Renombrar df_filtered_eisced a df_filtered
df_filtered <- df_filtered_eisced

# Mostrar la estructura del data frame renombrado
str(df_filtered)

# Verificar un resumen del data frame renombrado
summary(df_filtered)


##### 6. Descarte de las variables no consideradas relevantes para la tarea y reordenación de columnas.

## A continuación, excluimos las variables que no queremos retener para la actividad, traS selección de las relevantes
# Definir las variables a excluir
exclude_vars <- c("name", "essround", "edition", "proddate", "idno", 
                  "dweight", "pspwght", "pweight", "anweight", "prop", "stratum", "psu")

# Definir el orden deseado de las variables que queremos retener
desired_order <- c("cntry", "gndr", "agea", "atncrse", "bthcld", "eisced_grouped", "happy", "eisced", 
                   "ipcrtiv", "imprich", "ipeqopt", "ipshabt", "impsafe", "impdiff",
                   "ipfrule", "ipudrst", "ipmodst", "ipgdtim", "impfree", "iphlppl",
                   "ipsuces", "ipstrgv", "ipadvnt", "ipbhprp", "iprspot", "iplylfr",
                   "impenv", "imptrad", "impfun")

# Crear un nuevo data frame excluyendo las variables específicas
df_filtered_excluded <- df_filtered %>% select(-one_of(exclude_vars))

# Reordenar las columnas según el orden deseado
df_filtered_reordered <- df_filtered_excluded %>%
  select(all_of(desired_order))

# Generar el resumen de las variables reordenadas
summary_df <- summary(df_filtered_reordered)

# Mostrar el resumen
summary_df

# Renombrar df_filtered_reordered a df_filtered
df_filtered <- df_filtered_reordered

# Guardar el DataFrame depurado en un nuevo archivo CSV
write_csv(df_filtered, "df_filtered.csv")

##### Como procesos de depuración de la BD, hemos llevado a cano las siguientes acciones:

# 1. Conversión de las variables gndr, bthcld y atncrse a categórica/factor (están como numéricas).
# 2. Eliminación de sujetos con al menos un valor faltante.
# 3. Detección de outliers.
# 4. Inversión de la variable 'happy'
# 5. Variable eisced: exclusión de sus valores 0 y 55, agrupación de categorías y conversión en factor
# 6. Descarte de las variables no consideradas relevantes para la tarea y reordenación de columnas.


