######## Cosas inicio










######## Código parcial del m08_r1

###### EXPLORACIÓN
###### A continuación, llevamos a cabo las siguientes exploraciones:

##### Gráficos para ver DISTRIBUCIÓN DE LOS DATOS de las variables
##### Correlaciones entre las variables numéricas
##### Análisis descriptivo y pruebas de normalidad
######### Gráficos y diferencias por sexo / motivaciones
####### Gráficos y diferencias por sexo / happy, inpfree, ipeqopt y ipfrule 
##### Eisced / MOTIVACIONES
##### Agea / MOTIVACIONES
####### Clasificación por PAÍSES para cada una de las variables de motivación
######### Gráfico de Europa occidental con los resultados por cada tipo de motivación
######### Gráfico de Europa occidental con el tipo de motivación más alto por país
####### Gráfico de Europa occidental y diferencias por países por cada motivación y por happy, impfree, ipeqopt e ipfrule
######### Gráfico de mariposa por sexo (ejemplo)



# Cargar los paquetes necesarios
library(ggplot2)


# Cargar base de datos

df_filtered <- read.csv("df_filtered.csv")


# Variables categóricas y numéricas específicas
categorical_vars <- c("gndr", "atncrse", "bthcld", "eisced_grouped")
numerical_vars <- c("agea", "happy", "eisced", 
                    "ipcrtiv", "imprich", "ipeqopt", "ipshabt", "impsafe", "impdiff",
                    "ipfrule", "ipudrst", "ipmodst", "ipgdtim", "impfree", "iphlppl",
                    "ipsuces", "ipstrgv", "ipadvnt", "ipbhprp", "iprspot", "iplylfr",
                    "impenv", "imptrad", "impfun")

#### Gráficos para variables CATEGÓRICAS

for (var in categorical_vars) {
  p <- ggplot(df_filtered, aes_string(x = var)) +
    geom_bar(fill = "#89CFF0") +
    theme_minimal() +
    labs(title = paste("Distribución de", var), x = var, y = "Frecuencia") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  print(p)
}


#### Gráficos para variables NUMÉRICAS

for (var in numerical_vars) {
  p <- ggplot(df_filtered, aes_string(x = var)) +
    geom_histogram(binwidth = 1, fill = "#89CFF0", color = "black") +
    theme_minimal() +
    labs(title = paste("Distribución de", var), x = var, y = "Frecuencia") +
    geom_vline(aes_string(xintercept = paste("mean(", var, ", na.rm = TRUE)")), 
               color = "red", linetype = "dashed", size = 1) +
    geom_vline(aes_string(xintercept = paste("median(", var, ", na.rm = TRUE)")), 
               color = "green", linetype = "dashed", size = 1) +
    scale_x_continuous(breaks = seq(min(df_filtered[[var]], na.rm = TRUE), 
                                    max(df_filtered[[var]], na.rm = TRUE), 
                                    by = 1))
  print(p)
}


#### CORRELACIONES entre las variables numéricas

# Cargar librerías necesarias
library(ggplot2)
library(dplyr)
library(tidyr)
library(reshape2)

# Seleccionar las variables de interés
selected_vars <- df_filtered %>%
  select(motprosoc, motextr, motintr, happy, impfree, ipeqopt, ipfrule, agea)

# Calcular la matriz de correlación
cor_matrix <- cor(selected_vars, use = "complete.obs")

# Convertir la matriz de correlación en un data frame largo
melted_cor_matrix <- melt(cor_matrix)

# Crear el mapa de calor con los valores de correlación
ggplot(data = melted_cor_matrix, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1, 1), space = "Lab", 
                       name="Correlación") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1)) +
  coord_fixed() +
  geom_text(aes(label = round(value, 2)), color = "black", size = 4) +
  labs(title = "Mapa de Calor de Correlaciones",
       x = "Variables",
       y = "Variables")


#### Análisis descriptivo y pruebas de normalidad
#### Código para evaluar NORMALIDAD/Distribuciones y Generar Gráficos Q-Q

# Instalar y cargar los paquetes necesarios
if (!requireNamespace("nortest", quietly = TRUE)) {
  install.packages("nortest")
}
# Cargar los paquetes necesarios
library(dplyr)
library(ggplot2)
library(e1071)
library(gridExtra)
library(nortest)
library(knitr)
library(kableExtra)
# Variables numéricas específicas
numerical_vars <- c("agea", "happy", "eisced", 
                    "ipcrtiv", "imprich", "ipeqopt", "ipshabt", "impsafe", "impdiff",
                    "ipfrule", "ipudrst", "ipmodst", "ipgdtim", "impfree", "iphlppl",
                    "ipsuces", "ipstrgv", "ipadvnt", "ipbhprp", "iprspot", "iplylfr",
                    "impenv", "imptrad", "impfun")
# Función para calcular estadísticas descriptivas y pruebas de normalidad
analyze_distribution <- function(data, var) {
  values <- data[[var]]
  # Calcular estadísticos descriptivos
  mean_val <- mean(values, na.rm = TRUE)
  median_val <- median(values, na.rm = TRUE)
  skewness_val <- skewness(values, na.rm = TRUE)
  kurtosis_val <- kurtosis(values, na.rm = TRUE)
  # Inicializar variables de la prueba de normalidad
  normality_test <- NULL
  shapiro_w <- NA
  shapiro_p <- NA
  normality_method <- NA
  # Realizar prueba de Shapiro-Wilk si el tamaño de la muestra está en el rango permitido
  sample_size <- length(values[!is.na(values)])
  if (sample_size >= 3 && sample_size <= 5000) {
    normality_test <- shapiro.test(values)
    shapiro_w <- normality_test$statistic
    shapiro_p <- normality_test$p.value
    normality_method <- "Shapiro-Wilk"
  } else if (sample_size > 5000) {
    normality_method <- "Asumida Normal por TCL"
  } else if (sample_size < 3) {
    normality_method <- "Muestra Insuficiente"
  }
  # Resultados
  result <- data.frame(
    Variable = var,
    Media = mean_val,
    Mediana = median_val,
    Asimetría = skewness_val,
    Curtosis = kurtosis_val,
    Normality_Method = normality_method,
    Test_Statistic = shapiro_w,
    p_value = shapiro_p
  )
  return(result)
}
# Analizar distribución de cada variable en la base de datos filtrada
results <- do.call(rbind, lapply(numerical_vars, function(var) analyze_distribution(df_filtered, var)))
# Mostrar resultados
print(results)




##### Cálculo de outliers y asimetría para las 3 variables de motivación
# Función para analizar outliers y calcular la asimetría
analyze_variable <- function(df, variable) {
  values <- df[[variable]]
  # Identificar outliers
  Q1 <- quantile(values, 0.25, na.rm = TRUE)
  Q3 <- quantile(values, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  outliers <- values[values < lower_bound | values > upper_bound]
  # Calcular estadísticas descriptivas
  skewness_val <- e1071::skewness(values, na.rm = TRUE)
  kurtosis_val <- e1071::kurtosis(values, na.rm = TRUE)
  # Resultados
  result <- list(
    outliers = length(outliers),
    percentage_outliers = (length(outliers) / length(values)) * 100,
    skewness = skewness_val,
    kurtosis = kurtosis_val
  )
  return(result)
}

# Analizar variables motextr, motintr y motprosoc
motextr_analysis <- analyze_variable(df_filtered, "motextr")
motintr_analysis <- analyze_variable(df_filtered, "motintr")
motprosoc_analysis <- analyze_variable(df_filtered, "motprosoc")

# Crear un data frame con los resultados
analysis_df <- data.frame(
  variable = c("motextr", "motintr", "motprosoc"),
  outliers = c(motextr_analysis$outliers, motintr_analysis$outliers, motprosoc_analysis$outliers),
  percentage_outliers = c(motextr_analysis$percentage_outliers, motintr_analysis$percentage_outliers, motprosoc_analysis$percentage_outliers),
  skewness = c(motextr_analysis$skewness, motintr_analysis$skewness, motprosoc_analysis$skewness),
  kurtosis = c(motextr_analysis$kurtosis, motintr_analysis$kurtosis, motprosoc_analysis$kurtosis)
)
# Mostrar los resultados
print(analysis_df)

# Obtener el tipo de variable de cada columna
variable_types <- sapply(df_filtered, class)
print(variable_types)



############ Gráficos de SEXO / MOTIVACIONES
####### SEXOS SEPARADOS
# Cargar librerías necesarias
library(ggplot2)
library(dplyr)
library(tidyr)

# Crear una base de datos larga para ggplot
df_long <- df_filtered %>%
  select(gndr, motprosoc, motextr, motintr) %>%
  pivot_longer(cols = c("motprosoc", "motextr", "motintr"), names_to = "motivation", values_to = "value")

# Calcular medias por sexo y motivación
df_long_sex <- df_long %>%
  group_by(gndr, motivation) %>%
  summarise(mean_value = mean(value, na.rm = TRUE)) %>%
  ungroup()

# Crear el gráfico por sexo (Hombres)
ggplot(subset(df_long_sex, gndr == "Hombre"), aes(x = motivation, y = mean_value, fill = motivation)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Media de Motivaciones por Sexo (Hombres)",
       x = "Motivación",
       y = "Media de Motivación",
       fill = "Motivación") +
  theme_minimal()

# Crear el gráfico por sexo (Mujeres)
ggplot(subset(df_long_sex, gndr == "Mujer"), aes(x = motivation, y = mean_value, fill = motivation)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Media de Motivaciones por Sexo (Mujeres)",
       x = "Motivación",
       y = "Media de Motivación",
       fill = "Motivación") +
  theme_minimal()

############ Gráficos de SEXO / MOTIVACIONES
####### SEXOS JUNTOS
### t-student Sexo / Motivación
# Realizar pruebas t para cada motivación
t_test_results <- df_filtered %>%
  select(gndr, motprosoc, motextr, motintr) %>%
  pivot_longer(cols = c("motprosoc", "motextr", "motintr"), names_to = "motivation", values_to = "value") %>%
  group_by(motivation) %>%
  summarise(
    t_test = list(t.test(value ~ gndr)),
    p_value = t_test[[1]]$p.value,
    mean_male = t_test[[1]]$estimate[1],
    mean_female = t_test[[1]]$estimate[2]
  ) %>%
  select(-t_test)

# Mostrar resultados
print(t_test_results)

# Cargar librerías necesarias
library(ggplot2)
library(dplyr)
library(tidyr)

# Crear una base de datos larga para ggplot
df_long <- df_filtered %>%
  select(gndr, motprosoc, motextr, motintr) %>%
  pivot_longer(cols = c("motprosoc", "motextr", "motintr"), names_to = "motivation", values_to = "value")

# Calcular medias por sexo y motivación
df_long_sex <- df_long %>%
  group_by(gndr, motivation) %>%
  summarise(mean_value = mean(value, na.rm = TRUE)) %>%
  ungroup()

# Convertir la variable gndr a factor con etiquetas
df_long_sex$gndr <- factor(df_long_sex$gndr, levels = c("Hombre", "Mujer"))

# Crear el gráfico comparativo por sexo
ggplot(df_long_sex, aes(x = motivation, y = mean_value, fill = gndr)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Comparación de la Media de Motivaciones por Sexo",
       x = "Motivación",
       y = "Media de Motivación",
       fill = "Sexo") +
  theme_minimal()

####### Comparación SEXO y  1) happy, 2) inpfree, 3) ipeqopt y 3) ipfrule
# Realizar pruebas t para las variables adicionales
t_test_results_additional <- df_filtered %>%
  select(gndr, happy, impfree, ipeqopt, ipfrule) %>%
  pivot_longer(cols = c("happy", "impfree", "ipeqopt", "ipfrule"), names_to = "variable", values_to = "value") %>%
  group_by(variable) %>%
  summarise(
    t_test = list(t.test(value ~ gndr)),
    p_value = t_test[[1]]$p.value,
    mean_male = t_test[[1]]$estimate[1],
    mean_female = t_test[[1]]$estimate[2]
  ) %>%
  select(-t_test)

# Mostrar resultados
print(t_test_results_additional)
# Crear una base de datos larga para ggplot

df_long_additional <- df_filtered %>%
  select(gndr, happy, impfree, ipeqopt, ipfrule) %>%
  pivot_longer(cols = c("happy", "impfree", "ipeqopt", "ipfrule"), names_to = "variable", values_to = "value")

# Calcular medias por sexo y variable
df_long_sex_additional <- df_long_additional %>%
  group_by(gndr, variable) %>%
  summarise(mean_value = mean(value, na.rm = TRUE)) %>%
  ungroup()

# Convertir la variable gndr a factor con etiquetas
df_long_sex_additional$gndr <- factor(df_long_sex_additional$gndr, levels = c("Hombre", "Mujer"))

# Crear el gráfico comparativo por sexo para las variables adicionales
ggplot(df_long_sex_additional, aes(x = variable, y = mean_value, fill = gndr)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Comparación de la Media de Variables por Sexo",
       x = "Variable",
       y = "Media",
       fill = "Sexo") +
  theme_minimal()


# Cargar los paquetes necesarios para ANOVA
library(dplyr)
library(tidyr)
library(ggplot2)

# Variables de interés
motivation_vars <- c("motprosoc", "motextr", "motintr")

# Realizar ANOVAs para cada motivación según `eisced_grouped`
anova_results_motivation <- lapply(motivation_vars, function(var) {
  anova_result <- aov(as.formula(paste(var, "~ eisced_grouped")), data = df_filtered)
  summary(anova_result)
})

# Mostrar resultados de ANOVAs para motivaciones
names(anova_results_motivation) <- motivation_vars
anova_results_motivation

# Crear gráficos para motivaciones según `eisced_grouped`
df_long_motivation <- df_filtered %>%
  select(eisced_grouped, all_of(motivation_vars)) %>%
  pivot_longer(cols = all_of(motivation_vars), names_to = "motivation", values_to = "value")

ggplot(df_long_motivation, aes(x = eisced_grouped, y = value, fill = eisced_grouped)) +
  geom_boxplot() +
  facet_wrap(~motivation, scales = "free_y") +
  labs(title = "Distribución de Motivaciones por Nivel Educativo (eisced_grouped)",
       x = "Nivel Educativo (eisced_grouped)",
       y = "Valor de Motivación",
       fill = "Nivel Educativo") +
  theme_minimal()

# Crear categorías de edad
df_filtered <- df_filtered %>%
  mutate(age_category = cut(agea, 
                            breaks = c(15, 20, 30, 40, 50, 60, 70, 80, Inf), 
                            labels = c("15-20", "21-30", "31-40", "41-50", "51-60", "61-70", "71-80", "81+"),
                            right = FALSE))

# Verificar la creación de categorías de edad
table(df_filtered$age_category)

# Realizar ANOVAs para motivaciones según `age_category`
anova_results_motivation_age <- lapply(motivation_vars, function(var) {
  aov_result <- aov(df_filtered[[var]] ~ df_filtered$age_category)
  summary(aov_result)
})

# Mostrar resultados de ANOVAs para motivaciones según edad
names(anova_results_motivation_age) <- motivation_vars
anova_results_motivation_age

# Crear una base de datos larga para ggplot para motivaciones según `age_category`
df_long_age_motivation <- df_filtered %>%
  select(age_category, motprosoc, motextr, motintr) %>%
  pivot_longer(cols = c("motprosoc", "motextr", "motintr"), names_to = "motivation", values_to = "value")

# Calcular medias por categoría de edad y motivación
df_long_age_motivation_summary <- df_long_age_motivation %>%
  group_by(age_category, motivation) %>%
  summarise(mean_value = mean(value, na.rm = TRUE)) %>%
  ungroup()

# Crear el gráfico con tres barras de motivaciones para cada segmento de edad
ggplot(df_long_age_motivation_summary, aes(x = age_category, y = mean_value, fill = motivation)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Comparación de la Media de Motivaciones por Categoría de Edad",
       x = "Categoría de Edad",
       y = "Media de Motivación",
       fill = "Motivación") +
  theme_minimal()


################# Gráfico de happy / MOTIVACIÓN PPAL
######### Hay que calcular, para cada sujeto, cuál es su tipo de motivación más alta 
######### (motextr, motintr o motprosoc), y dividir los sujetos en tres grupos:
######### el grupo de sujetos para los que la motextr es la más alta, el de los sujetos
######### para los que la más alta es la motintr y el grupo de sujetos para los que
######### la más alta es la motprosoc.
######### Si un sujeto tiene la puntuación más alta en más de una de las motivaciones,
######## ese sujeto entra en más de un grupo (es decir, en dos o en los tres).

######## Determinar el número de sujetos que tienen una puntuación superior en cada tipo de motivación
######## en comparación con las otras dos.

# Cargar las librerías necesarias
library(dplyr)

# Crear nuevas columnas para identificar los sujetos con puntuaciones superiores en cada tipo de motivación
df_filtered <- df_filtered %>%
  mutate(
    highest_motextr = motextr >= motintr & motextr >= motprosoc,
    highest_motintr = motintr >= motextr & motintr >= motprosoc,
    highest_motprosoc = motprosoc >= motextr & motprosoc >= motintr
  )

# Contar el número de sujetos para cada tipo de motivación superior
counts <- df_filtered %>%
  summarise(
    motextr_count = sum(highest_motextr, na.rm = TRUE),
    motintr_count = sum(highest_motintr, na.rm = TRUE),
    motprosoc_count = sum(highest_motprosoc, na.rm = TRUE)
  )

# Calcular los porcentajes
percentages <- counts / nrow(df_filtered) * 100

# Combinar los resultados en un data frame
results <- bind_rows(counts, percentages) %>%
  mutate(type = c("Count", "Percentage"))

# Mostrar los resultados
print(results)


# Cargar librerías necesarias
library(ggplot2)
library(dplyr)

# Definir los cuartiles superiores para cada variable de motivación
top_quartile_threshold <- function(x) {
  quantile(x, 0.75, na.rm = TRUE)
}

# Calcular los umbrales de cuartil superior
motextr_threshold <- top_quartile_threshold(df_filtered$motextr)
motintr_threshold <- top_quartile_threshold(df_filtered$motintr)
motprosoc_threshold <- top_quartile_threshold(df_filtered$motprosoc)

# Crear una nueva columna para categorizar los sujetos en base a los umbrales de cuartil superior
df_filtered <- df_filtered %>%
  mutate(
    high_motextr = ifelse(motextr >= motextr_threshold, "High Motextr", "Low/Med Motextr"),
    high_motintr = ifelse(motintr >= motintr_threshold, "High Motintr", "Low/Med Motintr"),
    high_motprosoc = ifelse(motprosoc >= motprosoc_threshold, "High Motprosoc", "Low/Med Motprosoc")
  )

# Seleccionar los sujetos "más altos" en cada variable de motivación
df_high_mot <- df_filtered %>%
  filter(high_motextr == "High Motextr" | high_motintr == "High Motintr" | high_motprosoc == "High Motprosoc") %>%
  pivot_longer(cols = c("high_motextr", "high_motintr", "high_motprosoc"), names_to = "motivation", values_to = "level") %>%
  filter(level == "High Motextr" | level == "High Motintr" | level == "High Motprosoc") %>%
  mutate(motivation = recode(motivation, 
                             high_motextr = "Motextr",
                             high_motintr = "Motintr",
                             high_motprosoc = "Motprosoc"))

# Crear el gráfico comparando el nivel de felicidad entre los grupos de motivación más alta
ggplot(df_high_mot, aes(x = motivation, y = happy, fill = motivation)) +
  geom_boxplot() +
  labs(title = "Comparación del Nivel de Felicidad entre Sujetos con Alta Motivación",
       x = "Motivación",
       y = "Nivel de Felicidad (happy)") +
  theme_minimal() +
  theme(legend.position = "none")

## Son muy parecidas. Calculamos:
# Calcular media y desviación estándar para cada grupo de motivación
summary_stats <- df_high_mot %>%
  group_by(motivation) %>%
  summarise(
    mean_happy = mean(happy, na.rm = TRUE),
    sd_happy = sd(happy, na.rm = TRUE)
  )
# Mostrar los resultados
print(summary_stats)


####### Determinar el número de sujetos que tienen una puntuación superior en cada tipo de motivación
####### en comparación con las otras dos.

# Cargar las librerías necesarias
library(dplyr)

# Crear nuevas columnas para identificar los sujetos con puntuaciones superiores en cada tipo de motivación
df_filtered <- df_filtered %>%
  mutate(
    highest_motextr = motextr >= motintr & motextr >= motprosoc,
    highest_motintr = motintr >= motextr & motintr >= motprosoc,
    highest_motprosoc = motprosoc >= motextr & motprosoc >= motintr
  )

# Contar el número de sujetos para cada tipo de motivación superior
counts <- df_filtered %>%
  summarise(
    motextr_count = sum(highest_motextr, na.rm = TRUE),
    motintr_count = sum(highest_motintr, na.rm = TRUE),
    motprosoc_count = sum(highest_motprosoc, na.rm = TRUE)
  )

# Calcular los porcentajes
percentages <- counts / nrow(df_filtered) * 100

# Combinar los resultados en un data frame
results <- bind_rows(counts, percentages) %>%
  mutate(type = c("Count", "Percentage"))

# Mostrar los resultados
print(results)


# Cargar las librerías necesarias
library(dplyr)
library(ggplot2)

# Crear nuevas columnas para identificar los sujetos con puntuaciones superiores en cada tipo de motivación
df_filtered <- df_filtered %>%
  mutate(
    highest_motextr = motextr >= motintr & motextr >= motprosoc,
    highest_motintr = motintr >= motextr & motintr >= motprosoc,
    highest_motprosoc = motprosoc >= motextr & motprosoc >= motintr
  )

# Verificar la estructura de la base de datos filtrada para asegurarnos de que las nuevas columnas se hayan añadido
str(df_filtered)


##### Código para explorar happy en función de los highest de cada motivación
# Crear una nueva columna que indique el tipo de motivación más alta para cada sujeto
df_filtered <- df_filtered %>%
  mutate(
    motivation_group = case_when(
      highest_motextr & !highest_motintr & !highest_motprosoc ~ "motextr",
      highest_motintr & !highest_motextr & !highest_motprosoc ~ "motintr",
      highest_motprosoc & !highest_motextr & !highest_motintr ~ "motprosoc",
      highest_motextr & highest_motintr & !highest_motprosoc ~ "motextr_motintr",
      highest_motextr & highest_motprosoc & !highest_motintr ~ "motextr_motprosoc",
      highest_motintr & highest_motprosoc & !highest_motextr ~ "motintr_motprosoc",
      highest_motextr & highest_motintr & highest_motprosoc ~ "all",
      TRUE ~ "other"
    )
  )

# Filtrar solo los grupos de interés (motextr, motintr, motprosoc)
df_filtered <- df_filtered %>%
  filter(motivation_group %in% c("motextr", "motintr", "motprosoc"))

# Realizar ANOVA
anova_result <- aov(happy ~ motivation_group, data = df_filtered)
summary(anova_result)

# Calcular las medias y errores estándar para el gráfico de barras
df_summary <- df_filtered %>%
  group_by(motivation_group) %>%
  summarise(
    mean_happy = mean(happy, na.rm = TRUE),
    sd_happy = sd(happy, na.rm = TRUE),
    n = n()
  ) %>%
  mutate(se_happy = sd_happy / sqrt(n))

# Crear el gráfico de barras
ggplot(df_summary, aes(x = motivation_group, y = mean_happy, fill = motivation_group)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = mean_happy - se_happy, ymax = mean_happy + se_happy), width = 0.2) +
  labs(title = "Comparación de la Felicidad Media por Grupo de Motivación",
       x = "Grupo de Motivación",
       y = "Felicidad Media",
       fill = "Grupo de Motivación") +
  theme_minimal()




###### Clasificación por PAÍSES para cada una de las variables de motivación

# Ordenar por motprosoc
country_motprosoc <- df_filtered %>%
  group_by(cntry) %>%
  summarise(mean_motprosoc = mean(motprosoc, na.rm = TRUE)) %>%
  arrange(desc(mean_motprosoc))

# Ordenar por motextr
country_motextr <- df_filtered %>%
  group_by(cntry) %>%
  summarise(mean_motextr = mean(motextr, na.rm = TRUE)) %>%
  arrange(desc(mean_motextr))

# Ordenar por motintr
country_motintr <- df_filtered %>%
  group_by(cntry) %>%
  summarise(mean_motintr = mean(motintr, na.rm = TRUE)) %>%
  arrange(desc(mean_motintr))

# Mostrar los resultados
print(country_motprosoc)
print(country_motextr)
print(country_motintr)

##### Gráfico del mapa de Europa occidental con los resultados por MOTIVACIÓN
##### mismo color, pero diferente saturación
# Instalar y cargar las librerías necesarias
if (!requireNamespace("ggplot2")) install.packages("ggplot2")
if (!requireNamespace("dplyr")) install.packages("dplyr")
if (!requireNamespace("sf")) install.packages("sf")
if (!requireNamespace("rnaturalearth")) install.packages("rnaturalearth")
if (!requireNamespace("rnaturalearthdata")) install.packages("rnaturalearthdata")

# Cargar los paquetes necesarios
library(dplyr)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

# Cargar el shapefile de los países de Europa
europe_map <- ne_countries(scale = "medium", continent = "Europe", returnclass = "sf")

# Asegurar que los nombres de los países coinciden con los datos
country_labels <- c("United Kingdom", "France", "Portugal", "Italy", "Spain", 
                    "Germany", "Netherlands", "Belgium", "Ireland", "Austria", 
                    "Switzerland", "Denmark", "Finland", "Sweden", "Norway")

country_motextr <- country_motextr %>%
  mutate(cntry = recode(cntry, GB = "United Kingdom", FR = "France", PT = "Portugal", IT = "Italy", 
                        ES = "Spain", DE = "Germany", NL = "Netherlands", BE = "Belgium", 
                        IE = "Ireland", AT = "Austria", CH = "Switzerland", DK = "Denmark", 
                        FI = "Finland", SE = "Sweden", NO = "Norway"))

country_motintr <- country_motintr %>%
  mutate(cntry = recode(cntry, GB = "United Kingdom", FR = "France", PT = "Portugal", IT = "Italy", 
                        ES = "Spain", DE = "Germany", NL = "Netherlands", BE = "Belgium", 
                        IE = "Ireland", AT = "Austria", CH = "Switzerland", DK = "Denmark", 
                        FI = "Finland", SE = "Sweden", NO = "Norway"))

country_motprosoc <- country_motprosoc %>%
  mutate(cntry = recode(cntry, GB = "United Kingdom", FR = "France", PT = "Portugal", IT = "Italy", 
                        ES = "Spain", DE = "Germany", NL = "Netherlands", BE = "Belgium", 
                        IE = "Ireland", AT = "Austria", CH = "Switzerland", DK = "Denmark", 
                        FI = "Finland", SE = "Sweden", NO = "Norway"))

# Revisar los nombres en el mapa
unique(europe_map$name_long)

# Fusionar los datos de motivación con el mapa
merged_data_motextr <- merge(europe_map, country_motextr, by.x = "name_long", by.y = "cntry", all.x = TRUE)
merged_data_motintr <- merge(europe_map, country_motintr, by.x = "name_long", by.y = "cntry", all.x = TRUE)
merged_data_motprosoc <- merge(europe_map, country_motprosoc, by.x = "name_long", by.y = "cntry", all.x = TRUE)

# Crear el mapa para motextr con mayor contraste de saturación
ggplot() +
  geom_sf(data = europe_map, fill = "grey", color = "black") + 
  geom_sf(data = merged_data_motextr, aes(fill = mean_motextr), color = "black") +
  scale_fill_gradient(low = "#FFCCCC", high = "#CC0000", na.value = "grey", name = "Media") +
  theme_minimal() +
  coord_sf(xlim = c(-10, 30), ylim = c(35, 70)) + 
  labs(title = "Nivel de Motextr en Europa Occidental")

# Crear el mapa para motintr con mayor contraste de saturación
ggplot() +
  geom_sf(data = europe_map, fill = "grey", color = "black") + 
  geom_sf(data = merged_data_motintr, aes(fill = mean_motintr), color = "black") +
  scale_fill_gradient(low = "#CCCCFF", high = "#0000CC", na.value = "grey", name = "Media") +
  theme_minimal() +
  coord_sf(xlim = c(-10, 30), ylim = c(35, 70)) + 
  labs(title = "Nivel de Motintr en Europa Occidental")

# Crear el mapa para motprosoc con mayor contraste de saturación
ggplot() +
  geom_sf(data = europe_map, fill = "grey", color = "black") + 
  geom_sf(data = merged_data_motprosoc, aes(fill = mean_motprosoc), color = "black") +
  scale_fill_gradient(low = "#CCFFCC", high = "#006600", na.value = "grey", name = "Media") +
  theme_minimal() +
  coord_sf(xlim = c(-10, 30), ylim = c(35, 70)) + 
  labs(title = "Nivel de Motprosoc en Europa Occidental")

####### Gráfico Europa con el tipo de motivación más alto por país
# Calcular cuál es la motivación más alta para cada país

# Calcular cuál es la motivación más alta para cada país
country_motivation <- df_filtered %>%
  group_by(cntry) %>%
  summarise(
    mean_motprosoc = mean(motprosoc, na.rm = TRUE),
    mean_motextr = mean(motextr, na.rm = TRUE),
    mean_motintr = mean(motintr, na.rm = TRUE)
  ) %>%
  rowwise() %>%
  mutate(
    highest_motivation = case_when(
      mean_motprosoc >= mean_motextr & mean_motprosoc >= mean_motintr ~ "Motprosoc",
      mean_motextr >= mean_motprosoc & mean_motextr >= mean_motintr ~ "Motextr",
      mean_motintr >= mean_motprosoc & mean_motintr >= mean_motextr ~ "Motintr"
    )
  ) %>%
  ungroup()

# Ajustar los nombres de los países en country_motivation
country_motivation <- country_motivation %>%
  mutate(
    cntry = recode(cntry, GB = "United Kingdom", FR = "France", PT = "Portugal", IT = "Italy", 
                   ES = "Spain", DE = "Germany", NL = "Netherlands", BE = "Belgium", 
                   IE = "Ireland", AT = "Austria", CH = "Switzerland", DK = "Denmark", 
                   FI = "Finland", SE = "Sweden", NO = "Norway")
  )

# Cargar el shapefile de los países de Europa
europe_map <- ne_countries(scale = "medium", continent = "Europe", returnclass = "sf")

# Revisar los nombres en el mapa
unique(europe_map$name_long)

# Fusionar los datos de motivación con el mapa
merged_data_motivation <- merge(europe_map, country_motivation, by.x = "name_long", by.y = "cntry", all.x = TRUE)

# Crear el mapa con colores para cada tipo de motivación
ggplot() +
  geom_sf(data = europe_map, fill = "grey", color = "black") + 
  geom_sf(data = merged_data_motivation, aes(fill = highest_motivation), color = "black") +
  scale_fill_manual(values = c("Motprosoc" = "green", "Motextr" = "red", "Motintr" = "blue"), na.value = "grey", name = "Motivación más alta") +
  theme_minimal() +
  coord_sf(xlim = c(-10, 30), ylim = c(35, 70)) + 
  labs(title = "Tipo de Motivación más Alto en Europa Occidental")


###### Clasificación por países en cada motivación
### Combinadas y verticales
# Cargar librerías necesarias
library(ggplot2)
library(dplyr)
library(tidyr)
library(forcats)

# Ordenar los datos por cada tipo de motivación y agregar una columna de ranking
country_motextr <- df_filtered %>%
  group_by(cntry) %>%
  summarise(mean_motextr = mean(motextr, na.rm = TRUE)) %>%
  arrange(desc(mean_motextr)) %>%
  mutate(rank = row_number())

country_motintr <- df_filtered %>%
  group_by(cntry) %>%
  summarise(mean_motintr = mean(motintr, na.rm = TRUE)) %>%
  arrange(desc(mean_motintr)) %>%
  mutate(rank = row_number())

country_motprosoc <- df_filtered %>%
  group_by(cntry) %>%
  summarise(mean_motprosoc = mean(motprosoc, na.rm = TRUE)) %>%
  arrange(desc(mean_motprosoc)) %>%
  mutate(rank = row_number())

# Unir los datos en un solo DataFrame
country_motivation <- country_motextr %>%
  select(cntry, rank, mean_motextr) %>%
  rename(rank_motextr = rank) %>%
  left_join(country_motintr %>%
              select(cntry, rank, mean_motintr) %>%
              rename(rank_motintr = rank), by = "cntry") %>%
  left_join(country_motprosoc %>%
              select(cntry, rank, mean_motprosoc) %>%
              rename(rank_motprosoc = rank), by = "cntry")

# Crear un DataFrame largo para ggplot
country_motivation_long <- country_motivation %>%
  pivot_longer(cols = c(mean_motextr, mean_motintr, mean_motprosoc), 
               names_to = "motivation", values_to = "mean_value") %>%
  mutate(motivation = recode(motivation, 
                             mean_motextr = "Motextr",
                             mean_motintr = "Motintr",
                             mean_motprosoc = "Motprosoc"))

# Crear los gráficos de barras paralelas para el ranking de países en cada tipo de motivación
ggplot(country_motivation_long, aes(x = fct_reorder(cntry, mean_value, .desc = TRUE), y = mean_value, fill = motivation)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Comparación del Ranking de Países en Cada Tipo de Motivación",
       x = "País",
       y = "Media de Motivación",
       fill = "Motivación") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

### Gráficos horizontales separados para cada tipo de motivación
# Gráfico para Motextr
ggplot(country_motextr, aes(x = fct_reorder(cntry, mean_motextr), y = mean_motextr)) +
  geom_bar(stat = "identity", fill = "salmon", color = "black", width = 0.6) +
  coord_flip() +
  labs(title = "Ranking de Países por Motextr",
       x = "País",
       y = "Media de Motextr") +
  theme_minimal()

# Gráfico para Motintr
ggplot(country_motintr, aes(x = fct_reorder(cntry, mean_motintr), y = mean_motintr)) +
  geom_bar(stat = "identity", fill = "lightgreen", color = "black", width = 0.6) +
  coord_flip() +
  labs(title = "Ranking de Países por Motintr",
       x = "País",
       y = "Media de Motintr") +
  theme_minimal()

# Gráfico para Motprosoc
ggplot(country_motprosoc, aes(x = fct_reorder(cntry, mean_motprosoc), y = mean_motprosoc)) +
  geom_bar(stat = "identity", fill = "lightblue", color = "black", width = 0.6) +
  coord_flip() +
  labs(title = "Ranking de Países por Motprosoc",
       x = "País",
       y = "Media de Motprosoc") +
  theme_minimal()

######## ANOVAs que nos permitan explorar si hay diferencias
######## estadísticamente significativas entre países para cada una de las motivaciones.
######## Primero, todos los países; luego, sin Italia.
# Cargar librerías necesarias

# Cargar librerías necesarias
library(dplyr)

# Ordenar los datos por cada tipo de motivación y agregar una columna de ranking
country_motextr <- df_filtered %>%
  group_by(cntry) %>%
  summarise(mean_motextr = mean(motextr, na.rm = TRUE)) %>%
  arrange(desc(mean_motextr)) %>%
  mutate(rank = row_number())

country_motintr <- df_filtered %>%
  group_by(cntry) %>%
  summarise(mean_motintr = mean(motintr, na.rm = TRUE)) %>%
  arrange(desc(mean_motintr)) %>%
  mutate(rank = row_number())

country_motprosoc <- df_filtered %>%
  group_by(cntry) %>%
  summarise(mean_motprosoc = mean(motprosoc, na.rm = TRUE)) %>%
  arrange(desc(mean_motprosoc)) %>%
  mutate(rank = row_number())

# Unir los datos en un solo DataFrame
country_motivation <- country_motextr %>%
  select(cntry, rank, mean_motextr) %>%
  rename(rank_motextr = rank) %>%
  left_join(country_motintr %>%
              select(cntry, rank, mean_motintr) %>%
              rename(rank_motintr = rank), by = "cntry") %>%
  left_join(country_motprosoc %>%
              select(cntry, rank, mean_motprosoc) %>%
              rename(rank_motprosoc = rank), by = "cntry")

# Análisis ANOVA para cada motivación
# ANOVA para motextr
anova_motextr <- aov(motextr ~ cntry, data = df_filtered)
anova_motextr_summary <- summary(anova_motextr)

# ANOVA para motintr
anova_motintr <- aov(motintr ~ cntry, data = df_filtered)
anova_motintr_summary <- summary(anova_motintr)

# ANOVA para motprosoc
anova_motprosoc <- aov(motprosoc ~ cntry, data = df_filtered)
anova_motprosoc_summary <- summary(anova_motprosoc)

# Excluir Italia y repetir los ANOVAs
df_filtered_no_italy <- df_filtered %>%
  filter(cntry != "IT")

# ANOVA para motextr sin Italia
anova_motextr_no_italy <- aov(motextr ~ cntry, data = df_filtered_no_italy)
anova_motextr_no_italy_summary <- summary(anova_motextr_no_italy)

# ANOVA para motintr sin Italia
anova_motintr_no_italy <- aov(motintr ~ cntry, data = df_filtered_no_italy)
anova_motintr_no_italy_summary <- summary(anova_motintr_no_italy)

# ANOVA para motprosoc sin Italia
anova_motprosoc_no_italy <- aov(motprosoc ~ cntry, data = df_filtered_no_italy)
anova_motprosoc_no_italy_summary <- summary(anova_motprosoc_no_italy)

# Mostrar los resultados
anova_motextr_summary
anova_motintr_summary
anova_motprosoc_summary

anova_motextr_no_italy_summary
anova_motintr_no_italy_summary
anova_motprosoc_no_italy_summary

############# y ahora lo mismo con las otras variables
# Realizar ANOVAs para las variables especificadas
anova_happy <- aov(happy ~ cntry, data = df_filtered)
anova_impfree <- aov(impfree ~ cntry, data = df_filtered)
anova_ipeqopt <- aov(ipeqopt ~ cntry, data = df_filtered)
anova_ipfrule <- aov(ipfrule ~ cntry, data = df_filtered)

# Resúmenes de los ANOVAs
anova_happy_summary <- summary(anova_happy)
anova_impfree_summary <- summary(anova_impfree)
anova_ipeqopt_summary <- summary(anova_ipeqopt)
anova_ipfrule_summary <- summary(anova_ipfrule)

# Mostrar los resultados
anova_happy_summary
anova_impfree_summary
anova_ipeqopt_summary
anova_ipfrule_summary

####### y chi square para atncrse
# Prueba de chi-cuadrado para atncrse
# Crear una tabla de contingencia para atncrse y el país
atncrse_table <- table(df_filtered$cntry, df_filtered$atncrse)

# Realizar la prueba de chi-cuadrado
chi_square_test <- chisq.test(atncrse_table)

# Mostrar los resultados
chi_square_test

# Verificar las frecuencias en la tabla de contingencia
atncrse_table <- table(df_filtered$cntry, df_filtered$atncrse)
print(atncrse_table)

# Ajustar frecuencias añadiendo un valor pequeño a cada celda
atncrse_table_adj <- atncrse_table + 0.5

# Realizar la prueba de chi-cuadrado con la tabla ajustada
chi_square_test_adj <- chisq.test(atncrse_table_adj)

# Mostrar los resultados
chi_square_test_adj


##### Gráfico de mariposa por SEXO con las tres motivaciones y filtrado a solo Francia
# Filtrar los datos para incluir solo sujetos de Francia
# Filtrar los datos para Francia
df_france <- df_filtered %>%
  filter(cntry == "FR")

# Crear una base de datos larga para ggplot
df_long_france <- df_france %>%
  select(gndr, motprosoc, motextr, motintr) %>%
  pivot_longer(cols = c("motprosoc", "motextr", "motintr"), names_to = "motivation", values_to = "value")

# Crear una columna para separar las barras hacia la izquierda o la derecha
df_long_france <- df_long_france %>%
  mutate(value = ifelse(gndr == "Hombre", -value, value))

# Crear el gráfico de mariposa
ggplot(df_long_france, aes(x = motivation, y = value, fill = gndr)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Comparación de Motivaciones por Sexo en Francia",
       x = "Motivación",
       y = "Valor de Motivación",
       fill = "Sexo") +
  theme_minimal() +
  scale_y_continuous(labels = abs) +  # Mostrar valores absolutos en el eje y
  coord_flip()  # Girar el gráfico para que las barras sean horizontales

### Y ahora lo mismo pero con todos los países (sin filtrar por Francia)
# Crear una base de datos larga para ggplot
df_long_all_countries <- df_filtered %>%
  select(cntry, gndr, motprosoc, motextr, motintr) %>%
  pivot_longer(cols = c("motprosoc", "motextr", "motintr"), names_to = "motivation", values_to = "value")

# Crear una columna para separar las barras hacia la izquierda o la derecha
df_long_all_countries <- df_long_all_countries %>%
  mutate(value = ifelse(gndr == "Hombre", -value, value))

# Crear el gráfico de mariposa
ggplot(df_long_all_countries, aes(x = motivation, y = value, fill = gndr)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Comparación de Motivaciones por Sexo en Todos los Países",
       x = "Motivación",
       y = "Valor de Motivación",
       fill = "Sexo") +
  theme_minimal() +
  scale_y_continuous(labels = abs) +  # Mostrar valores absolutos en el eje y
  coord_flip()  # Girar el gráfico para que las barras sean horizontales



