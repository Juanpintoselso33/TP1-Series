# -*- coding: utf-8 -*-
# ---
# jupyter:
#   jupytext:
#     text_representation:
#       extension: .r
#       format_name: light
#       format_version: '1.5'
#       jupytext_version: 1.16.7
#   kernelspec:
#     display_name: R (System)
#     language: R
#     name: r_system
# ---

# # TP1: Metodología Box-Jenkins y Suavizado Exponencial ETS

# # 1. Configuración inicial y carga de datos

# ### 1.1 Instalación y carga de librerías

# + vscode={"languageId": "r"}
# Instalar paquetes si no están disponibles
# if(!require(readxl)) install.packages("readxl")
# if(!require(forecast)) install.packages("forecast")
# if(!require(tseries)) install.packages("tseries")
# if(!require(ggplot2)) install.packages("ggplot2")
# if(!require(dplyr)) install.packages("dplyr")
# if(!require(zoo)) install.packages("zoo")
# if(!require(lubridate)) install.packages("lubridate")

# Cargar librerías necesarias
library(readxl)
library(forecast)
library(tseries)
library(ggplot2)
library(dplyr)
library(zoo)
library(lubridate)

# Definir ruta del proyecto
project_path <- "C:/Users/trico/OneDrive/UBA/Series de tiempo/TP1"

# 2.1) Cargar función de diagnóstico de residuos
source(file.path(project_path, "corr_res.R"))

# Configurar opciones
options(scipen = 999) # Evitar notación científica
par(bg = "white") # Fondo blanco para gráficos base

# -

# ### 1.2 Lectura de datos

# + vscode={"languageId": "r"}
# Leer datos de Argentina (skip=1 para eliminar sólo el título)
datos_arg <- read_excel(
  file.path(project_path, "Datos Argentina.xlsx"),
  skip      = 1,
  col_names = c("Periodo", "EMAE", "IPC", "M2N", "M2R", "TASA")
)

# Leer datos de Chile (skip=3 para saltar título, línea en blanco y cabecera)
datos_chi <- read_excel(
  file.path(project_path, "Datos Chile.xlsx"),
  skip      = 3,
  col_names = c("Periodo", "IMACEC", "M2N", "IPC", "M2R", "TASA")
)

# Mostrar primeras filas para verificar
head(datos_arg, 5)
head(datos_chi, 5)

# -

# # 2. Preparación de series temporales
#
#

# ### 2.1 Creación de series temporales y diferencias logarítmicas

# + vscode={"languageId": "r"}
datos_arg$Fecha <- as.Date(as.yearmon(gsub("M", "-", datos_arg$Periodo), "%Y-%m"))
datos_chi$Fecha <- as.Date(as.numeric(datos_chi$Periodo), origin = "1899-12-30")

# Crear objetos ts (enero-2004 en adelante)
ts_emae <- ts(datos_arg$EMAE, start = c(2004, 1), frequency = 12)
ts_ipc_arg <- ts(datos_arg$IPC, start = c(2004, 1), frequency = 12)
ts_m2_arg <- ts(datos_arg$M2N, start = c(2004, 1), frequency = 12)
ts_tasa_arg <- ts(datos_arg$TASA, start = c(2004, 1), frequency = 12)

ts_imacec <- ts(datos_chi$IMACEC, start = c(2004, 1), frequency = 12)
ts_ipc_chi <- ts(datos_chi$IPC, start = c(2004, 1), frequency = 12)
ts_m2_chi <- ts(datos_chi$M2N, start = c(2004, 1), frequency = 12)
ts_tasa_chi <- ts(datos_chi$TASA, start = c(2004, 1), frequency = 12)


# + vscode={"languageId": "r"}
# Calcular diferencias logarítmicas de cada serie temporal previamente creada

# Función para calcular la diferencia logarítmica
# (aproxima la tasa de crecimiento)
calcular_diff_log <- function(x) diff(log(x))

# --- Series de Argentina ---
# Dif. log EMAE (Act. Económica) Arg
dl_emae <- calcular_diff_log(ts_emae)
# Dif. log IPC (Precios) Arg
dl_ipc_arg <- calcular_diff_log(ts_ipc_arg)
# Dif. log M2 (Agregado Monetario) Arg
dl_m2_arg <- calcular_diff_log(ts_m2_arg)
# Dif. log TASA (Interés) Arg
dl_tasa_arg <- calcular_diff_log(ts_tasa_arg)

# --- Series de Chile ---
# Dif. log IMACEC (Act. Económica) Chi
dl_imacec <- calcular_diff_log(ts_imacec)
# Dif. log IPC (Precios) Chi
dl_ipc_chi <- calcular_diff_log(ts_ipc_chi)
# Dif. log M2 (Agregado Monetario) Chi
dl_m2_chi <- calcular_diff_log(ts_m2_chi)
# Dif. log TASA (Interés) Chi
dl_tasa_chi <- calcular_diff_log(ts_tasa_chi)

# Definir ventanas de entrenamiento para cada serie diferenciada
# Período de entrenamiento: hasta diciembre 2022

# --- Ventanas de Entrenamiento - Argentina ---
# EMAE Arg (entrenamiento)
dl_emae_train <- window(dl_emae, end = c(2022, 12))
# IPC Arg (entrenamiento)
dl_ipc_arg_train <- window(dl_ipc_arg, end = c(2022, 12))
# M2 Arg (entrenamiento)
dl_m2_arg_train <- window(dl_m2_arg, end = c(2022, 12))
# TASA Arg (entrenamiento)
dl_tasa_arg_train <- window(dl_tasa_arg, end = c(2022, 12))

# --- Ventanas de Entrenamiento - Chile ---
# IMACEC Chi (entrenamiento)
dl_imacec_train <- window(dl_imacec, end = c(2022, 12))
# IPC Chi (entrenamiento)
dl_ipc_chi_train <- window(dl_ipc_chi, end = c(2022, 12))
# M2 Chi (entrenamiento)
dl_m2_chi_train <- window(dl_m2_chi, end = c(2022, 12))
# TASA Chi (entrenamiento)
dl_tasa_chi_train <- window(dl_tasa_chi, end = c(2022, 12))

# -

# # 3. Estimacion de modelos

# ## Argentina

# ### 3.1 Estimacion EMAE - Arg

# + vscode={"languageId": "r"}
p_emae <- 4
d_emae <- 0
q_emae <- 0
P_emae <- 0
D_emae <- 1
Q_emae <- 1

model_emae_arg <- Arima( # Sobrescribe la variable original
  dl_emae_train,
  order = c(p_emae, d_emae, q_emae),
  seasonal = list(order = c(P_emae, D_emae, Q_emae), period = 12),
  method = "ML" # Usar Máxima Verosimilitud (o CSS-ML si ML falla)
)

summary(model_emae_arg)

corr_res(
  model_emae_arg,
  lags = 26,
  p_q = p_emae + q_emae + P_emae + Q_emae
)

# -

# ### 3.2 Estimacion IPC - Arg

# + vscode={"languageId": "r"}
# Volviendo al modelo básico pero con mayor complejidad AR
p_ipc <- 3
d_ipc <- 0
q_ipc <- 0
P_ipc <- 0
D_ipc <- 1
Q_ipc <- 1

model_ipc_arg <- Arima(
  dl_ipc_arg_train,
  order = c(p_ipc, d_ipc, q_ipc),
  seasonal = list(order = c(P_ipc, D_ipc, Q_ipc), period = 12),
  method = "ML"
)

summary(model_ipc_arg)
corr_res(model_ipc_arg,
  lags = 26,
  p_q = p_ipc + q_ipc + P_ipc + Q_ipc
)

# -

# ### 3.3 Estimacion Agregado Monetario M2 - Arg

# + vscode={"languageId": "r"}
model_m2_arg <- Arima(
  dl_m2_arg_train,
  order    = c(1, 0, 4), # MA(4) añadido
  seasonal = list(order = c(0, 1, 1), period = 12),
  method   = "ML",
)

summary(model_m2_arg)

# Diagnóstico residuos
corr_res(model_m2_arg,
  lags = 26,
  p_q  = 1 + 4 + 0 + 1
) # 1 AR + 4 MA + 1 SMA

# -

# ### 3.4 Estimacion Tasa de interes - Arg

# + vscode={"languageId": "r"}
# Especificar órdenes del modelo IMACEC
p_tasa <- 4
d_tasa <- 0
q_tasa <- 0
P_tasa <- 0
D_tasa <- 1
Q_tasa <- 1


model_tasa_arg <- Arima(
  dl_tasa_arg_train,
  order = c(p_tasa, d_tasa, q_tasa),
  seasonal = list(order = c(P_tasa, D_tasa, Q_tasa), period = 12),
  method = "ML" #
)

# Resumen del modelo TASA con estructura forzada
summary(model_tasa_arg)

corr_res(
  model_tasa_arg,
  lags = 26,
  p_q = p_tasa + q_tasa + P_tasa + Q_tasa
)

# -

# ## Chile

# ### 3.5 Estimacion IMACEC - Chile

# + vscode={"languageId": "r"}
model_imacec_chi <- Arima(
  y = dl_imacec_train,
  order = c(4, 0, 2),
  seasonal = list(order = c(0, 1, 1), period = 12),
  include.drift = FALSE,
  include.mean = TRUE,
  method = "ML"
)

# Resumen del modelo
summary(model_imacec_chi)

# Cálculo del número de parámetros para el test de Ljung-Box
num_arma_params <- sum(model_imacec_chi$arma[c(1, 2, 3, 4)]) # p + q + P + Q
drift_present <- as.integer("drift" %in% names(model_imacec_chi$coef))
grados_libertad_q <- num_arma_params + drift_present

cat(
  "Calculando p_q como:", num_arma_params, "(arma) +", drift_present, "(drift) =",
  grados_libertad_q, "\n"
)

# Diagnóstico de residuos
corr_res(
  xreg = model_imacec_chi,
  lags = 35,
  p_q  = grados_libertad_q
)

# -

# ### 3.6 Estimacion IPC - Chile

# + vscode={"languageId": "r"}
# IPC Chile: Mejora del modelo con enfoque mixto

# TODO: probar desestacionalizacion x-12 con seasonal

# 1. Ajuste manual del modelo considerando componente estacional explícito
model_ipc_chi <- Arima(
  dl_ipc_chi_train,
  order = c(1, 1, 6),           # Simplificar componente ARMA
  seasonal = list(order = c(1, 0, 1), period = 12),  # Agregar componente estacional
  include.mean = TRUE,          # Mantener intercepto
  method = "ML"
)

# 2. Resumen del modelo
summary(model_ipc_chi)

# 3. Verificar correlación en residuos
p_q <- 1 + 1 + 1 + 1  # AR + MA + SAR + SMA
corr_res(model_ipc_chi, lags = 26, p_q = p_q)
# -

# ### 3.7 Estimacion Agregado Monetario M2 - Chile

# + vscode={"languageId": "r"}

model_m2_chi <- auto.arima(
  dl_m2_chi_train,
  seasonal = TRUE, # Permitir búsqueda estacional (P, Q, D)
  stepwise = TRUE, # Búsqueda por pasos (más rápida)
  approximation = TRUE # Usar aproximaciones (más rápido)
)

summary(model_m2_chi)

corr_res(
  model_m2_chi,
  lags = 26,
  p_q = sum(model_m2_chi$arma[c(1, 2, 3, 4)])
)
# -

# ### 3.8 Estimacion Tasa de interes - Chile

# + vscode={"languageId": "r"}
p_tasa_chi <- 4
d_tasa_chi <- 0
q_tasa_chi <- 0
P_tasa_chi <- 0
D_tasa_chi <- 1
Q_tasa_chi <- 1

model_tasa_chi <- Arima( # Sobrescribe la variable original
  dl_tasa_chi_train,
  order = c(p_tasa_chi, d_tasa_chi, q_tasa_chi),
  seasonal = list(order = c(P_tasa_chi, D_tasa_chi, Q_tasa_chi), period = 12),
  method = "ML" # Usar Máxima Verosimilitud (o CSS-ML si ML falla)
)

summary(model_tasa_chi)

corr_res(
  model_tasa_chi,
  lags = 26,
  p_q = p_tasa_chi + q_tasa_chi + P_tasa_chi + Q_tasa_chi
)


# -

# # 4 Pronosticos de Diferencias Logaritmicas

# + vscode={"languageId": "r"}
source("plot_forecast_comparison.R")

# -

# ## Argentina

# ### 4.1 Proyeccion EMAE - Argentina

# + vscode={"languageId": "r"}
# 2. Definir parámetros para el gráfico
h_forecast_emae <- 12 # Horizonte ya definido
model_to_plot_emae <- model_emae_arg # Modelo ARIMA ajustado (forzado)
original_ts_emae <- dl_emae
title_emae <- "Dif. Log. EMAE Argentina"
start_plot_emae <- 2022 # Año desde donde mostrar histórico
ylabel_emae <- "Diferencia Logarítmica" # Etiqueta eje Y

# 3. Llamar a la función para generar el objeto ggplot
plot_emae_comparison <- plot_forecast_vs_actual(
  model = model_to_plot_emae,
  original_ts = original_ts_emae,
  h = h_forecast_emae,
  series_title = title_emae,
  start_year_plot = start_plot_emae,
  ylab_text = ylabel_emae,
  show_pi = FALSE # No mostrar Intervalos de Predicción por ahora
)

# 4. Mostrar el gráfico generado
print(plot_emae_comparison)

# -

# ### 4.2 Proyeccion IPC - Argentina

# + vscode={"languageId": "r"}
h_forecast_ipc <- 12 # Horizonte
model_to_plot_ipc <- model_ipc_arg # Modelo ARIMA ajustado (forzado o auto.arima)
original_ts_ipc <- dl_ipc_arg # Serie original completa (diferencias log)
title_ipc <- "Dif. Log. IPC Argentina" # Título para el gráfico
start_plot_ipc <- 2022 # Año desde donde mostrar histórico
ylabel_ipc <- "Diferencia Logarítmica" # Etiqueta eje Y

plot_ipc_comparison <- plot_forecast_vs_actual(
  model = model_to_plot_ipc,
  original_ts = original_ts_ipc,
  h = h_forecast_ipc,
  series_title = title_ipc,
  start_year_plot = start_plot_ipc,
  ylab_text = ylabel_ipc,
  show_pi = FALSE # No mostrar Intervalos de Predicción
)

# 4. Mostrar el gráfico generado
print(plot_ipc_comparison)

# -

# ### 4.3 Proyeccion Agregado Monetario M2 - Argentina

# + vscode={"languageId": "r"}
# 2. Definir parámetros específicos para el gráfico del M2
h_forecast_m2 <- 12 # Horizonte
model_to_plot_m2 <- model_m2_arg # Modelo ARIMA ajustado
original_ts_m2 <- dl_m2_arg # Serie original completa (diferencias log)
title_m2 <- "Dif. Log. M2 Argentina" # Título para el gráfico
start_plot_m2 <- 2022 # Año desde donde mostrar histórico
ylabel_m2 <- "Diferencia Logarítmica" # Etiqueta eje Y

# 3. Llamar a la función para generar el objeto ggplot
plot_m2_comparison <- plot_forecast_vs_actual(
  model = model_to_plot_m2,
  original_ts = original_ts_m2,
  h = h_forecast_m2,
  series_title = title_m2,
  start_year_plot = start_plot_m2,
  ylab_text = ylabel_m2,
  show_pi = FALSE # No mostrar Intervalos de Predicción
)

# 4. Mostrar el gráfico generado
print(plot_m2_comparison)

# -

# ### 4.4 Proyeccion Tasa de interes - Argentina

# + vscode={"languageId": "r"}
h_forecast_tasa <- 12 # Horizonte
model_to_plot_tasa <- model_tasa_arg # Modelo ARIMA ajustado (forzado o auto.arima)
original_ts_tasa <- dl_tasa_arg # Serie original completa (diferencias log)
title_tasa <- "Dif. Log. TASA Argentina" # Título para el gráfico
start_plot_tasa <- 2022 # Año desde donde mostrar histórico
ylabel_tasa <- "Diferencia Logarítmica" # Etiqueta eje Y

plot_tasa_comparison <- plot_forecast_vs_actual(
  model = model_to_plot_tasa,
  original_ts = original_ts_tasa,
  h = h_forecast_tasa,
  series_title = title_tasa,
  start_year_plot = start_plot_tasa,
  ylab_text = ylabel_tasa,
  show_pi = FALSE # No mostrar Intervalos de Predicción
)

# 4. Mostrar el gráfico generado
print(plot_tasa_comparison)
# -

# ## Chile

# ### 4.5 Proyeccion IMACEC - Chile

# + vscode={"languageId": "r"}
# Modelo ARIMA simplificado sin regresores externos
h_forecast_imacec <- 12
model_to_plot_imacec <- model_imacec_chi
original_ts_imacec <- dl_imacec
title_imacec <- "Dif. Log. IMACEC Chile"
start_plot_imacec <- 2022
ylabel_imacec <- "Diferencia Logarítmica"

# Utilizar la función modular directamente sin complicaciones
plot_imacec_comparison <- plot_forecast_vs_actual(
  model           = model_to_plot_imacec,
  original_ts     = original_ts_imacec,
  h               = h_forecast_imacec,
  series_title    = title_imacec,
  start_year_plot = start_plot_imacec,
  ylab_text       = ylabel_imacec,
  show_pi         = FALSE
)

# Mostrar el gráfico
print(plot_imacec_comparison)

# -

# ### 4.6 Proyeccion IPC - Chile

# + vscode={"languageId": "r"}
# --- Graficar Pronóstico IPC Chile con modelo ARIMA vs Real usando función modular ---

# 2. Definir parámetros para el gráfico del IPC Chile
h_forecast_ipc_chi <- 12                      # Horizonte
model_to_plot_ipc_chi <- model_ipc_chi        # Usar el nuevo modelo ARIMA
original_ts_ipc_chi <- dl_ipc_chi             # Serie original completa (diferencias log)
title_ipc_chi <- "Dif. Log. IPC Chile (ARIMA)"  # Actualizar título
start_plot_ipc_chi <- 2022                    # Año desde donde mostrar histórico
ylabel_ipc_chi <- "Diferencia Logarítmica"    # Etiqueta eje Y

# 3. Llamar a la función para generar el objeto ggplot
# No necesitamos xreg ya que el nuevo modelo no usa regresores externos
plot_ipc_chi_comparison <- plot_forecast_vs_actual(
  model = model_to_plot_ipc_chi,
  original_ts = original_ts_ipc_chi,
  h = h_forecast_ipc_chi,
  series_title = title_ipc_chi,
  start_year_plot = start_plot_ipc_chi,
  ylab_text = ylabel_ipc_chi,
  show_pi = FALSE  # No mostrar Intervalos de Predicción
)

# 4. Mostrar el gráfico generado
print(plot_ipc_chi_comparison)
# -

# ### 4.6 Proyeccion Agregado Monetario M2 - Chile

# + vscode={"languageId": "r"}
# --- Graficar Pronóstico M2 Chile vs Real usando función modular ---

# 1. Cargar la función desde el script R (si no se ha hecho antes)
# source("plot_forecast_comparison.R")

# 2. Definir parámetros específicos para el gráfico del M2 Chile
h_forecast_m2_chi <- 12 # Horizonte (asumido)
model_to_plot_m2_chi <- model_m2_chi # Modelo ARIMA ajustado
original_ts_m2_chi <- dl_m2_chi # Serie original completa (diferencias log)
title_m2_chi <- "Dif. Log. M2 Chile" # Título para el gráfico
start_plot_m2_chi <- 2022 # Año desde donde mostrar histórico
ylabel_m2_chi <- "Diferencia Logarítmica" # Etiqueta eje Y

# 3. Llamar a la función para generar el objeto ggplot
plot_m2_chi_comparison <- plot_forecast_vs_actual(
  model = model_to_plot_m2_chi,
  original_ts = original_ts_m2_chi,
  h = h_forecast_m2_chi,
  series_title = title_m2_chi,
  start_year_plot = start_plot_m2_chi,
  ylab_text = ylabel_m2_chi,
  show_pi = FALSE # No mostrar Intervalos de Predicción
)

# 4. Mostrar el gráfico generado
print(plot_m2_chi_comparison)

# 5. (Opcional) Guardar el gráfico con dimensiones y resolución específicas
# DESCOMENTA para guardar la imagen.
# dir.create("graficos", showWarnings = FALSE)
# ggsave(
#   filename = "graficos/pronostico_m2_chi_comparacion.png", # Nombre de archivo
#   plot = plot_m2_chi_comparison,
#   width = 10,  # Ancho
#   height = 6,  # Alto
#   dpi = 300    # Resolución
# )
# print("Gráfico guardado en graficos/pronostico_m2_chi_comparacion.png")

# -

# ### 4.7 Proyeccion Diferencia Logaritmica de Tasa de Interes - Chile

# + vscode={"languageId": "r"}
# --- Graficar Pronóstico TASA Chile vs Real usando función modular ---

# 1. Cargar la función desde el script R (si no se ha hecho antes)
# source("plot_forecast_comparison.R")

# 2. Definir parámetros específicos para el gráfico de la TASA Chile
h_forecast_tasa_chi <- 12 # Horizonte
model_to_plot_tasa_chi <- model_tasa_chi # Modelo ARIMA ajustado (forzado o auto.arima)
original_ts_tasa_chi <- dl_tasa_chi # Serie original completa (diferencias log)
title_tasa_chi <- "Dif. Log. TASA Chile" # Título para el gráfico
start_plot_tasa_chi <- 2022 # Año desde donde mostrar histórico
ylabel_tasa_chi <- "Diferencia Logarítmica" # Etiqueta eje Y

# 3. Llamar a la función para generar el objeto ggplot
# Se usa el modelo actual 'model_tasa_chi'. No se pasa xreg.
plot_tasa_chi_comparison <- plot_forecast_vs_actual(
  model = model_to_plot_tasa_chi,
  original_ts = original_ts_tasa_chi,
  h = h_forecast_tasa_chi,
  series_title = title_tasa_chi,
  start_year_plot = start_plot_tasa_chi,
  ylab_text = ylabel_tasa_chi,
  show_pi = FALSE # No mostrar Intervalos de Predicción
)

# 4. Mostrar el gráfico generado
print(plot_tasa_chi_comparison)

# 5. (Opcional) Guardar el gráfico con dimensiones y resolución específicas
# DESCOMENTA para guardar la imagen.
# dir.create("graficos", showWarnings = FALSE)
# ggsave(
#   filename = "graficos/pronostico_tasa_chi_comparacion.png", # Nombre de archivo
#   plot = plot_tasa_chi_comparison,
#   width = 10,  # Ancho
#   height = 6,  # Alto
#   dpi = 300    # Resolución
# )
# print("Gráfico guardado en graficos/pronostico_tasa_chi_comparacion.png")

# -

# # 5 - Comparacion y evaluacion de proyecciones

# + vscode={"languageId": "r"}
# Cargar eval_pron.R
source("eval_pron.R")


# + vscode={"languageId": "r"}
# --- Retransformación con corrección de Jensen (con manejo de longitudes) ---
retransform_diflog <- function(fc, last_level) {
  # Convertir a valor escalar (número)
  last_log_value <- as.numeric(log(last_level))
  cum_log <- cumsum(fc$mean)

  # Crear vector de resultados
  h <- length(cum_log)
  forecast_levels <- numeric(h)

  # Cálculo manual para cada horizonte
  for (i in 1:h) {
    # Con o sin corrección de Jensen según disponibilidad
    if (!is.null(fc$se) && is.numeric(fc$se)) {
      sigma2 <- (fc$se[i])^2
      forecast_levels[i] <- exp(last_log_value + cum_log[i] + 0.5 * sigma2)
    } else {
      # Sin corrección de Jensen
      forecast_levels[i] <- exp(last_log_value + cum_log[i])
    }
  }

  # Convertir a objeto ts con la misma frecuencia y fecha de inicio que el pronóstico
  start_fc <- start(fc$mean)
  freq_fc <- frequency(fc$mean)
  return(ts(forecast_levels, start = start_fc, frequency = freq_fc))
}

# -

# ## 5.1 Comparacion y evaluacion de modelos argentinos

# + vscode={"languageId": "r"}
# --- Evaluación de Pronósticos ARGENTINA en Niveles (2023) ---

# 2. Definir horizonte y período de evaluación (común)
h_eval <- 12
start_eval <- c(2023, 1)
end_eval <- c(2023, 12)

# 3. Lista para almacenar los resultados de evaluación de Argentina
results_list_arg <- list()

# --- Procesamiento y Evaluación Series ARGENTINAS ---

# Configuración solo para Argentina
series_config_arg <- list(
  list(name = "EMAE Arg", model = model_emae_arg, original_ts = ts_emae, xreg = NULL),
  list(name = "IPC Arg", model = model_ipc_arg, original_ts = ts_ipc_arg, xreg = NULL),
  list(name = "M2 Arg", model = model_m2_arg, original_ts = ts_m2_arg, xreg = NULL),
  list(name = "TASA Arg", model = model_tasa_arg, original_ts = ts_tasa_arg, xreg = NULL)
)

# Bucle para series argentinas
for (cfg in series_config_arg) {
  cat("Procesando ARG:", cfg$name, "\n")

  # Generar pronóstico de diferencias logarítmicas
  fc_diflog <- tryCatch(
    {
      forecast(cfg$model, h = h_eval) # No se necesita xreg para Arg
    },
    error = function(e) {
      warning(paste("Error al pronosticar para", cfg$name, ":", e$message))
      return(NULL)
    }
  )
  if (is.null(fc_diflog)) next

  # Obtener último valor observado de 2022 para retransformación
  last_obs_2022 <- tail(window(cfg$original_ts, end = c(2022, 12)), 1)

  # Retransformar a niveles usando la función común
  fc_levels <- retransform_diflog(fc_diflog, last_obs_2022)

  # Obtener valores reales en niveles
  actual_levels <- window(cfg$original_ts, start = start_eval, end = end_eval)

  # Preparar vectores y validar longitudes
  Y_P <- as.vector(fc_levels)
  Y_A <- as.vector(actual_levels)
  if (length(Y_A) == 0) {
    warning(paste("No hay datos reales para", cfg$name))
    next
  }
  min_len <- min(length(Y_P), length(Y_A))
  if (min_len < h_eval) {
    warning(paste("Datos incompletos en", cfg$name))
  }
  Y_P <- Y_P[1:min_len]
  Y_A <- Y_A[1:min_len]

  # Evaluar
  evaluation_metrics <- tryCatch(
    {
      Eval_Pron(Y_P = Y_P, Y_A = Y_A, Nombre = cfg$name)
    },
    error = function(e) {
      warning(paste("Error en Eval_Pron para", cfg$name, ":", e$message))
      return(NULL)
    }
  )
  if (is.null(evaluation_metrics)) next

  results_list_arg[[cfg$name]] <- evaluation_metrics
  cat("Evaluación ARG completada para:", cfg$name, "\n\n")
}

# Consolidar y mostrar resultados de Argentina
if (length(results_list_arg) > 0) {
  final_results_table_arg <- do.call(cbind, results_list_arg)
  print("--- Tabla Evaluación ARGENTINA (Niveles 2023) ---")
  print(round(final_results_table_arg, 4))
} else {
  print("No se generaron resultados para Argentina.")
}

# -

# ## 5.2 Comparacion y evaluacion de modelos chilenos
#

# + vscode={"languageId": "r"}
# -----------------------------------------------------------------
# Evaluación de Pronósticos CHILE en Niveles (2023)
library(forecast)
library(lubridate)

if (!exists("Eval_Pron")) stop("Función Eval_Pron no encontrada.")

results_list_chi <- list()
if (!exists("h_eval")) h_eval <- 12
if (!exists("start_eval")) start_eval <- c(2023, 1)
if (!exists("end_eval")) end_eval <- c(2023, 12)

series_config_chi <- list(
  list(name = "IMACEC Chi", model = model_imacec_chi, original_ts = ts_imacec, needs_xreg_future = FALSE),
  list(name = "IPC Chi", model = model_ipc_chi, original_ts = ts_ipc_chi, needs_xreg_future = TRUE),
  list(name = "M2 Chi", model = model_m2_chi, original_ts = ts_m2_chi, needs_xreg_future = FALSE),
  list(name = "TASA Chi", model = model_tasa_chi, original_ts = ts_tasa_chi, needs_xreg_future = FALSE)
)

for (cfg in series_config_chi) {
  cat("Procesando Evaluación para:", cfg$name, "...\n")
  xreg_future <- NULL
  if (cfg$needs_xreg_future && cfg$name == "IPC Chi") {
    future_ts <- ts(1:h_eval, start = start_eval, frequency = 12)
    future_mes <- factor(cycle(future_ts), levels = 1:12)
    xreg_future <- model.matrix(~future_mes)[, -1]
  }

  fc_diflog <- tryCatch(
    {
      if (cfg$needs_xreg_future) {
        forecast(cfg$model, h = h_eval, xreg = xreg_future)
      } else {
        forecast(cfg$model, h = h_eval)
      }
    },
    error = function(e) {
      warning(e$message)
      return(NULL)
    }
  )
  if (is.null(fc_diflog)) next

  last_obs_2022 <- tail(window(cfg$original_ts, end = c(2022, 12)), 1)
  fc_levels <- retransform_diflog(fc_diflog, last_obs_2022)

  actual_levels <- window(cfg$original_ts, start = start_eval, end = end_eval)
  Y_P <- as.vector(fc_levels)
  Y_A <- as.vector(actual_levels)

  min_len <- min(length(Y_P), length(Y_A))
  if (min_len < h_eval) warning(paste(cfg$name, ": horizonte reducido a", min_len))
  Y_P <- Y_P[1:min_len]
  Y_A <- Y_A[1:min_len]

  eval_metrics <- tryCatch(
    {
      Eval_Pron(Y_P = Y_P, Y_A = Y_A, Nombre = cfg$name)
    },
    error = function(e) {
      warning(e$message)
      return(NULL)
    }
  )
  if (is.null(eval_metrics)) next

  results_list_chi[[cfg$name]] <- eval_metrics
  cat(" -> Evaluación completada.\n")
}

if (length(results_list_chi) > 0) {
  tabla_chi <- do.call(cbind, results_list_chi)
  cat("\n--- Tabla Evaluación CHILE (Niveles 2023) ---\n")
  print(round(tabla_chi, 4))
} else {
  cat("No se generaron resultados de evaluación para Chile.\n")
}

# -

# # 6. Comparación con Suavizado Exponencial (ETS) sin Tendencia

# ## 6.1 Comparacion ETS Argentina

# + vscode={"languageId": "r"}
# --- Ajuste, Pronóstico y Evaluación ARGENTINA con ETS (Sin Tendencia) ---

# 1. Cargar paquetes y funciones necesarias (si no están cargados)
# library(forecast)
# source("eval_pron.R") # Asegurarse que Eval_Pron esté disponible

# 2. Definir períodos y horizonte
end_train_ets <- c(2022, 12)
start_eval_ets <- c(2023, 1)
end_eval_ets <- c(2023, 12)
h_ets <- 12

# 3. Lista para almacenar resultados de evaluación ETS para Argentina
results_list_ets_arg <- list()

# 4. Configuración de las series ARGENTINAS (usando las series originales en NIVELES)
series_config_ets_arg <- list(
  list(name = "EMAE Arg", original_ts = ts_emae),
  list(name = "IPC Arg", original_ts = ts_ipc_arg),
  list(name = "M2 Arg", original_ts = ts_m2_arg),
  list(name = "TASA Arg", original_ts = ts_tasa_arg)
)

# --- Bucle para Ajuste, Pronóstico y Evaluación ETS (Solo Argentina) ---
cat("--- Iniciando Proceso ETS (trend='N') para ARGENTINA ---\n")
for (cfg in series_config_ets_arg) {
  cat("Procesando ETS para:", cfg$name, "\n")

  # Extraer datos de entrenamiento (NIVELES)
  training_ts <- window(cfg$original_ts, end = end_train_ets)

  # Validar si hay datos suficientes
  if (length(training_ts) < 2 * frequency(training_ts)) {
    warning(paste("Datos insuficientes para ETS en", cfg$name, ". Saltando."))
    next
  }

  # Ajustar modelo ETS (Sin Tendencia, Error/Seasonality automáticos)
  ets_model <- tryCatch(
    {
      ets(training_ts, trend = "N", allow.multiplicative.trend = FALSE)
    },
    error = function(e) {
      warning(paste("Error al ajustar ETS para", cfg$name, ":", e$message))
      return(NULL)
    }
  )

  if (is.null(ets_model)) {
    cat("  AJUSTE ETS FALLÓ para", cfg$name, "- No se puede continuar.\n\n")
    next
  }

  cat("  Modelo ETS ajustado:", ets_model$method, "\n")

  # Generar pronóstico ETS
  ets_forecast <- tryCatch(
    {
      forecast(ets_model, h = h_ets)
    },
    error = function(e) {
      warning(paste("Error al pronosticar ETS para", cfg$name, ":", e$message))
      return(NULL)
    }
  )

  if (is.null(ets_forecast)) next

  # Obtener pronóstico medio (Y_P) y valores reales (Y_A)
  Y_P_ets <- as.vector(ets_forecast$mean)
  actual_levels_eval <- window(cfg$original_ts, start = start_eval_ets, end = end_eval_ets)
  Y_A_ets <- as.vector(actual_levels_eval)

  # Validar longitudes
  if (length(Y_A_ets) == 0) {
    warning(paste("No hay datos reales 2023 para", cfg$name))
    next
  }
  min_len_ets <- min(length(Y_P_ets), length(Y_A_ets))
  if (min_len_ets < h_ets) {
    warning(paste("Datos incompletos ETS en", cfg$name))
  }
  Y_P_ets <- Y_P_ets[1:min_len_ets]
  Y_A_ets <- Y_A_ets[1:min_len_ets]

  # Evaluar usando Eval_Pron
  evaluation_metrics_ets <- tryCatch(
    {
      Eval_Pron(Y_P = Y_P_ets, Y_A = Y_A_ets, Nombre = paste(cfg$name, "(ETS)"))
    },
    error = function(e) {
      warning(paste("Error en Eval_Pron para ETS", cfg$name, ":", e$message))
      return(NULL)
    }
  )

  if (is.null(evaluation_metrics_ets)) next

  # Guardar resultados
  results_list_ets_arg[[cfg$name]] <- evaluation_metrics_ets

  cat("  Evaluación ETS completada para:", cfg$name, "\n\n")
}
cat("--- Proceso ETS para ARGENTINA Finalizado ---\n\n")

# 5. Consolidar y mostrar resultados ETS de Argentina
if (length(results_list_ets_arg) > 0) {
  final_results_table_ets_arg <- do.call(cbind, results_list_ets_arg)
  print("--- Tabla Resumen Evaluación Pronósticos ETS ARGENTINA (trend='N') (Niveles 2023) ---")
  # Renombrar columna si solo queda una
  if (ncol(final_results_table_ets_arg) == 1 && length(results_list_ets_arg) == 1) {
    colnames(final_results_table_ets_arg) <- names(results_list_ets_arg)
  }
  print(round(final_results_table_ets_arg, 4))
} else {
  print("No se generaron resultados de evaluación ETS válidos para Argentina.")
}

