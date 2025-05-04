# -*- coding: utf-8 -*-
# ---
# jupyter:
#   jupytext:
#     text_representation:
#       extension: .R
#       format_name: percent
#       format_version: '1.3'
#       jupytext_version: 1.17.0
#   kernelspec:
#     display_name: R (System)
#     language: R
#     name: r_system
# ---

# %% [markdown]
# # TP1: Metodología Box-Jenkins y Suavizado Exponencial ETS

# %% [markdown]
# # 1. Configuración inicial y carga de datos

# %% [markdown]
# ### 1.1 Instalación y carga de librerías

# %% vscode={"languageId": "r"}
# Instalar paquetes si no están disponibles
# if(!require(readxl)) install.packages("readxl")
# if(!require(forecast)) install.packages("forecast")
# if(!require(tseries)) install.packages("tseries")
# if(!require(ggplot2)) install.packages("ggplot2")
# if(!require(dplyr)) install.packages("dplyr") # Incluido en tidyverse
# if(!require(zoo)) install.packages("zoo")
# if(!require(lubridate)) install.packages("lubridate")
# if(!require(officer)) install.packages("officer")
# if(!require(flextable)) install.packages("flextable")
# if(!require(tidyverse)) install.packages("tidyverse")

# Cargar librerías necesarias
library(readxl)
library(forecast)
library(tseries)
library(zoo)
library(lubridate)
library(officer)
library(flextable)
library(tidyverse) # Incluye ggplot2, dplyr, etc.


# Definir ruta del proyecto (Asegúrate que esta ruta sea correcta para tu entorno)
project_path <- "C:/Users/trico/OneDrive/UBA/Series de tiempo/TP1" # Mantén tu ruta o ajústala si es necesario

# 2.1) Cargar función de diagnóstico de residuos
# Asegúrate que 'corr_res.R' esté en la ruta correcta o en 'project_path'
source(file.path(project_path, "corr_res.R"))
# Cargar funciones auxiliares (incluida save_corr_table)
# Cargar funciones auxiliares (incluida save_corr_table)
source(file.path(project_path, "save_utils.R"))
source(file.path(project_path, "plot_forecast_comparison.R"))
source(file.path(project_path, "eval_pron.R"))

# Configurar opciones
options(scipen = 999) # Evitar notación científica
par(bg = "white") # Fondo blanco para gráficos base

# %% [markdown]
# ### 1.2 Lectura de datos

# %% vscode={"languageId": "r"}
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


# %% [markdown]
# # 2. Preparación de series temporales
#
#

# %% [markdown]
# ### 2.1 Creación de series temporales y diferencias logarítmicas

# %% vscode={"languageId": "r"}
# Crear columna de Fecha para Argentina
datos_arg$Fecha <- as.Date(as.yearmon(gsub("M", "-", datos_arg$Periodo), "%Y-%m"))

# Crear columna de Fecha para Chile (corregido para formato YYYYMM)
datos_chi$Fecha <- as.Date(as.yearmon(gsub("M", "-", datos_chi$Periodo), "%Y-%m"))

# Crear objetos ts (time series) a partir de enero-2004
ts_emae     <- ts(datos_arg$EMAE, start = c(2004, 1), frequency = 12)
ts_ipc_arg  <- ts(datos_arg$IPC,  start = c(2004, 1), frequency = 12)
ts_m2_arg   <- ts(datos_arg$M2N,  start = c(2004, 1), frequency = 12)
ts_tasa_arg <- ts(datos_arg$TASA, start = c(2004, 1), frequency = 12)

ts_imacec   <- ts(datos_chi$IMACEC, start = c(2004, 1), frequency = 12)
ts_ipc_chi  <- ts(datos_chi$IPC,    start = c(2004, 1), frequency = 12)
ts_m2_chi   <- ts(datos_chi$M2N,    start = c(2004, 1), frequency = 12)
ts_tasa_chi <- ts(datos_chi$TASA,   start = c(2004, 1), frequency = 12)

# (Opcional) Verificar las fechas creadas
# head(datos_arg$Fecha)
# head(datos_chi$Fecha)

# %% vscode={"languageId": "r"}
# 2.1 Creación de series y diferencias logarítmicas

# Función para calcular la diferencia logarítmica
# (aproxima la tasa de crecimiento)
calcular_diff_log <- function(x) {
  diff(log(x))
}

# Lista con las series temporales originales en niveles
series <- list(
  emae     = ts_emae,
  ipc_arg  = ts_ipc_arg,
  m2_arg   = ts_m2_arg,
  tasa_arg = ts_tasa_arg,
  imacec   = ts_imacec,
  ipc_chi  = ts_ipc_chi,
  m2_chi   = ts_m2_chi,
  tasa_chi = ts_tasa_chi
)

# Bucle para crear las series con diferencias logarítmicas (dl_...)
# Nota: Se usa assign() para crear dinámicamente los nombres de las variables.
# Se ajusta la fecha de inicio porque diff() reduce la longitud en 1.
for(i in names(series)) {
  original_series <- series[[i]]
  transformed_series <- calcular_diff_log(original_series)
  # Convertir de nuevo a ts especificando inicio y frecuencia
  dl_series_ts <- ts(
    transformed_series,
    start     = time(original_series)[2], # El inicio es el segundo período del original
    # O alternativamente, si prefieres la notación año/mes: start(original_series)[1:2] + c(0, 1)
    frequency = frequency(original_series)
  )
  assign(paste0("dl_", i), dl_series_ts) # Crea ej: dl_emae, dl_ipc_arg, etc.
}

# Lista con los nombres de las series diferenciadas que se usarán para entrenamiento
train_series_names <- names(series) # Usamos los mismos nombres base

# Bucle para crear las ventanas de entrenamiento (..._train)
# Período de entrenamiento: hasta diciembre 2022
for (i in train_series_names) {
  # Accedemos a la variable dl_ creada en el bucle anterior usando get()
  full_dl_series <- get(paste0("dl_", i))
  # Creamos la ventana de entrenamiento
  train_window <- window(full_dl_series, end = c(2022, 12))
  # Asignamos a la nueva variable _train
  assign(paste0("dl_", i, "_train"), train_window) # Crea ej: dl_emae_train, etc.
}

# (Opcional) Verificar una de las series creadas y su versión de entrenamiento
# print(head(dl_emae))
# print(head(dl_emae_train))
# print(frequency(dl_emae))

# %% [markdown]
# # 3. Estimacion de modelos

# %% [markdown]
# ## Argentina

# %% [markdown]
# ### 3.1 Estimacion EMAE - Arg

# %% vscode={"languageId": "r"}
# 3.0 Estimación de modelos
## Argentina:
### 3.1 Estimación del EMAE para Argentina

# Definir órdenes del modelo ARIMA estacional
p_emae <- 4; d_emae <- 0; q_emae <- 0
P_emae <- 0; D_emae <- 1; Q_emae <- 1

# Estimar el modelo ARIMA
model_emae_arg <- forecast::Arima(
  dl_emae_train,
  order = c(p_emae, d_emae, q_emae),
  seasonal = list(order = c(P_emae, D_emae, Q_emae), period = 12),
  method = "ML"
)

# Calcular diagnóstico de residuos
corr1 <- corr_res(
  model_emae_arg,
  lags = 26,
  p_q = p_emae + q_emae + P_emae + Q_emae
)

# Mostrar tabla de diagnóstico de residuos en la notebook
print(corr1)

# Guardar la tabla de diagnóstico en un archivo .docx usando la nueva función
# Los archivos se guardarán en la subcarpeta 'output_tables' dentro de 'project_path'
save_corr_table(corr_result = corr1,
                filename_base = "emae_arg", # Nombre base del archivo
                output_dir = project_path)  # Directorio del proyecto

# Mostrar resumen del modelo ajustado
summary(model_emae_arg)

# %% [markdown]
# ### 3.2 Estimacion IPC - Arg

# %% vscode={"languageId": "r"}
### 3.2 IPC Argentina

# Definir órdenes del modelo ARIMA estacional
p_ipc <- 3; d_ipc <- 0; q_ipc <- 0
P_ipc <- 0; D_ipc <- 1; Q_ipc <- 1

# Estimar el modelo ARIMA
model_ipc_arg <- forecast::Arima( # Usar namespace explícito
  dl_ipc_arg_train,
  order = c(p_ipc, d_ipc, q_ipc),
  seasonal = list(order = c(P_ipc, D_ipc, Q_ipc), period = 12),
  method = "ML"
)

# Calcular diagnóstico de residuos
corr2 <- corr_res(
  model_ipc_arg,
  lags = 26,
  p_q = p_ipc + q_ipc + P_ipc + Q_ipc # Grados de libertad
)

# Mostrar tabla de diagnóstico de residuos en la notebook
print(corr2)

# Guardar la tabla de diagnóstico en un archivo .docx
save_corr_table(corr_result = corr2,
                filename_base = "ipc_arg", # Nombre base del archivo
                output_dir = project_path)

# Mostrar resumen del modelo ajustado
summary(model_ipc_arg)

# %% [markdown]
# ### 3.3 Estimacion Agregado Monetario M2 - Arg

# %% vscode={"languageId": "r"}
### 3.3 Estimación Agregado Monetario M2, Argentina

# Definir órdenes del modelo ARIMA estacional
p_m2 <- 1; d_m2 <- 0; q_m2 <- 4 # Órdenes no estacionales
P_m2 <- 0; D_m2 <- 1; Q_m2 <- 1 # Órdenes estacionales

# Estimar el modelo ARIMA
model_m2_arg <- forecast::Arima(
  dl_m2_arg_train,
  order    = c(p_m2, d_m2, q_m2),
  seasonal = list(order = c(P_m2, D_m2, Q_m2), period = 12),
  method   = "ML"
)

# Calcular diagnóstico de residuos
corr3 <- corr_res(
  model_m2_arg,
  lags = 26,
  p_q  = p_m2 + q_m2 + P_m2 + Q_m2 # Grados de libertad (1+4+0+1 = 6)
)

# Mostrar tabla de diagnóstico de residuos en la notebook
print(corr3)

# Guardar la tabla de diagnóstico en un archivo .docx
save_corr_table(corr_result = corr3,
                filename_base = "m2_arg", # Nombre base del archivo
                output_dir = project_path)

# Mostrar resumen del modelo ajustado
summary(model_m2_arg)

# %% [markdown]
# ### 3.4 Estimacion Tasa de interes - Arg

# %% vscode={"languageId": "r"}
### 3.4 Estimación de Tasas de interés, Argentina

# Especificar órdenes del modelo ARIMA estacional
p_tasa <- 4; d_tasa <- 0; q_tasa <- 0
P_tasa <- 0; D_tasa <- 1; Q_tasa <- 1

# Estimar el modelo ARIMA
model_tasa_arg <- forecast::Arima( # Usar namespace explícito
  dl_tasa_arg_train,
  order = c(p_tasa, d_tasa, q_tasa),
  seasonal = list(order = c(P_tasa, D_tasa, Q_tasa), period = 12),
  method = "ML"
)

# Calcular diagnóstico de residuos
corr4 <- corr_res(
  model_tasa_arg,
  lags = 26,
  p_q = p_tasa + q_tasa + P_tasa + Q_tasa # Grados de libertad
)

# Mostrar tabla de diagnóstico de residuos en la notebook
print(corr4)

# Guardar la tabla de diagnóstico en un archivo .docx
save_corr_table(corr_result = corr4,
                filename_base = "tasa_arg", # Nombre base del archivo
                output_dir = project_path)

# Mostrar resumen del modelo ajustado
summary(model_tasa_arg)

# %% [markdown]
# ## Chile

# %% [markdown]
# ### 3.5 Estimacion IMACEC - Chile

# %% vscode={"languageId": "r"}
## Chile:
### 3.5 Estimación IMACEC-Chile

# Definir órdenes del modelo ARIMA estacional
p_imacec <- 4; d_imacec <- 0; q_imacec <- 2
P_imacec <- 0; D_imacec <- 1; Q_imacec <- 1

# Estimar el modelo ARIMA
model_imacec_chi <- forecast::Arima( # Usar namespace explícito
  y = dl_imacec_train,
  order = c(p_imacec, d_imacec, q_imacec),
  seasonal = list(order = c(P_imacec, D_imacec, Q_imacec), period = 12),
  include.drift = FALSE, # Específico de este modelo
  include.mean = TRUE,   # Específico de este modelo
  method = "ML"
)

# Calcular grados de libertad para Ljung-Box (considerando drift si existe)
num_arma_params <- sum(model_imacec_chi$arma[c(1, 2, 3, 4)]) # p + q + P + Q
drift_present <- as.integer("drift" %in% names(model_imacec_chi$coef))
grados_libertad_q <- num_arma_params + drift_present

# Imprimir cómo se calcularon los grados de libertad (opcional, como en el script)
cat(
  "Calculando p_q para Ljung-Box como:", num_arma_params, "(arma) +",
  drift_present, "(drift/mean) =", grados_libertad_q, "\n"
)

# Calcular diagnóstico de residuos usando los grados de libertad calculados
# Nota: El script usa lags=35 aquí, mantenemos eso.
corr1c <- corr_res(
  model_imacec_chi, # El script usa 'xreg = model_imacec_chi', pero pasar el modelo directamente es más estándar
  lags = 35,
  p_q  = grados_libertad_q
)

# Mostrar tabla de diagnóstico de residuos en la notebook
print(corr1c)

# Guardar la tabla de diagnóstico en un archivo .docx
save_corr_table(corr_result = corr1c,
                filename_base = "imacec_chi", # Nombre base del archivo
                output_dir = project_path)

# Mostrar resumen del modelo ajustado
summary(model_imacec_chi)

# %% [markdown]
# ### 3.6 Estimacion IPC - Chile

# %% vscode={"languageId": "r"}
### 3.6 IPC Chile

p_ipc_c <- 1; d_ipc_c <- 1; q_ipc_c <- 6
P_ipc_c <- 1; D_ipc_c <- 0; Q_ipc_c <- 1

model_ipc_chi <- forecast::Arima( # Usar namespace explícito
  dl_ipc_chi_train,
  order = c(p_ipc_c, d_ipc_c, q_ipc_c),           # --> c(1, 1, 6)
  seasonal = list(order = c(P_ipc_c, D_ipc_c, Q_ipc_c), period = 12),  # --> c(1, 0, 1)
  include.mean = TRUE,
  method = "ML"
)

p_q_ipc_c <- 1 + 1 + 1 + 1  # AR + MA + SAR + SMA --> Resultado = 4

# Calcular diagnóstico de residuos usando p_q = 4
corr2c <- corr_res(
    model_ipc_chi,
    lags = 26,
    p_q = p_q_ipc_c # Usando p_q = 4
)

print(corr2c)

# Guardar la tabla de diagnóstico en un archivo .docx
save_corr_table(corr_result = corr2c,
                filename_base = "ipc_chi", # Nombre base del archivo
                output_dir = project_path)

summary(model_ipc_chi)

# %% [markdown]
# ### 3.7 Estimacion Agregado Monetario M2 - Chile

# %% vscode={"languageId": "r"}
### 3.7 M2, Chile (Usando auto.arima)

# Estimar el modelo usando auto.arima para búsqueda automática
# Los parámetros p,d,q,P,D,Q serán determinados por la función.
model_m2_chi <- forecast::auto.arima( # Usar namespace explícito
  dl_m2_chi_train,
  seasonal = TRUE,      # Permitir búsqueda estacional (P, Q, D)
  stepwise = TRUE,      # Búsqueda por pasos (más rápida)
  approximation = TRUE, # Usar aproximaciones (más rápido)
  # Se usan los parámetros exactos del script original
)

# Calcular grados de libertad para Ljung-Box basados en el modelo encontrado por auto.arima
# p_q = p + q + P + Q (extraídos del objeto 'arma' del modelo ajustado)
p_q_m2_c <- sum(model_m2_chi$arma[c(1, 2, 3, 4)])

# Calcular diagnóstico de residuos
corr3c <- corr_res(
  model_m2_chi,
  lags = 26,
  p_q = p_q_m2_c # Usando la suma de parámetros del modelo encontrado
)

# Mostrar tabla de diagnóstico de residuos en la notebook
print(corr3c)

# Guardar la tabla de diagnóstico en un archivo .docx
save_corr_table(corr_result = corr3c,
                filename_base = "m2_chi", # Nombre base del archivo
                output_dir = project_path)

# Mostrar resumen del modelo ajustado por auto.arima
summary(model_m2_chi)

# %% [markdown]
# ### 3.8 Estimacion Tasa de interes - Chile

# %% vscode={"languageId": "r"}
### 3.8 Tasa de interés, Chile

# Especificar órdenes del modelo ARIMA estacional
p_tasa_chi <- 4; d_tasa_chi <- 0; q_tasa_chi <- 0
P_tasa_chi <- 0; D_tasa_chi <- 1; Q_tasa_chi <- 1

# Estimar el modelo ARIMA
model_tasa_chi <- forecast::Arima( # Usar namespace explícito
  dl_tasa_chi_train,
  order = c(p_tasa_chi, d_tasa_chi, q_tasa_chi),
  seasonal = list(order = c(P_tasa_chi, D_tasa_chi, Q_tasa_chi), period = 12),
  method = "ML" # Usar Máxima Verosimilitud
)

# Calcular grados de libertad para Ljung-Box
p_q_tasa_c <- p_tasa_chi + q_tasa_chi + P_tasa_chi + Q_tasa_chi # p+q+P+Q

# Calcular diagnóstico de residuos
corr4c <- corr_res(
  model_tasa_chi,
  lags = 26,
  p_q = p_q_tasa_c # Usando los grados de libertad calculados
)

# Mostrar tabla de diagnóstico de residuos en la notebook
print(corr4c)

# Guardar la tabla de diagnóstico en un archivo .docx
save_corr_table(corr_result = corr4c,
                filename_base = "tasa_chi", # Nombre base del archivo
                output_dir = project_path)

# Mostrar resumen del modelo ajustado
summary(model_tasa_chi)

# %% [markdown]
# # 4 Pronosticos de Diferencias Logaritmicas

# %% [markdown]
# ## Argentina

# %% [markdown]
# ### 4.1 Proyeccion EMAE - Argentina

# %% vscode={"languageId": "r"}
# 4.0 Pronósticos
## 4.1: Proyección del EMAE, Argentina

# a) Definir parámetros del gráfico
h_forecast_emae <- 12
model_to_plot_emae <- model_emae_arg
original_ts_emae <- dl_emae
title_emae <- "Dif. Log. EMAE Argentina"
start_plot_emae <- 2022
ylabel_emae <- "Diferencia Logarítmica"

# b) Llamar a la función para generar el objeto ggplot
# (Asegúrate que la función plot_forecast_vs_actual esté definida/cargada previamente)
plot_emae_comparison <- plot_forecast_vs_actual(
  model = model_to_plot_emae,
  original_ts = original_ts_emae,
  h = h_forecast_emae,
  series_title = title_emae,
  start_year_plot = start_plot_emae,
  ylab_text = ylabel_emae,
  show_pi = FALSE
)

# c) Mostrar el gráfico generado (usando el método implícito de R, como en el script)
plot_emae_comparison

# %% [markdown]
# ### 4.2 Proyeccion IPC - Argentina

# %% vscode={"languageId": "r"}
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
plot_ipc_comparison


# %% [markdown]
# ### 4.3 Proyeccion Agregado Monetario M2 - Argentina

# %% vscode={"languageId": "r"}
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
plot_m2_comparison


# %% [markdown]
# ### 4.4 Proyeccion Tasa de interes - Argentina

# %% vscode={"languageId": "r"}
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
plot_tasa_comparison

# %% [markdown]
# ## Chile

# %% [markdown]
# ### 4.5 Proyeccion IMACEC - Chile

# %% vscode={"languageId": "r"}
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
plot_imacec_comparison


# %% [markdown]
# ### 4.6 Proyeccion IPC - Chile

# %% vscode={"languageId": "r"}
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
plot_ipc_chi_comparison

# %% [markdown]
# ### 4.6 Proyeccion Agregado Monetario M2 - Chile

# %% vscode={"languageId": "r"}
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
plot_m2_chi_comparison



# %% [markdown]
# ### 4.7 Proyeccion Diferencia Logaritmica de Tasa de Interes - Chile

# %% vscode={"languageId": "r"}
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
plot_tasa_chi_comparison



# %% [markdown]
# # 5 - Comparacion y evaluacion de proyecciones

# %% vscode={"languageId": "r"}
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


# %% [markdown]
# ## 5.1 Comparacion y evaluacion de modelos argentinos

# %% vscode={"languageId": "r"}
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


# %% [markdown]
# ## 5.2 Comparacion y evaluacion de modelos chilenos
#

# %% vscode={"languageId": "r"}
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


# %% [markdown]
# # 6. Comparación con Suavizado Exponencial (ETS) sin Tendencia

# %% [markdown]
# ## 6.1 Comparacion ETS Argentina

# %% vscode={"languageId": "r"}
### 5.1 EMAE:####
model_ets_emae <- forecast::ets(dl_emae_train, model = "ZNZ")

# Resumen del modelo ETS
summary(model_ets_emae)

# Pronóstico h=12
forecast_ets_emae <- forecast::forecast(model_ets_emae, h = 12)

# Gráfico: Pronóstico ETS vs Serie Observada (dl_emae completa)
autoplot(forecast_ets_emae) +
  autolayer(dl_emae, series = "Observado") + # Usa la serie dl_ completa
  ggtitle("ETS (ZNZ) sin tendencia: EMAE Argentina") +
  xlab("Tiempo (meses)") +
  ylab("Diferencia logarítmica") +
  theme_bw()





# %% vscode={"languageId": "r"}
### 5.2 IPC:####
# Estimar modelo ETS (ZNZ)
ets_ipc_arg <- forecast::ets(dl_ipc_arg_train, model = "ZNZ") # Variable renombrada como en script

# Resumen
summary(ets_ipc_arg)

# Pronóstico h=12
forecast_ets_ipc_arg <- forecast::forecast(ets_ipc_arg, h = 12)

# Gráfico: Pronóstico ETS vs Serie Observada (dl_ipc_arg completa)
autoplot(forecast_ets_ipc_arg) +
  autolayer(dl_ipc_arg, series = "Observado") + # Usa la serie dl_ completa
  # Título y etiquetas como en el script (incluyendo la nota humorística)
  ggtitle("ETS sin tendencia - IPC Argentina") +
  xlab("Tiempo") + ylab("Dif. log IPC") +
  theme_minimal() # Tema como en el script


# %% vscode={"languageId": "r"}
### 5.3 M2:####
# Estimar modelo ETS (ZNZ)
ets_m2_arg <- forecast::ets(dl_m2_arg_train, model = "ZNZ") # Variable renombrada como en script

# Resumen
summary(ets_m2_arg)

# Pronóstico h=12
forecast_ets_m2_arg <- forecast::forecast(ets_m2_arg, h = 12)

# Gráfico: Pronóstico ETS vs Serie Observada (dl_m2_arg completa)
autoplot(forecast_ets_m2_arg) +
  autolayer(dl_m2_arg, series = "Observado") + # Usa la serie dl_ completa
  ggtitle("ETS sin tendencia - M2 Argentina") +
  xlab("Tiempo") + ylab("Dif. log M2") +
  theme_minimal() # Tema como en el script


# %% vscode={"languageId": "r"}
### 5.4 Tasa de interés:####
# Estimar modelo ETS (ZNZ)
ets_tasa_arg <- forecast::ets(dl_tasa_arg_train, model = "ZNZ") # Variable renombrada como en script

# Resumen
summary(ets_tasa_arg)

# Pronóstico h=12
forecast_ets_tasa_arg <- forecast::forecast(ets_tasa_arg, h = 12)

# Gráfico: Pronóstico ETS vs Serie Observada (dl_tasa_arg completa)
autoplot(forecast_ets_tasa_arg) +
  autolayer(dl_tasa_arg, series = "Observado") + # Usa la serie dl_ completa
  ggtitle("ETS sin tendencia - Tasa Argentina") +
  xlab("Tiempo") + ylab("Dif. log Tasa") +
  theme_minimal() # Tema como en el script

# %% [markdown]
# ## 6.1 Comparacion ETS Chile

# %% vscode={"languageId": "r"}
### 5.5 IMACEC:####
# Estimar modelo ETS (ZNZ)
ets_imacec <- forecast::ets(dl_imacec_train, model = "ZNZ") # Variable renombrada como en script

# Resumen
summary(ets_imacec)

# Pronóstico h=12
forecast_ets_imacec <- forecast::forecast(ets_imacec, h = 12)

# Gráfico: Pronóstico ETS vs Serie Observada (dl_imacec completa)
autoplot(forecast_ets_imacec) +
  autolayer(dl_imacec, series = "Observado") + # Usa la serie dl_ completa
  ggtitle("ETS sin tendencia - IMACEC Chile") +
  xlab("Tiempo") + ylab("Dif. log IMACEC") +
  theme_minimal() # Tema como en el script

# %% vscode={"languageId": "r"}
### 5.6 IPC####
# Estimar modelo ETS (ZNZ)
ets_ipc_chi <- forecast::ets(dl_ipc_chi_train, model = "ZNZ") # Variable renombrada como en script

# Resumen
summary(ets_ipc_chi)

# Pronóstico h=12
forecast_ets_ipc_chi <- forecast::forecast(ets_ipc_chi, h = 12)

# Gráfico: Pronóstico ETS vs Serie Observada (dl_ipc_chi completa)
autoplot(forecast_ets_ipc_chi) +
  autolayer(dl_ipc_chi, series = "Observado") + # Usa la serie dl_ completa
  ggtitle("ETS sin tendencia - IPC Chile") +
  xlab("Tiempo") + ylab("Dif. log IPC") +
  theme_minimal() # Tema como en el script


# %% vscode={"languageId": "r"}
### 5.7 M2####
# Estimar modelo ETS (ZNZ)
ets_m2_chi <- forecast::ets(dl_m2_chi_train, model = "ZNZ") # Variable renombrada como en script

# Resumen
summary(ets_m2_chi)

# Pronóstico h=12
forecast_ets_m2_chi <- forecast::forecast(ets_m2_chi, h = 12)

# Gráfico: Pronóstico ETS vs Serie Observada (dl_m2_chi completa)
autoplot(forecast_ets_m2_chi) +
  autolayer(dl_m2_chi, series = "Observado") + # Usa la serie dl_ completa
  ggtitle("ETS sin tendencia - M2 Chile") +
  xlab("Tiempo") + ylab("Dif. log M2") +
  theme_minimal() # Tema como en el script


# %% vscode={"languageId": "r"}
### 5.8 Tasa de interés####
# Estimar modelo ETS (ZNZ)
ets_tasa_chi <- forecast::ets(dl_tasa_chi_train, model = "ZNZ") # Variable renombrada como en script

# Resumen
summary(ets_tasa_chi)

# Pronóstico h=12
forecast_ets_tasa_chi <- forecast::forecast(ets_tasa_chi, h = 12)

# Gráfico: Pronóstico ETS vs Serie Observada (dl_tasa_chi completa)
autoplot(forecast_ets_tasa_chi) +
  autolayer(dl_tasa_chi, series = "Observado") + # Usa la serie dl_ completa
  ggtitle("ETS sin tendencia - Tasa Chile") +
  xlab("Tiempo") + ylab("Dif. log Tasa") +
  theme_minimal() # Tema como en el script

# %% vscode={"languageId": "r"}
