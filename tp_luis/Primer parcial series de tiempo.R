
#1.0 TP1 Series de tiempo:####

comentario<-function(...){
  invisible(NULL)
}

comentario("Instalar aquellas librerías
por si acaso no están disponibles, por eso
           ponemos el if(!require) para que en caso
           de que la misma esté instalada, ignore el comando
           y pase a la siguiente:")

# if(!require(readxl)) install.packages("readxl")
# if(!require(forecast)) install.packages("forecast")
# if(!require(tseries)) install.packages("tseries")
# if(!require(ggplot2)) install.packages("ggplot2")
# if(!require(dplyr)) install.packages("dplyr")
# if(!require(zoo)) install.packages("zoo")
# if(!require(lubridate)) install.packages("lubridate")
# if(!require(officer)) install.packages("officer)
# if(!require(flextable)) install.packages("flextable)
# if(!require(tidyverse)) install.packages("tidyverse) #para jalar dplyr
library(officer)
library(flextable)
library(readxl)
library(forecast)
library(tseries)
library(ggplot2)
library(dplyr)
library(zoo)
library(lubridate)
library(dplyr)


directorio<-"G:/Mi unidad/TP1 series de tiempo"


comentario("A los efectos de comparar el método ARIMA con algún método alternativo, 
           se realizarán pronósticos de las eries de ambos países con el étodo de suavizado
           exponencial ETS, para el mismo periodo indicado enero 2023 a diciembre 2023.
           Para que las metodologías sean comparables, utilizar el m'todo sin tendencia
           (no trend) y con tipo de error y estacionalidad seleccionadas automáticamente
           con el criterio de Akaike. Utilizar como muestra las observaciones del periodo enero
           2004 a diciembre 2022")

source(file.path(directorio, "corr_res.R"))
setwd(directorio)

#Datos de argentina:
datos_arg <- read_excel(
  file.path(directorio, "Datos Argentina.xlsx"),
  skip      = 1,
  col_names = c("Periodo", "EMAE", "IPC", "M2N", "M2R", "TASA")
)

#Datos de Chile 
comentario("Saltamos con un skip=3 el título, una línea en blanco y la cabecera del
           excel.")
datos_chi <- read_excel(
  file.path(directorio, "Datos Chile.xlsx"),
  skip      = 3,
  col_names = c("Periodo", "IMACEC", "M2N", "IPC", "M2R", "TASA")
)

# Mostrar primeras filas para verificar
head(datos_arg, 5)
head(datos_chi, 5)

#2.0 Prepararción de series temporales####
##2.1 Creación de series y diferencias logarítmicas:####

datos_arg$Fecha<-as.Date(as.yearmon(gsub("M", "-", datos_arg$Periodo), "%Y-%m"))
datos_chi$Fecha<-as.Date(as.yearmon(gsub("M", "-", datos_chi$Periodo), origin= "1899-12-30"))

comentario("Creamos una serie de objetos de series temporales (ts)
           con la fecha según el horizonte temporal: 2004 en adelante (enero)")


ts_emae <- ts(datos_arg$EMAE, start = c(2004, 1), frequency = 12)
ts_ipc_arg <- ts(datos_arg$IPC, start = c(2004, 1), frequency = 12)
ts_m2_arg <- ts(datos_arg$M2N, start = c(2004, 1), frequency = 12)
ts_tasa_arg <- ts(datos_arg$TASA, start = c(2004, 1), frequency = 12)

ts_imacec <- ts(datos_chi$IMACEC, start = c(2004, 1), frequency = 12)
ts_ipc_chi <- ts(datos_chi$IPC, start = c(2004, 1), frequency = 12)
ts_m2_chi <- ts(datos_chi$M2N, start = c(2004, 1), frequency = 12)
ts_tasa_chi <- ts(datos_chi$TASA, start = c(2004, 1), frequency = 12)



comentario("En este apartado creamos una función y un loop para crear los 
           objetos ts versión diferenciada y logarítmica:
           a) Creamos una lista con los hombres de cada uno de los ts en nivel.
           
           b) Aplicamos la función a cada elemento de la lista series con un loop y 
           guardamos los resultados con nombres nuevos con la siguiente estructura:
           dl_[nombre original de la serie]")


calcular_diff_log <- function(x) {
  diff(log(x))
}   #Función que aplica diferencias al logaritmo:



series <- list(
  emae = ts_emae,
  ipc_arg = ts_ipc_arg,
  m2_arg = ts_m2_arg,
  tasa_arg = ts_tasa_arg,
  imacec = ts_imacec,
  ipc_chi = ts_ipc_chi,
  m2_chi = ts_m2_chi,
  tasa_chi = ts_tasa_chi
) #Objeto series

comentario("Siempre guardar con nombre=objeto en la 
           lista para que así el loop funcione, ya que lo 
           que itera el bucle es los nombres, cada 
           'name' de la lista series en donde cada i es
           cada name de los elementos de la lista.")


for(i in names(series)) {
  os<-series[[i]]
  serie_transformada<-calcular_diff_log(os)
  serie_ts<-ts(
    serie_transformada,
    start = start(os)[1:2]+ c(0 , 1), #La serie comienza un mes después 
    #por aplicar primeras diferencias 
    frequency = frequency(os)
  )
  assign(paste0("dl_",i), serie_ts)
}  #Loop para crear las variables:


##2.2 Creación de ventana de entrenamiento para pronósticos de los modelos:####

train <- list(
  dl_emae = dl_emae,
  dl_ipc_arg = dl_ipc_arg,
  dl_m2_arg = dl_m2_arg,
  dl_tasa_arg = dl_tasa_arg,
  dl_imacec = dl_imacec,
  dl_ipc_chi = dl_ipc_chi,
  dl_m2_chi = dl_m2_chi,
  dl_tasa_chi = dl_tasa_chi
)  #objeto con los nombres


for (i in names(train)){
  ds<-train[[i]]
  serie_train<-window(
    ds, end=c(2022, 12))
  assign(paste0(i, "_train"), serie_train)
} #loop para crear objetos de ts para entrenamiento.


#3.0 Estimación de modelos:####

##Argentina:####

##3.1 Estimación del EMAE para Argentina:####
p_emae <- 4
d_emae <- 0
q_emae <- 0
P_emae <- 0
D_emae <- 1
Q_emae <- 1

model_emae_arg<-forecast::Arima(
  dl_emae_train,
  order=c(p_emae, d_emae, q_emae),
  seasonal=list(order=c(P_emae,D_emae,Q_emae), period=12),
  method="ML"
)

corr1<-corr_res(model_emae_arg,
         lags=26,
         p_q=  p_emae + q_emae + P_emae + Q_emae)

corr1
corr1e<-as.data.frame(corr1)



n<-c("Rezagos", "Estadístico Q", "p-valor")
colnames(corr1)<-n

doc<-read_docx() %>%
  body_add_flextable(value=qflextable(corr1e)) %>% 
  body_add_par("Nota: Elaboración propia por los autores.")
print(doc, target="emae_corr_arg.docx")

summary(model_emae_arg)

## 3.2 IPC Argentina:####

# Volviendo al modelo básico pero con mayor complejidad AR
p_ipc <- 3
d_ipc <- 0
q_ipc <- 0
P_ipc <- 0
D_ipc <- 1
Q_ipc <- 1

model_ipc_arg<-forecast::Arima(
  dl_ipc_arg_train, 
  order=c(p_ipc, d_ipc, q_ipc),
  seasonal=list(order=c(P_ipc, D_ipc, Q_ipc),period=12),
  method="ML"
)

corr2<-corr_res(model_ipc_arg,
                lags=26,
                p_q= p_ipc + q_ipc + P_ipc + Q_ipc)

corr2e<-as.data.frame(corr2)




colnames(corr2)<-n

doc<-read_docx() %>%
  body_add_flextable(value=qflextable(corr2e)) %>% 
  body_add_par("Nota: Elaboración propia por los autores.")
print(doc, target="ipc_corr_arg.docx")

summary(model_ipc_arg)

## 3.3 Estimacion Agregado Monetario M2, Argentina:####

model_m2_arg <- forecast::Arima(
  dl_m2_arg_train,
  order    = c(1, 0, 4), # MA(4) añadido
  seasonal = list(order = c(0, 1, 1), period = 12),
  method   = "ML",
)



corr3<-corr_res(model_m2_arg,
         lags = 26,
         p_q  = 1 + 4 + 0 + 1
) # 1 AR + 4 MA + 1 SMA






colnames(corr3)<-n

corr3e<-as.data.frame(corr3)

doc<-read_docx() %>%
  body_add_flextable(value=qflextable(corr3e)) %>% 
  body_add_par("Nota: Elaboración propia por los autores.")
print(doc, target="m2_corr_arg.docx")


summary(model_m2_arg)


## 3.3 Estimación de Tasas de interés, Argentina####

p_tasa <- 4
d_tasa <- 0
q_tasa <- 0
P_tasa <- 0
D_tasa <- 1
Q_tasa <- 1


model_tasa_arg <- forecast::Arima(
  dl_tasa_arg_train,
  order = c(p_tasa, d_tasa, q_tasa),
  seasonal = list(order = c(P_tasa, D_tasa, Q_tasa), period = 12),
  method = "ML" #
)


corr4<-corr_res(
  model_tasa_arg,
  lags = 26,
  p_q = p_tasa + q_tasa + P_tasa + Q_tasa
)

colnames(corr4)<-n

corr4e<-as.data.frame(corr4)

doc<-read_docx() %>%
  body_add_flextable(value=qflextable(corr4e)) %>% 
  body_add_par("Nota: Elaboración propia por los autores.")
print(doc, target="tasa_corr_arg.docx")


summary(model_tasa_arg)

##Chile:####
## 3.5 Estimación IMACEC-Chile####

model_imacec_chi <- forecast::Arima(
  y = dl_imacec_train,
  order = c(4, 0, 2),
  seasonal = list(order = c(0, 1, 1), period = 12),
  include.drift = FALSE,
  include.mean = TRUE,
  method = "ML"
)

summary(model_imacec_chi)

comentario("Ahora calcularemos el número de parámetros para el test
           de Ljung-Box para estimar si existe autocorrelación.")

num_arma_params <- sum(model_imacec_chi$arma[c(1, 2, 3, 4)]) # p + q + P + Q
drift_present <- as.integer("drift" %in% names(model_imacec_chi$coef))
grados_libertad_q <- num_arma_params + drift_present

cat(
  "Calculando p_q como:", num_arma_params, "(arma) +", drift_present, "(drift) =",
  grados_libertad_q, "\n"
)

corr1c<-corr_res(
  xreg = model_imacec_chi,
  lags = 35,
  p_q  = grados_libertad_q
)


colnames(corr1c)<-n

corr1cc<-as.data.frame(corr1c)

doc<-read_docx() %>%
  body_add_flextable(value=qflextable(corr1cc)) %>% 
  body_add_par("Nota: Elaboración propia por los autores.")
print(doc, target="imacec_corr_chi.docx")

## 3.6 IPC Chile:####

model_ipc_chi <- forecast::Arima(
  dl_ipc_chi_train,
  order = c(1, 1, 6),           # Simplificar componente ARMA
  seasonal = list(order = c(1, 0, 1), period = 12),  # Agregar componente estacional
  include.mean = TRUE,          # Mantener intercepto
  method = "ML"
)


summary(model_ipc_chi)

p_q <- 1 + 1 + 1 + 1  # AR + MA + SAR + SMA
corr2c<-corr_res(model_ipc_chi, lags = 26, p_q = p_q)


colnames(corr2c)<-n

corr2cc<-as.data.frame(corr2c)

doc<-read_docx() %>%
  body_add_flextable(value=qflextable(corr2cc)) %>% 
  body_add_par("Nota: Elaboración propia por los autores.")
print(doc, target="ipc_corr_chi.docx")

## 3.7 M2, Chile:####

model_m2_chi <- forecast::auto.arima(
  dl_m2_chi_train,
  seasonal = TRUE, # Permitir búsqueda estacional (P, Q, D)
  stepwise = TRUE, # Búsqueda por pasos (más rápida)
  approximation = TRUE # Usar aproximaciones (más rápido)
)

summary(model_m2_chi)

corr3c<-corr_res(
  model_m2_chi,
  lags = 26,
  p_q = sum(model_m2_chi$arma[c(1, 2, 3, 4)])
)

colnames(corr3c)<-n

corr3cc<-as.data.frame(corr3c)

doc<-read_docx() %>%
  body_add_flextable(value=qflextable(corr3cc)) %>% 
  body_add_par("Nota: Elaboración propia por los autores.")
print(doc, target="m2_corr_chi.docx")
## 3.8 Tasa de interés, Chile:####

p_tasa_chi <- 4
d_tasa_chi <- 0
q_tasa_chi <- 0
P_tasa_chi <- 0
D_tasa_chi <- 1
Q_tasa_chi <- 1

model_tasa_chi <- forecast::Arima( # Sobrescribe la variable original
  dl_tasa_chi_train,
  order = c(p_tasa_chi, d_tasa_chi, q_tasa_chi),
  seasonal = list(order = c(P_tasa_chi, D_tasa_chi, Q_tasa_chi), period = 12),
  method = "ML" # Usar Máxima Verosimilitud (o CSS-ML si ML falla)
)

summary(model_tasa_chi)

corr4c<-corr_res(
  model_tasa_chi,
  lags = 26,
  p_q = p_tasa_chi + q_tasa_chi + P_tasa_chi + Q_tasa_chi
)

colnames(corr4c)<-n

corr4cc<-as.data.frame(corr4c)

doc<-read_docx() %>%
  body_add_flextable(value=qflextable(corr4cc)) %>% 
  body_add_par("Nota: Elaboración propia por los autores.")
print(doc, target="tasa_corr_chi.docx")

#4.0 Pronósticos####
comentario("Aquí generamos un programa que nos permite
           generar los pronósticos de forma gráfica sin tener que
           incurrir en generar el código para graficarlo y ahorrar líneas de código:
           se llama plot_forecast_comparison.")


source("plot_forecast_comparison.R")

## 4.1: Proyección del EMAE, Argentina:####

#a) Definir parámetros del gráfico:
h_forecast_emae <- 12 # Horizonte ya definido
model_to_plot_emae <- model_emae_arg # Modelo ARIMA ajustado (forzado)
original_ts_emae <- dl_emae
title_emae <- "Dif. Log. EMAE Argentina"
start_plot_emae <- 2022 # Año desde donde mostrar histórico
ylabel_emae <- "Diferencia Logarítmica" # Etiqueta eje Y

#b) Llamar a la función para generar el objeto ggplot.
plot_emae_comparison <- plot_forecast_vs_actual(
  model = model_to_plot_emae,
  original_ts = original_ts_emae,
  h = h_forecast_emae,
  series_title = title_emae,
  start_year_plot = start_plot_emae,
  ylab_text = ylabel_emae,
  show_pi = FALSE # No mostrar Intervalos de Predicción por ahora
)

# c) Mostrar el gráfico generado
plot_emae_comparison

#5.0 Modelo suavizado exponencial vs orignal:
## Argentina:####
## 5.1 EMAE:####
model_ets_emae<-forecast::ets(dl_emae_train, model="ZNZ")

summary(model_ets_emae)


forecast_ets_emae<-forecast(model_ets_emae, h=12)

autoplot(forecast_ets_emae)+ autolayer(dl_emae, series="Observado")+
  ggtitle("ETS sin tendencia: EMAE Argentina")+ xlab("Tiempo (meses)")+
  ylab("Diferencia logarítmica")+theme_bw()


## 5.2 IPC:####
ets_ipc_arg <- ets(dl_ipc_arg_train, model = "ZNZ")
summary(ets_ipc_arg)

forecast_ets_ipc_arg <- forecast(ets_ipc_arg, h = 12)
autoplot(forecast_ets_ipc_arg) +
  autolayer(dl_ipc_arg, series = "Observado") +
  ggtitle("ETS sin tendencia - IPC Argentina (nota: deva auspiciada por el FMI)") +
  xlab("Tiempo") + ylab("Dif. log IPC") +
  theme_minimal()

##5.3 M2:####
ets_m2_arg <- ets(dl_m2_arg_train, model = "ZNZ")
summary(ets_m2_arg)

forecast_ets_m2_arg <- forecast(ets_m2_arg, h = 12)
autoplot(forecast_ets_m2_arg) +
  autolayer(dl_m2_arg, series = "Observado") +
  ggtitle("ETS sin tendencia - M2 Argentina") +
  xlab("Tiempo") + ylab("Dif. log M2") +
  theme_minimal()

## 5.4 Tasa de interés:####
ets_tasa_arg <- ets(dl_tasa_arg_train, model = "ZNZ")
summary(ets_tasa_arg)

forecast_ets_tasa_arg <- forecast(ets_tasa_arg, h = 12)
autoplot(forecast_ets_tasa_arg) +
  autolayer(dl_tasa_arg, series = "Observado") +
  ggtitle("ETS sin tendencia - Tasa Argentina") +
  xlab("Tiempo") + ylab("Dif. log Tasa") +
  theme_minimal()
# Chile:####
##5.5 IMACEC:####

ets_imacec <- ets(dl_imacec_train, model = "ZNZ")
summary(ets_imacec)

forecast_ets_imacec <- forecast(ets_imacec, h = 12)
autoplot(forecast_ets_imacec) +
  autolayer(dl_imacec, series = "Observado") +
  ggtitle("ETS sin tendencia - IMACEC Chile") +
  xlab("Tiempo") + ylab("Dif. log IMACEC") +
  theme_minimal()

## 5.6 IPC####

ets_ipc_chi <- ets(dl_ipc_chi_train, model = "ZNZ")
summary(ets_ipc_chi)

forecast_ets_ipc_chi <- forecast(ets_ipc_chi, h = 12)
autoplot(forecast_ets_ipc_chi) +
  autolayer(dl_ipc_chi, series = "Observado") +
  ggtitle("ETS sin tendencia - IPC Chile") +
  xlab("Tiempo") + ylab("Dif. log IPC") +
  theme_minimal()
## 5.7 M2####

ets_m2_chi <- ets(dl_m2_chi_train, model = "ZNZ")
summary(ets_m2_chi)

forecast_ets_m2_chi <- forecast(ets_m2_chi, h = 12)
autoplot(forecast_ets_m2_chi) +
  autolayer(dl_m2_chi, series = "Observado") +
  ggtitle("ETS sin tendencia - M2 Chile") +
  xlab("Tiempo") + ylab("Dif. log M2") +
  theme_minimal()

## 5.8 Tasa de interés####
ets_tasa_chi <- ets(dl_tasa_chi_train, model = "ZNZ")
summary(ets_tasa_chi)

forecast_ets_tasa_chi <- forecast(ets_tasa_chi, h = 12)
autoplot(forecast_ets_tasa_chi) +
  autolayer(dl_tasa_chi, series = "Observado") +
  ggtitle("ETS sin tendencia - Tasa Chile") +
  xlab("Tiempo") + ylab("Dif. log Tasa") +
  theme_minimal()
