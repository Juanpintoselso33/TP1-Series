setwd("C:/Users/solgo/OneDrive/Escritorio/Maestria . Series de T")
getwd()
library(readxl)
#Librerias
# Instalar las librerías necesarias (solo la primera vez)
install.packages("readxl")
install.packages("forecast")
install.packages("tseries")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("zoo")
install.packages("lubridate")
# Cargar librerías necesarias
library(readxl)
library(forecast)
library(tseries)
library(ggplot2)
library(dplyr)
library(zoo)
library(lubridate)


Datos_Argentina_xls <- read_excel("Datos_Argentina.xls.xlsx")
View(Datos_Argentina_xls)
Datos_Chile <- read_excel("Datos Chile.xlsx", 
                          range = "A3:F200")
View(Datos_Chile)
# Argentina
#Cambiar el nombre de la columna ...1 a fecha
colnames(Datos_Argentina_xls)[colnames(Datos_Argentina_xls) == "...1"] <- "fecha"

# Sacamos la M y añadimos "-01" al final para completar el día del mes
Datos_Argentina_xls$fecha <- gsub("M", "-", Datos_Argentina_xls$fecha)
View(Datos_Argentina_xls)
Datos_Argentina_xls$fecha <- paste(Datos_Argentina_xls$fecha, "01", sep="-")
# Aseguramos que esté en formato Date
Datos_Argentina_xls$fecha <- as.Date(Datos_Argentina_xls$fecha, format="%Y-%m-%d")

#Chile
# Aseguramos que periodo en Chile esté en formato Date
Datos_Chile$periodo <- as.Date(Datos_Chile$Periodo, format="%Y-%m-%d")  # Asegurarse de que esté en formato Date
head(Datos_Argentina_xls$fecha)
head(Datos_Chile$periodo)

# Extraemos la serie de EMAE
EMAE <- Datos_Argentina_xls$EMAE  

# Creamos la serie de tiempo mensual
EMAE_ts <- ts(EMAE, start=c(2004, 1), frequency=12)
# Graficamos la serie temporal
windows()
plot.ts(EMAE_ts, main="Serie Temporal de EMAE (Argentina)", ylab="EMAE", xlab="Tiempo", col="black")
dev.off()

# Calculamos la tasa de variación logarítmica
tasa_EMAE_larga <- diff(log(EMAE_ts))
tasa_EMAE <- window(tasa_EMAE_larga, end=c(2022,12))

# Mostramos las primeras observaciones
head(tasa_EMAE, 8)

# Graficamos la tasa emae
windows()
plot.ts(tasa_EMAE, main="Tasa de Cambio Logarítmica de EMAE", ylab="Tasa de Cambio", xlab="Tiempo", col="black")
dev.off()


# Instalo forecast y lmtest
install.packages("forecast")
library(forecast)
library(lmtest)

# Autoarima ADITIVO, le pido que la parte seasonal no la use
auto.arima(tasa_EMAE, stationary=T, seasonal=F, ic="aic")
auto_emae=auto.arima(tasa_EMAE, stationary=T, seasonal=F, ic="aic") #Es un Ma2 
coeftest(auto_emae) # Es significativo al 10% recien
# Verifico ruido blanco 
corr_res(auto_emae,26,2) #p values son mayores a 0.05 , es ruido blanco
# Autoarima MULTIPLICATIVO, seasonal true 
auto.arima(tasa_EMAE, stationary=T, seasonal=T, ic="aic") #ma2
automul_emae=auto.arima(tasa_EMAE, stationary=T, seasonal=F, ic="aic")
coeftest(automul_emae)
# Verifico ruido blanco 
corr_res(automul_emae,26,2) #p values son mayores a 0.05. Es ruido blanco

#Modelo Manual 
# Probamos con un ar(1) + ma (1) (Aditivo) para la estacionalidad
emae_manual=arima(tasa_EMAE, order=c(1,0,1), fixed=c(NA,NA,NA))
coeftest(emae_manual)
corr_res(emae_manual, 26, 2)  #Es ruido blanco

# Modelo manual AR1 MA1 pero multiplicativo #
emae_manual_multi = arima(tasa_EMAE, 
                          order = c(1, 0, 1), 
                          seasonal = list(order = c(1, 0, 0), period = 12),
                          fixed = c(NA, NA, NA, NA))
coeftest(emae_manual_multi)
corr_res(emae_manual_multi, 26, 2)  #Es ruido blanco

# Elegimos el mejor modelo con AIC
AIC(auto_emae) 
AIC(automul_emae)
AIC(emae_manual)
AIC(emae_manual_multi)  
# El mejor es autoadit_emae ya que es el menor criterio segun AIC

# Elegimos el mejor modelo con BIC
BIC(auto_emae)
BIC (automul_emae)
BIC(emae_manual)
BIC(emae_manual_multi)  
# El mejor es el modelo autoadit_emae ya que es el menor criterio según BIC, coincide con AIC

# 3)Pronostico con el modelo autoadit_emae
# El período muestral de los modelos de regresión fueron enero/2004-diciembre/2022
# Esto implica que no necesito generar una nueva muestra reducida
# Tampoco generar estimaciones con la muestra recortada, ni probar ruido blanco

# Predigo auto_emae y emae_manual para los proximos 12 meses, ene/23-dic/23
auto_emae.p=predict(auto_emae,n.ahead=12)
emae_manual.p=predict(emae_manual,n.ahead=12)

# Genero una serie con los valores reales de emae para ene/23-dic/23
tasaemae_real = window(tasa_EMAE_larga, start=c(2023,1), end=c(2023,12))

# Longitud de cada uno
length(auto_emae.p$pred)
length(tasaemae_real)

# Calculo para ambos pronósticos los estadísticos de bondad 
# del pronóstico
Acc_1=accuracy(auto_emae.p$pred , tasaemae_real)
Acc_2=accuracy(emae_manual.p$pred , tasaemae_real)
Acc=rbind(Acc_1,Acc_2)
rownames(Acc)=c("auto_emae","emae_manual")  ; t(Acc)

# También puede hacerse con el comando Eval_Pron presentado
# que incluye la descomposición del Theil
Ev_1=Eval_Pron(auto_emae.p$pred , tasaemae_real,Nombre="auto_emae")
Ev_2=Eval_Pron(emae_manual.p$pred, tasaemae_real,Nombre="emae_manual")
Ev=cbind(Ev_1,Ev_2)    ; Ev

# Relleno los años no pronosticados
f_auto_emae <- ts(
  c(rep(NA, length(tasa_EMAE_larga) - 12), auto_emae.p$pred), 
  start=c(2004,2),  # Arranca en 2004 mes 2 por el diff()
  frequency=12
)

f_emae_manual <- ts(
  c(rep(NA, length(tasa_EMAE_larga) - 12), emae_manual.p$pred),
  start=c(2004,2),
  frequency=12
)

# Graficamos todo junto
windows()
plot(tasa_EMAE_larga, main="Pronóstico EMAE - diferencias logarítmicas", ylab="Tasa", xlab="Tiempo", col="black")
lines(f_auto_emae, col="blue", lty=2)      # modelo automático
lines(f_emae_manual, col="red", lty=2)      # modelo manual

#4)Recatar los niveles pronosticados

# Sumo las diferencias a partir del último período de la muestra utilizada
ultimo_valor_EMAE = log(as.numeric(window(EMAE_ts, start=c(2022,12), end=c(2022,12))))
predicciones_auto=diffinv(auto_emae.p$pred)+ultimo_valor_EMAE

# Y luego tomo antilogaritmos
antilog=exp(predicciones_auto)  
# Y retiro el último período de la muestra utilizada
antilog = window(antilog, start=c(2023,1))

# Y tomo los valores verdaderos de esos años para comparar
EMAE_REAL=window(EMAE_ts,start=c(2023,1) , end=c(2023,12))

# Graficar desde 2004 
EMAE_2004 = window(EMAE_ts, start=c(2004,1), end=c(2023,12))

# Relleno los años NO pronosticados con NA (hasta 2022-12)
f_auto = ts(c(rep(NA, length(window(EMAE_2004, end=c(2022,12)))), antilog),
            start=start(EMAE_2004), frequency=12)

# Gráfico todo junto
windows()
plot(EMAE_2004, ylim=range(c(EMAE_2004, antilog)), main="EMAE y Pronóstico Automático", ylab="Índice EMAE")
lines(f_auto, col="blue", lty=1)
legend("topleft", legend=c("Real", "Pronóstico Automático"), col=c("black", "blue"), lty=1)

# Calculo para el pronóstico automático los estadísticos de bondad del pronóstico
Ev_auto = Eval_Pron(antilog, EMAE_REAL, Nombre="Automático")
Ev = Ev_auto
Ev

#5 Suavizado exponencial
# Recorto la muestra (enero-2004 a diciembre-2022)
EMAE_hasta2022 = window(EMAE_ts, start=c(2004,1), end=c(2022,12))
# Modelo Holt-Winters sin tendencia (solo suavizado estacional)
hw_EMAE = hw(EMAE_hasta2022, h=12)
# Gráfico de las predicciones
# Graficar ambos pronósticos junto con la serie real
windows()
# Gráfico de la serie real (2004-2023) en negro
plot(window(EMAE_ts, start=c(2004,1), end=c(2023,12)), main="Pronóstico Holt-Winters vs ARIMA", ylab="Índice EMAE", col="black", type="l")
# Línea de Holt-Winters solo para el año 2023 en rojo
lines(hw_EMAE$mean, col="red", lty=1)  # Holt-Winters para 2023 en rojo
# Línea de la predicción ARIMA solo para el año 2023 en azul
lines(antilog, col="blue", lty=2)  # ARIMA para 2023 en azul
# Leyenda
legend("topleft", legend=c("Serie Real (2004-2023)", "Pronóstico Holt-Winters 2023", "Pronóstico ARIMA 2023"), 
       col=c("black", "red", "blue"), lty=c(1, 1, 2), cex=0.8)

#6) Para calcular los VALORES EN NIVELES de las variables en los períodos pronosticados vamos a hacer una tabla

# Creamos un data frame combinando los valores
tabla_valores <- data.frame(
  Fecha = time(EMAE_REAL),
  Real = as.numeric(EMAE_REAL),
  Holt_Winters = as.numeric(hw_EMAE$mean),
  ARIMA = as.numeric(antilog)
)

# Mostramos la tabla
print(tabla_valores)


# Estadísticos de bondad del pronóstico para HW

Ev_hw = Eval_Pron(hw_EMAE$mean, EMAE_REAL, Nombre="Holt-Winters")
Ev = Ev_hw
Ev



#)7)ARMADO DE TABLA de los estadisticos de bondad

# Calculo los estadísticos de bondad del pronóstico para ARIMA
Ev_arima = Eval_Pron(antilog, EMAE_REAL, Nombre="ARIMA")

# Calculo los estadísticos de bondad del pronóstico para Holt-Winters
Ev_hw = Eval_Pron(hw_EMAE$mean, EMAE_REAL, Nombre="Holt-Winters")

# Creo una tabla comparativa
tabla_comparacion = rbind(Ev_arima, Ev_hw)
print(tabla_comparacion)

Eval_Pron <- function(predichos, reales, Nombre="Modelo") {
  # Aseguro que predichos y reales tengan la misma longitud
  if(length(predichos) != length(reales)) {
    stop("Predichos y reales deben tener la misma longitud")
  }
  
  # Calcular métricas comunes
  error <- predichos - reales
  mae <- mean(abs(error))
  mse <- mean(error^2)
  rmse <- sqrt(mse)
  mape <- mean(abs(error / reales)) * 100
  
  # Retornar como data.frame con nombre de modelo
  return(data.frame(
    Modelo = Nombre,
    MAE = round(mae, 2),
    RMSE = round(rmse, 2),
    MAPE = round(mape, 2)
  ))
}

# Definir
EMAE_REAL <- window(EMAE_ts, start=c(2023,1), end=c(2023,12))

# Evaluaciones
Ev_arima <- Eval_Pron(antilog, EMAE_REAL, Nombre="ARIMA")
Ev_hw    <- Eval_Pron(hw_EMAE$mean, EMAE_REAL, Nombre="Holt-Winters")

# Tabla comparativa robusta
tabla_comparacion <- rbind(Ev_arima, Ev_hw)
print(tabla_comparacion)


##################CHILE######################################3
# Extraemos la serie de IMACEC
# Serie de IMACEC
IMACEC <- Datos_Chile$IMACEC  

# Creamos la serie de tiempo mensual IMACEC
IMACEC_ts <- ts(IMACEC, start=c(2004, 1), frequency=12)
# Graficamos la serie temporal IMACEC
windows()
plot.ts(IMACEC_ts, main="Serie Temporal de IMACEC (Chile)", ylab="IMACEC", xlab="Tiempo", col="orange")
dev.off()

# Calculamos la tasa de variación logarítmica
tasa_IMACEC_larga <- diff(log(IMACEC_ts))
tasa_IMACEC <- window(tasa_IMACEC_larga, end=c(2022,12))

# Mostramos las primeras observaciones
head(tasa_IMACEC, 8)

# Graficamos la tasa imacec
windows()
plot.ts(tasa_IMACEC, main="Tasa de Cambio Logarítmica de IMACEC", ylab="Tasa de Cambio", xlab="Tiempo", col="green")
dev.off()

# Autoarima ADITIVO, le pido que la parte seasonal no la use
auto.arima(tasa_IMACEC, stationary=T, seasonal=F, ic="bic")
auto_IMACEC=auto.arima(tasa_IMACEC, stationary=T, seasonal=F, ic="bic") #Es un ARIMA(4,0,2) con aic y un (0,0,3) con bic
coeftest(auto_IMACEC) # Son todos significativos excepto MA(1)
# Verifico ruido blanco 
corr_res(auto_IMACEC,26,3) # NO HAY RUIDO BLANCO, P VALORES CERCANOS A 0 EN LA MAYORIA DE LAGS

# Autoarima MULTIPLICATIVO, seasonal true 
auto.arima(tasa_IMACEC, stationary=T, seasonal=T, ic="aic") #ma2
automul_IMACEC=auto.arima(tasa_IMACEC, stationary=T, seasonal=T, ic="aic")
coeftest(automul_IMACEC) # ARIMA(0,0,2)(0,0,2)
# Verifico ruido blanco 
corr_res(automul_IMACEC,26,4) # TAMPOCO ES RUIDO BLANCO

#Modelo Manual 
# Probamos con un aditivo 
IMACEC_manual=arima(tasa_IMACEC, order=c(2,0,2), fixed=c(NA,NA,NA,NA,NA))
coeftest(IMACEC_manual)
corr_res(IMACEC_manual, 26, 4)  # NO es ruido blanco


# Probamos con modelo multiplicativo #
IMACEC_manual_multi = arima(tasa_IMACEC, 
                          order = c(1, 0, 1), 
                          seasonal = list(order = c(1, 0, 0), period = 12),
                          fixed = c(NA, NA, NA, NA))
coeftest(IMACEC_manual_multi)
corr_res(IMACEC_manual_multi, 26, 3)  #NO Es ruido blanco

#Probamos sin constante
modelo_IMACEC_112_nocons <- Arima(tasa_IMACEC, order = c(1,1,2), include.constant = FALSE)
coeftest(modelo_IMACEC_112_nocons)
corr_res(modelo_IMACEC_112_nocons, 26, 3) #NO es ruido blanco

library(forecast)

#Busco modelo alternativo

# Crear fechas para cada observación
dates <- seq(as.Date("2004-02-01"), by = "month", length.out = length(tasa_IMACEC))

# Crear regresores
regs <- data.frame(
  leap       = as.integer(leap_year(dates)),
  td_ld      = sapply(dates, function(d) {
    dias <- seq(floor_date(d, "month"), by = "day", length.out = days_in_month(d))
    sum(!weekdays(dias) %in% c("Friday","Saturday","Sunday"))
  }),
  AO_2010_02 = as.integer(dates == as.Date("2010-02-01")),
  AO_2020_04 = as.integer(dates == as.Date("2020-04-01"))
)

# Eliminar regresores constantes y convertir a matriz
regs <- as.matrix(regs[, sapply(regs, var, na.rm = TRUE) > 0])

# Paso 4: Estimar el modelo ARIMAX
model_imacec <- Arima(
  y        = tasa_IMACEC,
  order    = c(4, 0, 2),
  seasonal = list(order = c(1, 1, 2), period = 12),
  xreg     = regs,
  method   = "ML"
)

summary(model_imacec) #ARIMA(4,0,2)(1,1,2)[12] errors

# Grados de libertad
num_arma_params <- sum(model_imacec$arma[c(1, 2, 3, 4)])
drift_present   <- as.integer("drift" %in% names(model_imacec$coef))
grados_libertad_q <- num_arma_params + drift_present

cat("Calculando p_q como:", num_arma_params, "(arma) +", drift_present, "(drift) =",
    grados_libertad_q, "\n")

# Diagnóstico de residuos
corr_res(
  xreg = model_imacec,
  lags = 35,
  p_q  = grados_libertad_q
)

##################################################################3




# Elegimos el mejor modelo con AIC
AIC(auto_IMACEC) 
AIC(automul_IMACEC)
AIC(IMACEC_manual)
AIC(IMACEC_manual_multi)  
# El mejor es 

# Elegimos el mejor modelo con BIC
BIC(auto_IMACEC)
BIC (automul_IMACEC)
BIC(IMACEC_manual)
BIC(IMACEC_manual_multi)  
# El mejor es el modelo   

# 3)Pronostico con el modelo elegido
# El período muestral de los modelos de regresión fueron enero/2004-diciembre/2022
# Esto implica que no necesito generar una nueva muestra reducida
# Tampoco generar estimaciones con la muestra recortada, ni probar ruido blanco

# Predigo auto_emae y emae_manual para los proximos 12 meses, ene/23-dic/23
auto_IMACEC.p=predict(auto_IMACEC,n.ahead=12)
IMACEC_manual.p=predict(emae_IMACEC,n.ahead=12)

# Genero una serie con los valores reales de emae para ene/23-dic/23
tasaIMACEC_real = window(tasa_IMACEC_larga, start=c(2023,1), end=c(2023,12))

# Longitud de cada uno
length(auto_IMACEC.p$pred)
length(tasaIMACEC_real)

# Calculo para ambos pronósticos los estadísticos de bondad 
# del pronóstico
Acc_1=accuracy(auto_IMACEC.p$pred , tasaIMACEC_real)
Acc_2=accuracy(IMACEC_manual.p$pred , tasaIMACEC_real)
Acc=rbind(Acc_1,Acc_2)
rownames(Acc)=c("auto_IMACEC","emae_IMACEC")  ; t(Acc)

# También puede hacerse con el comando Eval_Pron presentado
# que incluye la descomposición del Theil
Ev_1=Eval_Pron(auto_IMACEC.p$pred , tasaIMACEC_real,Nombre="auto_IMACEC")
Ev_2=Eval_Pron(IMACEC_manual.p$pred, tasaIMACEC_real,Nombre="IMACEC_manual")
Ev=cbind(Ev_1,Ev_2)    ; Ev

# Relleno los años no pronosticados
f_auto_IMACEC <- ts(
  c(rep(NA, length(tasa_IMACEC_larga) - 12), auto_IMACEC.p$pred), 
  start=c(2004,2),  # Arranca en 2004 mes 2 por el diff()
  frequency=12
)

f_IMACEC_manual <- ts(
  c(rep(NA, length(tasa_IMACEC_larga) - 12), emae_manual.p$pred),
  start=c(2004,2),
  frequency=12
)

# Graficamos todo junto
windows()
plot(tasa_IMACEC_larga, main="Pronóstico IMACEC - diferencias logarítmicas", ylab="Tasa", xlab="Tiempo", col="black")
lines(f_auto_IMACEC, col="violet", lty=2)      # modelo automático
lines(f_IMACEC_manual, col="red", lty=2)      # modelo manual

#4)Recatar los niveles pronosticados

# Sumo las diferencias a partir del último período de la muestra utilizada
ultimo_valor_IMACEC = log(as.numeric(window(IMACEC_ts, start=c(2022,12), end=c(2022,12))))
predicciones_auto=diffinv(auto_IMACEC.p$pred)+ultimo_valor_IMACEC

# Y luego tomo antilogaritmos
nivelesIMACEC=exp(predicciones_auto)  
# Y retiro el último período de la muestra utilizada
nivelesIMACEC = window(nivelesIMACEC, start=c(2023,1))

# Y tomo los valores verdaderos de esos años para comparar
IMACEC_REAL=window(IMACEC_ts,start=c(2023,1) , end=c(2023,12))

# Graficar desde 2004 
IMACEC_2004 = window(IMACEC_ts, start=c(2004,1), end=c(2023,12))

# Relleno los años NO pronosticados con NA (hasta 2022-12)
f_auto = ts(c(rep(NA, length(window(IMACEC_2004, end=c(2022,12)))), nivelesIMACEC),
            start=start(IMACEC_2004), frequency=12)

# Gráfico todo junto
windows()
plot(EMAE_2004, ylim=range(c(IMACEC_2004, antilog)), main="IMACEC y Pronóstico Automático", ylab="Índice IMACEC")
lines(f_auto, col="blue", lty=1)
legend("topleft", legend=c("Real", "Pronóstico Automático"), col=c("black", "blue"), lty=1)

# Calculo para el pronóstico automático los estadísticos de bondad del pronóstico
Ev_auto = Eval_Pron(nivelesIMACEC, IMACEC_REAL, Nombre="Automático")
Ev = Ev_auto
Ev

#5 Suavizado exponencial
# Recorto la muestra (enero-2004 a diciembre-2022)
IMACEC_hasta2022 = window(IMACEC_ts, start=c(2004,1), end=c(2022,12))
# Modelo Holt-Winters sin tendencia (solo suavizado estacional)
hw_IMACEC = hw(IMACEC_hasta2022, h=12)
# Gráfico de las predicciones
# Graficar ambos pronósticos junto con la serie real
windows()
# Gráfico de la serie real (2004-2023) en negro
plot(window(IMACEC_ts, start=c(2004,1), end=c(2023,12)), main="Pronóstico Holt-Winters vs ARIMA", ylab="Índice IMACEC", col="black", type="l")
# Línea de Holt-Winters solo para el año 2023 en rojo
lines(hw_IMACEC$mean, col="red", lty=1)  # Holt-Winters para 2023 en rojo
# Línea de la predicción ARIMA solo para el año 2023 en azul
lines(nivelesIMACEC, col="violet", lty=2)  # ARIMA para 2023 en violeta
# Leyenda
legend("topleft", legend=c("Serie Real (2004-2023)", "Pronóstico Holt-Winters 2023", "Pronóstico ARIMA 2023"), 
       col=c("black", "red", "violet"), lty=c(1, 1, 2), cex=0.8)

#6) Para calcular los VALORES EN NIVELES de las variables en los períodos pronosticados vamos a hacer una tabla

# Creamos un data frame combinando los valores
tabla_valores <- data.frame(
  Fecha = time(IMACEC_REAL),
  Real = as.numeric(IMACEC_REAL),
  Holt_Winters = as.numeric(hw_IMACEC$mean),
  ARIMA = as.numeric(nivelesIMACEC)
)

# Mostramos la tabla
print(tabla_valores)


# Estadísticos de bondad del pronóstico para HW

Ev_hw = Eval_Pron(hw_IMACEC$mean, IMACEC_REAL, Nombre="Holt-Winters")
Ev = Ev_hw
Ev



#)7)ARMADO DE TABLA de los estadisticos de bondad

# Calculo los estadísticos de bondad del pronóstico para ARIMA
Ev_arima = Eval_Pron(nivelesIMACEC, IMACEC_REAL, Nombre="ARIMA")

# Calculo los estadísticos de bondad del pronóstico para Holt-Winters
Ev_hw = Eval_Pron(hw_IMACEC$mean, IMACEC_REAL, Nombre="Holt-Winters")

# Creo una tabla comparativa
tabla_comparacion = rbind(Ev_arima, Ev_hw)
print(tabla_comparacion)

Eval_Pron <- function(predichos, reales, Nombre="Modelo") {
  # Aseguro que predichos y reales tengan la misma longitud
  if(length(predichos) != length(reales)) {
    stop("Predichos y reales deben tener la misma longitud")
  }
  
  # Calcular métricas comunes
  error <- predichos - reales
  mae <- mean(abs(error))
  mse <- mean(error^2)
  rmse <- sqrt(mse)
  mape <- mean(abs(error / reales)) * 100
  
  # Retornar como data.frame con nombre de modelo
  return(data.frame(
    Modelo = Nombre,
    MAE = round(mae, 2),
    RMSE = round(rmse, 2),
    MAPE = round(mape, 2)
  ))
}

# Definir
IMACEC_REAL <- window(IMACEC_ts, start=c(2023,1), end=c(2023,12))

# Evaluaciones
Ev_arima <- Eval_Pron(nivelesIMACEC, IMACEC_REAL, Nombre="ARIMA")
Ev_hw    <- Eval_Pron(hw_EMAE$mean, IMACEC_REAL, Nombre="Holt-Winters")

# Tabla comparativa robusta
tabla_comparacion <- rbind(Ev_arima, Ev_hw)
print(tabla_comparacion)
