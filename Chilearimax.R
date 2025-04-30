##################CHILE######################################
#Datos
Datos_Chile <- read_excel("Datos Chile.xlsx", 
                          range = "A3:F200")
View(Datos_Chile)
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
