library(ggplot2)
library(forecast)
# library(ggfortify) # Quitar dependencia para evitar conflictos
library(stats) # Para window, time, frequency

#' Genera un gráfico comparando el pronóstico ARIMA con los valores reales.
#'
#' @param model Objeto del modelo ARIMA ajustado (salida de Arima o auto.arima).
#' @param original_ts Serie de tiempo completa original (objeto ts).
#' @param h Horizonte de pronóstico (número de períodos).
#' @param series_title Título descriptivo para la serie (usado en título y leyenda).
#' @param start_year_plot Año (numérico, e.g., 2022) desde el cual mostrar los datos históricos en el gráfico.
#' @param ylab_text Etiqueta para el eje Y. Default: "Valor Serie".
#' @param show_pi Booleano, si se deben mostrar los Intervalos de Predicción (PI). Default: FALSE.
#' @param pi_level Nivel de confianza para los PI (e.g., 0.95). Default: 0.95.
#' @param xreg (Opcional) Matriz o data frame con los valores futuros de los regresores para modelos ARIMAX.
#'             Debe tener 'h' filas.
#'
#' @return Un objeto ggplot listo para imprimir o guardar.
#'
plot_forecast_vs_actual <- function(model,
                                    original_ts,
                                    h,
                                    series_title,
                                    start_year_plot,
                                    ylab_text = "Valor Serie",
                                    show_pi = FALSE,
                                    pi_level = 0.95,
                                    xreg = NULL) {
    # Validar entradas básicas
    if (!inherits(model, "forecast_ARIMA")) {
        stop("El parámetro 'model' debe ser un objeto ARIMA de la librería forecast.")
    }
    if (!is.ts(original_ts)) {
        stop("El parámetro 'original_ts' debe ser un objeto de serie de tiempo (ts).")
    }
    # Validación para xreg
    if (!is.null(xreg) && nrow(xreg) != h) {
        stop("El número de filas en 'xreg' debe ser igual al horizonte 'h'.")
    }

    # 1. Generar pronóstico, usando xreg si se proporciona
    if (is.null(xreg)) {
        fc <- forecast(model, h = h, level = pi_level)
    } else {
        # Asegurarse que xreg es una matriz, como forecast espera a veces
        fc <- forecast(model, h = h, level = pi_level, xreg = as.matrix(xreg))
    }

    # 2. Obtener valores reales del período pronosticado
    start_forecast_time_val <- time(fc$mean)[1]
    end_forecast_time_val <- time(fc$mean)[h]

    # Asegurarse que el período real exista en la serie original
    if (end_forecast_time_val > time(original_ts)[length(original_ts)]) {
        warning("El horizonte de pronóstico se extiende más allá de la serie original. No se graficarán todos los puntos reales.")
        end_forecast_time_val <- time(original_ts)[length(original_ts)]
    }

    actual_ts_forecast_period <- window(original_ts,
        start = start_forecast_time_val,
        end = end_forecast_time_val
    )

    # 3. Preparar datos históricos para graficar
    historical_ts_plot <- window(fc$x, start = c(start_year_plot, 1))

    # Calcular límites del eje X
    min_x_limit <- start_year_plot
    max_x_limit <- end_forecast_time_val + (1 / frequency(original_ts)) * 2

    # Colores definidos
    forecast_color <- "#E69F00" # Naranja
    actual_color <- "#0072B2" # Azul
    historical_color <- "black" # Negro

    # 4. Construir el gráfico ggplot - Volviendo a usar 'series' en autolayer
    gg <- ggplot() +
        # Histórico
        autolayer(historical_ts_plot, series = "Histórico") +
        # Pronóstico (línea y opcionalmente PI)
        # Especificamos 'fill' directamente para el área PI si show_pi=TRUE
        autolayer(fc, series = "Pronóstico ARIMA", PI = show_pi, fill = forecast_color) +
        # Real (si hay datos)
        {
            if (length(actual_ts_forecast_period) > 0) {
                autolayer(actual_ts_forecast_period, series = "Real")
            }
        } +
        scale_x_continuous(limits = c(min_x_limit, max_x_limit), breaks = scales::pretty_breaks(n = 8)) +
        scale_y_continuous(labels = scales::number_format(accuracy = 0.001)) +
        # Escala manual para los colores de línea, mapeando los nombres de 'series'
        scale_color_manual(
            name = "Serie", # Título de la leyenda
            values = c("Histórico" = historical_color, "Pronóstico ARIMA" = forecast_color, "Real" = actual_color),
            breaks = c("Histórico", "Pronóstico ARIMA", "Real") # Orden en leyenda
        ) +
        # Como 'fill' se pasó directo a autolayer(fc,...), no necesitamos scale_fill_manual
        # pero sí ocultamos la leyenda de 'fill' que podría generarse automáticamente.
        guides(fill = "none") +
        labs(
            title = paste("Pronóstico vs Real:", series_title),
            subtitle = paste("Modelo:", deparse(substitute(model)), "- Horizonte:", h, "períodos"),
            x = "Año",
            y = ylab_text
            # No necesitamos 'color = "Serie"' aquí si scale_color_manual tiene 'name'
        ) +
        theme_bw(base_size = 12) +
        theme(
            plot.title = element_text(size = 16, face = "bold"),
            plot.subtitle = element_text(size = 11, face = "italic"),
            axis.title = element_text(size = 12),
            axis.text = element_text(size = 10),
            legend.position = "bottom",
            legend.title = element_text(size = 11, face = "bold"),
            legend.text = element_text(size = 10)
        )

    return(gg)
}

# Ejemplo de uso (para referencia, no se ejecuta aquí):
# source("plot_forecast_comparison.R")
# mi_grafico <- plot_forecast_vs_actual(
#   model = model_emae_arg,         # Modelo ajustado
#   original_ts = dl_emae,          # Serie original completa (diferencias log)
#   h = 12,                         # Horizonte
#   series_title = "Dif. Log. EMAE Argentina", # Título específico
#   start_year_plot = 2022,         # Año inicio gráfico
#   ylab_text = "Diferencia Logarítmica", # Etiqueta Y
#   show_pi = FALSE                 # No mostrar intervalos
# )
# print(mi_grafico)
#
# Para guardar con tamaño específico (más ancho):
# dir.create("graficos", showWarnings = FALSE)
# ggsave(
#    "graficos/pronostico_emae_comparacion_forzado_ancho.png",
#    plot = mi_grafico,
#    width = 10, # Más ancho
#    height = 6, # Menos alto
#    dpi = 300
# )
