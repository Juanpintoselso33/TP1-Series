# --- Función para guardar tablas de corr_res en .docx ---

#' Guarda una tabla de resultados de corr_res en un archivo .docx
#'
#' @param corr_result El objeto resultado de la función corr_res().
#' @param filename_base El nombre base para el archivo .docx (sin la extensión).
#' @param output_dir El directorio donde se guardará la subcarpeta 'output_tables'.
#' @param table_colnames Vector con los nombres de las columnas para la tabla.
#' @param note Texto de la nota al pie de la tabla.
#'
#' @return NULL. La función guarda el archivo como efecto secundario.
#' @examples
#' # Suponiendo que 'corr1' es el resultado de corr_res() y 'project_path' está definido
#' # save_corr_table(corr1, "emae_corr_arg", project_path)
save_corr_table <- function(corr_result,
                            filename_base,
                            output_dir,
                            table_colnames = c("Rezagos", "Estadístico Q", "p-valor"),
                            note = "Nota: Elaboración propia por los autores.") {
    # Verificar si los paquetes necesarios están instalados
    if (!requireNamespace("officer", quietly = TRUE) || !requireNamespace("flextable", quietly = TRUE)) {
        warning("Los paquetes 'officer' y 'flextable' son necesarios para guardar la tabla. Por favor, instálalos.", call. = FALSE)
        return(invisible(NULL)) # Salir si faltan paquetes
    }

    # Crear el subdirectorio para las tablas si no existe
    tables_subdir <- file.path(output_dir, "output_tables")
    if (!dir.exists(tables_subdir)) {
        dir.create(tables_subdir, recursive = TRUE, showWarnings = FALSE)
        message("Directorio creado: ", tables_subdir)
    }

    # Construir la ruta completa del archivo
    target_file <- file.path(tables_subdir, paste0(filename_base, "_corr.docx"))

    # Convertir a data frame y asignar nombres
    corr_df <- as.data.frame(corr_result)
    # Asegurarse de que el número de nombres coincide con las columnas
    if (ncol(corr_df) == length(table_colnames)) {
        colnames(corr_df) <- table_colnames
    } else {
        warning(paste("Número de columnas en la tabla (", ncol(corr_df), ") no coincide con el número de nombres proporcionados (", length(table_colnames), "). Usando nombres por defecto."), call. = FALSE)
    }


    # Crear el documento y añadir la tabla con flextable y officer
    doc <- officer::read_docx() %>%
        flextable::body_add_flextable(value = flextable::qflextable(corr_df)) %>%
        officer::body_add_par(note)

    # Guardar el archivo .docx
    tryCatch(
        {
            print(doc, target = target_file)
            message("Tabla guardada en: ", target_file)
        },
        error = function(e) {
            warning("Error al guardar el archivo .docx: ", e$message, call. = FALSE)
        }
    )

    return(invisible(NULL)) # Devolver NULL invisiblemente
}
