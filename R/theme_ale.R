# =============================================================================
# ARCHIVO: R/theme_ale.R
# DESCRIPCIÓN: Configuración completa del tema y paleta ale para el proyecto
# =============================================================================

#' @title Configuración de tema y paleta ale
#' @description Script que contiene todas las funciones y configuraciones
#' para usar la identidad visual ale en gráficos
#' @author Tu nombre
#' @date $(date)

# Cargar librerías necesarias
if (!requireNamespace("ggplot2", quietly = TRUE)) stop("ggplot2 es requerido")
if (!requireNamespace("scales", quietly = TRUE)) stop("scales es requerido")

library(ggplot2)
library(scales)
library(extrafont)

# =============================================================================
# PALETA GLOBAL ale
# =============================================================================

#' Paleta de colores ale
#' @export
PALETA_ALE <- c(
  "#001f3f", # Azul marino muy oscuro
  "#FFD700", # Amarillo brillante (dorado)
  "#4D4D4D", # Gris oscuro neutro
  "#004080", # Azul profundo
  "#FFCC00", # Amarillo cálido
  "#808080", # Gris medio
  "#003366", # Azul marino clásico
  "#FFE066", # Amarillo pastel
  "#C0C0C0"  # Gris claro
)

#' Color principal ale (primer color de la paleta)
#' @export
ALE_PRIMARY <- PALETA_ALE[1]

#' Color secundario ale
#' @export  
ALE_SECONDARY <- PALETA_ALE[4]

# =============================================================================
# TEMA ale
# =============================================================================

#' Tema ale para ggplot2
#' 
#' @param base_size Tamaño base de fuente
#' @param base_family Familia de fuente
#' @param legend_pos Posición de la leyenda
#' @param grid_y Mostrar líneas de cuadrícula horizontales
#' @param transparent_bg Fondo transparente
#' 
#' @return Un tema de ggplot2
#' @export
#' 
#' @examples
#' ggplot(mtcars, aes(wt, mpg)) + 
#'   geom_point() + 
#'   theme_ale()
theme_ale <- function(base_size = 14,
                       base_family = "Arial",
                       legend_pos = "bottom",
                       grid_y = TRUE,
                       transparent_bg = TRUE) {
  
  # Tema base
tema <- theme_classic(base_size = base_size, base_family = base_family)
  
  # Configuraciones específicas ale
  tema <- tema + theme(
    # Título con estilo ale

    paper = "navyblue",

    plot.title = element_text( 
      size = base_size + 2,
      hjust = 0,
      color = "white",
      face = "bold",
      margin = margin(b = 20)
    ),
    
    # Subtítulo
    plot.subtitle = element_text(
      size = base_size - 1,
      color = "#ffffffff",
      margin = margin(b = 5, t = -10)
    ),
    
    # Texto de ejes
    axis.text = element_text(size = base_size - 1.5, color="white"),
    
    # Líneas de ejes en color ale
    axis.line = element_line(color = "white", linewidth = 0.8),
    axis.ticks = element_line(color = "white", linewidth = 0.6),
    axis.ticks.length = unit(0.15, "cm"),
    
    # Sin títulos de ejes por defecto
    axis.title = element_blank(),
    
    # Leyenda
    legend.position = legend_pos,
    legend.title = element_text(color = "white", face = "bold"),
    legend.text = element_text(color = "white"),
    
    # Paneles
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
  
    plot.background = element_rect(fill = "transparent", color = NA),
    legend.background = element_rect(fill = "transparent", color = NA),
    legend.key = element_rect(fill = "transparent", color = NA)
    )
  
  return(tema)
}


# =============================================================================
# ESCALAS ale
# =============================================================================

#' Escala de colores ale para variables discretas
#' @param ... Argumentos adicionales para scale_color_manual
#' @export
scale_color_ale <- function(...) {
  scale_color_manual(values = PALETA_ALE, ...)
}


#' Escala de relleno ale para variables discretas
#' @param ... Argumentos adicionales para scale_fill_manual
#' @export
scale_fill_ale <- function(...) {
  scale_fill_manual(values = PALETA_ALE, ...)
}

#' Escala continua de colores ale
#' @param low Color bajo (por defecto primer color ale)
#' @param high Color alto (por defecto tercer color ale)
#' @param ... Argumentos adicionales
#' @export
scale_color_ale_continuous <- function(low = PALETA_ALE[1], 
                                       high = PALETA_ALE[4], 
                                       ...) {
  scale_color_gradient(low = low, high = high, ...)
}

#' Escala Y con formato de miles (K)
#' @param ... Argumentos adicionales para scale_y_continuous
#' @export
scale_y_miles <- function(...) {
  scale_y_continuous(
    labels = label_number(scale = 1/1000, suffix = "K"),
    ...
  )
}

#' Escala Y con formato de millones (M)
#' @param ... Argumentos adicionales para scale_y_continuous  
#' @export
scale_y_millones <- function(...) {
  scale_y_continuous(
    labels = label_number(scale = 1/1000000, suffix = "M"),
    ...
  )
}

# =============================================================================
# CONFIGURACIÓN DEL PROYECTO
# =============================================================================

#' Configurar entorno ale para el proyecto actual
#' 
#' Esta función configura automáticamente ggplot2 para usar
#' la paleta y tema ale como predeterminados
#' 
#' @param set_theme_default Si establecer theme_ale como tema por defecto
#' @param update_geoms Si actualizar geoms por defecto con colores ale
#' @param verbose Si mostrar mensajes de configuración
#' 
#' @export
setup_ale <- function(set_theme_default = TRUE, 
                       update_geoms = TRUE,
                       verbose = TRUE,
                       override_scales= TRUE) {
  
  if (update_geoms) {
    # Actualizar defaults de geoms comunes
    update_geom_defaults("point", list(colour = ALE_PRIMARY, size = 2))
    update_geom_defaults("line", list(colour = ALE_PRIMARY, linewidth = 1))
    update_geom_defaults("smooth", list(colour = ALE_SECONDARY))
    update_geom_defaults("bar", list(fill = ALE_PRIMARY))
    update_geom_defaults("col", list(fill = ALE_PRIMARY))
    update_geom_defaults("density", list(fill = ALE_PRIMARY, alpha = 0.6))
    update_geom_defaults("boxplot", list(fill = ALE_PRIMARY, alpha = 0.7))
  }
  
  if (set_theme_default) {
    # Establecer tema ale como predeterminado
    theme_set(theme_ale())
  }

  if (override_scales) {
  # Sobrescribir escalas discretas por defecto
  options(
    ggplot2.discrete.fill = PALETA_ALE,
    ggplot2.discrete.colour = PALETA_ALE
  )
}
  
  # Configurar paleta base de R también
  palette(PALETA_ALE)
  
  if (verbose) {
    cat("🎨 Configuración ale activada para el proyecto\n")
    cat("📊 Tema predeterminado:", ifelse(set_theme_default, "theme_ale()", "no cambiado"), "\n")
    cat("🔧 Geoms actualizados:", ifelse(update_geoms, "sí", "no"), "\n")
    cat("🌈 Paleta principal:", ALE_PRIMARY, "\n")
  }
  
  invisible(TRUE)
}

#' Restablecer configuración por defecto de ggplot2
#' @export
reset_ggplot_defaults <- function() {
  theme_set(theme_gray())
  # Aquí podrías agregar más resets si es necesario
  cat("↩️  Configuración de ggplot2 restablecida a valores por defecto\n")
}

# =============================================================================
# FUNCIONES DE UTILIDAD
# =============================================================================

#' Mostrar la paleta ale
#' @param n Número de colores a mostrar (por defecto todos)
#' @export
mostrar_PALETA_ALE <- function(n = length(PALETA_ALE)) {
  colores <- PALETA_ALE[1:min(n, length(PALETA_ALE))]
  
  df <- data.frame(
    x = 1:length(colores),
    y = 1,
    color = factor(colores, levels = colores)
  )
  
  p <- ggplot(df, aes(x, y, fill = color)) +
    geom_col(width = 0.8) +
    geom_text(aes(label = colores), 
              angle = 90, 
              hjust = 0.5, 
              color = "white", 
              fontface = "bold") +
    scale_fill_manual(values = colores) +
    labs(title = "Paleta ale", 
         subtitle = paste("Mostrando", length(colores), "colores")) +
    theme_void() +
    theme(
      legend.position = "none",
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
      plot.subtitle = element_text(hjust, size = 12)
    )
  
  return(p)
}

#' Crear gráfico de ejemplo con tema ale
#' @export  
ejemplo_ale <- function() {
  # Datos de ejemplo
  datos <- data.frame(
    x = 1:10,
    y = cumsum(rnorm(10, 5, 2)),
    grupo = rep(c("Grupo A", "Grupo B"), each = 5)
  )
  
  ggplot(datos, aes(x, y, color = grupo)) +
    geom_line(linewidth = 1.5) +
    geom_point(size = 3) +
    scale_color_ale() +
    scale_y_miles() +
    labs(
      title = "Ejemplo de Gráfico ale",
      subtitle = "Demonstración del tema y paleta de colores",
      x = "Tiempo",
      y = "Valores (Miles)",
      color = "Categoría"
    ) +
    theme_ale()
}

# =============================================================================
# MENSAJE DE CARGA
# =============================================================================

cat("✅ Tema ale cargado exitosamente\n")
cat("🚀 Ejecuta setup_ale() para configurar como predeterminado\n")
cat("🎨 Ejecuta mostrar_PALETA_ALE() para ver los colores\n")
cat("📊 Ejecuta ejemplo_ale() para ver un ejemplo\n")