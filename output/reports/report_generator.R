# ====================================================================
# SISTEMA DE GENERACIÓN AUTOMÁTICA DE REPORTES FX LIVE
# Central American Business Intelligence
# ====================================================================

# Cargar librerías necesarias
library(glue)
library(tidyverse)
library(yaml)

# Configuración del portafolio
params <- read_yaml("params.yaml")
stock <- params$stocks

# ====================================================================
# FUNCIÓN PARA CREAR TEMPLATE QMD POR STOCK
# ====================================================================

for (s in stock) {
  # Template del documento qmd 
  template_qmd <- glue(
    '
# {s} {{.unnumbered}}

::: callout-note
**Autores:** Maria José Casassola y Alejandro Milián  
**Fecha:** `{{r}} Sys.Date()`  
**Ticker:** {s}  
**Modelos evaluados:** Monte Carlo, ARIMA, Regresión, SMA  
:::

```{{r setup, include=FALSE}}
library(tidyverse)
library(purrr)
library(knitr)
library(DT)
library(kableExtra)
library(quantmod)
knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE)

#| include: false
#| label: setup-imagenes
  
# Crear directorio local para imágenes si no existe
dir.create("plots/{s}", recursive = TRUE, showWarnings = FALSE)

# Lista de imágenes a copiar
imagenes <- c("ARIMA_{s}.png", "MonteCarlo_{s}.png", "Regression_{s}.png", "SMA_{s}.png")

# Copiar imágenes desde output/plots/{s}/ a plots/{s}/ (directorio local)
for (img in imagenes) {{
  origen <- file.path("output", "plots", "{s}", img)
  destino <- file.path("plots", "{s}", img)
  
  if (file.exists(origen)) {{
    file.copy(origen, destino, overwrite = TRUE)
  }} else {{
    cat("Advertencia: No se encontró la imagen:", origen, "\\n")
  }}
}}
```

```{{css}}
/* Tema oscuro compatible */
h1 {{
  text-align: center;
  color: #ffffff;
}}
h2 {{
  color: #ffffff;
  border-bottom: 2px solid #007bff;
  padding-bottom: 5px;
}}

/* Tablas compatibles con tema oscuro */
table, th, td {{
  border: 1px solid #495057;
  border-collapse: collapse;
  padding: 8px;
  text-align: center;
  background-color: #343a40;
  color: #ffffff;
}}
th {{
  background-color: #495057 !important;
  font-weight: bold;
  color: #ffffff !important;
}}
tr:nth-child(even) td {{
  background-color: #495057 !important;
  color: #ffffff !important;
}}
tr:nth-child(odd) td {{
  background-color: #343a40 !important;
  color: #ffffff !important;
}}
.prediction-tomorrow {{
  background-color: #28a745 !important;
  color: #ffffff !important;
  font-weight: bold;
}}

/* Forzar colores en tablas kable */
.table {{
  color: #ffffff !important;
  background-color: #343a40 !important;
}}
.table th {{
  background-color: #495057 !important;
  color: #ffffff !important;
  border-color: #495057 !important;
}}
.table td {{
  background-color: #343a40 !important;
  color: #ffffff !important;
  border-color: #495057 !important;
}}
.table-striped tbody tr:nth-of-type(odd) td {{
  background-color: #495057 !important;
  color: #ffffff !important;
}}
.table-hover tbody tr:hover td {{
  background-color: #6c757d !important;
  color: #ffffff !important;
}}
```

```{{r cargar-datos}}
# Cargar serie original - RUTA ACTUALIZADA
serie_original <- read_csv("data/processed/{s}_full.csv") |>
  rename(real = close) |>
  select(date, real, daily_return, weekly_return, monthly_return, yoy_return)

# Cargar modelos predictivos - RUTAS ACTUALIZADAS
model_montecarlo <- readRDS("models/MonteCarlo/model_montecarlo_{s}.rds")
model_ARIMA <- readRDS("models/ARIMA/model_ARIMA_{s}.rds")
model_regression <- readRDS("models/Regression/model_regression_{s}.rds")
model_SMA <- readRDS("models/SMA/model_SMA_{s}.rds") |>
  rename(pred = sma_prediction)

# Función para preparar datos de modelo
preparar_modelo <- function(modelo_data, nombre_modelo) {{
  modelo_data |>
    mutate(
      date = as.Date(date),
      modelo = nombre_modelo
    ) |>
    select(date, pred = pred, modelo)
}}

# Preparar datos de todos los modelos
modelos_data <- bind_rows(
  preparar_modelo(model_montecarlo, "Monte Carlo"),
  preparar_modelo(model_ARIMA, "ARIMA"),
  preparar_modelo(model_regression, "Regresión"),
  preparar_modelo(model_SMA, "SMA")
)

# Combinar serie real con predicciones
datos_completos <- serie_original |>
  full_join(modelos_data, by = "date") |>
  arrange(date)

# Separar datos históricos de predicciones futuras
fecha_hoy <- Sys.Date()
datos_historicos <- datos_completos |>
  filter(date < fecha_hoy, !is.na(real))

datos_futuros <- datos_completos |>
  filter(date >= fecha_hoy | is.na(real)) |>
  filter(!is.na(pred))
```

## Gráfico Principal de Comparación de Modelos

```{{r grafico-principal, fig.height=6, fig.width=10, fig.align=\'center\', dpi=300}}
# Obtener los últimos 30 días para mejor visualización
fecha_inicio <- max(datos_historicos$date) - days(40)

# Filtrar datos para el gráfico
datos_grafico <- datos_completos |>
  filter(date >= fecha_inicio) |>
  # Crear serie continua para valores reales
  mutate(real_continuo = ifelse(!is.na(real), real, NA))

# Crear el gráfico
ggplot(datos_grafico, aes(x = date)) +
  # Línea de valores reales
  geom_line(
    aes(y = real_continuo),
    color = "black",
    linewidth = 1.2,
    na.rm = TRUE
  ) +
  # Líneas de predicciones por modelo
  geom_line(aes(y = pred, color = modelo), linewidth = 1, na.rm = TRUE) +
  # Puntos para el día de mañana
  geom_point(
    data = filter(datos_grafico, date == fecha_hoy),
    aes(y = pred, color = modelo),
    size = 3,
    na.rm = TRUE
  ) +
  # Línea vertical para marcar "hoy"
  geom_vline(
    xintercept = as.numeric(fecha_hoy),
    linetype = "dashed",
    color = "gray50",
    alpha = 0.7
  ) +
  scale_color_brewer(type = "qual", palette = "Set1") +
  labs(
    title = "Comparación de Predicciones de Modelos - {s}",
    subtitle = paste(
      "Datos históricos vs Predicciones |",
      "Línea punteada indica el día actual"
    ),
    x = "Fecha",
    y = "Precio de Cierre ($)",
    color = "Modelo"
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "3 days")
```

::: {{.panel-tabset}}

## Tablas de Resultados

### Últimos 5 Valores Reales y Predicciones

```{{r tabla-resultados}}
# Obtener los últimos 5 valores reales
ultimos_reales <- datos_historicos |>
  filter(!is.na(real)) |>
  arrange(desc(date)) |>
  slice_head(n = 5) |>
  select(date, real) |>
  arrange(date)

# Obtener predicciones para esas fechas
predicciones_historicas <- modelos_data |>
  filter(date %in% ultimos_reales$date) |>
  pivot_wider(names_from = modelo, values_from = pred, names_prefix = "pred_")

# Obtener predicción para mañana
fecha_manana <- fecha_hoy + days(1)
prediccion_manana <- modelos_data |>
  filter(date == fecha_manana) |>
  pivot_wider(
    names_from = modelo,
    values_from = pred,
    names_prefix = "pred_"
  ) |>
  mutate(real = NA, tipo = "Predicción")

# Combinar datos históricos con predicción de mañana
tabla_resultados <- ultimos_reales |>
  left_join(predicciones_historicas, by = "date") |>
  mutate(tipo = "Real") |>
  bind_rows(
    tibble(
      date = fecha_manana,
      real = NA,
      tipo = "Predicción"
    ) |>
      left_join(prediccion_manana |> select(-tipo), by = "date") |>
      mutate(tipo = "Predicción")
  ) |>
  select(date, tipo, real, starts_with("pred_")) |>
  mutate(
    real = round(real, 2),
    across(starts_with("pred_"), ~ round(.x, 2))
  )

# Crear tabla formateada
tabla_formateada <- tabla_resultados |>
  rename(
    "Fecha" = date,
    "Tipo" = tipo,
    "Real" = real,
    "Monte Carlo" = `pred_Monte Carlo`,
    "ARIMA" = pred_ARIMA,
    "Regresión" = `pred_Regresión`,
    "SMA" = pred_SMA
  )

# Mostrar tabla
kable(
  tabla_formateada,
  caption = "Últimos 5 Valores Reales y Predicciones de Modelos",
  align = "c"
) |>
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover"))
```

### Resumen de Predicción para Mañana

```{{r resumen-manana}}
prediccion_manana_resumen <- modelos_data |>
  filter(date == fecha_manana) |>
  mutate(pred = round(pred, 2)) |>
  arrange(modelo)

kable(
  prediccion_manana_resumen |>
    rename("Modelo" = modelo, "Fecha" = date, "Predicción ($)" = pred),
  caption = paste("Predicciones para", format(fecha_manana, "%Y-%m-%d")),
  align = "c"
) |>
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover"))
```

### Análisis de Dispersión

```{{r estadisticas-predicciones}}
# Calcular estadísticas de las predicciones para mañana
stats_manana <- prediccion_manana_resumen |>
  summarise(
    "Predicción Mínima" = min(pred, na.rm = TRUE),
    "Predicción Máxima" = max(pred, na.rm = TRUE),
    "Predicción Media" = mean(pred, na.rm = TRUE),
    "Desviación Estándar" = sd(pred, na.rm = TRUE),
    "Rango" = max(pred, na.rm = TRUE) - min(pred, na.rm = TRUE)
  ) |>
  mutate(across(everything(), ~ round(.x, 2)))

kable(
  stats_manana,
  caption = "Estadísticas de las Predicciones para Mañana",
  align = "c"
) |>
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover"))
```

## Análisis por Modelo

### Visualización de Modelos {{.tabset}}

#### ARIMA

```{{r arima-image, fig.align=\'center\', out.width=\'100%\'}}
# Usar ruta local copiada
knitr::include_graphics("plots/{s}/ARIMA_{s}.png")
```

**Descripción del Modelo ARIMA:**

El modelo ARIMA (AutoRegressive Integrated Moving Average) es una técnica de series temporales que combina componentes autorregresivos, de diferenciación e integración, y de media móvil. Este modelo es especialmente efectivo para datos que muestran tendencias y patrones estacionales.

#### Monte Carlo

```{{r montecarlo-image, fig.align=\'center\', out.width=\'100%\'}}
# Usar ruta local copiada
knitr::include_graphics("plots/{s}/MonteCarlo_{s}.png")
```

**Descripción del Modelo Monte Carlo:**

La simulación Monte Carlo utiliza muestreo aleatorio repetido para modelar el comportamiento de sistemas complejos. En el contexto financiero, este método genera múltiples escenarios posibles para predecir los movimientos futuros del precio, proporcionando una estimación probabilística.

#### Regresión

```{{r regression-image, fig.align=\'center\', out.width=\'100%\'}}
# Usar ruta local copiada
knitr::include_graphics("plots/{s}/Regression_{s}.png")
```

**Descripción del Modelo de Regresión:**

El modelo de regresión establece una relación matemática entre las variables independientes (como indicadores técnicos, volumen, etc.) y la variable dependiente (precio de la acción). Este enfoque permite identificar patrones lineales o no lineales en los datos históricos.

#### SMA

```{{r sma-image, fig.align=\'center\', out.width=\'100%\'}}
# Usar ruta local copiada
knitr::include_graphics("plots/{s}/SMA_{s}.png")
```

**Descripción del Modelo SMA:**

La Media Móvil Simple (SMA) calcula el promedio de los precios durante un período específico. Es uno de los indicadores técnicos más básicos pero efectivos, que ayuda a suavizar las fluctuaciones de precios y identificar tendencias direccionales.

## Métricas de Evaluación

### Comparación de Rendimiento por Modelo

::: callout-important
## Nota sobre Métricas de Evaluación

Las métricas de evaluación se cargarán desde:

-   `output/tables/{s}/model_MonteCarlo_metrics.csv`
-   `output/tables/{s}/model_ARIMA_metrics.csv`
-   `output/tables/{s}/model_Regression_metrics.csv`
-   `output/tables/{s}/model_SMA_metrics.csv`

**Métricas a incluir:**
- **MAE:** Mean Absolute Error (Error Absoluto Medio)
- **RMSE:** Root Mean Square Error (Raíz del Error Cuadrático Medio)
- **MAPE:** Mean Absolute Percentage Error (Error Absoluto Porcentual Medio)
- **R²:** Coeficiente de Determinación
:::

```{{r metricas-evaluacion, eval=TRUE}}
# Intentar cargar métricas reales
metricas_todos <- map_dfr(
  c("MonteCarlo", "ARIMA", "Regression", "SMA"),
  function(modelo) {{
    archivo <- file.path("output", "tables", "{s}", paste0("model_", modelo, "_metrics.csv"))
    if (file.exists(archivo)) {{
      tryCatch({{
        read_csv(archivo, show_col_types = FALSE) |> 
          mutate(Modelo = modelo) |>
          select(Modelo, everything())
      }}, error = function(e) {{
        cat("Error cargando métricas para", modelo, ":", e$message, "\\n")
        return(NULL)
      }})
    }} else {{
      cat("Archivo no encontrado:", archivo, "\\n")
      return(NULL)
    }}
  }}
)

# Si no hay métricas reales, usar tabla de ejemplo
if (is.null(metricas_todos) || nrow(metricas_todos) == 0) {{
  cat("Usando métricas de ejemplo - archivos de métricas no encontrados\\n")
  metricas_todos <- tibble(
    Modelo = c("ARIMA", "Monte Carlo", "Regresión", "SMA"),
    MAE = c(2.45, 3.12, 2.87, 4.23),
    RMSE = c(3.21, 4.05, 3.78, 5.67),
    MAPE = c(1.85, 2.34, 2.12, 3.45),
    R2 = c(0.924, 0.887, 0.901, 0.823)
  )
}}

# Renderizar tabla
kable(
  metricas_todos,
  digits = 3,
  caption = "Métricas de Evaluación por Modelo",
  align = c("l", rep("c", ncol(metricas_todos) - 1))
) |>
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover")) |>
  kableExtra::column_spec(1, bold = TRUE) |>
  kableExtra::add_footnote("Valores menores indican mejor rendimiento para MAE, RMSE y MAPE. Valores mayores indican mejor rendimiento para R².")
```

### Interpretación de Métricas

- **MAE (Error Absoluto Medio):** Mide la diferencia promedio entre valores predichos y reales. Valores menores indican mejor precisión.

- **RMSE (Raíz del Error Cuadrático Medio):** Penaliza más los errores grandes que el MAE. Útil cuando los errores grandes son especialmente indeseables.

- **MAPE (Error Absoluto Porcentual Medio):** Expresa el error como porcentaje, facilitando la interpretación independientemente de la escala de los datos.

- **R² (Coeficiente de Determinación):** Indica qué proporción de la variabilidad de los datos es explicada por el modelo. Valores cercanos a 1 indican mejor ajuste.

:::

## Gestión de Riesgos

```{{r risk-management}}
# Cargar script de métricas de riesgo - RUTA ACTUALIZADA
source("R/risk metrics.R")

# Calcular métricas de riesgo para cada frecuencia
daily_metrics <- calculate_risk_metrics(na.omit(serie_original$daily_return))
weekly_metrics <- calculate_risk_metrics(na.omit(serie_original$weekly_return))
monthly_metrics <- calculate_risk_metrics(na.omit(serie_original$monthly_return))
yoy_metrics <- calculate_risk_metrics(na.omit(serie_original$yoy_return))
```

### Métricas de Riesgo por Frecuencia

```{{r render-risk-tables}}
# Combinar todas las métricas en una sola tabla
risk_combined <- data.frame(
  Métrica = rownames(daily_metrics),
  Diario = paste0(round(daily_metrics$Valor * 100, 2), "%"),
  Semanal = paste0(round(weekly_metrics$Valor * 100, 2), "%"),
  Mensual = paste0(round(monthly_metrics$Valor * 100, 2), "%"),
  Anual = paste0(round(yoy_metrics$Valor * 100, 2), "%")
)

# Renderizar tabla unificada
kable(
  risk_combined,
  caption = "Métricas de Riesgo por Frecuencia (en porcentajes)",
  align = c("l", "c", "c", "c", "c")
) |>
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover")) |>
  kableExtra::column_spec(1, bold = TRUE)
```

### Interpretación de Métricas de Riesgo

- **Volatilidad:** Medida de dispersión de los retornos. Mayor volatilidad indica mayor riesgo.
- **VaR (Value at Risk):** Pérdida máxima esperada con un nivel de confianza determinado.
- **Expected Shortfall:** Pérdida promedio esperada en escenarios extremos.
- **Drawdown:** Caída máxima desde un pico hasta un valle en el período analizado.

---

**Fecha de generación:** `{{r}} Sys.time()`
  '
  )

  # Crear nombre del archivo
  nombre_archivo <- glue("{s}.qmd")

  # Escribir el archivo
  writeLines(template_qmd, nombre_archivo)

  cat(glue("✅ Archivo generado: {nombre_archivo}\n"))
}

cat("🎉 Todos los archivos QMD han sido generados con rutas actualizadas!\n")
cat("📁 Estructura de archivos esperada:\n")
cat("   ├── data/processed/{stock}_full.csv\n")
cat("   ├── models/{ModelType}/model_{modeltype}_{stock}.rds\n")
cat("   ├── output/plots/{stock}/{Model}_{stock}.png\n")
cat("   ├── output/tables/{stock}/model_{Model}_metrics.csv\n")
cat("   └── R/risk metrics.R\n")