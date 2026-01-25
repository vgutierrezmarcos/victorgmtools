# victorgmtools

`victorgmtools` es un paquete de R diseñado para facilitar y estandarizar tareas comunes de análisis de datos económicos, incluyendo cálculos de tasas de variación, gestión de datos de comercio exterior, obtención de datos de fuentes oficiales (FRED) y la aplicación de estilos visuales corporativos o estandarizados para gráficos y tablas.

## Instalación

Puedes instalar la versión de desarrollo de `victorgmtools` desde GitHub:

```r
# install.packages("remotes")
remotes::install_github("vgutierrezmarcos/victorgmtools")
```

### Dependencias externas

Este paquete depende del paquete `wtor` para la gestión de datos de la Organización Mundial del Comercio. Dado que no se encuentra en CRAN, debe instalarse previamente desde GitHub:

```r
# install.packages("remotes")
remotes::install_github("fabiansalazares/wtor")
```

## Funcionalidades Principales

El paquete se estructura en varios módulos, siendo los de cálculo y visualización los pilares centrales.

### 1. Cálculo y Análisis Económico (`funciones_calculos.R`)
Este módulo contiene funciones esenciales para el tratamiento de series temporales y datos económicos. Simplifica operaciones que suelen requerir múltiples pasos en `dplyr`.

*   **`get_tasa_variacion()`**: Calcula tasas de crecimiento (interanual, intermensual, trimestral, etc.) sobre una serie temporal.
*   **`get_variacion()`**: Calcula variaciones absolutas (diferencia numérica) entre periodos.
*   **`get_saldo()`**: Obtiene el saldo comercial (Exportaciones - Importaciones) a partir de datos de flujo.
*   **`get_cobertura()`**: Calcula la tasa de cobertura (Exportaciones / Importaciones * 100).
*   **`get_acumulado()`**: Genera valores acumulados sobre una ventana móvil (por ejemplo, acumulado de los últimos 12 meses o "Year-to-Date").

### 2. Visualización y Estilos (`funciones_graficos.R`)
Proporciona una capa de abstracción sobre `ggplot2` y `gt` para generar visualizaciones listas para publicación con una identidad visual coherente.

*   **`graficos_estilo_victorgm()`**: Aplica el tema visual estandarizado a objetos `ggplot`. Gestiona automáticamente:
    *   Fuentes tipográficas (por defecto "Segoe UI").
    *   Posición inteligente de la leyenda (incluyendo detección automática de huecos libres en el gráfico).
    *   Formato de ejes (miles, porcentajes, fechas, sufijos).
    *   Títulos, subtítulos y pies de página con ajuste de texto automático.
*   **`mapa_estilo_victorgm()`**: Genera mapas coropléticos estilizados.
    *   Soporte para **España** (Provincias y Comunidades Autónomas) y **Mundo**.
    *   Integra geometrías de `rnaturalearth`.
    *   *Nota*: Requiere nombres estandarizados (o códigos ISO3 para países) para una correcta vinculación.
*   **`tablas_estilo_victorgm()`**: Aplica estilos profesionales a tablas creadas con el paquete `gt`, manejando colores de fuente, bordes y formatos numéricos.
*   **`colores_victorgm()`**: Acceso directo a la paleta de colores corporativa/estándar del paquete.
*   **`to_giraph()`**: Convierte gráficos estáticos de `ggplot2` en gráficos interactivos HTML utilizando `ggiraph`.

---

## Otras Funcionalidades

### Integración con FRED (`funciones_fred.R`)
Conexión directa con la base de datos de la Reserva Federal de San Luis (FRED).
*   **`get_fred_tbl()`**: Descarga series temporales limpias y formateadas listas para análisis.
*   **`get_fred_plt()`**: Genera automáticamente gráficos estandarizados de las series descargadas.

### Análisis de Precios y Flujos Reales (`funciones_precios.R`)
Herramientas avanzadas para el análisis de comercio exterior, permitiendo descontar el efecto de los precios.
*   **`get_precios_ponderados()`**: Calcula índices de precios de exportación e importación (Índices de Valor Unitario).
*   **`get_flujos_reales()`**: Deflacta los flujos nominales para obtener series en términos de volumen (flujos reales).
*   **`get_descomposicion_tv()`**: Descompone la tasa de variación del valor nominal en la contribución de los precios y la contribución del volumen.
*   **Optimización**: Incluye versiones `_duckplyr` de estas funciones para procesar grandes volúmenes de datos (Big Data) de manera eficiente.

### Tipos de Cambio (`funciones_tipocambio.R`)
Utilidades para la conversión monetaria.
*   **`get_tipo_cambio()`**: Obtiene el tipo de cambio puntual entre dos divisas.
*   **`get_conversion()`**: Añade una columna con el valor convertido a una divisa destino en un dataframe.
*   **`get_tipo_cambio_df()`**: Descarga históricos de tipos de cambio.

### Manipulación de Textos (`funciones_strings.R`)
Funciones auxiliares para la limpieza de datos.
*   **`limpiar_nombres()`**: Estandariza nombres de columnas (snake_case, sin caracteres especiales).
*   **`quitar_tildes()`**: Elimina acentos y diacríticos de cadenas de texto.

---

## Ejemplos de Uso

### Estilizar un Gráfico
```r
library(ggplot2)
library(victorgmtools)

# Datos de ejemplo
df <- data.frame(
  fecha = seq(as.Date("2020-01-01"), by = "month", length.out = 12),
  valores = cumsum(rnorm(12))
)

# Crear gráfico base + aplicar estilo
p <- ggplot(df, aes(x = fecha, y = valores)) +
  geom_line() +
  graficos_estilo_victorgm(
    .title = "Gráfico de Ejemplo",
    .subtitle = "Serie acumulada simulada",
    .caption = "Fuente: Datos aleatorios. Elaboración propia.",
    .tipo_grafico_y = "millones" # Formatea eje Y automáticamente
  )

print(p)
```

### Calcular Tasas de Variación
```r
library(dplyr)

df_crecimiento <- df |>
  get_tasa_variacion(
    .periodos_atras = 1, 
    .col_fecha = "fecha", 
    .col_valores = "valores"
  )
```

## Licencia

MIT
