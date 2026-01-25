# victorgmtools

`victorgmtools` is an R package designed to streamline common data analysis tasks, including economic calculations, data retrieval from FRED, exchange rate conversions, and standardized visualization styling.

## Installation

You can install the development version of `victorgmtools` from GitHub (assuming it is hosted there) or locally:

```r
# install.packages("devtools")
devtools::install_github("your_username/victorgmtools")
```

### Dependencies

This package depends on `wtor`, which is available on GitHub. You must install it using:

```r
# install.packages("remotes")
remotes::install_github("fabiansalazares/wtor")
```

## Features

### 1. Calculations (`funciones_calculos.R`)
Perform common economic calculations with ease:
*   `get_saldo()`: Calculate trade balances (Exports - Imports).
*   `get_cobertura()`: Calculate coverage ratios (Exports / Imports).
*   `get_tasa_variacion()`: Compute growth rates (Year-over-Year, Month-over-Month, etc.).
*   `get_variacion()`: Compute absolute variations.
*   `get_acumulado()`: Calculate accumulated values over a rolling window (e.g., trailing 12 months).

### 2. FRED Data Integration (`funciones_fred.R`)
Directly access and visualize data from the Federal Reserve Economic Data (FRED):
*   `get_fred_tbl()`: Retrieve time series data in a tidy format.
*   `get_fred_plt()`: Generate standardized plots for FRED series.

### 3. Graphics and Styling (`funciones_graficos.R`)
Apply a consistent and professional style to your `ggplot2` visualizations and `gt` tables:
*   `graficos_estilo_victorgm()`: Apply the package's standardized theme to ggplots.
*   `mapa_estilo_victorgm()`: Style choropleth maps. **Note:** Requires standard names (e.g., matching `rnaturalearth`) for regions/provinces as automatic code-to-name mapping has been simplified.
*   `tablas_estilo_victorgm()`: Style `gt` tables.
*   `colores_victorgm()`: Access the custom color palette.
*   `to_giraph()`: Convert static plots to interactive ones using `ggiraph`.

### 4. Price and Flow Analysis (`funciones_precios.R`)
Tools for trade data analysis:
*   `get_precios_ponderados()`: Calculate weighted price indices.
*   `get_flujos_reales()`: Convert nominal trade flows to real terms.
*   `get_descomposicion_tv()`: Decompose growth rates into price and volume contributions.
*   **Optimized versions**: Includes `_duckplyr` versions for high-performance computing on large datasets.

### 5. String Manipulation (`funciones_strings.R`)
Helper functions for cleaning text data:
*   `limpiar_nombres()`: Standardize column names (snake_case, no special characters).
*   `quitar_tildes()`: Remove accents and diacritics from strings.

### 6. Exchange Rates (`funciones_tipocambio.R`)
Utilities for currency conversion:
*   `get_tipo_cambio()`: Get the exchange rate between two currencies for a specific date.
*   `get_conversion()`: Add a converted value column to a dataframe.
*   `get_tipo_cambio_df()`: Get a historical dataframe of exchange rates.

## Examples

### Styling a Plot
```r
library(ggplot2)
library(victorgmtools)

df <- data.frame(
  fecha = seq(as.Date("2020-01-01"), by = "month", length.out = 12),
  valores = cumsum(rnorm(12))
)

p <- ggplot(df, aes(x = fecha, y = valores)) +
  geom_line() +
  graficos_estilo_victorgm(
    .title = "Sample Plot",
    .subtitle = "Cumulative Sum",
    .caption = "Source: Random Data"
  )

print(p)
```

### Calculating Growth Rates
```r
library(dplyr)

df_growth <- df |>
  get_tasa_variacion(
    .periodos_atras = 1, 
    .col_fecha = "fecha", 
    .col_valores = "valores"
  )
```

## License

MIT
