# victorgmtools 0.1.0

*   **Initial Release**: First public release of the `victorgmtools` package.

## Features

*   **Calculations**: Added functions for common economic metrics including trade balance (`get_saldo`), coverage rates (`get_cobertura`), and growth rates (`get_tasa_variacion`, `get_acumulado`).
*   **FRED Integration**: Implemented tools to directly fetch and visualize data from the Federal Reserve Economic Data (FRED) (`get_fred_tbl`, `get_fred_plt`).
*   **Visualization & Styling**: 
    *   `graficos_estilo_victorgm()`: Standardized theme for `ggplot2`.
    *   `tablas_estilo_victorgm()`: Standardized styling for `gt` tables.
    *   `mapa_estilo_victorgm()`: Tools for creating stylized choropleth maps for Spain (Provinces/CCAA) and the World.
    *   `colores_victorgm()`: Custom color palettes.
*   **Trade Analysis**: Added advanced functions for decomposing trade values (`get_descomposicion_tv`) and calculating real flows (`get_flujos_reales`), including optimized versions using `duckplyr` for large datasets.
*   **Utilities**: String cleaning (`limpiar_nombres`, `quitar_tildes`) and exchange rate conversion tools.

## Changes

*   Removed dependency on `datacomexr`.
*   Integrated `wtor` (via Remotes) for World Trade Organization data.
*   Added internal metadata for Spanish regional geography (`get_provincias_metadata`).
