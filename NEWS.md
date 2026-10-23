# victorgmtools 0.2.0

Incorpora a victorgmtools lo genérico de `comerciotools` (el paquete de la SGEYPC) y las ideas
de formato de informes de `sgeypc_comun`, adaptado todo a la estética de victorgutierrezmarcos.es
(morado como tinta, verde salvia como papel, oro como filete, Source Sans 3).

## Cálculos

*   `get_saldo()` filtra antes los flujos distintos de exportación e importación y avisa con
    `warning()` (no `print()`) cuando hay exportaciones negativas.
*   `get_contribucion_acumulada()` calcula el total por fecha con una agregación explícita en
    lugar de leer la observación `periodos_atras + 1`, que fallaba con varios grupos.
*   Nueva `get_agregado()`: agrega las observaciones en bloques de `n` periodos (suma, media,
    mediana, máximo, mínimo o una función propia), alineados a la última observación o al
    calendario (trimestres naturales, semestres...).

## Precios y volúmenes (recuperado)

*   Vuelve `funciones_precios.R`: `get_precios_ponderados()` (índices tipo Paasche con cesta fija
    o móvil, también en duckdb con `.metodo = "SQL"`), `get_flujos_reales()`,
    `get_descomposicion_tv()` y los gráficos `get_precio_producto_plt()`, `get_rri_plt()`,
    `get_flujos_reales_plt()` y `get_descomposicion_tv_plt()`, que ahora usan
    `graficos_estilo_victorgm()`, el argumento `.estatico` y las paletas del paquete.
*   Corregido `get_flujos_reales()` con `.metodo = NULL` (fallaba al comparar con `"SQL"`).

## FRED

*   Nueva `get_fred_df()`. `get_fred_tbl()` usa `tablas_estilo_victorgm()` y `get_fred_plt()`
    se reescribe (antes fallaba al sumar escalas a un `girafe`); acepta `.title`, `.subtitle`,
    `.caption`, `.color` y `.estatico` (`.estatico_toggle` queda obsoleto).

## Formatos e informes en Quarto

*   Formato numérico español: `es_number()`, `es_label_number()`, `fmt_mill()`, `fmt_pct()`
    (con `signo = TRUE` para `+5,3 %`) y `fmt_ton()`.
*   Cifras en palabras: `fraccion_txt()` y `veces_txt()`.
*   Piezas de informe con el vocabulario de la SGEYPC y el CSS de la web
    (`css_informe_victorgm()`, con tema claro y oscuro): `kpis()`/`kpi()`, `resumen()`/`bloque()`,
    `figura()`, `figura_doble()` (misma escala en dos gráficos comparables), `tabla()`,
    `mostrar_plt()`, `iniciar_informe()` y `numeracion_anexo()`.
*   Reglas automáticas de estilo: `eje_y_derecha()` (series temporales) e `igualar_escalas()`.
*   `filas_leyenda()` calcula filas de leyenda equilibradas y `revisar_orden_geoms()` comprueba
    el orden de las capas (`geom_bar`, `geom_col`, `geom_hline`, `geom_vline`, `geom_abline`,
    `geom_line`, `geom_point`, `geom_label`) en ficheros `.R` y `.qmd`.

## Paletas y tema

*   `tokens_victorgm()` devuelve los tokens de color de la web (claro y oscuro);
    `colores_victorgm_categoricos()`, `paleta_secuencial_victorgm()`,
    `paleta_divergente_victorgm()` y `paleta_signo_victorgm()`.
*   Paletas temáticas con los colores de victorgm: `paleta_exportaciones_importaciones()`,
    `paleta_energeticos()`, `paleta_subsectores_energeticos_sec()` y `_cuci()`,
    `paleta_efecto_precio_volumen()`, `paleta_balanza_de_pagos()`, `paleta_sectores_sec()`,
    `paleta_sectores_cuci()`, `paleta_secciones_taric()`, `paleta_actividades_economicas()`,
    `paleta_modos_suministro()`, `paleta_ccaa()`, `paleta_chokepoints()`, `paleta_puertos()` y
    `paleta_rutas_flete()`.
*   `paleta_paises()` pasa de 110 a 270 claves (ISO2, continentes, áreas, bloques económicos,
    acuerdos de la UE y más países) y se traslada a `funciones_paletas.R`.
*   `tema_victorgm()` y `graficos_estilo_victorgm()` admiten `.oscuro = TRUE` para páginas con
    `data-theme="dark"`.

## Otros

*   `.col_flujo` sigue siendo `NULL` por defecto en las funciones de series (`get_tasa_variacion()`,
    `get_acumulado()`...) y `"flujo"` en las que lo necesitan (`get_saldo()`, `get_cobertura()`,
    `get_contribucion()`, `get_peso()`).
*   Primera suite de tests con testthat (cálculos, precios, formatos, paletas e informes).
*   `.gitattributes` fija finales de línea LF. Versión 0.2.0, `R >= 4.1.0`.

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
