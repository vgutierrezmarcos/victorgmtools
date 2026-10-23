# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Package Overview

**victorgmtools** is a personal R package for economic data analysis and presentation with the
visual identity of victorgutierrezmarcos.es (the author's website and blog): trade and time-series
metrics, price indices, FRED/WITS/Yahoo Finance access, and a styling layer for ggplot2, gt, maps
and Quarto reports. Documentation and function names are in Spanish.

It shares its roots with `comerciotools` (the package of the S.G. de Estudios y Prospección
Comercial, Ministerio de Economía). Only generic code is ported here, rewritten with this package's
conventions and colours; nothing that depends on the Ministry's servers (PostgreSQL/SQL Server
loaders, DATACOMEX, SGEYPC CSS) belongs in this repo, and it must stay MIT.

## Build & Development Commands

```r
devtools::load_all()     # load for interactive development
devtools::document()     # regenerate man/ and NAMESPACE (never edit NAMESPACE by hand)
devtools::test()         # testthat suite in tests/testthat
devtools::check()        # R CMD check
devtools::install()

remotes::install_github("fabiansalazares/wtor")   # required remote dependency
```

From WSL, R lives on Windows: run scripts with
`cmd.exe /c "C:\Users\vgutierrez\AppData\Local\Programs\R\R-4.5.0\bin\Rscript.exe <script>"`
(PowerShell is blocked). The project uses `devtools` (`PackageUseDevtools: Yes` in the .Rproj).

## Architecture

### Module Structure (R/)

| File | Purpose |
|------|---------|
| `funciones_calculos.R` | Core calculations: growth rates, variations, accumulation (`get_acumulado`, `get_acumulado_desde_1_enero`, `get_agregado`), moving averages, balance, coverage, weights, contributions. Foundation used by the others. |
| `funciones_precios.R` | Paasche-type weighted price indices (`get_precios_ponderados`, fixed or rolling basket, optional duckdb SQL path), real flows, price/volume decomposition and their plots. |
| `funciones_graficos.R` | Largest module. `graficos_estilo_victorgm()`, `tema_victorgm()` (both with `.oscuro`), `mapa_estilo_victorgm()` (Spain/World via sf/rnaturalearth), `tablas_estilo_victorgm()`, base colour sets, logo overlay, Google Fonts registration. |
| `funciones_paletas.R` | `tokens_victorgm()` (website design tokens, light and dark), categorical/sequential/diverging/sign palettes and all thematic palettes (`paleta_paises`, sectors SEC/CUCI, TARIC sections, CNAE, CCAA, energy, price/volume, balance of payments, chokepoints, ports, freight routes). |
| `funciones_informes.R` | Quarto report pieces with the SGEYPC vocabulary and the website look: `kpis`/`kpi`, `resumen`/`bloque`, `figura`, `figura_doble`, `tabla`, `mostrar_plt`, `iniciar_informe`, `numeracion_anexo`, `css_informe_victorgm`, plus the style rules `eje_y_derecha` and `igualar_escalas`. CSS in `inst/css/informe_victorgm.css`. |
| `funciones_formatos.R` | Spanish number formats (`es_number`, `es_label_number`, `fmt_mill`, `fmt_pct`, `fmt_ton`), figures in words (`fraccion_txt`, `veces_txt`), `filas_leyenda`. |
| `funciones_revision.R` | `revisar_orden_geoms()`: checks geom layer order in `.R`/`.qmd` files. |
| `funciones_fred.R` | FRED via `fredr`: `get_fred_df`, `get_fred_tbl`, `get_fred_plt`. |
| `funciones_tipocambio.R` | Exchange rates via `quantmod` (Yahoo Finance). |
| `funciones_comercio.R` | World Bank WITS tariffs (`get_tariffs`, `_plt`, `_tbl`), adapted from witstrainsr. |
| `funciones_strings.R`, `funciones_metadata.R` | Text utilities; Spanish province/CCAA metadata (INE codes). |

### Key Dependency Flow

```
funciones_calculos.R  <- core math, used by precios
funciones_paletas.R   <- tokens and palettes, used by graficos, precios, informes
funciones_graficos.R  <- styling layer consumed by precios, fred, tipocambio, comercio
funciones_informes.R  <- Quarto layer on top of graficos (ggiraph) and gt
```

### Conventions

- **Function naming:** exported functions use prefixes like `get_`, `crear_`, `aplicar_`, `tabla_`, `tema_`, `mapa_`, `paleta_`, `colores_`, `fmt_`; the report pieces keep the SGEYPC vocabulary (`figura`, `tabla`, `kpis`, `resumen`) for familiarity.
- **Parameter naming:** dot-prefixed (`.col_fecha`, `.col_flujo`, `.col_valores`, `.estatico`). `.col_flujo` defaults to `NULL` in the series functions and to `"flujo"` where the flow is required (`get_saldo`, `get_cobertura`, `get_contribucion`, `get_peso`).
- **Column references:** `rlang::sym()` + `!!`, or `.data[[col]]` inside ggplot aesthetics; grouping with `dplyr::across(dplyr::all_of())`.
- **Namespaces:** always fully qualified (`dplyr::filter`), native pipe `|>`.
- **Interactive vs static:** plot functions take `.estatico`; `graficos_estilo_victorgm()` returns a `girafe` when `.estatico = FALSE`, so add scales and layers *before* calling it (pass palettes through `.paleta_utilizada`).
- **Colours:** never ad-hoc hex in new code; use `tokens_victorgm()`, `colores_victorgm()` or a named palette. Semantic palettes map exports to purple, imports to gold, balance/success to green, errors to garnet.
- **Layer order rule:** `geom_bar`, `geom_col`, `geom_hline`, `geom_vline`, `geom_abline`, `geom_line`, `geom_point`, `geom_label`.
- **Documentation:** roxygen2 with `markdown = TRUE`; NAMESPACE is auto-generated.
- **Line endings:** LF (`.gitattributes`). Files saved from Windows with CRLF must be normalised before committing.

## Testing

`tests/testthat` covers calculations, prices, formats, palettes and report helpers with synthetic
data (no network). Functions that call FRED, Yahoo Finance or WITS are not tested automatically.
Ad-hoc scripts live in `pruebas/`.

## Key Dependencies

- **Data manipulation:** dplyr, tidyr, rlang, tidyselect, zoo, slider, lubridate, duckdb/duckplyr
- **Visualization:** ggplot2, scales, ggiraph, ggtext, ggfx, gt, htmltools, htmlwidgets, showtext/sysfonts
- **External APIs:** fredr (FRED), quantmod (Yahoo Finance), wtor (GitHub remote: fabiansalazares/wtor), httr/xml2 (WITS)
- **Geospatial:** sf, rnaturalearth
- **Suggested:** patchwork (`figura_doble` outside HTML), webshot2 (PNG of interactive plots), testthat
