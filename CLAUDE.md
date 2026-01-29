# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Package Overview

**victorgmtools** is an R package for economic data analysis — trade metrics, price indices, FRED/Yahoo Finance integration, and standardized visualization. Documentation and function names are in Spanish.

## Build & Development Commands

```r
# Load all functions for interactive development
devtools::load_all()

# Regenerate documentation (roxygen2 → man/ and NAMESPACE)
devtools::document()

# Full package check (R CMD check)
devtools::check()

# Install locally
devtools::install()

# Required remote dependency (must install first)
remotes::install_github("fabiansalazares/wtor")
```

The project uses `devtools` (configured in victorgmtools.Rproj with `PackageUseDevtools: Yes`). There is no test suite or CI/CD pipeline.

## Architecture

### Module Structure (R/)

| File | Purpose |
|------|---------|
| `funciones_calculos.R` | Core economic calculations: growth rates, accumulation, moving averages, contributions. Foundation module used by others. |
| `funciones_precios.R` | Weighted price indices (Laspeyres-type), real flows (deflation), price/volume decomposition. Has `_duckplyr` variants for large datasets. |
| `funciones_comercio.R` | Trade data access: country/product metadata, tariffs from World Bank WITS, data availability. |
| `funciones_fred.R` | FRED (Federal Reserve) time series download and visualization via `fredr`. |
| `funciones_tipocambio.R` | Exchange rates from Yahoo Finance via `quantmod`. |
| `funciones_graficos.R` | Largest module. ggplot2 themes, color palettes (`colores_victorgm()`), gt table styling, choropleth maps (Spain/World via `sf`/`rnaturalearth`), interactive plots via `ggiraph`. |
| `funciones_strings.R` | Text utilities: snake_case cleaning, diacritics removal. |
| `funciones_metadata.R` | Hardcoded Spanish province/autonomous community metadata (INE codes). |

### Key Dependency Flow

```
funciones_calculos.R  ← core math, used by precios and graficos
funciones_precios.R   ← uses calculos, has duckdb-optimized variants
funciones_graficos.R  ← styling layer consumed by fred, tipocambio, comercio, precios
funciones_strings.R   ← independent utility
funciones_metadata.R  ← independent lookup data
```

### Conventions

- **Function naming:** exported functions use prefixes like `get_`, `crear_`, `aplicar_`, `tabla_`, `tema_`, `mapa_`, `to_`, `colores_`
- **Parameter naming:** dot-prefixed (e.g., `.col_fecha`, `.col_flujo`, `.col_valores`)
- **Column references:** use `rlang::sym()` + `!!` quasiquotation for flexible column specification
- **Performance variants:** `_duckplyr` suffix indicates duckdb/SQL-optimized version for large datasets
- **Interactive vs static:** plot functions accept `.estatico` parameter to toggle between `ggiraph` and static `ggplot2` output
- **Documentation:** roxygen2 with `markdown = TRUE`; NAMESPACE is auto-generated — never edit it manually

## Key Dependencies

- **Data manipulation:** dplyr, tidyr, rlang, tidyselect, zoo, slider, lubridate
- **Visualization:** ggplot2, scales, ggiraph, ggtext, ggfx, gt
- **External APIs:** fredr (FRED), quantmod (Yahoo Finance), wtor (WTO — GitHub remote: fabiansalazares/wtor)
- **Geospatial:** sf, rnaturalearth
- **Performance:** duckdb, DBI, duckplyr
