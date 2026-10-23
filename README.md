# victorgmtools

`victorgmtools` es un paquete de R para el análisis de datos económicos y su presentación con la
estética de [victorgutierrezmarcos.es](https://victorgutierrezmarcos.es): cálculos sobre series
(tasas de variación, acumulados, saldos, contribuciones), índices de precios y descomposición
precio-volumen, acceso a FRED, WITS y tipos de cambio, y una capa de estilo para gráficos
`ggplot2`, tablas `gt`, mapas e informes Quarto.

Es un paquete personal, pensado para el blog y los análisis de la web y para trabajos sueltos.
Comparte raíz con `comerciotools`, el paquete de la S.G. de Estudios y Prospección Comercial,
del que toma lo genérico; nada de lo que depende de los servidores del Ministerio está aquí.

## Instalación

```r
# install.packages("remotes")
remotes::install_github("fabiansalazares/wtor")   # dependencia fuera de CRAN
remotes::install_github("vgutierrezmarcos/victorgmtools")
```

## Módulos

| Fichero | Qué hay |
|---|---|
| `funciones_calculos.R` | `get_tasa_variacion()`, `get_variacion()`, `get_acumulado()`, `get_acumulado_desde_1_enero()`, `get_agregado()`, `get_media_movil()`, `get_saldo()`, `get_cobertura()`, `get_peso()`, `get_contribucion()`, `get_contribucion_acumulada()`. |
| `funciones_precios.R` | `get_precios_ponderados()` (índices tipo Paasche, cesta fija o móvil, opción duckdb), `get_flujos_reales()`, `get_descomposicion_tv()` y sus gráficos (`get_rri_plt()`, `get_flujos_reales_plt()`, `get_descomposicion_tv_plt()`, `get_precio_producto_plt()`). |
| `funciones_graficos.R` | `graficos_estilo_victorgm()`, `tema_victorgm()`, `mapa_estilo_victorgm()`, `tablas_estilo_victorgm()`, `colores_victorgm()`. |
| `funciones_paletas.R` | `tokens_victorgm()`, paletas secuencial, divergente y de signo, y paletas temáticas (`paleta_paises()`, `paleta_exportaciones_importaciones()`, `paleta_sectores_sec()`, `paleta_ccaa()`...). |
| `funciones_informes.R` | Piezas de informe Quarto: `kpis()`, `resumen()`, `figura()`, `figura_doble()`, `tabla()`, `mostrar_plt()`, `css_informe_victorgm()`, `eje_y_derecha()`, `igualar_escalas()`. |
| `funciones_formatos.R` | `es_number()`, `fmt_mill()`, `fmt_pct()`, `fmt_ton()`, `fraccion_txt()`, `veces_txt()`, `filas_leyenda()`. |
| `funciones_revision.R` | `revisar_orden_geoms()`: comprueba el orden de las capas de los gráficos en `.R` y `.qmd`. |
| `funciones_fred.R` | `get_fred_df()`, `get_fred_tbl()`, `get_fred_plt()` (requiere clave de API de FRED). |
| `funciones_tipocambio.R` | `get_tipo_cambio()`, `get_tipo_cambio_df()`, `get_tipo_cambio_plt()`, `get_conversion()` (Yahoo Finance). |
| `funciones_comercio.R` | Aranceles del Banco Mundial (WITS): `get_tariffs()`, `get_tariffs_plt()`, `get_tariffs_tbl()`. |
| `funciones_strings.R`, `funciones_metadata.R` | `limpiar_nombres()`, `quitar_tildes()`, `get_provincias_metadata()`. |

## Estilo

El sistema de diseño es el de la web: morado `#5F2987` como tinta, verde salvia `#E2EFD9` como
papel y oro `#B8860B` como filete, con Source Sans 3 como tipografía de interfaz. `tokens_victorgm()`
expone esos tokens (y la variante oscura, berenjena) para que gráficos, tablas e informes los
compartan; `graficos_estilo_victorgm(.oscuro = TRUE)` y `css_informe_victorgm()` cubren el tema oscuro.

Reglas que el paquete ayuda a cumplir:

- un gráfico, una idea: el rótulo de `figura()` dice el mensaje y el subtítulo qué se mide;
- en series temporales el eje de valores va a la derecha (`eje_y_derecha()`, automático en `figura()`);
- dos gráficos comparables comparten escala (`figura_doble()`);
- las capas se añaden en el orden `geom_bar`, `geom_col`, `geom_hline`, `geom_vline`, `geom_abline`,
  `geom_line`, `geom_point`, `geom_label` (`revisar_orden_geoms()`);
- leyendas equilibradas (`filas_leyenda()`), paletas con nombre para que cada país o sector tenga
  siempre el mismo color, y gris para el contexto.

## Ejemplos

### Estilizar un gráfico

```r
library(ggplot2)
library(victorgmtools)

df <- data.frame(
  fecha = seq(as.Date("2020-01-01"), by = "month", length.out = 36),
  valores = cumsum(rnorm(36))
)

ggplot(df, aes(x = fecha, y = valores)) +
  geom_line() |>
  graficos_estilo_victorgm(
    .title = "Serie acumulada simulada",
    .subtitle = "Índice",
    .caption = "Fuente: datos aleatorios. Elaboración propia.",
    .tipo_grafico_y = "suffixing",
    .estatico = TRUE
  )
```

### Tasas, acumulados y agregados

```r
df |>
  get_tasa_variacion(.periodos_atras = 12) |>
  get_acumulado(.periodos_atras = 12) |>
  get_agregado(.periodos = 3, .transformacion = "media", .alineacion = "natural")
```

### Precio y volumen

```r
datos |>                                      # fecha, flujo, taric, valor_estadistico, peso
  get_precios_ponderados(.año_referencia = 2023) |>
  get_rri_plt(.title = "Relación real de intercambio")

datos |>
  get_flujos_reales(.año_referencia = 2023, .año_base = 2023) |>
  get_descomposicion_tv(.meses_atras = 12) |>
  get_descomposicion_tv_plt(.flujo_seleccionado = "E")
```

### Un artículo del blog en Quarto

```r
# chunk de configuración
victorgmtools::iniciar_informe()
victorgmtools::css_informe_victorgm()        # results: asis

# cifras y resumen
kpis(
  kpi("Exportaciones", fmt_mill(324500e6), fmt_pct(5.3, signo = TRUE), signo = 5.3, medida = "12 m"),
  kpi("Cobertura", fmt_pct(85.4), fmt_pct(-1.2, signo = TRUE), signo = -1.2)
)

# un gráfico numerado con rótulo y fuente
p <- ggplot(df, aes(fecha, valores)) + geom_line(colour = tokens_victorgm()[["primario"]])
figura(p, rotulo = "La serie crece desde 2022", subtitulo = "Índice, base 100", fuente = "INE")
```

## Desarrollo

```r
devtools::load_all()
devtools::document()
devtools::test()
devtools::check()
```

El paquete se publica con licencia MIT.
