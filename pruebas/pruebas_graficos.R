

bdeseries::get_catalog() |>
  dplyr::filter(frecuencia == "ANUAL") |>
  dplyr::filter(fuente != "Banco de España") |> 
  View()


codigos_descarga <- 	
  c(
    "D_AKR620PA",
    "D_AKR620PC",
    "D_AKR620PP",
    "D_AKB07000",
    "D_AKB07300",
    "D_AKB07100",
    "D_AKB07200"
  )

datos_df <- 
  bdeseries::get_series(codes = codigos_descarga)

datos_procesados_df <- 
  datos_df |> 
  dplyr::filter(
    unidades == "Porcentaje",
    as.Date(fecha) >= as.Date("1985-01-01")
  ) |> 
  dplyr::mutate(
    fecha = as.Date(fecha), # Asegurar que fecha es Date
    valores = valores * 1e-2,
    nombres_cortos = dplyr::if_else(nchar(nombres) > 70, stringr::str_sub(nombres, 70, 300), nombres),
    nombres_muy_cortos = dplyr::if_else(nchar(nombres) > 116, stringr::str_sub(nombres, 116, 300), nombres)
  )

# Calculate the last date for positioning labels
fecha_max <- max(datos_procesados_df$fecha)

viviendas_plt <- 
  ggplot2::ggplot(
    data = datos_procesados_df,
    mapping = ggplot2::aes(
      x = fecha,
      y = valores,
      fill = nombres_muy_cortos # Changed 'color' to 'fill' for areas
    )
  ) +
  # Interactive Area Layer
  ggiraph::geom_area_interactive(
    mapping = ggplot2::aes(
      tooltip = paste0(
        "Año: ", fecha |> format("%Y"), "\n",
        nombres_cortos, ": ", scales::number_format(big.mark = ".", decimal.mark = ",", accuracy = 0.01, scale = 1e2, suffix = "%")(valores)
      ),
      data_id = nombres_muy_cortos
    ),
    alpha = 0.8, # Added transparency to see grid lines if any
    position = "stack"
  ) +
  # Text Labels Layer (positioned at the last date inside the stack)
  ggiraph::geom_text_interactive(
    data = datos_procesados_df |> dplyr::filter(fecha == fecha_max),
    mapping = ggplot2::aes(
      label = nombres_muy_cortos,
      data_id = nombres_muy_cortos
    ),
    position = ggplot2::position_stack(vjust = 0.5), # Centers text vertically in the stack
    color = "white", # White text for contrast (change to black if your palette is light)
    fontface = "bold",
    size = 3,
    check_overlap = TRUE # Prevents labels from overlapping if areas are too small
  ) +
  ggplot2::geom_hline(yintercept = 0)

viviendas_plt <-
  viviendas_plt |> 
  graficos_estilo_victorgm(
    .tipo_grafico_y = "porcentaje",
    .title = "Régimen de tenencias de viviendas en España.",
    .subtitle = "Porcentaje.",
    .caption = "Fuente: Censos de viviendas (Instituto Nacional de Estadística).",
    .fuente_letra = "Segoe UI",
    .logo_path = "pruebas/favicon-512x512.png",
    .legend_position = "none"
  )

viviendas_plt |> 
  victorgmtools::mostrar()

