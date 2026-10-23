# =============================================================================
# Aplicación Shiny: Distribución salarial de la EES
# Encuesta de Estructura Salarial - Visualización interactiva
# =============================================================================

# --- Datos y constantes -------------------------------------------------------

datos_completos <- shiny::getShinyOption("ees_datos")

if (is.null(datos_completos)) {
  stop("No se recibieron datos. Lanza la app con app_salarios_ees(.ruta_datos).")
}

# Paleta de colores victorgm
colores <- victorgmtools::colores_victorgm()

# Variables disponibles para comparación
variables_comparacion <- c(
  "Sexo" = "sexo",
  "Tipo de propiedad" = "tipo_propiedad",
  "Tipo de jornada" = "tipo_jornada",
  "Tipo de contrato" = "tipo_contrato",
  "Nivel de estudios" = "nivel_estudios",
  "Grupo de edad" = "grupo_edad",
  "Tamano de la unidad" = "tamano_unidad",
  "Nacionalidad" = "nacionalidad",
  "Responsabilidad" = "responsabilidad",
  "Ambito de mercado" = "ambito_mercado",
  "Regulacion laboral" = "regulacion_laboral",
  "Ocupacion" = "ocupacion",
  "Actividad economica" = "actividad_economica"
)

# Solo ofrecer variables presentes en los datos
variables_comparacion <- variables_comparacion[
  variables_comparacion %in% names(datos_completos)
]

# Etiquetas legibles para los valores de cada variable
etiquetas_valores <- list(
  sexo = c("1" = "Hombre", "6" = "Mujer"),
  tipo_propiedad = c("1" = "Publico", "2" = "Privado"),
  tipo_jornada = c("1" = "Tiempo completo", "2" = "Tiempo parcial"),
  tipo_contrato = c("1" = "Indefinido", "2" = "Determinado"),
  nacionalidad = c("1" = "Espana", "2" = "Resto del mundo"),
  responsabilidad = c("0" = "No", "1" = "Si"),
  tamano_unidad = c(
    "1" = "1-49 trabajadores", "2" = "50-199 trabajadores",
    "3" = "200+ trabajadores"
  ),
  ambito_mercado = c(
    "1" = "Local/regional", "2" = "Nacional",
    "3" = "Union Europea", "4" = "Mundial"
  ),
  nivel_estudios = c(
    "1" = "< Primaria", "2" = "Primaria", "3" = "Secundaria",
    "4" = "FP superior", "5" = "Universitarios"
  ),
  grupo_edad = c(
    "1" = "< 19", "2" = "20-29", "3" = "30-39",
    "4" = "40-49", "5" = "50-59", "6" = "60+"
  ),
  regulacion_laboral = c(
    "1" = "Estatal sectorial", "2" = "Sectorial inferior",
    "3" = "Empresa/grupo", "4" = "Centro de trabajo", "5" = "Otra"
  )
)

# Función para etiquetar valores
etiquetar <- function(.datos, .variable) {
  if (.variable %in% names(etiquetas_valores)) {
    mapa <- etiquetas_valores[[.variable]]
    vals <- as.character(.datos[[.variable]])
    etiquetados <- mapa[vals]
    # Valores sin etiqueta mantienen su código
    etiquetados[is.na(etiquetados)] <- vals[is.na(etiquetados)]
    .datos[[.variable]] <- factor(etiquetados, levels = unique(etiquetados))
  } else {
    .datos[[.variable]] <- factor(.datos[[.variable]])
  }
  .datos
}

periodos_disponibles <- sort(unique(datos_completos$periodo))

# --- UI -----------------------------------------------------------------------

ui <- bslib::page_sidebar(
  title = "Distribucion salarial - Encuesta de Estructura Salarial",
  theme = bslib::bs_theme(
    version = 5,
    primary = colores[1],
    "navbar-bg" = colores[1]
  ),

  sidebar = bslib::sidebar(
    width = 320,
    title = "Filtros",

    shiny::selectInput(
      "var_comparacion", "Variable de comparacion",
      choices = variables_comparacion,
      selected = "sexo"
    ),

    shiny::checkboxGroupInput(
      "periodos", "Periodos",
      choices = periodos_disponibles,
      selected = periodos_disponibles
    ),

    shiny::radioButtons(
      "escala", "Escala del salario",
      choices = c("Nivel (EUR/hora)" = "nivel", "Logaritmico" = "log"),
      selected = "nivel"
    ),

    shiny::checkboxInput(
      "facetar_periodo", "Facetar por periodo",
      value = FALSE
    ),

    shiny::sliderInput(
      "lim_x", "Limite eje X (solo nivel)",
      min = 0, max = 150, value = c(0, 80), step = 5
    ),

    shiny::hr(),

    shiny::tags$p(
      shiny::tags$small(
        "Fuente: Encuesta de Estructura Salarial (INE).",
        shiny::tags$br(),
        "Salario bruto por hora en octubre.",
        shiny::tags$br(),
        "Se excluyen estratos ambiguos (0, 4)."
      )
    )
  ),

  bslib::navset_card_tab(
    id = "tabs",

    bslib::nav_panel(
      "Distribucion",
      shiny::plotOutput("plot_densidad", height = "600px")
    ),

    bslib::nav_panel(
      "Comparacion",
      shiny::radioButtons(
        "tipo_comparacion", NULL,
        choices = c("Boxplot" = "boxplot", "Violin" = "violin"),
        selected = "boxplot",
        inline = TRUE
      ),
      shiny::plotOutput("plot_comparacion", height = "600px")
    ),

    bslib::nav_panel(
      "Estadisticas",
      shiny::tableOutput("tabla_estadisticas")
    ),

    bslib::nav_panel(
      "Brecha salarial",
      shiny::plotOutput("plot_brecha", height = "500px"),
      shiny::tags$hr(),
      shiny::tableOutput("tabla_brecha")
    )
  )
)

# --- Server -------------------------------------------------------------------

server <- function(input, output, session) {

  # Reactive: datos filtrados por periodo
  datos_filtrados <- shiny::reactive({
    shiny::req(input$periodos)
    periodos_sel <- as.integer(input$periodos)

    df <- datos_completos |>
      dplyr::filter(.data$periodo %in% periodos_sel)

    # Etiquetar la variable de comparación
    var <- input$var_comparacion
    df <- df |>
      dplyr::filter(!is.na(.data[[var]]), .data[[var]] != "")

    df <- etiquetar(df, var)

    df
  })

  # Reactive: columna de salario según escala
  col_salario <- shiny::reactive({
    if (input$escala == "log") "ln_salario_hora" else "salario_hora"
  })

  etiqueta_eje_y <- shiny::reactive({
    if (input$escala == "log") "ln(salario/hora)" else "Salario por hora (EUR)"
  })

  # --- Tab 1: Distribución (densidad) ---
  output$plot_densidad <- shiny::renderPlot({
    df <- datos_filtrados()
    var <- input$var_comparacion
    col_y <- col_salario()

    p <- ggplot2::ggplot(
      df,
      ggplot2::aes(x = .data[[col_y]], fill = .data[[var]], color = .data[[var]])
    ) +
      ggplot2::geom_density(alpha = 0.35, linewidth = 0.6) +
      ggplot2::scale_fill_manual(values = colores) +
      ggplot2::scale_color_manual(values = colores) +
      ggplot2::labs(
        x = etiqueta_eje_y(),
        y = "Densidad",
        fill = names(variables_comparacion[variables_comparacion == var]),
        color = names(variables_comparacion[variables_comparacion == var])
      ) +
      victorgmtools::tema_victorgm() +
      ggplot2::theme(
        axis.title.x = ggplot2::element_text(),
        axis.title.y = ggplot2::element_text(),
        legend.text = ggplot2::element_text(size = 11),
        plot.title = ggplot2::element_text(
          size = 16, face = "bold", color = colores[1]
        ),
        plot.subtitle = ggplot2::element_text(size = 12, color = "grey40")
      ) +
      ggplot2::ggtitle(
        paste("Distribucion del salario por", tolower(
          names(variables_comparacion[variables_comparacion == var])
        )),
        subtitle = paste(
          "Periodos:", paste(input$periodos, collapse = ", "),
          " | N =", scales::comma(nrow(df))
        )
      )

    if (input$escala == "nivel") {
      p <- p + ggplot2::coord_cartesian(xlim = input$lim_x)
    }

    if (input$facetar_periodo && var != "periodo") {
      p <- p + ggplot2::facet_wrap(~ periodo, scales = "free_y")
    }

    p
  }, res = 96)

  # --- Tab 2: Comparación (boxplot / violin) ---
  output$plot_comparacion <- shiny::renderPlot({
    df <- datos_filtrados()
    var <- input$var_comparacion
    col_y <- col_salario()

    p <- ggplot2::ggplot(
      df,
      ggplot2::aes(x = .data[[var]], y = .data[[col_y]], fill = .data[[var]])
    )

    if (input$tipo_comparacion == "violin") {
      p <- p +
        ggplot2::geom_violin(alpha = 0.6, linewidth = 0.4) +
        ggplot2::geom_boxplot(width = 0.15, alpha = 0.8, outlier.shape = NA)
    } else {
      p <- p +
        ggplot2::geom_boxplot(alpha = 0.6, linewidth = 0.4, outlier.alpha = 0.02)
    }

    p <- p +
      ggplot2::stat_summary(
        fun = mean, geom = "point", shape = 18, size = 3, color = colores[7]
      ) +
      ggplot2::scale_fill_manual(values = colores) +
      ggplot2::labs(
        x = names(variables_comparacion[variables_comparacion == var]),
        y = etiqueta_eje_y()
      ) +
      victorgmtools::tema_victorgm() +
      ggplot2::theme(
        axis.title.x = ggplot2::element_text(),
        axis.title.y = ggplot2::element_text(),
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
        legend.position = "none",
        plot.title = ggplot2::element_text(
          size = 16, face = "bold", color = colores[1]
        ),
        plot.subtitle = ggplot2::element_text(size = 12, color = "grey40")
      ) +
      ggplot2::ggtitle(
        paste("Salario por", tolower(
          names(variables_comparacion[variables_comparacion == var])
        )),
        subtitle = paste(
          "Periodos:", paste(input$periodos, collapse = ", "),
          " | Rombo = media"
        )
      )

    if (input$escala == "nivel") {
      p <- p + ggplot2::coord_cartesian(ylim = input$lim_x)
    }

    if (input$facetar_periodo && var != "periodo") {
      p <- p + ggplot2::facet_wrap(~ periodo)
    }

    p
  }, res = 96)

  # --- Tab 3: Estadísticas ---
  output$tabla_estadisticas <- shiny::renderTable({
    df <- datos_filtrados()
    var <- input$var_comparacion
    col_y <- col_salario()

    stats <- df |>
      dplyr::group_by(grupo = .data[[var]]) |>
      dplyr::summarise(
        N = dplyr::n(),
        Media = mean(.data[[col_y]], na.rm = TRUE),
        Mediana = stats::median(.data[[col_y]], na.rm = TRUE),
        `Desv. tipica` = stats::sd(.data[[col_y]], na.rm = TRUE),
        Q25 = stats::quantile(.data[[col_y]], 0.25, na.rm = TRUE),
        Q75 = stats::quantile(.data[[col_y]], 0.75, na.rm = TRUE),
        .groups = "drop"
      )

    col_nombre <- names(variables_comparacion[variables_comparacion == var])
    names(stats)[1] <- col_nombre

    stats$N <- scales::comma(stats$N)

    cols_num <- c("Media", "Mediana", "Desv. tipica", "Q25", "Q75")
    for (cn in cols_num) {
      stats[[cn]] <- round(stats[[cn]], 3)
    }

    stats
  }, striped = TRUE, hover = TRUE, bordered = TRUE, width = "100%")

  # --- Tab 4: Brecha salarial ---
  output$plot_brecha <- shiny::renderPlot({
    df <- datos_filtrados()
    var <- input$var_comparacion

    # Calcular media por grupo y periodo
    brecha_df <- df |>
      dplyr::group_by(.data$periodo, grupo = .data[[var]]) |>
      dplyr::summarise(
        media_salario = mean(.data$salario_hora, na.rm = TRUE),
        media_ln = mean(.data$ln_salario_hora, na.rm = TRUE),
        n = dplyr::n(),
        .groups = "drop"
      )

    col_nombre <- names(variables_comparacion[variables_comparacion == var])

    p <- ggplot2::ggplot(
      brecha_df,
      ggplot2::aes(
        x = factor(.data$periodo),
        y = .data$media_salario,
        fill = .data$grupo
      )
    ) +
      ggplot2::geom_col(position = "dodge", width = 0.7) +
      ggplot2::scale_fill_manual(values = colores) +
      ggplot2::labs(
        x = "Periodo EES",
        y = "Salario medio por hora (EUR)",
        fill = col_nombre
      ) +
      victorgmtools::tema_victorgm() +
      ggplot2::theme(
        axis.title.x = ggplot2::element_text(),
        axis.title.y = ggplot2::element_text(),
        axis.text.x = ggplot2::element_text(angle = 0, hjust = 0.5),
        legend.text = ggplot2::element_text(size = 11),
        plot.title = ggplot2::element_text(
          size = 16, face = "bold", color = colores[1]
        ),
        plot.subtitle = ggplot2::element_text(size = 12, color = "grey40")
      ) +
      ggplot2::ggtitle(
        paste("Salario medio por hora por", tolower(col_nombre), "y periodo"),
        subtitle = "Salario bruto por hora en octubre (EUR)"
      )

    p
  }, res = 96)

  output$tabla_brecha <- shiny::renderTable({
    df <- datos_filtrados()
    var <- input$var_comparacion

    brecha_df <- df |>
      dplyr::group_by(Periodo = .data$periodo, Grupo = .data[[var]]) |>
      dplyr::summarise(
        N = dplyr::n(),
        `Media (EUR/h)` = round(mean(.data$salario_hora, na.rm = TRUE), 2),
        `Media ln(w)` = round(mean(.data$ln_salario_hora, na.rm = TRUE), 4),
        .groups = "drop"
      )

    brecha_df$N <- scales::comma(brecha_df$N)

    col_nombre <- names(variables_comparacion[variables_comparacion == var])
    names(brecha_df)[2] <- col_nombre

    brecha_df
  }, striped = TRUE, hover = TRUE, bordered = TRUE, width = "100%")
}

# --- Lanzar -------------------------------------------------------------------

shiny::shinyApp(ui = ui, server = server)
