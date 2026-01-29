# Registro de fuentes Google para ggplot2 ----

# Mapa interno de nombres de fuente a su nombre en Google Fonts
.fuentes_google <- c(
  "Source Sans 3"  = "Source Sans 3",
  "Source Sans Pro" = "Source Sans Pro",
  "Open Sans"      = "Open Sans",
  "Lato"           = "Lato",
  "Roboto"         = "Roboto"
)

#' Registrar fuente de Google Fonts para su uso en gráficos
#'
#' Descarga y registra una fuente de Google Fonts mediante `sysfonts` y activa
#' `showtext` para que ggplot2 pueda utilizarla. Se ejecuta de forma silenciosa
#' y solo descarga la fuente la primera vez.
#'
#' @param .fuente_letra Cadena de caracteres. Nombre de la fuente a registrar.
#' @return Lógico. `TRUE` si la fuente se registró correctamente o ya estaba disponible, `FALSE` en caso contrario.
#' @keywords internal
registrar_fuente <- function(.fuente_letra) {
  # Si la fuente ya esta registrada en sysfonts, no hacer nada
  if (.fuente_letra %in% sysfonts::font_families()) {
    showtext::showtext_auto()
    return(TRUE)
  }

  # Intentar registrar desde Google Fonts
  nombre_google <- .fuentes_google[.fuente_letra]
  if (is.na(nombre_google)) nombre_google <- .fuente_letra

  tryCatch({
    sysfonts::font_add_google(nombre_google, .fuente_letra)
    showtext::showtext_auto()
    return(TRUE)
  }, error = function(e) {
    # Si falla la descarga, avisar sin interrumpir
    message(
      "No se pudo descargar la fuente '", .fuente_letra,
      "' de Google Fonts. Se intentar\u00e1 usar la fuente del sistema o un fallback."
    )
    return(FALSE)
  })
}

#Colores SG estudios ----
#' Paleta de colores principal de victorgm
#'
#' Devuelve un vector de caracteres con los códigos hexadecimales de la paleta de colores principal.
#'
#' @return Un vector de caracteres con códigos de colores hexadecimales.
#' @export
colores_victorgm <- function() {
  c(
    "#5F2987", "#B8860B", "#4A9079",
    "#9B6BC6", "#D4A843", "#6BB39E",
    "#E31A1C", "#2C3E50", "#75A3FF",
    "#FF7F00", "#A63A50", "#7B3FAF",
    "#C9952E", "#3D7A64", "#C490E4",
    "#E8C36A", "#8FD4BB", "#843C0C",
    "#FFC000", "#46AF4B", "#A5A5A5",
    "#F8CBAD", "#ED7D31", "#421E06",
    "#800000", "#d00000", "#cfd4fc",
    "#576af4", "#081482", "#777777"
  )
}

#Colores SG estudios paired ----
#' Paleta de colores "paired" de victorgm
#'
#' Devuelve un vector de caracteres con los códigos hexadecimales de la paleta de colores "paired" (pares de colores relacionados).
#'
#' @return Un vector de caracteres con códigos de colores hexadecimales.
#' @export
colores_victorgm_paired <- function() {
  c(
    "#5F2987", "#9B6BC6",
    "#B8860B", "#D4A843",
    "#4A9079", "#6BB39E",
    "#E31A1C", "#F4A2A3",
    "#2C3E50", "#75A3FF",
    "#FF7F00", "#E8C36A"
  )
}

#' Paleta de colores alternativos de victorgm
#'
#' Devuelve un vector de caracteres con los códigos hexadecimales de una paleta de colores alternativa.
#'
#' @return Un vector de caracteres con códigos de colores hexadecimales.
#' @export
colores_alternativos <- function() {
  c(
    "#7B3FAF", "#C9952E", "#3D7A64",
    "#C490E4", "#E8C36A", "#8FD4BB",
    "#D9B8F0", "#F0D98A", "#B3E5D3",
    "#A63A50", "#75A3FF", "#2C3E50"
  )
}

#' Paleta de colores para web de victorgm
#'
#' Devuelve un vector de 16 colores optimizados para publicaciones en el blog.
#' Organizados en 5 niveles tonales de púrpura/dorado/verde más un neutro oscuro.
#'
#' @return Un vector de caracteres con 16 códigos de colores hexadecimales.
#' @export
colores_web <- function() {
  c(
    "#5F2987", "#7B3FAF", "#9B6BC6", "#C490E4", "#D9B8F0",
    "#B8860B", "#C9952E", "#D4A843", "#E8C36A", "#F0D98A",
    "#4A9079", "#3D7A64", "#6BB39E", "#8FD4BB", "#B3E5D3",
    "#2C3E50"
  )
}

# Paletas de colores categóricas para VGM ----
#' @return Vector nombrado de caracteres con códigos de color hexadecimales.
#' @examples
#' paleta_paises()
#' # Obtener color para España
#' paleta_paises()["España"]
#' @export
paleta_paises <- function() {
  c(
    "España" = "#E31A1C",
    "ESP" = "#E31A1C",
    "ES" = "#E31A1C",
    "UE" = "#004FEE",
    "UE27" = "#004FEE",
    "UE-27" = "#004FEE",
    "Zona euro" = "#004FEE",
    "Área del euro" = "#004FEE",
    "Francia" = "#A6CEE3",
    "FRA" = "#A6CEE3",
    "FR" = "#A6CEE3",
    "Alemania" = "#FF7F00",
    "DEU" = "#FF7F00",
    "DE" = "#FF7F00",
    "Portugal" = "#18BC9C",
    "PRT" = "#18BC9C",
    "PT" = "#18BC9C",
    "Italia" = "#B2DF8A",
    "ITA" = "#B2DF8A",
    "IT" = "#B2DF8A",
    "Países Bajos" = "#FFF5BB",
    "NLD" = "#FFF5BB",
    "NL" = "#FFF5BB",
    "Bélgica" = "#FDBF6F",
    "BEL" = "#FDBF6F",
    "BE" = "#FDBF6F",
    "Irlanda" = "#51E9CB",
    "IRL" = "#51E9CB",
    "IE" = "#51E9CB",
    "Reino Unido" = "#CCBE93",
    "GBR" = "#CCBE93",
    "Estados Unidos" = "#6A3D9A",
    "EE.UU." = "#6A3D9A",
    "EEUU" = "#6A3D9A",
    "USA" = "#6A3D9A",
    "US" = "#6A3D9A",
    "China" = "#F48383",
    "CHN" = "#F48383",
    "México" = "#46AF4B",
    "MEX" = "#46AF4B",
    "Argentina" = "#7BA1D8",
    "ARG" = "#7BA1D8",
    "Brasil" = "#2C3E50",
    "BRA" = "#2C3E50",
    "Canada" = "#CAB2D6",
    "Canadá" = "#CAB2D6",
    "CAN" = "#CAB2D6",
    "Japón" = "#843C0C",
    "JPN" = "#843C0C",
    # Nórdicos (Azules oscuros y Rosas fríos)
    "Suecia" = "#08519C", "SWE" = "#08519C", "SE" = "#08519C",      # Azul fuerte distinto a UE
    "Dinamarca" = "#FB9A99", "DNK" = "#FB9A99", "DK" = "#FB9A99",   # Rosa pastel
    "Finlandia" = "#00BFFF", "FIN" = "#00BFFF", "FI" = "#00BFFF",   # Deep Sky Blue
    
    # Europa Central y del Este (Magentas, Púrpuras y Amarillos oscuros)
    "Polonia" = "#E7298A", "POL" = "#E7298A", "PL" = "#E7298A",     # Magenta oscuro
    "Austria" = "#D95F02", "AUT" = "#D95F02", "AT" = "#D95F02",     # Rojo anaranjado oscuro (Dark Orange)
    "Hungría" = "#006400", "HUN" = "#006400", "HU" = "#006400",     # Verde oscuro (Dark Green)
    "República Checa" = "#7570B3", "CZE" = "#7570B3", "CZ" = "#7570B3", # Púrpura azulado
    "Eslovaquia" = "#8DD3C7", "SVK" = "#8DD3C7", "SK" = "#8DD3C7",  # Verde agua pálido
    
    # Balcanes y Sureste (Azules marinos, Limas y Dorados)
    "Grecia" = "#08306B", "GRC" = "#08306B", "GR" = "#08306B",      # Azul marino muy oscuro
    "Rumanía" = "#E6AB02", "ROU" = "#E6AB02", "RO" = "#E6AB02",     # Mostaza/Dorado
    "Bulgaria" = "#66A61E", "BGR" = "#66A61E", "BG" = "#66A61E",    # Verde oliva
    "Croacia" = "#C51B7D", "HRV" = "#C51B7D", "HR" = "#C51B7D",     # Rosa fucsia oscuro
    "Eslovenia" = "#B3DE69", "SVN" = "#B3DE69", "SI" = "#B3DE69",   # Verde lima claro
    
    # Bálticos (Grises azulados y Violetas)
    "Lituania" = "#FDB462", "LTU" = "#FDB462", "LT" = "#FDB462",    # Amarillo anaranjado suave
    "Letonia" = "#800080", "LVA" = "#800080", "LV" = "#800080",     # Púrpura puro
    "Estonia" = "#708090", "EST" = "#708090", "EE" = "#708090",     # Slate Gray
    
    # Pequeños / Islas
    "Chipre" = "#D9D9D9", "CYP" = "#D9D9D9", "CY" = "#D9D9D9",      # Gris claro
    "Luxemburgo" = "#BC80BD", "LUX" = "#BC80BD", "LU" = "#BC80BD",  # Lavanda
    "Malta" = "#FCCDE5", "MLT" = "#FCCDE5", "MT" = "#FCCDE5",       # Rosa muy pálido
    
    # Extra UE solicitados
    "Suiza" = "#525252", "CHE" = "#525252", "CH" = "#525252",       # Gris oscuro neutro (para no competir con rojos)
    "Turquía" = "#17BECF", "TUR" = "#17BECF", "TR" = "#17BECF",     # Cian oscuro/Turquesa
    "Marruecos" = "#8C564B", "MAR" = "#8C564B", "MA" = "#8C564B",    # Marrón castaña
    
    "Resto del mundo" = "#9DA9AA",
    "RdM" = "#9DA9AA",
    "RoW" = "#9DA9AA",
    "Otros" = "#9DA9AA",
    "Resto" = "#9DA9AA"
  )
}

# Función para aplicar paleta ----

#' Fijar estilo de gráfico estándar para VictorGM, con algunos parámetros opcionales
#'
#' Aplica la paleta de colores de victorgm a un gráfico ggplot.
#'
#' @param .x Un objeto ggplot al que se quiere aplicar la paleta.
#' @param .n_colors Número entero opcional. Si se especifica, interpola la paleta para generar el número de colores indicado. Por defecto es `NULL`, usando la paleta estándar.
#'
#' @return El objeto ggplot modificado con la escala de colores aplicada.
#' @export
aplicar_paleta_victorgm <- function(.x, .n_colors = NULL) {
  colores_victorgm <- victorgmtools::colores_victorgm()

  palette(colores_victorgm)

  pal <- palette(colores_victorgm)


  .plot_to_return_plt <-
    .x +
    ggplot2::scale_color_manual(values = colores_victorgm, aesthetics = c("colour", "fill"))

  if(!is.null(.n_colors)){
    .plot_to_return_plt <-
      .x +
      ggplot2::scale_color_manual(values = grDevices::colorRampPalette(pal)(.n_colors), aesthetics = c("colour", "fill"))
  }

  return(.plot_to_return_plt)
}

#Tema VGM ----
#' Tema ggplot2 de victorgm
#'
#' Aplica el tema visual estándar de victorgm a un gráfico ggplot.
#' Este tema se caracteriza por un fondo transparente, líneas de cuadrícula horizontales punteadas, y la ausencia de líneas verticales y bordes de ejes innecesarios.
#'
#' @return Un objeto theme de ggplot2.
#' @export
tema_victorgm <- function() {
    ggplot2::theme(
    axis.title.x = ggplot2::element_blank(),
    axis.title.y = ggplot2::element_blank(),
    axis.text.x = ggplot2::element_text(hjust = 1, angle = 45),
    axis.ticks = ggplot2::element_blank(),
    legend.key = ggplot2::element_rect(fill = NA, colour = NA),
    legend.title = ggplot2::element_blank(),
    legend.position = "bottom",
    panel.background = ggplot2::element_rect(fill = "transparent"),
    panel.grid.major = ggplot2::element_line(colour = "#D9D9D9", lineend = "round", linewidth = 0.75, linetype = "dotted", arrow = FALSE),
    panel.grid.major.x = ggplot2::element_blank(),
    panel.grid.minor.x = ggplot2::element_blank(),
    plot.background = ggplot2::element_rect(fill = "transparent", colour = NA),
    strip.background = ggplot2::element_rect(fill = "white")
  )
}

# Función general para dar formato a gráficos ----

#' Aplicar estilo completo VictorGM a un gráfico
#'
#' Función principal para aplicar el estilo corporativo completo de VictorGM a gráficos ggplot2.
#' Incluye configuración de escalas, ejes, leyendas, títulos, paletas de colores y fuentes tipográficas.
#'
#' @param .x Objeto ggplot2 al que aplicar el estilo.
#' @param .tipo_grafico_x Cadena de caracteres. Indica si se trata de una serie temporal o no. Puede tomar valores `"fecha"` o `"nofecha"`. Por defecto, `"fecha"`.
#' @param .tipo_grafico_y Cadena de caracteres. Sirve para indicar en ciertos casos la escala del eje y. Puede tomar valores `"millones"`, `"suffixing"`, `"porcentaje"` o `NULL`. Si es distinto a `NULL`, el resto de argumentos no se tienen en cuenta en la medida en que colisionen con los definidos para el tipo de gráfico. También permite la opción `"discreto"`, por si el eje Y no es numérico. Por defecto, `NULL`.
#' @param .scale Número. Factor por el que dividir los números del eje. Por defecto, `1`. Para millones, p.ej., habría que fijar a 1e+6 o 1000000.
#' @param .accuracy Número. Número de decimales a mostrar en eje. Por defecto, `1`. Para mostrar 0 decimales, fijar a 1; para 1 decimal, fijar a 0.1; para 2, a 0.01 y así...
#' @param .suffix Cadena de caracteres. Por defecto, cadena vacía.
#' @param .minimo_eje_x Número o fecha. Valor mínimo en el eje x. Para `.tipo_grafico_x = "fecha"`, debe ser una fecha. Para `.tipo_grafico_x = "nofecha"` con variable numérica, debe ser un número. Por defecto, `NULL` y el mínimo se adapta al gráfico.
#' @param .maximo_eje_x Número o fecha. Valor máximo en el eje x. Para `.tipo_grafico_x = "fecha"`, debe ser una fecha. Para `.tipo_grafico_x = "nofecha"` con variable numérica, debe ser un número. Por defecto, `NULL` y el máximo se adapta al gráfico.
#' @param .minimo_eje_y Número. Valor mínimo en el eje. Por defecto, `NULL` y el mínimo se adapta al gráfico
#' @param .maximo_eje_y Número. Valor máximo en el eje. Por defecto, `NULL` y el mínimo se adapta al gráfico
#' @param .date_labels Cadena de caracteres. Formato de las fechas en eje de abscisas. Por defecto, `"%Y"`. Para mostrar el año y mes abreviado, fijar a `"%Y-%b"`; para fijar sólo el número del año `"%Y"`. Para otros casos, ver documentación de `scale_x_date`.
#' @param .angle_x_axis_labels Número. Grado de inclinación de los datos en el eje horizontal. Sólo funciona con `.tipo_grafico_x = "fecha"`. Por defecto, `45`.
#' @param .date_breaks Cadena de caracteres. Frecuencia de las fechas en eje de abscisas. Por defecto, `"1 year"`. Para mostrar una fecha cada seis meses, fijar a `"6 months"`; para mostrar fechas cada 2 años, fijar a `"2 years"`.
#' @param .fecha_inicial_grafico Fecha. Sólo funciona con `.tipo_grafico_x = "fecha"`. Por defecto, `NULL`.
#' @param .fecha_final_grafico Fecha. Sólo funciona con `.tipo_grafico_x = "fecha"`. Por defecto, `NULL`.
#' @param .axis_position Cadena de caracteres. Posición del eje de ordenadas. Por defecto, `"right"`.
#' @param .legend_position Cadena de caracteres. Posición de la leyenda. Incluye la opción `"auto"`, que permite buscar un hueco "libre" dentro del gráfico y ponerlo en una esquina dentro del área de trazado. Por defecto, `"bottom"`.
#' @param .title_axis_size Número. Tamaño de la fuente en los títulos de los ejes. Sólo aplicable para `.tipo_grafico = "nofecha"`. Por defecto, `12`.
#' @param .text_axis_size Número. Tamaño de la fuente del texto de los ejes. Por defecto, `12`.
#' @param .legend_text_size Número. Tamaño de la fuente en la leyenda. Por defecto, `16`.
#' @param .text_size Número. Tamaño de la fuente en el resto del texto. Por defecto, `14`.
#' @param .nrows_legend_color Número. Número de filas a incluir en la leyenda. Por defecto, `3`.
#' @param .n_breaks_y Número. Número de breaks en el eje vertical. Por defecto, `NULL`.
#' @param .title_x_axis Título del eje x. Por defecto, `NULL`.
#' @param .title_y_axis Título del eje y. Por defecto, `NULL`.
#' @param .paleta_utilizada Vector de caracteres. Colores personalizados para la paleta. Si es NULL, usa la paleta VictorGM. Por defecto, `NULL`.
#' @param .n_colors Número. Cantidad de colores a interpolar de la paleta. Por defecto, `NULL`.
#' @param .title_x_axis Cadena de caracteres. Título del eje X. Por defecto, `NULL`.
#' @param .title_y_axis Cadena de caracteres. Título del eje Y. Por defecto, `NULL`.
#' @param .title Cadena de caracteres. Título del gráfico. Por defecto, `NULL`.
#' @param .title_size Número. Tamaño de la fuente del título. Por defecto, `18`.
#' @param .subtitle Cadena de caracteres. Subtítulo del gráfico. Habitualmente usado para definir las unidades de medida. Por defecto, `NULL`.
#' @param .subtitle_size Número. Tamaño de la fuente del subtítulo. Por defecto, `14`.
#' @param .color_titulo Cadena de caracteres. Color del título y subtítulo. Por defecto, `"#5F2987"` (morado).
#' @param .caption Cadena de caracteres. Nota al pie del gráfico. Por defecto, `" "` (espacio en blanco; se muestra el enlace URL si está definido).
#' @param .caption_size Número. Tamaño de la fuente de la nota al pie. Por defecto, `10`.
#' @param .fuente_letra Cadena de caracteres. Fuente tipográfica a utilizar en el gráfico. Por defecto, utiliza la fuente: `"Source Sans 3"`. Otras opciones comunes: `"Segoe UI"`, `"Arial"`, `"Times"`, `"Courier"`, `"Helvetica"`, etc.
#' @param .logo_path Cadena de caracteres. Ruta a una imagen PNG para incluir como logo en el gráfico. Por defecto, `NULL` (sin logo).
#' @param .logo_posicion Cadena de caracteres. Esquina en la que colocar el logo: `"topright"`, `"topleft"`, `"bottomright"` o `"bottomleft"`. Por defecto, `"bottomright"`.
#' @param .logo_escala Número. Tamaño del logo relativo al panel del gráfico (fracción de 0 a 1). Por defecto, `0.12`.
#' @return Objeto ggplot2 con el estilo corporativo VictorGM aplicado.
#' @examples
#' \dontrun{
#' library(ggplot2)
#' p <- ggplot(economics, aes(x = date, y = unemploy)) + geom_line()
#' graficos_estilo_victorgm(p, .tipo_grafico_y = "millones")
#' # Con logo
#' graficos_estilo_victorgm(p, .logo_path = "mi_logo.png", .logo_posicion = "bottomright")
#' }
#' @seealso \code{\link{tema_victorgm}}, \code{\link{aplicar_paleta_victorgm}}
#' @export
graficos_estilo_victorgm <- function(
    .x,
    .tipo_grafico_x = "fecha",
    .tipo_grafico_y = NULL,
    .scale = 1,
    .accuracy = 1,
    .suffix = "",
    .minimo_eje_x = NULL,
    .maximo_eje_x = NULL,
    .minimo_eje_y = NULL,
    .maximo_eje_y = NULL,
    .date_labels = "%Y",
    .angle_x_axis_labels = 90,
    .date_breaks = "1 year",
    .fecha_inicial_grafico = NULL,
    .fecha_final_grafico = NULL,
    .axis_position = "right",
    .legend_position = "bottom",
    .title_axis_size = 12,
    .text_axis_size = 12,
    .legend_text_size = 16,
    .text_size = 14,
    .nrows_legend_color = 3,
    .n_breaks_y = NULL,
    .paleta_utilizada = NULL,
    .n_colors = NULL,
    .title_x_axis = NULL,
    .title_y_axis = NULL,
    .title = NULL,
    .title_size = 18,
    .subtitle = NULL,
    .subtitle_size = 14,
    .color_titulo = "#5F2987",
    .caption = " ",
    .caption_size = 10,
    .fuente_letra = "Source Sans 3",
    .logo_path = NULL,
    .logo_posicion = "bottomright",
    .logo_escala = 0.12,
    .url_enlace = "www.victorgutierrezmarcos.es",
    .linea_separadora = TRUE,
    .colores_linea = c("#5F2987", "#E2EFD9", "#B8860B")
) {

  # Registrar fuente de Google Fonts si es necesario
  fuente_ok <- victorgmtools:::registrar_fuente(.fuente_letra)

  # Si falla la descarga de la fuente por defecto (Source Sans 3), usar sans como fallback
  # Solo intentamos el fallback si el usuario NO especificó una fuente personalizada distinta
  if (!fuente_ok && .fuente_letra == "Source Sans 3") {
    message("Usando 'sans' como fallback para 'Source Sans 3'.")
    .fuente_letra <- "sans"
  } else if (!fuente_ok) {
    # Si el usuario especificó otra fuente (ej: Segoe UI) y falló la descarga de Google,
    # asumimos que es una fuente del sistema y NO la cambiamos a 'sans'.
    # El mensaje de error de registrar_fuente ya avisó del fallo de descarga.
  }

  # Función auxiliar para encontrar la mejor posición de leyenda automáticamente

  encontrar_mejor_posicion_leyenda <- function(plot_object) {
    tryCatch({
      # Construir el gráfico para extraer datos
      plot_built <- ggplot2::ggplot_build(plot_object)
      
      if(length(plot_built$data) == 0) {
        return(list(pos = "bottom", just = NULL))
      }
      
      # Extraer datos de todas las capas
      all_data <- do.call(rbind, lapply(plot_built$data, function(layer) {
        if("x" %in% names(layer) && "y" %in% names(layer)) {
          data.frame(x = layer$x, y = layer$y, stringsAsFactors = FALSE)
        } else {
          NULL
        }
      }))
      
      if(is.null(all_data) || nrow(all_data) == 0) {
        return(list(pos = "bottom", just = NULL))
      }
      
      # Remover valores NA
      all_data <- all_data[complete.cases(all_data), ]
      
      if(nrow(all_data) < 4) {
        return(list(pos = "bottom", just = NULL))
      }
      
      # Calcular límites
      x_min <- min(all_data$x, na.rm = TRUE)
      x_max <- max(all_data$x, na.rm = TRUE)
      x_medio <- x_min + (x_max - x_min) / 2
      
      y_min <- min(all_data$y, na.rm = TRUE)
      y_max <- max(all_data$y, na.rm = TRUE)
      y_medio <- y_min + (y_max - y_min) / 2
      
      # Clasificar puntos por cuadrantes
      cuadrantes <- all_data %>%
        dplyr::mutate(
          cuadrante = dplyr::case_when(
            x <= x_medio & y >= y_medio ~ "sup_izq",
            x > x_medio & y >= y_medio ~ "sup_der",
            x <= x_medio & y < y_medio ~ "inf_izq",
            x > x_medio & y < y_medio ~ "inf_der"
          )
        )
      
      # Contar densidad por cuadrante
      densidad_cuadrantes <- cuadrantes %>%
        dplyr::count(cuadrante, name = "puntos") %>%
        dplyr::arrange(puntos)
      
      # Encontrar cuadrante menos denso
      cuadrante_menos_denso <- densidad_cuadrantes$cuadrante[1]
      
      # Asignar posición según cuadrante menos denso
      posicion_leyenda <- switch(
        cuadrante_menos_denso,
        "sup_izq" = list(pos = c(0.02, 0.98), just = c(0, 1)),
        "sup_der" = list(pos = c(0.98, 0.98), just = c(1, 1)),
        "inf_izq" = list(pos = c(0.02, 0.02), just = c(0, 0)),
        "inf_der" = list(pos = c(0.98, 0.02), just = c(1, 0)),
        list(pos = "bottom", just = NULL)  # fallback
      )
      
      return(posicion_leyenda)
      
    }, error = function(e) {
      # En caso de error, usar posición por defecto
      return(list(pos = "bottom", just = NULL))
    })
  }
  
  # Determinar posición de leyenda
  if(.legend_position == "auto") {
    posicion_auto <- encontrar_mejor_posicion_leyenda(.x)
    legend_pos <- posicion_auto$pos
    legend_just <- posicion_auto$just
  } else {
    legend_pos <- .legend_position
    legend_just <- NULL
  }
  
  .plot_to_return_plt <-
    .x +
    tema_victorgm() +
    ggplot2::theme(
      text = ggplot2::element_text(size = .text_size, family = .fuente_letra),
      legend.position = legend_pos,
      legend.text = ggplot2::element_text(size = .legend_text_size, family = .fuente_letra),
      title = ggplot2::element_text(family = .fuente_letra),
      plot.margin = ggplot2::margin(t = 5, r = 10, b = 5, l = 10),
      legend.margin = ggplot2::margin(t = 1, r = 1, b = 1, l = 1),
      axis.text = ggplot2::element_text(size = .text_axis_size, family = .fuente_letra)
    ) +
    ggplot2::guides(color = ggplot2::guide_legend(nrow = .nrows_legend_color, byrow = TRUE),
                    fill = ggplot2::guide_legend(nrow = .nrows_legend_color, byrow = TRUE))
  
  # Aplicar justificación y estilo especial para leyenda automática
  if(.legend_position == "auto" && !is.null(legend_just)) {
    .plot_to_return_plt <- .plot_to_return_plt +
      ggplot2::theme(
        legend.justification = legend_just,
        legend.background = ggplot2::element_rect(
          fill = ggplot2::alpha("white", 0.9),
          colour = "white",
          linewidth = 0.3
        ),
        legend.key.size = ggplot2::unit(0.4, "cm"),
        legend.margin = ggplot2::margin(t = 1, r = 1, b = 1, l = 1)
      )
  }
  
  suffix_definido <- .suffix
  scale_definido <- .scale
  accuracy_definido <- .accuracy
  
  # Función auxiliar para determinar el valor máximo del gráfico
  get_max_value <- function(plot_object, default_max = 1e6) {
    # Obtener datos construidos del gráfico
    plot_built <- try(ggplot2::ggplot_build(plot_object), silent = TRUE)
    
    if (inherits(plot_built, "try-error")) {
      return(default_max)
    }
    
    # Extraer todos los valores numéricos del eje Y
    y_values <- numeric()
    
    # Revisar todas las capas de datos
    if (!is.null(plot_built$data)) {
      for (layer_data in plot_built$data) {
        # Buscar columnas y, ymax, ymin y yend que suelen contener los valores del eje Y
        y_cols <- intersect(names(layer_data), c("y", "ymax", "ymin", "yend"))
        if (length(y_cols) > 0) {
          for (col in y_cols) {
            if (is.numeric(layer_data[[col]])) {
              y_values <- c(y_values, layer_data[[col]])
            }
          }
        }
      }
    }
    
    # Si encontramos valores, usar el máximo
    if (length(y_values) > 0 && max(y_values, na.rm = TRUE) > 0) {
      return(max(abs(y_values), na.rm = TRUE))
    } else {
      return(default_max)
    }
  }
  
  # Determinar si el eje Y es discreto (no requiere transformaciones numéricas)
  es_eje_y_discreto <- length(.tipo_grafico_y) > 0 && .tipo_grafico_y == "discreto"
  
  # Determinar escala automáticamente para tipo_grafico_y = "suffixing"
  if (length(.tipo_grafico_y) > 0 && .tipo_grafico_y == "suffixing") {
    # Determinar el máximo valor para elegir la escala apropiada
    if (!is.null(.maximo_eje_y)) {
      max_value <- .maximo_eje_y
    } else {
      max_value <- get_max_value(.x)
    }
    
    # Aplicar la escala adecuada según la magnitud
    if (max_value < 1e3) {
      suffix_definido <- ""
      scale_definido <- 1
    } else if (max_value < 1e6) {
      suffix_definido <- "K"
      scale_definido <- 1e-3
    } else if (max_value < 1e9) {
      suffix_definido <- "M"
      scale_definido <- 1e-6
    } else if (max_value < 1e12) {
      suffix_definido <- "MM"
      scale_definido <- 1e-9
    } else if (max_value < 1e15) {
      suffix_definido <- "B"
      scale_definido <- 1e-12
    }else{
      suffix_definido <- "T"
      scale_definido <- 1e-15
    }
  } else if (length(.tipo_grafico_y) > 0 && !es_eje_y_discreto) {
    if (.tipo_grafico_y == "millones") {
      suffix_definido <- "M"
      scale_definido <- 1e-6
    } else if (.tipo_grafico_y == "porcentaje") {
      suffix_definido <- "%"
      scale_definido <- 1e2
    }
  }
  
  if (.tipo_grafico_x == "fecha") {
    # Determinar límites del eje x
    # Prioridad: .minimo_eje_x/.maximo_eje_x > .fecha_inicial_grafico/.fecha_final_grafico
    
    lim_min <- if(!is.null(.minimo_eje_x)) .minimo_eje_x else .fecha_inicial_grafico
    lim_max <- if(!is.null(.maximo_eje_x)) .maximo_eje_x else .fecha_final_grafico
    
    if (is.null(lim_min) && is.null(lim_max)) {
      limits_x <- NULL
    } else {
      # Construir vector de limites de longitud 2
      # Usar NA_real_ en lugar de NA para asegurar tipo correcto si ambos son NA (aunque este caso está cubierto por el if)
      # Convertir a Date si es necesario para evitar warnings de escala
      
      lim_min_val <- if(is.null(lim_min)) NA else lim_min
      lim_max_val <- if(is.null(lim_max)) NA else lim_max
      
      limits_x <- c(lim_min_val, lim_max_val)
    }
    
    # Aplicar escala del eje X
    .plot_to_return_plt <-
      .plot_to_return_plt +
      ggplot2::scale_x_date(
        limits = limits_x,
        date_labels = .date_labels,
        date_breaks = .date_breaks,
        guide = ggplot2::guide_axis(angle = .angle_x_axis_labels)
      )
    
    # Aplicar escala del eje Y solo si NO es discreto
    if (!es_eje_y_discreto) {
      if(!is.null(.minimo_eje_y) & !is.null(.maximo_eje_y)) {
        .plot_to_return_plt <-
          .plot_to_return_plt +
          ggplot2::scale_y_continuous(
            labels = scales::number_format(
              accuracy = accuracy_definido,
              scale = scale_definido,
              suffix = suffix_definido,
              big.mark = ".",
              decimal.mark = ","
            ),
            limits = c(.minimo_eje_y, .maximo_eje_y),
            position = .axis_position,
            n.breaks = .n_breaks_y
          )
      } else {
        .plot_to_return_plt <-
          .plot_to_return_plt +
          ggplot2::scale_y_continuous(
            labels = scales::number_format(
              accuracy = accuracy_definido,
              scale = scale_definido,
              suffix = suffix_definido,
              big.mark = ".",
              decimal.mark = ","
            ),
            position = .axis_position,
            n.breaks = .n_breaks_y
          )
      }
    }
    
  } else if (.tipo_grafico_x == "nofecha") {
    # Determinar si hay límites para el eje x
    if(!is.null(.minimo_eje_x) && !is.null(.maximo_eje_x)){
      limits_x <- c(.minimo_eje_x, .maximo_eje_x)
    } else {
      limits_x <- NULL
    }
    
    # Aplicar escala del eje Y solo si NO es discreto
    if (!es_eje_y_discreto) {
      if(!is.null(.minimo_eje_y) & !is.null(.maximo_eje_y)) {
        .plot_to_return_plt <-
          .plot_to_return_plt +
          ggplot2::scale_y_continuous(
            labels = scales::number_format(
              accuracy = accuracy_definido,
              scale = scale_definido,
              suffix = suffix_definido,
              big.mark = ".",
              decimal.mark = ","
            ),
            limits = c(.minimo_eje_y, .maximo_eje_y),
            position = .axis_position,
            n.breaks = .n_breaks_y
          )
      } else {
        .plot_to_return_plt <-
          .plot_to_return_plt +
          ggplot2::scale_y_continuous(
            labels = scales::number_format(
              accuracy = accuracy_definido,
              scale = scale_definido,
              suffix = suffix_definido,
              big.mark = ".",
              decimal.mark = ","
            ),
            position = .axis_position,
            n.breaks = .n_breaks_y
          )
      }
    }
    
    # Añadir tema con grid horizontal
    .plot_to_return_plt <-
      .plot_to_return_plt +
      ggplot2::theme(
        panel.grid.major.x = ggplot2::element_line(colour = "#D9D9D9",
                                                   lineend = "round",
                                                   linewidth = 0.6,
                                                   linetype = "dotted",
                                                   arrow = FALSE)
      )
    
    # Añadir scale_x_continuous solo si hay límites definidos
    if(!is.null(limits_x)){
      .plot_to_return_plt <-
        .plot_to_return_plt +
        ggplot2::scale_x_continuous(limits = limits_x)
    }
  }
  
  if (is.null(.paleta_utilizada)){
    .plot_to_return_plt <-
      .plot_to_return_plt |>
      aplicar_paleta_victorgm(.n_colors = .n_colors)
  } else if(is.null(.n_colors)){
    .plot_to_return_plt <-
      .plot_to_return_plt +
      ggplot2::scale_color_manual(values = .paleta_utilizada, aesthetics = c("colour", "fill"))
  } else{
    .plot_to_return_plt <-
      .plot_to_return_plt +
      ggplot2::scale_color_manual(values = grDevices::colorRampPalette(.paleta_utilizada)(.n_colors), aesthetics = c("colour", "fill"))
  }
  
  if(!is.null(.title)){
    titulo_formateado <- stringr::str_wrap(.title, width = 40)

    .plot_to_return_plt <-
      .plot_to_return_plt +
      ggplot2::labs(title = titulo_formateado) +
      ggplot2::theme(plot.title = ggplot2::element_text(size = .title_size, face = "bold", color = .color_titulo, margin = ggplot2::margin(b = 5), family = .fuente_letra))
  }

  if(!is.null(.subtitle)){
    subtitulo_formateado <- stringr::str_wrap(.subtitle, width = 50)

    .plot_to_return_plt <-
      .plot_to_return_plt +
      ggplot2::labs(subtitle = subtitulo_formateado) +
      ggplot2::theme(plot.subtitle = ggplot2::element_text(size = .subtitle_size, family = .fuente_letra, face = "italic", color = .color_titulo, margin = ggplot2::margin(b = 10)))
  }

  # Acumuladores de margen - se aplican una sola vez al final
  bottom_margin_pt <- 5
  top_margin_pt <- 5
  right_margin_pt <- 10
  left_margin_pt <- 10
  has_caption <- (!is.null(.caption) && trimws(.caption) != "") ||
                 (!is.null(.url_enlace) && .url_enlace != "")

  # Estimar distancia desde el panel inferior hasta el área del caption
  # Se usa para posicionar la línea separadora y el logo
  tick_cm <- 0.3
  label_cm <- .text_axis_size * 4 / 72 * 2.54 * abs(sin(.angle_x_axis_labels * pi / 180)) +
              .text_axis_size / 72 * 2.54 * abs(cos(.angle_x_axis_labels * pi / 180))
  caption_area_offset_cm <- tick_cm + label_cm

  # Margen superior del caption para separarlo de la línea separadora
  caption_top_margin <- if (.linea_separadora) 5 else 2

  if(!is.null(.caption)){
    # Añadir enlace si existe
    if (!is.null(.url_enlace) && .url_enlace != "") {
      # Crear el enlace HTML con estilo
      link_html <- sprintf("<br><span style='font-size:%dpx; color:gray50;'>%s</span>",
                           round(.caption_size * 0.9),
                           .url_enlace)
      .caption <- paste0(.caption, link_html)
    }

    .plot_to_return_plt <-
      .plot_to_return_plt +
      ggplot2::labs(caption = .caption) +
      ggplot2::theme(plot.caption = ggtext::element_markdown(
        hjust = 0, family = .fuente_letra, size = .caption_size, color = "gray50",
        margin = ggplot2::margin(t = caption_top_margin)
      ))
    bottom_margin_pt <- bottom_margin_pt + 15
  } else if (!is.null(.url_enlace) && .url_enlace != "") {
    # Si no hay caption pero sí enlace
    .plot_to_return_plt <-
      .plot_to_return_plt +
      ggplot2::labs(caption = .url_enlace) +
      ggplot2::theme(plot.caption = ggtext::element_markdown(
        hjust = 0, family = .fuente_letra, size = .caption_size, color = "gray50",
        margin = ggplot2::margin(t = caption_top_margin)
      ))
    bottom_margin_pt <- bottom_margin_pt + 15
  }

  if(is.null(.title_x_axis)){
    .plot_to_return_plt <-
      .plot_to_return_plt +
      ggplot2::theme(axis.title.x = ggplot2::element_blank())
  } else {
    .plot_to_return_plt <-
      .plot_to_return_plt +
      ggplot2::labs(x = .title_x_axis) +
      ggplot2::theme(axis.title.x = ggplot2::element_text(family = .fuente_letra, size = .title_axis_size))
  }

  if(is.null(.title_y_axis)){
    .plot_to_return_plt <-
      .plot_to_return_plt +
      ggplot2::theme(axis.title.y = ggplot2::element_blank())
  } else {
    .plot_to_return_plt <-
      .plot_to_return_plt +
      ggplot2::labs(y = .title_y_axis) +
      ggplot2::theme(axis.title.y = ggplot2::element_text(family = .fuente_letra, size = .title_axis_size))
  }

  # Configurar update_geom_defaults para que geom_text use la fuente por defecto
  ggplot2::update_geom_defaults("text", list(family = .fuente_letra))
  ggplot2::update_geom_defaults("label", list(family = .fuente_letra))

  # Insertar línea separadora
  if (.linea_separadora) {
    # Crear gradiente
    grad <- grDevices::colorRampPalette(.colores_linea)(100)
    # Crear raster grob (1 pixel alto, 100 ancho)
    linea_grob <- grid::rasterGrob(
      matrix(grad, nrow = 1),
      width = grid::unit(1, "npc"),
      height = grid::unit(2, "pt"),
      interpolate = TRUE,
      vp = grid::viewport(y = 0, just = "bottom")
    )

    # Posición adaptativa: justo debajo de las etiquetas del eje X, encima del caption
    y_linea <- grid::unit(0, "npc") - grid::unit(caption_area_offset_cm, "cm")

    linea_grob$y <- y_linea

    .plot_to_return_plt <- .plot_to_return_plt +
      ggplot2::annotation_custom(
        linea_grob,
        xmin = -Inf, xmax = Inf,
        ymin = -Inf, ymax = Inf
      ) +
      ggplot2::coord_cartesian(clip = "off")

    bottom_margin_pt <- bottom_margin_pt + 8
  }

  # Insertar logo si se ha indicado ruta
  if (!is.null(.logo_path)) {
    if (!file.exists(.logo_path)) {
      stop(paste("Archivo de logo no encontrado:", .logo_path))
    }

    img <- png::readPNG(.logo_path)
    dims <- dim(img)
    ar <- dims[1] / dims[2] # alto / ancho

    # Autoscaling usando snpc (Square Normalized Parent Coordinates)
    # Esto mantiene la proporción relativa al lado más pequeño del gráfico
    logo_width <- grid::unit(.logo_escala * 2, "snpc")
    logo_height <- grid::unit(.logo_escala * 2 * ar, "snpc")

    margin_pad_cm <- 0.2

    # Variables para configuración
    x_pos <- NULL
    y_pos <- NULL
    hjust_val <- NULL
    vjust_val <- NULL

    # Para posiciones inferiores, alinear el logo verticalmente con el caption
    logo_y_bottom <- grid::unit(0, "npc") - grid::unit(caption_area_offset_cm + 0.5, "cm")

    # Configurar posición según .logo_posicion
    if (.logo_posicion == "bottomright") {
      x_pos <- grid::unit(1, "npc")
      y_pos <- logo_y_bottom
      hjust_val <- 1
      vjust_val <- 0.5
      bottom_margin_pt <- bottom_margin_pt + 40

    } else if (.logo_posicion == "bottomleft") {
      x_pos <- grid::unit(0, "npc")
      y_pos <- logo_y_bottom
      hjust_val <- 0
      vjust_val <- 0.5
      bottom_margin_pt <- bottom_margin_pt + 40

    } else if (.logo_posicion == "topright") {
      x_pos <- grid::unit(1, "npc")
      y_pos <- grid::unit(1, "npc") + grid::unit(margin_pad_cm, "cm")
      hjust_val <- 1
      vjust_val <- 0
      top_margin_pt <- top_margin_pt + 25

    } else if (.logo_posicion == "topleft") {
      x_pos <- grid::unit(0, "npc")
      y_pos <- grid::unit(1, "npc") + grid::unit(margin_pad_cm, "cm")
      hjust_val <- 0
      vjust_val <- 0
      top_margin_pt <- top_margin_pt + 25

    } else {
      stop("'.logo_posicion' debe ser 'topright', 'topleft', 'bottomright' o 'bottomleft'.")
    }

    logo_grob <- grid::rasterGrob(
      img,
      x = x_pos,
      y = y_pos,
      width = logo_width,
      height = logo_height,
      hjust = hjust_val,
      vjust = vjust_val,
      interpolate = TRUE
    )

    .plot_to_return_plt <- .plot_to_return_plt +
      ggplot2::annotation_custom(
        logo_grob,
        xmin = -Inf, xmax = Inf,
        ymin = -Inf, ymax = Inf
      ) +
      ggplot2::coord_cartesian(clip = "off")
  }

  # Aplicar márgenes acumulados una sola vez
  .plot_to_return_plt <- .plot_to_return_plt +
    ggplot2::theme(plot.margin = ggplot2::margin(
      t = top_margin_pt, r = right_margin_pt,
      b = bottom_margin_pt, l = left_margin_pt
    ))

  return(.plot_to_return_plt)
}

# Funciones necesarias para mostrar gráficos interactivos y tablas con gt ----

#' Convertir gráfico ggplot2 a ggiraph interactivo
#'
#' Convierte un objeto ggplot2 en un gráfico interactivo usando ggiraph.
#' Incluye efectos de hover con reducción de opacidad de elementos no seleccionados.
#'
#' @param .plot Objeto ggplot2 a convertir en interactivo.
#' @param .width Número. Ancho del gráfico SVG en pulgadas. Por defecto, `6`.
#' @param .height Número. Alto del gráfico SVG en pulgadas. Por defecto, `5`.
#' @return Objeto ggiraph interactivo.
#' @examples
#' \dontrun{
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(x = wt, y = mpg)) + ggiraph::geom_point_interactive(aes(tooltip = row.names(mtcars)))
#' to_giraph(p)
#' }
#' @export
to_giraph <- function(.plot,
                      .width = 6,
                      .height = 5) {
  ggiraph::girafe(
    ggobj = .plot,
    width_svg = .width,
    height_svg = .height,
    options = list(
      # ggiraph::opts_tooltip(css = ''),
      ggiraph::opts_hover(css = ''), ## CSS code of line we're hovering over
      ggiraph::opts_hover_inv(
        css = "opacity:0.4;"
      ), ## CSS code of all other lines
      ggiraph::opts_sizing(rescale = FALSE), ## Fixes sizes to dimensions below
      position = "topright",
      saveaspng = TRUE
    )
  )
  
}


# estatico ---

#' Convertir gráfico a estático o interactivo
#'
#' Convierte un gráfico ggplot2 a formato interactivo (ggiraph) o lo mantiene estático
#' según el valor del parámetro .estatico.
#'
#' @param .plot Objeto ggplot2 a procesar.
#' @param .estatico Lógico. Si TRUE, devuelve el gráfico estático. Si FALSE, lo convierte a interactivo. Por defecto, `FALSE`.
#' @param .width Número. Ancho del gráfico en pulgadas. Por defecto, `6`.
#' @param .height Número. Alto del gráfico en pulgadas. Por defecto, `5`.
#' @return Objeto ggplot2 (si estático) o ggiraph (si interactivo).
#' @seealso \code{\link{to_giraph}}, \code{\link{mostrar}}
#' @export
estatico <- function(.plot,
                     .estatico,
                     .width = 6,
                     .height = 5) {
  
  .plot_return <- .plot
  
  if(!.estatico) {
    .plot_return <-
      .plot_return |>
      comerciotools::to_giraph(.width = .width, .height = .height) |>
      ggiraph::girafe_options(
        ggiraph::opts_sizing(
          rescale = TRUE,
          width = 1
        )
      )
    
    return(.plot_return)
  }
  return(.plot_return)
}

#' Mostrar gráfico o tabla de forma inteligente
#'
#' Muestra un objeto ggplot o gt adaptándose al contexto de renderizado.
#' Para ggplot: puede mostrar versión interactiva (ggiraph) o estática.
#' Para tablas gt: en HTML las muestra directamente, en LaTeX/PDF usa webshot2 para capturar.
#'
#' @param .x Objeto ggplot2 o gt_tbl a mostrar.
#' @param .estatico Lógico. Si TRUE y el objeto es ggplot, lo muestra estático. Si FALSE, lo convierte a interactivo. Por defecto, `FALSE`.
#' @param .width Número. Ancho del gráfico en pulgadas. Por defecto, `6`.
#' @param .height Número. Alto del gráfico en pulgadas. Por defecto, `5`.
#' @return El objeto procesado para visualización según el contexto.
#' @examples
#' \dontrun{
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point()
#' mostrar(p) # Interactivo en HTML
#' mostrar(p, .estatico = TRUE) # Siempre estático
#' }
#' @seealso \code{\link{estatico}}, \code{\link{to_giraph}}
#' @export
mostrar <- function(.x,
                    .estatico = FALSE,
                    .width = 6,
                    .height = 5) {
  object_name <- deparse(substitute(.x))
  x <- tryCatch({
    objeto <- eval(substitute(.x))
    if(inherits(objeto, "ggplot")) {
      return(objeto |>
               estatico(
                 .estatico = .estatico,
                 .width = .width,
                 .height = .height
               )
      )
    } else if(inherits(objeto, "gt_tbl")) {
      if (knitr::is_html_output()){
        return(objeto)
      } else if (knitr::is_latex_output()) {
        objeto <-
          objeto |>
          gt::as_latex()
        
        return(objeto)
      } else {
        return(objeto)
      }
    }
  },
  error = function(e) {
    message(sprintf("Cannot find object passed as argument"))
    return(NULL)
  })
  
  return(x)
}

# Función general para dar formato a mapas coropléticos ----
#' Fijar estilo de mapas coropléticos (por defecto) o de burbujas para VictorGM, con algunos parámetros opcionales. Habría que añadir el título al gráfico generado.
#' @param .x Data frame que contenga 2 columnas: `.col_nombres` y `.col_valores` con los valores a representar.
#' @param .tipo_mapa Cadena de caracteres. Puede tomar valores `"coroplético"` (por defecto) o `"burbujas"`.
#' @param .region_geografica Cadena de caracteres. Puede tomar valores `"España"` o `"Mundo"`. Por defecto, `"España"`.
#' @param .unidades_territoriales Cadena de caracteres. Solo funciona en caso de que `.region_geografica == "España"`, ya que en caso contrario se representara por países. Puede tomar valores `"pais"`, `"ccaa"` o `"provincia"`. Por defecto, `"provincia"`.
#' @param .col_nombres Cadena de caracteres. Sirve para indicar el nombre de la columna que contiene las áreas geográficas. En caso de `.unidades_territoriales == "provincias"` y `.unidades_territoriales == "ccaa"` se espera que introduzca un df con los códigos por provincias o de las comunidades de datacomex. En caso de `.unidades_territoriales == "pais"` se utilizará el código iso3A (se puede cambiar en el parámetro `.codigo_pais`). Por defecto, `"nombres"`.
#' @param .col_valores Cadena de caracteres. Sirve para indicar el nombre de la columna que contiene los valores a representar. Por defecto, `"valores"`.
#' @param .codigo_pais Cadena de caracteres. Solo funciona en caso de que las unidades geográficas sean países. Puede tomar valores `"iso3A"` (si se usa el código iso a 3 letras), `"iso_code"` (si se usa el código iso a 3 dígitos) o `"datacomex"` (si se usa el código de datacomex). Por defecto, `"iso3A"`.
#' @param .color_negativo Cadena de caracteres. Color que representará los valores negativos. Por defecto, `"#E31A1CFF"`, rojo de la paleta de VictorGM
#' @param .color_positivo Cadena de caracteres. Color que representará los valores positivos. Por defecto, `"#18BC9CFF"`, verde de la paleta de VictorGM
#' @param .color_neutro Cadena de caracteres. Color que representará el nivel neutro. Por defecto, `"white"`.
#' @param .nivel_neutro Número. Cuantía que se representará con el color neutro en la escala de 3 colores. Por defecto, `0`.
#' @param .color_na Cadena de caracteres. Color que representará a las unidades territoriales con `NA`. Por defecto, `"grey50"`.
#' @param .color_burbujas Cadena de caracteres. Color de las burbujas (solo para mapas de burbujas). Por defecto, `"#18BC9C"`.
#' @param .color_fondo_mapa Cadena de caracteres. Color de fondo del mapa (solo para mapas de burbujas). Por defecto, `"grey95"`.
#' @param .tamano_min_burbuja Número. Tamaño mínimo de las burbujas. Por defecto, `1`.
#' @param .tamano_max_burbuja Número. Tamaño máximo de las burbujas. Por defecto, `20`.
#' @param .alpha_burbujas Número entre 0 y 1. Transparencia de las burbujas. Por defecto, `0.7`.
#' @param .resaltar Cadena de caracteres o vector. Permite establecer una o varias unidades territoriales a destacar en el mapa mediante sombras. Se establecerán con el código que aparezca en el dataframe `.x`. Por defecto, `NULL`.
#' @param .scale Número. Factor por el que multiplicar los números del dataframe Por defecto, `1`. Para millones, p.ej., habría que fijar a 1e+6 o 1000000.
#' @param .accuracy Número. Número de decimales a mostrar en el interactivo (en caso de que se represente la versión interactiva). Para mostrar 0 decimales, fijar a `1`; para 1 decimal, fijar a `0.1`; para 2, a `0.01` y así sucesivamente. Por defecto, `0.01` (i.e. 2 decimales).
#' @param .suffix Cadena de caracteres. Por ejemplo, en caso de porcentaje, establecer `" %"` para que aparezca es sufijo tanto en la leyenda como en el interactivo (en su caso). Por defecto, cadena vacía.
#' @param .title Título del mapa Por defecto, `NULL`.
#' @param .subtitle Subtitulo del mapa. Habitualmente usado para definir las unidades de medida. Por defecto, `NULL`.
#' @param .color_titulo Cadena de caracteres. Color del título y subtítulo. Por defecto, `"#5F2987"` (morado).
#' @param .caption Nota al pie del mapa. Por defecto, `" "` (espacio en blanco; se muestra el enlace URL si está definido).
#' @param .fuente_letra Cadena de caracteres. Fuente tipográfica a utilizar en el gráfico. Por defecto, utiliza la fuente: `"Source Sans 3"`. Otras opciones comunes: `"Segoe UI"`, `"Arial"`, `"Times"`, `"Courier"`, `"Helvetica"`, etc.
#' @param .logo_path Cadena de caracteres. Ruta a una imagen PNG para incluir como logo en el gráfico. Por defecto, `NULL` (sin logo).
#' @param .logo_posicion Cadena de caracteres. Esquina en la que colocar el logo: `"topright"`, `"topleft"`, `"bottomright"` o `"bottomleft"`. Por defecto, `"bottomright"`.
#' @param .logo_escala Número. Tamaño del logo relativo. Por defecto, `0.12`.
#' @export
mapa_estilo_victorgm <- function(
    .x,
    .tipo_mapa = "coroplético",
    .region_geografica = "España",
    .unidades_territoriales = "provincia",
    .col_nombres = "nombres",
    .col_valores = "valores",
    .codigo_pais = "iso3A",
    .color_negativo = "#A63A50",
    .color_positivo = "#4A9079",
    .color_neutro = "white",
    .nivel_neutro = 0,
    .color_na = "grey50",
    .color_burbujas = "#4A9079",
    .color_fondo_mapa = "grey95",
    .tamano_min_burbuja = 1,
    .tamano_max_burbuja = 20,
    .alpha_burbujas = 0.7,
    .resaltar = NULL,
    .scale = 1,
    .accuracy = 0.01,
    .suffix = "",
    .title = NULL,
    .title_size = 18,
    .subtitle = NULL,
    .subtitle_size = 14,
    .color_titulo = "#5F2987",
    .caption = " ",
    .caption_size = 10,
    .fuente_letra = "Source Sans 3",
    .logo_path = NULL,
    .logo_posicion = "bottomright",
    .logo_escala = 0.12,
    .url_enlace = "www.victorgutierrezmarcos.es",
    .linea_separadora = TRUE,
    .colores_linea = c("#5F2987", "#E2EFD9", "#B8860B")
) {

  # Registrar fuente de Google Fonts si es necesario
  fuente_ok <- victorgmtools:::registrar_fuente(.fuente_letra)

  # Si falla la descarga de la fuente por defecto (Source Sans 3), usar sans como fallback
  if (!fuente_ok && .fuente_letra == "Source Sans 3") {
    message("Usando 'sans' como fallback para 'Source Sans 3'.")
    .fuente_letra <- "sans"
  } else if (!fuente_ok) {
    # Misma logica para mapas: si es custom y falla Google, asumir sistema
  }

  # Validar tipo de mapa
  if (!.tipo_mapa %in% c("coroplético", "burbujas")) {
    stop("El parámetro .tipo_mapa debe ser 'coroplético' o 'burbujas'")
  }
  
  if(.region_geografica != "España"){
    .unidades_territoriales  <- "pais"
  }
  
  if(.region_geografica == "España" && .unidades_territoriales == "provincia"){
    .x <-
      .x |>
      dplyr::rename(provincia = !!rlang::sym(.col_nombres)) |>
      dplyr::left_join(comerciotools:::get_provincias_metadata(), by = "provincia") |>
      dplyr::rename(nombres = nombre_provincia) |>
      dplyr::mutate(
        nombres = dplyr::case_when(
          nombres == "Araba/Alava" ~ "Álava",
          nombres == "Balears, Illes" ~ "Baleares",
          nombres == "Coruña, A" ~ "La Coruña",
          nombres == "Girona" ~ "Gerona",
          nombres == "Lleida" ~ "Lérida",
          nombres == "Rioja, La" ~ "La Rioja",
          nombres == "Ourense" ~ "Orense",
          nombres == "Palmas, Las" ~ "Las Palmas",
          TRUE ~ nombres
        ))
    
    espana_sf <-
      rnaturalearth::ne_states(country = "spain", returnclass = "sf")
    
    espana_datos <-
      merge(espana_sf, .x,
            by.x = "name",
            by.y = "nombres",
            all.x = TRUE)
    
    espana_datos <-
      espana_datos |>
      dplyr::mutate(
        name = dplyr::case_when(
          name == "Álava" ~ "Araba/Alava",
          name == "Baleares" ~ "Balears, Illes",
          name == "La Coruña" ~ "A Coruña",
          name == "Gerona" ~ "Girona",
          name == "Lérida" ~ "Lleida",
          name == "La Rioja" ~ "Rioja, La",
          name == "Orense" ~ "Ourense",
          name == "Las Palmas" ~ "Palmas, Las",
          TRUE ~ name
        )
      )
    
    canarias <-
      espana_datos[grepl("Canarias", espana_datos$nombre_comunidad), ] |>
      sf::st_transform(4326)
    peninsula <-
      espana_datos[!grepl("Canarias", espana_datos$nombre_comunidad), ] |>
      sf::st_transform(4326)
    
    shift_geometry <- function(sf_obj, x_shift = 7, y_shift = 6) {
      geom <- sf::st_geometry(sf_obj)
      new_geom <- geom + c(x_shift, y_shift)
      sf::st_set_crs(new_geom, sf::st_crs(geom))
    }
    
    canarias_shifted <- canarias
    sf::st_geometry(canarias_shifted) <- shift_geometry(canarias, 7, 6)
    
    sf::st_crs(canarias_shifted) <- sf::st_crs(peninsula)
    
    .x <- rbind(peninsula, canarias_shifted)
  } else if(.region_geografica == "España" && .unidades_territoriales == "ccaa"){
    .x <-
      .x |>
      dplyr::rename(comunidad = !!rlang::sym(.col_nombres)) |>
      dplyr::left_join(comerciotools:::get_provincias_metadata(), by = "comunidad") |>
      dplyr::rename(nombres = nombre_comunidad) |>
      dplyr::mutate(
        nombres = dplyr::case_when(
          nombres == "Asturias, Principado de" ~ "Asturias",
          nombres == "Canarias" ~ "Canary Is.",
          nombres == "Navarra, Comunidad Foral de" ~ "Foral de Navarra",
          nombres == "Balears, Illes" ~ "Islas Baleares",
          nombres == "Rioja, La" ~ "La Rioja",
          nombres == "Madrid, Comunidad de" ~ "Madrid",
          nombres == "Murcia, Región de" ~ "Murcia",
          nombres == "Comunitat Valenciana" ~ "Valenciana",
          TRUE ~ nombres
        ))
    
    espana_sf <-
      rnaturalearth::ne_states(country = "spain", returnclass = "sf") |>
      dplyr::group_by(region) |>
      dplyr::summarise(geometry = sf::st_union(geometry)) |>
      dplyr::ungroup() |>
      dplyr::rename(name = region)
    
    espana_datos <-
      merge(espana_sf, .x,
            by.x = "name",
            by.y = "nombres",
            all.x = TRUE)
    
    espana_datos <-
      espana_datos |>
      dplyr::mutate(
        name = dplyr::case_when(
          name == "Asturias" ~ "Asturias, Principado de",
          name == "Canary Is." ~ "Canarias",
          name == "Foral de Navarra" ~ "Navarra, Comunidad Foral de",
          name == "Islas Baleares" ~ "Balears, Illes",
          name == "La Rioja" ~ "Rioja, La",
          name == "Madrid" ~ "Madrid, Comunidad de",
          name == "Murcia" ~ "Murcia, Región de",
          name == "Valenciana" ~ "Comunitat Valenciana",
          TRUE ~ name
        )
      )
    
    canarias <-
      espana_datos[grepl("Canarias", espana_datos$nombre_comunidad), ] |>
      sf::st_transform(4326)
    peninsula <-
      espana_datos[!grepl("Canarias", espana_datos$nombre_comunidad), ] |>
      sf::st_transform(4326)
    
    shift_geometry <- function(sf_obj, x_shift = 7, y_shift = 6) {
      geom <- sf::st_geometry(sf_obj)
      new_geom <- geom + c(x_shift, y_shift)
      sf::st_set_crs(new_geom, sf::st_crs(geom))
    }
    
    canarias_shifted <- canarias
    sf::st_geometry(canarias_shifted) <- shift_geometry(canarias, 7, 6)
    
    sf::st_crs(canarias_shifted) <- sf::st_crs(peninsula)
    
    .x <- rbind(peninsula, canarias_shifted)
  } else if(.region_geografica == "Mundo"){
    if(.codigo_pais == "iso3A"){
      .x <-
        .x |>
        dplyr::rename(iso3A = !!rlang::sym(.col_nombres)) |>
        dplyr::left_join(wtor::get_partner_economies(), by = "iso3A") |>
        dplyr::rename(nombres = iso3A)
    }
    else if(.codigo_pais == "iso_code"){
      .x <-
        .x |>
        dplyr::rename(iso_code = !!rlang::sym(.col_nombres)) |>
        dplyr::left_join(comerciotools::get_iso_datacomex(), by = "iso_code") |>
        dplyr::select(-nombre, -iso_code, -datacomex) |>
        dplyr::left_join(wtor::get_partner_economies(), by = "iso3A") |>
        dplyr::rename(nombres = iso3A)
    } else if (.codigo_pais == "datacomex"){
      .x <-
        .x |>
        dplyr::rename(datacomex = !!rlang::sym(.col_nombres)) |>
        dplyr::left_join(comerciotools::get_iso_datacomex(), by = "datacomex") |>
        dplyr::select(-nombre, -iso_code, -datacomex) |>
        dplyr::left_join(wtor::get_partner_economies(), by = "iso3A") |>
        dplyr::rename(nombres = iso3A)
    }
    
    mundo_sf <-
      rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") |>
      dplyr::mutate(iso_a3 = dplyr::case_when(iso_a3 != -99 ~ iso_a3,
                                              TRUE ~ adm0_a3)) |>
      dplyr::select(iso_a3) |>
      dplyr::rename(name = iso_a3)
    
    mundo_datos <-
      merge(mundo_sf, .x,
            by.x = "name",
            by.y = "nombres",
            all.x = TRUE)
    
    .x <-
      mundo_datos |>
      dplyr::rename(iso3A = name) |>
      dplyr::left_join(wtor::get_partner_economies(lang = "3"), by = "iso3A")
  }
  
  # Crear el mapa base
  if (.tipo_mapa == "coroplético") {
    # Mapa coroplético (código original)
    .map_to_return <-
      ggplot2::ggplot(data = .x) +
      ggiraph::geom_sf_interactive(ggplot2::aes(fill = .data[[.col_valores]],
                                                tooltip = paste0(name, "\n",
                                                                 scales::number_format(scale = .scale, accuracy = .accuracy, suffix = .suffix, big.mark = ".", decimal.mark = ",")(.data[[.col_valores]]))))
  } else {
    # Mapa de burbujas
    # Primero crear un mapa base sin datos
    if(.region_geografica == "España"){
      mapa_base <- rnaturalearth::ne_states(country = "spain", returnclass = "sf")
      
      # Aplicar las mismas transformaciones para Canarias
      canarias_base <- mapa_base[grepl("Canarias", mapa_base$region), ] |> sf::st_transform(4326)
      peninsula_base <- mapa_base[!grepl("Canarias", mapa_base$region), ] |> sf::st_transform(4326)
      
      shift_geometry <- function(sf_obj, x_shift = 7, y_shift = 6) {
        geom <- sf::st_geometry(sf_obj)
        new_geom <- geom + c(x_shift, y_shift)
        sf::st_set_crs(new_geom, sf::st_crs(geom))
      }
      
      canarias_base_shifted <- canarias_base
      sf::st_geometry(canarias_base_shifted) <- shift_geometry(canarias_base, 7, 6)
      sf::st_crs(canarias_base_shifted) <- sf::st_crs(peninsula_base)
      
      mapa_base <- rbind(peninsula_base, canarias_base_shifted)
      
    } else {
      mapa_base <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
    }
    
    # Calcular centroides para las burbujas
    .x_centroides <- .x |>
      dplyr::filter(!is.na(.data[[.col_valores]])) |>
      dplyr::mutate(
        centroid = sf::st_centroid(geometry),
        lon = sf::st_coordinates(centroid)[,1],
        lat = sf::st_coordinates(centroid)[,2]
      )
    
    .map_to_return <-
      ggplot2::ggplot() +
      ggplot2::geom_sf(data = mapa_base, fill = .color_fondo_mapa, color = "white", size = 0.3) +
      ggiraph::geom_point_interactive(
        data = .x_centroides,
        ggplot2::aes(
          x = lon,
          y = lat,
          size = abs(.data[[.col_valores]]),
          tooltip = paste0(name, "\n",
                           scales::number_format(scale = .scale, accuracy = .accuracy, suffix = .suffix, big.mark = ".", decimal.mark = ",")(.data[[.col_valores]]))
        ),
        color = .color_burbujas,
        alpha = .alpha_burbujas
      ) +
      ggplot2::scale_size_continuous(
        name = "",
        range = c(.tamano_min_burbuja, .tamano_max_burbuja),
        labels = scales::label_number(
          scale = .scale,
          big.mark = ".",
          decimal.mark = ",",
          suffix = .suffix
        )
      )
  }
  
  # Aplicar resaltado si es necesario
  if(!is.null(.resaltar) && .tipo_mapa == "coroplético"){
    if(.region_geografica == "Mundo"){
      if(.codigo_pais == "iso_3A"){
        .paises <-
          comerciotools::get_iso_datacomex() |>
          dplyr::select(
            codigo = iso3A,
            nombre = nombre
          )
        
        .resaltar_name <-
          .paises |>
          dplyr::filter(codigo %in% .resaltar) |>
          dplyr::pull(nombre)
      } else if(.codigo_pais == "iso_code"){
        .paises <-
          comerciotools::get_iso_datacomex() |>
          dplyr::select(
            codigo = iso_code,
            nombre = nombre
          )
        
        .resaltar_name <-
          .paises |>
          dplyr::filter(codigo %in% .resaltar) |>
          dplyr::pull(nombre)
      } else if(.codigo_pais == "datacomex"){
        .paises <-
          comerciotools::get_iso_datacomex() |>
          dplyr::select(
            codigo = datacomex,
            nombre = nombre
          )
        
        .resaltar_name <-
          .paises |>
          dplyr::filter(codigo %in% .resaltar) |>
          dplyr::pull(nombre)
      } else{
        .resaltar_name <- NULL
      }
    }else if(.region_geografica == "España" && .unidades_territoriales == "ccaa"){
      .ccaa <-
        comerciotools:::get_provincias_metadata() |>
        dplyr::select(
          codigo = comunidad,
          nombre = nombre_comunidad
        )
      
      .resaltar_name <-
        .ccaa |>
        dplyr::filter(codigo %in% .resaltar) |>
        dplyr::pull(nombre)
    }else if(.region_geografica == "España" && .unidades_territoriales == "provincia"){
      .provincias <-
        comerciotools:::get_provincias_metadata() |>
        dplyr::select(
          codigo = provincia,
          nombre = nombre_provincia
        )
      
      .resaltar_name <-
        .provincias |>
        dplyr::filter(codigo %in% .resaltar) |>
        dplyr::pull(nombre)
    }
    
    .map_to_return <-
      .map_to_return +
      ggfx::with_shadow(
        ggiraph::geom_sf_interactive(
          data = .x |> dplyr::filter(name == .resaltar_name),
          mapping = ggplot2::aes(fill = .data[[.col_valores]],
                                 tooltip = paste0(name, "\n",
                                                  scales::number_format(scale = .scale, accuracy = .accuracy, suffix = .suffix, big.mark = ".", decimal.mark = ",")(.data[[.col_valores]]))),
          color = "white",
          linewidth = 0.8),
        sigma = 0,
        x_offset = 4,
        y_offset = 4
      )
  }
  
  # Aplicar escalas de color según el tipo de mapa
  if (.tipo_mapa == "coroplético") {
    .map_to_return <-
      .map_to_return +
      ggplot2::scale_fill_gradient2(
        name = "",
        low = .color_negativo,
        mid = .color_neutro,
        high = .color_positivo,
        na.value = .color_na,
        midpoint = .nivel_neutro,
        labels = scales::label_number(
          scale = .scale,
          big.mark = ".",
          decimal.mark = ",",
          suffix = .suffix
        )
      )
  }
  
  # Aplicar tema y configuraciones comunes
  .map_to_return <-
    .map_to_return +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank()
    )
  
  if(.region_geografica == "España"){
    .map_to_return <-
      .map_to_return +
      ggplot2::geom_rect(
        ggplot2::aes(xmin = -11.3, xmax = -6,
                     ymin = 33.5, ymax = 35.5),
        fill = NA,
        color = "darkgrey",
        linewidth = 0.25)
  }
  
  if(!is.null(.title)){
    titulo_formateado <- stringr::str_wrap(.title, width = 40)

    .map_to_return <-
      .map_to_return +
      ggplot2::labs(title = titulo_formateado) +
      ggplot2::theme(plot.title = ggplot2::element_text(size = .title_size, face = "bold", color = .color_titulo, margin = ggplot2::margin(b = 5), family = .fuente_letra))
  }

  if(!is.null(.subtitle)){
    subtitulo_formateado <- stringr::str_wrap(.subtitle, width = 50)

    .map_to_return <-
      .map_to_return +
      ggplot2::labs(subtitle = subtitulo_formateado) +
      ggplot2::theme(plot.subtitle = ggplot2::element_text(size = .subtitle_size, family = .fuente_letra, face = "italic", color = .color_titulo, margin = ggplot2::margin(b = 10)))
  }

  # Acumuladores de margen - se aplican una sola vez al final
  bottom_margin_pt <- 5
  top_margin_pt <- 5
  right_margin_pt <- 10
  left_margin_pt <- 10
  has_caption <- (!is.null(.caption) && trimws(.caption) != "") ||
                 (!is.null(.url_enlace) && .url_enlace != "")

  # Mapas no tienen etiquetas de eje, distancia fija desde el panel al caption
  caption_area_offset_cm <- 0.3

  # Margen superior del caption para separarlo de la línea separadora
  caption_top_margin <- if (.linea_separadora) 5 else 2

  if(!is.null(.caption)){
    # Añadir enlace si existe
    if (!is.null(.url_enlace) && .url_enlace != "") {
      link_html <- sprintf("<br><span style='font-size:%dpx; color:gray50;'>%s</span>",
                           round(.caption_size * 0.9),
                           .url_enlace)
      .caption <- paste0(.caption, link_html)
    }

    .map_to_return <-
      .map_to_return +
      ggplot2::labs(caption = .caption) +
      ggplot2::theme(plot.caption = ggtext::element_markdown(
        hjust = 0, family = .fuente_letra, size = .caption_size, color = "gray50",
        margin = ggplot2::margin(t = caption_top_margin)
      ))
    bottom_margin_pt <- bottom_margin_pt + 15
  } else if (!is.null(.url_enlace) && .url_enlace != "") {
    .map_to_return <-
      .map_to_return +
      ggplot2::labs(caption = .url_enlace) +
      ggplot2::theme(plot.caption = ggtext::element_markdown(
        hjust = 0, family = .fuente_letra, size = .caption_size, color = "gray50",
        margin = ggplot2::margin(t = caption_top_margin)
      ))
    bottom_margin_pt <- bottom_margin_pt + 15
  }

  # Configurar update_geom_defaults para que geom_text use la fuente por defecto
  ggplot2::update_geom_defaults("text", list(family = .fuente_letra))
  ggplot2::update_geom_defaults("label", list(family = .fuente_letra))

  # Insertar línea separadora
  if (.linea_separadora) {
    grad <- grDevices::colorRampPalette(.colores_linea)(100)
    linea_grob <- grid::rasterGrob(
      matrix(grad, nrow = 1),
      width = grid::unit(1, "npc"),
      height = grid::unit(2, "pt"),
      interpolate = TRUE,
      vp = grid::viewport(y = 0, just = "bottom")
    )

    # Posición fija para mapas (no tienen etiquetas de eje X)
    y_linea <- grid::unit(0, "npc") - grid::unit(caption_area_offset_cm, "cm")
    linea_grob$y <- y_linea

    .map_to_return <- .map_to_return +
      ggplot2::annotation_custom(
        linea_grob,
        xmin = -Inf, xmax = Inf,
        ymin = -Inf, ymax = Inf
      ) +
      ggplot2::coord_sf(clip = "off")

    bottom_margin_pt <- bottom_margin_pt + 8
  }

  # Insertar logo si se ha indicado ruta
  if (!is.null(.logo_path)) {
    if (!file.exists(.logo_path)) {
      stop(paste("Archivo de logo no encontrado:", .logo_path))
    }

    img <- png::readPNG(.logo_path)
    dims <- dim(img)
    ar <- dims[1] / dims[2]

    # Autoscaling usando snpc
    logo_width <- grid::unit(.logo_escala * 2, "snpc")
    logo_height <- grid::unit(.logo_escala * 2 * ar, "snpc")

    margin_pad_cm <- 0.2

    x_pos <- NULL
    y_pos <- NULL
    hjust_val <- NULL
    vjust_val <- NULL

    # Para posiciones inferiores, alinear el logo verticalmente con el caption
    logo_y_bottom <- grid::unit(0, "npc") - grid::unit(caption_area_offset_cm + 0.5, "cm")

    # Configurar posición según .logo_posicion
    if (.logo_posicion == "bottomright") {
      x_pos <- grid::unit(1, "npc")
      y_pos <- logo_y_bottom
      hjust_val <- 1
      vjust_val <- 0.5
      bottom_margin_pt <- bottom_margin_pt + 40

    } else if (.logo_posicion == "bottomleft") {
      x_pos <- grid::unit(0, "npc")
      y_pos <- logo_y_bottom
      hjust_val <- 0
      vjust_val <- 0.5
      bottom_margin_pt <- bottom_margin_pt + 40

    } else if (.logo_posicion == "topright") {
      x_pos <- grid::unit(1, "npc")
      y_pos <- grid::unit(1, "npc") + grid::unit(margin_pad_cm, "cm")
      hjust_val <- 1
      vjust_val <- 0
      top_margin_pt <- top_margin_pt + 25

    } else if (.logo_posicion == "topleft") {
      x_pos <- grid::unit(0, "npc")
      y_pos <- grid::unit(1, "npc") + grid::unit(margin_pad_cm, "cm")
      hjust_val <- 0
      vjust_val <- 0
      top_margin_pt <- top_margin_pt + 25

    } else {
      stop("'.logo_posicion' debe ser 'topright', 'topleft', 'bottomright' o 'bottomleft'.")
    }

    logo_grob <- grid::rasterGrob(
      img,
      x = x_pos,
      y = y_pos,
      width = logo_width,
      height = logo_height,
      hjust = hjust_val,
      vjust = vjust_val,
      interpolate = TRUE
    )

    .map_to_return <- .map_to_return +
      ggplot2::annotation_custom(
        logo_grob,
        xmin = -Inf, xmax = Inf,
        ymin = -Inf, ymax = Inf
      ) +
      ggplot2::coord_sf(clip = "off")
  }

  # Aplicar márgenes acumulados una sola vez
  .map_to_return <- .map_to_return +
    ggplot2::theme(plot.margin = ggplot2::margin(
      t = top_margin_pt, r = right_margin_pt,
      b = bottom_margin_pt, l = left_margin_pt
    ))

  return(.map_to_return)
}

# Función general para dar formato a tablas ----

#' Función para aplicar estilo de tabla estándar para VictorGM
#'
#' @param .data Data frame con los datos a mostrar en la tabla
#' @param .title Cadena de caracteres. Título principal de la tabla. Por defecto, `NULL`.
#' @param .subtitle Cadena de caracteres. Subtítulo de la tabla. Por defecto, `NULL`.
#' @param .caption Cadena de caracteres. Nota al pie de la tabla. Por defecto, `NULL`.
#' @param .columnas_numericas Vector de caracteres. Nombres de las columnas que contienen valores numéricos. Por defecto, detecta automáticamente.
#' @param .columnas_porcentaje Vector de caracteres. Nombres de las columnas que representan porcentajes. Por defecto, `NULL`.
#' @param .columnas_colores_positivo_negativo Vector de caracteres. Columnas donde aplicar colores según valores positivos/negativos. Por defecto, `NULL`.
#' @param .color_positivo Cadena de caracteres. Color para valores positivos. Por defecto, `"#38A169"` (verde).
#' @param .color_negativo Cadena de caracteres. Color para valores negativos. Por defecto, `"#E53E3E"` (rojo).
#' @param .color_fondo_positivo Cadena de caracteres. Color de fondo para valores positivos. Por defecto, `"#F0FFF4"`.
#' @param .color_fondo_negativo Cadena de caracteres. Color de fondo para valores negativos. Por defecto, `"#FFF5F5"`.
#' @param .columnas_separador_derecha Vector de caracteres. Columnas después de las cuales añadir un separador vertical. Por defecto, `NULL`.
#' @param .color_encabezado Cadena de caracteres. Color de fondo de los encabezados. Por defecto, `"#2B3A67"`.
#' @param .color_texto_encabezado Cadena de caracteres. Color del texto de los encabezados. Por defecto, `"#FFFFFF"`.
#' @param .color_borde_encabezado Cadena de caracteres. Color del borde inferior de los encabezados. Por defecto, `"#FCD757"`.
#' @param .color_filas_alternas Cadena de caracteres. Color de fondo para filas alternas. Por defecto, `"#F7F9FC"`.
#' @param .precision_decimales Número. Decimales para valores numéricos. Por defecto, `2`.
#' @param .formato_miles Cadena de caracteres. Separador de miles. Por defecto, `"."`.
#' @param .formato_decimal Cadena de caracteres. Separador decimal. Por defecto, `","`.
#' @param .font_size Cadena de caracteres. Tamaño de fuente para toda la tabla. Por defecto, `14`.
#' @param .fuente_letra Cadena de caracteres. Fuente tipográfica. Por defecto, `"Source Sans 3"`.
#' @param .padding_celdas Número. Espaciado interno de las celdas en píxeles. Por defecto, `10`.
#' @param .agrupaciones Lista con agrupaciones de columnas. Por defecto, `NULL`.
#' @param .ancho_tabla Cadena de caracteres. Ancho de la tabla. Por defecto, `"100%"`.
#' @param .centrar_columnas Vector de caracteres. Columnas a centrar. Por defecto, columnas numéricas.
#' @param .primera_columna_fija Lógico. Si TRUE, mantiene la primera columna como etiqueta fija. Por defecto, `TRUE`.
#' @param .resaltar_filas Vector numérico. Índices de filas a resaltar. Por defecto, `NULL`.
#' @param .color_resaltado Cadena de caracteres. Color de fondo para filas resaltadas. Por defecto, `"#FFFACD"`.
#' @param .logo_path Cadena de caracteres. Ruta a una imagen (PNG o JPEG) para incluir como logo en la tabla. Se muestra alineado a la derecha bajo la tabla. Por defecto, `NULL` (sin logo).
#'
#' @examples
#' # Ejemplo básico con datos de comercio exterior
#' datos_comercio <- data.frame(
#'   Concepto = c("Exportaciones", "Importaciones", "Saldo"),
#'   Mensual_Jun_2025 = c(33.77, 37.36, -3.59),
#'   Var_Jun_2025 = c(2.42, 10.91, -2.88),
#'   Mensual_May_2025 = c(35.00, 37.54, -2.54),
#'   Var_May_2025 = c(0.82, 1.29, -194.09),
#'   Acumulado_12_meses = c(386.51, 436.08, -49.57),
#'   Var_Acumulado = c(2.02, 4.12, -9.60)
#' )
#'
#' # Uso básico
#' tabla_estilo_victorgm(
#'   .data = datos_comercio,
#'   .title = "España - Comercio Exterior",
#'   .subtitle = "Millones de euros y variaciones interanuales (%)",
#'   .caption = "Fuente: DataComex. Elaboración: Víctor Gutiérrez Marcos"
#' )
#'
#' @export
tablas_estilo_victorgm <- function(
    .data,
    .title = NULL,
    .subtitle = NULL,
    .caption = NULL,
    .title_size = 18,
    .subtitle_size = 14,
    .caption_size = 12,
    .columnas_numericas = NULL,
    .columnas_variacion = NULL,
    .forzar_signo_variacion = TRUE,
    .columnas_cuota = NULL,
    .columnas_colores_positivo_negativo = NULL,
    .color_positivo = "#4A9079",
    .color_negativo = "#A63A50",
    .color_fondo_positivo = "#EDF5F2",
    .color_fondo_negativo = "#F5EDEF",
    .columnas_separador_derecha = NULL,
    .color_encabezado = "#5F2987",
    .color_texto_encabezado = "#FFFFFF",
    .color_borde_encabezado = "#B8860B",
    .color_filas_alternas = "#F8F5FC",
    .precision_decimales = 2,
    .formato_miles = ".",
    .formato_decimal = ",",
    .font_size = 14,
    .fuente_letra = "Source Sans 3",
    .padding_celdas = 10,
    .agrupaciones = NULL,
    .ancho_tabla = "100%",
    .centrar_columnas = NULL,
    .primera_columna_fija = TRUE,
    .resaltar_filas = NULL,
    .color_resaltado = "#FFFACD",
    .usar_google_fonts = FALSE,
    .logo_path = NULL
) {
  
  # Cargar librerías necesarias
  if (!requireNamespace("gt", quietly = TRUE)) {
    stop("El paquete 'gt' es necesario. Por favor, instálalo con: install.packages('gt')")
  }
  
  # Validación de entrada
  if (!is.data.frame(.data)) {
    stop("El argumento .data debe ser un data frame")
  }
  
  if (nrow(.data) == 0) {
    warning("El data frame está vacío")
    return(gt::gt(.data))
  }
  
  # Detectar automáticamente columnas numéricas si no se especifican
  if (is.null(.columnas_numericas)) {
    .columnas_numericas <- names(.data)[sapply(.data, is.numeric)]
  }
  
  # Centrar columnas numéricas por defecto si no se especifica
  if (is.null(.centrar_columnas)) {
    .centrar_columnas <- .columnas_numericas
  }
  
  # Crear tabla base
  .tabla <- gt::gt(.data) |>
    # Configuración general
    gt::tab_options(
      table.border.top.style = "hidden",
      table.border.bottom.style = "hidden",
      table_body.border.bottom.style = "hidden",
      data_row.padding = gt::px(.padding_celdas),
      table.width = .ancho_tabla,
      column_labels.border.bottom.style = "solid",
      column_labels.border.bottom.width = gt::px(3),
      column_labels.border.bottom.color = .color_borde_encabezado
    )
  
  # CONFIGURAR FUENTES
  # Para fuentes conocidas de Google Fonts, usar gt::google_font() como primaria
  # con fallback a fuentes del sistema. Esto garantiza que la fuente se cargue
  # en HTML sin necesidad de instalacion local.
  es_google_font <- .fuente_letra %in% names(.fuentes_google)

  if (es_google_font || .usar_google_fonts) {
    nombre_google <- if (es_google_font) .fuentes_google[[.fuente_letra]] else .fuente_letra
    tryCatch({
      .tabla <- .tabla |>
        gt::opt_table_font(
          font = list(
            gt::google_font(nombre_google),
            .fuente_letra,
            "system-ui",
            "sans-serif"
          ),
          size = paste0(.font_size, "px")
        )
    }, error = function(e) {
      .tabla <<- configurar_fuente_tabla(.tabla, .fuente_letra, .font_size)
    })
  } else {
    .tabla <- configurar_fuente_tabla(.tabla, .fuente_letra, .font_size)
  }
  
  # Añadir título y subtítulo - CORREGIDO para compatibilidad HTML/LaTeX
  if (!is.null(.title)) {
    if (!is.null(.subtitle)) {
      # Detectar formato de salida
      if (exists("knitr") && requireNamespace("knitr", quietly = TRUE)) {
        if (knitr::is_latex_output()) {
          # Para LaTeX/PDF: usar texto plano sin HTML
          .tabla <- .tabla |>
            gt::tab_header(
              title = .title,
              subtitle = .subtitle
            ) |>
            gt::tab_style(
              style = list(
                gt::cell_text(size = gt::px(.title_size), weight = "bold")
              ),
              locations = gt::cells_title(groups = "title")
            ) |>
            gt::tab_style(
              style = list(
                gt::cell_text(size = gt::px(.subtitle_size))
              ),
              locations = gt::cells_title(groups = "subtitle")
            )
        } else {
          # Para HTML: usar HTML con estilos inline
          .tabla <- .tabla |>
            gt::tab_header(
              title = gt::html(paste0("<strong style='font-size:", .title_size, "px'>", .title, "</strong>")),
              subtitle = gt::html(paste0("<span style='font-size:", .subtitle_size, "px'>", .subtitle, "</span>"))
            )
        }
      } else {
        # Si knitr no está disponible, asumir HTML
        .tabla <- .tabla |>
          gt::tab_header(
            title = gt::html(paste0("<strong style='font-size:", .title_size, "px'>", .title, "</strong>")),
            subtitle = gt::html(paste0("<span style='font-size:", .subtitle_size, "px'>", .subtitle, "</span>"))
          )
      }
    } else {
      # Solo título
      if (exists("knitr") && requireNamespace("knitr", quietly = TRUE) && knitr::is_latex_output()) {
        .tabla <- .tabla |>
          gt::tab_header(title = .title) |>
          gt::tab_style(
            style = list(
              gt::cell_text(size = gt::px(.title_size), weight = "bold")
            ),
            locations = gt::cells_title(groups = "title")
          )
      } else {
        .tabla <- .tabla |>
          gt::tab_header(
            title = gt::html(paste0("<strong style='font-size:", .title_size, "px'>", .title, "</strong>"))
          )
      }
    }
  }
  
  # Añadir nota al pie si se proporciona - CORREGIDO
  if (!is.null(.caption)) {
    # if (exists("knitr") && requireNamespace("knitr", quietly = TRUE) && knitr::is_latex_output()) {
    # Para LaTeX: texto plano
    .tabla <- .tabla |>
      gt::tab_source_note(source_note = .caption) |>
      gt::tab_style(
        style = list(
          gt::cell_text(size = gt::px(.caption_size))
        ),
        locations = gt::cells_source_notes()
      )
    # } else {
    #   # Para HTML: con estilo
    #   .tabla <- .tabla |>
    #     gt::tab_source_note(
    #       source_note = gt::html(paste0("<span style='font-size:", .caption_size, "px'>", .caption, "</span>"))
    #     )
    # }
  }
  
  # Aplicar agrupaciones si se proporcionan
  if (!is.null(.agrupaciones)) {
    for (i in seq_along(.agrupaciones)) {
      grupo <- .agrupaciones[[i]]
      # Verificar que las columnas existan
      cols_grupo <- intersect(grupo$columns, names(.data))
      if (length(cols_grupo) > 0) {
        .tabla <- .tabla |>
          gt::tab_spanner(
            label = grupo$label,
            columns = gt::all_of(cols_grupo)
          )
      }
    }
  }
  
  # Estilo de encabezados
  .tabla <- .tabla |>
    gt::tab_style(
      style = list(
        gt::cell_fill(color = .color_encabezado),
        gt::cell_text(color = .color_texto_encabezado, weight = "bold", align = "center")
      ),
      locations = gt::cells_column_labels(columns = gt::everything())
    )
  
  # Estilo para agrupaciones de columnas (spanners)
  if (!is.null(.agrupaciones)) {
    .tabla <- .tabla |>
      gt::tab_style(
        style = list(
          gt::cell_fill(color = .color_encabezado),
          gt::cell_text(color = .color_texto_encabezado, weight = "bold", align = "center")
        ),
        locations = gt::cells_column_spanners(spanners = gt::everything())
      )
  }
  
  # Primera columna como etiqueta fija (si aplica)
  if (.primera_columna_fija && ncol(.data) > 1) {
    primera_col <- names(.data)[1]
    .tabla <- .tabla |>
      gt::tab_style(
        style = list(
          gt::cell_text(weight = "bold", align = "left"),
          gt::cell_fill(color = .color_filas_alternas)
        ),
        locations = gt::cells_body(columns = gt::all_of(primera_col))
      )
  }
  
  # Filas alternas
  cols_alternas <- if (.primera_columna_fija && ncol(.data) > 1) {
    names(.data)[-1]
  } else {
    names(.data)
  }
  
  if (nrow(.data) > 1 && length(cols_alternas) > 0) {
    filas_pares <- seq(2, nrow(.data), by = 2)
    if (length(filas_pares) > 0) {
      .tabla <- .tabla |>
        gt::tab_style(
          style = gt::cell_fill(color = .color_filas_alternas),
          locations = gt::cells_body(
            columns = gt::all_of(cols_alternas),
            rows = filas_pares
          )
        )
    }
  }
  
  # Resaltar filas específicas
  if (!is.null(.resaltar_filas)) {
    filas_validas <- .resaltar_filas[.resaltar_filas <= nrow(.data) & .resaltar_filas > 0]
    if (length(filas_validas) > 0) {
      .tabla <- .tabla |>
        gt::tab_style(
          style = gt::cell_fill(color = .color_resaltado),
          locations = gt::cells_body(rows = filas_validas)
        )
    }
  }
  
  # Formateo de columnas numéricas
  if (length(.columnas_numericas) > 0) {
    cols_num_existentes <- intersect(.columnas_numericas, names(.data))
    
    for (col in cols_num_existentes) {
      es_variacion <- !is.null(.columnas_variacion) && col %in% .columnas_variacion
      es_cuota <- !is.null(.columnas_cuota) && col %in% .columnas_cuota
      
      if (es_variacion) {
        valores_col <- .data[[col]]
        valores_no_na <- valores_col[!is.na(valores_col)]
        
        if (length(valores_no_na) > 0) {
          .tabla <- .tabla |>
            gt::fmt_percent(
              columns = gt::all_of(col),
              decimals = .precision_decimales,
              dec_mark = .formato_decimal,
              sep_mark = .formato_miles,
              force_sign = .forzar_signo_variacion
            )
        }
      } else if (es_cuota) {
        valores_col <- .data[[col]]
        valores_no_na <- valores_col[!is.na(valores_col)]
        
        if (length(valores_no_na) > 0) {
          .tabla <- .tabla |>
            gt::fmt_percent(
              columns = gt::all_of(col),
              decimals = .precision_decimales,
              dec_mark = .formato_decimal,
              sep_mark = .formato_miles,
              force_sign = FALSE
            )
        }
      } else {
        .tabla <- .tabla |>
          gt::fmt_number(
            columns = gt::all_of(col),
            decimals = .precision_decimales,
            dec_mark = .formato_decimal,
            sep_mark = .formato_miles
          )
      }
    }
  }
  
  # Centrar columnas especificadas
  if (length(.centrar_columnas) > 0) {
    cols_existentes <- intersect(.centrar_columnas, names(.data))
    if (length(cols_existentes) > 0) {
      .tabla <- .tabla |>
        gt::cols_align(
          align = "center",
          columns = gt::all_of(cols_existentes)
        )
    }
  }
  
  # Aplicar colores para valores positivos y negativos
  if (!is.null(.columnas_colores_positivo_negativo)) {
    cols_colores <- intersect(.columnas_colores_positivo_negativo, names(.data))
    
    for (col in cols_colores) {
      if (col %in% names(.data)) {
        valores <- .data[[col]]
        
        if (any(!is.na(valores))) {
          filas_negativas <- which(!is.na(valores) & valores < 0)
          if (length(filas_negativas) > 0) {
            .tabla <- .tabla |>
              gt::tab_style(
                style = list(
                  gt::cell_fill(color = .color_fondo_negativo),
                  gt::cell_text(color = .color_negativo, weight = "bold")
                ),
                locations = gt::cells_body(
                  columns = gt::all_of(col),
                  rows = filas_negativas
                )
              )
          }
          
          filas_positivas <- which(!is.na(valores) & valores > 0)
          if (length(filas_positivas) > 0) {
            .tabla <- .tabla |>
              gt::tab_style(
                style = list(
                  gt::cell_fill(color = .color_fondo_positivo),
                  gt::cell_text(color = .color_positivo, weight = "bold")
                ),
                locations = gt::cells_body(
                  columns = gt::all_of(col),
                  rows = filas_positivas
                )
              )
          }
        }
      }
    }
  }
  
  # Añadir separadores verticales
  if (!is.null(.columnas_separador_derecha)) {
    cols_separador <- intersect(.columnas_separador_derecha, names(.data))
    
    for (col in cols_separador) {
      .tabla <- .tabla |>
        gt::tab_style(
          style = gt::cell_borders(
            sides = "right",
            color = .color_borde_encabezado,
            weight = gt::px(2),
            style = "solid"
          ),
          locations = gt::cells_body(columns = gt::all_of(col))
        ) |>
        gt::tab_style(
          style = gt::cell_borders(
            sides = "right",
            color = .color_borde_encabezado,
            weight = gt::px(2),
            style = "solid"
          ),
          locations = gt::cells_column_labels(columns = gt::all_of(col))
        )
    }
  }

  # Insertar logo si se ha indicado ruta
  if (!is.null(.logo_path)) {
    if (!file.exists(.logo_path)) {
      stop(paste("Archivo de logo no encontrado:", .logo_path))
    }

    logo_html <- gt::local_image(file = .logo_path, height = 40)
    .tabla <- .tabla |>
      gt::tab_source_note(
        gt::html(paste0(
          '<div style="text-align: right; padding-top: 5px;">',
          logo_html,
          '</div>'
        ))
      )
  }

  return(.tabla)
}

# Función de alias - mantener compatibilidad con el nombre sin 's'

#' Alias de tablas_estilo_victorgm
#'
#' Alias para mantener compatibilidad con versiones anteriores.
#' Use \code{\link{tablas_estilo_victorgm}} para la documentación completa.
#'
#' @inheritParams tablas_estilo_victorgm
#' @export
tabla_estilo_victorgm <- tablas_estilo_victorgm

#' Configurar fuente tipográfica en tabla gt
#'
#' Configura la fuente tipográfica de una tabla gt con fallbacks automáticos
#' según la fuente especificada.
#'
#' @param .tabla Objeto gt al que aplicar la configuración de fuente.
#' @param .fuente_letra Cadena de caracteres. Nombre de la fuente principal. Por defecto, "Source Sans 3".
#' @param .font_size Número. Tamaño de la fuente en píxeles.
#' @return Objeto gt con la fuente configurada.
#' @export
configurar_fuente_tabla <- function(.tabla, .fuente_letra, .font_size) {
  # Stack de fuentes fallback según el sistema
  fuentes_sistema <- list(
    "Source Sans 3" = list("Source Sans 3", "Source Sans Pro", "Segoe UI", "Arial", "sans-serif"),
    "Source Sans Pro" = list("Source Sans Pro", "Source Sans 3", "Segoe UI", "Arial", "sans-serif"),
    "Open Sans" = list("Open Sans", "Segoe UI", "Helvetica", "Arial", "sans-serif"),
    "Lato" = list("Lato", "Segoe UI", "Helvetica", "Arial", "sans-serif"),
    "Roboto" = list("Roboto", "Segoe UI", "Helvetica", "Arial", "sans-serif"),
    "Segoe UI" = list("Segoe UI", "Tahoma", "Geneva", "Verdana", "sans-serif"),
    "Arial" = list("Arial", "Helvetica", "sans-serif"),
    "Times" = list("Times New Roman", "Times", "serif"),
    "Courier" = list("Courier New", "Courier", "monospace"),
    "Helvetica" = list("Helvetica", "Arial", "sans-serif")
  )
  
  # Obtener el stack de fuentes o usar genérico
  font_stack <- if (.fuente_letra %in% names(fuentes_sistema)) {
    fuentes_sistema[[.fuente_letra]]
  } else {
    list(.fuente_letra, "system-ui", "sans-serif")
  }
  
  # Configurar fuente sin Google Fonts
  .tabla |>
    gt::opt_table_font(
      font = font_stack,
      size = paste0(.font_size, "px")
    )
}

# Función auxiliar para crear agrupaciones de columnas fácilmente
#' Crear agrupación de columnas para tabla gt
#'
#' @param label Etiqueta de la agrupación
#' @param columns Vector de nombres de columnas a agrupar
#' @return Lista con label y columns
#' @export
crear_agrupacion <- function(label, columns) {
  if (!is.character(label) || length(label) != 1) {
    stop("El argumento 'label' debe ser una cadena de caracteres")
  }
  if (!is.character(columns) || length(columns) == 0) {
    stop("El argumento 'columns' debe ser un vector de caracteres no vacío")
  }
  return(list(label = label, columns = columns))
}