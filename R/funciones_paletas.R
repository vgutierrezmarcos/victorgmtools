# Tokens de diseño de victorgutierrezmarcos.es ----

#' Tokens de color de victorgutierrezmarcos.es
#'
#' Devuelve los colores del sistema de diseño de la web (`styles.css`): morado como tinta, verde
#' salvia como papel y oro como filete. Con `.oscuro = TRUE` devuelve la variante del tema oscuro
#' (berenjena). Sirve para que gráficos, tablas e informes usen los mismos tokens que la página.
#'
#' @param .oscuro Valor lógico. Si `TRUE`, los tokens del tema oscuro. Por defecto, `FALSE`.
#' @return Vector nombrado de colores: `primario`, `primario_oscuro`, `primario_claro`, `primario_palido`,
#'   `fondo`, `fondo_claro`, `fondo_crema`, `superficie`, `texto`, `texto_suave`, `texto_claro`, `borde`,
#'   `borde_fuerte`, `borde_claro`, `oro`, `oro_claro`, `exito`, `exito_fondo`, `error`, `error_fondo`, `aviso_fondo`.
#' @examples
#' tokens_victorgm()[["primario"]]
#' tokens_victorgm(.oscuro = TRUE)[["fondo"]]
#' @export
tokens_victorgm <- function(.oscuro = FALSE) {
  if (isTRUE(.oscuro)) {
    return(c(
      primario = "#c3a2e0", primario_oscuro = "#a57fcb", primario_claro = "#d4bdeb", primario_palido = "#2e2538",
      fondo = "#16131a", fondo_claro = "#1b1820", fondo_crema = "#211d26", superficie = "#211d26",
      texto = "#ece6ef", texto_suave = "#b3aabb", texto_claro = "#8f8698",
      borde = "#3a3342", borde_fuerte = "#544a5e", borde_claro = "#2e2935",
      oro = "#d9ae45", oro_claro = "#e6c46e",
      exito = "#8dba84", exito_fondo = "#1f2a1d", error = "#e0899a", error_fondo = "#3a1e25", aviso_fondo = "#33291a"
    ))
  }
  c(
    primario = "#5F2987", primario_oscuro = "#4a1f6b", primario_claro = "#7a3ca8", primario_palido = "#f1ebf6",
    fondo = "#E2EFD9", fondo_claro = "#edf5e7", fondo_crema = "#fbfaf6", superficie = "#ffffff",
    texto = "#2a2530", texto_suave = "#5e5866", texto_claro = "#7a7480",
    borde = "#c8d8c0", borde_fuerte = "#a9bb9e", borde_claro = "#dde8d6",
    oro = "#b8860b", oro_claro = "#daa520",
    exito = "#3f6b3a", exito_fondo = "#e6efdf", error = "#8e2a3b", error_fondo = "#f6e4e6", aviso_fondo = "#f6eed8"
  )
}

#' Colores categóricos de victorgmtools ordenados por contraste
#'
#' Reordena [colores_victorgm()] de forma que los primeros colores sean los más distinguibles entre
#' sí (morado, oro, verde, pizarra, azul, naranja, rojo...), dejando los tonos claros y las
#' variantes para el final. Es la secuencia con la que se construyen las paletas temáticas.
#'
#' @param n Número de colores. Por defecto, `NULL` (los 30).
#' @return Vector de colores hexadecimales. Si `n` supera 30, se interpolan.
#' @examples
#' colores_victorgm_categoricos(5)
#' @export
colores_victorgm_categoricos <- function(n = NULL) {
  orden <- c(1, 2, 3, 8, 9, 10, 7, 11, 18, 20, 15, 16, 17, 28, 19, 22, 4, 5, 6, 14, 12, 13, 29, 23, 24, 25, 26, 27, 21, 30)
  colores <- colores_victorgm()[orden]
  if (is.null(n)) return(colores)
  if (n <= length(colores)) return(colores[seq_len(n)])
  grDevices::colorRampPalette(colores)(n)
}

#' Paleta secuencial de victorgmtools
#'
#' Rampa de un solo tono (morado por defecto) de claro a oscuro, para una magnitud que va de menos
#' a más (cuotas, intensidades). Pensada para mapas coropléticos y mapas de calor.
#'
#' @param n Número de colores. Por defecto, `6`.
#' @param .color Color base. Por defecto, el morado de la web (`"#5F2987"`).
#' @return Vector de `n` colores hexadecimales.
#' @examples
#' paleta_secuencial_victorgm(5)
#' @export
paleta_secuencial_victorgm <- function(n = 6, .color = "#5F2987") {
  stopifnot(is.numeric(n), n >= 1)
  extremos <- c(tokens_victorgm()[["primario_palido"]], .color, tokens_victorgm()[["primario_oscuro"]])
  if (.color != "#5F2987") {
    rgb <- grDevices::col2rgb(.color) / 255
    extremos <- c(grDevices::rgb(0.9 + 0.1 * rgb[1], 0.9 + 0.1 * rgb[2], 0.9 + 0.1 * rgb[3]),
                  .color,
                  grDevices::rgb(rgb[1] * 0.7, rgb[2] * 0.7, rgb[3] * 0.7))
  }
  grDevices::colorRampPalette(extremos)(n)
}

#' Paleta divergente de victorgmtools
#'
#' Rampa para magnitudes con signo (saldos, variaciones): del granate de error al verde de éxito
#' de la web, centrada en el crema del papel.
#'
#' @param n Número de colores (impar para que el centro sea el neutro). Por defecto, `7`.
#' @return Vector de `n` colores hexadecimales.
#' @examples
#' paleta_divergente_victorgm(5)
#' @export
paleta_divergente_victorgm <- function(n = 7) {
  stopifnot(is.numeric(n), n >= 2)
  tk <- tokens_victorgm()
  grDevices::colorRampPalette(c("#5e1a28", tk[["error"]], "#d9a3ad", tk[["fondo_crema"]], "#9cc294", tk[["exito"]], "#24421f"))(n)
}

#' Paleta de signo (sube, baja, neutro)
#'
#' Colores para codificar el signo de una variación con los tokens de éxito y error de la web.
#' El signo no debe ser la única codificación (por daltonismo): acompáñalo de posición o etiqueta.
#'
#' @return Vector nombrado con `sube`, `baja` y `neutro`.
#' @examples
#' paleta_signo_victorgm()[["sube"]]
#' @export
paleta_signo_victorgm <- function() {
  tk <- tokens_victorgm()
  c(sube = tk[["exito"]], baja = tk[["error"]], neutro = tk[["texto_suave"]])
}

# Paletas temáticas ----

# Construye una paleta con varias claves por color: `claves` es una lista de vectores de nombres
.paleta_multiclave <- function(claves, colores) {
  stopifnot(length(claves) == length(colores))
  nombres <- unlist(claves, use.names = FALSE)
  valores <- rep(colores, times = lengths(claves))
  stats::setNames(valores, nombres)
}

.gris_resto <- "#9DA9AA"

#' Paleta de exportaciones, importaciones y saldo
#'
#' Exportaciones en morado, importaciones en oro y saldo en verde. Incluye las claves `"E"` e `"I"`.
#'
#' @return Vector nombrado de colores.
#' @examples
#' paleta_exportaciones_importaciones()
#' @export
paleta_exportaciones_importaciones <- function() {
  .paleta_multiclave(
    list(c("Exportaciones", "Exportaci\u00f3n", "E", "X"),
         c("Importaciones", "Importaci\u00f3n", "I", "M"),
         c("Saldo", "Saldo comercial")),
    c("#5F2987", "#B8860B", "#4A9079")
  )
}

#' Paleta de productos energéticos y no energéticos
#'
#' @return Vector nombrado de colores.
#' @examples
#' paleta_energeticos()
#' @export
paleta_energeticos <- function() {
  .paleta_multiclave(
    list(c("Energ\u00e9ticos", "Productos energ\u00e9ticos"),
         c("No energ\u00e9ticos", "Productos no energ\u00e9ticos"),
         c("Total")),
    c("#B8860B", "#5F2987", "#2C3E50")
  )
}

#' Paleta de subsectores energéticos (clasificación SEC)
#'
#' @return Vector nombrado de colores.
#' @examples
#' paleta_subsectores_energeticos_sec()
#' @export
paleta_subsectores_energeticos_sec <- function() {
  .paleta_multiclave(
    list(c("Carbon", "Carb\u00f3n"),
         c("Gas"),
         c("Petroleo y derivados", "Petr\u00f3leo y derivados"),
         c("Corriente electrica", "Corriente el\u00e9ctrica"),
         c("Total energ\u00e9ticos")),
    c("#843C0C", "#D4A843", "#A63A50", "#4A9079", "#B8860B")
  )
}

#' Paleta de subsectores energéticos (clasificación CUCI)
#'
#' @return Vector nombrado de colores.
#' @examples
#' paleta_subsectores_energeticos_cuci()
#' @export
paleta_subsectores_energeticos_cuci <- function() {
  .paleta_multiclave(
    list(c("Hulla, coque y briquetas"),
         c("Gas natural y manufacturado"),
         c("Petr\u00f3leo, productos derivados del petr\u00f3leo y productos conexos"),
         c("Corriente el\u00e9ctrica"),
         c("Total energ\u00e9ticos")),
    c("#843C0C", "#D4A843", "#A63A50", "#4A9079", "#B8860B")
  )
}

#' Paleta de efecto precio y efecto volumen
#'
#' Precio en oro, volumen en morado y variación total en pizarra. Las claves coinciden con las
#' etiquetas que usa [get_descomposicion_tv_plt()].
#'
#' @return Vector nombrado de colores.
#' @examples
#' paleta_efecto_precio_volumen()
#' @export
paleta_efecto_precio_volumen <- function() {
  .paleta_multiclave(
    list(c("Efecto precio", "Contribuci\u00f3n precio", "Contribuci\u00f3n del precio", "Precio"),
         c("Efecto cantidad", "Efecto volumen", "Contribuci\u00f3n cantidad", "Contribuci\u00f3n volumen",
           "Contribuci\u00f3n del volumen", "Volumen", "Cantidad"),
         c("Variaci\u00f3n total", "Nominal", "Total", "Tasa de variaci\u00f3n")),
    c("#B8860B", "#5F2987", "#2C3E50")
  )
}

#' Paleta de las rúbricas de la balanza de pagos
#'
#' @return Vector nombrado de colores.
#' @examples
#' paleta_balanza_de_pagos()
#' @export
paleta_balanza_de_pagos <- function() {
  c(
    "Bienes" = "#5F2987",
    "Servicios" = "#4A9079",
    "Servicios totales" = "#4A9079",
    "Turismo" = "#B8860B",
    "Servicios no tur\u00edsticos" = "#6BB39E",
    "Rentas primarias" = "#9B6BC6",
    "Rentas secundarias" = "#D4A843",
    "Cuenta de capital" = "#A5A5A5",
    "Cap/Nec de Financiaci\u00f3n" = "#2C3E50",
    "Capacidad/Necesidad de financiaci\u00f3n" = "#2C3E50"
  )
}

#' Paleta de los sectores económicos (clasificación SEC de DATACOMEX)
#'
#' Admite el nombre con y sin tilde, el código (`"1"`, `"01"`) y la etiqueta `"1 - ALIMENTACIÓN..."`.
#'
#' @return Vector nombrado de colores.
#' @examples
#' paleta_sectores_sec()[["Bienes de equipo"]]
#' @export
paleta_sectores_sec <- function() {
  sectores <- list(
    c("Alimentacion, bebidas y tabaco", "Alimentaci\u00f3n, bebidas y tabaco", "ALIMENTACI\u00d3N, BEBIDAS Y TABACO"),
    c("Productos energeticos", "Productos energ\u00e9ticos", "PRODUCTOS ENERGETICOS"),
    c("Materias primas", "MATERIAS PRIMAS"),
    c("Semimanufacturas", "SEMIMANUFACTURAS"),
    c("Bienes de equipo", "BIENES DE EQUIPO"),
    c("Sector automovil", "Sector autom\u00f3vil", "SECTOR AUTOMOVIL"),
    c("Bienes de consumo duradero", "BIENES DE CONSUMO DURADERO"),
    c("Manufacturas de consumo", "MANUFACTURAS DE CONSUMO"),
    c("Otras mercancias", "Otras mercanc\u00edas", "OTRAS MERCANCIAS")
  )
  claves <- lapply(seq_along(sectores), function(i) {
    mayus <- sectores[[i]][length(sectores[[i]])]
    c(sectores[[i]][-length(sectores[[i]])],
      paste0(i, " - ", mayus), paste0("0", i, " - ", mayus), as.character(i), paste0("0", i))
  })
  .paleta_multiclave(claves, c(colores_victorgm_categoricos(8), .gris_resto))
}

#' Paleta de los sectores de la clasificación CUCI
#'
#' Admite el nombre, el código (`"0"` a `"9"`) y la etiqueta `"0 - Productos alimenticios..."`.
#'
#' @return Vector nombrado de colores.
#' @examples
#' paleta_sectores_cuci()[["7"]]
#' @export
paleta_sectores_cuci <- function() {
  nombres <- c(
    "Productos alimenticios y animales vivos",
    "Bebidas y tabacos",
    "Materiales crudos no comestibles, excepto los combustibles",
    "Combustibles y lubricantes minerales y productos conexos",
    "Aceites, grasas y ceras de origen animal y vegetal",
    "Productos qu\u00edmicos y productos conexos, n.e.p.",
    "Art\u00edculos manufacturados, clasificados principalmente seg\u00fan el material",
    "Maquinaria y equipo de transporte",
    "Art\u00edculos manufacturados diversos",
    "Mercanc\u00edas y operaciones no clasificadas en otro rubro de la CUCI"
  )
  claves <- lapply(seq_along(nombres), function(i) {
    cod <- as.character(i - 1)
    c(paste0(cod, " - ", nombres[i]), nombres[i], cod)
  })
  .paleta_multiclave(claves, c(colores_victorgm_categoricos(9), .gris_resto))
}

#' Paleta de las secciones del arancel (TARIC/SA)
#'
#' Admite el numeral romano de la sección, el nombre oficial en español y en inglés y un nombre corto.
#'
#' @return Vector nombrado de colores.
#' @examples
#' paleta_secciones_taric()[["XVI"]]
#' @export
paleta_secciones_taric <- function() {
  secciones <- list(
    c("I", "Animales vivos y productos del reino animal", "Live animals; animal products", "Animales y derivados"),
    c("II", "Productos del reino vegetal", "Vegetable products", "Vegetales, plantas y frutas"),
    c("III", "Grasas y aceites, animales, vegetales o de origen microbiano, y productos de su desdoblamiento; grasas alimenticias elaboradas; ceras de origen animal o vegetal",
      "Animal or vegetable fats and oils and their cleavage products; prepared edible fats; animal or vegetable waxes", "Grasas y aceites"),
    c("IV", "Productos de las industrias alimentarias; bebidas, l\u00edquidos alcoh\u00f3licos y vinagre; tabaco y suced\u00e1neos del tabaco elaborados; productos, incluso con nicotina, destinados para la inhalaci\u00f3n sin combusti\u00f3n; otros productos que contengan nicotina destinados para la absorci\u00f3n de nicotina en el cuerpo humano",
      "Prepared foodstuffs; beverages, spirits and vinegar; tobacco and manufactured tobacco substitutes", "Alimentos procesados, bebidas y tabaco"),
    c("V", "Productos minerales", "Mineral products", "Minerales y energ\u00eda"),
    c("VI", "Productos de las industrias qu\u00edmicas o de las industrias conexas", "Products of the chemical or allied industries", "Productos qu\u00edmicos y farmac\u00e9uticos"),
    c("VII", "Pl\u00e1stico y sus manufacturas; caucho y sus manufacturas", "Plastics and articles thereof; rubber and articles thereof", "Pl\u00e1sticos y caucho"),
    c("VIII", "Pieles, cueros, peleter\u00eda y manufacturas de estas materias; art\u00edculos de talabarter\u00eda o guarnicioner\u00eda; art\u00edculos de viaje, bolsos de mano (carteras) y continentes similares; manufacturas de tripa",
      "Raw hides and skins, leather, furskins and articles thereof; saddlery and harness; travel goods, handbags and similar containers; articles of animal gut (other than silk-worm gut)", "Pieles, cuero y equipaje"),
    c("IX", "Madera, carb\u00f3n vegetal y manufacturas de madera; corcho y sus manufacturas; manufacturas de esparter\u00eda o cester\u00eda",
      "Wood and articles of wood; wood charcoal; cork and articles of cork; manufactures of straw, of esparto or of other plaiting materials; basketware and wickerwork", "Madera y corcho"),
    c("X", "Pasta de madera o de las dem\u00e1s materias fibrosas celul\u00f3sicas; papel o cart\u00f3n para reciclar (desperdicios y desechos); papel o cart\u00f3n y sus aplicaciones",
      "Pulp of wood or of other fibrous cellulosic material; recovered (waste and scrap) paper or paperboard; paper and paperboard and articles thereof", "Papel, cart\u00f3n y celulosa"),
    c("XI", "Materias textiles y sus manufacturas", "Textiles and textile articles", "Textiles y ropa"),
    c("XII", "Calzado, sombreros y dem\u00e1s tocados, paraguas, quitasoles, bastones, l\u00e1tigos, fustas y sus partes; plumas preparadas y art\u00edculos de plumas; flores artificiales; manufacturas de cabello",
      "Footwear, headgear, umbrellas, sun umbrellas, walking-sticks, seat-sticks, whips, riding-crops and parts thereof; prepared feathers and articles made therewith; artificial flowers; articles of human hair", "Calzado y accesorios de moda"),
    c("XIII", "Manufacturas de piedra, yeso fraguable, cemento, amianto (asbesto), mica o materias an\u00e1logas; productos cer\u00e1micos; vidrio y sus manufacturas",
      "Articles of stone, plaster, cement, asbestos, mica or similar materials; ceramic products; glass and glassware", "Piedra, cer\u00e1mica y vidrio"),
    c("XIV", "Perlas finas (naturales) o cultivadas, piedras preciosas o semipreciosas, metales preciosos, chapados de metal precioso (plaqu\u00e9) y manufacturas de estas materias; bisuter\u00eda; monedas",
      "Natural or cultured pearls, precious or semi-precious stones, precious metals, metals clad with precious metal and articles thereof; imitation jewellery; coin", "Joyer\u00eda y metales preciosos"),
    c("XV", "Metales comunes y manufacturas de estos metales", "Base metals and articles of base metal", "Metalurgia"),
    c("XVI", "M\u00e1quinas y aparatos, material el\u00e9ctrico y sus partes; aparatos de grabaci\u00f3n o reproducci\u00f3n de sonido, aparatos de grabaci\u00f3n o reproducci\u00f3n de im\u00e1genes y sonido en televisi\u00f3n, y las partes y accesorios de estos aparatos",
      "Machinery and mechanical appliances; electrical equipment; parts thereof; sound recorders and reproducers, television image and sound recorders and reproducers, and parts and accessories of such articles", "Maquinaria y electr\u00f3nica"),
    c("XVII", "Material de transporte", "Vehicles, aircraft, vessels and associated transport equipment", "Veh\u00edculos y transporte"),
    c("XVIII", "Instrumentos y aparatos de \u00f3ptica, fotograf\u00eda o cinematograf\u00eda, de medida, control o de precisi\u00f3n; instrumentos y aparatos medicoquir\u00fargicos; aparatos de relojer\u00eda; instrumentos musicales; partes y accesorios de estos instrumentos o aparatos",
      "Optical, photographic, cinematographic, measuring, checking, precision, medical or surgical instruments and apparatus; clocks and watches; musical instruments; parts and accessories thereof", "Instrumentos de precisi\u00f3n y m\u00e9dicos"),
    c("XIX", "Armas, municiones, y sus partes y accesorios", "Arms and ammunition; parts and accessories thereof", "Armamento"),
    c("XX", "Mercanc\u00edas y productos diversos", "Miscellaneous manufactured articles", "Mobiliario, juguetes y manufacturas varias"),
    c("XXI", "Objetos de arte o colecci\u00f3n y antig\u00fcedades", "Works of art, collectors' pieces and antiques", "Arte y antig\u00fcedades"),
    c("Otras mercancias", "Otras mercanc\u00edas", "Otros", "Other", "NA")
  )
  .paleta_multiclave(secciones, c(colores_victorgm_categoricos(21), .gris_resto))
}

#' Paleta de las actividades económicas (secciones CNAE)
#'
#' @return Vector nombrado de colores.
#' @examples
#' paleta_actividades_economicas()[["Industria manufacturera"]]
#' @export
paleta_actividades_economicas <- function() {
  actividades <- c(
    "Agricultura, ganader\u00eda, silvicultura y pesca; Industrias extractivas",
    "Industria manufacturera",
    "Suministro de energ\u00eda el\u00e9ctrica y agua, saneamiento y gesti\u00f3n de residuos",
    "Construcci\u00f3n",
    "Comercio al por mayor y al por menor; reparaci\u00f3n de veh\u00edculos de motor y motocicletas",
    "Transporte y almacenamiento",
    "Informaci\u00f3n y comunicaciones",
    "Actividades financieras y de seguros",
    "Actividades profesionales, cient\u00edficas y t\u00e9cnicas",
    "Actividades administrativas y servicios auxiliares"
  )
  c(
    "Total CNAE" = "#2C3E50",
    "Total" = "#2C3E50",
    stats::setNames(colores_victorgm_categoricos(length(actividades)), actividades),
    "Resto de actividades" = .gris_resto,
    "No consta" = "#777777"
  )
}

#' Paleta de los modos de suministro del comercio de servicios
#'
#' @return Vector nombrado de colores.
#' @examples
#' paleta_modos_suministro()[["M1"]]
#' @export
paleta_modos_suministro <- function() {
  .paleta_multiclave(
    list(c("TOTAL MODOS", "Total agregado (todos los modos)", "TOTAL", "Total"),
         c("Modo 1: Suministro transfronterizo", "M1", "Modo 1"),
         c("Modo 2: Consumo en Espa\u00f1a (exportaciones) o en el extranjero (importaciones)", "M2", "Modo 2"),
         c("Modo 3: Presencia comercial", "M3", "Modo 3"),
         c("Modo 4: Presencia de personas f\u00edsicas", "M4", "Modo 4")),
    c("#2C3E50", colores_victorgm_categoricos(4))
  )
}

#' Paleta de las comunidades autónomas
#'
#' Admite el código INE (`"01"` a `"19"`), el nombre oficial (`"Madrid, Comunidad de"`) y el nombre
#' corto (`"Madrid"`). Los nombres cortos coinciden con los de [get_provincias_metadata()].
#'
#' @return Vector nombrado de colores.
#' @examples
#' paleta_ccaa()[["Madrid"]]
#' @export
paleta_ccaa <- function() {
  ccaa <- list(
    c("01", "Andaluc\u00eda"),
    c("02", "Arag\u00f3n"),
    c("03", "Asturias, Principado de", "Asturias"),
    c("04", "Balears, Illes", "Baleares", "Illes Balears", "Islas Baleares"),
    c("05", "Canarias"),
    c("06", "Cantabria"),
    c("07", "Castilla y Le\u00f3n"),
    c("08", "Castilla-La Mancha", "Castilla - La Mancha"),
    c("09", "Catalu\u00f1a"),
    c("10", "Comunitat Valenciana", "C. Valenciana", "Comunidad Valenciana"),
    c("11", "Extremadura"),
    c("12", "Galicia"),
    c("13", "Madrid, Comunidad de", "Madrid", "Comunidad de Madrid"),
    c("14", "Murcia, Regi\u00f3n de", "Murcia", "Regi\u00f3n de Murcia"),
    c("15", "Navarra, Comunidad Foral de", "Navarra", "Comunidad Foral de Navarra"),
    c("16", "Pa\u00eds Vasco", "Euskadi"),
    c("17", "Rioja, La", "La Rioja"),
    c("18", "Ceuta"),
    c("19", "Melilla"),
    c("00", "No determinado", "Sin determinar")
  )
  .paleta_multiclave(ccaa, c(colores_victorgm_categoricos(19), "#A5A5A5"))
}

#' Paleta de países, bloques y agrupaciones
#'
#' Colores fijos para que un mismo país o bloque tenga el mismo color en todos los gráficos. Admite
#' el nombre en español (y algunos en inglés), el código ISO3 y el ISO2, además de continentes,
#' áreas, bloques económicos y acuerdos comerciales de la UE. España va siempre en rojo y la UE en azul.
#'
#' @return Vector nombrado de colores hexadecimales.
#' @examples
#' paleta_paises()[["Espa\u00f1a"]]
#' setdiff(c("Francia", "Rusia"), names(paleta_paises()))
#' @export
paleta_paises <- function() {
  c(
    "Espa\u00f1a" = "#E31A1C", "Spain" = "#E31A1C", "ESP" = "#E31A1C", "ES" = "#E31A1C",
    "UE" = "#004FEE", "UE27" = "#004FEE", "UE 27" = "#004FEE", "UE-27" = "#004FEE",
    "Uni\u00f3n Europea" = "#004FEE", "Uni\u00f3n Europea (UE-27)" = "#004FEE", "AREA_UE27" = "#004FEE",
    "Zona euro" = "#004FEE", "Zona Euro" = "#004FEE", "\u00c1rea del euro" = "#004FEE", "AREA_ZONA_EURO" = "#004FEE",
    "Resto UE-27" = "#8AA4BE", "Resto UE (no euro)" = "#6EACD9", "AREA_RESTO_UE" = "#6EACD9",
    "Extra-UE" = "#B8860B", "Mundo" = "#5e5866",
    "Francia" = "#A6CEE3", "FRA" = "#A6CEE3", "FR" = "#A6CEE3",
    "Alemania" = "#FF7F00", "DEU" = "#FF7F00", "DE" = "#FF7F00",
    "Portugal" = "#18BC9C", "PRT" = "#18BC9C", "PT" = "#18BC9C",
    "Italia" = "#B2DF8A", "ITA" = "#B2DF8A", "IT" = "#B2DF8A",
    "Pa\u00edses Bajos" = "#FFF5BB", "NLD" = "#FFF5BB", "NL" = "#FFF5BB",
    "B\u00e9lgica" = "#FDBF6F", "BEL" = "#FDBF6F", "BE" = "#FDBF6F",
    "Irlanda" = "#51E9CB", "IRL" = "#51E9CB", "IE" = "#51E9CB",
    "Reino Unido" = "#CCBE93", "GBR" = "#CCBE93", "GB" = "#CCBE93", "UK" = "#CCBE93",
    "Estados Unidos" = "#6A3D9A", "United States" = "#6A3D9A", "EE.UU." = "#6A3D9A", "EE. UU." = "#6A3D9A",
    "EEUU" = "#6A3D9A", "USA" = "#6A3D9A", "US" = "#6A3D9A",
    "China" = "#F48383", "CHN" = "#F48383", "CN" = "#F48383",
    "M\u00e9xico" = "#46AF4B", "MEX" = "#46AF4B", "MX" = "#46AF4B",
    "Argentina" = "#7BA1D8", "ARG" = "#7BA1D8", "AR" = "#7BA1D8",
    "Brasil" = "#2C3E50", "BRA" = "#2C3E50", "BR" = "#2C3E50",
    "Canada" = "#CAB2D6", "Canad\u00e1" = "#CAB2D6", "CAN" = "#CAB2D6", "CA" = "#CAB2D6",
    "Jap\u00f3n" = "#843C0C", "JPN" = "#843C0C", "JP" = "#843C0C",
    # N\u00f3rdicos
    "Suecia" = "#08519C", "SWE" = "#08519C", "SE" = "#08519C",
    "Dinamarca" = "#FB9A99", "DNK" = "#FB9A99", "DK" = "#FB9A99",
    "Finlandia" = "#00BFFF", "FIN" = "#00BFFF", "FI" = "#00BFFF",
    "Noruega" = "#2E5984", "NOR" = "#2E5984", "NO" = "#2E5984",
    # Europa central y del este
    "Polonia" = "#E7298A", "POL" = "#E7298A", "PL" = "#E7298A",
    "Austria" = "#D95F02", "AUT" = "#D95F02", "AT" = "#D95F02",
    "Hungr\u00eda" = "#006400", "HUN" = "#006400", "HU" = "#006400",
    "Rep\u00fablica Checa" = "#7570B3", "Chequia" = "#7570B3", "CZE" = "#7570B3", "CZ" = "#7570B3",
    "Eslovaquia" = "#8DD3C7", "SVK" = "#8DD3C7", "SK" = "#8DD3C7",
    # Balcanes y sureste
    "Grecia" = "#08306B", "GRC" = "#08306B", "GR" = "#08306B",
    "Ruman\u00eda" = "#E6AB02", "ROU" = "#E6AB02", "RO" = "#E6AB02",
    "Bulgaria" = "#66A61E", "BGR" = "#66A61E", "BG" = "#66A61E",
    "Croacia" = "#C51B7D", "HRV" = "#C51B7D", "HR" = "#C51B7D",
    "Eslovenia" = "#B3DE69", "SVN" = "#B3DE69", "SI" = "#B3DE69",
    # B\u00e1lticos
    "Lituania" = "#FDB462", "LTU" = "#FDB462", "LT" = "#FDB462",
    "Letonia" = "#800080", "LVA" = "#800080", "LV" = "#800080",
    "Estonia" = "#708090", "EST" = "#708090", "EE" = "#708090",
    # Peque\u00f1os e islas
    "Chipre" = "#D9D9D9", "CYP" = "#D9D9D9", "CY" = "#D9D9D9",
    "Luxemburgo" = "#BC80BD", "LUX" = "#BC80BD", "LU" = "#BC80BD",
    "Malta" = "#FCCDE5", "MLT" = "#FCCDE5", "MT" = "#FCCDE5",
    # Resto de Europa y Mediterr\u00e1neo
    "Suiza" = "#525252", "CHE" = "#525252", "CH" = "#525252",
    "Turqu\u00eda" = "#17BECF", "T\u00fcrkiye" = "#17BECF", "TUR" = "#17BECF", "TR" = "#17BECF",
    "Marruecos" = "#8C564B", "MAR" = "#8C564B", "MA" = "#8C564B",
    "T\u00fanez" = "#E29982", "TUN" = "#E29982", "TN" = "#E29982",
    "Argelia" = "#B5A642", "DZA" = "#B5A642", "DZ" = "#B5A642",
    "Rusia" = "#8B0000", "RUS" = "#8B0000", "RU" = "#8B0000",
    "Ucrania" = "#F2C200", "UKR" = "#F2C200", "UA" = "#F2C200",
    # Asia y Ocean\u00eda
    "Corea del Sur" = "#3A77AB", "KOR" = "#3A77AB", "KR" = "#3A77AB",
    "India" = "#EDA952", "IND" = "#EDA952", "IN" = "#EDA952",
    "Vietnam" = "#895C44", "VNM" = "#895C44", "VN" = "#895C44",
    "Tailandia" = "#459B8F", "THA" = "#459B8F", "TH" = "#459B8F",
    "Taiw\u00e1n" = "#A3A75A", "TWN" = "#A3A75A", "TW" = "#A3A75A",
    "Malasia" = "#D891AB", "MYS" = "#D891AB", "MY" = "#D891AB",
    "Indonesia" = "#AF6836", "IDN" = "#AF6836", "ID" = "#AF6836",
    "Singapur" = "#3D9E86", "SGP" = "#3D9E86", "SG" = "#3D9E86",
    "Australia" = "#C9952E", "AUS" = "#C9952E", "AU" = "#C9952E",
    "Arabia Saud\u00ed" = "#117A65", "SAU" = "#117A65", "SA" = "#117A65",
    "Emiratos \u00c1rabes Unidos" = "#1F618D", "ARE" = "#1F618D", "AE" = "#1F618D",
    # Am\u00e9rica
    "Chile" = "#C76A46", "CHL" = "#C76A46", "CL" = "#C76A46",
    "Colombia" = "#E0AC46", "COL" = "#E0AC46", "CO" = "#E0AC46",
    "Per\u00fa" = "#B83679", "PER" = "#B83679", "PE" = "#B83679",
    # Continentes
    "\u00c1frica" = "#B38737", "CONT_AFRICA" = "#B38737",
    "Am\u00e9rica" = "#284E6C", "CONT_AMERICA" = "#284E6C",
    "Asia" = "#A13A32", "CONT_ASIA" = "#A13A32",
    "Europa" = "#4685B8", "CONT_EUROPA" = "#4685B8",
    "Australia y Ocean\u00eda" = "#874CA2", "Ocean\u00eda" = "#874CA2", "CONT_OCEANIA" = "#874CA2",
    "Am\u00e9rica del Norte" = "#45AB6A", "CONT_AMERICA_NORTE" = "#45AB6A", "AREA_AMN" = "#45AB6A",
    # \u00c1reas
    "Am\u00e9rica Latina" = "#E9A050", "AREA_AML" = "#E9A050",
    "Resto de Am\u00e9rica" = "#CF8B3D", "AREA_AMR" = "#CF8B3D",
    "Asia (exc. Oriente Medio)" = "#C5665D", "AREA_ASE" = "#C5665D",
    "Oriente Medio" = "#994521", "AREA_ASO" = "#994521",
    "Resto de Europa" = "#6ED395", "AREA_EUR" = "#6ED395",
    # Bloques econ\u00f3micos
    "ASEAN" = "#EAC24D", "BLOQ_ASEAN" = "#EAC24D",
    "MERCOSUR" = "#3D9E86", "BLOQ_MERCOSUR" = "#3D9E86",
    "OPEP" = "#77438E", "BLOQ_OPEP" = "#77438E",
    "USMCA (T-MEC)" = "#3C739E", "BLOQ_USMCA" = "#3C739E",
    "Consejo de Cooperaci\u00f3n del Golfo" = "#A84239", "BLOQ_CCG" = "#A84239",
    "OCDE" = "#38495B", "BLOQ_OCDE" = "#38495B",
    "EFTA / AELC" = "#48B99E", "EFTA" = "#48B99E", "BLOQ_EFTA" = "#48B99E",
    "BRICS" = "#DA844D", "BLOQ_BRICS" = "#DA844D",
    "Comunidad Andina" = "#D0AD46", "BLOQ_ANDINA" = "#D0AD46",
    "Sistema de la Integraci\u00f3n Centroamericana" = "#AA7EBD", "BLOQ_SICA" = "#AA7EBD",
    "G7" = "#2C3E50", "G20" = "#5e5866",
    # Acuerdos comerciales de la UE
    "Acuerdos en vigor" = "#3D975D", "ACU_VIGOR" = "#3D975D",
    "Acuerdos en proceso" = "#CC635C", "ACU_PROCESO" = "#CC635C",
    "Vecindad" = "#6298BF", "ACU_AREA_VECINDAD" = "#6298BF",
    "Mediterr\u00e1neo y Oriente Medio" = "#D37A4B", "ACU_AREA_MED" = "#D37A4B",
    "Balcanes Occidentales" = "#9F6EB4", "ACU_AREA_BALCANES" = "#9F6EB4",
    "Pa\u00edses del Este (DCFTA)" = "#62C6B1", "ACU_AREA_DCFTA" = "#62C6B1",
    "Otros pa\u00edses vecinos" = "#E3C455", "ACU_AREA_OTROS_VEC" = "#E3C455",
    "Asia y Ocean\u00eda" = "#E1776E", "ACU_AREA_ASIA_OCEANIA" = "#E1776E",
    "ACU_AREA_AMERICA" = "#5F6D7B",
    "\u00c1frica, Caribe y Pac\u00edfico" = "#C3723B", "ACU_AREA_ACP" = "#C3723B",
    "Asia Central" = "#89CCA5", "ACU_AREA_ASIA_CENTRAL" = "#89CCA5",
    # Resto
    "Resto del mundo" = .gris_resto, "RdM" = .gris_resto, "RoW" = .gris_resto,
    "Otros" = .gris_resto, "Resto" = .gris_resto, "Otros pa\u00edses" = .gris_resto
  )
}

#' Paleta de los puntos de estrangulamiento del comercio marítimo
#'
#' Nombres en inglés (como en PortWatch) con alias en español para los más citados.
#'
#' @return Vector nombrado de colores.
#' @examples
#' paleta_chokepoints()[["Suez Canal"]]
#' @export
paleta_chokepoints <- function() {
  puntos <- list(
    c("Suez Canal", "Canal de Suez", "Suez"),
    c("Panama Canal", "Canal de Panam\u00e1", "Panam\u00e1"),
    c("Malacca Strait", "Estrecho de Malaca", "Malaca"),
    c("Strait of Hormuz", "Estrecho de Ormuz", "Ormuz"),
    c("Bab el-Mandeb Strait", "Bab el-Mandeb", "Bab El Mandeb", "Bab-el-Mandeb"),
    c("Cape of Good Hope", "Cabo de Buena Esperanza", "Cabo Buena Esperanza", "Buena Esperanza"),
    c("Gibraltar Strait", "Estrecho de Gibraltar", "Gibraltar"),
    c("Dover Strait", "Estrecho de Dover", "Canal de la Mancha"),
    c("Bosporus Strait", "B\u00f3sforo", "Estrecho del B\u00f3sforo"),
    c("Korea Strait"), c("Taiwan Strait", "Estrecho de Taiw\u00e1n"), c("Oresund Strait"),
    c("Lombok Strait"), c("Luzon Strait"), c("Sunda Strait"), c("Makassar Strait"),
    c("Bohai Strait"), c("Torres Strait"), c("Tsugaru Strait"), c("Magellan Strait", "Estrecho de Magallanes"),
    c("Mindoro Strait"), c("Balabac Strait"), c("Bering Strait"), c("Kerch Strait"),
    c("Ombai Strait"), c("Mona Passage"), c("Windward Passage"), c("Yucatan Channel")
  )
  .paleta_multiclave(puntos, colores_victorgm_categoricos(length(puntos)))
}

#' Paleta de puertos
#'
#' Los diez primeros puertos españoles en tonos de la gama morado-oro de la web y veinte puertos
#' mundiales con el resto de la secuencia categórica.
#'
#' @return Vector nombrado de colores.
#' @examples
#' paleta_puertos()[["Algeciras"]]
#' @export
paleta_puertos <- function() {
  espana <- c("Algeciras", "Las Palmas (de Gran Canaria)", "Barcelona", "Valencia", "Bilbao",
              "Tarragona", "Huelva", "Santa Cruz De Tenerife", "Cartagena", "Ceuta")
  mundo <- c("Singapore", "Shanghai (Pudong)", "Rotterdam", "Antwerp", "Busan", "Ningbo", "Hong Kong",
             "Nagoya", "Yokohama", "Kaohsiung", "Ulsan", "Tianjin Xin Gang", "Qingdao", "Port Klang",
             "Kobe", "Guangzhou (Nansha)", "Gwangyang (Kwangyang)", "Tokyo", "Amsterdam", "Manila")
  colores_espana <- c("#5F2987", "#B8860B", "#7B3FAF", "#C9952E", "#9B6BC6", "#D4A843", "#4a1f6b",
                      "#843C0C", "#C490E4", "#E8C36A")
  colores_mundo <- colores_victorgm_categoricos()[c(3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 19, 20, 24, 25, 26, 27)]
  c(stats::setNames(colores_espana, espana), stats::setNames(colores_mundo, mundo),
    "Las Palmas" = "#B8860B", "Santa Cruz de Tenerife" = "#843C0C", "Shanghai" = "#9B6BC6")
}

#' Paleta de las rutas de flete marítimo
#'
#' Rutas del índice Drewry (desde Shanghái) con alias en español e inglés.
#'
#' @return Vector nombrado de colores.
#' @examples
#' paleta_rutas_flete()[["Rotterdam"]]
#' @export
paleta_rutas_flete <- function() {
  .paleta_multiclave(
    list(c("Nueva York", "New York"),
         c("Rotterdam", "R\u00f3terdam"),
         c("G\u00e9nova", "Genova", "Genoa", "Geneva"),
         c("Los \u00c1ngeles", "Los Angeles"),
         c("Global", "\u00cdndice Drewry Global", "Drewry WCI"),
         c("Retorno desde Rotterdam", "Retorno Rotterdam"),
         c("Retorno desde Los \u00c1ngeles", "Retorno Los \u00c1ngeles", "Retorno desde Los Angeles", "Retorno Los Angeles")),
    c("#5F2987", "#B8860B", "#4A9079", "#2C3E50", "#E31A1C", "#D4A843", "#75A3FF")
  )
}
