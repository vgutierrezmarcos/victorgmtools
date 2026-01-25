# Metadatos geográficos ----

#' Obtener metadatos de provincias y comunidades autónomas españolas (INE)
#'
#' Devuelve un tibble con los códigos y nombres oficiales de las provincias y comunidades autónomas de España según el INE.
#'
#' @return Un tibble con 4 columnas:
#' \item{provincia}{Código INE de la provincia (2 caracteres)}
#' \item{nombre_provincia}{Nombre oficial de la provincia}
#' \item{comunidad}{Código INE de la comunidad autónoma (2 caracteres)}
#' \item{nombre_comunidad}{Nombre oficial de la comunidad autónoma}
#' @export
get_provincias_metadata <- function() {
  dplyr::tibble(
    provincia = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10",
                  "11", "12", "13", "14", "15", "16", "17", "18", "19", "20",
                  "21", "22", "23", "24", "25", "26", "27", "28", "29", "30",
                  "31", "32", "33", "34", "35", "36", "37", "38", "39", "40",
                  "41", "42", "43", "44", "45", "46", "47", "48", "49", "50",
                  "51", "52"),
    nombre_provincia = c("Araba/Álava", "Albacete", "Alicante/Alacant", "Almería", "Ávila", "Badajoz", "Balears, Illes", "Barcelona", "Burgos", "Cáceres",
                         "Cádiz", "Castellón/Castelló", "Ciudad Real", "Córdoba", "Coruña, A", "Cuenca", "Girona", "Granada", "Guadalajara", "Gipuzkoa",
                         "Huelva", "Huesca", "Jaén", "León", "Lleida", "Rioja, La", "Lugo", "Madrid", "Málaga", "Murcia",
                         "Navarra", "Ourense", "Asturias", "Palencia", "Palmas, Las", "Pontevedra", "Salamanca", "Santa Cruz de Tenerife", "Cantabria", "Segovia",
                         "Sevilla", "Soria", "Tarragona", "Teruel", "Toledo", "Valencia/València", "Valladolid", "Bizkaia", "Zamora", "Zaragoza",
                         "Ceuta", "Melilla"),
    comunidad = c("16", "08", "10", "01", "07", "11", "04", "09", "07", "11",
                  "01", "10", "08", "01", "12", "08", "09", "01", "08", "16",
                  "01", "02", "01", "07", "09", "17", "12", "13", "01", "14",
                  "15", "12", "03", "07", "05", "12", "07", "05", "06", "07",
                  "01", "07", "09", "02", "08", "10", "07", "16", "07", "02",
                  "18", "19"),
    nombre_comunidad = c("País Vasco", "Castilla - La Mancha", "Comunitat Valenciana", "Andalucía", "Castilla y León", "Extremadura", "Balears, Illes", "Cataluña", "Castilla y León", "Extremadura",
                         "Andalucía", "Comunitat Valenciana", "Castilla - La Mancha", "Andalucía", "Galicia", "Castilla - La Mancha", "Cataluña", "Andalucía", "Castilla - La Mancha", "País Vasco",
                         "Andalucía", "Aragón", "Andalucía", "Castilla y León", "Cataluña", "Rioja, La", "Galicia", "Madrid, Comunidad de", "Andalucía", "Murcia, Región de",
                         "Navarra, Comunidad Foral de", "Galicia", "Asturias, Principado de", "Castilla y León", "Canarias", "Galicia", "Castilla y León", "Canarias", "Cantabria", "Castilla y León",
                         "Andalucía", "Castilla y León", "Cataluña", "Aragón", "Castilla - La Mancha", "Comunitat Valenciana", "Castilla y León", "País Vasco", "Castilla y León", "Aragón",
                         "Ceuta", "Melilla")
  )
}
