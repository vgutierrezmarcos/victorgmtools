# Aranceles----

#' Obtener datos de aranceles de WITS/TRAINS
#'
#' Obtiene datos de aranceles desde la base de datos World International Trade Solution (WITS)
#' del Banco Mundial. Función adaptada del paquete witstrainsr. Los datos están armonizados
#' a nivel de 6 dígitos del Sistema Armonizado (HS6) e incluyen tasas preferenciales y NMF.
#'
#' @param reporter Vector de caracteres con códigos ISO de 3 dígitos de países declarantes, o "all".
#' @param partner Vector de caracteres con códigos ISO de 3 dígitos de países socios, "all" o "000" (mundo). Por defecto, "000".
#' @param product Vector de caracteres con códigos de producto HS6 de 6 dígitos, o "all". Por defecto, "all".
#' @param year Vector de caracteres, numérico o entero con años en formato de 4 dígitos, o "all". Por defecto, "all".
#' @param datatype Tipo de datos: "reported" (solo aranceles) o "aveestimated" (incluye equivalentes ad valorem). Por defecto, "reported".
#' @param verbose Lógico. Si TRUE, muestra mensajes de progreso. Por defecto, TRUE.
#' @return Tibble con columnas: reporter_country_code, partner_country_code, product_code, year, tariff_simple_average, tariff_type, n_total_lines, etc.
#' @examples
#' \dontrun{
#' get_tariffs(reporter = "ESP", partner = "000", year = 2023)
#' get_tariffs(reporter = "USA", partner = "CHN", product = "all", year = 2020:2023)
#' }
#' @export
get_tariffs <- function(reporter,
                        partner = "000",
                        product = "all",
                        year = "all",
                        datatype = "reported",
                        verbose = TRUE) {
  
  countries_df <- victorgmtools::get_countries()
  
  # Función para dividir productos por capítulos HS2 (2 primeros dígitos)
  split_by_hs2 <- function() {
    hs2_chapters <- sprintf("%02d", 1:97)
    return(hs2_chapters)
  }
  
  # Función auxiliar para resolver códigos
  resolve_codes <- function(codes, param_name) {
    if (length(codes) == 1 && (codes[1] == "all" || codes[1] == "000")) {
      return(codes)
    }
    
    resolved_codes <- character(length(codes))
    
    for (i in seq_along(codes)) {
      code <- codes[i]
      
      if (code %in% countries_df$country_code) {
        resolved_codes[i] <- code
      } else if (code %in% countries_df$iso3_code) {
        country_row <- countries_df[countries_df$iso3_code == code, ]
        resolved_codes[i] <- country_row$country_code[1]
        if (verbose) {
          message(paste("Convertido", param_name, ":", code, "->", resolved_codes[i]))
        }
      } else {
        stop(paste("Código no encontrado en", param_name, ":", code))
      }
    }
    
    return(resolved_codes)
  }
  
  # Función para hacer una solicitud individual CON MEJOR MANEJO DE ERRORES
  fetch_single <- function(rep, part, prod, yr, dt) {
    url_request <- paste0(
      "https://wits.worldbank.org/API/V1/SDMX/V21/datasource/TRN",
      "/reporter/", paste0(rep, collapse = ";"),
      "/partner/", paste0(part, collapse = ";"),
      "/product/", paste0(prod, collapse = ";"),
      "/year/", paste0(yr, collapse = ";"),
      "/datatype/", dt
    )
    
    check_error <- tryCatch({
      response <- httr::GET(url_request)
      
      # Si es error 400, devolver indicador especial
      if (httr::status_code(response) == 400) {
        if (verbose) message("  Error 400 - La consulta es demasiado grande")
        return("ERROR_400")
      }
      
      # Si es otro error HTTP
      if (httr::http_error(response)) {
        if (verbose) message(paste("  Error HTTP", httr::status_code(response)))
        return(NULL)
      }
      
      httr::content(response, "text", encoding = "UTF-8")
    }, error = function(e) {
      # Si el error contiene "400", también devolver ERROR_400
      if (grepl("400", e$message)) {
        if (verbose) message("  Error 400 detectado en excepción")
        return("ERROR_400")
      }
      if (verbose) message(paste("  Error en petición:", e$message))
      return(NULL)
    })
    
    if (is.null(check_error)) return(NULL)
    
    # Detectar error 400
    if (is.character(check_error) && length(check_error) == 1 && check_error == "ERROR_400") {
      return("ERROR_400")
    }
    
    # Detectar error de datos grandes
    if (stringr::str_detect(check_error, "large data")) {
      if (verbose) message("  Datos demasiado grandes")
      return("LARGE_DATA")
    }
    
    if (stringr::str_detect(check_error, "wits:error")) {
      error_msg <- tryCatch({
        stringr::str_extract(check_error, "((?<=(\\.\"&gt;)).+)(.+(?=(&lt;\\/wits:message)))")
      }, error = function(e) "Error desconocido")
      if (verbose) warning(paste("Error API:", error_msg))
      return(NULL)
    }
    
    if (stringr::str_detect(check_error, "NoRecordsFound")) {
      if (verbose) message("  Sin registros")
      return(NULL)
    }
    
    xml_request <- tryCatch({
      xml2::read_xml(url_request)
    }, error = function(e) {
      if (verbose) message("  Error al parsear XML")
      return(NULL)
    })
    
    if (is.null(xml_request)) return(NULL)
    
    xml_data <- xml2::xml_find_all(xml_request, xpath = "//Obs")
    xml_parameters <- xml2::xml_find_all(xml_request, xpath = "//Series")
    
    if (length(xml_data) == 0) {
      if (verbose) message("  Sin datos en respuesta")
      return(NULL)
    }
    
    tariffs_df <- data.frame(
      reporter_country_code = xml2::xml_attr(xml_parameters, attr = "REPORTER"),
      partner_country_code = xml2::xml_attr(xml_parameters, attr = "PARTNER"),
      product_code = xml2::xml_attr(xml_parameters, attr = "PRODUCTCODE"),
      year = xml2::xml_attr(xml_data, attr = "TIME_PERIOD"),
      tariff_simple_average = as.numeric(xml2::xml_attr(xml_data, attr = "OBS_VALUE")),
      tariff_type = xml2::xml_attr(xml_data, attr = "TARIFFTYPE"),
      n_total_lines = as.integer(xml2::xml_attr(xml_data, attr = "TOTALNOOFLINES")),
      n_pref_lines = as.integer(xml2::xml_attr(xml_data, attr = "NBR_PREF_LINES")),
      n_mfn_lines = as.integer(xml2::xml_attr(xml_data, attr = "NBR_MFN_LINES")),
      n_specific_duty_lines = as.integer(xml2::xml_attr(xml_data, attr = "NBR_NA_LINES")),
      tariff_sum_rates = as.numeric(xml2::xml_attr(xml_data, attr = "SUM_OF_RATES")),
      tariff_min_rate = as.numeric(xml2::xml_attr(xml_data, attr = "MIN_RATE")),
      tariff_max_rate = as.numeric(xml2::xml_attr(xml_data, attr = "MAX_RATE")),
      nomen_code = xml2::xml_attr(xml_data, attr = "NOMENCODE"),
      datatype = dt,
      stringsAsFactors = FALSE
    )
    
    return(tariffs_df)
  }
  
  # Función auxiliar para verificar si es un error especial
  is_error <- function(x, error_type) {
    if (is.null(x)) return(FALSE)
    if (is.character(x) && length(x) == 1 && x[1] == error_type) return(TRUE)
    return(FALSE)
  }
  
  # Función para dividir parámetros cuando hay errores
  split_params_aggressive <- function(param_list, param_name) {
    if (length(param_list) == 1) {
      # Si es un solo elemento y es "all", dividir de forma especial
      if (param_list[1] == "all") {
        if (param_name == "partner") {
          # Top 30 socios más importantes
          top_countries <- c(
            "840", "156", "392", "276", "826", "250", "380", "724", "528", "056",
            "124", "702", "704", "076", "032", "036", "554", "484", "586", "458",
            "051", "152", "196", "268", "643", "710", "616", "410", "376", "428"
          )
          return(as.list(top_countries))
        }
      }
      return(list(param_list))
    }
    
    # Si es un vector, dividir en grupos más pequeños (grupos de 10)
    chunk_size <- min(10, ceiling(length(param_list) / 2))
    chunks <- split(param_list, ceiling(seq_along(param_list) / chunk_size))
    return(chunks)
  }
  
  # Resolver códigos
  reporter <- resolve_codes(reporter, "reporter")
  partner <- resolve_codes(partner, "partner")
  
  # Convertir year a character
  year <- as.character(year)
  
  # Validaciones
  if (!(is.character(reporter) && (length(reporter) == 1 && reporter[1] == "all" || all(nchar(reporter) == 3)))) {
    stop("El parámetro 'reporter' debe ser \"all\" o un vector de códigos de 3 dígitos.")
  }
  if (!(is.character(partner) && (length(partner) == 1 && (partner[1] == "all" || partner[1] == "000") || all(nchar(partner) == 3)))) {
    stop("El parámetro 'partner' debe ser \"all\", \"000\" o un vector de códigos de 3 dígitos.")
  }
  if (!(is.character(product) && (length(product) == 1 && product[1] == "all" || all(nchar(product) == 6)))) {
    stop("El parámetro 'product' debe ser \"all\" o un vector de códigos HS6.")
  }
  if (!(length(year) == 1 && year[1] == "all" || all(nchar(year) == 4))) {
    stop("El parámetro 'year' debe ser \"all\" o un vector de años de 4 dígitos.")
  }
  
  # Manejar múltiples datatypes
  if (length(datatype) == 1 && datatype == "all") {
    datatype <- c("reported", "aveestimated")
  }
  if (!all(datatype %in% c("reported", "aveestimated"))) {
    stop("El parámetro 'datatype' debe ser \"reported\", \"aveestimated\" o \"all\".")
  }
  
  # Intentar consulta directa primero
  if (verbose) message("Intentando consulta...")
  
  result <- fetch_single(reporter, partner, product, year, datatype[1])
  
  # Si hay más datatypes, intentar con los demás
  if (length(datatype) > 1 && !is.null(result) && !is_error(result, "ERROR_400") && !is_error(result, "LARGE_DATA")) {
    for (i in 2:length(datatype)) {
      result_extra <- fetch_single(reporter, partner, product, year, datatype[i])
      if (!is.null(result_extra) && !is_error(result_extra, "ERROR_400") && !is_error(result_extra, "LARGE_DATA")) {
        result <- rbind(result, result_extra)
      }
    }
    if (verbose) message("Consulta exitosa")
    return(tibble::tibble(result))
  }
  
  # Si no es error, retornar resultado
  if (!is.null(result) && !is_error(result, "ERROR_400") && !is_error(result, "LARGE_DATA")) {
    if (verbose) message("Consulta exitosa")
    return(tibble::tibble(result))
  }
  
  # Si es ERROR_400 o LARGE_DATA, dividir la consulta
  if (verbose) message("Dividiendo consulta debido a error o tamaño...")
  
  # Estrategia de división: priorizar reporter > year > partner > product
  results_list <- list()
  
  # CASO 1: Dividir reporters si hay más de 1 o es "all"
  if (length(reporter) > 10 || reporter[1] == "all") {
    if (verbose) message("Dividiendo por reporteros...")
    reporters_split <- split_params_aggressive(reporter, "reporter")
    
    for (i in seq_along(reporters_split)) {
      if (verbose && i %% 5 == 0) {
        message(sprintf("  Procesando grupo %d/%d de reporteros", i, length(reporters_split)))
      }
      
      for (dt in datatype) {
        res <- fetch_single(reporters_split[[i]], partner, product, year, dt)
        
        if (!is.null(res) && !is_error(res, "ERROR_400") && !is_error(res, "LARGE_DATA")) {
          results_list[[length(results_list) + 1]] <- res
        } else if (is_error(res, "ERROR_400") || is_error(res, "LARGE_DATA")) {
          # Si aún falla, dividir por años
          if (year[1] == "all" || length(year) > 1) {
            years_to_try <- if (year[1] == "all") as.character(2010:2023) else year
            
            for (y in years_to_try) {
              res_year <- fetch_single(reporters_split[[i]], partner, product, y, dt)
              if (!is.null(res_year) && !is_error(res_year, "ERROR_400") && !is_error(res_year, "LARGE_DATA")) {
                results_list[[length(results_list) + 1]] <- res_year
              }
              Sys.sleep(0.05)
            }
          }
        }
      }
      Sys.sleep(0.1)
    }
    
    # CASO 2: Dividir años
  } else if (year[1] == "all" || length(year) > 5) {
    if (verbose) message("Dividiendo por años...")
    
    years_to_process <- if (year[1] == "all") as.character(2010:2023) else year
    
    for (y in years_to_process) {
      if (verbose && which(years_to_process == y) %% 3 == 0) {
        message(sprintf("  Procesando año %s", y))
      }
      
      for (dt in datatype) {
        res <- fetch_single(reporter, partner, product, y, dt)
        
        if (!is.null(res) && !is_error(res, "ERROR_400") && !is_error(res, "LARGE_DATA")) {
          results_list[[length(results_list) + 1]] <- res
        } else if (is_error(res, "ERROR_400") || is_error(res, "LARGE_DATA")) {
          # Dividir por partners
          if (partner[1] == "all" || length(partner) > 5) {
            partners_split <- split_params_aggressive(partner, "partner")
            
            for (part_group in partners_split) {
              res_part <- fetch_single(reporter, part_group, product, y, dt)
              if (!is.null(res_part) && !is_error(res_part, "ERROR_400") && !is_error(res_part, "LARGE_DATA")) {
                results_list[[length(results_list) + 1]] <- res_part
              }
              Sys.sleep(0.05)
            }
          }
        }
      }
      Sys.sleep(0.1)
    }
    
    # CASO 3: Dividir partners
  } else if (partner[1] == "all" || length(partner) > 10) {
    if (verbose) message("Dividiendo por países socios...")
    partners_split <- split_params_aggressive(partner, "partner")
    
    for (i in seq_along(partners_split)) {
      if (verbose) message(sprintf("  Procesando grupo %d/%d de socios", i, length(partners_split)))
      
      for (dt in datatype) {
        res <- fetch_single(reporter, partners_split[[i]], product, year, dt)
        if (!is.null(res) && !is_error(res, "ERROR_400") && !is_error(res, "LARGE_DATA")) {
          results_list[[length(results_list) + 1]] <- res
        }
      }
      Sys.sleep(0.1)
    }
    
    # CASO 4: Último recurso - devolver tibble vacío
  } else {
    if (verbose) message("No se pueden obtener datos con estos parámetros")
    return(tibble::tibble())
  }
  
  # Combinar resultados
  results_list <- results_list[!sapply(results_list, is.null)]
  
  if (length(results_list) == 0) {
    if (verbose) message("No se encontraron datos")
    return(tibble::tibble())
  }
  
  final_result <- do.call(rbind, results_list)
  
  if (verbose) {
    message(sprintf("✓ Consulta completada: %d filas obtenidas", nrow(final_result)))
  }
  
  return(tibble::tibble(final_result))
}

#' Obtener disponibilidad de datos TRAINS
#'
#' Obtiene información sobre la disponibilidad de datos en el dataset TRAINS de UNCTAD.
#' No requiere parámetros. Función adaptada del paquete witstrainsr.
#'
#' @return Tibble con información de disponibilidad: iso3_code, country_code, country_name, year, nomen_code, n_preferential_agreement, partner_list, specific_duty, last_update.
#' @examples
#' \dontrun{
#' availability <- get_availability()
#' }
#' @export
get_availability <- function(){
  tmp_xml <- xml2::xml_find_all(xml2::read_xml("https://wits.worldbank.org/API/V1/wits/datasource/trn/dataavailability/"),
                                xpath = "//wits:reporter")
  availability_df <-
    data.frame(
      iso3_code = xml2::xml_attr(tmp_xml, attr = "iso3Code"),
      country_code = xml2::xml_attr(tmp_xml, attr = "countrycode"),
      country_name = xml2::xml_text(xml2::xml_find_all(tmp_xml[1], xpath = "//wits:name")),
      year = xml2::xml_text(xml2::xml_find_all(tmp_xml[1], xpath = "//wits:year")),
      nomen_code = xml2::xml_text(
        xml2::xml_find_all(tmp_xml[1], xpath = "//wits:reporternernomenclature")
      ),
      n_preferential_agreement = as.integer(xml2::xml_text(
        xml2::xml_find_all(tmp_xml[100], xpath = "//wits:numberofpreferentialagreement")
      )),
      partner_list = xml2::xml_text(xml2::xml_find_all(tmp_xml[1], xpath = "//wits:partnerlist")),
      specific_duty = xml2::xml_text(
        xml2::xml_find_all(tmp_xml[1], xpath = "//wits:isspecificdutyexpressionestimatedavailable")
      ),
      last_update = xml2::xml_text(
        xml2::xml_find_all(tmp_xml[1], xpath = "//wits:lastupdateddate")
      ),
      stringsAsFactors = FALSE
    )
  
  availability_df |>
    tibble::tibble()
}

#' Obtener lista de países TRAINS
#'
#' Obtiene nombres, códigos alfanuméricos, códigos ISO3 y otros metadatos de los países
#' disponibles en el dataset TRAINS de UNCTAD. No requiere parámetros.
#' Función adaptada del paquete witstrainsr.
#'
#' @return Tibble con columnas: iso3_code, country_code, name, is_reporter, is_partner, is_group.
#' @examples
#' \dontrun{
#' countries <- get_countries()
#' }
#' @export
get_countries <- function() {
  tmp_xml <- xml2::xml_find_all(xml2::read_xml("https://wits.worldbank.org/API/V1/wits/datasource/trn/country/ALL"),
                                xpath = "//wits:country")
  countries_df <- data.frame(iso3_code = xml2::xml_text(xml2::xml_find_all(tmp_xml,
                                                                           xpath = "//wits:iso3Code")), country_code = xml2::xml_attr(tmp_xml,
                                                                                                                                      attr = "countrycode"), name = xml2::xml_text(xml2::xml_find_all(tmp_xml,
                                                                                                                                                                                                      xpath = "//wits:name")), is_reporter = xml2::xml_attr(tmp_xml,
                                                                                                                                                                                                                                                            attr = "isreporter"), is_partner = xml2::xml_attr(tmp_xml,
                                                                                                                                                                                                                                                                                                              attr = "ispartner"), is_group = ifelse(xml2::xml_attr(tmp_xml,
                                                                                                                                                                                                                                                                                                                                                                    attr = "isgroup") == "Yes", "1", "0"), stringsAsFactors = FALSE) |>
    rbind(data.frame(iso3_code = "000", country_code = "000",
                     name = "World", is_reporter = "0", is_partner = "1",
                     is_group = "1", stringsAsFactors = FALSE)) |> dplyr::arrange(.data$iso3_code)
  
  countries_df |>
    tibble::tibble()
}

#' Obtener nomenclaturas disponibles en TRAINS
#'
#' Obtiene información sobre las nomenclaturas disponibles en el dataset TRAINS de UNCTAD.
#' No requiere parámetros. Función adaptada del paquete witstrainsr.
#'
#' @return Tibble con columnas: nomen_code, description.
#' @examples
#' \dontrun{
#' nomenclatures <- get_nomen_codes()
#' }
#' @export
get_nomen_codes <- function(){
  tmp_xml <- xml2::xml_find_all(xml2::read_xml("https://wits.worldbank.org/API/V1/wits/datasource/trn/nomenclature/"),
                                xpath = "//wits:nomenclature")
  nomen_codes_df <-
    data.frame(nomen_code = xml2::xml_attr(tmp_xml, "nomenclaturecode"),
               description = xml2::xml_text(tmp_xml), stringsAsFactors = FALSE)
  
  nomen_codes_df |>
    tibble::tibble()
}

#' Obtener lista de productos TRAINS
#'
#' Obtiene la lista de productos disponibles en el dataset TRAINS de UNCTAD.
#' No requiere parámetros. Función adaptada del paquete witstrainsr.
#'
#' @return Tibble con columnas: product_code, product_desc.
#' @examples
#' \dontrun{
#' products <- get_products()
#' }
#' @export
get_products <- function(){
  tmp_xml <- xml2::xml_find_all(xml2::read_xml("https://wits.worldbank.org/API/V1/wits/datasource/trn/product/all"),
                                xpath = "//wits:product")
  products_df <-
    data.frame(product_code = xml2::xml_attr(tmp_xml, attr = "productcode"),
               product_desc = xml2::xml_text(xml2::xml_find_all(tmp_xml[1],
                                                                xpath = "//wits:productdescription")), stringsAsFactors = FALSE)
  
  products_df |>
    tibble::tibble()
}