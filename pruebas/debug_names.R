
tryCatch({
  library(dplyr)
  library(victorgmtools)
  
  codigos_descarga <- c(
    "D_AKR620PA",
    "D_AKR620PC",
    "D_AKR620PP",
    "D_AKB07000",
    "D_AKB07300",
    "D_AKB07100",
    "D_AKB07200"
  )
  
  datos_df <- bdeseries::get_series(codes = codigos_descarga)
  
  datos_procesados_df <- 
    datos_df |> 
    dplyr::filter(
      unidades == "Porcentaje",
      as.Date(fecha) >= as.Date("1985-01-01")
    ) |> 
    dplyr::mutate(
      nombres_cortos = dplyr::if_else(nchar(nombres) > 70, stringr::str_sub(nombres, 70, 300), nombres),
      nombres_muy_cortos = dplyr::if_else(nchar(nombres) > 116, stringr::str_sub(nombres, 116, 300), nombres)
    )
    
  print(unique(datos_procesados_df$nombres_muy_cortos))
  
}, error = function(e) {
  print(e)
})
