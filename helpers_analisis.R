
# Graficar probabilidad versus codificación manual a nivel de glosa
cortar_evaluar <-  function(corte, data, agrupar = "no") {
  #corte = 0.9
  #data = pred
  agrupar <- match.arg(agrupar, c("si", "no" ))
  
  # Se usa como objeto intermedio
  parte1 <-  data %>% 
    mutate(coincide = if_else(predicho == real, 1, 0),
           no_coincide = if_else(predicho != real, 1, 0),
           prob = as.numeric(as.character(prob_max)))
  
  # Para calcular los falsos positivos
  fp <- parte1 %>% 
    filter(prob > corte) %>%
    group_by(predicho) %>%
    summarise(fp = sum(no_coincide)) 
  
  # Calcular los totales de cada clase
  totales_clase <- parte1 %>% 
    group_by(real) %>% 
    mutate(completa = n()) %>% 
    slice(1)
  
  # Para sacar cosas a nivel de clase
  if (agrupar == "si") {

    resultado <- parte1 %>% 
      filter(prob > corte) %>% 
      group_by(real) %>% 
      summarise(acierto = sum(coincide),
                total = n(),
                fn = total - acierto,
                prob_media = mean(prob)) %>% 
      ungroup() %>% 
      mutate(por_acierto = acierto / total) %>% 
      left_join(totales_clase %>% select(completa, real, codigo), by = "real") %>% 
      left_join(fp, by = c("real" = "predicho"))  %>% 
      mutate(precision = acierto / (acierto + fp),
             recall = acierto / (acierto + fn ),
             f1 = 2 * (precision * recall) / (precision + recall),
             f1 =  if_else(is.nan(f1) | is.na(f1), 0, f1 ),
             f1_macro = mean(f1, na.rm = T),
             f1_weighted = sum(f1 * total) / sum(total))  
      
  }
  
  
  # Aquí es para lo no agrupado
  if (agrupar == "no") {
    
    resultado <- parte1 %>% 
      filter(prob > corte) %>% 
      group_by(real) %>% 
      summarise(acierto = sum(coincide),
                total = n(),
                fn = total - acierto) %>% 
      ungroup() %>% 
      mutate(por_acierto = acierto / total) %>% 
      left_join(totales_clase %>% select(completa, real), by = "real") %>% 
      left_join(fp, by = c("real" = "predicho"))  %>% 
      mutate(precision = acierto / (acierto + fp),
             recall = acierto / (acierto + fn ),
             f1 = 2 * (precision * recall) / (precision + recall),
             f1 =  if_else(is.nan(f1) | is.na(f1), 0, f1 ),
             f1_macro = mean(f1),
             f1_weighted = sum(f1 * total) / sum(total))  %>% 
      mutate(acierto = sum(acierto),
             total = sum(total),
             por_acierto = acierto / total,
             completa = nrow(parte1)) %>% 
      dplyr::slice(1) %>% 
      select(acierto, total, por_acierto, completa, f1_macro, f1_weighted)
    
  }
  
  resultado
}



#----------------------------------------------------------


cortar_clases <-  function(corte) {
  
  filtro_clases <-  prob_acierto %>% 
    filter(prob_media_pred >= corte) %>% 
    pull(predicho)
  
  
  parte1 <-  predicciones_xgb %>% 
    mutate(coincide = if_else(predicho == real, 1, 0),
           no_coincide = if_else(predicho != real, 1, 0),
           prob = as.numeric(as.character(prob_max)))
  
  
  resultado <- parte1 %>% 
    filter(real %in% filtro_clases) %>% 
    group_by(real) %>% 
    summarise(acierto = sum(coincide),
              total = n(),
              fn = total - acierto) %>% 
    ungroup() %>% 
    mutate(por_acierto = acierto / total) %>% 
    left_join(totales_clase %>% select(completa, real), by = "real") %>% 
    left_join(fp, by = c("real" = "predicho"))  %>% 
    mutate(precision = acierto / (acierto + fp),
           recall = acierto / (acierto + fn ),
           f1 = 2 * (precision * recall) / (precision + recall),
           f1 =  if_else(is.nan(f1) | is.na(f1), 0, f1 ),
           f1_macro = mean(f1, na.rm = T),
           f1_weighted = sum(f1 * total) / sum(total),
           acc_global = sum(acierto) / sum(total),
           total = sum(total),
           completa = nrow(parte1), 
           n_clases = length(filtro_clases) )  %>% 
    dplyr::slice(1) %>% 
    select(acc_global, total, completa, f1_macro, f1_weighted, n_clases)
  
  return(resultado)
  
}

