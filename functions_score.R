calculaCalificacionConcepto <- function(concepto, cualitativos, balance, estado, buro, tipo_persona) {
  ###########################
  ##Condiciones cualitativas
  ###########################
  concepto$valor <- NA
  
  # Cualitativos
  concepto$valor[concepto$concepto=="Edad del Principal Accionista"] <- 
    cualitativos$edad_principal_accionista
  concepto$valor[concepto$concepto=="Antiguedad del Principal Accionista en el domicilio"] <- 
    cualitativos$antiguedad_principal_accionista_domicilio
  concepto$valor[concepto$concepto=="Antiguedad en el negocio"] <- 
    cualitativos$antiguedad_negocio
  concepto$valor[concepto$concepto=="Experiencia del Principal Accionista en el giro"] <- 
    cualitativos$experiencia_principal_accionista_giro
  concepto$valor[concepto$concepto=="Estados Financieros"] <-   
    cualitativos$estados_financieros
  concepto$valor[concepto$concepto=="Ventas Anuales"] <- 
    cualitativos$ventas_anuales
  # Cuantitativos
  concepto$valor[concepto$concepto=="Endeudamiento"] <-  
    balance$pas_total_pasivo/balance$act_total__activo
  concepto$valor[concepto$concepto=="Liquidez"] <- 
    balance$act_total_circulante/balance$pas_total_pasivo_corto_plazo
  concepto$valor[concepto$concepto=="Liquidez – Inventarios"] <- 
    (balance$act_total_circulante - balance$act_inventarios) / balance$pas_total_pasivo_corto_plazo
  concepto$valor[concepto$concepto=="ROA"] <- 
    estado$utilidad_ejercicio/balance$act_total__activo
  concepto$valor[concepto$concepto=="Margen Operativo"] <- 
    estado$utilidad_operativa / estado$total_ventas
  concepto$valor[concepto$concepto=="Cobertura de Intereses"] <- 
    estado$utilidad_operativa / estado$gastos_prod_fin
  concepto$valor[concepto$concepto=="Capacidad Endeudamiento"] <- 
    estado$utilidad_operativa / 
    (balance$pas_total_pasivo - 
       min(balance$pas_proveedores, balance$act_inventarios + balance$act_clientes) - 
       min(balance$act_impuestos_por_recuperar, balance$pas_impuestos_por_pagar) -
       balance$act_caja_y_bancos - 
       balance$act_inversiones_en_valores - 
       balance$act_cuentas_por_cobrar)
  
  # Score
  concepto$valor[is.na(concepto$valor)] <- 0
  concepto$score <- 1
  concepto$score[concepto$valor >= concepto$rango_min] <- concepto$score[concepto$valor >= concepto$rango_min] + 1
  concepto$score[concepto$valor >= concepto$rango_min_2] <- concepto$score[concepto$valor >= concepto$rango_min_2] + 1
  concepto$score[concepto$valor > concepto$rango_max] <- 1
  
  # No se toma en cuenta la capacidad de endeudamiento
  concepto$score[concepto$concepto=="Capacidad Endeudamiento"] <- 3
  
  
  
  
  concepto$score[concepto$concepto=="Endeudamiento"] <- 
    ifelse(concepto$valor[concepto$concepto=="Endeudamiento"] <= 
             concepto$rango_min[concepto$concepto=="Endeudamiento"], 3,
           ifelse(concepto$valor[concepto$concepto=="Endeudamiento"] <= 
                    concepto$rango_min_2[concepto$concepto=="Endeudamiento"], 
                  2, 1))
  
  # Buro
  concepto$valor[concepto$concepto == "Score Califica"] <- buro$score_califica
  concepto$valor[concepto$concepto == "Buró Principal Accionista Persona Moral"] <- 
    ifelse(buro$buro_moral_paccionista == 0, "Sin Información", 
           ifelse(buro$buro_moral_paccionista == 1, "Malo", "Bueno"))
  concepto$valor[concepto$concepto == "Buró Principal Accionista Persona Física"] <- 
    ifelse(buro$buro_fisica_paccionista == 0, "Sin Información", 
           ifelse(buro$buro_fisica_paccionista == 1, "Malo", "Bueno"))
  
  # Buro   
  if(buro$atraso == 1 && buro$score_califica >= 470) {
    concepto$score[concepto$concepto == "Score Califica"] <- 3
  } 
  if(buro$atraso == 0 && buro$score_califica >= 488) {
    concepto$score[concepto$concepto == "Score Califica"] <- 3
  }

  concepto$score[concepto$concepto == "Buró Principal Accionista Persona Moral"] <- 
    ifelse(buro$buro_moral_paccionista==1, 1, 3)
    
  concepto$score[concepto$concepto == "Buró Principal Accionista Persona Física"] <- 
    ifelse(buro$buro_fisica_paccionista==1, 1, 3)
  
  if(tipo_persona != "P. Moral") {
    concepto$score[concepto$concepto == "Buró Principal Accionista Persona Moral"] <- 3
  }

  concepto
}

calculaCalificacion <- function(concepto, cualitativos, balance, estado, buro, tipo_persona) {
  califConcepto <- calculaCalificacionConcepto(concepto, cualitativos, balance, estado, buro, tipo_persona)
  # Calificacion general
  score <- min(califConcepto$score)
  if (score == 0) {
    score <- score + 1
  }
  score
}