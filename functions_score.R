calculaCalificacionConcepto <- function(cualitativos, balance, estado, concepto) {
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
  concepto$score[concepto$valor >= concepto$rango_max] <- 1
  
  
  concepto
}

calculaCalificacion <- function(cualitativos, balance, estado, concepto) {
  califConcepto <- calculaCalificacionConcepto(cualitativos, balance, estado, concepto)
  # Calificacion general
  min(califConcepto$score)
}