score  <-  function(cualitativos, balance, estado) {
  ###########################
  ##Condiciones cualitativas
  ###########################
  C_Cualitativas <- c()
  #Condicion 1  Edad [25,70] a la fecha de vencimiento   
  C_Cualitativas[1] <- cualitativos$edad_principal_accionista > 25 && cualitativos$edad_principal_accionista < 70
  
  #Condicion 2 Antiguedad Domicilio  >  2 aÒos  en el actual         >  3 aÒos en el actual y el anterior 
  C_Cualitativas[2] <- cualitativos$antiguedad_principal_accionista_domicilio > 2
  
  #Condicion 3 Antiguedad negocio MÌn 3 aÒos (desde la const.)
  C_Cualitativas[3] <- cualitativos$antiguedad_negocio > 3
  
  #Condicion 4 MÌn 4 aÒos de experiencia   
  C_Cualitativas[4] <- cualitativos$experiencia_principal_accionista_giro > 4  
  
  #Condicion 5 Estados Financieros firmados por el PA (ultimo y parcial no mas de 3 meses)
  C_Cualitativas[5] <- cualitativos$estados_financieros == 1  
  
  #Condicion 6 [$10,000,000 MXP ,  $30,000,000MXP]
  C_Cualitativas[6] <- cualitativos$ventas_anuales > 10000000 && cualitativos$ventas_anuales < 30000000
  
  ###########################
  ##Condiciones cuantitativas
  ###########################
  C_Cuantitativas <- c()
  #Condicion 1  Endeudamiento  (PT/AT)  Acepto(.4)Reviso(.6)Rechazo
  C_Cuantitativas[1] <-  balance$pas_total_pasivo/balance$act_total__activo
  
  #Condicion 2 LÌquidez AC / PC        Rechazo(1.2)Reviso(2)Acepto
  C_Cuantitativas[2] <- balance$act_total_circulante/balance$pas_total_pasivo_corto_plazo 
  
  #Condicion 3 LÌquidez (AC - INV) / PC  Rechazo(.85)Reviso(1)Acepto
  C_Cuantitativas[3] <- (balance$act_total_circulante - balance$act_inventarios) / 
    balance$pas_total_pasivo_corto_plazo
  
  #Condicion 4 ROA UTNET/AT Rechazo(.02)Reviso(.04)Acepto
  C_Cuantitativas[4] <- estado$utilidad_ejercicio/balance$act_total__activo
  
  #Condicion 5 Margen Operativo UTOP/vts Rechazo(.04)Reviso(.06)Acepto
  C_Cuantitativas[5] <- estado$utilidad_operativa / estado$total_ventas
  
  
  #Condicion 6 Cobertura de intereses UTOP/GF Rechazo(1.5)Reviso(1.8)Acepto
  C_Cuantitativas[6] <- estado$utilidad_operativa / estado$gastos_prod_fin
  
  #MIN(PROV, INVENTARIO+CLIENTES)
  C7.1 <- min(balance$pas_proveedores, balance$act_inventarios + balance$act_clientes)  
  
  #MIN(IMPUESTOS PAGADOS,IMPUESTOS POR PAGAR)
  C7.2 <- min(balance$act_impuestos_por_recuperar, balance$pas_impuestos_por_pagar)
  
  #Condicion 7 Deuda Neta UTOP/ [PT-MIN(PROV, INVENTARIO+CLIENTES)-MIN(IMPUESTOS PAGADOS, 
  # IMPUESTOS POR PAGAR) -CAJAYBANCOS -inversiones en val - cuentas por cobrar 
  ##Rechazo(.3)Reviso(.5)Acepto
  C_Cuantitativas[7] <- estado$utilidad_operativa / 
    (balance$pas_total_pasivo-C7.1 - 
       C7.2-balance$act_caja_y_bancos - 
       balance$act_inversiones_en_valores - 
       balance$act_cuentas_por_cobrar)
  
  CutOff1 <- c(.6,1.2,.85,.02,.04,1.5,.3)
  CutOff2 <-  c(.4, 2, 1, .04,.06,2,.5)
    
  Score <- c()
  Score[1] <- 1
  
  if(sum(C_Cualitativas) == 6)
    Score[1] <- 3
  
  if (C_Cuantitativas[1]  >  CutOff1[1])
    Score[2] <- 1
  else if (C_Cuantitativas[1]  >  CutOff2[1])
    Score[2] <- 2
  else
    Score[2] <- 3
  
  for (i in 2:7){
    if (C_Cuantitativas[i] < CutOff1[i])
      Score[i+1] <- 1
    else if (C_Cuantitativas[i] < CutOff2[i]) 
      Score[i+1] <- 2
    else
      Score[i+1] <- 3
  }
  print(Score)
  Calificacion <- c()
  
  if (sum(Score ==3) == 8)
    Calificacion <- 3
  else if (sum(Score == 1) > 0)
    Calificacion <- 1
  else
    Calificacion <- 2
  
  Calificacion
}
