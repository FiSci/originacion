# Funciones para leer informacion de empresa

getEmpresasDB <- function(params, usuario_id) {
  con <- dbConnect(MySQL(), 
                   user=params$user,
                   password=params$password,
                   dbname=params$dbname,
                   host=params$host
  )
  query <- paste("select 
                 ei.id empresa_info_id, 
	               e.id empresa_id, 
                 e.nombre nombre, 
                 e.rfc rfc, 
                 ei.fecha_informacion, 
                 ei.estado_resultados_fecha, 
                 ei.balance_fecha, 
                 ei.cualitativo_fecha,
                 ue.usuario_id
                 from usuario_empresa ue 
                 left join empresa e on ue.empresa_id = e.id 
                 left join empresa_info ei on e.id = ue.empresa_id and e.id = ei.empresa_id and ue.usuario_id
                 where ue.usuario_id = ", 
                 usuario_id, sep="")
  res <- dbGetQuery(con, query)
  dbDisconnect(con)
  res
}

getInfoCualitativosDB <- function(params, empresa_info_id) {
  con <- dbConnect(MySQL(), 
                   user=params$user,
                   password=params$password,
                   dbname=params$dbname,
                   host=params$host
  )
  query <- paste("select empresa_info_id,
                          edad_principal_accionista,
                          antiguedad_principal_accionista_domicilio,
                          antiguedad_negocio,
                          experiencia_principal_accionista_giro,
                          estados_financieros,
                          ventas_anuales
                  from info_cualitativo where empresa_info_id =",empresa_info_id , sep="")
  res <- dbGetQuery(con, query)
  dbDisconnect(con)
  res
}

getInfoBalanceDB <- function(params, empresa_info_id) {
  con <- dbConnect(MySQL(), 
                   user=params$user,
                   password=params$password,
                   dbname=params$dbname,
                   host=params$host
  )
  query <- paste("select 
                    empresa_info_id,
                    act_caja_y_bancos,
                    act_inversiones_en_valores,
                    act_cuentas_por_cobrar,
                    act_clientes,
                    act_deudores_diversos_documentos_por_cobrar,
                    act_impuestos_por_recuperar,
                    act_anticipo_a_proveedores,
                    act_estimacion_de_cuentas_incobrables,
                    act_companias_afiliadas,
                    act_total_cuentas_por_cobrar,
                    act_inventarios,
                    act_otros_activos_circulantes,
                    act_total_circulante,
                    act_activos_diferidos,
                    act_documentos_por_cobrar_lgo_pzo,
                    act_edificios_y_terrenos,
                    act_maquinaria_y_equipo,
                    act_depreciacion,
                    act_total_activo_largo_plazo,
                    act_total__activo,
                    pas_porcion_circulante_de_creditos_a_lp,
                    pas_prestamos_bancarios_cp,
                    pas_proveedores,
                    pas_acreedores,
                    pas_documentos_por_pagar,
                    pas_impuestos_por_pagar,
                    pas_companias_afiliadas,
                    pas_total_pasivo_corto_plazo,
                    pas_prestamos_bancarios_lp,
                    pas_otros_pasivos_lp,
                    pas_impuestos_diferidos,
                    pas_total_pasivo_largo_plazo,
                    pas_total_pasivo,
                    cap_capital_social,
                    cap_reservas,
                    cap_result_acumulados,
                    cap_revaluacion_de_activo_fijo,
                    cap_aportaciones_p_futuros_aumentos_de_capital,
                    cap_resultado_del_ejercicio,
                    cap_total_capital_contable,
                    total_pasivo_y_capital
                 from info_balance where empresa_info_id =",empresa_info_id , sep="")
  res <- dbGetQuery(con, query)
  dbDisconnect(con)
  res
}

getInfoEdoResDB <- function(params, empresa_info_id) {
  con <- dbConnect(MySQL(), 
                   user=params$user,
                   password=params$password,
                   dbname=params$dbname,
                   host=params$host
  )
  query <- paste("select 
                    empresa_info_id,
                    total_ventas,
                    devolucion_sobre_ventas,
                    rebajas_sobre_ventas,
                    total_ventas_netas,
                    costo_ventas,
                    utilidad_bruta,
                    gastos_operacion,
                    gastos_venta,
                    gastos_admin,
                    gastos_otros,
                    utilidad_operativa,
                    costo_integral_fin,
                    gastos_prod_fin,
                    perdida_cambios,
                    otros_productos,
                    otros_ingresos,
                    utilidad_antes_imptos_partidas_especiales,
                    provision_impto_activo,
                    impto_isr,
                    participacion_utilidades,
                    utilidad_ejercicio
                from info_estado_resultados where empresa_info_id =",empresa_info_id , sep="")
  res <- dbGetQuery(con, query)
  dbDisconnect(con)
  res
}

###

createInsertQueryFromList <- function(x) {
  fields <- paste("(", paste(names(x), collapse=","), ")", sep="" )
  values <- paste("('", paste(unlist(x), collapse="','"), "')", sep="")
  paste(fields, "VALUES", values)
}

writeEmpresaDB <- function(params, usuario_id, valueList) { 
  query <- paste("CALL sp_insertempresa(@FLAG, ", 
                 usuario_id, ",'",
                 valueList$rfc, "','",
                 valueList$nombre, "','",
                 valueList$razon_social, "')",
                 sep=""
  )
  con <- dbConnect(MySQL(), 
                   user=params$user,
                   password=params$password,
                   dbname=params$dbname,
                   host=params$host
  )
  res <- dbSendQuery(con, query)
  mysqlCloseResult(res)
  res <- dbGetQuery(con, "select @FLAG;")
  dbDisconnect(con)
  res[1,1]
}

writeFechaDB <- function(params, usuario_id, valueList) { 
  query <- paste("CALL sp_insertfecha(@FLAG, ", 
                 usuario_id, ",'",
                 valueList$empresa, "','",
                 valueList$fecha, "')",
                 sep=""
  )
  con <- dbConnect(MySQL(), 
                   user=params$user,
                   password=params$password,
                   dbname=params$dbname,
                   host=params$host
  )
  res <- dbSendQuery(con, query)
  mysqlCloseResult(res)
  res <- dbGetQuery(con, "select @FLAG;")  
  dbDisconnect(con)
  res[1,1]
}

####Escribe Cualitativos (7)
writeCualitativosDB <- function(params, usuario_id, valueList) { 
  
  query <-paste( "INSERT INTO originacion.info_cualitativo(
    	                empresa_info_id,edad_principal_accionista,","
		                  antiguedad_principal_accionista_domicilio,","
                      antiguedad_negocio,experiencia_principal_accionista_giro,","
		                  estados_financieros,ventas_anuales)"
                 
                 ,"VALUES('",
                 
                      valueList$empresa_info_id,"','",valueList$edad_principal_accionista,"','",
                      valueList$antiguedad_principal_accionista_domicilio,"','",
                      valueList$antiguedad_negocio,"','",valueList$experiencia_principal_accionista_giro,"','",
                      valueList$estados_financieros,"','",valueList$ventas_anuales,"')"
            ,sep="")
  
  con <- dbConnect(MySQL(), 
                   user=params$user,
                   password=params$password,
                   dbname=params$dbname,
                   host=params$host
  )
  dbSendQuery(con, query)
  #  res <- dbSendQuery(con, "select @FLAG;")
  dbDisconnect(con)
  #  res
}

####Escribe Balance (42)
writeBalanceDB <- function(params, usuario_id, valueList) { 
   
  query <-paste( "INSERT INTO originacion.info_balance(
          		           empresa_info_id,act_caja_y_bancos,act_inversiones_en_valores,","
                         act_cuentas_por_cobrar,act_clientes,act_deudores_diversos_documentos_por_cobrar,","
                         act_impuestos_por_recuperar,act_anticipo_a_proveedores,","
                         act_estimacion_de_cuentas_incobrables,act_companias_afiliadas,"," 
                         act_total_cuentas_por_cobrar,act_inventarios,","
                         act_otros_activos_circulantes,act_total_circulante,","
                         act_activos_diferidos,act_documentos_por_cobrar_lgo_pzo,"," 	
                         act_edificios_y_terrenos,act_maquinaria_y_equipo,","
                         act_depreciacion,act_total_activo_largo_plazo,","
                         act_total__activo,pas_porcion_circulante_de_creditos_a_lp,"," 
                         pas_prestamos_bancarios_cp,pas_proveedores,","
                         pas_acreedores,pas_documentos_por_pagar,","  
                         pas_impuestos_por_pagar,pas_companias_afiliadas,","
                         pas_total_pasivo_corto_plazo,pas_prestamos_bancarios_lp,","
                         pas_otros_pasivos_lp,pas_impuestos_diferidos,pas_total_pasivo_largo_plazo,","
                         pas_total_pasivo,cap_capital_social,cap_reservas,"," 
                         cap_result_acumulados,cap_revaluacion_de_activo_fijo,","
                         cap_aportaciones_p_futuros_aumentos_de_capital,cap_resultado_del_ejercicio,","
                         cap_total_capital_contable,total_pasivo_y_capital)"
                 
                 ,"VALUES('",
                 
                         valueList$empresa_info_id,"','",valueList$act_caja_y_bancos,"','",
                         valueList$act_inversiones_en_valores,"','",valueList$act_cuentas_por_cobrar,"','",
                         valueList$act_clientes,"','",valueList$act_deudores_diversos_documentos_por_cobrar,"','",
                         valueList$act_impuestos_por_recuperar,"','",valueList$act_anticipo_a_proveedores,"','",
                         valueList$act_estimacion_de_cuentas_incobrables,"','",valueList$act_companias_afiliadas,"','",
                         valueList$act_total_cuentas_por_cobrar,"','",valueList$act_inventarios,"','",
                         valueList$act_otros_activos_circulantes,"','",valueList$act_total_circulante,"','",
                         valueList$act_activos_diferidos,"','",valueList$act_documentos_por_cobrar_lgo_pzo,"','",
                         valueList$act_edificios_y_terrenos,"','",valueList$act_maquinaria_y_equipo,"','",
                         valueList$act_depreciacion,"','",valueList$act_total_activo_largo_plazo,"','",
                         valueList$act_total__activo,"','",valueList$pas_porcion_circulante_de_creditos_a_lp,"','",
                         valueList$pas_prestamos_bancarios_cp,"','",valueList$pas_proveedores,"','",
                         valueList$pas_acreedores,"','",valueList$pas_documentos_por_pagar,"','",
                         valueList$pas_impuestos_por_pagar,"','",valueList$pas_companias_afiliadas,"','",      
                         valueList$pas_total_pasivo_corto_plazo,"','",valueList$pas_prestamos_bancarios_lp,"','",
                         valueList$pas_otros_pasivos_lp,"','",valueList$pas_impuestos_diferidos,"','",
                         valueList$pas_total_pasivo_largo_plazo,"','",valueList$pas_total_pasivo,"','",
                         valueList$cap_capital_social,"','",valueList$cap_reservas,"','",
                         valueList$cap_result_acumulados,"','",valueList$cap_revaluacion_de_activo_fijo,"','",
                         valueList$cap_aportaciones_p_futuros_aumentos_de_capital,"','",valueList$cap_resultado_del_ejercicio,"','",
                         valueList$cap_total_capital_contable,"','",valueList$total_pasivo_y_capital,"')"
                 ,sep="")
  
  con <- dbConnect(MySQL(), 
                   user=params$user,
                   password=params$password,
                   dbname=params$dbname,
                   host=params$host
  )
  dbSendQuery(con, query)
  
  dbDisconnect(con)
}


####Escribe Estado de Resultados(22)
writeEstadoDB <- function(params, usuario_id, valueList) { 
  
  query <-paste( "INSERT INTO originacion.info_estado_resultados(empresa_info_id,total_ventas,","
                        	devolucion_sobre_ventas,rebajas_sobre_ventas,total_ventas_netas,costo_ventas,"," 
                          utilidad_bruta,gastos_operacion,gastos_venta,gastos_admin,"," 
                          gastos_otros,utilidad_operativa,"," 
                          costo_integral_fin,gastos_prod_fin,"," 
                          perdida_cambios,otros_productos,"," 
                          otros_ingresos,utilidad_antes_imptos_partidas_especiales,","
                          provision_impto_activo,impto_isr,"," 
                          participacion_utilidades,utilidad_ejercicio)"
  		      ,"VALUES('",
                        valueList$empresa_info_id,"','",valueList$total_ventas,"','",
                        valueList$devolucion_sobre_ventas,"','",
                        valueList$rebajas_sobre_ventas,"','",valueList$total_ventas_netas,"','",
                        valueList$costo_ventas,"','",valueList$utilidad_bruta,"','",
                        valueList$gastos_operacion,"','",valueList$gastos_venta,"','",
                        valueList$gastos_admin,"','",valueList$gastos_otros,"','",
                        valueList$utilidad_operativa,"','",valueList$costo_integral_fin,"','",
                        valueList$gastos_prod_fin,"','",valueList$perdida_cambios,"','",
                        valueList$otros_productos,"','",valueList$otros_ingresos,"','",
                        valueList$utilidad_antes_imptos_partidas_especiales,"','",
                        valueList$provision_impto_activo,"','",valueList$impto_isr,"','",
                        valueList$participacion_utilidades,"','",valueList$utilidad_ejercicio,"')",
            sep="")
  con <- dbConnect(MySQL(), 
                   user=params$user,
                   password=params$password,
                   dbname=params$dbname,
                   host=params$host
  )
  
  dbSendQuery(con, query)
  
  dbDisconnect(con)
}

