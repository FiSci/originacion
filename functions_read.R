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
                  e.razon_social,
                 e.rfc rfc,
                 ei.fecha_informacion, 
                 ei.estado_resultados_fecha, 
                 ei.balance_fecha, 
                 ei.cualitativo_fecha,
                  ei.buro_fecha,
                  ei.terms_fecha,
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

getInfoBuroDB <- function(params, empresa_info_id) {
  con <- dbConnect(MySQL(), 
                   user=params$user,
                   password=params$password,
                   dbname=params$dbname,
                   host=params$host
  )
  query <- paste("select 
                 empresa_info_id, atraso, score_califica, buro_moral_paccionista, buro_fisica_paccionista
                 from info_buro where empresa_info_id =",empresa_info_id , sep="")
  res <- dbGetQuery(con, query)
  dbDisconnect(con)
  res
}

getScoreDB <- function(params, empresa_info_id) {
  con <- dbConnect(MySQL(), 
                   user=params$user,
                   password=params$password,
                   dbname=params$dbname,
                   host=params$host
  )
  query <- paste("select score from empresa_info where id =",empresa_info_id , sep="")
  res <- dbGetQuery(con, query)
  dbDisconnect(con)
  if(is.na(res$score[1])) {
    ret <- 0
  } else {
    ret <- res$score[1]
  }
}

getTipoPersonaDB <- function(params, empresa_id) {
  con <- dbConnect(MySQL(), 
                   user=params$user,
                   password=params$password,
                   dbname=params$dbname,
                   host=params$host
  )
  query <- paste("select razon_social from empresa where id = ", empresa_id , sep="")
  res <- dbGetQuery(con, query)
  dbDisconnect(con)
  res
}

getInfoTermsDB <- function(params, empresa_info_id) {
  con <- dbConnect(MySQL(), 
                   user=params$user,
                   password=params$password,
                   dbname=params$dbname,
                   host=params$host
  )
  query <- paste("select 
                 monto_solicitado,
                 monto_autorizado,
                 destino_credito,
                 tipo_ministracion,
                 forma_pago,
                 vigencia_linea,
                 vigencia_contrato,
                 plazo_disposiciones,
                 tasa_ordinaria,
                 tasa_moratoria,
                 comision_apertura,
                 fuente_fondeo, 
                 costo_fondeo,
                 moneda,
                 garantia,
                 costo_garantia,
                 nombre_aval,
                 propone,
                 autoriza1,
                 autoriza2,
                  comentarios
                 from info_terms where empresa_info_id =",empresa_info_id , sep="")
  res <- dbGetQuery(con, query)
  dbDisconnect(con)
  res
}

getNombresCalificaDB <- function(params, facultad) {
  con <- dbConnect(MySQL(), 
                   user=params$user,
                   password=params$password,
                   dbname=params$dbname,
                   host=params$host
  )
  query <- paste("select id, nombres, apellido_paterno, apellido_materno, area, facultad, puesto 
                 from usuario 
                 where facultad = ",facultad , sep="")
  dbGetQuery(con, "SET NAMES utf8")
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

borraCalificacionDB <- function(params, empresa_info_id) {
  query <- paste("update empresa_info set score = NULL, buro_fecha = NULL, balance_fecha = NULL,  
                 cualitativo_fecha = NULL, estado_resultados_fecha = NULL
                 where id = ", 
                 empresa_info_id,
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
  dbDisconnect(con)
}

writeEmpresaDB <- function(params, usuario_id, valueList) { 
  query <- paste("CALL sp_insertempresa(@FLAG, ", 
                 usuario_id, ",'",
                 valueList$rfc, "','",
                 valueList$nombre, "','", 
                 valueList$tipo_persona, "')",
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
                      valueList$estados_financieros,"','",valueList$ventas_anuales,
                 "') 
                  ON DUPLICATE KEY UPDATE 
                      contador=contador+1,
                      edad_principal_accionista=values(edad_principal_accionista),
                      antiguedad_principal_accionista_domicilio=values(antiguedad_principal_accionista_domicilio),
                      antiguedad_negocio=values(antiguedad_negocio),
                      experiencia_principal_accionista_giro=values(experiencia_principal_accionista_giro),
                      estados_financieros=values(estados_financieros),
                      ventas_anuales=values(ventas_anuales)",
            sep="")
  
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
                         cap_total_capital_contable,total_pasivo_y_capital)",
                 "VALUES('",
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
                         valueList$cap_total_capital_contable,"','",valueList$total_pasivo_y_capital,
                 "') ON DUPLICATE KEY UPDATE 
                          contador=contador+1,
                          act_caja_y_bancos=values(act_caja_y_bancos),
                          act_inversiones_en_valores=values(act_inversiones_en_valores),
                          act_cuentas_por_cobrar=values(act_cuentas_por_cobrar),
                          act_clientes=values(act_clientes),
                          act_deudores_diversos_documentos_por_cobrar=values(act_deudores_diversos_documentos_por_cobrar),
                          act_impuestos_por_recuperar=values(act_impuestos_por_recuperar),
                          act_anticipo_a_proveedores=values(act_anticipo_a_proveedores),
                          act_estimacion_de_cuentas_incobrables=values(act_estimacion_de_cuentas_incobrables),
                          act_companias_afiliadas=values(act_companias_afiliadas),
                          act_total_cuentas_por_cobrar=values(act_total_cuentas_por_cobrar),
                          act_inventarios=values(act_inventarios),
                          act_otros_activos_circulantes=values(act_otros_activos_circulantes),
                          act_total_circulante=values(act_total_circulante),
                          act_activos_diferidos=values(act_activos_diferidos),
                          act_documentos_por_cobrar_lgo_pzo=values(act_documentos_por_cobrar_lgo_pzo),
                          act_edificios_y_terrenos=values(act_edificios_y_terrenos),
                          act_maquinaria_y_equipo=values(act_maquinaria_y_equipo),
                          act_depreciacion=values(act_depreciacion),
                          act_total_activo_largo_plazo=values(act_total_activo_largo_plazo),
                          act_total__activo=values(act_total__activo),
                          pas_porcion_circulante_de_creditos_a_lp=values(pas_porcion_circulante_de_creditos_a_lp),
                          pas_prestamos_bancarios_cp=values(pas_prestamos_bancarios_cp),
                          pas_proveedores=values(pas_proveedores),
                          pas_acreedores=values(pas_acreedores),
                          pas_documentos_por_pagar=values(pas_documentos_por_pagar),
                          pas_impuestos_por_pagar=values(pas_impuestos_por_pagar),
                          pas_companias_afiliadas=values(pas_companias_afiliadas),
                          pas_total_pasivo_corto_plazo=values(pas_total_pasivo_corto_plazo),
                          pas_prestamos_bancarios_lp=values(pas_prestamos_bancarios_lp),
                          pas_otros_pasivos_lp=values(pas_otros_pasivos_lp),
                          pas_impuestos_diferidos=values(pas_impuestos_diferidos),
                          pas_total_pasivo_largo_plazo=values(pas_total_pasivo_largo_plazo),
                          pas_total_pasivo=values(pas_total_pasivo),
                          cap_capital_social=values(cap_capital_social),
                          cap_reservas=values(cap_reservas),
                          cap_result_acumulados=values(cap_result_acumulados),
                          cap_revaluacion_de_activo_fijo=values(cap_revaluacion_de_activo_fijo),
                          cap_aportaciones_p_futuros_aumentos_de_capital=values(cap_aportaciones_p_futuros_aumentos_de_capital),
                          cap_resultado_del_ejercicio=values(cap_resultado_del_ejercicio),
                          cap_total_capital_contable=values(cap_total_capital_contable),
                          total_pasivo_y_capital=values(total_pasivo_y_capital)"
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
                        valueList$participacion_utilidades,"','",valueList$utilidad_ejercicio,
            "') ON DUPLICATE KEY UPDATE 
                        contador=contador+1,
                        total_ventas=values(total_ventas),
                        devolucion_sobre_ventas=values(devolucion_sobre_ventas),
                        rebajas_sobre_ventas=values(rebajas_sobre_ventas),
                        total_ventas_netas=values(total_ventas_netas),
                        costo_ventas=values(costo_ventas),
                        utilidad_bruta=values(utilidad_bruta),
                        gastos_operacion=values(gastos_operacion),
                        gastos_venta=values(gastos_venta),
                        gastos_admin=values(gastos_admin),
                        gastos_otros=values(gastos_otros),
                        utilidad_operativa=values(utilidad_operativa),
                        costo_integral_fin=values(costo_integral_fin),
                        gastos_prod_fin=values(gastos_prod_fin),
                        perdida_cambios=values(perdida_cambios),
                        otros_productos=values(otros_productos),
                        otros_ingresos=values(otros_ingresos),
                        utilidad_antes_imptos_partidas_especiales=values(utilidad_antes_imptos_partidas_especiales),
                        provision_impto_activo=values(provision_impto_activo),
                        impto_isr=values(impto_isr),
                        participacion_utilidades=values(participacion_utilidades),
                        utilidad_ejercicio=values(utilidad_ejercicio)",
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

writeBuroDB <- function(params, usuario_id, valueList) { 
  
  query <-paste( "INSERT INTO originacion.info_buro(
                 empresa_info_id, atraso, score_califica, buro_moral_paccionista,
                 buro_fisica_paccionista)"
                 ," VALUES(",
                 valueList$empresa_info_id,",",
                 valueList$atraso,",",
                 valueList$score_califica,",",
                 valueList$buro_moral_paccionista,",",
                 valueList$buro_fisica_paccionista,") 
                 ON DUPLICATE KEY UPDATE 
                 contador=contador+1,
                 atraso=values(atraso),
                 score_califica=values(score_califica),
                 buro_moral_paccionista=values(buro_moral_paccionista),
                 buro_fisica_paccionista=values(buro_fisica_paccionista)",
                 sep="")
  
  con <- dbConnect(MySQL(), 
                   user=params$user,
                   password=params$password,
                   dbname=params$dbname,
                   host=params$host
  )
  dbSendQuery(con, query)
  #res <- dbSendQuery(con, "select @FLAG;")
  dbDisconnect(con)
  #  res
}

writeTermsDB <- function(params, usuario_id, valueList) { 
  
  query <-paste( "INSERT INTO originacion.info_terms(
                 empresa_info_id,monto_solicitado,monto_autorizado,","
                 destino_credito,tipo_ministracion,forma_pago,vigencia_linea,","
                 vigencia_contrato,plazo_disposiciones,tasa_ordinaria,","
                 tasa_moratoria,comision_apertura,fuente_fondeo, costo_fondeo, "," 
                 moneda,garantia,costo_garantia,nombre_aval,nombre_obligado_solidario,
                 propone,autoriza1,autoriza2, comentarios)",
                 " VALUES(",
                 valueList$empresa_info_id,",",valueList$monto_solicitado,",",
                 valueList$monto_autorizado,",'",valueList$destino_credito,"','",
                 valueList$tipo_ministracion,"','",valueList$forma_pago,"',",
                 valueList$vigencia_linea,",",valueList$vigencia_contrato,",'",
                 valueList$plazo_disposiciones,"',",valueList$tasa_ordinaria,",'",
                 valueList$tasa_moratoria,"',",valueList$comision_apertura,",'",
                 valueList$fuente_fondeo,"','", valueList$costo_fondeo, "','",
                 valueList$moneda,"','",
                 valueList$garantia,"',",valueList$costo_garantia,",'",
                 valueList$nombre_aval, "','", valueList$nombre_obligado_solidario, "',",
                 valueList$propone,",",
                 valueList$autoriza1,",",valueList$autoriza2, ",'",
                 valueList$comentarios, 
                 "')                 
                 ON DUPLICATE KEY UPDATE 
                 contador=contador+1,
                 monto_solicitado=values(monto_solicitado),
                 monto_autorizado=values(monto_autorizado),
                 destino_credito=values(destino_credito),
                 tipo_ministracion=values(tipo_ministracion),
                 forma_pago=values(forma_pago),
                 vigencia_linea=values(vigencia_linea),
                 vigencia_contrato=values(vigencia_contrato),
                 plazo_disposiciones=values(plazo_disposiciones),
                 tasa_ordinaria=values(tasa_ordinaria),
                 tasa_moratoria=values(tasa_moratoria),
                 comision_apertura=values(comision_apertura),
                 fuente_fondeo=values(fuente_fondeo),
                 costo_fondeo=values(costo_fondeo),
                 moneda=values(moneda),
                 garantia=values(garantia),
                 costo_garantia=values(costo_garantia),
                 nombre_aval=values(nombre_aval),
                 nombre_obligado_solidario=values(nombre_obligado_solidario),
                 propone=values(propone),
                 autoriza1=values(autoriza1),
                 autoriza2=values(autoriza2), 
                 comentarios=values(comentarios)
                 "
                 ,sep="")
  query <- iconv(query, from="utf-8", to="latin1")
  con <- dbConnect(MySQL(), 
                   user=params$user,
                   password=params$password,
                   dbname=params$dbname,
                   host=params$host
  )
  dbSendQuery(con, query)
  
  dbDisconnect(con)
}

writeScoreDB <- function(params, score, id) {
  query <-paste("update empresa_info set score = ", score, 
                " where id = ", id,
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

getInfoEmpresaDB_reporte <- function(params, empresa_info_id) {
  con <- dbConnect(MySQL(), 
                   user=params$user,
                   password=params$password,
                   dbname=params$dbname,
                   host=params$host
  )
  query <- paste("select 
  e.rfc, 
	e.nombre, 
	e.razon_social,
	i.fecha_informacion,
	i.estado_resultados_fecha,
	i.balance_fecha,
	i.cualitativo_fecha,
	i.buro_fecha,
	i.score,
	u.nombres, 
	u.apellido_paterno, 
	u.apellido_materno, 
	u.area,
  u.puesto
	from empresa_info i inner join empresa e inner join usuario u 
	on i.empresa_id = e.id and e.usuario_insercion_id = u.id 
	where i.id = ",empresa_info_id , sep="")
  dbGetQuery(con, "SET NAMES utf8")
  res <- dbGetQuery(con, query)
  dbDisconnect(con)
  res
}

getInfoUsuario_reporte <- function(params, usuario_id) {
  con <- dbConnect(MySQL(), 
                   user=params$user,
                   password=params$password,
                   dbname=params$dbname,
                   host=params$host
  )
  query <- paste("select nombres, apellido_paterno, apellido_materno, puesto 
                 from usuario 
                 where id = ",usuario_id , sep="")
  dbGetQuery(con, "SET NAMES utf8")
  res <- dbGetQuery(con, query)
  dbDisconnect(con)
  res
}

getInfoTermsDB_reporte <- function(params, empresa_info_id) {
  con <- dbConnect(MySQL(), 
                   user=params$user,
                   password=params$password,
                   dbname=params$dbname,
                   host=params$host
  )
  query <- paste("select 
                 monto_solicitado,
                 monto_autorizado,
                 destino_credito,
                 tipo_ministracion,
                 forma_pago,
                 vigencia_linea,
                 vigencia_contrato,
                 plazo_disposiciones,
                 tasa_ordinaria,
                 tasa_moratoria,
                 comision_apertura,
                 fuente_fondeo, 
                 costo_fondeo,
                 moneda,
                 garantia,
                 costo_garantia,
                 nombre_aval,
                 nombre_obligado_solidario,
                 propone,
                 autoriza1,
                 autoriza2,
                  comentarios
                 from info_terms where empresa_info_id =",empresa_info_id , sep="")
  dbGetQuery(con, "SET NAMES utf8")
  res <- dbGetQuery(con, query)
  dbDisconnect(con)
  outputsReporte=list("Capital de Trabajo"="capital_trabajo")
  res$destino_credito=names(outputsReporte[which(outputsReporte==res$destino_credito)])
  
  outputsReporte=list("Única"="unica")
  res$tipo_ministracion=names(outputsReporte[which(outputsReporte==res$tipo_ministracion)])
  
  outputsReporte=list("Capital e intereses mensual"= "cap_int_mensual")
  res$forma_pago=names(outputsReporte[which(outputsReporte==res$forma_pago)])
  
  outputsReporte=list("Única"="unica", "Mensual"= "mensual")
  res$plazo_disposiciones=names(outputsReporte[which(outputsReporte==res$plazo_disposiciones)])
  
  outputsReporte=list("Recursos propios"="propios", "NAFIN"="nafin")
  res$fuente_fondeo=names(outputsReporte[which(outputsReporte==res$fuente_fondeo)])
  
  outputsReporte=list("MXN"="MXN")
  res$moneda=names(outputsReporte[which(outputsReporte==res$moneda)])
  
  outputsReporte=list("Sin Garantía"="NA", "Hipotecaria"= "hipotecaria")
  res$garantia=names(outputsReporte[which(outputsReporte==res$garantia)])
  
  
  res
}















