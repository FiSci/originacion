library(shiny)
library(devtools)
library(shinyIncubator)
library(RMySQL)
library(digest)

source("./conf.R")
source("./functions_login.R")
source("./functions_read.R")
source("./functions_format.R")
source("./functions_score.R")
source("./functions_check.R")

# Define server logic 
shinyServer(function(input, output, session) {
  # Lee la lista de usuarios de la BD
  usuariosDF <- getUsuariosDB(paramsDB)
  
  # Lee lista de usuarios para mostrarlos
  output$seleccionaUsuario <- renderUI({
     selectInput("usuario_id", "Selecciona Usuario:", showUsuarios(usuariosDF))
   })
  # Hace log in
  loginStatus <- reactive({
    if(input$loginButton == 0) 
      return(-999)
    # Verifica si el password ingresado es correcto
    input$loginButton
    ret <- isolate({userAuth(input$usuario_id, input$password, paramsDB)})
    # Mensaje de error
    output$loginStatusMsg <- renderText({
      if(ret == 0){
        txt <- "Login exitoso"
      }else {
        txt <- "Error en la contraseña"
      } 
      txt
    })
   ret
  }) 
  # Asigna la respuesta de login a un output
  output$loginStatus <- renderText(loginStatus())
    
  # Logica de cambio de password
  cambioPassStatus <- reactive({
    if(input$newPasswordButton == 0)
      return(-999)
    # Verifica si el cambio de contraseña fue correcto
    ret <- isolate({
      userNewPass(input$usuario_id, 
                  input$passwordReg,
                  input$newPasswordReg1,
                  input$newPasswordReg2,
                  paramsDB)
    })
    # Mensaje de error
     output$cambioPassStatusMsg <- renderText({
       switch(as.character(ret),
              "0"={txt <- "Cambio de password exitoso"},
              "1"={txt <- "Error en la contraseña"},
              "-1"={txt <- "Password no coincide"}
              )
       txt
     })
    ret  
  })
  # Asigna la respuesta de cambio password a un output
  output$cambioPassStatus <- renderText(cambioPassStatus())
  
  #############################################
  # Si el login es correcto, toda la logica del programa
  observe({
    if(loginStatus() == 0) {
      # Lee permisos del usuario
      logedId <- isolate(input$usuario_id)
      
      empresasDF <- reactive({
        input$usuario_id
        writeEmpresa()
        writeFecha()
        writeCualitativos()
        writeEstado()
        writeBalance()
        getEmpresasDB(paramsDB, logedId)  
      })
        
      # Lee las empresas de la BD para el usuario registrado
      output$seleccionaEmpresa <- renderUI({
        selectInput("empresa_id", "Selecciona Empresa", showEmpresas(empresasDF()), "Selecciona Empresa")
      })
      
      output$seleccionaFecha <- renderUI({
        selectInput("empresa_info_id", "Selecciona Fecha", showFechas(input$empresa_id, empresasDF()), "Selecciona Fecha")
      })
      
      # Guarda la nueva empresa en la BD      
      writeEmpresa <- reactive({
        if (input$writeEmpresaButton == 0) 
          return(-999)
         
        #Guarda en la BD 
        # Revisar que los datos introducidos tengan el formato especificado
        valueList = isolate(list(nombre=inputFormat(input$capturaEmpNombre), 
                                 rfc=inputFormat(input$capturaEmpRFC)))
        
        # Revisa que el RFC sea correcto
        # Error handling
        if(checkRFC(valueList$rfc)) {
          ret <- -1
        } else{
          # Guarda la empresa en la BD
          ret <- writeEmpresaDB(paramsDB, logedId, valueList)  
        }
        output$writeEmpresaStatusMsg <- renderText({
          if(ret == 1) {
            return("La empresa ya existe en la base. Si no la puedes ver en el menú 
             pide permiso al promotor encargado de esa empresa")
          }
          if(ret == -1) {
            return("El formato del RFC debe ser ABC123456XXX")
          } else {
            "Empresa introducida correctamente"
          }
        })
        ret
      })
      output$writeEmpresa <- renderText(writeEmpresa())

      # Guarda nueva fecha
      writeFecha <- reactive({
        if (input$writeFechaButton == 0) 
          return(-999)
        #Guarda en la BD 
        # Revisar que los datos introducidos tengan el formato especificado
        valueList = isolate(list(fecha=input$capturaFecha,
                                 empresa=input$empresa_id))
        ret <- writeFechaDB(paramsDB, logedId, valueList)
        # Error handling
        output$writeFechaStatusMsg <- renderText({
          switch(ret, 
            '1'={txt <- "La fecha ya existe para esa empresa"},
            {txt <- "Fecha introducida correctamente"}
          )
        })
        ret
      })
      output$writeFecha <- renderText(writeFecha())
      
      
      ### Genera el cuadro de informacion de la empresa
      observe({
        empresa_id <- input$empresa_id
        empresasDF <- empresasDF()
        if(!is.null(empresa_id)){
          if(empresa_id == -999 | empresa_id == -998) {
            output$nombreEmpresa <- renderText("NO HAY EMPRESA/FECHA SELECCIONADA")
            output$rfcEmpresa <- renderText("NO HAY EMPRESA/FECHA SELECCIONADA")
            output$rsEmpresa <- renderText("NO HAY EMPRESA/FECHA SELECCIONADA")
          } else {
            output$nombreEmpresa <- renderText(showInfoEmpresa(empresa_id, empresasDF)[1])
            output$rfcEmpresa <- renderText(showInfoEmpresa(empresa_id, empresasDF)[2])
            output$rsEmpresa <- renderText(showInfoEmpresa(empresa_id, empresasDF)[3])
          }
        }
      })
      
      observe({
        empresa_info_id <- input$empresa_info_id
        empresasDF <- empresasDF()
        if(!is.null(empresa_info_id)) {
          if (empresa_info_id == -999 | empresa_info_id == -998) {
            output$Balance <- renderText("NO HAY EMPRESA/FECHA SELECCIONADA")
            output$Estado <- renderText("NO HAY EMPRESA/FECHA SELECCIONADA")
            output$Cualit <- renderText("NO HAY EMPRESA/FECHA SELECCIONADA")
            output$status <- renderText("NO HAY EMPRESA/FECHA SELECCIONADA")
#            output$calificacion <- renderText("NO HAY EMPRESA/FECHA SELECCIONADA")
            
          } else {          
            output$Balance <- renderText("No Capturado")
            output$Estado <- renderText("No Capturado")
            output$Cualit <- renderText("No Capturado")
            
            status <- showStatus(empresa_info_id,empresasDF)
            output$status <- renderText(status)
             
            actualizaCuadro <- Captura(empresa_info_id, empresasDF)            
            if(actualizaCuadro$balance_fecha == 1) {
              output$Balance <- renderText("Capturado")
            }
            if (actualizaCuadro$estado_resultados_fecha == 1) {
              output$Estado <- renderText("Capturado")
            }
            if (actualizaCuadro$cualitativo_fecha == 1) {  
              output$Cualit <- renderText("Capturado")
            }
            if (status == "Completo") {
              calificacion <- getScoreDB(paramsDB, empresa_info_id)
              output$calificacion <- renderText(calificacion)
            }
          }
        }
        
      })

      #Despliega informacion financiera de la empresa         
      observe({
        # Score flag: si la informacion esta completa (flag=1) obtiene el score
        scoreFlag <- 0
        # Reactive value: cuando cambia el id de la empresa o se introduce una nueva,
        # lee la informacion de la empresa con ese id 
        empresa_info_id <- input$empresa_info_id
        empresasDF <- empresasDF()
        # Regresa Null por default para evitar que aparezca menu para introducir datos al mismo
        # tiempo que la tabla
        output$tableCualit <- renderTable({NULL})
        output$tableBalance <- renderTable({NULL})
        output$tableEdoRes <- renderTable({NULL})
        if(!is.null(empresa_info_id)) {
          if(empresa_info_id != -999 & empresa_info_id != -998) {
            cualitativosDF <- getInfoCualitativosDB(paramsDB, empresa_info_id)
            balanceDF <- getInfoBalanceDB(paramsDB, empresa_info_id)
            EdoResDF <- getInfoEdoResDB(paramsDB, empresa_info_id)

            if(dim(cualitativosDF)[1] > 0 ) {
              output$tableCualit <- renderTable({creaTablaCualitativos(cualitativosDF,empresa_info_id)})
              scoreFlag <- 1
            }
            if(dim(balanceDF)[1] > 0 ) {
              output$tableBalance <- renderTable({creaTablaBalance(balanceDF,empresa_info_id)})
              scoreFlag <- scoreFlag + 1
            }
            if(dim(EdoResDF)[1] > 0 ) {
              output$tableEdoRes <- renderTable({creaTablaEdoRes(EdoResDF,empresa_info_id)})
              scoreFlag <- scoreFlag + 1
            }
          } 
        }
#         output$score <- renderText({
#           if(scoreFlag < 3) 
#             return(-999)
#           else if(score){
#             
#           }
#           score(cualitativosDF, balanceDF, EdoResDF)
#         })
      })
      
      





      observe({
        # Actualiza el valor de los inputs a 0
        input$empresa_info_id
        updateNumericInput(session, inputId="edad_principal_accionista", value=0)
        updateNumericInput(session, inputId="antiguedad_principal_accionista_domicilio", value=0)
        updateNumericInput(session, inputId="antiguedad_negocio", value=0)
        updateNumericInput(session, inputId="experiencia_principal_accionista_giro", value=0)
        updateNumericInput(session, inputId="estados_financieros", value=0)
        updateNumericInput(session, inputId="ventas_anuales", value=0)  
        updateNumericInput(session, inputId="act_caja_y_bancos", value=0)
        updateNumericInput(session, inputId="act_inversiones_en_valores", value=0)
        updateNumericInput(session, inputId="act_cuentas_por_cobrar", value=0)
        updateNumericInput(session, inputId="act_clientes", value=0)
        updateNumericInput(session, inputId="act_deudores_diversos_documentos_por_cobrar", value=0)
        updateNumericInput(session, inputId="act_impuestos_por_recuperar", value=0)
        updateNumericInput(session, inputId="act_anticipo_a_proveedores", value=0)
        updateNumericInput(session, inputId="act_estimacion_de_cuentas_incobrables", value=0)
        updateNumericInput(session, inputId="act_companias_afiliadas", value=0)
        updateNumericInput(session, inputId="act_total_cuentas_por_cobrar", value=0)
        updateNumericInput(session, inputId="act_inventarios", value=0)
        updateNumericInput(session, inputId="act_otros_activos_circulantes", value=0)
        updateNumericInput(session, inputId="act_total_circulante" , value=0)
        updateNumericInput(session, inputId="act_activos_diferidos" , value=0)
        updateNumericInput(session, inputId="act_documentos_por_cobrar_lgo_pzo" , value=0)
        updateNumericInput(session, inputId="act_edificios_y_terrenos" , value=0)
        updateNumericInput(session, inputId="act_maquinaria_y_equipo" , value=0)
        updateNumericInput(session, inputId="act_depreciacion" , value=0)
        updateNumericInput(session, inputId="act_total_activo_largo_plazo" , value=0)
        updateNumericInput(session, inputId="act_total__activo" , value=0)
        updateNumericInput(session, inputId="pas_porcion_circulante_de_creditos_a_lp" , value=0)
        updateNumericInput(session, inputId="pas_prestamos_bancarios_cp" , value=0)
        updateNumericInput(session, inputId="pas_proveedores" , value=0)
        updateNumericInput(session, inputId="pas_acreedores" , value=0)
        updateNumericInput(session, inputId="pas_documentos_por_pagar" , value=0)
        updateNumericInput(session, inputId="pas_impuestos_por_pagar" , value=0)
        updateNumericInput(session, inputId="pas_companias_afiliadas" , value=0)
        updateNumericInput(session, inputId="pas_total_pasivo_corto_plazo" , value=0)
        updateNumericInput(session, inputId="pas_prestamos_bancarios_lp" , value=0)
        updateNumericInput(session, inputId="pas_otros_pasivos_lp" , value=0)
        updateNumericInput(session, inputId="pas_impuestos_diferidos" , value=0)
        updateNumericInput(session, inputId="pas_total_pasivo_largo_plazo" , value=0)
        updateNumericInput(session, inputId="pas_total_pasivo" , value=0)
        updateNumericInput(session, inputId="cap_capital_social" , value=0)
        updateNumericInput(session, inputId="cap_reservas" , value=0)
        updateNumericInput(session, inputId="cap_result_acumulados" , value=0)
        updateNumericInput(session, inputId="cap_revaluacion_de_activo_fijo" , value=0)
        updateNumericInput(session, inputId="cap_aportaciones_p_futuros_aumentos_de_capital" , value=0)
        updateNumericInput(session, inputId="cap_resultado_del_ejercicio" , value=0)
        updateNumericInput(session, inputId="cap_total_capital_contable" , value=0)
        updateNumericInput(session, inputId="total_pasivo_y_capital" , value=0)
        updateNumericInput(session, inputId="total_ventas" ,value=0)
        updateNumericInput(session, inputId="devolucion_sobre_ventas" , value=0)
        updateNumericInput(session, inputId="rebajas_sobre_ventas" , value=0)
        updateNumericInput(session, inputId="total_ventas_netas" , value=0)
        updateNumericInput(session, inputId="costo_ventas" , value=0)
        updateNumericInput(session, inputId="utilidad_bruta" , value=0)
        updateNumericInput(session, inputId="gastos_operacion" , value=0)
        updateNumericInput(session, inputId="gastos_venta" , value=0)
        updateNumericInput(session, inputId="gastos_admin" , value=0)
        updateNumericInput(session, inputId="gastos_otros" , value=0)
        updateNumericInput(session, inputId="utilidad_operativa" , value=0)
        updateNumericInput(session, inputId="costo_integral_fin" , value=0)
        updateNumericInput(session, inputId="gastos_prod_fin" , value=0)
        updateNumericInput(session, inputId="perdida_cambios" , value=0)
        updateNumericInput(session, inputId="otros_productos" , value=0)
        updateNumericInput(session, inputId="otros_ingresos" , value=0)
        updateNumericInput(session, inputId="utilidad_antes_imptos_partidas_especiales" , value=0)
        updateNumericInput(session, inputId="provision_impto_activo" , value=0)
        updateNumericInput(session, inputId="impto_isr" , value=0)
        updateNumericInput(session, inputId="participacion_utilidades" , value=0)
        updateNumericInput(session, inputId="utilidad_ejercicio" , value=0)
      })
      
      ##Despliega Información de Balance
        
      #####Grabar en la BD los estados financieros 
      # Guarda la informacion de Cualitativos en la BD      
      writeCualitativos <- reactive({
        err <- 0
        if (input$writeCualitativosButton == 0) 
          return(-999)
        #Guarda en la BD 
        # Revisar que los datos introducidos tengan el formato especificado
        valueList = isolate(list(empresa_info_id=input$empresa_info_id,
                                 edad_principal_accionista=input$edad_principal_accionista, 
                                 antiguedad_principal_accionista_domicilio=input$antiguedad_principal_accionista_domicilio,
                                 antiguedad_negocio=input$antiguedad_negocio, 
                                 experiencia_principal_accionista_giro=input$experiencia_principal_accionista_giro, 
                                 estados_financieros=input$estados_financieros,
                                 ventas_anuales=input$ventas_anuales
        ))
        ### Los errores van del más general al más particular con la finalidad
        # de detectarlos desde el principio
        
        if(noVacios(valueList) != 0) {
          err <- 5
          errMsg <- "ERROR: No es posible introducir datos vacios"
        } else {
          if(checkCualit_edad(valueList, min=15, max=99)) {
            err <- 10
            errMsg <- "ERROR: Edad del principal accionista fuera de rango"
          }
          if(checkCualit_antiguedadDomicilio(valueList, min=0, max=99)) {
            err <- 20
            errMsg <- "ERROR: Antigüedad del principal accionista en domicilio fuera de rango"
          }
          if(checkCualit_antiguedadNegocio(valueList, min=0, max=99)) {
            err <- 30
            errMsg <- "ERROR: Antigüedad del principal accionista en negocio fuera de rango"
          }
          if(checkCualit_experienciaPrincipalAccionistaGiro(valueList, min=0, max=99)) {
            err <- 40
            errMsg <- "ERROR: Experiencia del principal accionista fuera de rango"
          }
          if(checkCualit_ventasAnuales(valueList, min=0, max=100000000)) {
            err <- 50
            errMsg <- "ERROR: Ventas anuales fuera de rango"
          } 
          if(checkCualit_estadosFinancieros(valueList)) {
            err <- 60
            errMsg <- "ERROR: Estados financieros 1.-Si están auditados 0.-Si NO están auditados"
          } 
        }
        output$writeCualitativosMsg <- renderText({
          if(err == 0) {
            return(NULL)
          }else{
            errMsg
          }
        })
        if(err == 0) {
          writeCualitativosDB(paramsDB, logedId, valueList)
        }
        err
      })
      output$writeCualitativos <- renderText(writeCualitativos())
      
      
      # Guarda la informacion de Balance en la BD      
      writeBalance <- reactive({
        err <- 0
        if (input$writeBalanceButton == 0)
          return(-999) 
          #Guarda en la BD 
          # Revisar que los datos introducidos tengan el formato especificado
        valueList = isolate(list(empresa_info_id=input$empresa_info_id,
                                 act_caja_y_bancos=input$act_caja_y_bancos, 
                                 act_inversiones_en_valores=input$act_inversiones_en_valores,
                                 act_cuentas_por_cobrar=input$act_cuentas_por_cobrar, 
                                 act_clientes=input$act_clientes, 
                                 act_deudores_diversos_documentos_por_cobrar=input$act_deudores_diversos_documentos_por_cobrar, 
                                 act_impuestos_por_recuperar=input$act_impuestos_por_recuperar, 
                                 act_anticipo_a_proveedores=input$act_anticipo_a_proveedores, 
                                 act_estimacion_de_cuentas_incobrables=input$act_estimacion_de_cuentas_incobrables,
                                 act_companias_afiliadas=input$act_companias_afiliadas,
                                 act_total_cuentas_por_cobrar=input$act_total_cuentas_por_cobrar,
                                 act_inventarios=input$act_inventarios,
                                 act_otros_activos_circulantes=input$act_otros_activos_circulantes,
                                 act_total_circulante=input$act_total_circulante,
                                 act_activos_diferidos=input$act_activos_diferidos,
                                 act_documentos_por_cobrar_lgo_pzo=input$act_documentos_por_cobrar_lgo_pzo, 
                                 act_edificios_y_terrenos=input$act_edificios_y_terrenos, 
                                 act_maquinaria_y_equipo=input$act_maquinaria_y_equipo, 
                                 act_depreciacion=input$act_depreciacion,
                                 act_total_activo_largo_plazo=input$act_total_activo_largo_plazo,
                                 act_total__activo=input$act_total__activo, 
                                 pas_porcion_circulante_de_creditos_a_lp=input$pas_porcion_circulante_de_creditos_a_lp,
                                 pas_prestamos_bancarios_cp=input$pas_prestamos_bancarios_cp, 
                                 pas_proveedores=input$pas_proveedores, 
                                 pas_acreedores=input$pas_acreedores, 
                                 pas_documentos_por_pagar=input$pas_documentos_por_pagar,
                                 pas_impuestos_por_pagar=input$pas_impuestos_por_pagar,
                                 pas_companias_afiliadas=input$pas_companias_afiliadas,
                                 pas_total_pasivo_corto_plazo=input$pas_total_pasivo_corto_plazo,
                                 pas_prestamos_bancarios_lp=input$pas_prestamos_bancarios_lp,
                                 pas_otros_pasivos_lp=input$pas_otros_pasivos_lp,
                                 pas_impuestos_diferidos=input$pas_impuestos_diferidos,
                                 pas_total_pasivo_largo_plazo=input$pas_total_pasivo_largo_plazo,
                                 pas_total_pasivo=input$pas_total_pasivo,
                                 cap_capital_social=input$cap_capital_social,
                                 cap_reservas=input$cap_reservas,
                                 cap_result_acumulados=input$cap_result_acumulados,
                                 cap_revaluacion_de_activo_fijo=input$cap_revaluacion_de_activo_fijo,
                                 cap_aportaciones_p_futuros_aumentos_de_capital=input$cap_aportaciones_p_futuros_aumentos_de_capital,
                                 cap_resultado_del_ejercicio=input$cap_resultado_del_ejercicio,
                                 cap_total_capital_contable=input$cap_total_capital_contable,
                                 total_pasivo_y_capital=input$total_pasivo_y_capital
        ))
        ### Los errores van del más general al más particular con la finalidad
        # de detectarlos desde el principio
        
        if(noVacios(valueList) != 0) {
          err <- 5
          errMsg <- "ERROR: No es posible introducir datos vacios"
        } else {
          if(checkBal_PasivoCapital(valueList, tol=.005) != 0) {
            err <- 10
            errMsg <- "ERROR: Total Pasivo y Capital = Total Pasivo + Total Capital Contable"
          }
          if(checkBal_ActivosPasivos(valueList, tol=.005) != 0) {
            err <- 20
            errMsg <- "ERROR: Total Activo = Total Pasivo + Total Capital Contable"
          }
          if(checkBal_ActivosTotales(valueList, tol=.005) != 0) {
            err <- 30
            errMsg <- "ERROR: Total Activo = Total Circulante + Activos Diferidos + Total Activo Largo Plazo"
          }
        }
        output$writeBalanceMsg <- renderText({
          if(err==0) {
            return(NULL)
          }else{
            errMsg
          }
        })
        if(err == 0) {
          writeBalanceDB(paramsDB, logedId, valueList)          
        }
        err
      })
      output$writeBalance <- renderText(writeBalance())
      
      # Guarda la informacion de Estado en la BD      
      writeEstado <- reactive({
        err <- 0
        if (input$writeEstadoButton == 0) 
          return(-999)
        #Guarda en la BD 
        # Revisar que los datos introducidos tengan el formato especificado
        valueList = isolate(list(empresa_info_id=input$empresa_info_id,
                                 total_ventas=input$total_ventas, 
                                 devolucion_sobre_ventas=input$devolucion_sobre_ventas, 
                                 rebajas_sobre_ventas=input$rebajas_sobre_ventas,
                                 total_ventas_netas=input$total_ventas_netas, 
                                 costo_ventas=input$costo_ventas, 
                                 utilidad_bruta=input$utilidad_bruta,
                                 gastos_operacion=input$gastos_operacion, 
                                 gastos_venta=input$gastos_venta, 
                                 gastos_admin=input$gastos_admin, 
                                 gastos_otros=input$gastos_otros,
                                 utilidad_operativa=input$utilidad_operativa, 
                                 costo_integral_fin=input$costo_integral_fin, 
                                 gastos_prod_fin=input$gastos_prod_fin, 
                                 perdida_cambios=input$perdida_cambios, 
                                 otros_productos=input$otros_productos, 
                                 otros_ingresos=input$otros_ingresos,
                                 utilidad_antes_imptos_partidas_especiales=input$utilidad_antes_imptos_partidas_especiales, 
                                 provision_impto_activo=input$provision_impto_activo,
                                 impto_isr=input$impto_isr, 
                                 participacion_utilidades=input$participacion_utilidades, 
                                 utilidad_ejercicio=input$utilidad_ejercicio
        ))
        if(noVacios(valueList) != 0) {
          err <- 5
          errMsg <- "ERROR: No es posible introducir datos vacios"
        } else {
          if(checkEdoRes_UtilidadBruta(valueList, tol=.005) != 0) {
            err <- 10
            errMsg <- "ERROR: Utilidad Bruta = Ventas totales - Costo de ventas"
          }
        }
        output$writeEstadoResMsg <- renderText({
          if(err==0) {
            return(NULL)
          }else{
            errMsg
          }
        })
        if(err == 0) {
          writeEstadoDB(paramsDB, logedId, valueList)          
        }
        err
      })
      output$writeEstado <- renderText(writeEstado())
      
    }    
  })
  
  
})
    
