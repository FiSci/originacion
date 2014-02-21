library(shiny)
library(devtools)
library(datasets)
library(shinyIncubator)
library(RMySQL)
library(digest)
library(DBI)
library(RODBC)

source("./conf.R")
source("./functions_login.R")
source("./functions_read.R")
source("./functions_format.R")


# Define server logic 
shinyServer(function(input, output) {
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
#      empresasDF <- getEmpresasDB(paramsDB, logedId)
      
      # Lee las empresas de la BD para el usuario registrado
      output$seleccionaEmpresa <- renderUI({
        input$writeEmpresaButton
        empresasDF <- getEmpresasDB(paramsDB, logedId)
        selectInput("empresa_id", "Selecciona Empresa", showEmpresas(empresasDF), "Selecciona Empresa")
      })
      
      output$seleccionaFecha <- renderUI({
        input$writeFechaButton
        empresasDF <- getEmpresasDB(paramsDB, logedId)
        selectInput("empresa_info_id", "Selecciona Fecha", showFechas(input$empresa_id, empresasDF),"")
      })
      
      # Guarda la nueva empresa en la BD      
      writeEmpresa <- reactive({
        if (input$writeEmpresaButton == 0) 
          return(-999)
         
        #Guarda en la BD 
        # Revisar que los datos introducidos tengan el formato especificado
        valueList = isolate(list(nombre=input$capturaEmpNombre, 
                                 rfc=input$capturaEmpRFC, 
                                 razon_social=input$capturaEmpRS))
        ret <- writeEmpresaDB(paramsDB, logedId, valueList)
        # Error handling
        output$writeEmpresaStatusMsg <- renderText({
          switch(ret, 
                 '1'={txt <- "La empresa ya existe en la base, favor de pedir 
                      permiso para acceder a su información al promotor encargado 
                      de la empresa"},
                  {txt <- "Empresa introducida correctamente"}
          )
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
      
      
      ###outputs para el cuadro Información Empresa
      output$nombreEmpresa <- renderText({
        if (input$consultaButton == 0) 
          return("")
        if (existe(isolate(input$empresa_info_id), empresasDF)!=0)
          showInfoEmpresa(isolate(input$empresa_info_id), empresasDF)[1]
        else
          return("")
      })
      
      output$rfcEmpresa <- renderText({ 
        if (input$consultaButton == 0) 
          return("")
        if (existe(isolate(input$empresa_info_id), empresasDF)!=0)
          showInfoEmpresa(isolate(input$empresa_info_id), empresasDF)[2]
        else
          return("")
      })
      
      output$rsEmpresa <- renderText({ 
        if (input$consultaButton == 0) 
          return("")
        if (existe(isolate(input$empresa_info_id), empresasDF)!=0)
          showInfoEmpresa(isolate(input$empresa_info_id), empresasDF)[3]
        else
          return("")
      })  
      
      output$Status<- renderText({
        if (input$consultaButton == 0) 
          return("")
        if (existe(isolate(input$empresa_info_id), empresasDF)!=0)
          showStatus(isolate(input$empresa_info_id),empresasDF)
        else
          return("")
      })
      
      output$Balance<- renderText({
        if (input$consultaButton == 0) return("")
        input$writeBalanceButton
        empresasDF <- getEmpresasDB(paramsDB, logedId)
        if (existe(isolate(input$empresa_info_id), empresasDF)!=0)
          if(Captura(isolate(input$empresa_info_id),empresasDF)$balance_fecha==1)
            return("Capturado")
        else return("No Capturado")
        else
          return("")
      })
      
      output$Estado<- renderText({
        if (input$consultaButton == 0) 
          return("")
        input$writeEstadoButton
        empresasDF <- getEmpresasDB(paramsDB, logedId)
        if (existe(isolate(input$empresa_info_id), empresasDF)!=0)
          if (Captura(isolate(input$empresa_info_id),empresasDF)$estado_resultados_fecha==1)
            return("Capturado")
        else return("No Capturado")
        else
          return("")
      })
      
      output$Cualit <- renderText({
        if (input$consultaButton == 0) 
          return(NULL) 
        input$writeCualitativosButton
        empresasDF <- getEmpresasDB(paramsDB, logedId)
          if (existe(isolate(input$empresa_info_id), empresasDF)!=0)
            if (Captura(isolate(input$empresa_info_id),empresasDF)$cualitativo_fecha==1)
              return("Capturado")
            else return("No Capturado")
          else
            return("")
               
      })
      

      ####outputs para el cuadro de Estados Financieros
      
      ##Despliega Información de Cualitativa
      output$tableCualit <- renderTable({
        if (input$consultaButton == 0) 
           return(NULL)
        if(existe(isolate(input$empresa_info_id), empresasDF)!=0){
          if (input$writeCualitativosButton >= 0){
            cualitativosDF<-getInfoCualitativosDB(paramsDB,isolate(input$empresa_info_id))
            if(existe(isolate(input$empresa_info_id), cualitativosDF)!=0){
              dat<-creaTablaCualitativos(cualitativosDF,isolate(input$empresa_info_id))
              return(dat)
            }
            else
              return(NULL)
        }}
        else
          return(NULL)   
       })
      
      ##Despliega Información de Balance
      output$tableBalance <- renderTable({
        if (input$consultaButton == 0) 
          return(NULL)
        if(existe(isolate(input$empresa_info_id), empresasDF)!=0){
          if (input$writeBalanceButton >= 0){
            balanceDF<-getInfoBalanceDB(paramsDB,isolate(input$empresa_info_id))
            if(existe(isolate(input$empresa_info_id), balanceDF)!=0){
                dat<-creaTablaBalance(balanceDF,isolate(input$empresa_info_id))
                return(dat)
            }
            else
                return(NULL)
        }}
        else
          return(NULL)   
      })
      
      ##Despliega Información de Estado
      output$tableEdoRes <- renderTable({
        if (input$consultaButton == 0) 
          return(NULL)
          if(existe(isolate(input$empresa_info_id), empresasDF)!=0){
            if (input$writeEstadoButton >= 0){
              EdoResDF<-getInfoEdoResDB(paramsDB,isolate(input$empresa_info_id))
              if(existe(isolate(input$empresa_info_id), EdoResDF)!=0){  
                dat<-creaTablaEdoRes(EdoResDF,isolate(input$empresa_info_id))
                return(dat)
              }
              else
                return(NULL)
            }}
        else
          return(NULL)   
      })
      
      #####Grabar en la BD los estados financieros 
      # Guarda la informacion de Cualitativos en la BD      
        writeCualitativos <- reactive({
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
          writeCualitativosDB(paramsDB, logedId, valueList)
          0
        })
        output$writeCualitativos <- renderText(writeCualitativos())
      
      
      # Guarda la informacion de Balance en la BD      
      writeBalance <- reactive({
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
        writeBalanceDB(paramsDB, logedId, valueList)
        0
      })
      output$writeBalance <- renderText(writeBalance())
      
      # Guarda la informacion de Estado en la BD      
      writeEstado <- reactive({
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
        writeEstadoDB(paramsDB, logedId, valueList)
        0
      })
      output$writeEstado <- renderText(writeEstado())
      
      
    }    
  })
  
  
})
    
