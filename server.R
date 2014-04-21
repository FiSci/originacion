sistema_calificacion_version <- "V1.0"

library(shiny)
library(devtools)
library(shinyIncubator)
library(RMySQL)
library(digest)
library(knitr)
library(xtable)

source("./conf.R")
source("./functions_login.R")
source("./functions_read.R")
source("./functions_format.R")
source("./functions_score.R")
source("./functions_check.R")

reglas_calificacion <- read.csv("conceptos_calificacion.csv")
catalogo_cualitativo <- read.csv("catalogo_cualitativo.csv", colClasses=c("character", "character"))
catalogo_balance <- read.csv("catalogo_balance.csv", colClasses=c("character", "character"))
catalogo_estado <- read.csv("catalogo_estado.csv", colClasses=c("character", "character"))
catalogo_buro <- read.csv("catalogo_buro.csv", colClasses=c("character", "character"))


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
        modificaInfo()
        writeEmpresa()
        writeBuro()
        writeFecha()
        writeCualitativos()
        writeEstado()
        writeBalance()
        calificacion()
        getEmpresasDB(paramsDB, logedId)  
      })
      
      # Lee las empresas de la BD para el usuario registrado
      output$seleccionaEmpresa <- renderUI({
        selectInput("empresa_id", "Selecciona Empresa", showEmpresas(empresasDF()), "Selecciona Empresa")
      })
      
      output$seleccionaFecha <- renderUI({
        selectInput("empresa_info_id", "Selecciona Fecha", showFechas(input$empresa_id, empresasDF()), "Selecciona Fecha")
      })
      
      ###################################
      # Guarda nueva empresa
      ###################################    
      writeEmpresa <- reactive({
        if (input$writeEmpresaButton == 0) 
          return(-999)
        
        #Guarda en la BD 
        # Revisar que los datos introducidos tengan el formato especificado
        valueList = isolate(list(nombre=inputFormat(input$capturaEmpNombre), 
                                 rfc=inputFormat(input$capturaEmpRFC),
                                 tipo_persona=input$tipoPersona
                                 ))
        
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
                 '1'= {txt <- "La fecha ya existe para esa empresa"},
                      {txt <- "Fecha introducida correctamente"}
          )
        })
        ret
      })
      output$writeFecha <- renderText(writeFecha())

###################################
# Despliega informacion 
###################################
# Despliega nombre y RFC de la empresa
observe({
  empresa_id <- input$empresa_id
  empresasDF <- isolate(empresasDF())
  if(!is.null(empresa_id)){
    if(empresa_id == -999 | empresa_id == -998) {
      output$nombreEmpresa <- renderText("NO HAY EMPRESA/FECHA SELECCIONADA")
      output$rfcEmpresa <- renderText("NO HAY EMPRESA/FECHA SELECCIONADA")
      output$tipoPersona <- renderText("NO HAY EMPRESA/FECHA SELECCIONADA")
    } else {
      output$nombreEmpresa <- renderText(showInfoEmpresa(empresa_id, empresasDF)[1])
      output$rfcEmpresa <- renderText(showInfoEmpresa(empresa_id, empresasDF)[2])
      output$tipoPersona <- renderText(showInfoEmpresa(empresa_id, empresasDF)[3])
    }
  }
})

# Despliega variables de estatus de la empresa
observe({
  empresa_info_id <- input$empresa_info_id
  empresasDF <- empresasDF()
  if(!is.null(empresa_info_id)) {
    if (empresa_info_id == -999 | empresa_info_id == -998) {
      output$Balance <- renderText(-999)
      output$Estado <- renderText(-999)
      output$Cualit <- renderText(-999)
      output$Buro <- renderText(-999)
      output$status <- renderText(-999)
      #            output$calificacion <- renderText("NO HAY EMPRESA/FECHA SELECCIONADA")
      
    } else {          
      output$Balance <- renderText("No Capturado")
      output$Estado <- renderText("No Capturado")
      output$Cualit <- renderText("No Capturado")
      output$Buro <- renderText("No Capturado")
      
      status <- showStatus(empresa_info_id, empresasDF)
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
      if (actualizaCuadro$buro_fecha == 1) {  
        output$Buro <- renderText("Capturado")
      }
#      if (status == "Completo") {
        calificacion <- getScoreDB(paramsDB, empresa_info_id)
        output$calificacion <- renderText(calificacion)
        output$calificacionMsg <- renderText({
          if(calificacion == 0)
            return('<div><h4>EMPRESA NO CALIFICADA</h4></div>')
          if(calificacion == 1)
            return('<div style="color:red"><h4>EMPRESA RECHAZADA PARA PRODUCTO PYMES</h4></div>')
          if(calificacion == 2)
            return('<div style="color:#FFCC00"><h4>EMPRESA PARCIALMENTE APROBADA PARA PRODUCTO PYMES</h4></div>')
          if(calificacion == 3)
            return('<div style="color:green"><h4>EMPRESA APROBADA PARA PRODUCTO PYMES</h4></div>')
        })
        output$downloadDictamenPDF <- downloadHandler(
          filename = "report.pdf",
          content = function(file){
            dat <- getInfoEmpresaDB_reporte(paramsDB, empresa_info_id)
            usuario <- getInfoUsuario_reporte(paramsDB, input$usuario_id)
            
            fileName <- paste("calif", format(Sys.time(), "%Y%m%d%H%M%S"), sep="")
            if(dat$score == 0)
              msg <- 'EMPRESA NO CALIFICADA'
            if(dat$score == 1){
              msg <- 'EMPRESA RECHAZADA PARA PRODUCTO PYMES'
              fileName2 <- "reporte_a.Rnw"
            }
            if(dat$score == 2){
              msg <- 'EMPRESA PARCIALMENTE APROBADA PARA PRODUCTO PYMES'
              fileName2 <- "reporte_a.Rnw"
            }
            if(dat$score == 3) {
              msg <- 'EMPRESA APROBADA PARA PRODUCTO PYMES'
              fileName2 <- "reporte_a.Rnw"
            }
              
            #
            fechaReporte <- format(Sys.time(), '%Y-%m-%d %H:%M:%S')
            # generate PDF
            knit2pdf(paste("./latex/", fileName2, sep=""), output=paste("./latex/", fileName, ".tex", sep=""))
    
            # copy pdf to 'file'
            file.copy(paste("./latex/", fileName, ".pdf", sep=""), file)
    
            # delete generated files
            file.remove(
                paste("./latex/", fileName, ".pdf", sep=""), 
                paste("./latex/", fileName, ".tex", sep=""), 
                paste("./latex/", fileName, ".aux", sep=""), 
                paste("./latex/", fileName, ".log", sep="")
                )
    
            # delete folder with plots
            # unlink("figure", recursive = TRUE)
          },
          contentType = "application/pdf"
        )
#      }
    }
  }
})

# Califica
calificacion <- reactive({
    if(input$calculaScoreButton == 0)
      return(-999)
    empresa_id <- isolate({input$empresa_id})
    # Lee info de la BD
    empresa_info_id <- isolate(input$empresa_info_id)
    
    cualitativosDF <- getInfoCualitativosDB(paramsDB, empresa_info_id)
    balanceDF <- getInfoBalanceDB(paramsDB, empresa_info_id)
    EdoResDF <- getInfoEdoResDB(paramsDB, empresa_info_id)
    buroDF <- getInfoBuroDB(paramsDB, empresa_info_id)
    # Calcula calificacion
      score <- calculaCalificacion(reglas_calificacion, cualitativosDF, balanceDF, EdoResDF, buroDF, getTipoPersonaDB(paramsDB, empresa_id))
      writeScoreDB(paramsDB, score, empresa_info_id)
      score    
})

# Despliega informacion financiera de la empresa         
observe({
  # Reactive value: cuando cambia el id de la empresa o se introduce una nueva,
  # lee la informacion de la empresa con ese id 
  empresa_info_id <- input$empresa_info_id
  empresa_id <- isolate(input$empresa_id)
  empresasDF <- empresasDF()
  # Regresa Null por default para evitar que aparezca menu para introducir datos al mismo
  # tiempo que la tabla
  output$tableCualit <- renderTable({NULL})
  output$tableBalance <- renderTable({NULL})
  output$tableEdoRes <- renderTable({NULL})
  output$tableBuro <- renderTable({NULL})
  
  if(!is.null(empresa_info_id)) {
    if(empresa_info_id != -999 & empresa_info_id != -998) {
      cualitativosDF <- getInfoCualitativosDB(paramsDB, empresa_info_id)
      balanceDF <- getInfoBalanceDB(paramsDB, empresa_info_id)
      EdoResDF <- getInfoEdoResDB(paramsDB, empresa_info_id)
      buroDF <- getInfoBuroDB(paramsDB, empresa_info_id)
      calificacion <- getScoreDB(paramsDB, empresa_info_id)
      output$tableResumen <- renderTable({
        if(calificacion != 0) {
          dat <- calculaCalificacionConcepto(reglas_calificacion, cualitativosDF, balanceDF, EdoResDF, buroDF, showInfoEmpresa(empresa_id, empresasDF)[3])
          dat$Flag <- NA
          dat$Flag[dat$score == 1] <- '<img src="img/red.png"></img>'
          dat$Flag[dat$score == 2] <- '<img src="img/yellow.png"></img>'
          dat$Flag[dat$score == 3] <- '<img src="img/green.png"></img>'
          return(dat)
        } else {
          return(NULL)
        }
        }, sanitize.text.function = function(x) x
        )
      output$tableCualit <- renderTable({
        if(dim(cualitativosDF)[1] > 0 ) {
          return(formatoTabla(cualitativosDF, catalogo_cualitativo))
        } else {
          return(NULL)
        } 
      }, xinclude.rownames=FALSE
      )
      output$tableBalance <- renderTable({
        if(dim(balanceDF)[1] > 0 ) {
          return(formatoTabla(balanceDF, catalogo_balance))  
        } else {
          return(NULL)
        }
      }, include.rownames=FALSE
      )
      output$tableEdoRes <- renderTable({
        if(dim(EdoResDF)[1] > 0 ) {
          return(formatoTabla(EdoResDF, catalogo_estado)) 
        } else {
          return(NULL)
        }
      }, include.rownames=FALSE
      )
      output$tableBuro <- renderTable({
        if(dim(buroDF)[1] > 0 ) {
          return(formatoTablaBuro(buroDF, catalogo_buro)) 
        } else {
          return(NULL)
        }
      }, include.rownames=FALSE
      )
    } 
  }
})

###################################
# Grabar en la BD info de la empresa 
###################################
observe({
  empresa_info_id <- input$empresa_info_id
  if(is.null(empresa_info_id))
    return(NULL)
  if(input$tabsCalificacion == "Variables Buró") {
    buroDF <- getInfoBuroDB(paramsDB, empresa_info_id)
    output$seleccionaAtraso <- renderUI({
      radioButtons("atrasoBuro", "Indica si la empresa tiene atrasos en el Buró", 
                   list("Sin atraso" = 0,
                        "Con Atraso" = 1
                    ),
                   selected=ifelse(dim(buroDF)[1] == 0, "Sin atraso", 
                                   ifelse(buroDF$atraso == 0, 
                                          "Sin atraso", 
                                          "Con Atraso"))
                   )
    })
    output$introduceScoreBuro <- renderUI({
      numericInput("scoreBuro", "Score Reporte Califica",
                   value=ifelse(dim(buroDF)[1] == 0, 0, buroDF$score_califica), min=0, max=1000)
    })
    output$seleccionaCompBuro_pmoral <- renderUI({
      radioButtons("compBuro_pmoral", "Comportamiento Principal Accionista en Buró Moral",
                   list("Sin Informacion" = 0,
                        "Malo" = 1,
                        "Bueno" = 3
                   ),
                   selected=ifelse(dim(buroDF)[1] == 0, "Sin Informacion", 
                                   ifelse(buroDF$buro_moral_paccionista == 0, 
                                          "Sin Informacion",
                                          ifelse(buroDF$buro_moral_paccionista==1,
                                                 "Malo",
                                                 "Bueno"
                                                 )
                                          )
                    )
            )
    })
    output$seleccionaCompBuro_pfisica <- renderUI({
      radioButtons("compBuro_pfisica", "Comportamiento Principal Accionista en Buró P. Física",
                   list("Sin Informacion" = 0,
                        "Malo" = 1,
                        "Bueno" = 3
                   ),
                   selected=ifelse(dim(buroDF)[1] == 0, "Sin Informacion", 
                                   ifelse(buroDF$buro_fisica_paccionista == 0, 
                                          "Sin Informacion",
                                          ifelse(buroDF$buro_fisica_paccionista==1,
                                                 "Malo",
                                                 "Bueno"
                                          )
                                   )
                   )
      )
    })
    output$writeBuroMsg <- renderText({NULL})
  }
  if(input$tabsCalificacion == "Cualitativos") {
    cualitativosDF <- getInfoCualitativosDB(paramsDB, empresa_info_id)
    if(dim(cualitativosDF)[1] == 0) {
      cualitativosDF <- data.frame(edad_principal_accionista=0,
                                   antiguedad_principal_accionista_domicilio=0,
                                   antiguedad_negocio=0,
                                   experiencia_principal_accionista_giro=0,
                                   estados_financieros=0,
                                   ventas_anuales=0)
    } 
    output$cual_edad_principal_accionista <- renderUI({
      numericInput("edad_principal_accionista", "Edad del Principal Accionista",
                   value=cualitativosDF$edad_principal_accionista)
      })
    output$cual_antiguedad_principal_accionista_domicilio <- renderUI({
      numericInput("antiguedad_principal_accionista_domicilio", "Antiguedad del Principal Accionista en el domicilio", 
                   value=cualitativosDF$antiguedad_principal_accionista_domicilio)
    })
    output$cual_antiguedad_negocio <- renderUI({
      numericInput("antiguedad_negocio", "Antiguedad en el negocio", 
                   value=cualitativosDF$antiguedad_negocio)
    })
    output$cual_experiencia_principal_accionista_giro <- renderUI({
      numericInput("experiencia_principal_accionista_giro", "Experiencia del Principal Accionista en el giro", 
                   value=cualitativosDF$experiencia_principal_accionista_giro)
    })
    output$cual_estados_financieros <- renderUI({
      selectInput("estados_financieros", "Estados Financieros Completos", list(No=0, Si=1),
                  selected=ifelse(cualitativosDF$estados_financieros == 0,"No", "Si"))

    })
    output$cual_ventas_anuales <- renderUI({
      numericInput("ventas_anuales", "Ventas Anuales", 
                   value=cualitativosDF$ventas_anuales)
    })
    output$writeCualitativosMsg <- renderText({NULL})
  } 
  if(input$tabsCalificacion == "Balance") {
    balanceDF <- getInfoBalanceDB(paramsDB, empresa_info_id)
    if(dim(balanceDF)[1] == 0) {
      balanceDF <- data.frame(
        act_caja_y_bancos = 0,
        act_inversiones_en_valores = 0,
        act_cuentas_por_cobrar = 0,
        act_clientes = 0,
        act_deudores_diversos_documentos_por_cobrar = 0,
        act_impuestos_por_recuperar = 0,
        act_anticipo_a_proveedores = 0,
        act_estimacion_de_cuentas_incobrables = 0,
        act_companias_afiliadas = 0,
        act_total_cuentas_por_cobrar = 0,
        act_inventarios = 0,
        act_otros_activos_circulantes = 0,
        act_total_circulante = 0,
        act_activos_diferidos = 0,
        act_documentos_por_cobrar_lgo_pzo = 0,
        act_edificios_y_terrenos = 0,
        act_maquinaria_y_equipo = 0,
        act_depreciacion = 0,
        act_total_activo_largo_plazo = 0,
        act_total__activo = 0,
        pas_porcion_circulante_de_creditos_a_lp = 0,
        pas_prestamos_bancarios_cp = 0,
        pas_proveedores = 0,
        pas_acreedores = 0,
        pas_documentos_por_pagar = 0,
        pas_impuestos_por_pagar = 0,
        pas_companias_afiliadas = 0,
        pas_total_pasivo_corto_plazo = 0,
        pas_prestamos_bancarios_lp = 0,
        pas_otros_pasivos_lp = 0,
        pas_impuestos_diferidos = 0,
        pas_total_pasivo_largo_plazo = 0,
        pas_total_pasivo = 0,
        cap_capital_social = 0,
        cap_reservas = 0,
        cap_result_acumulados = 0,
        cap_revaluacion_de_activo_fijo = 0,
        cap_aportaciones_p_futuros_aumentos_de_capital = 0,
        cap_resultado_del_ejercicio = 0,
        cap_total_capital_contable = 0,
        total_pasivo_y_capital = 0
        )
    }
    output$bal_act_caja_y_bancos  								<- renderUI({numericInput("act_caja_y_bancos", "Caja y Bancos", value=balanceDF$act_caja_y_bancos)}) 
    output$bal_act_inversiones_en_valores                           <- renderUI({numericInput("act_inversiones_en_valores", "Inversiones en Valores", value=balanceDF$act_inversiones_en_valores)})
    output$bal_act_cuentas_por_cobrar                               <- renderUI({numericInput("act_cuentas_por_cobrar", "Cuentas por Cobrar", value=balanceDF$act_cuentas_por_cobrar)})
    output$bal_act_clientes                                         <- renderUI({numericInput("act_clientes", "Clientes", value=balanceDF$act_clientes)})
    output$bal_act_deudores_diversos_documentos_por_cobrar          <- renderUI({numericInput("act_deudores_diversos_documentos_por_cobrar", "Deudores Diversos / Documentos por Cobrar", value=balanceDF$act_deudores_diversos_documentos_por_cobrar)})
    output$bal_act_impuestos_por_recuperar                          <- renderUI({numericInput("act_impuestos_por_recuperar", "Impuestos por Recuperar", value=balanceDF$act_impuestos_por_recuperar)})
    output$bal_act_anticipo_a_proveedores                           <- renderUI({numericInput("act_anticipo_a_proveedores", "Pagos anticipados", value=balanceDF$act_anticipo_a_proveedores)})
    output$bal_act_estimacion_de_cuentas_incobrables                <- renderUI({numericInput("act_estimacion_de_cuentas_incobrables", "Estimación de Cuentas Incobrables", value=balanceDF$act_estimacion_de_cuentas_incobrables)})
    output$bal_act_companias_afiliadas                              <- renderUI({numericInput("act_companias_afiliadas", "Compañías Afiliadas", value=balanceDF$act_companias_afiliadas)})
    output$bal_act_total_cuentas_por_cobrar                         <- renderUI({numericInput("act_total_cuentas_por_cobrar", "Total Cuentas por Cobrar", value=balanceDF$act_total_cuentas_por_cobrar)})
    output$bal_act_inventarios                                      <- renderUI({numericInput("act_inventarios", "Inventarios", value=balanceDF$act_inventarios)})
    output$bal_act_otros_activos_circulantes                        <- renderUI({numericInput("act_otros_activos_circulantes", "Otros Activos Circulantes", value=balanceDF$act_otros_activos_circulantes)})
    output$bal_act_total_circulante                                 <- renderUI({numericInput("act_total_circulante", "Total  Circulante", value=balanceDF$act_total_circulante)})
    output$bal_act_activos_diferidos                                <- renderUI({numericInput("act_activos_diferidos", "Activos Diferidos", value=balanceDF$act_activos_diferidos)})
    output$bal_act_documentos_por_cobrar_lgo_pzo                    <- renderUI({numericInput("act_documentos_por_cobrar_lgo_pzo", "Otros Activos de Largo Plazo", value=balanceDF$act_documentos_por_cobrar_lgo_pzo)})
    output$bal_act_edificios_y_terrenos                             <- renderUI({numericInput("act_edificios_y_terrenos", "Edificios y terrenos", value=balanceDF$act_edificios_y_terrenos)})
    output$bal_act_maquinaria_y_equipo                              <- renderUI({numericInput("act_maquinaria_y_equipo", "Maquinaria y equipo", value=balanceDF$act_maquinaria_y_equipo)})
    output$bal_act_depreciacion                                     <- renderUI({numericInput("act_depreciacion", "Depreciación", value=balanceDF$act_depreciacion)})
    output$bal_act_total_activo_largo_plazo                         <- renderUI({numericInput("act_total_activo_largo_plazo", "Total Activo Largo Plazo", value=balanceDF$act_total_activo_largo_plazo)})
    output$bal_act_total__activo                                    <- renderUI({numericInput("act_total__activo", "TOTAL  ACTIVO", value=balanceDF$act_total__activo)})
    output$bal_pas_porcion_circulante_de_creditos_a_lp              <- renderUI({numericInput("pas_porcion_circulante_de_creditos_a_lp", "Porción circulante de créditos a LP", value=balanceDF$pas_porcion_circulante_de_creditos_a_lp)})
    output$bal_pas_prestamos_bancarios_cp                           <- renderUI({numericInput("pas_prestamos_bancarios_cp", "Préstamos Bancarios", value=balanceDF$pas_prestamos_bancarios_cp)})
    output$bal_pas_proveedores                                      <- renderUI({numericInput("pas_proveedores", "Proveedores", value=balanceDF$pas_proveedores)})
    output$bal_pas_acreedores                                       <- renderUI({numericInput("pas_acreedores", "Acreedores", value=balanceDF$pas_acreedores)})
    output$bal_pas_documentos_por_pagar                             <- renderUI({numericInput("pas_documentos_por_pagar", "Documentos por Pagar", value=balanceDF$pas_documentos_por_pagar)})
    output$bal_pas_impuestos_por_pagar                              <- renderUI({numericInput("pas_impuestos_por_pagar", "Impuestos por Pagar", value=balanceDF$pas_impuestos_por_pagar)})
    output$bal_pas_companias_afiliadas                              <- renderUI({numericInput("pas_companias_afiliadas", "Compañías Afiliadas", value=balanceDF$pas_companias_afiliadas)})
    output$bal_pas_total_pasivo_corto_plazo                         <- renderUI({numericInput("pas_total_pasivo_corto_plazo", "Total Pasivo Corto Plazo", value=balanceDF$pas_total_pasivo_corto_plazo)})
    output$bal_pas_prestamos_bancarios_lp                           <- renderUI({numericInput("pas_prestamos_bancarios_lp", "Préstamos Bancarios", value=balanceDF$pas_prestamos_bancarios_lp)})
    output$bal_pas_otros_pasivos_lp                                 <- renderUI({numericInput("pas_otros_pasivos_lp", "Otros Pasivos L.P", value=balanceDF$pas_otros_pasivos_lp)})
    output$bal_pas_impuestos_diferidos                              <- renderUI({numericInput("pas_impuestos_diferidos", "Impuestos Diferidos ", value=balanceDF$pas_impuestos_diferidos)})
    output$bal_pas_total_pasivo_largo_plazo                         <- renderUI({numericInput("pas_total_pasivo_largo_plazo", "Total Pasivo Largo Plazo", value=balanceDF$pas_total_pasivo_largo_plazo)})
    output$bal_pas_total_pasivo                                     <- renderUI({numericInput("pas_total_pasivo", "TOTAL  PASIVO", value=balanceDF$pas_total_pasivo)})
    output$bal_cap_capital_social                                   <- renderUI({numericInput("cap_capital_social", "Capital Social ", value=balanceDF$cap_capital_social)})
    output$bal_cap_reservas                                         <- renderUI({numericInput("cap_reservas", "Reservas", value=balanceDF$cap_reservas)})
    output$bal_cap_result_acumulados                                <- renderUI({numericInput("cap_result_acumulados", "Resultados  Acumulados", value=balanceDF$cap_result_acumulados)})
    output$bal_cap_revaluacion_de_activo_fijo                       <- renderUI({numericInput("cap_revaluacion_de_activo_fijo", "Revaluación de Activo Fijo", value=balanceDF$cap_revaluacion_de_activo_fijo)})
    output$bal_cap_aportaciones_p_futuros_aumentos_de_capital       <- renderUI({numericInput("cap_aportaciones_p_futuros_aumentos_de_capital", "Aportaciones p/ futuros aumentos de capital", value=balanceDF$cap_aportaciones_p_futuros_aumentos_de_capital)})
    output$bal_cap_resultado_del_ejercicio                          <- renderUI({numericInput("cap_resultado_del_ejercicio", "Resultado del Ejercicio", value=balanceDF$cap_resultado_del_ejercicio)})
    output$bal_cap_total_capital_contable                           <- renderUI({numericInput("cap_total_capital_contable", "Total Capital Contable", value=balanceDF$cap_total_capital_contable)})
    output$bal_total_pasivo_y_capital                               <- renderUI({numericInput("total_pasivo_y_capital", "TOTAL  PASIVO Y CAPITAL", value=balanceDF$total_pasivo_y_capital)})
    output$writeBalanceMsg <- renderText({NULL})
  }
  if(input$tabsCalificacion == "Estado") {
    EdoResDF <- getInfoEdoResDB(paramsDB, empresa_info_id)
    if(dim(EdoResDF)[1] == 0) {
      EdoResDF <- data.frame(
        total_ventas = 0,
        devolucion_sobre_ventas = 0,
        rebajas_sobre_ventas = 0,
        total_ventas_netas = 0,
        costo_ventas = 0,
        utilidad_bruta = 0,
        gastos_operacion = 0,
        gastos_venta = 0,
        gastos_admin = 0,
        gastos_otros = 0,
        utilidad_operativa = 0,
        costo_integral_fin = 0,
        gastos_prod_fin = 0,
        perdida_cambios = 0,
        otros_productos = 0,
        otros_ingresos = 0,
        utilidad_antes_imptos_partidas_especiales = 0,
        provision_impto_activo = 0,
        impto_isr = 0,
        participacion_utilidades = 0,
        utilidad_ejercicio = 0
        )
    }
    output$edo_total_ventas <- renderUI({numericInput("total_ventas","TOTAL DE VENTAS", value=EdoResDF$total_ventas)})
    output$edo_devolucion_sobre_ventas <- renderUI({numericInput("devolucion_sobre_ventas","DEVOLUCION SOBRE VENTAS ", value=EdoResDF$devolucion_sobre_ventas)})
    output$edo_rebajas_sobre_ventas <- renderUI({numericInput("rebajas_sobre_ventas","REBAJAS SOBRE VENTAS", value=EdoResDF$rebajas_sobre_ventas)})
    output$edo_total_ventas_netas <- renderUI({numericInput("total_ventas_netas","TOTAL DE VENTAS NETAS", value=EdoResDF$total_ventas_netas)})
    output$edo_costo_ventas <- renderUI({numericInput("costo_ventas","COSTO DE VENTAS", value=EdoResDF$costo_ventas)})
    output$edo_utilidad_bruta <- renderUI({numericInput("utilidad_bruta","UTILIDAD  BRUTA", value=EdoResDF$utilidad_bruta)})
    output$edo_gastos_operacion <- renderUI({numericInput("gastos_operacion","GASTOS  DE OPERACIÓN", value=EdoResDF$gastos_operacion)})
    output$edo_gastos_venta <- renderUI({numericInput("gastos_venta","Gastos de Venta", value=EdoResDF$gastos_venta)})
    output$edo_gastos_admin <- renderUI({numericInput("gastos_admin","Gastos de Administración", value=EdoResDF$gastos_admin)})
    output$edo_gastos_otros <- renderUI({numericInput("gastos_otros","Otros Gastos de Operación", value=EdoResDF$gastos_otros)})
    output$edo_utilidad_operativa <- renderUI({numericInput("utilidad_operativa","UTILIDAD OPERATIVA", value=EdoResDF$utilidad_operativa)})
    output$edo_costo_integral_fin <- renderUI({numericInput("costo_integral_fin","Costo Integral de Financiamiento", value=EdoResDF$costo_integral_fin)})
    output$edo_gastos_prod_fin <- renderUI({numericInput("gastos_prod_fin","Gastos (Productos Financieros)})", value=EdoResDF$gastos_prod_fin)})
    output$edo_perdida_cambios <- renderUI({numericInput("perdida_cambios","Perdida (Utilidad en Cambios)})", value=EdoResDF$perdida_cambios)})
    output$edo_otros_productos <- renderUI({numericInput("otros_productos","Otros Productos (Gastos)})", value=EdoResDF$otros_productos)})
    output$edo_otros_ingresos <- renderUI({numericInput("otros_ingresos","Otros Ingresos", value=EdoResDF$otros_ingresos)})
    output$edo_utilidad_antes_imptos_partidas_especiales <- renderUI({numericInput("utilidad_antes_imptos_partidas_especiales","UTILIDAD (PERDIDA)}) ANTES DE PROVISION PARA IMPUESTOS Y PARTIDAS ESPECIALES", value=EdoResDF$utilidad_antes_imptos_partidas_especiales)})
    output$edo_provision_impto_activo <- renderUI({numericInput("provision_impto_activo","Provisión para impuesto al activo", value=EdoResDF$provision_impto_activo)})
    output$edo_impto_isr <- renderUI({numericInput("impto_isr","Impuesto sobre la renta", value=EdoResDF$impto_isr)})
    output$edo_participacion_utilidades <- renderUI({numericInput("participacion_utilidades","Participación en las utilidades", value=EdoResDF$participacion_utilidades)})
    output$edo_utilidad_ejercicio <- renderUI({numericInput("utilidad_ejercicio","UTILIDAD (PERDIDA)}) DEL EJERCICIO", value=EdoResDF$utilidad_ejercicio)})
    output$writeEstadoResMsg <- renderText({NULL})
  }
})

modificaInfo <- reactive({
  if (input$modificaInfoButton == 0) 
    return(-999)
  borraCalificacionDB(paramsDB, isolate(input$empresa_info_id))
})

# Guarda la informacion de Cualitativos en la BD      
writeCualitativos <- reactive({
  err <- -1
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
  err <- 0
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
    if(err == -1) {
      return(NULL)
    }else if(err == 0){
      return('<div style="color:green"><h4>Informacion guardada correctamente</h4></div>')
    } else{
      return(paste('<div style="color:red"><h4>', errMsg, '</h4></div>', sep=""))
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
  err <- -1
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
  err <- 0
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
    if(err == -1) {
      return(NULL)
    }else if(err == 0){
      return('<div style="color:green"><h4>Informacion guardada correctamente</h4></div>')
    } else{
      return(paste('<div style="color:red"><h4>', errMsg, '</h4></div>', sep=""))
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
  err <- -1
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
  err <- 0
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
    if(err == -1) {
      return(NULL)
    }else if(err == 0){
      return('<div style="color:green"><h4>Informacion guardada correctamente</h4></div>')
    } else{
      return(paste('<div style="color:red"><h4>', errMsg, '</h4></div>', sep=""))
    }
  })
  if(err == 0) {
    writeEstadoDB(paramsDB, logedId, valueList)          
  }
  err
})
output$writeEstado <- renderText(writeEstado())

# Guarda la informacion del buro
writeBuro <- reactive({
  err <- -1
  if (input$writeBuroButton == 0) 
    return(-999)
  #Guarda en la BD 
  # Revisar que los datos introducidos tengan el formato especificado
  valueList = isolate(list(empresa_info_id=input$empresa_info_id,
                           atraso=as.numeric(input$atrasoBuro),
                           score_califica=input$scoreBuro,
                           buro_moral_paccionista=as.numeric(input$compBuro_pmoral), 
                           buro_fisica_paccionista=as.numeric(input$compBuro_pfisica)
  ))
  ### Los errores van del más general al más particular con la finalidad
  # de detectarlos desde el principio
  err <- 0
   if(noVacios(valueList) != 0) {
     err <- 5
     errMsg <- "ERROR: No es posible introducir datos vacios"
   } else {
     if(valueList$score_califica < 0 || valueList$score_califica > 1000) {
       err <- 10
       errMsg <- "ERROR: El score califica fuera de rango"
     }
   }

  output$writeBuroMsg <- renderText({
    if(err == -1) {
      return(NULL)
    }else if(err == 0){
      return('<div style="color:green"><h4>Informacion guardada correctamente</h4></div>')
    } else{
      return(paste('<div style="color:red"><h4>', errMsg, '</h4></div>', sep=""))
    }
  })
  if(err == 0) {
    writeBuroDB(paramsDB, logedId, valueList)
  }
  err
})
output$writeBuro <- renderText(writeBuro())

observe({
  empresa_info_id <- input$empresa_info_id
  if(is.null(empresa_info_id))
    return(NULL)
  if(input$tabsGenerales == "Términos y condiciones") {
    print("Hell yeah!")
#    termsDF <- getInfoBuroDB(paramsDB, empresa_info_id)
termsDF <- data.frame(
  monto_solicitado=numeric(0),  
  monto_autorizado=numeric(0),	
  destino_credito=numeric(0),		
  ministracion=numeric(0),		
  forma_pago=numeric(0),			
  garantia=numeric(0),			
  vigencia_linea=numeric(0),		
  vigencia_contrato=numeric(0),	
  plazo_disposiciones=numeric(0),	
  tasa_ordinaria=numeric(0),		
  tasa_moratoria=numeric(0),		
  comision_apertura=numeric(0),	
  fuente_fondeo=numeric(0),		
  moneda=numeric(0),				
  costo_garantia=numeric(0),	
  nombre_aval=character(0))			
	

    output$termsMontoSolicitado <- renderUI({
      numericInput("termsMontoSolicitado", "Monto Solicitado:",
        value=ifelse(dim(termsDF)[1] == 0, "", termsDF$monto_solicitado),
        min=0, max=300000000
      )
    })
    output$termsMontoAutorizado <- renderUI({
      numericInput("termsMontoAutorizado", "Monto Autorizado:",
        value=ifelse(dim(termsDF)[1] == 0, "", termsDF$monto_autorizado),
        min=0, max=30000000
      )
    })
    output$selecDestino <- renderUI({
      selectInput("selecDestino", "Destino:", list("Capital de Trabajo"="capital_trabajo"),
                  selected=ifelse(dim(termsDF)[1] == 0, "", termsDF$destino_credito)
                  )
    })
    output$selecMinistracion <- renderUI({
      selectInput("selecMinistracion", "Ministración:", list("Única"="unica"),
                  selected=ifelse(dim(termsDF)[1] == 0, "", termsDF$tipo_ministracion)
                  )
    })
    output$selecFormaPago <- renderUI({
      selectInput("selecFormaPago", "Forma de Pago:", list("Capital e intereses mensual"= "cap_int_mensual"),
                  selected=ifelse(dim(termsDF)[1] == 0, "", termsDF$forma_pago)
                  )
    })
    output$selecGarantia <- renderUI({
      selectInput("selecGarantia", "Garantía:", list("Sin Garantía"="NA", "Hipotecaria"= "hipotecaria"),
                  selected=ifelse(dim(termsDF)[1] == 0, "", termsDF$garantia)
                  )
    })
    output$vigenciaLinea <- renderUI({
      numericInput("vigenciaLinea", "Vigencia de la linea (en meses):",
               value=ifelse(dim(termsDF)[1] == 0, "", termsDF$vigencia_linea),
               min=0, max=60
      )
    })
    output$vigenciaContrato <- renderUI({
      numericInput("vigenciaContrato", "Vigencia del contrato (en meses):",
               value=ifelse(dim(termsDF)[1] == 0, "", termsDF$vigencia_contrato),
               min=0, max=60
      )
    })
    output$plazoDisposiciones <- renderUI({
      selectInput("plazoDisposiciones", "Plazo de las disposiciones:", 
                  list("Única"="unica", "Mensual"= "mensual"), 
                  selected=ifelse(dim(termsDF)[1] == 0, "", termsDF$plazo_disposiciones)
                    )
    })
    output$tasaOrdinaria <- renderUI({
      selectInput("tasaOrdinaria", "Tasa de interés ordinaria:", 
              list("3.6"=.036, "12"=.12, "15"=.15), 
              selected=ifelse(dim(termsDF)[1] == 0, "", termsDF$tasa_ordinaria)
              )
    })
    output$tasaMoratoria <- renderUI({
      selectInput("tasaMoratoria", "Tasa de interés moratoria:", 
              list("*3"="*3"),
              selected=ifelse(dim(termsDF)[1] == 0, "", termsDF$tasa_moratoria)
              )
    })
    output$comisionApertura <- renderUI({
      selectInput("comisionApertura", "Comisión por apertura:", 
              list("1%"=.01, "2%"=.02),
              selected=ifelse(dim(termsDF)[1] == 0, "", termsDF$comision_apertura)
              )
    })
    output$fuenteFondeo <- renderUI({
      selectInput("fuenteFondeo", "Fuente de fondeo:", 
              list("Recursos propios"="propios", "NAFIN"="nafin"),
              selected=ifelse(dim(termsDF)[1] == 0, "", termsDF$fuente_fondeo)
      )
    })
    output$monedaCredito <- renderUI({
      selectInput("monedaCredito", "Moneda:", 
              list("MXN"="MXN"),
              selected=ifelse(dim(termsDF)[1] == 0, "", termsDF$moneda)
      )
    })
    output$costoGarantia <- renderUI({
      numericInput("costoGarantia", "Costo de la garantía:",
               value=ifelse(dim(termsDF)[1] == 0, "", termsDF$costo_garantia),
               min=0, max=100
      )
    })   
    output$nombreAval <- renderUI({
      textInput("nombreAval", "Nombre del aval:",
               value=ifelse(dim(termsDF)[1] == 0, "", termsDF$nombre_aval)
      )
    }) 

  }
})
    }    
  })
})

