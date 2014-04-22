library(shiny)
narrowSidebar <- HTML('<style>.span4 {max-width: 300px}</style>')

# Define UI for miles per gallon application
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel(
    list("PYME Express", HTML('<img src="img/LOGO-COMPLETO-RGB.jpg", height = 350, width = 350" style="float:left"/>','<h3> Sistema Calificación </h3>' ))
  ),
  
#  HTML('<img src="img/red.png"></img>'),
  # Sidebar with controls to select the variable to plot against mpg
  # and to specify whether outliers should be included
  sidebarPanel(
    tags$head(narrowSidebar),
    conditionalPanel(
      condition = "output.loginStatus != '0'",
      wellPanel(
        h4("Ingresa a tu cuenta"),
        uiOutput("seleccionaUsuario"),
        textInput("password", "Password:"),
        br(),
        actionButton("loginButton", "Login"),
        div(textOutput("loginStatus"), style = "opacity:0"),
        div(textOutput("loginStatusMsg"), style = "color:red")
      ),
      wellPanel(
        h4("Cambio de contraseña"),
        textInput("passwordReg", "Password anterior:"),
        textInput("newPasswordReg1", "Nuevo password:"),
        textInput("newPasswordReg2", "Repite nuevo password:"),
        br(),
        actionButton("newPasswordButton", "Cambiar"),
        div(textOutput("cambioPassStatus"), style = "opacity:0"),
        div(textOutput("cambioPassStatusMsg"), style = "color:red")
      )
    ), 
    conditionalPanel(
      condition="output.loginStatus == '0'",
      wellPanel(
        uiOutput("seleccionaEmpresa"),
        uiOutput("seleccionaFecha")      
      ),
      # Panel para introducir nueva empresa
      conditionalPanel(
        condition = "input.empresa_id == '-998'",
        wellPanel(
          textInput("capturaEmpNombre", "Nombre:"),
          textInput("capturaEmpRFC", "RFC:"),
          radioButtons("tipoPersona", "Tipo de persona",
                       list("Persona Moral" = "P. Moral",
                            "Persona Fisica con Actividad Empresarial" = "P. Fisica con Actividad Empresarial"
                       ),
                       selected="Persona Moral"
          ),
          actionButton("writeEmpresaButton", "Guardar Empresa"),
          div(textOutput("writeEmpresaStatusMsg"), style = "color:red")
        )
      ),
      # Panel para introducir nueva fecha
      conditionalPanel(
        condition = "input.empresa_id != '-999'",
        conditionalPanel(
          condition = "input.empresa_id != '-998'",
          conditionalPanel(
            condition = "input.empresa_info_id == '-998'",
            wellPanel(
              dateInput("capturaFecha", "Introduce fecha", value = NULL, min = "2010-01-01",
                        max = Sys.Date(), format = "yyyy-mm-dd", startview = "month",
                        weekstart = 0, language = "es"),
              #textInput("capturaFecha", "Fecha:"),
              actionButton("writeFechaButton", "Guardar Fecha"),
              div(textOutput("writeFechaStatusMsg"), style = "color:red")
            )
          )
        )
      )
      
    ) # Cierra el panel de login
  ),
  ################
  mainPanel(
    wellPanel(
      h4(style="width: 25%; float:left", "Nombre:", br(), "RFC:", br(), "Tipo Empresa:"),
      h4(style="width: 75%; float:right", textOutput("nombreEmpresa"), textOutput("rfcEmpresa"), textOutput("tipoPersona")),
      br(),
      br(),
      br()
      ),
    wellPanel(
      h4(style="width: 15%; float:left", "Calificacion:"),
    conditionalPanel(
      condition = "output.status == 'Completo'",
      htmlOutput("calificacionMsg"),
    conditionalPanel(
      condition = "output.status == 'Incompleto'",
        div(style="width: 85%; float:right; color:red", "INFORMACIÓN INCOMPLETA")
    )
    )),
    br(),
    conditionalPanel(
      condition = "output.status != '-999' && output.loginStatusMsg == 'Login exitoso'",
      tabsetPanel(id="tabsGenerales",
        tabPanel("Resultado",
          conditionalPanel(
            condition = "output.calificacion == 0",
            h4(style="color:red", "Empresa no calificada")
          ),
          conditionalPanel(
            condition = "output.calificacion != 0",
            tableOutput("tableResumen")
          )
        ),
      tabPanel("Calificador",
               conditionalPanel(
                 condition = "output.calificacion == 0",
                conditionalPanel(
                    condition = "output.status == 'Completo'",
                  wellPanel(
                    actionButton("calculaScoreButton", "Oprime para calificar")
                  )
                )
                ),
                 wellPanel(
                   actionButton("modificaInfoButton", "Modificar Información")
                 ),
#                 ),
                 #textOutput("status")
                 tabsetPanel(id="tabsCalificacion",
                             tabPanel("Cualitativos",                   
                                      htmlOutput("writeCualitativosMsg"),
                                      conditionalPanel(
                                        condition = "output.Cualit != 'Capturado'",
                                        wellPanel(
                                          actionButton("writeCualitativosButton", "Grabar")
                                        ),
                                        wellPanel(
                                          #div(style="width: 20%; float:left")
                                          uiOutput("cual_edad_principal_accionista"),
                                          uiOutput("cual_antiguedad_principal_accionista_domicilio"),
                                          uiOutput("cual_antiguedad_negocio"),
                                          uiOutput("cual_experiencia_principal_accionista_giro"),
                                          uiOutput("cual_estados_financieros"),
                                          uiOutput("cual_ventas_anuales")
                                        )
                                      ),
                                      conditionalPanel(
                                        condition = "output.Cualit != 'No Capturado'",
                                        wellPanel(
                                          tableOutput("tableCualit")
                                        )
                                      )
                             ),
                             tabPanel("Balance",
                                      htmlOutput("writeBalanceMsg"),
                                      conditionalPanel(
                                        condition = "output.Balance != 'Capturado'",
                                        wellPanel(
                                          actionButton("writeBalanceButton", "Grabar")
                                        ),
                                        wellPanel(
                                          div(class="span5",h4("Activo"),
                                              uiOutput("bal_act_caja_y_bancos"),
                                              uiOutput("bal_act_inversiones_en_valores"),
                                              uiOutput("bal_act_cuentas_por_cobrar"),
                                              uiOutput("bal_act_clientes"),
                                              uiOutput("bal_act_deudores_diversos_documentos_por_cobrar"),
                                              uiOutput("bal_act_impuestos_por_recuperar"),
                                              uiOutput("bal_act_anticipo_a_proveedores"),
                                              uiOutput("bal_act_estimacion_de_cuentas_incobrables"),
                                              uiOutput("bal_act_companias_afiliadas"),
                                              uiOutput("bal_act_total_cuentas_por_cobrar"),
                                              uiOutput("bal_act_inventarios"),
                                              uiOutput("bal_act_otros_activos_circulantes"),
                                              uiOutput("bal_act_total_circulante"),
                                              uiOutput("bal_act_activos_diferidos"),
                                              uiOutput("bal_act_documentos_por_cobrar_lgo_pzo"),
                                              uiOutput("bal_act_edificios_y_terrenos"),
                                              uiOutput("bal_act_maquinaria_y_equipo"),
                                              uiOutput("bal_act_depreciacion"),
                                              uiOutput("bal_act_total_activo_largo_plazo"),
                                              uiOutput("bal_act_total__activo"),
                                              #basura para que no quede con formato feo 
                                              div("writeEstado", style = "opacity:0"),div("writeEstado", style = "opacity:0"),
                                              div("writeEstado", style = "opacity:0"),
                                              div("writeEstado", style = "opacity:0"),div("writeEstado", style = "opacity:0"),
                                              div("writeEstado", style = "opacity:0")
                                          ),
                                          HTML('<h4>Pasivo</h1>'),
                                          uiOutput("bal_pas_porcion_circulante_de_creditos_a_lp"),
                                          uiOutput("bal_pas_prestamos_bancarios_cp"),
                                          uiOutput("bal_pas_proveedores"),
                                          uiOutput("bal_pas_acreedores"),
                                          uiOutput("bal_pas_documentos_por_pagar"),
                                          uiOutput("bal_pas_impuestos_por_pagar"),
                                          uiOutput("bal_pas_companias_afiliadas"),
                                          uiOutput("bal_pas_total_pasivo_corto_plazo"),
                                          uiOutput("bal_pas_prestamos_bancarios_lp"),
                                          uiOutput("bal_pas_otros_pasivos_lp"),
                                          uiOutput("bal_pas_impuestos_diferidos"),
                                          uiOutput("bal_pas_total_pasivo_largo_plazo"),
                                          uiOutput("bal_pas_total_pasivo"),
                                          h4("Capital"),
                                          uiOutput("bal_cap_capital_social"),
                                          uiOutput("bal_cap_reservas"),
                                          uiOutput("bal_cap_result_acumulados"),
                                          uiOutput("bal_cap_revaluacion_de_activo_fijo"),
                                          uiOutput("bal_cap_aportaciones_p_futuros_aumentos_de_capital"),
                                          uiOutput("bal_cap_resultado_del_ejercicio"),
                                          uiOutput("bal_cap_total_capital_contable"),
                                          uiOutput("bal_total_pasivo_y_capital")
                                        )
                                      ),
                                      conditionalPanel(
                                        condition = "output.Balance != 'No Capturado'",
                                        wellPanel(
                                          tableOutput("tableBalance")
                                        )
                                      )
                             ),
                             tabPanel("Estado",
                                      htmlOutput("writeEstadoResMsg"),
                                      conditionalPanel(
                                        condition = "output.Estado != 'Capturado'",
                                        wellPanel(
                                          actionButton("writeEstadoButton", "Grabar")
                                        ),
                                        wellPanel(
                                          div(class="span5",
                                              uiOutput("edo_total_ventas"),
                                              uiOutput("edo_devolucion_sobre_ventas"),
                                              uiOutput("edo_rebajas_sobre_ventas"),
                                              uiOutput("edo_total_ventas_netas"),
                                              uiOutput("edo_costo_ventas"),
                                              uiOutput("edo_utilidad_bruta"),
                                              uiOutput("edo_gastos_operacion"),
                                              uiOutput("edo_gastos_venta"),
                                              uiOutput("edo_gastos_admin"),
                                              uiOutput("edo_gastos_otros"),
                                              uiOutput("edo_utilidad_operativa")
                                          ),
                                          uiOutput("edo_costo_integral_fin"),
                                          uiOutput("edo_gastos_prod_fin"),
                                          uiOutput("edo_perdida_cambios"),
                                          uiOutput("edo_otros_productos"),
                                          uiOutput("edo_otros_ingresos"),
                                          uiOutput("edo_utilidad_antes_imptos_partidas_especiales"),
                                          uiOutput("edo_provision_impto_activo"),
                                          uiOutput("edo_impto_isr"),
                                          uiOutput("edo_participacion_utilidades"),
                                          uiOutput("edo_utilidad_ejercicio"),
                                          #basura para que no quede con formato feo 
                                          div("writeEstado", style = "opacity:0"),div("writeEstado", style = "opacity:0")
                                        )
                                      ),
                                      conditionalPanel(
                                        condition = "output.Estado != 'No Capturado'",
                                        wellPanel(
                                          tableOutput("tableEdoRes")
                                        )
                                      )
                             ),
                             tabPanel("Variables Buró",
                                      htmlOutput("writeBuroMsg"),
                                      conditionalPanel(
                                        condition = "output.Buro != 'Capturado'",
                                        wellPanel(
                                          actionButton("writeBuroButton", "Grabar")  
                                        ),
                                        wellPanel(
                                          uiOutput("seleccionaAtraso"),
                                          uiOutput("introduceScoreBuro"),
                                          uiOutput("seleccionaCompBuro_pfisica"),
                                          conditionalPanel(
                                            condition = "output.tipoPersona == 'P. Moral'",
                                            uiOutput("seleccionaCompBuro_pmoral")
                                          )
                                        )
                                      ),
                                      conditionalPanel(
                                        condition = "output.Buro != 'No Capturado'",
                                        wellPanel(
                                          tableOutput("tableBuro")
                                        )
                                      )  
                             )               
                )
      ),
      tabPanel("Términos y condiciones",
               conditionalPanel(
                 condition = "output.calificacion != 0",
                 downloadButton("downloadDictamenPDF", "Download Dictamen")
               ),
               wellPanel(
                 h5("Características del crédito"),
                 uiOutput("termsMontoSolicitado"),
                 uiOutput("termsMontoAutorizado"),
                 uiOutput("selecDestino"),
                 uiOutput("selecMinistracion"),
                 uiOutput("selecFormaPago"),
                 uiOutput("vigenciaLinea"),
                 uiOutput("vigenciaContrato"),
                 uiOutput("plazoDisposiciones"),
                 uiOutput("tasaOrdinaria"),
                 uiOutput("tasaMoratoria"),
                 uiOutput("comisionApertura"),
                 uiOutput("fuenteFondeo"),
                 uiOutput("monedaCredito"),
                 h5("Garantías y avales"),
                 uiOutput("selecGarantia"),
                 uiOutput("costoGarantia"),
                 uiOutput("nombreAval")
               )
      )
      )
    ),
    div(textOutput("writeEmpresa"), style = "opacity:1"),
    div(textOutput("writeFecha"),style = "opacity:1"),
    div(textOutput("writeCualitativos"), style = "opacity:1"),
    div(textOutput("writeEstado"), style = "opacity:1"),
    div(textOutput("writeBalance"), style = "opacity:1"),
    div(textOutput("writeBuro"), style = "opacity:1"),
    div(textOutput("status"), style = "opacity:1"),
    div(textOutput("Balance"), style = "opacity:1"),
    div(textOutput("Estado"), style = "opacity:1"),
    div(textOutput("Cualit"), style = "opacity:1"),
    div(textOutput("Buro"), style = "opacity:1"),
    div(textOutput("calificacion"), style = "opacity:1")
  )  
))
