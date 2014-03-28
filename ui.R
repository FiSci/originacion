library(shiny)
narrowSidebar <- HTML('<style>.span4 {max-width: 300px}</style>')

# Define UI for miles per gallon application
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Precalificación de crédito"),
  
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
      h4(style="width: 15%; float:left", "Nombre:", br(), "RFC:"),
      h4(style="width: 85%; float:right", textOutput("nombreEmpresa"), textOutput("rfcEmpresa")),
      br(),
      br()
      ),
    wellPanel(
      h4(style="width: 15%; float:left", "Calificacion:"),
    conditionalPanel(
      condition = "output.status == 'Completo'",
      htmlOutput("calificacionMsg"),
      conditionalPanel(
        condition = "output.calificacion == '0'",
          actionButton("calculaScoreButton", "Oprime para calificar")
      ),
    conditionalPanel(
      condition = "output.status == 'Incompleto'",
        div(style="width: 85%; float:right; color:red", "INFORMACIÓN INCOMPLETA")
    )
    )),
    br(),
    conditionalPanel(
      condition = "output.status != '-999' && output.loginStatusMsg == 'Login exitoso'",
      tabsetPanel(
        tabPanel("Estado",
               tabsetPanel(
                 tabPanel("Resumen",
                          conditionalPanel(
                            condition = "output.calificacion == 0",
                            h4(style="color:red", "Empresa no calificada")
                          ),
                          tableOutput("tableResumen")
                 ),
                 tabPanel("Cualitativos",
                          conditionalPanel(
                            condition = "output.Cualit=='No Capturado'",
                            h4("Informacion no disponible", style = "color:red")          
                          ),
                          tableOutput("tableCualit")
                  ),
                 tabPanel("Balance",
                          conditionalPanel(
                            condition = "output.Balance=='No Capturado'",
                            h4("Informacion no disponible", style = "color:red")          
                          ),
                          tableOutput("tableBalance")
                  ),
                 tabPanel("Estado",
                          conditionalPanel(
                            condition = "output.Estado=='No Capturado'",
                            h4("Informacion no disponible", style = "color:red")          
                          ),
                          tableOutput("tableEdoRes")
                 ),
                 tabPanel("Variables Buró",
                          conditionalPanel(
                            condition = "output.Buro=='No Capturado'",
                            h4("Informacion no disponible", style = "color:red")          
                          )
#                          ,tableOutput("tableEdoRes")
                 )
                 
               )
      ),
      tabPanel("Calificador",
               #textOutput("status")
               tabsetPanel(id="tabsCalificacion",
                 tabPanel("Cualitativos",                   
                            htmlOutput("writeCualitativosMsg"),
                            wellPanel(
                              #div(style="width: 20%; float:left")
                              uiOutput("cual_edad_principal_accionista"),
                              uiOutput("cual_antiguedad_principal_accionista_domicilio"),
                              uiOutput("cual_antiguedad_negocio"),
                              uiOutput("cual_experiencia_principal_accionista_giro"),
                              uiOutput("cual_estados_financieros"),
                              uiOutput("cual_ventas_anuales")
                            ),
                            actionButton("writeCualitativosButton", "Grabar"),
                            br()
                 ),
                 tabPanel("Balance",
                            htmlOutput("writeBalanceMsg"),
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
                            ),
                            actionButton("writeBalanceButton", "Grabar"),
                            br()         
                 ),
                 tabPanel("Estado",
                              htmlOutput("writeEstadoResMsg"),
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
                            ),
                            actionButton("writeEstadoButton", "Grabar"),
                            br()
                 ),
                 tabPanel("Variables Buró",
                          htmlOutput("writeBuroMsg"),
                          wellPanel(
                            uiOutput("seleccionaTipoPersona"),
                            uiOutput("introduceScoreBuro"),
                            uiOutput("seleccionaCompBuro_pmoral"),
                            uiOutput("seleccionaCompBuro_pfisica")
                          ),
                          actionButton("writeBuroButton", "Grabar"),
                          br()
                 )
               )
      )         
      )
    ),
    div(textOutput("writeEmpresa"), style = "opacity:1"),
    div(textOutput("writeFecha"),style = "opacity:1"),
    div(textOutput("writeCualitativos"), style = "opacity:1"),
    div(textOutput("writeEstado"), style = "opacity:1"),
    div(textOutput("writeBalance"), style = "opacity:1"),
    div(textOutput("status"), style = "opacity:1"),
    div(textOutput("Balance"), style = "opacity:1"),
    div(textOutput("Estado"), style = "opacity:1"),
    div(textOutput("Cualit"), style = "opacity:1"),
    div(textOutput("calificacion"), style = "opacity:1")
  )  
))
