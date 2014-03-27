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
      div(style="width: 15%; float:left", strong("Nombre:"), br(), strong("RFC:")),
      div(style="width: 85%; float:right", textOutput("nombreEmpresa"), textOutput("rfcEmpresa"))    
      ),
    wellPanel(
      div(style="width: 15%; float:left", strong("Calificación:")),
    conditionalPanel(
      condition = "output.status == 'Completo'",
      conditionalPanel(
        condition = "output.calificacion == '0'",
          div(style="width: 85%; float:right", "Datos completos"),
          actionButton("calculaScoreButton", "Oprime para calificar")
      ),
      conditionalPanel(
        condition = "output.calificacion == '1'",
          div(style="width: 85%; float:right; color:red", "EMPRESA RECHAZADA PARA PRODUCTO PYMES")
      ),
      conditionalPanel(
        condition = "output.calificacion == '2'",
          div(style="width: 85%; float:right; color:#FFCC00", "EMPRESA PARCIALMENTE APROBADA PARA PRODUCTO PYMES")
      ),
      conditionalPanel(
        condition = "output.calificacion == '3'",
          div(style="width: 85%; float:right; color:green", "EMPRESA APROBADA PARA PRODUCTO PYMES")
      )
    ,
    conditionalPanel(
      condition = "output.status == 'Incompleto'",
        div(style="width: 85%; float:right; color:red", "INFORMACIÓN INCOMPLETA")
    )
    )),
    br(),
    tabsetPanel(
      tabPanel("Estado",
               tabsetPanel(
                 tabPanel("Resumen",
                          conditionalPanel(
                            condition = "output.calificacion == 0",
                            div(style="color:red", "Empresa no calificada")
                          ),
                          conditionalPanel(
                            condition = "output.calificacion != 0",
                            tableOutput("tableResumen")
                          )
                 ),
                 tabPanel("Cualitativos",
                          conditionalPanel(
                            condition = "output.Cualit=='No Capturado'",
                            div("Informacion no disponible", style = "color:red")          
                          ),
                          conditionalPanel(
                            condition = "output.Cualit=='Capturado'",
                            tableOutput("tableCualit")
                          )
                  ),
                 tabPanel("Balance",
                          conditionalPanel(
                            condition = "output.Balance=='No Capturado'",
                            div("Informacion no disponible", style = "color:red")          
                          ),
                          conditionalPanel(
                            condition = "output.Balance=='Capturado'",
                            tableOutput("tableBalance")
                          )
                  ),
                 tabPanel("Estado",
                          conditionalPanel(
                            condition = "output.Estado=='No Capturado'",
                            div("Informacion no disponible", style = "color:red")          
                          ),
                          conditionalPanel(
                            condition = "output.Estado=='Capturado'",
                            tableOutput("tableEdoRes")
                          )
                 )
               )
      ),
      tabPanel("Calificador",
               #textOutput("status")
               tabsetPanel(
                 tabPanel("Cualitativos",                   
                            htmlOutput("writeCualitativosMsg"),
                            wellPanel(
                              #div(style="width: 20%; float:left")
                              numericInput("edad_principal_accionista", "Edad del Principal Accionista", value=0),
                              numericInput("antiguedad_principal_accionista_domicilio", "Antiguedad del Principal Accionista en el domicilio", value=0),
                              numericInput("antiguedad_negocio", "Antiguedad en el negocio", value=0),
                              numericInput("experiencia_principal_accionista_giro", "Experiencia del Principal Accionista en el giro", value=0),
                              numericInput("estados_financieros", "Estados Financieros", value=0),
                              numericInput("ventas_anuales", "Ventas Anuales", value=0)
                            ),
                            actionButton("writeCualitativosButton", "Grabar"),
                            br()
                 ),
                 tabPanel("Balance",
                            htmlOutput("writeBalanceMsg"),
                            wellPanel(
                              div(class="span5",h4("Activo"),
                                  numericInput("act_caja_y_bancos" , "Caja y Bancos", value=0),
                                  numericInput("act_inversiones_en_valores" , "Inversiones en Valores", value=0),
                                  numericInput("act_cuentas_por_cobrar" , "Cuentas por Cobrar", value=0),
                                  numericInput("act_clientes" , "Clientes", value=0),
                                  numericInput("act_deudores_diversos_documentos_por_cobrar" , "Deudores Diversos / Documentos por Cobrar", value=0),
                                  numericInput("act_impuestos_por_recuperar" , "Impuestos por Recuperar", value=0),
                                  numericInput("act_anticipo_a_proveedores" , "Pagos anticipados", value=0),
                                  numericInput("act_estimacion_de_cuentas_incobrables" , "Estimaciòn de Cuentas Incobrables", value=0),
                                  numericInput("act_companias_afiliadas" , "Compañias Afiliadas", value=0),
                                  numericInput("act_total_cuentas_por_cobrar" , "Total Cuentas por Cobrar", value=0),
                                  numericInput("act_inventarios" , "Inventarios", value=0),
                                  numericInput("act_otros_activos_circulantes" , "Otros Activos Circulantes", value=0),
                                  numericInput("act_total_circulante" , "Total  Circulante", value=0),
                                  numericInput("act_activos_diferidos" , "Activos Diferidos ", value=0),
                                  numericInput("act_documentos_por_cobrar_lgo_pzo" , "Otros Activos de Largo Plazo", value=0),
                                  numericInput("act_edificios_y_terrenos" , "Edificios y terrenos", value=0),
                                  numericInput("act_maquinaria_y_equipo" , "Maquinaria y equipo", value=0),
                                  numericInput("act_depreciacion" , "Depreciacion", value=0),
                                  numericInput("act_total_activo_largo_plazo" , "Total Activo Largo Plazo", value=0),
                                  numericInput("act_total__activo" , "TOTAL  ACTIVO", value=0),
                                  #basura para que no quede con formato feo 
                                  div(print("writeEstado"), style = "opacity:0"),div(print("writeEstado"), style = "opacity:0"),
                                  div(print("writeEstado"), style = "opacity:0"),
                                  div(print("writeEstado"), style = "opacity:0"),div(print("writeEstado"), style = "opacity:0"),
                                  div(print("writeEstado"), style = "opacity:0")
                              ),
                              HTML('<h4>Pasivo</h1>'),
                              numericInput("pas_porcion_circulante_de_creditos_a_lp" , "Porción circulante de créditos a LP", value=0),
                              numericInput("pas_prestamos_bancarios_cp" , "Préstamos Bancarios", value=0),
                              numericInput("pas_proveedores" , "Proveedores", value=0),
                              numericInput("pas_acreedores" , "Acreedores", value=0),
                              numericInput("pas_documentos_por_pagar" , "Documentos por Pagar", value=0),
                              numericInput("pas_impuestos_por_pagar" , "Impuestos por Pagar", value=0),
                              numericInput("pas_companias_afiliadas" , "Compañias Afiliadas", value=0),
                              numericInput("pas_total_pasivo_corto_plazo" , " Total Pasivo Corto Plazo ", value=0),
                              numericInput("pas_prestamos_bancarios_lp" , " Préstamos Bancarios ", value=0),
                              numericInput("pas_otros_pasivos_lp" , " Otros Pasivos L.P ", value=0),
                              numericInput("pas_impuestos_diferidos" , " Impuestos Diferidos ", value=0),
                              numericInput("pas_total_pasivo_largo_plazo" , " Total Pasivo Largo Plazo ", value=0),
                              numericInput("pas_total_pasivo" , " TOTAL  PASIVO ", value=0),
                              h4("Capital"),
                              numericInput("cap_capital_social" , " Capital Social ", value=0),
                              numericInput("cap_reservas" , " Reservas ", value=0),
                              numericInput("cap_result_acumulados" , " Resultados Acumulados ", value=0),
                              numericInput("cap_revaluacion_de_activo_fijo" , "Revaluacion de Activo Fijo ", value=0),
                              numericInput("cap_aportaciones_p_futuros_aumentos_de_capital" , " Aportaciones p/ futuros aumentos de capital ", value=0),
                              numericInput("cap_resultado_del_ejercicio" , " Resultado del Ejercicio ", value=0),
                              numericInput("cap_total_capital_contable" , " Total Capital Contable ", value=0),
                              numericInput("total_pasivo_y_capital" , " TOTAL  PASIVO Y CAPITAL", value=0)
                            ),
                            actionButton("writeBalanceButton", "Grabar"),
                            br()         
                 ),
                 tabPanel("Estado",
                              htmlOutput("writeEstadoResMsg"),
                          wellPanel(
                              div(class="span5",
                                  numericInput("total_ventas" , "TOTAL DE VENTAS ", value=0),
                                  numericInput("devolucion_sobre_ventas" , "DEVOLUCION SOBRE VENTAS ", value=0),
                                  numericInput("rebajas_sobre_ventas" , "REBAJAS SOBRE VENTAS ", value=0),
                                  numericInput("total_ventas_netas" , "TOTAL DE VENTAS NETAS ", value=0),
                                  numericInput("costo_ventas" , "COSTO DE VENTAS ", value=0),
                                  numericInput("utilidad_bruta" , "UTILIDAD  BRUTA ", value=0),
                                  numericInput("gastos_operacion" , "GASTOS  DE OPERACION ", value=0),
                                  numericInput("gastos_venta" , "Gastos de Venta ", value=0),
                                  numericInput("gastos_admin" , "Gastos de Administracion ", value=0),
                                  numericInput("gastos_otros" , "Otros Gastos de Operación ", value=0),
                                  numericInput("utilidad_operativa" , "UTILIDAD OPERATIVA ", value=0)),
                              numericInput("costo_integral_fin" , "Costo Integral de Financiamiento ", value=0),
                              numericInput("gastos_prod_fin" , "Gastos (Productos Financieros) ", value=0),
                              numericInput("perdida_cambios" , "Perdida (Utilidad en Cambios) ", value=0),
                              numericInput("otros_productos" , "Otros Productos (Gastos) ", value=0),
                              numericInput("otros_ingresos" , "Otros Ingresos ", value=0),
                              numericInput("utilidad_antes_imptos_partidas_especiales" , "UTILIDAD / (PERDIDA) ANTES DE PROVISION PARA IMPUESTOS Y PARTIDAS ESPECIALES", value=0),
                              numericInput("provision_impto_activo" , "Provisión para impuesto al activo ", value=0),
                              numericInput("impto_isr" , "Impuesto sobre la renta ", value=0),
                              numericInput("participacion_utilidades" , "Participación en las utilidades ", value=0),
                              numericInput("utilidad_ejercicio" , "UTILIDAD / (PERDIDA) DEL EJERCICIO ", value=0),
                              #basura para que no quede con formato feo 
                              div(print("writeEstado"), style = "opacity:0"),div(print("writeEstado"), style = "opacity:0")
                            ),
                            actionButton("writeEstadoButton", "Grabar"),
                            br()
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
