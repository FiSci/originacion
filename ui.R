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
          textInput("capturaEmpRS", "Razon Social:"),
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
    div(class="span5",
    div(textOutput("writeEmpresa"), style = "opacity:0"),
    div(textOutput("writeFecha"),style = "opacity:0"),
    div(textOutput("writeCualitativos"), style = "opacity:0")),
    div(textOutput("writeEstado"), style = "opacity:0"),
    div(textOutput("writeBalance"), style = "opacity:0"),
       wellPanel(
         h4("Informacion de la Empresa", align = "center"),
         br(),         
         div(class="span5",strong(print("Nombre:")),textOutput("nombreEmpresa"),
         strong(print("RFC:")), textOutput("rfcEmpresa"),
             strong(print("Razon Social:")), textOutput("rsEmpresa"),
         #El siguiente renglon es solo para ocupar el espacio y que no quede mas acomodada la info del cuadro
         div(print("writeEstado"), style = "opacity:0"),div(print("writeEstado"), style = "opacity:0")),
         strong(print("Status")), textOutput("Status"),
         strong(print("Calificacion")), textOutput("score"),
         strong(print("Balance General")), textOutput("Balance"),
         strong(print("Estado de Resultados")), textOutput("Estado"),
         strong(print("Informacion Cualitativa")), textOutput("Cualit")
       ),
       
       h4("Estados Financieros", align = "center"),
       
       tabsetPanel( 
         tabPanel("Cualitativos",
                  conditionalPanel(
                    condition = "output.Cualit=='Capturado'",
                    wellPanel(
                      tableOutput("tableCualit")
                    )                  
                  ),                    
                  conditionalPanel(
                    condition = "output.Cualit=='No Capturado'",
                    wellPanel(
                      numericInput("edad_principal_accionista", "Edad del Principal Accionista",0,25,99),
                      numericInput("antiguedad_principal_accionista_domicilio", "Antiguedad del Principal Accionista en el domicilio",0,0,99),
                      numericInput("antiguedad_negocio", "Antiguedad en el negocio",0,0,99),
                      numericInput("experiencia_principal_accionista_giro", "Experiencia del Principal Accionista en el giro",0,0,99),
                      numericInput("estados_financieros", "Estados Financieros",0,0,1),
                      numericInput("ventas_anuales", "Ventas Anuales",0,0)
                    ),
                    actionButton("writeCualitativosButton", "Grabar"),
                    br()
                  )         
         ),
         
         tabPanel("Balance",
                  conditionalPanel(
                    condition = "output.Balance=='Capturado'",
                    wellPanel(
                      tableOutput("tableBalance")
                    )                  
                  ),
                  conditionalPanel(
                    condition = "output.Balance=='No Capturado'",
                    wellPanel(
                      div(class="span5",h4("Activo"),
                        numericInput("act_caja_y_bancos" , "Caja y Bancos",0,0),
                        numericInput("act_inversiones_en_valores" , "Inversiones en Valores",0,0),
                        numericInput("act_cuentas_por_cobrar" , "Cuentas por Cobrar",0,0),
                        numericInput("act_clientes" , "Clientes",0,0),
                        numericInput("act_deudores_diversos_documentos_por_cobrar" , "Deudores Diversos / Documentos por Cobrar",0,0),
                        numericInput("act_impuestos_por_recuperar" , "Impuestos por Recuperar",0,0),
                        numericInput("act_anticipo_a_proveedores" , "Pagos anticipados",0,0),
                        numericInput("act_estimacion_de_cuentas_incobrables" , "Estimaciòn de Cuentas Incobrables",0,0),
                        numericInput("act_companias_afiliadas" , "Compañias Afiliadas",0,0),
                        numericInput("act_total_cuentas_por_cobrar" , "Total Cuentas por Cobrar",0,0),
                        numericInput("act_inventarios" , "Inventarios",0,0),
                        numericInput("act_otros_activos_circulantes" , "Otros Activos Circulantes",0,0),
                        numericInput("act_total_circulante" , "Total  Circulante",0,0,0),
                        numericInput("act_activos_diferidos" , "Activos Diferidos ",0,0),
                        numericInput("act_documentos_por_cobrar_lgo_pzo" , "Otros Activos de Largo Plazo",0,0),
                        numericInput("act_edificios_y_terrenos" , "Edificios y terrenos",0,0),
                        numericInput("act_maquinaria_y_equipo" , "Maquinaria y equipo",0,0),
                        numericInput("act_depreciacion" , "Depreciacion",0,0),
                        numericInput("act_total_activo_largo_plazo" , "Total Activo Largo Plazo",0,0),
                        numericInput("act_total__activo" , "TOTAL  ACTIVO",0,0),
                        #basura para que no quede con formato feo 
                        div(print("writeEstado"), style = "opacity:0"),div(print("writeEstado"), style = "opacity:0"),
                        div(print("writeEstado"), style = "opacity:0"),
                        div(print("writeEstado"), style = "opacity:0"),div(print("writeEstado"), style = "opacity:0"),
                        div(print("writeEstado"), style = "opacity:0")
                          ),
                      HTML('<h4>Pasivo</h1>'),
                        numericInput("pas_porcion_circulante_de_creditos_a_lp" , "Porción circulante de créditos a LP",0,0),
                        numericInput("pas_prestamos_bancarios_cp" , "Préstamos Bancarios",0,0),
                        numericInput("pas_proveedores" , "Proveedores",0,0),
                        numericInput("pas_acreedores" , "Acreedores",0,0),
                        numericInput("pas_documentos_por_pagar" , "Documentos por Pagar",0,0),
                        numericInput("pas_impuestos_por_pagar" , "Impuestos por Pagar",0,0),
                        numericInput("pas_companias_afiliadas" , "Compañias Afiliadas",0,0),
                        numericInput("pas_total_pasivo_corto_plazo" , " Total Pasivo Corto Plazo ",0,0),
                        numericInput("pas_prestamos_bancarios_lp" , " Préstamos Bancarios ",0,0),
                        numericInput("pas_otros_pasivos_lp" , " Otros Pasivos L.P ",0,0),
                        numericInput("pas_impuestos_diferidos" , " Impuestos Diferidos ",0,0),
                        numericInput("pas_total_pasivo_largo_plazo" , " Total Pasivo Largo Plazo ",0,0),
                        numericInput("pas_total_pasivo" , " TOTAL  PASIVO ",0,0),
                      h4("Capital"),
                        numericInput("cap_capital_social" , " Capital Social ",0,0),
                        numericInput("cap_reservas" , " Reservas ",0,0),
                        numericInput("cap_result_acumulados" , " Resultados Acumulados ",0,0),
                        numericInput("cap_revaluacion_de_activo_fijo" , "Revaluacion de Activo Fijo ",0,0),
                        numericInput("cap_aportaciones_p_futuros_aumentos_de_capital" , " Aportaciones p/ futuros aumentos de capital ",0,0),
                        numericInput("cap_resultado_del_ejercicio" , " Resultado del Ejercicio ",0,0),
                        numericInput("cap_total_capital_contable" , " Total Capital Contable ",0,0),
                        numericInput("total_pasivo_y_capital" , " TOTAL  PASIVO Y CAPITAL",0,0)
                    ),
                    actionButton("writeBalanceButton", "Grabar"),
                    br()
                  )         
         ),
         tabPanel("Estado",
                  conditionalPanel(
                    condition = "output.Estado=='Capturado'",
                    wellPanel(
                      tableOutput("tableEdoRes")
                    )                  
                  ),
                  conditionalPanel(
                    condition = "output.Estado=='No Capturado'",
                    wellPanel(
                      div(class="span5",
                      numericInput("total_ventas" , "TOTAL DE VENTAS ",0,0),
                      numericInput("devolucion_sobre_ventas" , "DEVOLUCION SOBRE VENTAS ",0,0),
                      numericInput("rebajas_sobre_ventas" , "REBAJAS SOBRE VENTAS ",0,0),
                      numericInput("total_ventas_netas" , "TOTAL DE VENTAS NETAS ",0,0),
                      numericInput("costo_ventas" , "COSTO DE VENTAS ",0,0),
                      numericInput("utilidad_bruta" , "UTILIDAD  BRUTA ",0,0),
                      numericInput("gastos_operacion" , "GASTOS  DE OPERACION ",0,0),
                      numericInput("gastos_venta" , "Gastos de Venta ",0,0),
                      numericInput("gastos_admin" , "Gastos de Administracion ",0,0),
                      numericInput("gastos_otros" , "Otros Gastos de Operación ",0,0),
                      numericInput("utilidad_operativa" , "UTILIDAD OPERATIVA ",0,0)),
                      numericInput("costo_integral_fin" , "Costo Integral de Financiamiento ",0,0),
                      numericInput("gastos_prod_fin" , "Gastos (Productos Financieros) ",0,0),
                      numericInput("perdida_cambios" , "Perdida (Utilidad en Cambios) ",0,0),
                      numericInput("otros_productos" , "Otros Productos (Gastos) ",0,0),
                      numericInput("otros_ingresos" , "Otros Ingresos ",0,0),
                      numericInput("utilidad_antes_imptos_partidas_especiales" , "UTILIDAD / (PERDIDA) ANTES DE PROVISION PARA IMPUESTOS Y PARTIDAS ESPECIALES",0,0),
                      numericInput("provision_impto_activo" , "Provisión para impuesto al activo ",0,0),
                      numericInput("impto_isr" , "Impuesto sobre la renta ",0,0),
                      numericInput("participacion_utilidades" , "Participación en las utilidades ",0,0),
                      numericInput("utilidad_ejercicio" , "UTILIDAD / (PERDIDA) DEL EJERCICIO ",0,0),
                      #basura para que no quede con formato feo 
                      div(print("writeEstado"), style = "opacity:0"),div(print("writeEstado"), style = "opacity:0")
                    ),
                    actionButton("writeEstadoButton", "Grabar"),
                    br()
                  )         
         )
      )
  )  
))
