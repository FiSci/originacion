library(shiny)
library(leaflet)
library(ShinyDash)

# Define UI for miles per gallon application
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Precalificación de crédito"),
  
  # Sidebar with controls to select the variable to plot against mpg
  # and to specify whether outliers should be included
  sidebarPanel(
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
        uiOutput("seleccionaFecha"),
        actionButton("consultaButton", "Consultar Informacion")
      ),
      conditionalPanel(
        condition = "input.empresa_id == 'Nueva Empresa'",
        wellPanel(
          textInput("capturaEmpNombre", "Nombre:"),
          textInput("capturaEmpRFC", "RFC:"),
          textInput("capturaEmpRS", "Razon Social:"),
          actionButton("writeEmpresaButton", "Guardar Empresa")
        )
      ),
      conditionalPanel(
        condition = "input.empresa_id != 'Selecciona Empresa'",
        conditionalPanel(
          condition = "input.empresa_id != 'Nueva Empresa'",
          conditionalPanel(
            condition = "input.empresa_info_id == 'Nueva Fecha'",
            wellPanel(
              textInput("capturaFecha", "Fecha:"),
              actionButton("writeFechaButton", "Guardar Fecha")
            )
          )
        )
      )
      
    ) # Cierra el panel de login
  ),
  ################
  mainPanel(
    div(textOutput("writeEmpresa"), style = "opacity:0"),
    div(textOutput("writeCualitativos"), style = "opacity:0"),
    div(textOutput("writeEstado"), style = "opacity:0"),
    div(textOutput("writeBalance"), style = "opacity:0"),
       wellPanel(
         h4("Informacion de la Empresa"),
         print("Nombre:"),textOutput("nombreEmpresa"),
         print("RFC:"), textOutput("rfcEmpresa"),
         print("Razon Social:"), textOutput("rsEmpresa"),
         print("Status"), textOutput("Status"),
         print("Balance General"), textOutput("Balance"),
         print("Estado de Resultados"), textOutput("Estado"),
         print("Informacion Cualitativa"), textOutput("Cualit")
       ),
       
       h4("Estados Financieros"),
       
       tabsetPanel( 
         tabPanel("Cualitativos",
                  tableOutput("tableCualit"),
                  conditionalPanel(
                    condition = "output.Cualit=='No Capturado'",
                    wellPanel(
                      numericInput("edad_principal_accionista", "Edad del Principal Accionista",0),
                      numericInput("antiguedad_principal_accionista_domicilio", "Antiguedad del Principal Accionista en el domicilio",0),
                      numericInput("antiguedad_negocio", "Antiguedad en el negocio",0),
                      numericInput("experiencia_principal_accionista_giro", "Experiencia del Principal Accionista en el giro",0),
                      numericInput("estados_financieros", "Estados Financieros",0),
                      numericInput("ventas_anuales", "Ventas Anuales",0)
                    ),
                    actionButton("writeCualitativosButton", "Grabar")
                  )         
         ),
         
         tabPanel("Balance",
                  tableOutput("tableBalance"),
                  conditionalPanel(
                    condition = "output.Balance=='No Capturado'",
                    wellPanel(
                      h2("Activo"),
                        numericInput("act_caja_y_bancos" , "Caja y Bancos",0),
                        numericInput("act_inversiones_en_valores" , "Inversiones en Valores",0),
                        numericInput("act_cuentas_por_cobrar" , "Cuentas por Cobrar",0),
                        numericInput("act_clientes" , "Clientes",0),
                        numericInput("act_deudores_diversos_documentos_por_cobrar" , "Deudores Diversos / Documentos por Cobrar",0),
                        numericInput("act_impuestos_por_recuperar" , "Impuestos por Recuperar",0),
                        numericInput("act_anticipo_a_proveedores" , "Pagos anticipados",0),
                        numericInput("act_estimacion_de_cuentas_incobrables" , "Estimaciòn de Cuentas Incobrables",0),
                        numericInput("act_companias_afiliadas" , "Compañias Afiliadas",0),
                        numericInput("act_total_cuentas_por_cobrar" , "Total Cuentas por Cobrar",0),
                        numericInput("act_inventarios" , "Inventarios",0),
                        numericInput("act_otros_activos_circulantes" , "Otros Activos Circulantes",0),
                        numericInput("act_total_circulante" , "Total  Circulante",0),
                        numericInput("act_activos_diferidos" , "Activos Diferidos ",0),
                        numericInput("act_documentos_por_cobrar_lgo_pzo" , "Otros Activos de Largo Plazo",0),
                        numericInput("act_edificios_y_terrenos" , "Edificios y terrenos",0),
                        numericInput("act_maquinaria_y_equipo" , "Maquinaria y equipo",0),
                        numericInput("act_depreciacion" , "Depreciacion",0),
                        numericInput("act_total_activo_largo_plazo" , "Total Activo Largo Plazo",0),
                        numericInput("act_total__activo" , "TOTAL  ACTIVO",0),
                      h2("Pasivo"),
                        numericInput("pas_porcion_circulante_de_creditos_a_lp" , "Porción circulante de créditos a LP",0),
                        numericInput("pas_prestamos_bancarios_cp" , "Préstamos Bancarios",0),
                        numericInput("pas_proveedores" , "Proveedores",0),
                        numericInput("pas_acreedores" , "Acreedores",0),
                        numericInput("pas_documentos_por_pagar" , "Documentos por Pagar",0),
                        numericInput("pas_impuestos_por_pagar" , "Impuestos por Pagar",0),
                        numericInput("pas_companias_afiliadas" , "Compañias Afiliadas",0),
                        numericInput("pas_total_pasivo_corto_plazo" , " Total Pasivo Corto Plazo ",0),
                        numericInput("pas_prestamos_bancarios_lp" , " Préstamos Bancarios ",0),
                        numericInput("pas_otros_pasivos_lp" , " Otros Pasivos L.P ",0),
                        numericInput("pas_impuestos_diferidos" , " Impuestos Diferidos ",0),
                        numericInput("pas_total_pasivo_largo_plazo" , " Total Pasivo Largo Plazo ",0),
                        numericInput("pas_total_pasivo" , " TOTAL  PASIVO ",0),
                      h2("Capital"),
                        numericInput("cap_capital_social" , " Capital Social ",0),
                        numericInput("cap_reservas" , " Reservas ",0),
                        numericInput("cap_result_acumulados" , " Resultados Acumulados ",0),
                        numericInput("cap_revaluacion_de_activo_fijo" , "Revaluacion de Activo Fijo ",0),
                        numericInput("cap_aportaciones_p_futuros_aumentos_de_capital" , " Aportaciones p/ futuros aumentos de capital ",0),
                        numericInput("cap_resultado_del_ejercicio" , " Resultado del Ejercicio ",0),
                        numericInput("cap_total_capital_contable" , " Total Capital Contable ",0),
                        numericInput("total_pasivo_y_capital" , " TOTAL  PASIVO Y CAPITAL",0)
                    ),
                    actionButton("writeBalanceButton", "Grabar")
                  )         
         ),
         tabPanel("Estado",
                  tableOutput("tableEdoRes"),
                  conditionalPanel(
                    condition = "output.Estado=='No Capturado'",
                    wellPanel(
                      numericInput("total_ventas" , "TOTAL DE VENTAS ",0),
                      numericInput("devolucion_sobre_ventas" , "DEVOLUCION SOBRE VENTAS ",0),
                      numericInput("rebajas_sobre_ventas" , "REBAJAS SOBRE VENTAS ",0),
                      numericInput("total_ventas_netas" , "TOTAL DE VENTAS NETAS ",0),
                      numericInput("costo_ventas" , "COSTO DE VENTAS ",0),
                      numericInput("utilidad_bruta" , "UTILIDAD  BRUTA ",0),
                      numericInput("gastos_operacion" , "GASTOS  DE OPERACION ",0),
                      numericInput("gastos_venta" , "Gastos de Venta ",0),
                      numericInput("gastos_admin" , "Gastos de Administracion ",0),
                      numericInput("gastos_otros" , "Otros Gastos de Operación ",0),
                      numericInput("utilidad_operativa" , "UTILIDAD OPERATIVA ",0),
                      numericInput("costo_integral_fin" , "Costo Integral de Financiamiento ",0),
                      numericInput("gastos_prod_fin" , "Gastos (Productos Financieros) ",0),
                      numericInput("perdida_cambios" , "Perdida (Utilidad en Cambios) ",0),
                      numericInput("otros_productos" , "Otros Productos (Gastos) ",0),
                      numericInput("otros_ingresos" , "Otros Ingresos ",0),
                      numericInput("utilidad_antes_imptos_partidas_especiales" , "UTILIDAD / (PERDIDA) ANTES DE PROVISION PARA IMPUESTOS Y PARTIDAS ESPECIALES ",0),
                      numericInput("provision_impto_activo" , "Provisión para impuesto al activo ",0),
                      numericInput("impto_isr" , "Impuesto sobre la renta ",0),
                      numericInput("participacion_utilidades" , "Participación en las utilidades ",0),
                      numericInput("utilidad_ejercicio" , "UTILIDAD / (PERDIDA) DEL EJERCICIO ",0)
                    ),
                    actionButton("writeEstadoButton", "Grabar")
                  )         
         )
      )
  )  
))
