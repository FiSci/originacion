library(shiny)

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
       textOutput("writeFecha"),
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
         tabPanel("Balance" #,tableOutput("tableBalance"),actionButton("GrabaB", "Grabar")
         ), 
         tabPanel("Estado"#, tableOutput("tableEstado"),actionButton("GrabaE", "Grabar")
         ), 
         tabPanel("Cualitativos",
           tableOutput("tableCualit"),
           conditionalPanel(
             condition = "output.Cualit=='No Capturado'",
              wellPanel(
                numericInput("edadAccionista", "Edad del Principal Accionista",0)
                #,
                #numericInput("antiAccionista", "Antiguedad del Principal Accionista en el domicilio",0),
                #numericInput("antiNegocio", "Antiguedad en el negocio",0),
                #numericInput("expAccionista", "Experiencia del Principal Accionista en el giro",0),
                #numericInput("estadosFin", "Estados Financieros",0),
                #numericInput("ventasAnuales", "Ventas Anuales",0)
              ),
               actionButton("writeCualitativosButton", "Grabar")
           )         
        )
      )
  )  
))
