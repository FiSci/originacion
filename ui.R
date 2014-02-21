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
      # Panel para introducir nueva empresa
      conditionalPanel(
        condition = "input.empresa_id == 'Nueva Empresa'",
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
        condition = "input.empresa_id != 'Selecciona Empresa'",
        conditionalPanel(
          condition = "input.empresa_id != 'Nueva Empresa'",
          conditionalPanel(
            condition = "input.empresa_info_id == 'Nueva Fecha'",
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
    div(textOutput("writeEmpresa"), style = "opacity:0"),
    div(textOutput("writeFecha"),style = "opacity:0"),
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
#           tableOutput("tableCualit"),
           textOutput("cualit"),
           conditionalPanel(
             condition = "output.cualit=='No Capturado'",
            
              wellPanel(
                numericInput("EdadAccionista", "Edad del Principal Accionista",0),
                numericInput("AntiAccionista", "Antiguedad del Principal Accionista en el domicilio",0),
                numericInput("AntiNegocio", "Antiguedad en el negocio",0),
                numericInput("ExpAccionista", "Experiencia del Principal Accionista en el giro",0),
                numericInput("EdoFin", "Estados Financieros",0)
              ),
               actionButton("GrabaC", "Grabar")
           )         
        )
      )
  )  
))
