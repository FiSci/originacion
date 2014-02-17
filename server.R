library(shiny)
library(devtools)
library(datasets)
library(shinyIncubator)
library(RMySQL)
library(digest)

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
      empresasDF <- getEmpresasDB(paramsDB, logedId)
      
      # Lee las empresas de la BD para el usuario registrado
      output$seleccionaEmpresa <- renderUI({
        input$writeEmpresaButton
        empresasDF <- getEmpresasDB(paramsDB, logedId)
        selectInput("empresa_id", "Selecciona Empresa", showEmpresas(empresasDF), "Selecciona Empresa")
      })
      
      output$seleccionaFecha <- renderUI({
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
        writeEmpresaDB(paramsDB, logedId, valueList)
        0
      })
      output$writeEmpresa <- renderText(writeEmpresa())


      # Guarda nueva fecha    
      output$writeFecha <- renderText({
        if (input$writeFechaButton == 0) 
          return("")
        else 
          return(isolate(input$capturaFecha))
      })
      
      
      
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
            cualitativosDF<-getInfoCualitativosDB(paramsDB,isolate(input$empresa_info_id))
            if(existe(isolate(input$empresa_info_id), cualitativosDF)==0) 
              return(NULL)
            else{
              dat <- t(cualitativosDF[cualitativosDF$empresa_info_id==isolate(input$empresa_info_id)])
              Name<-cbind(rownames(dat),dat)
              colnames(Name)[1]<-"Nombre_Base"
              Name<-as.data.frame(Name)
              A<-merge(Catalogo,Name, by.x="Nombre_Base",by.y="Nombre_Base", all.x=TRUE)
              dat<-A[,2:3]
              names(dat)<-c("Descripcion","Valor")
              return(dat)
            }
         }
         else
           return(NULL)
       })   
    }    
  })
  
  
})
    

  # Agregar boton y cuando se pique se correra un codigo que consulta en BD la info de la
  # empresa
  
