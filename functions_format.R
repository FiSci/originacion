inputFormat <- function(x) {
  x <- trim(x)
  x <- toupper(x)
  x
}
# Funciones que formatean los datos para mostrarlos en la UI
showEmpresas <- function(empresas) {
  if(length(empresas) > 0) {
    uniqueEmpresas <- unique(empresas[,c("empresa_id","nombre")])
    ret <- as.list(uniqueEmpresas$empresa_id)
    names(ret) <- uniqueEmpresas$nombre
    ret <- c(list("Selecciona Empresa"=-999), ret, list("Nueva Empresa"=-998))
  } else {
    ret <- list("Nueva Empresa"=-998)
  }
  ret 
}  

showFechas <- function(empresa_id, empresasDF){
  if(length(empresasDF) > 0) {
    fechaEmpresa <- empresasDF[empresasDF$empresa_id==empresa_id, c("empresa_info_id", "fecha_informacion")]
    if(sum(is.na(fechaEmpresa$empresa_info_id)) == 0 ) {
      ret <- as.list(fechaEmpresa$empresa_info_id)
      names(ret) <- fechaEmpresa$fecha_informacion
      ret <- c(ret, "Nueva Fecha"=-998)
    } else{
      ret <- list("Nueva Fecha"=-998)
    }
      ret <- c(list("Selecciona Fecha"=-999), ret)
  } else {
    ret <- list("Nueva Fecha"=-998)
  }
  
}


showInfoCualitativos <- function(info) {
  info <- as.data.frame(t(info))
  names(info) <- "VALOR"
  info
}

showUsuarios <- function(info) {
  ret <- as.list(info$id)
  names(ret) <- info$email
  ret
}

showNombresCalifica <- function(usuarios) {
  ret <- list()
  if(length(usuarios) > 0) {
    ret <- as.list(usuarios$id)
    names(ret) <- paste(usuarios$nombres,usuarios$apellido_paterno,usuarios$apellido_materno,sep=" ")
  } 
  ret
}  
###########
showInfoEmpresa <- function(empresa_id, empresasDF) {
  Informacion <- empresasDF[!is.na(empresasDF$empresa_id), ]
  Informacion <- Informacion[Informacion$empresa_id==empresa_id, ]
  ret1<- Informacion$nombre[1]
  ret2 <- Informacion$rfc[1]
  ret3 <- Informacion$razon_social[1]
  c(ret1,ret2,ret3)
}

showStatus <- function(empresa_info_id, empresasDF){
  Informacion <- empresasDF[!is.na(empresasDF$empresa_info_id), ]
  Informacion <- Informacion[Informacion$empresa_info_id == empresa_info_id, 
                             c("estado_resultados_fecha","balance_fecha","cualitativo_fecha", "buro_fecha")]
  if (sum(is.na(Informacion))>0)
    Status="Incompleto"
  else
    Status="Completo"
}

existe <-function(empresa_info_id, Tabla){
  existencia <- sum(Tabla[!is.na(Tabla$empresa_info_id),]$empresa_info_id==empresa_info_id)
  existencia
}

Captura <-function(empresa_info_id, empresasDF){
  Informacion <- empresasDF[!is.na(empresasDF$empresa_info_id), ]
  Informacion <- Informacion[Informacion$empresa_info_id == empresa_info_id, 
                             c("estado_resultados_fecha","balance_fecha","cualitativo_fecha", "buro_fecha", "terms_fecha")]
  Informacion[!is.na(Informacion)] <- 1
  Informacion[is.na(Informacion)] <- 0  
  Informacion
}

####Funciones para formatear las tablas que se muestran con datos
formatoTabla <- function(x, catalogo){
  dat <- as.data.frame(t(x[,-1]))
  newNames <- catalogo$Nombre_Tabla[catalogo$Nombre_Base %in% rownames(dat)]
  data.frame(Concepto=newNames, Valor=dat[,1])
}

formatoTablaBuro <- function(x, catalogo){
  dat <- formatoTabla(x, catalogo)
  dat$Valor[dat$Concepto=="Atraso"] <- 
    ifelse(dat$Valor[dat$Concepto=="Atraso"]==0, "No", "Si")
  
  dat$Valor[dat$Concepto=="Comportamiento Principal Accionista en Buró Persona Moral"] <- 
    ifelse(dat$Valor[dat$Concepto=="Comportamiento Principal Accionista en Buró Persona Moral"]==0, 
           "Sin Referencias",
           ifelse(dat$Valor[dat$Concepto=="Comportamiento Principal Accionista en Buró Persona Moral"]==1,
                  "Malo",
                  "Bueno")
    )
           
  dat$Valor[dat$Concepto=="Comportamiento Principal Accionista en Buró Persona Física"] <- 
    ifelse(dat$Valor[dat$Concepto=="Comportamiento Principal Accionista en Buró Persona Física"]==0, 
            "Sin Referencias",
            ifelse(dat$Valor[dat$Concepto=="Comportamiento Principal Accionista en Buró Persona Física"]==1,
                    "Malo",
                    "Bueno")
    )
  dat

}
