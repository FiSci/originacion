# Funciones que formatean los datos para mostrarlos en la UI
showEmpresas <- function(empresas) {
  uniqueEmpresas <- unique(empresas[,c("empresa_id","nombre")])
  ret <- as.list(uniqueEmpresas$empresa_id)
  names(ret) <- uniqueEmpresas$nombre
  c(list("Selecciona Empresa"="Selecciona Empresa"), ret, list("Nueva Empresa"="Nueva Empresa"))
}  

showFechas <- function(empresa_id, empresasDF){
  fechaEmpresa <- empresasDF[empresasDF$empresa_id==empresa_id, c("empresa_info_id", "fecha_informacion")]
  if(sum(is.na(fechaEmpresa$empresa_info_id)) == 0 ) {
    ret <- as.list(fechaEmpresa$empresa_info_id)
    names(ret) <- fechaEmpresa$fecha_informacion
    ret <- c(ret, list("Nueva Fecha"="Nueva Fecha"))
  } else{
    ret <- list("Nueva Fecha"="Nueva Fecha")
  }
  ret
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
###########
showInfoEmpresa <- function(empresa_info_id, empresasDF){
  Informacion <- empresasDF[!is.na(empresasDF$empresa_info_id), ]
  Informacion <- Informacion[Informacion$empresa_info_id==empresa_info_id, ]
  ret1<- Informacion$nombre
  ret2 <- Informacion$rfc
  ret3 <- Informacion$rfc
  ret<-c(ret1,ret2,ret3)
  ret
}

showStatus <- function(empresa_info_id, empresasDF){
  Informacion <- empresasDF[!is.na(empresasDF$empresa_info_id), ]
  Informacion <- Informacion[Informacion$empresa_info_id==empresa_info_id, c("estado_resultados_fecha","balance_fecha","cualitativo_fecha")]
  if (sum(is.na(Informacion))>1)
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
  Informacion <- Informacion[Informacion$empresa_info_id==empresa_info_id, c("estado_resultados_fecha","balance_fecha","cualitativo_fecha")]
  Informacion[!is.na(Informacion)]<-1
  Informacion[is.na(Informacion)]<-0  
  Informacion
}

####Funciones para formatear las tablas que se muestran con datos

####Cualitativos
creaTablaCualitativos<-function (cualitativosDF,empresa_info_id){
    dat <- t(cualitativosDF[cualitativosDF$empresa_info_id==empresa_info_id])
    Name <- cbind(rownames(dat),dat)
    colnames(Name)[1]<-"Nombre_Base"
    Name <- as.data.frame(Name)
    A <- merge(catalogo_cualitativo,Name, by.x="Nombre_Base",by.y="Nombre_Base", all.x=TRUE)
    dat <- A[,2:3]
    names(dat) <- c("Descripcion","")
    dat
}

####Balance
creaTablaBalance<-function (balanceDF,empresa_info_id){
  dat <- t(balanceDF[balanceDF$empresa_info_id==empresa_info_id])
  Name<-cbind(rownames(dat),dat)
  colnames(Name)[1]<-"Nombre_Base"
  Name<-as.data.frame(Name)
  A<-merge(catalogo_balance,Name, by.x="Nombre_Base",by.y="Nombre_Base", all.x=TRUE)
  dat<-A[,2:3]
  names(dat)<-c("Descripcion","")
  dat
}

####Estado
creaTablaEdoRes<-function (EdoResDF,empresa_info_id){
  dat <- t(EdoResDF[EdoResDF$empresa_info_id==empresa_info_id])
  Name<-cbind(rownames(dat),dat)
  colnames(Name)[1]<-"Nombre_Base"
  Name<-as.data.frame(Name)
  A<-merge(catalogo_estado,Name, by.x="Nombre_Base",by.y="Nombre_Base", all.x=TRUE)
  dat<-A[,2:3]
  names(dat)<-c("Descripcion","")
  dat
}
