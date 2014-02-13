#
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
  Info <- empresasDF[empresasDF$empresa_info_id==empresa_info_id, c("nombre","rfc")]
  ret1<- Info$nombre
  ret2 <- Info$rfc
  ret3 <- Info$rfc
  ret<-c(ret1,ret2,ret3)
}

showStatus <- function(empresa_info_id, empresasDF){
  Informacion <- empresasDF[empresasDF$empresa_info_id==empresa_info_id, c("estado_resultados_fecha","balance_fecha","cualitativo_fecha")]
  if (sum(is.na(Informacion))>2)
    Status="Incompleto"
  else
    Status="Completo"
}

existe <-function(empresa_info_id, A){
  A <- sum(A$empresa_info_id==isolate(empresa_info_id))
  A
}

Captura <-function(empresa_info_id, empresasDF){
  Informacion <- empresasDF[empresasDF$empresa_info_id==empresa_info_id, c("estado_resultados_fecha","balance_fecha","cualitativo_fecha")]
  Informacion[!is.na(Informacion)]<-1
  Informacion[is.na(Informacion)]<-0  
  Informacion
}