# Funciones para leer informacion de empresa

getEmpresasDB <- function(params, usuario_id) {
  con <- dbConnect(MySQL(), 
                   user=params$user,
                   password=params$password,
                   dbname=params$dbname,
                   host=params$host
  )
  query <- paste("select 
                 ei.id empresa_info_id, 
	               e.id empresa_id, 
                 e.nombre nombre, 
                 e.rfc rfc, 
                 ei.fecha_informacion, 
                 ei.estado_resultados_fecha, 
                 ei.balance_fecha, 
                 ei.cualitativo_fecha,
                 ue.usuario_id
                 from usuario_empresa ue 
                 left join empresa e on ue.empresa_id = e.id 
                 left join empresa_info ei on e.id = ue.empresa_id and e.id = ei.empresa_id and ue.usuario_id
                 where ue.usuario_id = ", 
                 usuario_id, sep="")
  res <- dbGetQuery(con, query)
  dbDisconnect(con)
  res
}
getInfoCualitativosDB <- function(params, empresa_info_id) {
  con <- dbConnect(MySQL(), 
                   user=params$user,
                   password=params$password,
                   dbname=params$dbname,
                   host=params$host
  )
  query <- paste("select * from info_cualitativo where empresa_info_id =",empresa_info_id , sep="")
  res <- dbGetQuery(con, query)
  dbDisconnect(con)
  res
}
getInfoBalanceDB <- function(params, empresa_info_id) {
  # Esta funcion debe regresar toda la informacion del balance en un DF
}
getInfoEdoResDB <- function(params, empresa_info_id) {
  # Esta funcion debe regresar toda la informacion de los estados de res en un DF
}

###

createInsertQueryFromList <- function(x) {
  fields <- paste("(", paste(names(x), collapse=","), ")", sep="" )
  values <- paste("('", paste(unlist(x), collapse="','"), "')", sep="")
  paste(fields, "VALUES", values)
}

writeEmpresaDB <- function(params, usuario_id, valueList) { 
  query <- paste("CALL sp_insertempresa(@FLAG, ", 
                 usuario_id, ",'",
                 valueList$rfc, "','",
                 valueList$nombre, "','",
                 valueList$razon_social, "')",
                 sep=""
  )
  con <- dbConnect(MySQL(), 
                   user=params$user,
                   password=params$password,
                   dbname=params$dbname,
                   host=params$host
  )
  dbSendQuery(con, query)
#  res <- dbSendQuery(con, "select @FLAG;")
  dbDisconnect(con)
#  res
}

writeFechaDB <- function(params, usuario_id, valueList) { 
  query <- paste("CALL sp_insertfecha(@FLAG, ", 
                 usuario_id, ",'",
                 valueList$empresa, "','",
                 valueList$fecha, "')",
                 sep=""
  )
  con <- dbConnect(MySQL(), 
                   user=params$user,
                   password=params$password,
                   dbname=params$dbname,
                   host=params$host
  )
  dbSendQuery(con, query)
  #  res <- dbSendQuery(con, "select @FLAG;")
  dbDisconnect(con)
  #  res
}
