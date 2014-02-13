# Funciones para hacer login y cambio de contraseña
getUsuariosDB <- function(params) {
  con <- dbConnect(MySQL(), 
                   user=params$user,
                   password=params$password,
                   dbname=params$dbname,
                   host=params$host
  )
  query <- "select id, email from usuario"
  res <- dbGetQuery(con, query)
  dbDisconnect(con)
  res
}

userAuth <- function(user_id, password, params) {
  ret <- 0
  con <- dbConnect(MySQL(), 
                   user=params$user,
                   password=params$password,
                   dbname=params$dbname,
                   host=params$host)
  query <- paste("select auth from usuario where id = ", user_id, sep="")
  res <- dbGetQuery(con, query)
  dbDisconnect(con)
  if(dim(res)[1] == 0) {
    ret <- -1
  } else if(res$auth != digest(password)){
    ret <- 1
  }
  ret
}

userNewPass <- function(user_id, password, newPassword1, newPassword2, params) {
  if(digest(newPassword1) == digest(newPassword2)) {
    ret <- userAuth(user_id, password, params)
  } else {
    ret <- -1
  }
  if(ret == 0) {
    con <- dbConnect(MySQL(), 
                     user=params$user,
                     password=params$password,
                     dbname=params$dbname,
                     host=params$host)
    query <- paste("update usuario set auth = '", digest(newPassword1), "' where id = ", user_id, sep="")
    dbSendQuery(con, query)
    dbDisconnect(con)
  }
  ret 
}

