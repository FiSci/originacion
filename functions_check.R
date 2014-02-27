library(gdata)

# Revisa que la entrada de RFC unicamente puedan ser letras 
# mayusculas y numeros
checkRFC <- function(x) {
  status <- 0
  # Revisa que la longitud del RFC sea = 12
  if(nchar(x) != 12) {
    status <- 1
  }
  # Unicamente acepta letras mayusculas y numeros
  if(!grepl(pattern="^[A-Z0-9]*$", x)) {
    status <- 1
  }
  # Tres primeros caracteres son letras mayusculas
  if(!grepl(pattern="^[A-Z]*$", substr(x, start=1, stop=3))) {
    print(substr(x, start=1, stop=3))
    status <- 1
  }
  #
  if(!grepl(pattern="^[0-9]*$", substr(x, start=4, stop=9))) {
    print(substr(x, start=4, stop=9))
    status <- 1
  }
  status
}
#

# No vacios
noVacios <- function(x) {
  ret <- 0
  if(!is.null(x)) {
    if(sum(is.na(unlist(x))) > 0) {
      ret <- 1
    }  
  }
  ret
}

# Cualitativos
checkCualit_edad <- function(x, min, max) {
  ret <- 0
  if(x$edad_principal_accionista < min | x$edad_principal_accionista > max) {
    ret <- 1
  }
  ret
}

checkCualit_antiguedadDomicilio <- function(x, min, max) {
  ret <- 0
  if(x$antiguedad_principal_accionista_domicilio < min | x$antiguedad_principal_accionista_domicilio > max) {
    ret <- 1
  }
  ret
}

checkCualit_antiguedadNegocio <- function(x, min, max) {
  ret <- 0
  if(x$antiguedad_negocio < min | x$antiguedad_negocio > max) {
    ret <- 1
  }
  ret
}

checkCualit_experienciaPrincipalAccionistaGiro <- function(x, min, max) {
  ret <- 0
  if(x$experiencia_principal_accionista_giro < min | x$experiencia_principal_accionista_giro > max) {
    ret <- 1
  }
  ret
}

checkCualit_ventasAnuales <- function(x, min, max) {
  ret <- 0
  if(x$ventas_anuales < min | x$ventas_anuales > max) {
    ret <- 1
  }
  ret
}

checkCualit_estadosFinancieros <- function(x) {
  ret <- 0
  if(x$estados_financieros < 0 | x$estados_financieros > 1) {
    ret <- 1
  }
  ret
}

# Balance
checkBal_ActivosTotales <- function(x, tol=0.005) {
  ret <- 0
  dif <- x$act_total__activo - (x$act_total_circulante + x$act_total_activo_largo_plazo + x$act_activos_diferidos)
  if(abs(dif) > x$act_total__activo * tol) {
    ret <- 1
  }
  ret
}

checkBal_ActivosPasivos <- function(x, tol=0.005) {
  ret <- 0
  dif <- x$act_total__activo - (x$pas_total_pasivo + x$cap_total_capital_contable)
  if(abs(dif) > x$act_total__activo * tol) {
    ret <- 1
  }
  ret
}

checkBal_PasivoCapital <- function(x, tol=0.005) {
  ret <- 0
  dif <- x$total_pasivo_y_capital - (x$pas_total_pasivo + x$cap_total_capital_contable)
  if(abs(dif) > x$total_pasivo_y_capital * tol) {
    ret <- 1
  }
  ret
}

# Resultados
checkEdoRes_UtilidadBruta <- function(x, tol=0.005) {
  ret <- 0
  dif <- x$utilidad_bruta - (x$total_ventas - x$costo_ventas)
  if(abs(dif) > x$utilidad_bruta * tol) {
    ret <- 1
  }
  ret
}


