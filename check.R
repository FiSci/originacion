library(gdata)

inputFormat <- function(x) {
  x <- trim(x)
  x <- toupper(x)
  x
}

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

