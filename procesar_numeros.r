# Función para leer los números desde un archivo
leer_numeros <- function(archivo) {
  if (!file.exists(archivo)) {
    stop("El archivo no existe. Por favor, verifica la ruta y el nombre del archivo.")
  }
  # Leer los números del archivo y convertirlos a un vector de enteros
  numeros <- as.integer(readLines(archivo))
  return(numeros)
}

# Función para calcular estadísticos
calcular_estadisticos <- function(numeros) {
  estadisticos <- list(
    media = mean(numeros),
    mediana = median(numeros),
    desviacion_estandar = sd(numeros)
  )
  return(estadisticos)
}

# Función para escribir los resultados en un archivo
escribir_resultados <- function(archivo_salida, estadisticos, cuadrados) {
  fileConn <- file(archivo_salida, "w")
  
  # Escribir los estadísticos en el archivo
  cat("Resultados del análisis:\n\n", file = fileConn)
  cat("Media: ", estadisticos$media, "\n", file = fileConn)
  cat("Mediana: ", estadisticos$mediana, "\n", file = fileConn)
  cat("Desviación Estándar: ", estadisticos$desviacion_estandar, "\n\n", file = fileConn)
  
  # Verificar si hay alta variabilidad
  if (estadisticos$desviacion_estandar > 10) {
    cat("Alta variabilidad detectada en los datos.\n\n", file = fileConn)
  }
  
  # Escribir los cuadrados de los números
  cat("Cuadrados de los números:\n", file = fileConn)
  cat(paste(cuadrados, collapse = ", "), "\n", file = fileConn)
  
  close(fileConn)
}

# Programa principal
archivo_entrada <- "numeros.txt" # Nombre del archivo de entrada
archivo_salida <- "resultados.txt" # Nombre del archivo de salida

# Leer los números
numeros <- leer_numeros(archivo_entrada)

# Calcular estadísticos
estadisticos <- calcular_estadisticos(numeros)

# Calcular el cuadrado de cada número usando sapply
cuadrados <- sapply(numeros, function(x) x^2)

# Escribir los resultados en el archivo de salida
escribir_resultados(archivo_salida, estadisticos, cuadrados)

# Confirmación de ejecución
cat("El análisis ha finalizado. Los resultados se han guardado en el archivo:", archivo_salida, "\n")
