# api_ejemplo.R
library(plumber)

#* @apiTitle API de Ejemplo para Análisis de Datos
#* @apiDescription API creada con Plumber para el examen de Data Science

#* Endpoint básico de salud
#* @get /health
function() {
  return(list(status = "OK", timestamp = Sys.time()))
}

#* Saludo personalizado
#* @get /saludo/<nombre>
function(nombre) {
  return(list(mensaje = paste("¡Hola", nombre, "!")))
}

#* Operaciones matemáticas - Suma
#* @get /suma
#* @param a:numeric Primer número
#* @param b:numeric Segundo número
function(a, b) {
  resultado <- a + b
  return(list(
    operacion = "suma",
    a = a,
    b = b,
    resultado = resultado
  ))
}

#* Estadísticas básicas de un vector
#* @post /estadisticas
#* @param datos Array de números
function(datos) {
  numeros <- as.numeric(datos)
  
  stats <- list(
    n = length(numeros),
    media = mean(numeros),
    mediana = median(numeros),
    desviacion_estandar = sd(numeros),
    minimo = min(numeros),
    maximo = max(numeros)
  )
  
  return(stats)
}

#* Filtrar datos por umbral
#* @post /filtrar
#* @param datos Array de números
#* @param umbral:numeric Valor mínimo para filtrar
function(datos, umbral) {
  numeros <- as.numeric(datos)
  filtrados <- numeros[numeros >= umbral]
  
  return(list(
    datos_originales = numeros,
    umbral = umbral,
    datos_filtrados = filtrados,
    cantidad_filtrados = length(filtrados)
  ))
}

#* Generar datos aleatorios
#* @get /datos-aleatorios
#* @param n:int Cantidad de números a generar
#* @param tipo Tipo de distribución: normal, uniforme
function(n = 10, tipo = "normal") {
  n <- as.integer(n)
  
  datos <- switch(tipo,
                  "normal" = rnorm(n),
                  "uniforme" = runif(n),
                  rnorm(n)  # por defecto normal
  )
  
  return(list(
    tipo = tipo,
    cantidad = n,
    datos = datos,
    estadisticas = list(
      media = mean(datos),
      desviacion = sd(datos)
    )
  ))
}

# Para ejecutar la API, usa este código en la consola:
# pr <- plumb("api_ejemplo.R")
# pr$run(host = "127.0.0.1", port = 8000)