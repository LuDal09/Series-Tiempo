
###para ver el mejor aic


# Definir una lista para almacenar modelos y AIC
modelos_aic <- list()

# Iterar sobre diferentes órdenes para encontrar los mejores modelos
for (p in 0:5) {
  for (d in 0:2) {
    for (q in 0:5) {
      modelo <- Arima(train, order = c(p, d, q),method = "CSS-ML",include.drift = T,include.constant = T )
      aic_valor <- modelo$aic
      modelos_aic[[paste("ARIMA", p, d, q, sep = "_")]] <- aic_valor
    }
  }
}

# Ordenar y mostrar los 10 mejores modelos 

mejores_modelos <- sort(unlist(modelos_aic))[1:20]
print(mejores_modelos)





####significacia 

# Definir una lista para almacenar modelos con sus AIC y significancia
modelos_significativos <- list()

# Iterar sobre diferentes órdenes para encontrar los mejores modelos
for (p in 0:5) {
  for (d in 0:2) {
    for (q in 0:5) {
      tryCatch({
        # Ajustar modelo ARIMA
        modelo <- Arima(train, order = c(p, d, q))
        
        # Verificar significancia de los coeficientes
        prueba <- coeftest(modelo)
        significativos <- all(prueba[, "Pr(>|z|)"] < 0.05) # Todos los coeficientes significativos
        
        # Guardar modelo si cumple la condición
        if (significativos) {
          aic_valor <- AIC(modelo)
          modelos_significativos[[paste("ARIMA", p, d, q, sep = "_")]] <- aic_valor
        }
      }, error = function(e) {
        # Ignorar modelos que no puedan ser ajustados
      })
    }
  }
}

# Ordenar y mostrar los 10 mejores modelos con coeficientes significativos
mejores_modelos_significativos <- sort(unlist(modelos_significativos))[1:15]
print(mejores_modelos_significativos)




#####por si acaso
val=ts_split(yt.c,sample.out=h)
train_9=val$train 
test_9=val$test





library(forecast)
library(lmtest)

# Definir una lista para almacenar modelos con sus AIC y significancia
modelos_significativos <- list()

# Iterar sobre diferentes órdenes para encontrar los mejores modelos
for (p in 0:5) {
  for (d in 1) {
    for (q in 0:5) {
      for (P in 2) { # Ajuste de parámetros estacionales
        for (D in 1) {
          for (Q in 1) {
            tryCatch({
              # Ajustar modelo ARIMA
              modelo <- Arima(train, order = c(p, d, q), 
                              seasonal = list(order = c(P, D, Q)))
              
              # Verificar significancia de los coeficientes
              prueba <- coeftest(modelo)
              if (!is.null(prueba)) {
                significativos <- all(prueba[, "Pr(>|z|)"] < 0.01, na.rm = TRUE)
                
                # Guardar modelo si cumple la condición
                if (significativos) {
                  aic_valor <- AIC(modelo)
                  modelo_nombre <- paste("ARIMA", p, d, q, P, D, Q, sep = "_")
                  modelos_significativos[[modelo_nombre]] <- aic_valor
                }
              }
            }, error = function(e) {
              # Ignorar modelos que no puedan ser ajustados
            })
          }
        }
      }
    }
  }
}

# Ordenar y mostrar los 15 mejores modelos con coeficientes significativos
if (length(modelos_significativos) > 0) {
  mejores_modelos_significativos <- sort(unlist(modelos_significativos))[1:60]
  print(mejores_modelos_significativos)
} else {
  print("No se encontraron modelos significativos.")
}




# Filtrar modelos significativos que cumplan d = 1 y D = 1
modelos_filtrados <- modelos_significativos[
  grepl("_1_.*_.*_1_", names(modelos_significativos))
]

# Ordenar y mostrar los mejores modelos filtrados
if (length(modelos_filtrados) > 0) {
  # Limitar la cantidad de modelos a un máximo de 60 o al total disponible
  limite_modelos <- min(60, length(modelos_filtrados))
  mejores_modelos_significativos <- sort(unlist(modelos_filtrados))[1:limite_modelos]
  
  print("Mejores modelos significativos con d = 1 y D = 1:")
  print(mejores_modelos_significativos)
} else {
  print("No se encontraron modelos significativos con d = 1 y D = 1.")
}


modelo$
