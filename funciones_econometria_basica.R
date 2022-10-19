library(tidyverse)

theme_set(theme_classic() + theme(plot.title = element_text(hjust = .5)))

# ANALISIS EXPLORATORIO -----------

exploratorio <- function(data, x, y) {
  require(ggplot2)
  valor_x <- data |> select({{ x }}) ;valor_x <- valor_x[,1];
  valor_y <- data |> select({{ y }}); valor_y <- valor_y[,1];
  
  
  grafico_puntoz <- grafico_puntos(data, {{ x }}, {{ y }})
  histograma_x <- grafico_histograma(data, {{ x }})  
  histograma_y <- grafico_histograma(data, {{ y }})  
  tabla_resumen_x <- estadistica_univariante(valor_x)
  tabla_resumen_y <- estadistica_univariante(valor_y)
  
  lista <- list("grafico_puntos" = grafico_puntos, "histograma_x" = histograma_x, 
                "histograma_y" = histograma_y, "tabla_resumen_x" = tabla_resumen_x,
                "tabla_resumen_y" = tabla_resumen_y)
  return(lista)
}

grafico_histograma <- function(data, variable) {
  x <- dplyr::select(data, {{ variable }})
  ancho <- calcular_ancho(x[,1])
  return (ggplot(data, aes(x = {{ variable }} )) + 
            geom_histogram(binwidth = ancho, color = "black", fill = "white") + 
            geom_density(aes(y = ..count..*ancho), color = "red"))
}

grafico_puntos <- function(data, x, y) {
  return(ggplot(data, aes(x = {{ x }}, y = {{ y }} )) + geom_point())
}

calcular_ancho <- function(x) {
  return((max(x) - min(x)) / nclass.Sturges(x))
}

estadistica_univariante <- function(x) {
  df <- data.frame(unclass(summary(x))) |> rownames_to_column("medida") 
  colnames(df) <- c("medida", "valor")
  df |> pivot_wider(names_from = medida, values_from = valor) -> df 
  return(df)
}


# COMPLETAR PARA LA FUNCION EXPLORATORIO ----------

estadistica_bivariante <- function(x, y) {
  
  return()
}


# REGRESION LINEAL SIMPLE ---------------------

modelar <- function(datos, x, y) {
  
  modelo <- lm(paste(y, '~', x), data = datos)
  histograma_residuos <- grafico_residuos(modelo) 
  grafico_residuos_ajustados <- grafico_homocedasticidad(modelo)
  
  h_residuos <- hipotesis_residuos_normal(residuals(modelo))
  h_homocedasticidad <- hipotesis_homocedasticidad(modelo)
  h_autocorrelacion <- hipotesis_autocorrelacion(modelo)
  lista <- list("modelo" = modelo, residuos = histograma_residuos,"h_residuos" = h_residuos,
                "h_homocedasticidad" = h_homocedasticidad,
                "g_homocedasticidad" = grafico_residuos_ajustados,
                "h_autocorrelacion" = h_autocorrelacion)
  return(lista)  
}


grafico_residuos <- function(modelo) {
  ancho <- calcular_ancho(residuals(modelo))
  p <- ggplot(modelo, aes(x = residuals(modelo))) + 
    geom_histogram(color = "black", fill = "white", binwidth = ancho) +
    geom_density(aes(y = ..count..* ancho ), color = "red", size = 1.2) + 
    geom_vline(xintercept = mean(residuals(modelo)), color = "blue")
  return(p)
}

hipotesis_residuos_normal <- function(x) {
  if (length(x) > 50) {
    test <- nortest::lillie.test(x)    
  } else {
    test <- shapiro.test(x)
  }
  return(test)
}

hipotesis_homocedasticidad <- function(modelo) {
  
  return(lmtest::bptest(modelo))
}


# COMPLETAR PARA FUNCION MODELAR ---------------------------------------------

grafico_homocedasticidad <- function(modelo) {
  
  p <- ggplot(modelo, aes(x = fitted(modelo), y = residuals(modelo))) + geom_point() +
    labs(x = "valores ajustados", y = "residuos", title = "Residuos VS valores predichos")
  
  return(p)
}

hipotesis_autocorrelacion <- function(modelo) {
  # escribe tu codigo
  
  return(lmtest::dwtest(modelo))
}

grafico_autocorrelacion <- function() {
  # escribe tu codigo
  return()
}










