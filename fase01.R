#creando funcion con un parametro de entrada
estadistica <- function(x){
  z <- ceiling( length(x)/2)  #ceiling redondea para arriba , floor redondea hacia abajo
  tabla <- data.frame(x)
  tabla[z, "media"] <-mean(x)# va imprimir la media en la fila que le asigne
  tabla[z, "DE"] <- sd(x)
  tabla
}
estadistica(a)

#Creando funcion para experimento gravitacional

propagacion <- function(x,y){
  z <- ceiling(length(y)/2)
  tabla <- data.frame(x,y)
  tabla[, "g"] <- 4*pi^2*x/y^2
  tabla[z, "gpromedio"] <- mean(tabla$g) #calulo de la media
  tabla[z, "sigmag"] <- sd(tabla$g)/sqrt(length(y))
  tabla
  library(xtable)
  xtable(tabla)
}

l <- c(0.512, 0.597, 0.682, 0.797, 0.883)
T <- c(1.448, 1.566, 1.669, 1.804, 1.896)

propagacion(l,T)



