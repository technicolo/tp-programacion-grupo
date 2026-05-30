#-------------------------------------------------------------------------------
#       PROGRAMACIÓN 1 - TRABAJO PRÁCTICO - AÑO 2026
#-------------------------------------------------------------------------------
# 
# EQUIPO Nº xx:
# 
# - Galimberti, Bruno Ezequiel
# - Apellido, nombre
# - Apellido, nombre
# - Apellido, nombre
#
# ARCHIVO: jugar.R
#
# Este archivo debe ser ejecutado desde una terminal con la instrucción Rscript
# jugar.R, desde el directorio en el que esté guardado junto con cualquier otro
# archivo que sea creado como parte de la solución.
# 
# Agregar aquí cualquier comentario que consideren importante sobre la
# resolución del TP o sobre este archivo de código.
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# Paquete "farkle"
#-------------------------------------------------------------------------------

# El paquete "farkle" ofrece funciones auxiliares que simplifican muchas partes
# de la solución de este trabajo. Se recomienda leer la documentación con
# atención y correr los ejemplos.
# Las líneas de código que se encargan de la instalación del paquete no deben
# quedar en este script, pero sí la sentencia library para cargarlo.

# Instalación
# install.packages("pak")
# Sys.setlocale("LC_ALL", "English_United States.1252")
# install.packages("remotes")
# remotes::install_github("ee-unr/programacion-1/tp/farkle")
library("farkle")

#-------------------------------------------------------------------------------
# Definición de funciones propias.
# Se pueden escribir acá o en otro script .R y cargarlo con source()
# No olvidarse de documentar las nuevas funciones escritas.
# Puede ser una buena idea crear una función para cada parte del juego o para
# cada tarea que haya que realizar
#-------------------------------------------------------------------------------
#jaosjdoasjdosajdsadasdf
# COMPLETAR

##
#FUNCION CONTINUAR
continuar <- function(){
  x <- 0
  while (x != 1) {
    texto_lento("Ingresa la tecla (c) para continuar.")
    cont <- leer_palabra()
    if(cont == "c"){
      limpiar_consola()
      x <- 1
    }else{
      texto_lento("\nIngreso erróneo.\n")
    }        
  }
}

##
#FUNCION RONDA (NO HECHO)
ronda <- function(jug_pts_1, jug_pts_2, nro_ronda){
  titulo("RONDA ", nro_ronda, ancho = 100)
  cat("INFORMACION DE LA PARTIDA\n\nJugador    Puntos\n--------  -------\n", jugador1, "       ", )
}

##
#FUNCION TURNO
turno <- function(jug, pts = 0){
  x <- 0
  tiradas <- 0
  acumulado <- 0
  dados <- 5
  while (x == 0) {
    cat("==============================================\n")
    cat("Turno de ", jug, " con ", pts, "puntos")
    cat("\n==============================================\n")
    cat("\nINFORMACIÓN DEL TURNO\n\n")
    cat("Tiradas = ", tiradas, "\n")
    tiradas <- tiradas + 1 
    cat("Acumulado = ", acumulado, "\n")
    cat("Dados disponibles = ", dados, "\n\n")
    tirar <- leer_opciones("¿Tirar dados?", "Si", "No")
    if(tirar == 2){
      if(pts + acumulado > 1000){
        cat("\nSuperaste los 1000 puntos. ¡Perdiste los puntos del turno!\n")
        continuar()
        return(0)
      }
      return(acumulado)
    }
    if(tirar == 1){
      tirada <- tirar_dados(dados)
      cat("Salieron los siguientes dados: ", mostrar_dados(tirada))
      uno <- contar_dados(tirada, 1)
      cinco <- contar_dados(tirada, 5)
      tot_tira <- uno*100 + cinco*50
      
      if(tot_tira == 0){
        cat("\nNo salió ningún 1 ni 5. ¡Perdiste el turno!\n")
        continuar()
        return(0)
      }
      
      acumulado <- acumulado + tot_tira
      dados <- dados - uno - cinco
      cat("\nSacaste ", tot_tira, "puntos.\n")
      
      if(dados == 0){
        cat("\n¡Retiraste todos los dados! Volvés a tirar con 5 dados.\n")
        dados <- 5
      }
      
      continuar()
    }
  } 
  return(acumulado)
}

#-------------------------------------------------------------------------------
# Programa principal

##
#INFORMACION DEL JUEGO (LENTO)
titulo("F A R K L E", ancho = 100)
texto_lento("BIENVENIDOS\nFarkle es un juego de estrategia y gestión de riesgo en el que dos jugadores compiten por alcanzar el puntaje máximo de 1000 puntos antes que su rival.\nLa partida se desarrolla a lo largo de múltiples rondas. En cada una de estas rondas, los jugadores participan por turnos respetando un orden. Dentro de su turno, cada jugador puede lanzar los dados una o más veces, dependiendo del resultado de cada tirada.\n\nREGLAS\n - Cada jugador comienza lanzando cinco dados.\n - Los (1) suman 100 puntos, y los (5) suman 50.\n - Luego de cada tirada, el jugador decide si plantarse o seguir tirando.\n - Si el jugador sigue tirando, lanza los dados que no sumaron puntos en la tirada anterior.\n - Si en una tirada el jugador no suma puntos, pierde todo lo acumulado en ese turno.\n\nEVENTOS ESPECIALES\n - Si en una tirada todos los dados suman puntos, el jugador puede volver a tirar cinco dados.\n - Si en una tirada el jugador supera el puntaje máximo, pierde todo lo acumulado en ese turno.\n - Si ambos jugadores alcanzan el puntaje máximo en la misma ronda, se considera empate.")
##

##
#NOMBRES DE LOS PARTICIPANTES (LENTO)
texto_lento("\nAntes de comenzar, ingrese los nombres de los jugadores.")
cat("\nNombre del jugador 1: ")
jugador1 <- leer_palabra()
cat("Nombre del jugador 2: ")
jugador2 <- leer_palabra()

##
#CONFIRMACIÓN DEL ENFRENTAMIENTO
texto_lento("\n¡Perfecto!\nVan a jugar ", jugador1, " contra ", jugador2, ".\n", sep = "")
texto_lento("¿Están listos?")
continuar()
pts1 <- 0
pts2 <- 0

while (pts1 < 1000 && pts2 < 1000) {
  pts1 <- pts1 + turno(jugador1, pts1)
  
  if (pts1 >= 1000) {
    pts2 <- pts2 + turno(jugador2, pts2)
    break
  }
  
  pts2 <- pts2 + turno(jugador2, pts2)
}

if (pts1 == 1000 && pts2 == 1000) {
  titulo("EMPATE")
  cat("¡Ambos jugadores llegaron a 1000 puntos en la misma ronda!\n")
} else if (pts1 == 1000) {
  titulo("¡GANADOR!")
  cat("¡Felicitaciones", jugador1, "ganaste la partida!\n")
} else {
  titulo("¡GANADOR!")
  cat("¡Felicitaciones", jugador2, "ganaste la partida!\n")
}

pausa()

#-------------------------------------------------------------------------------

# COMPLETAR