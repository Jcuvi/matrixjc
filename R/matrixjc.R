# PAQUETE QUE GENERA UN MENU PARA MATRICES
# POR JOEL CUVI
# FECHA: 5/02/2025

crear_matrix <- function(Nf, Nc) {
  M1 <- matrix(NA, nrow = Nf, ncol = Nc)
  return(M1)
}

ingreso_param <- function() {
  cat("Ingrese el numero de filas: \n")
  Nf <- as.numeric(readLines(n = 1))
  cat("Ingrese el numero de columnas: \n")
  Nc <- as.numeric(readLines(n = 1))
  return(list(Nf = Nf, Nc = Nc))
}

val_matriz <- function(M1, Nf, Nc) {
  for (i in 1:Nf) {
    for (j in 1:Nc) {
      cat("Ingrese el elemento[", i, ",", j, "]: \n")
      M1[i, j] <- as.numeric(readLines(n = 1))
    }
  }
  return(M1)
}

cant_par_impar <- function(M1, Nf, Nc) {
  p = 0
  imp = 0
  for (i in 1:Nf) {
    for (j in 1:Nc) {
      if (M1[i, j] %% 2 == 0) {
        p <- p + 1
      } else {
        imp <- imp + 1
      }
    }
  }
  return(list(p = p, imp = imp))
}

val_par_vect <- function(M1, Nf, Nc) {
  v1 <- c()
  v2 <- c()
  k <- 1
  q <- 1
  for (i in 1:Nf) {
    for (j in 1:Nc) {
      if (M1[i, j] %% 2 == 0) {
        v1[k] <- M1[i, j]
        k <- k + 1
      } else {
        v2[q] <- M1[i, j]
        q <- q + 1
      }
    }
  }
  return(list(v1 = v1, v2 = v2))
}

menu <- function() {
  cat("MENU PRINCIPAL \n")
  cat("1. Visualizar matriz \n")
  cat("2. Cuantos pares e impares \n")
  cat("3. Valores pares e impares \n")
  cat("4. Salir \n")
  cat("Escoja una opcion: \n")
  Op1 <- as.numeric(readLines(n = 1))
  return(Op1)
}

menu5 <- function() {
  F <- ingreso_param()
  Nf1 <- F$Nf
  Nc1 <- F$Nc
  M <- crear_matrix(Nf1, Nc1)
  Mat1 <- val_matriz(M, Nf1, Nc1)
  # Cambio a numérica los valores de la matriz
  Mat2 <- matrix(as.numeric(Mat1), nrow = nrow(Mat1), ncol = ncol(Mat1))
  ban = 0
  while (ban == 0) {
    Op1 <- menu()

    switch(Op1,
           "1" = {
             cat("Visualizar la matriz ingresada: \n")
             print(Mat1)
             print(Mat2)
           },
           "2" = {
             cat("Existen en la matriz: \n")
             c1 <- cant_par_impar(Mat2, Nf1, Nc1)
             cat("Pares =", c1$p, "\n")
             cat("Impares =", c1$imp, "\n")
           },
           "3" = {
             cat("Valores pares e impares de la matriz: \n")
             L1vect <- val_par_vect(Mat2, Nf1, Nc1)
             cat("Vector 1 con pares: \n")
             print(L1vect$v1)
             cat("Vector 2 con impares: \n")
             print(L1vect$v2)
           },
           "4" = {
             cat("Saliendo del sistema.....\n")
             ban = 1
           },
           {
             cat("Error, digite bien la opcion \n")
           }
    )
  }
}
