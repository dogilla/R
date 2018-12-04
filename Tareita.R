#Probabilidad 1
#Integrantes:
#--Guzman Mosco Mario Alexis
#--Hernández Pozos Moisés
#--Ramos Calpulalpan Karem
#--Romero Pérez Minerva María

#simula valores de la variable Hipergeometrica
simulaHiperGeom <- function(eneg, ene, eme){
  x<-c()
  u<-runif(1)
  #elementos de categoria A
  a <- eme
  #probabilidad de sacar un elemento de la categoria A
  pa <- a/eneg
  #tomar elementos de 1 a n
  for(i in 1:ene){
    if(u<pa){
      x[i] = 1
      #si tomamos un elemento de A lo sacamos porque es sin reemplazo
      a <- a-1
      #volvemos a calcular la proba de sacar A 
      eneg <- eneg -1
      pa <- a/eneg
    }else{
      #sacamos el elemento B de la poblacion
      eneg <- eneg-1
      #tambien recalculamos la proba de sacar A
      pa <- a/eneg 
      #no contamos nada pues fue fracaso
      x[i] = 0
    }
  }
  #sumamos los exitos
  return(sum(x[1:ene]))
}

#simula valores de la variable Gamma
simulaGamma <- function(ene,lambda){
  x<-c()
  y<-runif(ene)
  for(i in 1:ene){
    #funcion inversa de la funcion de distribucion exponencial
    x[i]=-(1/lambda)*log(y[i])
  }
  #la suma de exponenciales es gamma
  return(sum(x[1:ene]))
}

#simula valores de la variable Poisson.
#algoritmo de Knuth para generar valores Poisson a partir de la funcion exponencial
simulaPoisson <- function(lambda){
  #No basamos en el hecho de que el tiempo de llegada entre poissones es exponencial
  l <- exp(-(lambda))
  p <- 1
  k <- 0
  #como en R no exite do while, simulamos con repeat
  repeat {
    #contamos las poissones
    k <- k+1
    u <- runif(1)
    p <- p*u
    #negamos la condicion que deberia cumplirse en un while normal
    if(!(p>l)){
      break
    }
  }
  return(k-1)
}

#simula valores de la variable Binomial Negativa
simulaBinNeg <- function(ene, pe){
  x<-c()
  for (i in 1:ene){
    u=runif(1)
    #se cuenta un fracaso
    if(u>pe){
      x[i]=1
    #En este caso, hubo un exito, y el conteo termina
    }else{
      x[i]=0
      return(sum(x[1:i]))
    }
  }
}

leeParam <- function(){
  n <- as.integer(readline(prompt="Escribe cuantos valores quieres generar: "))
  p <- as.integer(readline(prompt="Escribe el valor λ para la distribucion Poisson: "))
  h1 <- as.integer(readline(prompt="Escribe el tamaño de la poblacion (N) para la Hipergeometrica: "))
  h2 <- as.integer(readline(prompt="Escribe el tamaño de la muestra (n) para la distribucion Hipergeometrica: ")) 
  h3 <- as.integer(readline(prompt="Escribe el numero de elementos que son del primer tipo (m) Hipergeometrica: ")) 
  g1 <- as.double(readline(prompt="Escribe el valor n para la distribucion Gamma: ")) 
  g2 <- as.double(readline(prompt="Escribe el valor λ para la distribucion Gamma: "))
  b1 <- as.integer(readline(prompt="Escribe el numero de ensayos (n) para la distribucion B. Negativa: "))
  b2 <- as.double(readline(prompt="Escribe la probabilidad de exito (p) para la distribucion B. Negativa: "))
  a <- 0
  print("Poisson: ")
  while(a<n){
    print(simulaPoisson(p))
    a<-a+1
  }
  a <- 0
  print("Hipergeomerica: ")
  while(a<n){
    print(simulaHiperGeom(h1,h2,h3))
    a<-a+1
  }
  a <- 0
  print("Gamma: ")
  while(a<n){
    print(simulaGamma(g1,g2))
    a<-a+1
  }
  a <- 0
  print("Binomial Negativa: ")
  while(a<n){
    print(simulaBinNeg(b1,b2))
    a<-a+1
  }
}

leeParam()

