#declarar un vector
y<-c(1,2,3,4)
x<-c("a,b,c,d") 
###vector vacio
X<-c()

###declarar un funcion
cuadrado<-function(x){
  cuadrado=(x*x)+5
  return(cuadrado)
}
cuadrado(4)

y<-runif(100)
#tomar la entrada 1
y[1]
#tomar de la entrada 2 ala 5
y[2:5]

simula_exp<-function(lambda){
  u<-runif(1)
  x=-(1/lambda)*log(u)
  return(x)
}
#simular el valor 3
simula_exp(3)

replicate(100,simula_exp(4))

simula_exp2<-function(n,lambda){
  #declaramos un vector vacio que vamos a llamar 
  x<-c()
  #necesitamosn n valores uniformes o,1
  y<-runif(n)
  #ciclo
  for(i in 1:n){
    
    x[i]=-(1/lambda)*log(y[1])
    
  }
  
  return(x)
  
  
}
z<-simula_exp2(50,2)
##esto es un valor de la distribucion gamma de parametros (50,2)
sum(z[1:50])

mean(simula_exp2(50000,4))

simula_gamma<-function(n,lambda){
  ##declaramos un vector vacio que vamos a llamar 
  x<-c()
  ##necesitamosn n valores uniformes o,1
  y<-runif(n)
  ###ciclo
  for(i in 1:n){
    
    X[i]=-(1/lambda)*log(y[i])
    
    
  }
  return(sum(x[1:n]))
}
simula_gamma(200,4)




simula_Bernoulli<-function(p){
  u=runif(1)
  if(u<p){
    x=0
  }
  else{
    x=1
  } 
  return(x)
}

simula_Bernoulli(1/6)

replicate(100,simula_Bernoulli(1/4))

mean(replicate(100, simula_Bernoulli(1/4)))

simula_Binomial<-function(n,p){
  x<-c()
  for (i in 1:n) {
    u=runif(1)
    if(u>p){
    x[i]=0
  }
  else{
    x[i]=1
  }
  }
  
  return(sum(x[1:n]))
}

simula_Binomial(100,.9)

replicate(500,simula_Binomial(100,.9))


