#================================Prueba Ji-Cuadrada===============
ji<-function(alpha,x){
  
alpha 
prueba = chisq.test(x)
a = prueba$parameter
e = prueba$statistic
e
vc = qchisq(p=alpha, df=a, lower.tail=FALSE)
vc
if(e>vc)
{
  r<-paste("Se Rechaza H0   ", "Ji^2 > EP   ",e," > ",vc)
}else{
  r<-paste("No se Rechaza H0   ", "Ji^2 < EP   ",e," < ",vc)
}
return(r)
}#Fin


#Llenado de Datos=
alfa<- 0.05          #Escriba e valor de alfa
valores<-c(18,25,26,27,26,25,20,22,23,25,25,28,22,27,20,19,31,26,27,
           25,24,21,29,28,22
           ,24,26,25,25,24) #Escriba los valores deltro del vector


ji(alfa,valores) #Imprimir el resultado






#================================Kolmogorov=========

kol<-function(v,alpha){
  miu<-mean(v)
  sig<-(var(v))^(1/2)
  #Obtenemos el tamaño
  n=length(v)
  x=sort(v)
  Fn<-ecdf(v)
  y=c(min(x)-1,x)
  
  #Busqueda del max
  D1<-D2<-0
  
  for(i in 2:(n+1))
  {
    D1[i]=abs(Fn(y[i])-pnorm(y[i],miu,sqrt(sig)))
    D2[i]=abs(Fn(y[i-1])-pnorm(y[i],miu,sqrt(sig)))
  }
  
  Dprin<-max(D1,D2)
  Dprin
  
  #Parte 2
  n<-length(v)
  m<-1000
  D<-rep(0,m)
  for (j in 1:m){
    #Simulacion de las variables uniformes
    x=runif(n,0,1)
    #Se ordena la muestra
    x=sort(x)
    #Calculamos la funcion de distribucion empirica
    Fn=ecdf(x)
    #A la muestra ordenada le agregamos el 0 al principio
    #Sirve para el caso F_n(x(0))=0
    y=c(0,x)
    #Inicializamos busqueda de supremo
    D1=0
    D2=0
    for (i in 2:(n+1)){
      D1[i]=abs(Fn(y[i])-y[i])
      D2[i]=abs(Fn(y[i-1])-y[i])
    }
    #Obtenemos maximo de maximos
    D[j]= max(D1,D2)
  }
  #Cuantiles, comparar con los cuantiles obtenidos en Conover
  com<-round(quantile(D, 1-alpha),3)
  
  if (Dprin<com)
  {
    r<-paste("No se Rechaza H0   ", "EP < W   ",Dprin," < ",com)
  }else{
    r<-paste("Se Rechaza H0    ", "EP > W   ",Dprin," > ",com)
  }
  r
  return(r)
  
  
  
  
  

}#Fin
#Ingresar datos
v<-c(0.447, 0.509, 0.563, 0.627, 0.669) 
z<-c(.435,.43,.76,.54)
alfa<-0.05
kol(z,alfa)

#================================Lilliefors=======

lilli<-function(x,alpha){
  n <- length(x)
  mu <- mean(x)
  sigma.2 <- var(x)
  z=(x-mu)/sqrt(sigma.2)
  #Ordenamos la muestra
  z=sort(z)
  #Calculamos la funcion de distribucion empirica
  Fn=ecdf(z)
  #A la muestra ordenada le agregamos un nuevo minimo al principio
  #Sirve para tener definido el caso F_n(z(0))=0
  y=c(min(z)-1,z)
  #Inicializamos busqueda de supremo
  D1=0
  D2=0
  for (i in 2:(n+1)){
    D1[i]=abs(Fn(y[i])-pnorm(y[i],0,1))
    D2[i]=abs(Fn(y[i-1])-pnorm(y[i],0,1))
  }
  D=max(D1,D2)
  D
  
  library(nortest)
  s<-lillie.test(x)
  if(s$p.value>alpha)
  {
    r<-paste("No se Rechaza H0   ", "P-Valor > Alfa   ",s$p.value," > ",alpha)
  }else{
    r<-paste("Se Rechaza H0   ", "P-Valor < Alfa   ",s$p.value," < ",alpha)
  }
  return(r)
}
x <- c(0.447, 0.509, 0.563, 0.627, 0.669)  #Ingrese datos
alpa<-0.05   #Ingrese el alfa

lilli(x,alpa)

