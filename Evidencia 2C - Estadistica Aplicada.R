#Evidencia #2 - Luis Angel Mendoza Gonzalez - 20210303

install.packages("TSA")
library(TSA)
library(tseries)

#Ejercicio 1

data(nottem)
nottem
plot(nottem)

#Estacionaridad en Media
summary(lm(nottem~time(nottem)))
#Es estacionaria en media, no presenta una tendencia

#Estacionaridad en Varianza
adf.test(nottem,alternative = "stationary")
# P-value = 0.01 - La seria es estacionaria en varianza


data(hours)
hours
plot(hours)

#Estacionaridad en Media
summary(lm(hours~time(hours)))
#No es estacionaria en media, presenta una tendencia

#Estacionaridad en Varianza
adf.test(hours,alternative = "stationary")
#P-value = 0.2156 - La seria es no estacionaria en varianza

#Ejercicio 2 - Modelos

#Modelo AR (25)

Valor<-AIC(arima(nottem,order=c(25,0,0),method = "ML"))
Valor

#Modelo MA
Model1<-Inf
for (i in 1:22)
{
  Model2<-AIC(arima(nottem, order = c(0,0,i), method = "ML"))
  if(Model2 < Model1)
  {
    Model1<-Model2
    ModelMA.Mejor<-i
  }
}
ModelMA.Mejor #El orden del mejor MA
Model2 #El mejor AIC 

#Modelo ARMA
M.ARMA <- AIC(arima(nottem,order=c(11,0,9),method = "ML"))
M.ARMA

#Grafica de Modelos

plot(nottem,xlim=c(1940,1944),main = "Modelos obtenidos")
lines(predict(arima(nottem,order=c(0,0,20),method = "ML"),n.ahead=50)$pred,col="yellow")
lines(predict(arima(nottem,order=c(25,0,0),method = "ML"),n.ahead=50)$pred,col="red")
lines(predict(arima(nottem,order=c(11,0,9),method = "ML"),n.ahead=50)$pred,col="blue")
legend(1941,66,legend = c("AR"),fill = c("yellow"))
legend(1942,66,legend = c("MA"),fill = c("red"))
legend(1943,66,legend = c("ARMA"),fill = c("blue"))

#Grafica de las predicciones

help(plot)
plot(nottem,xlim=c(1940,1945),main = "50 Predicciones de la serie de Tiempo: Nottem")
lines(predict(arima(nottem,order=c(11,0,9),method = "ML"),n.ahead=50)$pred,col="blue")
Predicciones <- predict(arima(nottem,order=c(11,0,9),method = "ML"),n.ahead=50)$pred
Predicciones

#3 a
#Modelo Lineal
summary(lm(hours~time(hours))) #Tendencia Lineal
a <- lm(hours~time(hours))
#Modelo Polinomico

b<-lm(hours~poly(to,degree = 3)) #revisando polinomio, (lm(y contra el grado de polinomio))
summary(b) #modelo significativo r2=66.91% y pvalor pequeño

#Modelo Logaritmico
diff(hours)
correc1 <- diff(log(hours))
correc1
summary(lm(correc1~time(correc1)))

plot(correc1,type="p")

#Graficos de Dispersion

a <- lm(hours~time(hours))
diff(hours)
correc1 <- diff(log(hours))
b<-lm(hours~poly(to,degree = 3))

plot(hours,type = "p")
abline(a,col="green")
legend(1986,41.5,legend = c("Lineal"),fill = c("green"))
abline(correc1,col="blue")
abline(b,col="yellow")


