#Codigo para problema 2
mis_dades <- iris
mis_dades
x<-mis_dades$Petal.Length
mean(x)#media
sd(x)#distania típica
#histograma
hist(x)

y <- mis_dades$Sepal.Length
mean(y)
#grafico x,y
plot(x,y)
#sacar m
m<- sum((x-mean(x))*(y-mean(y)))/sum((x-mean(x))^2)
m
#sacar b
b<- mean(y)-m*mean(x)
b
#prediccion
m*1.5+b
####
mod<-lm(y~x)
mod
summary(mod)
#predicciones
data.frame(x=1.5)
predict(mod, data.frame(x=1.5))

data.frame(x=x)
ypred<-predict(mod, data.frame(x=x))
#grafico
plot(x,y, col="red",pch=16)
lines(x,ypred)
#recta r
Rsq<- sum((ypred-mean(y))^2)/sum((y-mean(y))^2)
Rsq

summary(mod)
