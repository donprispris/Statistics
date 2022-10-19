

# Esteban Cardenas 179150
#Proyecto Final Estadistica


#Librerias
library(ggplot2)
library(lubridate)
install.packages("dplyr")
library("dplyr")
#Carga de datos

foodDrinks = read.csv("FoodDrinks.csv")
ticketsf= read.csv("Tickets.csv")
blockbuster=read.csv("Blockbusters.csv")

#depuramos los tipos de datos

ticketsf$week <- ymd(ticketsf$week)
blockbuster$week <- ymd(blockbuster$week)
foodDrinks$week <- ymd(foodDrinks$week)

#añadimos los meses en venta de tickets
ticketsf$mes <- as.numeric(format(ticketsf$week,'%m'))
ticketsf$ano <- as.numeric(format(ticketsf$week,'%y'))


#----El comportamiento de las ventas de boletos [30 puntos]------
# A) Grafiquen la venta de tickets distinguiendo entre las regiones y a nivel mensual. 
#    Comenten sobre lo que se observa en la o las gráficas. Noten que existen varios casos atípicos.
#    Propongan una hipótesis sobre la causa que los genera y justifiquen el razonamiento.


# creamos el a dataset
data=data.frame("Area"= rep(c(1,2,3,4),12), "Meses"=c(rep(c("Enero"),4),rep(c("Febrero"),4),rep(c("Marzo"),4),rep(c("Abril"),4),rep(c("Mayo"),4),
                                                    rep(c("Junio"),4),rep(c("Julio"),4),rep(c("Agosto"),4),rep(c("Septiembre"),4),rep(c("Octubre"),4),
                                                    rep(c("Noviembre"),4),rep(c("Diciembre"),4)), "Frecuencia"=c(rep(0,48)))
#sacamos su frecuencia
count=1
n=1
for (i in data$Area){
  #sacamos el set que nos interesa
  if(i != 4){
    retval <- subset(ticketsf,region==i & mes == count)
    data[n,3]<-sum(retval$tickets)

    n=n+1
  }
  else{
    count=count+1
    data[n,3]<-sum(retval$tickets)

    n=n+1
  }
  
}

ggplot(data, aes(x = Area, y = Frecuencia, fill = Meses)) +
  geom_col()




#B)Los datos con los que se cuenta «acaban» en julio de 2019. 
#¿Es razonable pensar que la venta de boletos en taquilla para el mes de agosto de 2019 será similar entre regiones?
#Para responder esta pregunta, calculen, para cada región,
#la probabilidad de que las ventas de taquilla superen los 4.5 millones de boletos en dicho mes para cada región. 
#Comparen los resultados y comenten.

#frecuencia de ventas de boletos en agosto para cada region

#x-año
#y-frecuencia

#se aplicara un algorimo para calcular todos los meses
#n numero de datos, media y desviacion estandard
#mes
m=1
c(unique(ticketsf[6]))

area1=data.frame("Años"=c(2001:2031),"Frecuencia de Agosto"=rep(0,31))
area2=data.frame("Años"=c(2001:2031),"Frecuencia de Agosto"=rep(0,31))
area3=data.frame("Años"=c(2001:2031),"Frecuencia de Agosto"=rep(0,31))
area4=data.frame("Años"=c(2001:2031),"Frecuencia de Agosto"=rep(0,31))

itera=c(1:31)


n=31
#para area 1
for (i in itera){
  retval <- subset(ticketsf,region==1 & mes == 8 & ano==i)
  area1[i,2]<-sum(retval$tickets)
}

#media de area1
media1=mean(area1$Frecuencia.de.Agosto)/1000
#desviacion estandar area1
desv1=sqrt(var(area1$Frecuencia.de.Agosto))/1000


#para area 2
for (i in itera){
  retval <- subset(ticketsf,region==2 & mes == 8 & ano==i)
  area2[i,2]<-sum(retval$tickets)
}


#media de area2
media2=mean(area2$Frecuencia.de.Agosto)/1000
#desviacion estandar area2
desv2=sqrt(var(area2$Frecuencia.de.Agosto))/1000

#para area 3
for (i in itera){
  retval <- subset(ticketsf,region==3 & mes == 8 & ano==i)
  area3[i,2]<-sum(retval$tickets)
}
#media de area3
media3=mean(area3$Frecuencia.de.Agosto)/1000
#desviacion estandar area1
desv3=sqrt(var(area3$Frecuencia.de.Agosto))/1000
#para area 4
for (i in itera){
  retval <- subset(ticketsf,region==4 & mes == 8 & ano==i)
  area4[i,2]<-sum(retval$tickets)
}
#media de area4
media4=mean(area4$Frecuencia.de.Agosto)/1000
#desviacion estandar area1
desv4=sqrt(var(area4$Frecuencia.de.Agosto))/1000

#grafica de todas las distribuciones normales


#Create a sequence of 1000 x values based on population mean and standard deviation
x <- seq(-1000, 2000, length = 1000) 

plot(x, dnorm(x,media2, desv2), type="l",xlab="x", ylab="Densidad de Probabilidad")
lines(x, dnorm(x,media3,desv3), col="red")
lines(x, dnorm(x,media4,desv4), col="blue")
lines(x, dnorm(x,media1,desv1), col="green")

title(main = "Distribucion normal de las ventas de tickets en Agosto por Area")

# Add a legend
legend(1500, 0.001, legend=c("Area 1", "Area 2","Area 3","Area 4"),
       col=c("green","black", "red","blue"), lty=1:2, cex=0.8)
abline(v = media1,                       # Add line for mean
       col = "green",
       lwd = 1)
abline(v = media2,                       # Add line for mean
       col = "black",
       lwd = 1)
abline(v = media3,                       # Add line for mean
       col = "red",
       lwd = 1)
abline(v = media4,                       # Add line for mean
       col = "blue",
       lwd = 1)


da=data.frame("Area"=c("Area 1", "Area 2","Area 3","Area 4"),"Media"=c(media1*1000,media2*1000,media3*1000,media4*1000)
              ,"Desviacion std"=c(desv1*1000,desv2*1000,desv3*1000,desv4*1000),
              "P(x>500)"=c(1-pnorm(400,mean=media1,sd=desv1),1-pnorm(400,mean=media2,sd=desv2),1-pnorm(400,mean=media3,sd=desv3),1-pnorm(400,mean=media4,sd=desv4)))


# c) Uno de los criterios de la compañía es que si la venta de boletos a nivel nacional y en una semana dada es superior a los 4 millones de boletos,
#entonces se considera lo que la compañía nombra una semana outstanding.
#Además, uno de los principales indicadores para evaluar el desempeño de la compañía es el «momio» (en inglés, odds) de obtener una semana outstanding.

#primero calculamos od de cada semana
meanTikets=mean(ticketsf$tickets)
stdTickets=sqrt(var(ticketsf$tickets))


ticketsf$probabilidad <- as.numeric(pnorm(ticketsf$tickets,mean=meanTikets,sd=stdTickets))

ticketsf$odds <- as.numeric(ticketsf$probabilidad/(1-ticketsf$probabilidad))


#hacemos una frecuencia de odds
oddDta=data.frame("odds"=c(rep(c(.1,.2,.3,.4,.5,.6,.7,.8,.9),11)))
aux=data.frame("odds"=c(rep(0,9),rep(1,9),rep(2,9),rep(3,9),rep(4,9),rep(5,9),rep(6,9),rep(7,9),rep(8,9),rep(9,9),rep(10,9)))
oddD=oddDta +aux

oddD$Frecuencia<-c(rep(0,99))



#saca frecuencias
co=1
for (i in oddD$odds){
  retval2 <- subset(ticketsf,odds<=i & odds > (i-.1))
  oddD[co,2]<-length(retval2$odds)
  co=co+1
}




oddD$FrecuenciaAcumulada<-cumsum(oddD$Frecuencia)

FrecuenciaAcumulada<-oddD$FrecuenciaAcumulada
Odds<-oddD$odds
plot(Odds,FrecuenciaAcumulada)

title(main = "Frecuencia  acumulada de los odds de ventas por semana")
points(Odds[6],FrecuenciaAcumulada[6],pch=4,col="red",cex=2)
text(Odds[6], FrecuenciaAcumulada[6], labels="             Q2")

points(Odds[3],FrecuenciaAcumulada[3],pch=4,col="red",cex=2)
text(Odds[3], FrecuenciaAcumulada[3], labels="             Q1")

points(Odds[15],FrecuenciaAcumulada[15],pch=4,col="red",cex=2)
text(Odds[15], FrecuenciaAcumulada[15], labels="             Q3")

points(Odds[99],FrecuenciaAcumulada[99],pch=4,col="red",cex=2)
text(Odds[99], FrecuenciaAcumulada[99], labels="    Q4")
y<-oddD$Frecuencia
x<-oddD$odds


#Ajustar el modelo
model <- lm(y ~ log(x))

#resumen del modelo
summary(model)


Odds<-oddD$odds
Frecuencia<-oddD$Frecuencia
#plot x vs. y
plot(Odds,Frecuencia)


title(main = "Frecuencia de los odds de ventas por semana")
#define x-values to use for regression line
x=seq(from=1,to=15,length.out=1000)

#use the model to predict the y-values based on the x-values
y=predict(model,newdata=list(x=seq(from=1,to=15,length.out=1000)),
          interval="confidence")

#add the fitted regression line to the plot (lwd specifies the width of the line)
matlines(x,y, lwd=2)



#----El comportamiento cuando se presentan blockbusters [20 puntos]-------

#a)La compañía considera que una película es un blockbuster si ésta recaudó,durante todo el tiempo que estuvo en cartelera, más de 15 millones de dólares.
#Considerando la información a total país, y separando la información por semana blockbuster y semana regular, hagan un diagrama de caja y brazos para determinar si asiste,
#en promedio, más gente al cine cuando hay un blockbuster en cartelera que cuando no lo hay. Interpreten la gráfica y comenten.



#semanas con block buster
bluck<-subset(blockbuster,blockbuster==1)

#semanas sin block buster
nobluck<-subset(blockbuster,blockbuster==0)  


bluck$mes<-as.numeric(format(bluck$week,'%m'))
bluck$ano<-as.numeric(format(bluck$week,'%y'))


nobluck$mes<-as.numeric(format(nobluck$week,'%m'))
nobluck$ano<-as.numeric(format(nobluck$week,'%y'))

bluck$Frecuencia<-rep(0,length(bluck$week))
nobluck$Frecuencia<-rep(0,length(nobluck$week))
#añadimos las frecuencias de esas semanas

#como no tenemos todas las frecuencias de las semanas, usamos fechas cercanas o si no encuentra una cercana al mes pondra una media


for(i in 1:length(nobluck$week)){
  for(j in i:length(ticketsf$week)){
    if(nobluck[i,3]==ticketsf[j,5] && nobluck[i,4]==ticketsf[j,6]){
      nobluck[i,5]<-ticketsf[j,4]
    }
  }
}


for(i in 1:length(bluck$week)){
  for(j in i:length(ticketsf$week)){
    if(bluck[i,3]==ticketsf[j,5] && bluck[i,4]==ticketsf[j,6]){
      bluck[i,5]<-ticketsf[j,4]
    }
  }
}


#Ahora aplicamos Bootstrap a los valores que no pudo llenar
#sacamos los sets que usaremos como bootstrap

setBluck<-c(unique(bluck$Frecuencia))
setNoBluck<-c(unique(nobluck$Frecuencia))

for(i in 1:length(nobluck$week)){
  if (nobluck[i,5]==0){
    nobluck[i,5]=sample(setNoBluck,1)
  }
}

for(i in 1:length(bluck$week)){
  if (bluck[i,5]==0){
    bluck[i,5]=sample(setBluck,1)
  }
}

dat<-data.frame("Block Buster"=bluck$Frecuencia,"No Block Buster"=nobluck$Frecuencia)

boxplot(bluck$Frecuencia,nobluck$Frecuencia, horizontal = TRUE,names=c("Block Buster", "No Block Buster"),col=c("red","blue"))
title(main="Diagrama caja brazos de ventas de boletos entre peliculas blockbuster y no blockbuster")


#2B) Para indagar un poco más al respecto,
#obtengan un intervalo de confianza para determinar si es posible asegurar que las semanas blockbuster 
#venden más en dulcería que las semanas regulares. Comenten los resultados
#y concluyan.


#sacamos los intervalos de confianza para una muestra normal
aBluck <- mean(bluck$Frecuencia)
sBluck <- sqrt(var(bluck$Frecuencia))
nBluck <- length(bluck$week)
error <- qnorm(0.8)*sBluck/sqrt(nBluck)
left <- aBluck-error
right <- aBluck+error

anoBluck <- mean(nobluck$Frecuencia)
snoBluck <- sqrt(var(nobluck$Frecuencia))
nnoBluck <- length(nobluck$week)
noerror <- qnorm(0.8)*snoBluck/sqrt(nnoBluck)
noleft <- anoBluck-noerror
noright <- anoBluck+noerror

#los ponemos en un data frame
ic<-data.frame("venta"=c("block","noblock"),"-z"=c(left,noleft),"z"=c(right,noright))


#--------------------------El comportamiento de las ventas en dulcería [20 puntos]---------------------------------------
#La venta de dulcería representa más del 65% del total de ingresos de la compañía, por lo que es una de las áreas más relevantes para la empresa.
#Ahora, consideren que los datos de las ventas en dulcería se tienen tanto en valor (ganancia en dólares) como en unidades (número de productos vendidos). 
#Grafiquen ambas métricas de ventas en el tiempo y para cada tipo de familia. Observen el comportamiento en cada una y comenten: ¿se observa un patrón similar en ambos casos y, si es el caso, en qué difieren?
#A partir de las ventas en valor y las ventas en unidades, ¿cómo se puede obtener una nueva variable que refleje el precio promedio (semanal) de cada familia? 
#  Grafiquen la variable del punto anterior a lo largo del tiempo y comenten.


#este analisis se podria replicar para todas las areas pero solo lo aplicaremos a una
#que en este caso sera area 1

dat<-subset(foodDrinks,foodDrinks$region==1 )
dat2<-subset(dat,select = -c(region,quantity))
dat3<-subset(dat,select = -c(region,sales))



ggplot(dat2, aes(x = week, y = sales, fill = family)) +
  geom_col()+scale_x_date(date_labels = "%Y")
title(main = "Ventas por semana de Area 1")

gg<-ggplot(dat3, aes(x = week, y = quantity, fill = family)) +
  geom_col()+scale_x_date(date_labels = "%Y")
title(main = "Cantidad de articulos vendidos por semana de Area 1")


#Durante el mes de septiembre de 2018, la empresa decidó implementar una estrategia comercial que consistió en modificar los precios de la oferta de productos de dulcería.
#No obstante, algunos directores consideraron que dichos cambios en el precio no fueron estadísticamente significativos. Para validar lo anterior, calculen un intervalo de confianza para el nivel
#de precio antes de septiembre de 2018 y otro para el nivel a partir de dicho mes. ¿Es cierto que no existe una diferencia estadísticamente significativa en términos de dicha estrategia? Comparen los resultados entre las familias y comenten.


#para poder calcular un intervalo de confianza debemos seleccionar 2 muestras
#la muestra uno sera n=20 hasta septiembre 2018
au<-subset(foodDrinks,foodDrinks$week <=as.Date("2017-09-30"),select = -c(region,quantity,week,family))
set1<-sample_n(au,30)

#sacamos los intervalos de confianza para una muestra normal
mean <- mean(set1$sales)
std <- sqrt(var(set1$sales))
n <- length(set1$sales)
error <- qnorm(0.8)*std/sqrt(n)
left <- mean-error
right <- mean+error

#la segunda muestra sera de n=20 desde septiembre 2018

au2<-subset(foodDrinks,foodDrinks$week >as.Date("2017-09-30"),select = -c(region,quantity,week,family))
set2<-sample_n(au,30)

#sacamos los intervalos de confianza para una muestra normal
mean2 <- mean(set2$sales)
std2 <- sqrt(var(set2$sales))
n2 <- length(set2$sales)
error2 <- qnorm(0.8)*std/sqrt(n2)
left2 <- mean2-error
right2 <- mean2+error

#lo ponemos en un data frame

dam<-data.frame("Fecha"=c("antes","despues"),"-z"=c(left,left2),"z"=c(right,right2))
