
###LIBRERIA  
library(tseries)
library(forecast)
library(readxl)
library(RcmdrMisc)
library(ggplot2)
library(ggfortify)
library(dplyr)
library(TSstudio)
library(xts)
library(TSA)
library(lmtest)
library(car)
library(FinTS)
library(ggfortify)
library(urca)


#####BASE DATOS  


#nueva base de datos 2013 enero  a 2024 JULIO mes 7
cordoba <- read_excel("precioleche-promedio-cordoba.xlsx")
View(cordoba)
str(cordoba)   


#
print(cordoba)
yt2<- ts(cordoba$precioleche, start = c(2013, 1), end = c(2024, 7) , frequency = 12)
yt2
end(yt2)
autoplot(yt2) 
yt.c=tsclean(yt2)

###graficos  
######
Percentc=cordoba %>% 
  group_by(as.factor(cordoba$Año)) %>% 
  do(data.frame(t(quantile(.$precioleche, probs = c(0.2, .5, .80)))))
Percentc

plot(log(Percentc$X50.), log(Percentc$X80.-Percentc$X20.), xlab='Nivel', ylab='Dispersión', col='red')


modelo <- lm(log(Percentc$X80. - Percentc$X20.) ~ log(Percentc$X50.)) 
abline(modelo, col='blue')
### Tendencia 


numSummary(cordoba[,'precioleche'],groups=cordoba$Año,
           statistic=c('mean','sd','cv','quantiles'), quantiles=c(0.25,0.5,0.75))


ts_plot(yt.c,
        title = "Precios mensuales de leche cruda en finca ",
        Ytitle = "Precio promedio por litro",
        Xtitle = "Fuente: DANE",
        slider = TRUE,
        line.mode =  "lines+markers")


ts_plot(yt.c,
        title = "Precios mensuales de leche cruda en finca ",
        Ytitle = "Precio promedio por litro",
        Xtitle = "Fuente: DANE",
        slider = TRUE,)

ts_plot(yt.c,
        title = "Precios mensuales de leche cruda en finca ",
        Ytitle = "Precio promedio por litro",
        Xtitle = "Fuente: DANE",
        slider = F,)

### Estacionalidad 

ts_seasonal(yt.c, type = "normal", title = "Grafico de perfiles Precio de la leche cruda ", Ygrid = TRUE, Xgrid = TRUE, last = NULL, palette = "Set1", palette_normal = "viridis")










#analisis preliminar


adf.test(yt.c)
kpss.test(yt.c)
#no estacionaria 


ts_cor(yt.c)

###FASE 1------------------------------------------------------------------------

#Diferenciacion 
yt.d1=diff(yt.c,d=1)
plot(yt.d1)
yt.d1 
ts_plot(yt.d1) 


plot(yt.c)    ###limpia

###ver si la serie es estacionaria difencidad
### adf.test
adf.test(yt.baa) 





#opcion 2
yt.d2=diff(yt.d1,d=1)
plot(yt.d2)
  
plot(yt.c)  # observar grafico que no son constante 



##### CONTROL de la varianza y la media  : tiene que ser constates
d=ndiffs(yt.c)
d
 
lambda=BoxCox.lambda(yt.c)
lambda



yt.bc=BoxCox(yt.c,lambda)
yt.bc
plot(yt.bc)


##### Prueba para Estacionalidad ADF # No es estacionaria 
adf.test(yt.bc)


da=ndiffs(yt.bc)
da

yt.baa=diff(yt.bc,d=1)
yt.baa

plot(yt.baa) 
ts_plot(yt.baa)

#### Prueba para Estacionalidad ADF #YA LA SERIES ES ESTACIONARIA DIFRENCIADA
adf.test(yt.baa)





#Dividimos nuestra serie transformada en datos de entrenamiento y datos de prueba 
h=12
datos=ts_split(yt.bc, sample.out = h)
train=datos$train; train     # entranamiento 
test=datos$test ;            # prueba
test  
acf(train) 

#verificamos cuantas veces hay que diferenciar 


d=ndiffs(train); d
y.d1=diff(train,differences = d) 

acf(y.d1) #### acf
ts_cor(y.d1, lag.max = 36) 
ts_plot(y.d1)
orden=eacf(y.d1,ar.max =15,ma.max=15) #15
matrix=orden$eacf

#orden=eacf(train,ar.max =15,ma.max=15) #no pa
#orden$eacf



###FASE 2------------------------------------------------------------------------
#





auto=auto.arima(train) #### forma automatica 
auto
#modelo 1 modelo candidato 
####ml

M11=Arima(train,order = c(3,1,2),method = "ML",include.drift = T,include.constant = T)
M11
coeftest(M11)
summary(M11)




##CSS
M11.2=Arima(train,order = c(3,1,2),method = "CSS",include.drift = T,include.constant = F)
M11.2
coeftest(M11.2)
summary(M11.2)

##cSS-ML

M11.3=Arima(train,order = c(3,1,2),method = "CSS-ML",include.drift = T,include.constant = T)
M11.3
coeftest(M11.3)
summary(M11.3)


# DADO QUE EL MODELO CON TODAS LAS POSIBLES COMBINACIONES CON CONSTATE SIN CONSTE 
#NO DAN SIGNIFICATIVAS REDIFINIMOS EL POSIBLE MODELO

#reajuste del modelo

#PRINCIPIO parsimonioso

##modelo2 (2,1,2)
##ml

M2=Arima(train,order = c(2,1,2),method = "ML",include.drift = T,include.constant = F)
M2
coeftest(M2)
summary(M2)
##CSS

M2.2=Arima(train,order = c(2,1,2),method = "CSS",include.drift = T,include.constant = F)
M2.2 
coeftest(M2.2) ### BUENN MODELO DE ESTE  
summary(M2.2) 


## CSS-ML 
M2.3=Arima(train,order = c(2,1,2),method = "CSS-ML",include.drift = T,include.constant = F)
M2.3
coeftest(M2.3)

summary(M2.3) 




##modelo3  ##
##ml

M3=Arima(train,order = c(0,1,2),method = "ML",include.drift = T,include.constant = F)
M3
coeftest(M3)
summary(M3)
##CSS

M3.2=Arima(train,order = c(0,1,2),method = "CSS",include.drift = T,include.constant = F) ######MODELO RE BUENO
##rmse mae varianza log  
M3.2
coeftest(M3.2)
summary(M3.2)
## CSS-ML 
M3.3=Arima(train,order = c(0,1,2),method = "CSS-ML",include.drift = T,include.constant = F)
M3.3
coeftest(M3.3)
summary(M3.3)
###?Arima

 
#m2.2 css
#m3.2 css


summary(M2.2)
summary(M3.2)


#

####FASE-3-EVALUACION-Y-DIAGNOSTICOS -DE -LOS -2 -MODELO -FINAL-ARIMA--------------------------------------------
  





##modelo 2-------------------------------------------------------------
## Suma de los coeficientes diferente de 1
coef(M2.2)
A=sum(M2.2$coef[1:4])
A 





###EVALUACION DEL MODELO 2

M2.2$residuals


## Media de los Residuos: One Sample t-test
t.test(residuals(M2.2))  
###se rechaza la h0 la media de los residuales es diferente de 0.




## Normalidad de los Residuos: Kolmogorov-Smirnov test 
ks.test(residuals(M2.2), "pnorm")  ### no son normales


par(mfrow = c(1, 2))
qqnorm(residuals(M2.2))
qqline(residuals(M2.2))
hist(residuals(M2.2),ylim = c(0, 0.4),  breaks = 10, freq = F, main = "Histogram of resifuals", xlab = "")
lines(density(residuals(M2.2)), col = "red", lwd = "2")
par(mfrow = c(1, 2))


#### Homocedasticidad de los Residuos: ARCH LM-test
ArchTest(residuals(M2.2))  
#los residuos parecen tener una varianza constante en el tiempo.




#### Independencia de los Residuos: Ljung-Box 


log(length(M2.2$x))  

n=length(M2.2$x);n
m=5 


Box=Box.test(M2.2$residuals, lag = m,type = "Ljung-Box");Box
acf(M2.2$residuals)

alpha <- 0.05
chi <- qchisq( 1-alpha, m) ;chi
#### es menor el estadístico no se rechaza la ho son independente los residuos


## Dependencias No Lineales: Prueba BDS

r22<-bds.test(residuals(M2.2))

r22 
resul.22 <- data.frame(cbind(t(r22$p.value)), 
                       round(r22$parameter$eps, 5))
colnames(resul.22) <- c('Embedding 2', 'Embedding 3', "Epsilon")
row.names(resul.22) <- NULL
valores <- t(resul.22[, -3])

limite_superior <- max(valores, na.rm = TRUE) * 1.2  
gra <- barplot(
  valores, 
  beside = TRUE,  # Barras agrupadas
  names.arg = resul.22$Epsilon,  # Epsilon en el eje x
  col = c("#87CEEB", "#FFB6C1"),  # Colores
  ylim = c(0, limite_superior),  # Límite dinámico
  xlab = "Epsilon", 
  ylab = "P-valor", 
  main = "Resultados del Test BDS para ARIMA(2,1,2)"
)

abline(h = 0.05, col = "red", lty = 1, lwd = 2)
legend(
  "top", 
  legend = c("Dim 2", "Dim 3", "Valor p = 0.05"),
  fill = c("#87CEEB", "#FFB6C1", "red"), 
  bty = "n", 
  horiz = TRUE  
)




ggtsdiag(M2.2)






###EVALUACION DEL MODELO 3.2 css........................................


## Suma de los coeficientes diferente de 1
coef(M3.2)
A3=sum(M3.2$coef)
A3 




t.test(M3.2$residuals)


## Media de los Residuos: One Sample t-test
t.test(residuals(M3.2))  
###la media de los residuos no es significativamente diferente de cero no rechazama ho




## Normalidad de los Residuos: Kolmogorov-Smirnov test 
ks.test(residuals(M3.2), "pnorm")  ### no son normales


par(mfrow = c(1, 2))
qqnorm(residuals(M3.2))
qqline(residuals(M3.2))
hist(residuals(M3.2),ylim = c(0, 10000),  breaks = 10, freq = F, main = "Histogram of resifuals", xlab = "")
lines(density(residuals(M3.2)), col = "red", lwd = "2")
par(mfrow = c(1, 2))


#### Homocedasticidad de los Residuos: ARCH LM-test
ArchTest(residuals(M3.2))  
#los residuos parecen tener una varianza constante en el tiempo.
#Los residuales no presentan heterocedasticidad condicional



#### Independencia de los Residuos: Ljung-Box 


log(length(M3.2$x))  

n=length(M3.2$x);n
m=5 


Box=Box.test(M3.2$residuals, lag = m,type = "Ljung-Box");Box
acf(M3.2$residuals)

alpha <- 0.05
chi <- qchisq( 1-alpha, m) ;chi
#### es menor el estadístico no se rechaza la ho son independente los residuos


## Dependencias No Lineales: Prueba BDS

r32<-bds.test(residuals(M3.2))

r32 
resul.32 <- data.frame(cbind(t(r32$p.value)), 
                      round(r32$parameter$eps, 5))
colnames(resul.32) <- c('Embedding 2', 'Embedding 3', "Epsilon")
row.names(resul.32) <- NULL
valores <- t(resul.32[, -3])

limite_superior <- max(valores, na.rm = TRUE) * 1.2  
gra <- barplot(
  valores, 
  beside = TRUE,  # Barras agrupadas
  names.arg = resul.32$Epsilon,  # Epsilon en el eje x
  col = c("#87CEEB", "#FFB6C1"),  # Colores
  ylim = c(0, limite_superior),  # Límite dinámico
  xlab = "Epsilon", 
  ylab = "P-valor", 
  main = "Resultados del Test BDS para ARIMA(0,1,2)"
)

abline(h = 0.05, col = "red", lty = 1, lwd = 2)
legend(
  "top", 
  legend = c("Dim 2", "Dim 3", "Valor p = 0.05"),
  fill = c("#87CEEB", "#FFB6C1", "red"), 
  bty = "n", 
  horiz = TRUE  
)



ggtsdiag(M3.2)












#####Fase 4 :Metricas desempeño arima-----------------------------------------------------



H=12


#modelo 2
pronosm2=forecast(M2.2,H,bootstrap = T)
pronosm2


pronosm2$fitted # y entre
pronosm2$mean # y prueba
pronosm2$residuals # residuales entranamiento 





autoplot(pronosm2)
plot(pronosm2)

InvBoxCox(pronosm2$mean, lambda) ##### regresar los datos a los normales 



accuracy(pronosm2$fitted,train) ## estranamiento
accuracy(M2.2) ## el modelo

accuracy(pronosm2$mean,test) ### prueba


#

#modelo 3
pronosm3=forecast(M3.2,H,bootstrap = T)
pronosm3


pronosm3$fitted # y entre
pronosm3$mean # y prueba
pronosm3$residuals # residuales entranamiento 





autoplot(pronosm3)
plot(pronosm3)

InvBoxCox(pronosm3$mean, lambda) ##### regresar los datos a los normales 



accuracy(pronosm3$fitted,train) ## estranamiento
accuracy(M3.2) ## el modelo

accuracy(pronosm3$mean,test) ### prueba








#modelo 2 (2,1,2) #modelo escogido FINAL

accuracy(InvBoxCox(pronosm2$fitted, lambda), InvBoxCox(train, lambda)) ###entramiento 

accuracy(InvBoxCox(pronosm2$mean, lambda), InvBoxCox(test, lambda)) ###prueba



#modelo 3 (0,1,2)

accuracy(InvBoxCox(pronosm3$fitted, lambda), InvBoxCox(train, lambda)) ###entramiento 

accuracy(InvBoxCox(pronosm3$mean, lambda), InvBoxCox(test, lambda)) ###prueba




 
###### Parte de  arima multiplicativa- SARIMA

#.........................................................................-----,,,,,
## Fase 1 

# D y d 
ts_cor(train)

## Diferenciación 
D = nsdiffs(train) ;D
d = ndiffs(train) ;d

plot(train)
t_ds <- diff(diff(train,lag=12),d=1)
ts_plot(t_ds)




### Estacionariedad de la serie diferenciada 
### Identificación P y Q: ACF y PACF
ts_cor(t_ds, lag.max = 60) 
adf.test(t_ds) ## 
kpss.test(t_ds)
acf(yt_ds)
### Identificacion los p y q usando los datos de entrenamiento 
yt_ds <- t_ds
orden <- eacf(yt_ds, ar.max = 10, ma.max = 10)
orden_eacf<-data.frame(orden$eacf)
orden_eacf






n <- length(yt.c) 

# Dimensiones de la tabla EACF
p_max <- nrow(orden_eacf) - 1  # Orden AR máximo
q_max <- ncol(orden_eacf) - 1  # Orden MA máximo

# Inicializar una matriz vacía para los resultados
selec <- matrix("0", nrow = p_max + 1, ncol = q_max + 1)

# Evaluar cada valor de la matriz EACF según la fórmula del umbral
for (k in 0:p_max) {
  for (j in 0:q_max) {
    # Calcular el umbral dinámico para cada combinación (k, j)
    umbral <- 1.96 / sqrt(n - k - j)
    # Verificar si el valor absoluto supera el umbral
    if (abs(as.numeric(orden_eacf[k + 1, j + 1])) > umbral) {
      selec[k + 1, j + 1] <- "x"
    }
  }
}

# Asignar nombres a filas y columnas
colnames(selec) <- 0:q_max  # Orden MA (columnas)
row.names(selec) <- 0:p_max  # Orden AR (filas)

# Mostrar la tabla de posibles candidatos
print(selec)

umbral <- 1.96 / sqrt(n - k - j)
umbral
#FASE 2-------------------------------------------------------------------------- 



#P=1 ,D=1  Q:1


########     modelo candidato 
####modelo 1
##ML
modelo1 <- Arima(train, order = c(1, 1, 1),
                 seasonal = list(order = c(2, 1, 1), period = 12),
                 method = 'ML') 
coeftest(modelo1) 
modelo1
summary(modelo1)
#CSS-ML
modelo1.2 <- Arima(train, order = c(1, 1, 1),
                   seasonal = list(order = c(2, 1, 1), period = 12),
                   method = 'CSS-ML'); coeftest(modelo1.2)

modelo1.2
summary(modelo1.2)


##CSS
modelo1.3 <- Arima(train, order = c(1, 1, 1),
                   seasonal = list(order = c(2, 1, 1), period = 12),
                   method = 'CSS'); coeftest(modelo1.3)

modelo1.3  
summary(modelo1.3)







###redefine el modelo ya que no es significativo # se aplica el principio  
#parcimonioso 

#0,1,1 no tiene significativo
#1,1,2 tampo es significativo
#0,1,2 tampoco da significativo



####modelo 2--------------------------------------------------------------------
##ML
modelo2.1<-Arima(train, order = c(0, 1, 0),
                 seasonal = list(order = c(2, 1, 1), period = 12),
                 method = 'ML'); coeftest(modelo2.1) 
modelo2.1
summary(modelo2.1)
#CSS-ML
modelo2.2 <- Arima(train, order = c(0, 1, 0),
                   seasonal = list(order = c(2, 1, 1), period = 12),
                   method = 'CSS-ML'); coeftest(modelo2.2)

modelo2.2 
summary(modelo2.2)




##CSS
modelo2.3 <- Arima(train, order = c(0, 1, 0),
                   seasonal = list(order = c(2, 1, 1), period = 12),
                   method = 'CSS'); coeftest(modelo2.3)
modelo2.3
summary(modelo2.3) 

#### modelo con coeficiente todo
#?Arima














######## FASE 3 EVALUACION Y DIASGNOTICO  







###FASE3  ------------modelo 2------------------------------------





###EVALUACION DEL MODELO 2

modelo2.3$residuals


## Media de los Residuos: One Sample t-test
t.test(residuals(modelo2.3))  
###la media de los residuos no es significativamente diferente de cero no rechazama ho




## Normalidad de los Residuos: Kolmogorov-Smirnov test 
ks.test(residuals(modelo2.3), "pnorm")  ### no son normales


par(mfrow = c(1, 2))
qqnorm(residuals(modelo2.3))
qqline(residuals(modelo2.3))
hist(residuals(modelo2.3),ylim = c(0, 0.4),  breaks = 10, freq = F, main = "Histogram of resifuals", xlab = "")
lines(density(residuals(modelo2.3)), col = "red", lwd = "2")
par(mfrow = c(1, 2))


#### Homocedasticidad de los Residuos: ARCH LM-test
ArchTest(residuals(modelo2.3))  
#los residuos parecen tener una varianza constante en el tiempo.




#### Independencia de los Residuos: Ljung-Box 


log(length(modelo2.3$x))  

n=length(modelo2.3$x);n
m=5 


Box=Box.test(modelo2.3$residuals, lag = m,type = "Ljung-Box");Box
acf(modelo2.3$residuals)

alpha <- 0.05
chi <- qchisq( 1-alpha, m) ;chi
#### es menor el estadístico no se rechaza la ho son independente los residuos


## Dependencias No Lineales: Prueba BDS

bds.test(residuals(modelo2.3))


## Dependencias No Lineales: Prueba BDS

r2.2<-bds.test(residuals(modelo2.3))
r2.2 
resul2 <- data.frame(cbind(t(r2.2$p.value)), 
                     round(r2.2$parameter$eps, 5))
colnames(resul2) <- c('Embedding 2', 'Embedding 3', "Epsilon")
row.names(resul2) <- NULL
valores <- t(resul2[, -3])

limite_superior <- max(valores, na.rm = TRUE) * 1.2  
gra <- barplot(
  valores, 
  beside = TRUE,  # Barras agrupadas
  names.arg = resul2$Epsilon,  # Epsilon en el eje x
  col = c("#87CEEB", "#FFB6C1"),  # Colores
  ylim = c(0, limite_superior),  # Límite dinámico
  xlab = "Epsilon", 
  ylab = "P-valor", 
  main = "Resultados del Test BDS para ARIMA(0,1,0)(2,1,1)[12]"
)

abline(h = 0.05, col = "red", lty = 1, lwd = 2)
legend(
  "top", 
  legend = c("Dim 2", "Dim 3", "Valor p = 0.05"),
  fill = c("#87CEEB", "#FFB6C1", "red"), 
  bty = "n", 
  horiz = TRUE  
)

















######Fase 4 metricas--------------------------------------------------------------------------- 
H=12







 
# modelo 2
pronos2=forecast(modelo2.3,H,bootstrap = T)
pronos2






#2*
pronos2$fitted # y entre
pronos2$mean # y prueba
pronos2$residuals # residuales entranamiento 





#### modelo 2-----------------------------------------------------------------
autoplot(pronos2)
plot(pronos2)

InvBoxCox(pronos2$mean, lambda) ##### regresar los datos a los normales 



accuracy(pronos2$fitted,train) ## estranamiento
accuracy(modelo2) ## el modelo

accuracy(pronos2$mean,test) ### prueba
















# Calcular las medidas de precisión para las predicciones ajustadas (Prueba) y las predicciones en el conjunto de entrenamiento (Entrenamiento)



#modelo2
accuracy(InvBoxCox(pronos2$fitted, lambda), InvBoxCox(train, lambda)) ###entramiento 

accuracy(InvBoxCox(pronos2$mean, lambda), InvBoxCox(test, lambda)) ###prueba








# FASE CON EL AUTOARIMA------------------------------------------------------------
######ahora el EL CON LA FUNCION AUTO ARIMA


#####auto arima
arima1 <- auto.arima(train)
arima1
auto.arima(train,seasonal=T, ic ="aic",trace=T,stepwise = FALSE)


#ARIMA(0,1,0)(0,1,1)[12] 


########posible modelo




#FAS2-------------------------------------------
####modelo CON AUTOARIMA --------------------------------------------------------------------
##ML
modeloAU <- Arima(train, order = c(0, 1, 0),
                   seasonal = list(order = c(0, 1, 1), period = 12),
                   method = 'ML'); coeftest(modeloAU) 
modeloAU
summary(modeloAU)

#CSS-ML
modeloAU2.2 <- Arima(train, order = c(0, 1, 0),
                   seasonal = list(order = c(0, 1, 1), period = 12),
                   method = 'CSS-ML'); coeftest(modeloAU2.2)

modeloAU2.2
summary(modeloAU2.2)

#modelo bueno
##CSS
modeloAU2.3 <- Arima(train, order = c(0, 1, 0),
                   seasonal = list(order = c(0, 1, 1), period = 12),
                   method = 'CSS'); coeftest(modeloAU2.3)
modeloAU2.3
summary(modeloAU2.3)



#FASE 3
###modelo AUTORIMA
## Suma de los coeficientes diferente de 1
coef(modeloAU2.2)
modelo5A=sum(modeloAU2.2$coef)
modelo5A 





###EVALUACION DEL MODELO 2

modeloAU2.2$residuals


## Media de los Residuos: One Sample t-test
t.test(residuals(modeloAU2.2))  
###la media de los residuos no es significativamente diferente de cero no rechazama ho




## Normalidad de los Residuos: Kolmogorov-Smirnov test 
ks.test(residuals(modeloAU2.2), "pnorm")  ### no son normales


par(mfrow = c(1, 2))
qqnorm(residuals(modeloAU2.2))
qqline(residuals(modeloAU2.2))
hist(residuals(modeloAU2.3),ylim = c(0, 0.4),  breaks = 10, freq = F, main = "Histogram of resifuals", xlab = "")
lines(density(residuals(modeloAU2.2)), col = "red", lwd = "2")
par(mfrow = c(1, 2))


#### Homocedasticidad de los Residuos: ARCH LM-test
ArchTest(residuals(modeloAU2.2))  
#los residuos parecen tener una varianza constante en el tiempo.




#### Independencia de los Residuos: Ljung-Box 


log(length(modeloAU2.2$x))  

n=length(modeloAU2.2$x);n
m=5 


Box=Box.test(modeloAU2.2$residuals, lag = m,type = "Ljung-Box");Box
acf(modeloAU2.3$residuals)

alpha <- 0.05
chi <- qchisq( 1-alpha, m) ;chi
#### es menor el estadístico no se rechaza la ho son independente los residuos


## Dependencias No Lineales: Prueba BDS

r.a<-bds.test(residuals(modeloAU2.2))



r.a 
resul.a <- data.frame(cbind(t(r.a$p.value)), 
                     round(r.a$parameter$eps, 5))
colnames(resul.a) <- c('Embedding 2', 'Embedding 3', "Epsilon")
row.names(resul.a) <- NULL
valores <- t(resul.a[, -3])

limite_superior <- max(valores, na.rm = TRUE) * 1.2  
gra <- barplot(
  valores, 
  beside = TRUE,  # Barras agrupadas
  names.arg = resul.a$Epsilon,  # Epsilon en el eje x
  col = c("#87CEEB", "#FFB6C1"),  # Colores
  ylim = c(0, limite_superior),  # Límite dinámico
  xlab = "Epsilon", 
  ylab = "P-valor", 
  main = "Resultados del Test BDS para ARIMA(0,1,0)(0,1,1)[12]"
)

abline(h = 0.05, col = "red", lty = 1, lwd = 2)
legend(
  "top", 
  legend = c("Dim 2", "Dim 3", "Valor p = 0.05"),
  fill = c("#87CEEB", "#FFB6C1", "red"), 
  bty = "n", 
  horiz = TRUE  
)




######Fase 4 metricas  
H=12



# modelo autoarima 0,1,0,- 0,1,1 [12]
pronosAU=forecast(modeloAU2.2,H,bootstrap = T)
pronosAU


autoplot(pronosAU)
plot(pronosAU)



#modelo H=12
accuracy(InvBoxCox(pronosAU$fitted, lambda), InvBoxCox(train, lambda)) ###entramiento 

accuracy(InvBoxCox(pronosAU$mean, lambda), InvBoxCox(test, lambda)) ###prueba


