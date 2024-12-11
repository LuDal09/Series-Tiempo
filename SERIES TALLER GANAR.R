### Trabajo Series de tiempo 
library(tseries)
library(forecast)
library(readxl)
library(RcmdrMisc)
library(ggplot2)
library(ggfortify)
library(dplyr)
library(TSstudio)
library(lmtest)
library(dplyr) 
#install.packages("openxlsx") 
library(openxlsx)
library(urca)

#serieleche <- read_excel("series-historicas-precios-mayoristas-leche-2013-2019 (1).xlsx")
#View(serieleche)
 #str(serieleche) 

#nueva base de datos 2013 a 2024 JULIO mes 7
cordoba <- read_excel("precioleche-promedio-cordoba.xlsx")
View(cordoba)
str(cordoba)   


#cordoba <- serieleche %>%
 # filter(`Nombre departamento` == "CÓRDOBA") %>%  
  #group_by(Año, Mes) %>%  
 #summarise(precioleche= mean(`Precio promedio por litro`, na.rm = TRUE), .groups = "drop")   
#View(cordoba)

#write.xlsx(cordoba, "precioleche-promedio-cordoba.xlsx") # convertir a xlsx

# Imprimir el nuevo dataframe con los promedios
print(cordoba)
yt2<- ts(cordoba$precioleche, start = c(2013, 1), end = c(2024, 7) , frequency = 12)
yt2
end(yt2)
autoplot(yt2) 
yt.c=tsclean(yt2)
 
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

### Estacionalidad 

ts_seasonal(yt.c, type = "normal", title = "Descomposición Estacional Precio de la leche cruda ", Ygrid = TRUE, Xgrid = TRUE, last = NULL, palette = "Set1", palette_normal = "viridis")



# Tradicional

ggseasonplot(yt.c, year.labels=TRUE, year.labels.left=TRUE) +
  ylab("litros") +xlab("Mes")+
  ggtitle("Gráfico de estacionalidad:Precios menuales de leche cruda en finca")

ggplot(cordoba, aes(x=as.factor(Mes), y=`precioleche`, fill=as.factor(Mes))) + 
  geom_boxplot(alpha=0.7) +
  theme(legend.position="none")+xlab('Mes')

ggseasonplot(yt.c, polar=TRUE) +
  ylab("Toneladas") +
  ggtitle("Gráfico polar de estacionalidad: Precios menuales de leche cruda en finca")

ggsubseriesplot(yt.c) +
  ylab("Toneladas") +xlab("Mes")+
  ggtitle("Gráfico de estacionalidad: Precios menuales de leche cruda en finca") 
 

###otro graficos  
ts_seasonal()


ts_seasonal(yt.c, type = "normal")
ts_seasonal(yt.c, type = "box",, title = "Descomposición Estacional Box-Precio de la leche cruda ")
ts_seasonal(yt.c, type = "cycle")

ts_seasonal(yt.c, type = "all")
ts_heatmap(yt.c)
ts_polar(yt.c)  
###escoger 
# Descomposición aditiva
descomposicion_aditiva <- decompose(yt.c, type = "additive")
plot(descomposicion_aditiva)

# Descomposición multiplicativa
descomposicion_multiplicativa <- decompose(yt.c, type = "multiplicative")
plot(descomposicion_multiplicativa)

acf(yt.c, main="ACF de la serie")
pacf(yt.c, main="PACF de la serie")

### Enfoque clasico

###Escogemos el modelo_Multiplicativo 
Percent=cordoba %>% 
  group_by(as.factor(cordoba$Año)) %>% 
  do(data.frame(t(quantile(.$precioleche, probs = c(0.2, .5, .80)))))

plot(log(Percent$X50.), log(Percent$X80.-Percent$X20.), xlab='Nivel', ylab='Dispersión', col='red')


#### 4.2_1 coeficiente de autocorrelacion  

log(length(cordoba$Año))  

n=length(cordoba$Año);n
m=5 


Box.test(yt.c, lag = m,type = "Box-Pierce") 


Box.test(yt.c, lag = m,type = "Ljung-Box")



####forma manual  
acf_est=acf(yt.c,main="ACF de la serie",type="correlation",lag.max=9,plot = F)
acf_est 

acf_values <- acf_est$acf[2:(m + 1)]  # Omitimos el lag 0

# Estadístico de prueba de Box-Pierce
Q_Box_Pierce <- n * sum(acf_values^2)
Q_Box_Pierce
# Estadístico de prueba de Ljung-Box
Q_Ljung_Box <- n * (n + 2) * sum(acf_values^2 / (n - 1:m))
Q_Ljung_Box




alpha <- 0.05
chi_critical <- qchisq( 1-alpha, m)

cat("Valor crítico de chi-cuadrado:", chi_critical, "\n")

# Decisión de rechazo
if (Q_Box_Pierce > chi_critical) {
  cat("Rechazamos H0 para la prueba de Box-Pierce.\n")
} else {
  cat("No rechazamos H0 para la prueba de Box-Pierce.\n")
}

if (Q_Ljung_Box > chi_critical) {
  cat("Rechazamos H0 para la prueba de Ljung-Box.\n")
} else {
  cat("No rechazamos H0 para la prueba de Ljung-Box.\n")
}



#4.2_2 

acf(yt.c, main="ACF de la serie",type="covariance")  

 
pacf(yt.c, main="PACF de la serie") 


acf_est=acf(yt.c,main="ACF de la serie",type="correlation",lag.max=9,plot = F)
acf_est 

ts=ts_cor(yt.c,type="acf",seasonal=F) 
ts

ts_cor(yt.c,type="pacf",seasonal=F)
 
banda=2/sqrt(n);banda


### 5 estacionaria 
###prueba de homocedasticidad 
h=12
n=length(yt.c);n
n.entre=n-h
model=lm(cordoba$precioleche~cordoba$t, data=cordoba[1:n.entre,])
model
model$fitted.values 
# 

bptest(model) #### homocedasticidad 
dwtest(model) #autocorrelacio 

# la series presenta hetorocedastiticidad y autocorrelacion 
###escogemos PP 





###ADF

adf.test(yt.c)
####con tendencia e intercepto

caso1=ur.df(yt.c,type="trend",lags=4);caso1 
summary(caso1)

caso1_mod=ur.df(yt.c,type="trend",lags=4,selectlags=c("AIC")) ;caso1_mod
summary(caso1_mod)
 
caso11=ur.df(yt.c,type="trend",lags=1);caso11 
summary(caso11)
 
adf.test(yt.c,k=1)

caso12=ur.df(yt.c,type="trend",lags=0);caso12 
summary(caso12) 


adf.test(yt.c,k=0) 


#### sin tendencia con intercepto 


caso2=ur.df(yt.c,type="drift",lags=4,selectlags=c("AIC")) ;caso2
summary(caso2)


caso2_mod=ur.df(yt.c,type="drift",lags=0);caso2_mod 
summary(caso2_mod)  


#### sin tedencia e intercepto 


caso3=ur.df(yt.c,type="none",lags=4,selectlags=c("AIC")) ;caso3
summary(caso3)


caso3_mod=ur.df(yt.c,type="none",lags=0);caso3_mod 
summary(caso3_mod)  
 



###prueba phills perro 

#no estacionaria vs esta....
pp.test(yt.c)

##con tendencia e  intercepto 

caso1.pp=ur.pp(yt.c,model="trend") ;caso1.pp
summary(caso1.pp)

###con   intercepto pero sin tendencia 

caso2.pp=ur.pp(yt.c,model="constant");caso2.pp
summary(caso2.pp)
?ur.pp 

####sin tendecia e intercepto manual ya que no existe en urca

yt2_centrado <- yt.c - mean(yt.c)

# prueba de pp sin intercepto y sin tendencia
pp_test <- ur.pp(yt2_centrado, type = "Z-alpha", model = "constant", lags = "long")


summary(pp_test)


####kpss 



kpss.test(yt.c) 

caso1.kpss=ur.kpss(yt.c,type="tau");caso1.kpss 
summary(caso1.kpss) 


caso2.kpss=ur.kpss(yt.c,type="mu");caso2.kpss 
summary(caso2.kpss)






