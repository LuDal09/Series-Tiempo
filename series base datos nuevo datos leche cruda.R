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
 
df23<- read_excel("anex-SIPSALeche-SerieHistoricaPrecios-2023.xlsx", 
                                                         skip = 6)
View(df23) 

str(df23)
df22<- read_excel("series-historicas-precios-mayoristas-leche-2022.xlsx", 
            skip = 6) 
View(df22)
df21<- read_excel("series-historicas-precios-mayoristas-leche-2021.xlsx", 
                  skip = 6) 
View(df21) 


df20<- read_excel("series-historicas-precios-mayoristas-leche-2020.xlsx", 
                  skip = 6)
View(df20) 


df24<- read_excel("anex-SIPSALeche-SerieHistoricaPrecios-2024.xlsx", 
                  skip = 6)
View(df24)  


### año 24 

# Supongamos que tu dataframe se llama 'df' y la columna con fechas es 'fecha'
df24$fecha <- as.Date(df24$`Mes y año`, format = "%d/%m/%Y")

# Extraer el mes en formato numérico (01, 02, etc.)
df24$mes <- format(df24$fecha, "%m")

# Extraer el año en formato de dos dígitos (22, 23, etc.)
df24$año <- format(df24$fecha, "%y")

# Verifica los resultados
head(df24)
View(df24) 





### año 23 

# Supongamos que tu dataframe se llama 'df' y la columna con fechas es 'fecha'
df23$fecha <- as.Date(df23$`Mes y año`, format = "%d/%m/%Y")

# Extraer el mes en formato numérico (01, 02, etc.)
df23$mes <- format(df23$fecha, "%m")

# Extraer el año en formato de dos dígitos (22, 23, etc.)
df23$año <- format(df23$fecha, "%y")

# Verifica los resultados
head(df23)
View(df23) 

### año 22

df22$fecha <- as.Date(df22$`Mes y año`, format = "%d/%m/%Y")

# Extraer el mes en formato numérico (01, 02, etc.)
df22$mes <- format(df22$fecha, "%m")

# Extraer el año en formato de dos dígitos (22, 23, etc.)
df22$año <- format(df22$fecha, "%y")

# Verifica los resultados
head(df22)
View(df22) 
 

###año 21

df21$fecha <- as.Date(df21$`Mes y año`, format = "%d/%m/%Y")

# Extraer el mes en formato numérico (01, 02, etc.)
df21$mes <- format(df21$fecha, "%m")

# Extraer el año en formato de dos dígitos (22, 23, etc.)
df21$año <- format(df21$fecha, "%y")

# Verifica los resultados
head(df21)
View(df21) 

### 
write.xlsx(df23, "precioleche-promedio-cordoba203.xlsx")
write.xlsx(df22, "precioleche-promedio-cordoba202.xlsx")
write.xlsx(df21, "precioleche-promedio-cordoba201.xlsx")

#### año 20
str(df20)
View(df20)
cordobanueva20 <- df20 %>%
  filter(`Nombre departamento` == "CÓRDOBA") %>%  
  group_by(Año, Mes) %>%  
  summarise(precioleche= mean(`Precio promedio por litro`, na.rm = TRUE), .groups = "drop")   
View(cordobanueva20)

 

write.xlsx(cordobanueva20, "precioleche-promedio-cordoba200.xlsx")
 
#año 21

View(df21)
cordobanueva21 <- df21 %>%
  filter(`Nombre departamento` == "CÓRDOBA") %>%  
  group_by(año, mes) %>%  
  summarise(precioleche= mean(`Precio promedio por litro`, na.rm = TRUE), .groups = "drop")   
View(cordobanueva21)



write.xlsx(cordobanueva21, "precioleche-promedio-cordoba201.xlsx")



#año 22 



View(df22)
cordobanueva22 <- df22 %>%
  filter(`Nombre departamento` == "CÓRDOBA") %>%  
  group_by(año, mes) %>%  
  summarise(precioleche= mean(`Precio promedio por litro`, na.rm = TRUE), .groups = "drop")   
View(cordobanueva22)


write.xlsx(cordobanueva22, "precioleche-promedio-cordoba202.xlsx")



#año 23 



View(df23)
cordobanueva23 <- df23 %>%
  filter(`Nombre departamento` == "Córdoba") %>%  
  group_by(año, mes) %>%  
  summarise(precioleche= mean(`Precio promedio por litro`, na.rm = TRUE), .groups = "drop")   
View(cordobanueva23)


write.xlsx(cordobanueva23, "precioleche-promedio-cordoba203.xlsx")
 



#año 24



View(df24)
cordobanueva24 <- df24 %>%
  filter(`Nombre departamento` == "Córdoba") %>%  
  group_by(año, mes) %>%  
  summarise(precioleche= mean(`Precio promedio por litro`, na.rm = TRUE), .groups = "drop")   
View(cordobanueva24)


write.xlsx(cordobanueva24, "precioleche-promedio-cordoba204.xlsx")

