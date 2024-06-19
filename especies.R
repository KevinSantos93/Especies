library(vegan)
library(dplyr)
library(nortest)
library(ggplot2)
library(tidyr)
library(factoextra)
library(patchwork)
library(BiodiversityR)
library(GGally)
library(CCA)

especies <- read.csv('especies.csv', sep = ';')

#número de especies (familia) por observación
par(mfrow=c(1,1),cex=1.2) # particiona ventana grafica en 2x2

#Calculo de la riqueza
specnumber(especies[,4:35])

#Abundancia de las familias
abundancia <- boxplot(especies[,4:35], ylab = "Abundancia", "Especies")
estimateR(especies[,4:35])

#Curvas de acumulación
#Existen diferentes metodos para estimar la riqueza.
saco<-specaccum(comm=especies[,4:35],method="collector")
sae<-specaccum(especies[,4:35], "exact")
sac<-specaccum(especies[,4:35], "coleman")
sarf<-specaccum(especies[,4:35], "rarefaction")
par(mfrow=c(2,2),cex=1.2) # particiona ventana grafica en 2x2
plot(saco,ci.type="line",ci.lty=2)
plot(sae,ci.type="line",ci.lty=2)
plot(sac,ci.type="line",ci.lty=2)
plot(sarf,ci.type="line",ci.lty=2)


#Utilizando cantidad de especies observadas, podemos calcular la riqueza con métodos no paramétricos
pool <- poolaccum(especies[,4:35])
summary(pool, display = "chao")
plot(pool)


##Análisis de correspondencia
correlacion <- read.csv('correlacion.csv', sep = ';')
summary(correlacion)

ggpairs(correlacion[,3:11], title = 'Época lluviosa')
ggpairs(correlacion[,12:20], title = 'Época Seca')
ggduo(correlacion,columnsX = 3:11,columnsY = 12:20,
      types = list(continuous = "smooth_lm"),
      title = "Correlación entre observaciones en época seca y lluviosa",
)

##Matriz de correlaciones
mat_cor  <- matcor(correlacion[,3:11],correlacion[,12:20])
mat_cor
cc1 <- cc(correlacion[,3:11],correlacion[,12:20])
cc1$cor
cc1[3:4]
plt.cc(cc1, var.label = TRUE, d1 = 1, d2 = 2, type = "b")
