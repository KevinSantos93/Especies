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
library(fossil)

especies <- read.csv('especies.csv', sep = ';')

#número de especies (familia) por observación
par(mfrow=c(1,1),cex=1.2) # particiona ventana grafica en 2x2

#Calculo de la riqueza
specnumber(especies[,4:35])

#Abundancia de las familias

especies %>%
  pivot_longer(cols = Hydrobiosidae:ND,
               names_to = "Especie",
               values_to = "Abundancia") %>%
  ggplot(aes(x =Especie, y = Abundancia)) +
    geom_boxplot() +
    labs(x = "Especie", y = "Abundancia") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Calcula los estimadores Chao, Chao corregido, primer y segundo orden de jacknife
#y bootstrap
estimateR(especies[,4:35])

###Calculamos chao2, con el paquete fossil

##Obtenemos las incidencias, para calclular chao2
incidencias <- especies %>%
  mutate_at(vars(4:35), ~ ifelse(. > 0, 1, 0))

##Calculo de chao2
estimateR(incidencias[4], method = "chao2")


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

##Correlación entre especies en la epoca lluviosa
ggpairs(correlacion[,3:11], title = 'Época lluviosa')

##Correlación entre muestras de la época seca
ggpairs(correlacion[,12:20], title = 'Época Seca')

##Correlación entre las diferentes épocas
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
