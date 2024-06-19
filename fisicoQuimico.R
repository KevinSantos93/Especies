library(vegan)
library(dplyr)
library(nortest)
library(ggplot2)
library(tidyr)
if(!require(devtools)) install.packages("devtools")
devtools::install_github("kassambara/factoextra")
library(factoextra)
library(patchwork)

variablesFisicas <- read.csv('fisicoQuimico.csv', encoding = 'latin1', sep = ';')

#Realiza el test de normalidad de shapiro para cada una de las variables
shapiro_test <- variablesFisicas %>%
  summarise(p_valor_temperatura = shapiro.test(temp..C.)$p.value,
            p_valor_oxigenoDisuelto = shapiro.test(Odisuelto..mg.L.)$p.value,
            p_valor_conductividadElectrica = shapiro.test(condElec.µS.cm.)$p.value,
            p_valor_pH = shapiro.test(pH)$p.value,
            p_valor_dureza = shapiro.test(dureza.mg.LCaCO3.)$p.value,
            p_valor_caudal = shapiro.test(caudal.m3.s.1.)$p.value)

#Según los resultados, solo el caudal y la dureza se dsitribuyen normalmente, ya que tienen p valores
#menores que 0.05 (p < 0.05)

##Para comprobar, se realiza el test de Anderson-Darling

AD_test <- variablesFisicas %>%
  summarise(p_valor_temperatura = ad.test(temp..C.)$p.value,
            p_valor_oxigenoDisuelto = ad.test(Odisuelto..mg.L.)$p.value,
            p_valor_conductividadElectrica = ad.test(condElec.µS.cm.)$p.value,
            p_valor_pH = ad.test(pH)$p.value,
            p_valor_dureza = ad.test(dureza.mg.LCaCO3.)$p.value,
            p_valor_caudal = ad.test(caudal.m3.s.1.)$p.value)

##Al realizar el test de Anderson-Darling, se comprueba que el caudal y la dureza son las
#unicas variables que se distribuyen normamlente.

##Mostramos gráficamente la distribución de los datos

# Grafica el Q-Q plot con ggplot2
variablesFisicas %>%
  select(c(4:9)) %>%
  gather(variable, valor) %>%
  ggplot(aes(sample = valor)) +
    stat_qq() +
    facet_wrap(~ variable, scales = 'free')

##Graficamente se confirma que el caudal y la dureza cumplen con distribuirse normalmente.

####Estandarización de las variable a log(x + 1) ya que hay una observación con 0

# Estandarización logarítmica para cada variable
variablesEstandarizadas <- variablesFisicas %>%
  mutate(
    temp..C. = log(temp..C. + 1),
    Odisuelto..mg.L. = log(Odisuelto..mg.L. + 1),
    condElec.µS.cm. = log(condElec.µS.cm. + 1),
    pH = log(pH + 1),
    dureza.mg.LCaCO3. = log(dureza.mg.LCaCO3. + 1),
    caudal.m3.s.1. = log(caudal.m3.s.1. + 1)
  )

##Análisis de componentes principales por rio
escalado <- scale(variablesEstandarizadas[,4:9])
escalado <- cbind(escalado, variablesEstandarizadas[,1:3])



#Análisis de componentes para la cuenca Penagos

penagos <- escalado %>%
  filter(cuenca == 'Penagos')

#Aplica el pca
penagos.pca <- prcomp(penagos[, 1:6], scale = TRUE)

#Resultados del análisis
summary(penagos.pca)

#Se observa qye la primera componente explica el 55.65% de la
#varianza total, mientras que las primeras tres explican el 95.64%

#Visualización gráfica
fviz_eig(penagos.pca)

#distribución de las observaciones. Observaciones similares estan mas cercanas
fviz_pca_ind(penagos.pca,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

#Gráfico de las variables. Las que correlacionan positivamente se encuentran en
#el mismo cuadrante de la gráfica. Las que correlacionan negativamente se encuentran
#en cuadrantes opuestos. Por ejemplo, caudal correlacionan negativamente con la dureza
#y el oxigeno disuelto.

#El color representa la contribución a la componente. Las variables de la temperatura
# y e pH son las que menos contribuyen a las componentes.
fviz_pca_var(penagos.pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)


#Muestra cómo se agrupan las observaciones por periodo
groups <- as.factor(penagos$Periodo)
periodo <- fviz_pca_ind(penagos.pca,
             col.ind = groups, # color by groups
             palette = c("#00AFBB",  "#FC4E07"),
             addEllipses = TRUE, # Concentration ellipses
             ellipse.type = "confidence",
             legend.title = "Época",
             repel = TRUE
)

#Muestra cómo se agrupan las observaciones por periodo
groups <- as.factor(penagos$punto)
punto <- fviz_pca_ind(penagos.pca,
             col.ind = groups, # color by groups
             palette = c("#00AFBB",  "#FC4E07", "#E7B800"),
             addEllipses = TRUE, # Concentration ellipses
             ellipse.type = "confidence",
             legend.title = "Puntos",
             repel = TRUE
)

#Muestra las dos gráficas juntas
periodo/punto


#Análisis de componentes para la cuenca Morro
morro <- escalado %>%
  filter(cuenca == 'Morro')
#Aplica el pca
morro.pca <- prcomp(morro[, 1:6], scale = TRUE)

#Resultados del análisis
summary(morro.pca)

#Se observa qye la primera componente explica el 53.09% de la
#varianza total, mientras que las primeras tres explican el 94.62%
#Observese que dos componentes explican mas de 3 cuartas partes de la
#varianza total

#Visualización gráfica
fviz_eig(morro.pca)

#distribución de las observaciones. Observaciones similares estan mas cercanas
fviz_pca_ind(morro.pca,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

#Gráfico de las variables. Las que correlacionan positivamente se encuentran en
#el mismo cuadrante de la gráfica. Las que correlacionan negativamente se encuentran
#en cuadrantes opuestos.

#El color representa la contribución a la componente. La variable del oxigeno disuelto
#es la que menos contribuye a las componentes.
fviz_pca_var(morro.pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)


#Muestra cómo se agrupan las observaciones por periodo
groups <- as.factor(morro$Periodo)
periodo <- fviz_pca_ind(morro.pca,
             col.ind = groups, # color by groups
             palette = c("#00AFBB",  "#FC4E07"),
             addEllipses = TRUE, # Concentration ellipses
             ellipse.type = "confidence",
             legend.title = "Época",
             repel = TRUE
)

#Muestra cómo se agrupan las observaciones por punto
groups <- as.factor(morro$punto)
punto <- fviz_pca_ind(morro.pca,
             col.ind = groups, # color by groups
             palette = c("#00AFBB",  "#FC4E07", "#E7B800"),
             addEllipses = TRUE, # Concentration ellipses
             ellipse.type = "confidence",
             legend.title = "Puntos",
             repel = TRUE
)
periodo/punto
#Análisis de componentes para la cuenca Tejar

tejar <- escalado %>%
  filter(cuenca == 'Tejar ')
#Aplica el pca
tejar.pca <- prcomp(tejar[, 1:6], scale = TRUE)

#Resultados del análisis
summary(tejar.pca)

#Se observa qye la primera componente explica el 51.12% de la
#varianza total, mientras que las primeras tres explican el 93.59%

#Visualización gráfica
fviz_eig(tejar.pca)

#distribución de las observaciones. Observaciones similares estan mas cercanas
fviz_pca_ind(tejar.pca,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

#Gráfico de las variables. Las que correlacionan positivamente se encuentran en
#el mismo cuadrante de la gráfica. Las que correlacionan negativamente se encuentran
#en cuadrantes opuestos. Por ejemplo, caudal correlacionan negativamente con la dureza
#y el oxigeno disuelto.

#El color representa la contribución a la componente. Laa variable del oxigeno disuelto
#es la que menos contribuye a las componentes.
fviz_pca_var(tejar.pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)


#Muestra cómo se agrupan las observaciones por periodo
groups <- as.factor(tejar$Periodo)
periodo <- fviz_pca_ind(tejar.pca,
             col.ind = groups, # color by groups
             palette = c("#00AFBB",  "#FC4E07"),
             addEllipses = TRUE, # Concentration ellipses
             ellipse.type = "confidence",
             legend.title = "Época",
             repel = TRUE
)

#Muestra cómo se agrupan las observaciones por periodo
groups <- as.factor(penagos$punto)
punto <- fviz_pca_ind(res.pca,
             col.ind = groups, # color by groups
             palette = c("#00AFBB",  "#FC4E07", "#E7B800"),
             addEllipses = TRUE, # Concentration ellipses
             ellipse.type = "confidence",
             legend.title = "Puntos",
             repel = TRUE
)
 periodo/punto
 