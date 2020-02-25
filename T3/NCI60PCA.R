#############################################################
##
## erojasoviedo@gmail.com - Octubre - 2019
##
## Descripcion:
## Reduciendo la dimensión: Analsis de Componentes Principales
##
## Reference:
## https://rpubs.com/Cristina_Gil/PCA
## Cristina Gil Martínez (cristina_gil_m@hotmail.com)
##
#############################################################

##############################
# 0 - Load libraries
##############################

library(ISLR)
library(FactoMineR)
library(factoextra)

# names(NCI60)
# NCI60$data
# NCI60$labs

##############################
# 1 - Functions
##############################

## Print data subset
colores <- function(vec) {
  # la función rainbow() devuelve un vector que contiene el 
  # número de colores distintos
  col <- rainbow(length(unique(vec)))
  return(col[as.numeric(as.factor(vec))])
}


##############################
# 2 - Explore data
##############################

datos.nci <- NCI60$data
# dim(datos.nci)
## [1]   64 6830 64:Attributos 6830:Observaciones
head(datos.nci)[, 1:6]

# Tipos de cáncer distintos en el set de datos
unique(NCI60$labs)
# [1] "CNS"         "RENAL"       "BREAST"      "NSCLC"       "UNKNOWN"     "OVARIAN"     "MELANOMA"   
# [8] "PROSTATE"    "LEUKEMIA"    "K562B-repro" "K562A-repro" "COLON"       "MCF7A-repro" "MCF7D-repro"

# Número de muestras por tipo de cáncer
table(NCI60$labs)


##############################
# 3 - Prepare data
##############################

# Media de la expresión de cada gen (muestra de los 10 primeros). 
# (MARGIN = 2 para que se aplique la función a las columnas)
apply(X = datos.nci, MARGIN = 2, FUN = mean)[1:10]


##############################
# 4 - Cálculo de componentes principales
##############################

# @##################################################################@
# @### estandarizar las variables - desviación estándar igual a 1 @###
# @##################################################################@

## Centra las variables para que tengan media de 0. 
## Con el argumento scale = TRUE indicamos que queremos escalar 
## las variables para que tengan desviación estándar igual a 1.
pca.nci <- prcomp(datos.nci, scale = TRUE)

names(pca.nci)
## [1] "sdev"     "rotation" "center"   "scale"    "x"

# Muestra de los primeros 6 elementos del vector de loadings de los 5 primeros componentes
head(pca.nci$rotation)[, 1:5]
# head(pca.nci$center)
# head(pca.nci$scale)
# head(pca.nci$x)[, 1:5]

dim(pca.nci$rotation)
## [1] 6830   64

# acceder a los vectores de los scores 
head(pca.nci$x)[,1:5]

# desviación estándar de cada componente principal:
pca.nci$sdev

# Varianza explicada por cada componente
pca.nci$sdev^2

# varianza explicada es mayor en la primera componente 
# que en las subsiguientes
summary(pca.nci)


# @##################################################################@
# @### Generar PCA - desviación estándar igual a 1 @###
# @##################################################################@

# objeto PCA
pca2.nci <- PCA(X = datos.nci, scale.unit = TRUE, ncp = 64, graph = FALSE)

print(pca2.nci)
## **Results for the Principal Component Analysis (PCA)**
## The analysis was performed on 64 individuals, described by 6830 variables

## (varianza explicada) es mayor en la primera componente que en las subsiguientes
head(pca2.nci$eig)
## eigenvalue percentage of variance cumulative percentage of variance
#         eigenvalue percentage of variance cumulative percentage of variance
# comp 1   775.8157              11.358942                          11.35894
# comp 2   461.4486               6.756203                          18.11514
# comp 3   392.8508               5.751842                          23.86699
# comp 4   290.1080               4.247554                          28.11454
# comp 5   255.0986               3.734972                          31.84951
# comp 6   247.1524               3.618630                          35.46814


##############################
# 5 - Representación
##############################

# @######################################@
# @### Representación - Observaciones @###
# @######################################@

## axes 1 y 2 se corresponden con PC1 y PC2, [PC1 - PC64] 
## pudiendo escoger otros
fviz_pca_ind(pca.nci, geom.ind = "point", 
             col.ind = "#FC4E07", 
             axes = c(1, 2), 
             pointsize = 1.5) 


par(mfrow = c(1,2))
# Observaciones sobre PC1 y PC2
plot(pca.nci$x[,1:2], col = colores(NCI60$labs), 
     pch = 19, 
     xlab = "Z1", 
     ylab = "Z2")

# Observaciones sobre PC1 y PC3
plot(pca.nci$x[,c(1, 3)], col = colores(NCI60$labs), 
     pch = 19, 
     xlab = "Z1", 
     ylab = "Z3")

# @##################################@
# @### Representación - Variables @###
# @##################################@

## La correlación entre una variable y un componente principal 
## se utiliza como la coordenada de dicha variable sobre el 
## componente principal.

#  gráfico de correlación de variables
fviz_pca_var(pca.nci, col.var = "cos2", 
             geom.var = "arrow", 
             labelsize = 2, 
             repel = FALSE)

# Principal Component Analysis Results for variables
var <- get_pca_var(pca.nci)
var

##############################
# 6 - Elección Componentes Principales
##############################

# @##################################@
# @### Elección PCA - Scree plot @###
# @##################################@

## scree plot que represente los eigenvalores ordenador de mayor a menor.
fviz_screeplot(pca.nci, addlabels = TRUE, ylim = c(0, 20))


# @#################################################@
# @### Elección PCA - Contribución de variables @###
# @#################################################@

## con un gran número de variables, podríamos decidir mostrar 
## solo aquellas con mayor contribución

# Top 10 variables que más contribuyen a PC1
## La línea roja discontinua indica el valor medio de contribución
fviz_contrib(pca.nci, choice = "var", axes = 1, top = 10)
## INTERPRETAR:
## En la representación anterior, el gen 5951 es la que más contribuye a la PC1.


# @####################################################@
# @### Proporción de varianza explicada y acumulada @###
# @####################################################@

## Calcular la proporción de varianza explicada (PVE) por 
## cada componente principal
PVE <- 100*pca.nci$sdev^2/sum(pca.nci$sdev^2)
PVE
# [1] 1.135894e+01 6.756203e+00 5.751842e+00 4.247554e+00 3.734972e+00
# [6] 3.618630e+00 3.066222e+00 2.685903e+00 2.529498e+00 2.375869e+00
# [1] 11.35%       6.75%        5.75%        4.24%        3.73%
# [6] 3.61%        3.06%        2.68%        2.52         2.37%

## INTERPRETAR
## El primer componente principal explica el 11,35% de la varianza, 
## mientras que la segunda solo un 6,75%.
## De manera conjunta, los primeros 7 componentes principales explican 
## en torno al 40% de la varianza de los datos, lo cual no es una 
## cantidad muy alta.
# 11.35% + 6.75% + 5.75% + 4.24% + 3.73% + 3.61% + 3.06% = 38.49%

par(mfrow = c(1,2))
plot(PVE, type = "o", 
     ylab = "PVE", 
     xlab = "Componente principal", 
     col = "blue")
plot(cumsum(PVE), type = "o", 
     ylab = "PVE acumulada", 
     xlab = "Componente principal", 
     col = "brown3")
