#Análisis de componentes principales 

library(pacman)
p_load(haven,dplyr,factoextra,FactoMineR,readr,rgl,fpc, psych,xlsx)

setwd("/Users/emilianorubioramos/Desktop/8vo Semestre/Cómputo Cientifico")

data_pca<-read.csv("data_pca.csv", sep=";", dec=",")
View(data_pca)

# Normalizar datos
data1<-scale(data_pca)

# Datos ya Normalizados
View(data1)

# Realizar PCA

# Diagnostico para el PCA si el determinante sale cercano a 0 se puede hacer por componentes principales
det(cor(data1))



pca<-princomp(data1)
pca$loadings
# Diagnostico
summary(pca) # consideramos sólo las varianzas mayores al 15% (i.e componentes 1&2)



# Revisar varianza y eigenvalores
fviz_eig(pca,choice="variance")
# En efecto los componentes 1&2 son los que nos aportan mayor varianza

fviz_eig(pca,choice="eigenvalue")
#Solo dos componentes tienen un eigenvalor mayor a la unidad, lo adecuado es extraer unicamente dos factores 

#Análisis grafico 
#El coseno cuadrado se utiliza para medir la calidad de la representacion de las variables originales en el espacio de los componentes principales
#Específicamente, el coseno cuadrado de una variable en un componente principal es el cuadrado del coseno del ángulo entre la variable original y el 
#componente principal 

fviz_pca_ind(pca,
             col.ind = "cos2",
             gradient.cols=c("red","yellow","green"),
             repel = FALSE)

#El coseno cuadrado indica qué proporción de la varianza de la variable
#original es explicada por el componente principal.
#valores altos de coseno cuadrado (cercanos a 1) significan que la 
#variable está bien representada por el componente principal, 
#mientras que valores bajos (cercanos a 0) indican una mala representación

#Las observaciones en color verde fuerte son representadas en mejor medida
#las observaciones 28,23,25 y 27 no son tan bien representadas, pero son la minoria 

#Gráfico de las cargas

#Cuánto contribuye cada variable a las diferentes componentes principales?
#Los componentes contribuyen en diferente medida a los cuadrantes dentro de la representacion bidimensional 

fviz_pca_var(pca,
             col.var = "contrib",
             gradient.cols=c("red","yellow","green"),
             repel = FALSE)
#Las variables con mayor contribución positiva son y, x14, x1, x15 y x9,mientras que x5 tiene una contribución negativa importante.




x11()
psych::cor.plot(data1)
#analisis como lo proporciona spss
#se debe notar que todas las variables en un pca deben estar altamente
#correlacionadas para que tenga sentido realizarlo
det(cor(data1))

#El determinante de la matriz de correlación es muy bajo, por lo que el grado de asociación es muy alto.

#Resultado del pca por factores: 
#la rotación más común es VARIMAX
#En un PCA, los componentes principales iniciales pueden ser díficiles de interpretar porque cada variable puede tener cargas
#de manera que cada variable tenga una carga alta en un solo componente, haciendo que la estructura sea más simple y cara. 
# PCA con rotación varimax
pca2 <- psych::principal(data1, nfactors = 16, residuals = FALSE, 
                         rotate = "varimax", scores = TRUE, 
                         oblique.scores = FALSE, method = "regression")

# Mostrar los pesos de las variables en cada componente
for (i in 1:16) {
  cat(paste("Pesos en el Componente", i, ":\n"))
  print(pca2$weights[,i])
  cat("\n")
}

#Nuevas variables obtenidas cuya principal caracteristica es que son ortogonales, es decir, linealmente independientes.
pca2$scores





