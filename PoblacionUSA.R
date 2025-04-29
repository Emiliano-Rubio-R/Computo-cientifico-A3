# Análisis de Componentes Principales para 2020 y 2021

library(pacman)
p_load(haven, dplyr, factoextra, FactoMineR, readr, fpc, psych, xlsx, readxl)

# Establecer directorio de trabajo
setwd("/Users/emilianorubioramos/Desktop/8vo Semestre/Cómputo Cientifico")

# Cargar datos
data <- read_excel("PoblacionUSA.xlsx", sheet = "Hoja1")
View(data)

# Separar datos para 2020 y 2021
data_2020 <- data[, c(2, 3, 5, 7, 9, 11, 13, 15, 17, 19)] # Columnas relevantes para 2020
data_2021 <- data[, c(4, 6, 8, 10, 12, 14, 16, 18, 20)] # Columnas relevantes para 2021

# Normalizar datos
data_2020_scaled <- scale(data_2020)
data_2021_scaled <- scale(data_2021)

# Diagnóstico para PCA: Determinante de la matriz de correlación
det(cor(data_2020_scaled))
det(cor(data_2021_scaled))

psych::KMO(data_2020_scaled)
psych::KMO(data_2021_scaled)

# Realizar PCA para 2020
pca_2020 <- princomp(data_2020_scaled)
summary(pca_2020) # Revisar varianzas explicadas

# Visualización de eigenvalores y varianza para 2020
fviz_eig(pca_2020, choice = "variance")
fviz_eig(pca_2020, choice = "eigenvalue")

# Gráficos de observaciones y variables para 2020
fviz_pca_ind(pca_2020,
             col.ind = "cos2",
             gradient.cols = c("red", "yellow", "green"),
             repel = FALSE)

fviz_pca_var(pca_2020,
             col.var = "contrib",
             gradient.cols = c("red", "yellow", "green"),
             repel = FALSE)

# Realizar PCA para 2021
pca_2021 <- princomp(data_2021_scaled)
summary(pca_2021) # Revisar varianzas explicadas

# Visualización de eigenvalores y varianza para 2021
fviz_eig(pca_2021, choice = "variance")
fviz_eig(pca_2021, choice = "eigenvalue")

# Gráficos de observaciones y variables para 2021
fviz_pca_ind(pca_2021,
             col.ind = "cos2",
             gradient.cols = c("red", "yellow", "green"),
             repel = FALSE)

fviz_pca_var(pca_2021,
             col.var = "contrib",
             gradient.cols = c("red", "yellow", "green"),
             repel = FALSE)

x11()
# Correlación entre variables
psych::cor.plot(data_2020_scaled)
psych::cor.plot(data_2021_scaled)

# PCA con rotación varimax para 2020
pca2_2020 <- psych::principal(data_2020_scaled, nfactors = 2, residuals = FALSE, 
                              rotate = "varimax", scores = TRUE, 
                              oblique.scores = FALSE, method = "regression")

# Mostrar pesos de las variables en cada componente para 2020
for (i in 1:2) {
  cat(paste("Pesos en el Componente", i, "para 2020:\n"))
  print(pca2_2020$weights[, i])
  cat("\n")
}

# PCA con rotación varimax para 2021
pca2_2021 <- psych::principal(data_2021_scaled, nfactors = 2, residuals = FALSE, 
                              rotate = "varimax", scores = TRUE, 
                              oblique.scores = FALSE, method = "regression")

# Mostrar pesos de las variables en cada componente para 2021
for (i in 1:2) {
  cat(paste("Pesos en el Componente", i, "para 2021:\n"))
  print(pca2_2021$weights[, i])
  cat("\n")
}

# Nuevas variables obtenidas (scores)
pca2_2020$scores
pca2_2021$scores
