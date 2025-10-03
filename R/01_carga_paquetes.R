# ============================
# INSTALAR Y CARGAR PAQUETES
# ============================

# Instalar solo si no están instalados
paquetes <- c("haven", "dplyr", "labelled", "caret", "Hmisc", 
              "ggcorrplot", "ggplot2", "reshape2", "pROC", 
              "MASS", "randomForest", "cluster", "factoextra", "grid")

paquetes_instalados <- paquetes %in% rownames(installed.packages())
if (any(!paquetes_instalados)) {
  install.packages(paquetes[!paquetes_instalados])
}

# Cargar librerías
library(haven)
library(dplyr)    
library(labelled)
library(caret)
library(Hmisc)
library(ggcorrplot)
library(ggplot2)
library(reshape2)
library(pROC)
library(MASS)
library(randomForest)
library(cluster)
library(factoextra)
library(grid)
