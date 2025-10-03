# =========================================
# RANDOM FOREST - CONJUNTO DE CCAA
# =========================================

library(randomForest)
library(pROC)
library(caret)
library(ggplot2)

#Crear carpeta para guardar resultados
dir.create("resultados/rf", showWarnings = FALSE, recursive = TRUE)

# Preparar datos
variables_a_excluir <- c("CCAA", "PROV", "SEXO", "impacto_salud_mental", "impacto_raw", "EDAD")
datos_todas <- datos_filtrados_conjunta[, !(names(datos_filtrados_conjunta) %in% variables_a_excluir)]
datos_todas$impacto_salud_mental <- as.factor(datos_filtrados_conjunta$impacto_salud_mental)

# Entrenar modelo Random Forest
set.seed(123)
rf_todas <- randomForest(impacto_salud_mental ~ ., 
                         data = datos_todas, 
                         ntree = 500, 
                         importance = TRUE)


# Matriz de confusión OOB y métricas
cat("=== MATRIZ DE CONFUSIÓN OOB - Conjunto Total ===\n")
print(rf_todas$confusion)

# Extraer matriz OOB solo con clases
matriz_oob <- rf_todas$confusion[, 1:3]  
clases <- rownames(matriz_oob)

# Calcular Precision, Recall y F1-score
metricas_oob <- data.frame(
  Clase = clases,
  Precision = round(diag(matriz_oob) / colSums(matriz_oob), 3),
  Recall = round(diag(matriz_oob) / rowSums(matriz_oob), 3)
)
metricas_oob$F1_Score <- round(2 * metricas_oob$Precision * metricas_oob$Recall / 
                                 (metricas_oob$Precision + metricas_oob$Recall), 3)

cat("\n=== MÉTRICAS A PARTIR DE MATRIZ OOB - Conjunto Total ===\n")
print(metricas_oob)


# Probabilidades OOB y AUC multicategoría
probabilidades_oob <- predict(rf_todas, type = "prob")
roc_multi <- multiclass.roc(datos_todas$impacto_salud_mental, probabilidades_oob)

cat("\n=== AUC MULTICLASE (OOB) - Conjunto Total ===\n")
print(roc_multi$auc)


# Curvas ROC por clase (uno contra todos)
clases <- levels(datos_todas$impacto_salud_mental)
colors <- rainbow(length(clases))

plot(0, 0, type = "n", xlim = c(1,0), ylim = c(0,1),
     xlab = "FPR", ylab = "TPR", main = "Curvas ROC por Clase (OOB) - Conjunto Total")

for (i in seq_along(clases)) {
  etiqueta_binaria <- as.numeric(datos_todas$impacto_salud_mental == clases[i])
  roc_bin <- roc(etiqueta_binaria, probabilidades_oob[, clases[i]])
  lines(1 - roc_bin$specificities, roc_bin$sensitivities, col = colors[i], lwd = 2)
}
legend("bottomright", legend = clases, col = colors, lwd = 2)


# Matriz de confusión sobre entrenamiento
predicciones_rf <- predict(rf_todas, newdata = datos_todas)
conf_matrix_train <- confusionMatrix(predicciones_rf, datos_todas$impacto_salud_mental)
cat("\n=== MATRIZ DE CONFUSIÓN SOBRE TRAINING - Conjunto Total ===\n")
print(conf_matrix_train)


# Importancia de variables y top 10
imp_vars <- importance(rf_todas)
imp_vars_sorted <- imp_vars[order(imp_vars[,1], decreasing = TRUE), ]
top_10 <- head(imp_vars_sorted, 10)

cat("\n=== TOP 10 VARIABLES MÁS IMPORTANTES - Conjunto Total ===\n")
print(top_10)

# Graficar top 10 variables por clase
par(mfrow = c(1,3), mar = c(6,6,4,2))
barplot(top_10[,1], names.arg = rownames(top_10), las = 2, col = "skyblue", 
        main = "Alta Afectación", ylab = "Importancia")
barplot(top_10[,2], names.arg = rownames(top_10), las = 2, col = "skyblue", 
        main = "Moderada Afectación", ylab = "Importancia")
barplot(top_10[,3], names.arg = rownames(top_10), las = 2, col = "skyblue", 
        main = "Baja Afectación", ylab = "Importancia")
par(mfrow = c(1,1)) # reset layout


# Evolución del error OOB
plot(rf_todas)

# Matriz de confusión OOB
write.csv(rf_todas$confusion, "resultados/rf/matriz_confusion_oob_conjunto_ccaa.csv", row.names = TRUE)

# Métricas OOB (Precision, Recall, F1)
write.csv(metricas_oob, "resultados/rf/metricas_oob_conjunto_ccaa.csv", row.names = FALSE)

# AUC multicategoría OOB
auc_oob_df <- data.frame(AUC = as.numeric(roc_multi$auc))
write.csv(auc_oob_df, "resultados/rf/auc_oob_conjunto_ccaa.csv", row.names = FALSE)

# Curvas ROC por clase OOB
png("resultados/rf/curvas_roc_oob_conjunto_ccaa.png", width = 900, height = 700)
plot(0, 0, type = "n", xlim = c(1,0), ylim = c(0,1),
     xlab = "FPR", ylab = "TPR", main = "Curvas ROC por Clase (OOB) - Conjunto Total")
for (i in seq_along(clases)) {
  etiqueta_binaria <- as.numeric(datos_todas$impacto_salud_mental == clases[i])
  roc_bin <- roc(etiqueta_binaria, probabilidades_oob[, clases[i]])
  lines(1 - roc_bin$specificities, roc_bin$sensitivities, col = colors[i], lwd = 2)
}
legend("bottomright", legend = clases, col = colors, lwd = 2)
dev.off()

# Matriz de confusión sobre los datos de entrenamiento
write.csv(as.data.frame(conf_matrix_train$table), "resultados/rf/matriz_confusion_train_conjunto_ccaa.csv", row.names = TRUE)

# Importancia de variables
write.csv(imp_vars_sorted, "resultados/rf/importancia_variables_conjunto_ccaa.csv", row.names = TRUE)

# Graficar top 10 variables por clase
top_vars <- rownames(top_10)

# Alta Afectación
png("resultados/rf/top10_alta_conjunto_ccaa.png", width = 800, height = 600)
barplot(top_10[,1], names.arg = top_vars, las = 2, col = "skyblue",
        main = "Top 10 Variables - Alta Afectación (Conjunto Total)", ylab = "Importancia", cex.names = 0.7)
dev.off()

# Moderada Afectación
png("resultados/rf/top10_moderada_conjunto_ccaa.png", width = 800, height = 600)
barplot(top_10[,2], names.arg = top_vars, las = 2, col = "skyblue",
        main = "Top 10 Variables - Moderada Afectación (Conjunto Total)", ylab = "Importancia", cex.names = 0.7)
dev.off()

# Baja Afectación
png("resultados/rf/top10_baja_conjunto_ccaa.png", width = 800, height = 600)
barplot(top_10[,3], names.arg = top_vars, las = 2, col = "skyblue",
        main = "Top 10 Variables - Baja Afectación (Conjunto Total)", ylab = "Importancia", cex.names = 0.7)
dev.off()

# Evolución del error OOB
png("resultados/rf/error_oob_conjunto_ccaa.png", width = 800, height = 600)
plot(rf_todas, main = "Evolución del Error OOB - Conjunto Total")
dev.off()