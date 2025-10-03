# =========================================
# RANDOM FOREST - GALICIA
# =========================================

library(randomForest)
library(pROC)
library(caret)

# Crear carpeta para guardar datos
dir.create("resultados/rf", showWarnings = FALSE, recursive = TRUE)

# Preparar datos
datos_gl$impacto_salud_mental <- as.factor(datos_gl$impacto_salud_mental)

# Entrenar modelo Random Forest
set.seed(123)
rf_gl <- randomForest(
  impacto_salud_mental ~ ., 
  data = datos_gl,
  ntree = 500,
  importance = TRUE
)

# Matriz de confusión OOB y métricas
cat("=== MATRIZ DE CONFUSIÓN OOB - Galicia ===\n")
print(rf_gl$confusion)

matriz_oob <- rf_gl$confusion[, 1:3]
clases <- rownames(matriz_oob)

precision <- recall <- f1 <- numeric(length(clases))
for (i in seq_along(clases)) {
  tp <- matriz_oob[i, i]
  fp <- sum(matriz_oob[-i, i])
  fn <- sum(matriz_oob[i, -i])
  
  precision[i] <- tp / (tp + fp)
  recall[i]    <- tp / (tp + fn)
  f1[i]        <- 2 * precision[i] * recall[i] / (precision[i] + recall[i])
}

metricas_oob <- data.frame(
  Clase = clases,
  Precision = round(precision, 3),
  Recall = round(recall, 3),
  F1_Score = round(f1, 3)
)
cat("\n=== MÉTRICAS A PARTIR DE MATRIZ OOB - Galicia ===\n")
print(metricas_oob)

# Probabilidades OOB y AUC multicategoría
probabilidades_oob <- predict(rf_gl, type = "prob")
roc_multi <- multiclass.roc(datos_gl$impacto_salud_mental, probabilidades_oob)
cat("\n=== AUC MULTICLASE (OOB) - Galicia ===\n")
print(roc_multi$auc)

# Curvas ROC por clase
clases <- levels(datos_gl$impacto_salud_mental)
colors <- rainbow(length(clases))

plot(0, 0, type = "n", xlim = c(1, 0), ylim = c(0, 1),
     xlab = "FPR", ylab = "TPR",
     main = "Curvas ROC por Clase (OOB) - Galicia")

roc_list <- list()
for (i in seq_along(clases)) {
  clase <- clases[i]
  etiqueta_binaria <- as.numeric(datos_gl$impacto_salud_mental == clase)
  predicciones_clase <- probabilidades_oob[, clase]
  roc_bin <- roc(etiqueta_binaria, predicciones_clase)
  roc_list[[clase]] <- roc_bin
  lines(1 - roc_bin$specificities, roc_bin$sensitivities, col = colors[i], lwd = 2)
}
legend("bottomright", legend = clases, col = colors, lwd = 2)

# Matriz de confusión sobre los datos de entrenamiento
predicciones_rf <- predict(rf_gl, newdata = datos_gl)
conf_matrix_train <- confusionMatrix(predicciones_rf, datos_gl$impacto_salud_mental)
cat("\n=== MATRIZ DE CONFUSIÓN SOBRE TRAINING - Galicia ===\n")
print(conf_matrix_train)

# Importancia de variables
imp_vars <- importance(rf_gl)
imp_vars_sorted <- imp_vars[order(imp_vars[,1], decreasing = TRUE), ]

cat("\n=== TOP 10 VARIABLES MÁS IMPORTANTES - Galicia ===\n")
print(head(imp_vars_sorted, 10))

# Graficar top 10 variables por clase
top_10 <- head(imp_vars_sorted, 10)

barplot(top_10[,1], names.arg = rownames(top_10), las = 2, 
        col = "skyblue", main = "Top 10 Variables - Alta Afectación", 
        ylab = "Importancia", cex.names = 0.7)

barplot(top_10[,2], names.arg = rownames(top_10), las = 2, 
        col = "skyblue", main = "Top 10 Variables - Moderada Afectación", 
        ylab = "Importancia", cex.names = 0.7)

barplot(top_10[,3], names.arg = rownames(top_10), las = 2, 
        col = "skyblue", main = "Top 10 Variables - Baja Afectación", 
        ylab = "Importancia", cex.names = 0.7)

# Evolución del error OOB
plot(rf_gl)

# Matriz de confusión OOB
write.csv(rf_gl$confusion, "resultados/rf/matriz_confusion_oob_galicia.csv", row.names = TRUE)

# Métricas OOB (Precision, Recall, F1)
write.csv(metricas_oob, "resultados/rf/metricas_oob_galicia.csv", row.names = FALSE)

# AUC multicategoría OOB
auc_oob_df <- data.frame(AUC = as.numeric(roc_multi$auc))
write.csv(auc_oob_df, "resultados/rf/auc_oob_galicia.csv", row.names = FALSE)

# Curvas ROC por clase OOB
png("resultados/rf/curvas_roc_oob_galicia.png", width = 900, height = 700)
plot(0, 0, type = "n", xlim = c(1, 0), ylim = c(0, 1),
     xlab = "FPR", ylab = "TPR",
     main = "Curvas ROC por Clase (OOB) - Galicia")
for (i in seq_along(clases)) {
  lines(1 - roc_list[[clases[i]]]$specificities, roc_list[[clases[i]]]$sensitivities, col = colors[i], lwd = 2)
}
legend("bottomright", legend = clases, col = colors, lwd = 2)
dev.off()

# Matriz de confusión sobre los datos de entrenamiento
write.csv(as.data.frame(conf_matrix_train$table), "resultados/rf/matriz_confusion_train_galicia.csv", row.names = TRUE)

# Importancia de variables
write.csv(imp_vars_sorted, "resultados/rf/importancia_variables_galicia.csv", row.names = TRUE)

# Graficar top 10 variables por clase
top_vars <- rownames(top_10)

# Alta Afectación
png("resultados/rf/top10_alta_galicia.png", width = 800, height = 600)
barplot(top_10[,1], names.arg = top_vars, las = 2, col = "skyblue",
        main = "Top 10 Variables - Alta Afectación (Galicia)", ylab = "Importancia", cex.names = 0.7)
dev.off()

# Moderada Afectación
png("resultados/rf/top10_moderada_galicia.png", width = 800, height = 600)
barplot(top_10[,2], names.arg = top_vars, las = 2, col = "skyblue",
        main = "Top 10 Variables - Moderada Afectación (Galicia)", ylab = "Importancia", cex.names = 0.7)
dev.off()

# Baja Afectación
png("resultados/rf/top10_baja_galicia.png", width = 800, height = 600)
barplot(top_10[,3], names.arg = top_vars, las = 2, col = "skyblue",
        main = "Top 10 Variables - Baja Afectación (Galicia)", ylab = "Importancia", cex.names = 0.7)
dev.off()

# Evolución del error OOB
png("resultados/rf/error_oob_galicia.png", width = 800, height = 600)
plot(rf_gl, main = "Evolución del Error OOB - Galicia")
dev.off()