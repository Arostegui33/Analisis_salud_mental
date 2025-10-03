# =========================================
# RANDOM FOREST - ANDALUCÍA
# =========================================

library(randomForest)
library(pROC)
library(caret)

# Crear carpetas para guardar
if(!dir.exists("resultados/rf")) dir.create("resultados/rf", recursive = TRUE)

# Preparar datos
datos_and$impacto_salud_mental <- as.factor(datos_and$impacto_salud_mental)

# Entrenar modelo Random Forest
set.seed(123)
rf_andalucia <- randomForest(
  impacto_salud_mental ~ ., 
  data = datos_and,
  ntree = 500,
  importance = TRUE
)

# Matriz de confusión OOB y métricas
cat("=== MATRIZ DE CONFUSIÓN OOB ===\n")
print(rf_andalucia$confusion)

# Extraer matriz OOB solo con clases
matriz_oob <- rf_andalucia$confusion[, 1:3]  
clases <- rownames(matriz_oob)

# Calcular Precision, Recall y F1-score
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

cat("\n=== MÉTRICAS A PARTIR DE MATRIZ OOB ===\n")
print(metricas_oob)

# Probabilidades OOB y AUC multicategoría
probabilidades_oob <- predict(rf_andalucia, type = "prob")
roc_multi <- multiclass.roc(datos_and$impacto_salud_mental, probabilidades_oob)
cat("\n=== AUC MULTICLASE (OOB) ===\n")
print(roc_multi$auc)

# Curvas ROC por clase
clases <- levels(datos_and$impacto_salud_mental)
colors <- rainbow(length(clases))

plot(0, 0, type = "n", xlim = c(1, 0), ylim = c(0, 1),
     xlab = "FPR", ylab = "TPR",
     main = "Curvas ROC por Clase (OOB)")

roc_list <- list()
for (i in seq_along(clases)) {
  clase <- clases[i]
  etiqueta_binaria <- as.numeric(datos_and$impacto_salud_mental == clase)
  predicciones_clase <- probabilidades_oob[, clase]
  roc_bin <- roc(etiqueta_binaria, predicciones_clase)
  roc_list[[clase]] <- roc_bin
  lines(1 - roc_bin$specificities, roc_bin$sensitivities, col = colors[i], lwd = 2)
}
legend("bottomright", legend = clases, col = colors, lwd = 2)

# Matriz de confusión sobre los datos de entrenamiento
predicciones_rf <- predict(rf_andalucia, newdata = datos_and)
conf_matrix_train <- confusionMatrix(predicciones_rf, datos_and$impacto_salud_mental)
cat("\n=== MATRIZ DE CONFUSIÓN SOBRE TRAINING ===\n")
print(conf_matrix_train)

# Importancia de variables
imp_vars <- importance(rf_andalucia)
imp_vars_sorted <- imp_vars[order(imp_vars[,1], decreasing = TRUE), ]

cat("\n=== TOP 10 VARIABLES MÁS IMPORTANTES - ANDALUCÍA ===\n")
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
plot(rf_andalucia)

# Matriz de confusión OOB
write.csv(rf_andalucia$confusion, "resultados/rf/matriz_confusion_oob_andalucia.csv", row.names = TRUE)

# Métricas OOB (Precision, Recall, F1)
write.csv(metricas_oob, "resultados/rf/metricas_oob_andalucia.csv", row.names = FALSE)

# AUC multicategoría OOB
auc_oob_df <- data.frame(AUC = as.numeric(roc_multi$auc))
write.csv(auc_oob_df, "resultados/rf/auc_oob_andalucia.csv", row.names = FALSE)

# Curvas ROC por clase OOB
png("resultados/rf/curvas_roc_oob_andalucia.png", width = 900, height = 700)
plot(0, 0, type = "n", xlim = c(1, 0), ylim = c(0, 1),
     xlab = "FPR", ylab = "TPR",
     main = "Curvas ROC por Clase (OOB) - Andalucía")
for (i in seq_along(clases)) {
  lines(1 - roc_list[[clases[i]]]$specificities, roc_list[[clases[i]]]$sensitivities, col = colors[i], lwd = 2)
}
legend("bottomright", legend = clases, col = colors, lwd = 2)
dev.off()

# Matriz de confusión sobre training
write.csv(as.data.frame(conf_matrix_train$table), "resultados/rf/matriz_confusion_train_andalucia.csv", row.names = TRUE)

# Importancia de variables
write.csv(imp_vars_sorted, "resultados/rf/importancia_variables_andalucia.csv", row.names = TRUE)

# Graficar top 10 variables por clase
top_vars <- rownames(top_10)
# Alta Afectación
png("resultados/rf/top10_alta_andalucia.png", width = 800, height = 600)
barplot(top_10[,1], names.arg = top_vars, las = 2, col = "skyblue",
        main = "Top 10 Variables - Alta Afectación (Andalucía)", ylab = "Importancia", cex.names = 0.7)
dev.off()

# Moderada Afectación
png("resultados/rf/top10_moderada_andalucia.png", width = 800, height = 600)
barplot(top_10[,2], names.arg = top_vars, las = 2, col = "skyblue",
        main = "Top 10 Variables - Moderada Afectación (Andalucía)", ylab = "Importancia", cex.names = 0.7)
dev.off()

# Baja Afectación
png("resultados/rf/top10_baja_andalucia.png", width = 800, height = 600)
barplot(top_10[,3], names.arg = top_vars, las = 2, col = "skyblue",
        main = "Top 10 Variables - Baja Afectación (Andalucía)", ylab = "Importancia", cex.names = 0.7)
dev.off()

# Evolución del error OOB
png("resultados/rf/error_oob_andalucia.png", width = 800, height = 600)
plot(rf_andalucia, main = "Evolución del Error OOB - Andalucía")
dev.off()
