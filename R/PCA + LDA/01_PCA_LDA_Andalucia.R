# ==============================
# PCA - Andalucía
# ==============================

# Crear carpetas para guardar resultados
if(!dir.exists("resultados/pca")) dir.create("resultados/pca", recursive = TRUE)
if(!dir.exists("resultados/lda")) dir.create("resultados/lda", recursive = TRUE)

# Filtrar datos Andalucía 
datos_and <- datos_filtrados[1:327, ]

# Convertir variables a numéricas 
datos_and$P3  <- as.numeric(as.character(datos_and$P3))
datos_and$P3A <- as.numeric(as.character(datos_and$P3A))

# Selección de variables numéricas relevantes para PCA
vars_pca_and <- c(
  "P1","P2","P3","P3A","P5_1","P5_2","P5_3","P5_4","P6_1","P6_2","P6_3","P6_4","P6_5","P6_6","P6_7",
  "P7","P8","P9","P10",
  paste0("P11_", 1:9),
  paste0("P12_", 1:15),
  "P13_dolores","P14A","P14B","P16A","P16B","P18","P26","P27","P28","P29"
)

# Subconjunto y estandarización
datos_and_num <- datos_and[, vars_pca_and]
datos_estandarizados_and <- scale(datos_and_num, center = TRUE, scale = TRUE)

# PCA
pca_and <- prcomp(datos_estandarizados_and, center = TRUE, scale. = TRUE)

# Varianza explicada
varianza_explicada <- pca_and$sdev^2 / sum(pca_and$sdev^2)

# Scree plot
plot(varianza_explicada, type = "b", main = "Scree Plot - Andalucía",
     xlab = "Componente Principal", ylab = "Varianza Explicada",
     col = "blue", pch = 19)
abline(v = 15, col = "red", lty = 2)  

# Resumen PCA
summary(pca_and)

# Loadings y scores para PC1-PC15
loadings_and <- pca_and$rotation[, 1:15]
scores_and   <- pca_and$x[, 1:15]

# Dispersión en espacio PC1-PC15
library(ggplot2)
ggplot(as.data.frame(scores_and), aes(x = PC1, y = PC15)) +
  geom_point(color = "darkorange", size = 3, alpha = 0.7) +
  labs(title = "PCA: Dispersión PC1 vs PC15 (Andalucía)") +
  theme_minimal()

# Heatmap correlaciones Variables - PCs
library(reshape2)
cor_matrix_and   <- cor(datos_estandarizados_and, scores_and)
heatmap_data_and <- melt(cor_matrix_and, varnames = c("Variable","Componente"), value.name = "Correlacion")

ggplot(subset(heatmap_data_and, Componente %in% paste0("PC", 1:15)),
       aes(x = Componente, y = reorder(Variable, Correlacion), fill = Correlacion)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  labs(title = "Correlaciones Variables - PC1 a PC15") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 6))

# Top 5 loadings por PC

# Convertir rownames a columna antes de melt
loadings_and_df <- as.data.frame(loadings_and)
loadings_and_df$Variable <- rownames(loadings_and_df)

# Melt con columna correcta
loadings_long <- melt(loadings_and_df, 
                      id.vars = "Variable", 
                      variable.name = "PC", 
                      value.name = "Loading")

# Seleccionar top 5 loadings por PC
top_loadings <- loadings_long %>%
  group_by(PC) %>%
  slice_max(order_by = abs(Loading), n = 5) %>%
  ungroup()

ggplot(top_loadings, aes(x = Variable, y = Loading, fill = Loading)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(Loading, 2)), 
            position = position_stack(vjust = 0.5), size = 2) +  # posición segura
  coord_flip() +
  facet_wrap(~ PC, scales = "free_y", ncol = 3) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  labs(title = "Top 5 Loadings PC1-PC15 (Andalucía)") +
  theme_minimal(base_size = 9)

# ==============================
# Preparar variable de respuesta para LDA
# ==============================

excluir_vars <- c("CCAA", "PROV", "SEXO", "EDAD")
cols_num <- names(Filter(is.numeric, datos_and[, setdiff(names(datos_and), excluir_vars)]))
datos_and$impacto_raw <- rowSums(datos_and[, cols_num], na.rm = TRUE)

datos_and$impacto_salud_mental <- cut(datos_and$impacto_raw,
                                      breaks = c(-Inf, 90, 140, Inf),
                                      labels = c("Alta Afectación","Moderada Afectación","Baja Afectación"))

scores_and <- as.data.frame(pca_and$x[, 1:15])
scores_and$impacto_salud_mental <- datos_and$impacto_salud_mental



# ==============================
# LDA - Andalucía
# ==============================

library(MASS)
library(caret)
library(pROC)

# Crear modelo LDA usando los scores de PCA y la variable de impacto
lda_and <- lda(impacto_salud_mental ~ ., data = scores_and)

# Resumen del modelo
summary(lda_and)

# Proporción de varianza explicada por cada función discriminante
prop_var <- lda_and$svd^2 / sum(lda_and$svd^2)
names(prop_var) <- paste0("LD", seq_along(prop_var))
cat("=== Proporción de varianza explicada por cada función discriminante ===\n")
print(round(100 * prop_var, 2))

# Coeficientes de LD1
coef_ld1 <- lda_and$scaling[,1]
cat("=== Ecuación LD1 ===\nLD1 = ")
cat(round(coef_ld1[1],4), "*", names(coef_ld1)[1])
for(i in 2:length(coef_ld1)) {
  signo <- ifelse(coef_ld1[i] >= 0, "+", "-")
  cat(" ", signo, " ", abs(round(coef_ld1[i],4)), "*", names(coef_ld1)[i])
}
cat("\n")

# Predicciones
pred_lda_and <- predict(lda_and)

# Matriz de confusión
confusion_and <- table(Real = scores_and$impacto_salud_mental, 
                       Predicho = pred_lda_and$class)
print(confusion_and)

# Métricas con caret
conf_matrix_and <- confusionMatrix(pred_lda_and$class, scores_and$impacto_salud_mental)
cat("=== Matriz de Confusión y Métricas ===\n")
print(conf_matrix_and)
cat("\nPrecision por clase:\n")
print(conf_matrix_and$byClass[,"Precision"])
cat("\nRecall por clase:\n")
print(conf_matrix_and$byClass[,"Recall"])
cat("\nF1-score por clase:\n")
print(conf_matrix_and$byClass[,"F1"])

# AUC multiclasificación
roc_multiclass <- multiclass.roc(scores_and$impacto_salud_mental, pred_lda_and$posterior)
cat("\n=== AUC multiclasificación ===\n")
print(roc_multiclass$auc)

# Graficar curvas ROC por clase
cols <- rainbow(length(levels(scores_and$impacto_salud_mental)))
plot(0,0,type="n", xlim=c(1,0), ylim=c(0,1),
     xlab="Tasa de Falsos Positivos (1 - Especificidad)",
     ylab="Tasa de Verdaderos Positivos (Sensibilidad)",
     main="Curvas ROC por Clase")
for (i in seq_along(levels(scores_and$impacto_salud_mental))) {
  cl <- levels(scores_and$impacto_salud_mental)[i]
  roc_i <- roc(response = as.numeric(scores_and$impacto_salud_mental == cl), 
               predictor = pred_lda_and$posterior[, cl])
  lines(roc_i, col = cols[i], lwd = 2)
  auc_i <- auc(roc_i)
  cat(paste("AUC para clase", cl, "=", round(auc_i,3)), "\n")
}
legend("bottomright", legend = levels(scores_and$impacto_salud_mental), col = cols, lwd = 2)

# Coeficientes de la función discriminante 1
coef_lda <- lda_and$scaling[, 1]
ord <- order(abs(coef_lda), decreasing = TRUE)
cat("\n=== Coeficientes variables LD1 ===\n")
print(coef_lda[ord])

# Top 5 loadings PCA relevantes por PCs importantes
pcs_importantes <- c("PC1","PC4","PC2","PC8")
for (pc in pcs_importantes) {
  cat("\n=== Top 5 loadings para", pc, "===\n")
  loadings <- pca_and$rotation[, pc]
  ord <- order(abs(loadings), decreasing = TRUE)
  print(loadings[ord][1:5])
}

# Gráfico de coeficientes LD1
coef_ordenado <- coef_lda[order(coef_lda, decreasing = TRUE)]
barplot(coef_ordenado,
        horiz = TRUE,
        las = 1,
        col = ifelse(coef_ordenado > 0, "steelblue","firebrick"),
        main = "Coeficientes LD1",
        xlab = "Valor del coeficiente",
        cex.names = 0.7)

# Dispersión LD1 vs LD2
lda_df <- as.data.frame(pred_lda_and$x)
lda_df$Clase <- scores_and$impacto_salud_mental

ggplot(lda_df, aes(x = LD1, y = LD2, color = Clase)) +
  geom_point(size = 3, alpha = 0.7) +
  labs(title = "Representación LDA: Andalucía", x = "LD1", y = "LD2") +
  theme_minimal()

# Varianza explicada
write.csv(varianza_explicada, "resultados/pca/varianza_explicada_andalucia.csv", row.names = TRUE)

# Loadings
write.csv(loadings_and, "resultados/pca/loadings_andalucia.csv", row.names = TRUE)

# Scores
write.csv(scores_and, "resultados/pca/scores_andalucia.csv", row.names = FALSE)

# Scree plot
png("resultados/pca/screeplot_andalucia.png", width = 800, height = 600)
plot(varianza_explicada, type = "b", main = "Scree Plot - Andalucía",
     xlab = "Componente Principal", ylab = "Varianza Explicada",
     col = "blue", pch = 19)
abline(v = 15, col = "red", lty = 2)
dev.off()

# Dispersión PC1 vs PC15
png("resultados/pca/dispersion_pc1_pc15_andalucia.png", width = 800, height = 600)
ggplot(as.data.frame(scores_and), aes(x = PC1, y = PC15)) +
  geom_point(color = "darkorange", size = 3, alpha = 0.7) +
  labs(title = "PCA: Dispersión PC1 vs PC15 (Andalucía)") +
  theme_minimal()
dev.off()

# Heatmap correlaciones
png("resultados/pca/heatmap_corr_andalucia.png", width = 900, height = 700)
ggplot(subset(heatmap_data_and, Componente %in% paste0("PC", 1:15)),
       aes(x = Componente, y = reorder(Variable, Correlacion), fill = Correlacion)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  labs(title = "Correlaciones Variables - PC1 a PC15") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 6))
dev.off()

# Matriz de confusión y métricas
write.csv(as.data.frame(conf_matrix_and$table), "resultados/lda/matriz_confusion_andalucia.csv", row.names = TRUE)

# Coeficientes LD1
write.csv(coef_lda, "resultados/lda/coef_ld1_andalucia.csv", row.names = TRUE)

# Barplot coef LD1
png("resultados/lda/barplot_coef_ld1_andalucia.png", width = 800, height = 600)
barplot(coef_ordenado,
        horiz = TRUE,
        las = 1,
        col = ifelse(coef_ordenado > 0, "steelblue","firebrick"),
        main = "Coeficientes LD1",
        xlab = "Valor del coeficiente",
        cex.names = 0.7)
dev.off()

# Dispersión LD1 vs LD2
png("resultados/lda/dispersion_ld1_ld2_andalucia.png", width = 800, height = 600)
ggplot(lda_df, aes(x = LD1, y = LD2, color = Clase)) +
  geom_point(size = 3, alpha = 0.7) +
  labs(title = "Representación LDA: Andalucía", x = "LD1", y = "LD2") +
  theme_minimal()
dev.off()
