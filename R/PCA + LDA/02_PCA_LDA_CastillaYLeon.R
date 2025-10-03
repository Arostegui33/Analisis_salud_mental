# ==============================
# PCA - Castilla y León
# ==============================

# Crear carpetas para guardar resultados
if(!dir.exists("resultados/pca")) dir.create("resultados/pca", recursive = TRUE)
if(!dir.exists("resultados/lda")) dir.create("resultados/lda", recursive = TRUE)

# Filtrar solo Castilla y León
datos_cl <- datos_filtrados[328:427, ]

# Convertir variables P3, P3A a numéricas
datos_cl$P3 <- as.numeric(as.character(datos_cl$P3))
datos_cl$P3A <- as.numeric(as.character(datos_cl$P3A))

# Seleccionar variables numéricas relevantes para PCA
vars_pca_cl <- c("P1", "P2", "P3", "P3A",
                 "P5_1","P5_2","P5_3","P5_4",
                 "P6_1","P6_2","P6_3","P6_4","P6_5","P6_6","P6_7",
                 "P7","P8","P9","P10",
                 "P11_1","P11_2","P11_3","P11_4","P11_5","P11_6","P11_7","P11_8","P11_9",
                 "P12_1","P12_2","P12_3","P12_4","P12_5","P12_6","P12_7","P12_8","P12_9","P12_10",
                 "P12_11","P12_12","P12_13","P12_14","P12_15",
                 "P13_dolores","P14A","P14B","P16A","P16B","P18","P26","P27","P28","P29")

datos_cl_num <- datos_cl[, vars_pca_cl]

# Estandarizar variables
datos_estandarizados_cl <- scale(datos_cl_num)

# Realizar PCA
pca_cl <- prcomp(datos_estandarizados_cl, center = TRUE, scale. = TRUE)

# Varianza explicada por cada componente
varianza_explicada_cl <- pca_cl$sdev^2 / sum(pca_cl$sdev^2)

# Scree plot
plot(varianza_explicada_cl, type = "b", main = "Scree Plot - Castilla y León",
     xlab = "Componente Principal", ylab = "Proporción de Varianza Explicada",
     col = "blue", pch = 19)
abline(v = 15, col = "red", lty = 2)

# Resumen PCA
summary(pca_cl)

# Loadings PC1 a PC15
loadings_cl <- pca_cl$rotation[, 1:15]
scores_cl <- pca_cl$x[, 1:15]

# Gráfico dispersión PC1 vs PC15
ggplot(as.data.frame(scores_cl), aes(x = PC1, y = PC15)) +
  geom_point(color = "darkgreen", size = 3, alpha = 0.7) +
  labs(title = "PCA: Dispersión Observaciones PC1 vs PC15 (Castilla y León)",
       x = "PC1", y = "PC15") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Correlación variables originales y PCs
cor_matrix_cl <- cor(datos_estandarizados_cl, scores_cl)
heatmap_data_cl <- reshape2::melt(cor_matrix_cl, varnames = c("Variable","Componente"), value.name = "Correlacion")

ggplot(heatmap_data_cl[heatmap_data_cl$Componente %in% paste0("PC", 1:15), ],
       aes(x = Componente, y = reorder(Variable, Correlacion), fill = Correlacion)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  labs(title = "Heatmap de Correlaciones Variables-PCs (PC1 a PC15)",
       x = "Componente Principal", y = "Variable", fill = "Correlación") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 6), plot.title = element_text(hjust = 0.5))

# Top 5 loadings por PC
loadings_cl_df <- as.data.frame(loadings_cl)
loadings_cl_df$Variable <- rownames(loadings_cl_df)
loadings_long_cl <- reshape2::melt(loadings_cl_df, id.vars = "Variable",
                                   variable.name = "PC", value.name = "Loading")

library(dplyr)
top_loadings_cl <- loadings_long_cl %>%
  group_by(PC) %>%
  slice_max(order_by = abs(Loading), n = 5) %>%
  ungroup() %>%
  group_by(PC) %>%
  mutate(Variable = factor(Variable, levels = Variable[order(Loading)])) %>%
  ungroup()

ggplot(top_loadings_cl, aes(x = Variable, y = Loading, fill = Loading)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(Loading, 2)), 
            position = position_stack(vjust = 0.5), size = 2) +
  coord_flip() +
  facet_wrap(~ PC, scales = "free_y", ncol = 3) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  labs(title = "Top 5 Loadings PC1-PC15 (Castilla y León)") +
  theme_minimal(base_size = 9)

# ==============================
# Preparar variable de respuesta para LDA
# ==============================

excluir_vars <- c("CCAA", "PROV", "SEXO", "EDAD")
cols_num <- names(Filter(is.numeric, datos_cl[, setdiff(names(datos_cl), excluir_vars)]))

# Crear impacto_raw solo con variables numéricas
datos_cl$impacto_raw <- rowSums(datos_cl[, cols_num], na.rm = TRUE)

# Categorizar impacto_raw
datos_cl$impacto_salud_mental <- cut(
  datos_cl$impacto_raw,
  breaks = c(-Inf, 90, 140, Inf),
  labels = c("Alta Afectación","Moderada Afectación","Baja Afectación")
)

# Preparar scores para LDA
scores_cl <- as.data.frame(pca_cl$x[, 1:15])
scores_cl$impacto_salud_mental <- datos_cl$impacto_salud_mental

# ==============================
# LDA - Castilla y León
# ==============================

library(MASS)
lda_cl <- lda(impacto_salud_mental ~ ., data = scores_cl)
summary(lda_cl)

# Proporción de varianza explicada
prop_var_cl <- lda_cl$svd^2 / sum(lda_cl$svd^2)
names(prop_var_cl) <- paste0("LD", seq_along(prop_var_cl))
cat("=== Proporción de varianza explicada por cada función discriminante ===\n")
print(round(100*prop_var_cl,2))

# Ecuación LD1
coef_ld1 <- lda_cl$scaling[,1]
cat("LD1 = ", round(coef_ld1[1],4), "*", names(coef_ld1)[1])
for(i in 2:length(coef_ld1)) {
  signo <- ifelse(coef_ld1[i]>=0, "+","-")
  cat(" ", signo," ", abs(round(coef_ld1[i],4)),"*", names(coef_ld1)[i])
}
cat("\n")

# Predicciones y matriz de confusión
pred_lda_cl <- predict(lda_cl)
real <- scores_cl$impacto_salud_mental
pred <- pred_lda_cl$class
prob_lda <- pred_lda_cl$posterior

library(caret)
conf_matrix_cl <- confusionMatrix(pred, real)
cat("=== Matriz de Confusión y Métricas ===\n")
print(conf_matrix_cl)
cat("\nPrecisión por clase:\n"); print(conf_matrix_cl$byClass[,"Precision"])
cat("\nRecall por clase:\n"); print(conf_matrix_cl$byClass[,"Recall"])
cat("\nF1-score por clase:\n"); print(conf_matrix_cl$byClass[,"F1"])

# ROC multiclasificación
library(pROC)
roc_multiclass <- multiclass.roc(real, prob_lda)
cat("\n=== AUC Multiclase ===\n"); print(roc_multiclass$auc)

# Gráfico ROC
cols <- rainbow(length(levels(real)))
plot(0,0,type="n",xlim=c(1,0),ylim=c(0,1),
     xlab="Tasa de Falsos Positivos (1 - Especificidad)",
     ylab="Tasa de Verdaderos Positivos (Sensibilidad)",
     main="Curvas ROC por Clase")
for(i in seq_along(levels(real))){
  clase <- levels(real)[i]
  roc_i <- roc(response=as.numeric(real==clase), predictor=prob_lda[,clase])
  lines(roc_i,col=cols[i],lwd=2)
  cat(paste("AUC para clase", clase, "=", round(auc(roc_i),3)),"\n")
}
legend("bottomright", legend=levels(real), col=cols, lwd=2)

# Coeficientes LD1
coef_ld1 <- lda_cl$scaling[,1]
ord <- order(abs(coef_ld1), decreasing = TRUE)
cat("\n=== Coeficientes LD1 ===\n")
print(coef_ld1[ord])

# Loadings principales por PC
pcs_importantes <- c("PC1","PC6","PC9","PC4")
for(pc in pcs_importantes){
  cat("\n=== 5 loadings más relevantes para", pc, "===\n")
  loadings <- pca_cl$rotation[,pc]
  ord <- order(abs(loadings), decreasing = T)
  print(head(loadings[ord], 5))
}

# Varianza explicada
write.csv(varianza_explicada_cl, "resultados/pca/varianza_explicada_cl.csv", row.names = TRUE)

# Loadings
write.csv(loadings_cl, "resultados/pca/loadings_cl.csv", row.names = TRUE)

# Scores
write.csv(scores_cl, "resultados/pca/scores_cl.csv", row.names = FALSE)

# Scree plot
png("resultados/pca/screeplot_cl.png", width = 800, height = 600)
plot(varianza_explicada_cl, type = "b", main = "Scree Plot - Castilla y León",
     xlab = "Componente Principal", ylab = "Varianza Explicada",
     col = "blue", pch = 19)
abline(v = 15, col = "red", lty = 2)
dev.off()

# Dispersión PC1 vs PC15
png("resultados/pca/dispersion_pc1_pc15_cl.png", width = 800, height = 600)
ggplot(as.data.frame(scores_cl), aes(x = PC1, y = PC15)) +
  geom_point(color = "darkgreen", size = 3, alpha = 0.7) +
  labs(title = "PCA: Dispersión PC1 vs PC15 (Castilla y León)") +
  theme_minimal()
dev.off()

# Heatmap correlaciones
png("resultados/pca/heatmap_corr_cl.png", width = 900, height = 700)
ggplot(subset(heatmap_data_cl, Componente %in% paste0("PC", 1:15)),
       aes(x = Componente, y = reorder(Variable, Correlacion), fill = Correlacion)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  labs(title = "Correlaciones Variables - PC1 a PC15") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 6))
dev.off()

# Barplot coef LD1
png("resultados/lda/barplot_coef_ld1_cl.png", width = 800, height = 600)
barplot(coef_ld1[ord],
        horiz = TRUE,
        las = 1,
        col = ifelse(coef_ld1[ord] > 0, "steelblue","firebrick"),
        main = "Coeficientes LD1",
        xlab = "Valor del coeficiente",
        cex.names = 0.7)
dev.off()

# Dispersión LD1 vs LD2
png("resultados/lda/dispersion_ld1_ld2_cl.png", width = 800, height = 600)
ggplot(data.frame(LD1 = pred_lda_cl$x[,1], LD2 = pred_lda_cl$x[,2], Clase = scores_cl$impacto_salud_mental),
       aes(x = LD1, y = LD2, color = Clase)) +
  geom_point(size = 3, alpha = 0.7) +
  labs(title = "Representación LDA: Castilla y León", x = "LD1", y = "LD2") +
  theme_minimal()
dev.off()

# Matriz de confusión y métricas
write.csv(as.data.frame(conf_matrix_cl$table), "resultados/lda/matriz_confusion_cl.csv", row.names = TRUE)

# Coeficientes LD1
write.csv(coef_ld1, "resultados/lda/coef_ld1_cl.csv", row.names = TRUE)