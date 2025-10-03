# ==============================
# PCA - GALICIA
# ==============================

# Crear carpetas para guardar resultados
if(!dir.exists("resultados/pca")) dir.create("resultados/pca", recursive = TRUE)
if(!dir.exists("resultados/lda")) dir.create("resultados/lda", recursive = TRUE)

# Filtrar solo Galicia
datos_gl <- subset(datos_filtrados, CCAA == "Galicia")

# Convertir variables P3 y P3A a numéricas
datos_gl$P3 <- as.numeric(as.character(datos_gl$P3))
datos_gl$P3A <- as.numeric(as.character(datos_gl$P3A))

# Seleccionar variables numéricas para PCA
vars_pca_gl <- c("P1", "P2", "P3", "P3A",
                 "P5_1", "P5_2", "P5_3", "P5_4", "P6_1", "P6_2", "P6_3", "P6_4", "P6_5", "P6_6", "P6_7",
                 "P7", "P8", "P9", "P10",
                 "P11_1", "P11_2", "P11_3", "P11_4", "P11_5", "P11_6", "P11_7", "P11_8", "P11_9",
                 "P12_1", "P12_2", "P12_3", "P12_4", "P12_5", "P12_6", "P12_7", "P12_8", "P12_9", "P12_10",
                 "P12_11", "P12_12", "P12_13", "P12_14", "P12_15",
                 "P13_dolores", "P14A", "P14B", "P16A", "P16B", "P18", "P26", "P27", "P28", "P29")
datos_gl_num <- datos_gl[, vars_pca_gl]

# Estandarizar y ejecutar PCA
datos_estandarizados_gl <- scale(datos_gl_num)
pca_gl <- prcomp(datos_estandarizados_gl, center = TRUE, scale. = TRUE)

# Scree plot y varianza explicada
varianza_explicada_gl <- pca_gl$sdev^2 / sum(pca_gl$sdev^2)
plot(varianza_explicada_gl, type = "b", main = "Scree Plot - Galicia",
     xlab = "Componente Principal", ylab = "Proporción de Varianza Explicada",
     col = "blue", pch = 19)
abline(v = 15, col = "red", lty = 2)
summary(pca_gl)

# Dispersión PC1 vs PC15
ggplot(as.data.frame(pca_gl$x[, 1:15]), aes(x = PC1, y = PC15)) +
  geom_point(color = "darkgreen", size = 3, alpha = 0.7) +
  labs(title = "PCA: Dispersión Observaciones PC1 vs PC15 (Galicia)", x = "PC1", y = "PC15") +
  theme_minimal() + theme(plot.title = element_text(hjust = 0.5))

# Heatmap correlaciones Variables-PCs
library(reshape2)
cor_matrix_gl <- cor(datos_estandarizados_gl, pca_gl$x[, 1:15])
heatmap_data_gl <- melt(cor_matrix_gl, varnames = c("Variable", "Componente"), value.name = "Correlacion")
ggplot(heatmap_data_gl[heatmap_data_gl$Componente %in% paste0("PC", 1:15), ],
       aes(x = Componente, y = reorder(Variable, Correlacion), fill = Correlacion)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  labs(title = "Heatmap Variables-PCs (PC1 a PC15)", x = "PC", y = "Variable") +
  theme_minimal() + theme(axis.text.y = element_text(size = 6), plot.title = element_text(hjust = 0.5))

# Loadings top 5 por PC
library(dplyr)
loadings_long_gl <- melt(as.data.frame(pca_gl$rotation[, 1:15]) %>% 
                           mutate(Variable = rownames(.)), 
                         id.vars = "Variable", variable.name = "PC", value.name = "Loading")
top_loadings_gl <- loadings_long_gl %>% 
  group_by(PC) %>% 
  slice_max(order_by = abs(Loading), n = 5) %>%
  mutate(Variable = factor(Variable, levels = Variable[order(Loading)])) %>% 
  ungroup()

ggplot(top_loadings_gl, aes(x = Variable, y = Loading, fill = Loading)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(Loading, 2)), 
            position = position_stack(vjust = 0.5), size = 2) +
  coord_flip() +
  facet_wrap(~ PC, scales = "free_y", ncol = 3) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  labs(title = "Top 5 Loadings PC1-PC15 (Galicia)") +
  theme_minimal(base_size = 9)

# ==============================
# Preparar variable respuesta para LDA
# ==============================
excluir_vars <- c("CCAA", "PROV", "SEXO", "EDAD")
cols_num <- names(Filter(is.numeric, datos_gl[, setdiff(names(datos_gl), excluir_vars)]))
datos_gl$impacto_raw <- rowSums(datos_gl[, cols_num], na.rm = TRUE)
datos_gl$impacto_salud_mental <- cut(datos_gl$impacto_raw,
                                     breaks = c(-Inf, 90, 140, Inf),
                                     labels = c("Alta Afectación", "Moderada Afectación", "Baja Afectación"))

scores_gl <- as.data.frame(pca_gl$x[, 1:15])
scores_gl$impacto_salud_mental <- datos_gl$impacto_salud_mental


# ==============================
# LDA - Galicia
# ==============================

library(MASS)
lda_gl <- lda(impacto_salud_mental ~ ., data = scores_gl)
summary(lda_gl)

# Proporción varianza LD
prop_var_gl <- lda_gl$svd^2 / sum(lda_gl$svd^2)
names(prop_var_gl) <- paste0("LD", seq_along(prop_var_gl))
print(round(100 * prop_var_gl, 2))

# Ecuación LD1
coef_ld1 <- lda_gl$scaling[,1]
cat("LD1 =", paste(round(coef_ld1,4), "*", names(coef_ld1), collapse = " + "), "\n")

# Predicciones, matriz de confusión y métricas
library(caret)
pred_lda_gl <- predict(lda_gl)
conf_matrix <- confusionMatrix(pred_lda_gl$class, scores_gl$impacto_salud_mental)
print(conf_matrix)
print(conf_matrix$byClass[, c("Precision","Recall","F1")])

# AUC multiclasificación
library(pROC)
roc_multiclass <- multiclass.roc(scores_gl$impacto_salud_mental, pred_lda_gl$posterior)
print(roc_multiclass$auc)

# Curvas ROC por clase
cols <- rainbow(length(levels(scores_gl$impacto_salud_mental)))
plot(0, 0, type = "n", xlim = c(1,0), ylim = c(0,1), 
     xlab="1-Especificidad", ylab="Sensibilidad",
     main="Curvas ROC por Clase (Galicia)")
for (i in seq_along(levels(scores_gl$impacto_salud_mental))) {
  cl <- levels(scores_gl$impacto_salud_mental)[i]
  lines(roc(as.numeric(scores_gl$impacto_salud_mental == cl), 
            pred_lda_gl$posterior[, cl]), col=cols[i], lwd=2)
}
legend("bottomright", legend=levels(scores_gl$impacto_salud_mental), col=cols, lwd=2)

# Coeficientes LD1 ordenados y gráfico
coef_ld1_ord <- coef_ld1[order(coef_ld1, decreasing = TRUE)]
barplot(coef_ld1_ord, horiz=TRUE, las=1, 
        col=ifelse(coef_ld1_ord>0,"steelblue","firebrick"),
        main="Coeficientes LD1", xlab="Valor del coeficiente", cex.names=0.7)

# Visualización LDA
lda_df <- as.data.frame(pred_lda_gl$x)
lda_df$Clase <- scores_gl$impacto_salud_mental
ggplot(lda_df, aes(x=LD1, y=LD2, color=Clase)) +
  geom_point(size=3, alpha=0.7) +
  labs(title="Representación LDA: Galicia", x="LD1", y="LD2") +
  theme_minimal()
datos_gl <- subset(datos_filtrados, CCAA == "Galicia")

# Convertir variables P3 y P3A a numéricas
datos_gl$P3 <- as.numeric(as.character(datos_gl$P3))
datos_gl$P3A <- as.numeric(as.character(datos_gl$P3A))

# Seleccionar variables numéricas para PCA
vars_pca_gl <- c("P1", "P2", "P3", "P3A",
                 "P5_1", "P5_2", "P5_3", "P5_4", "P6_1", "P6_2", "P6_3", "P6_4", "P6_5", "P6_6", "P6_7",
                 "P7", "P8", "P9", "P10",
                 "P11_1", "P11_2", "P11_3", "P11_4", "P11_5", "P11_6", "P11_7", "P11_8", "P11_9",
                 "P12_1", "P12_2", "P12_3", "P12_4", "P12_5", "P12_6", "P12_7", "P12_8", "P12_9", "P12_10",
                 "P12_11", "P12_12", "P12_13", "P12_14", "P12_15",
                 "P13_dolores", "P14A", "P14B", "P16A", "P16B", "P18", "P26", "P27", "P28", "P29")
datos_gl_num <- datos_gl[, vars_pca_gl]

# Estandarizar y ejecutar PCA
datos_estandarizados_gl <- scale(datos_gl_num)
pca_gl <- prcomp(datos_estandarizados_gl, center = TRUE, scale. = TRUE)

# Scree plot y varianza explicada
varianza_explicada_gl <- pca_gl$sdev^2 / sum(pca_gl$sdev^2)
plot(varianza_explicada_gl, type = "b", main = "Scree Plot - Galicia",
     xlab = "Componente Principal", ylab = "Proporción de Varianza Explicada",
     col = "blue", pch = 19)
abline(v = 15, col = "red", lty = 2)
summary(pca_gl)

# Dispersión PC1 vs PC15
ggplot(as.data.frame(pca_gl$x[, 1:15]), aes(x = PC1, y = PC15)) +
  geom_point(color = "darkgreen", size = 3, alpha = 0.7) +
  labs(title = "PCA: Dispersión Observaciones PC1 vs PC15 (Galicia)", x = "PC1", y = "PC15") +
  theme_minimal() + theme(plot.title = element_text(hjust = 0.5))

# Heatmap correlaciones Variables-PCs
library(reshape2)
cor_matrix_gl <- cor(datos_estandarizados_gl, pca_gl$x[, 1:15])
heatmap_data_gl <- melt(cor_matrix_gl, varnames = c("Variable", "Componente"), value.name = "Correlacion")
ggplot(heatmap_data_gl[heatmap_data_gl$Componente %in% paste0("PC", 1:15), ],
       aes(x = Componente, y = reorder(Variable, Correlacion), fill = Correlacion)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  labs(title = "Heatmap Variables-PCs (PC1 a PC15)", x = "PC", y = "Variable") +
  theme_minimal() + theme(axis.text.y = element_text(size = 6), plot.title = element_text(hjust = 0.5))

# Loadings top 5 por PC
library(dplyr)
loadings_long_gl <- melt(as.data.frame(pca_gl$rotation[, 1:15]) %>% 
                           mutate(Variable = rownames(.)), 
                         id.vars = "Variable", variable.name = "PC", value.name = "Loading")
top_loadings_gl <- loadings_long_gl %>% 
  group_by(PC) %>% 
  slice_max(order_by = abs(Loading), n = 5) %>%
  mutate(Variable = factor(Variable, levels = Variable[order(Loading)])) %>% 
  ungroup()

ggplot(top_loadings_gl, aes(x = Variable, y = Loading, fill = Loading)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(Loading, 2)), 
            position = position_stack(vjust = 0.5), size = 2) +
  coord_flip() +
  facet_wrap(~ PC, scales = "free_y", ncol = 3) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  labs(title = "Top 5 Loadings PC1-PC15 (Galicia)") +
  theme_minimal(base_size = 9)

# ==============================
# Preparar variable respuesta para LDA
# ==============================
excluir_vars <- c("CCAA", "PROV", "SEXO", "EDAD")
cols_num <- names(Filter(is.numeric, datos_gl[, setdiff(names(datos_gl), excluir_vars)]))
datos_gl$impacto_raw <- rowSums(datos_gl[, cols_num], na.rm = TRUE)
datos_gl$impacto_salud_mental <- cut(datos_gl$impacto_raw,
                                     breaks = c(-Inf, 90, 140, Inf),
                                     labels = c("Alta Afectación", "Moderada Afectación", "Baja Afectación"))

scores_gl <- as.data.frame(pca_gl$x[, 1:15])
scores_gl$impacto_salud_mental <- datos_gl$impacto_salud_mental


# ==============================
# LDA - Galicia
# ==============================

library(MASS)
lda_gl <- lda(impacto_salud_mental ~ ., data = scores_gl)
summary(lda_gl)

# Proporción varianza LD
prop_var_gl <- lda_gl$svd^2 / sum(lda_gl$svd^2)
names(prop_var_gl) <- paste0("LD", seq_along(prop_var_gl))
print(round(100 * prop_var_gl, 2))

# Ecuación LD1
coef_ld1 <- lda_gl$scaling[,1]
cat("LD1 =", paste(round(coef_ld1,4), "*", names(coef_ld1), collapse = " + "), "\n")

# Predicciones, matriz de confusión y métricas
library(caret)
pred_lda_gl <- predict(lda_gl)
conf_matrix <- confusionMatrix(pred_lda_gl$class, scores_gl$impacto_salud_mental)
print(conf_matrix)
print(conf_matrix$byClass[, c("Precision","Recall","F1")])

# AUC multiclasificación
library(pROC)
roc_multiclass <- multiclass.roc(scores_gl$impacto_salud_mental, pred_lda_gl$posterior)
print(roc_multiclass$auc)

# Curvas ROC por clase
cols <- rainbow(length(levels(scores_gl$impacto_salud_mental)))
plot(0, 0, type = "n", xlim = c(1,0), ylim = c(0,1), 
     xlab="1-Especificidad", ylab="Sensibilidad",
     main="Curvas ROC por Clase (Galicia)")
for (i in seq_along(levels(scores_gl$impacto_salud_mental))) {
  cl <- levels(scores_gl$impacto_salud_mental)[i]
  lines(roc(as.numeric(scores_gl$impacto_salud_mental == cl), 
            pred_lda_gl$posterior[, cl]), col=cols[i], lwd=2)
}
legend("bottomright", legend=levels(scores_gl$impacto_salud_mental), col=cols, lwd=2)

# Coeficientes LD1 ordenados y gráfico
coef_ld1_ord <- coef_ld1[order(coef_ld1, decreasing = TRUE)]
barplot(coef_ld1_ord, horiz=TRUE, las=1, 
        col=ifelse(coef_ld1_ord>0,"steelblue","firebrick"),
        main="Coeficientes LD1", xlab="Valor del coeficiente", cex.names=0.7)

# Visualización LDA
lda_df <- as.data.frame(pred_lda_gl$x)
lda_df$Clase <- scores_gl$impacto_salud_mental
ggplot(lda_df, aes(x=LD1, y=LD2, color=Clase)) +
  geom_point(size=3, alpha=0.7) +
  labs(title="Representación LDA: Galicia", x="LD1", y="LD2") +
  theme_minimal()

# Varianza explicada
write.csv(varianza_explicada_gl, "resultados/pca/varianza_explicada_gl.csv", row.names = TRUE)

# Loadings
write.csv(pca_gl$rotation[,1:15], "resultados/pca/loadings_gl.csv", row.names = TRUE)

# Scores
write.csv(scores_gl, "resultados/pca/scores_gl.csv", row.names = FALSE)

# Scree plot
png("resultados/pca/screeplot_gl.png", width = 800, height = 600)
plot(varianza_explicada_gl, type = "b", main = "Scree Plot - Galicia",
     xlab = "Componente Principal", ylab = "Varianza Explicada",
     col = "blue", pch = 19)
abline(v = 15, col = "red", lty = 2)
dev.off()

# Dispersión PC1 vs PC15
png("resultados/pca/dispersion_pc1_pc15_gl.png", width = 800, height = 600)
ggplot(as.data.frame(pca_gl$x[, 1:15]), aes(x = PC1, y = PC15)) +
  geom_point(color = "darkgreen", size = 3, alpha = 0.7) +
  labs(title = "PCA: Dispersión PC1 vs PC15 (Galicia)", x = "PC1", y = "PC15") +
  theme_minimal()
dev.off()

# Heatmap correlaciones
png("resultados/pca/heatmap_corr_gl.png", width = 900, height = 700)
ggplot(subset(heatmap_data_gl, Componente %in% paste0("PC", 1:15)),
       aes(x = Componente, y = reorder(Variable, Correlacion), fill = Correlacion)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  labs(title = "Correlaciones Variables - PC1 a PC15") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 6))
dev.off()

# Barplot coef LD1
png("resultados/lda/barplot_coef_ld1_gl.png", width = 800, height = 600)
barplot(coef_ld1_ord,
        horiz = TRUE,
        las = 1,
        col = ifelse(coef_ld1_ord > 0,"steelblue","firebrick"),
        main = "Coeficientes LD1",
        xlab = "Valor del coeficiente",
        cex.names = 0.7)
dev.off()

# Dispersión LD1 vs LD2
png("resultados/lda/dispersion_ld1_ld2_gl.png", width = 800, height = 600)
ggplot(lda_df, aes(x=LD1, y=LD2, color=Clase)) +
  geom_point(size=3, alpha=0.7) +
  labs(title="Representación LDA: Galicia", x="LD1", y="LD2") +
  theme_minimal()
dev.off()

# Matriz de confusión y métricas
write.csv(as.data.frame(conf_matrix$table), "resultados/lda/matriz_confusion_gl.csv", row.names = TRUE)

# Coeficientes LD1
write.csv(coef_ld1, "resultados/lda/coef_ld1_gl.csv", row.names = TRUE)