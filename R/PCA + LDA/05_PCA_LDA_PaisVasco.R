# ==============================
# PCA - PAÍS VASCO
# ==============================

# Crear carpetas para guardar resultados
if(!dir.exists("resultados/pca")) dir.create("resultados/pca", recursive = TRUE)
if(!dir.exists("resultados/lda")) dir.create("resultados/lda", recursive = TRUE)

# Filtrar País Vasco
datos_pv <- datos_filtrados[813:871, ]

# Convertir variables P3, P3A a numéricas
datos_pv$P3 <- as.numeric(as.character(datos_pv$P3))
datos_pv$P3A <- as.numeric(as.character(datos_pv$P3A))

# Selección de variables numéricas para PCA
vars_pca_pv <- c("P1", "P2", "P3", "P3A",
                 "P5_1", "P5_2", "P5_3", "P5_4",
                 "P6_1", "P6_2", "P6_3", "P6_4", "P6_5", "P6_6", "P6_7",
                 "P7", "P8", "P9", "P10",
                 "P11_1", "P11_2", "P11_3", "P11_4", "P11_5", "P11_6", "P11_7", "P11_8", "P11_9",
                 "P12_1", "P12_2", "P12_3", "P12_4", "P12_5", "P12_6", "P12_7", "P12_8", "P12_9",
                 "P12_10", "P12_11", "P12_12", "P12_13", "P12_14", "P12_15",
                 "P13_dolores", "P14A", "P14B", "P16A", "P16B", "P18", "P26", "P27", "P28", "P29")

datos_pv_num <- datos_pv[, vars_pca_pv]

# Estandarizar variables
datos_estandarizados_pv <- scale(datos_pv_num)

# Seleccionar solo columnas numéricas
cols_num <- names(Filter(is.numeric, datos_pv[, setdiff(names(datos_pv), c("CCAA","PROV","SEXO","EDAD"))]))

# Filtrar columnas con varianza no nula
cols_num <- cols_num[sapply(datos_pv[, cols_num], function(x) var(x, na.rm = TRUE) != 0)]

# Estandarizar
datos_estandarizados_pv <- scale(datos_pv[, cols_num])

# Reemplazar NA por 0 si existen
datos_estandarizados_pv[is.na(datos_estandarizados_pv)] <- 0

# PCA
pca_pv <- prcomp(datos_estandarizados_pv, center = TRUE, scale. = TRUE)

# Scree plot
varianza_explicada_pv <- pca_pv$sdev^2 / sum(pca_pv$sdev^2)
plot(varianza_explicada_pv, type = "b", main = "Scree Plot - País Vasco",
     xlab = "Componente Principal", ylab = "Proporción de Varianza Explicada",
     col = "darkgreen", pch = 19)
abline(v = 15, col = "blue", lty = 2)

# Scores PC1 a PC15
scores_pv <- as.data.frame(pca_pv$x[, 1:15])

# Loadings PC1 a PC15 y top 5 por PC - País Vasco
loadings_pv <- pca_pv$rotation[, 1:15]
loadings_pv_df <- as.data.frame(loadings_pv)
loadings_pv_df$Variable <- rownames(loadings_pv_df)

loadings_long_pv <- melt(loadings_pv_df, id.vars = "Variable",
                         variable.name = "PC", value.name = "Loading")

top_loadings_pv <- loadings_long_pv %>%
  group_by(PC) %>%
  slice_max(order_by = abs(Loading), n = 5) %>%
  ungroup() %>%
  group_by(PC) %>%
  mutate(Variable = factor(Variable, levels = Variable[order(Loading)])) %>%
  ungroup()

ggplot(top_loadings_pv, aes(x = Variable, y = Loading, fill = Loading)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(Loading, 2)), position = position_stack(vjust = 0.5), size = 2) +
  coord_flip() +
  facet_wrap(~ PC, scales = "free_y", ncol = 3) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  theme_minimal(base_size = 9) +
  labs(title = "Top 5 Loadings PC1-PC15 (País Vasco)", x = "Variable", y = "Carga (Loading)")

# ==============================
# Preparar variable de respuesta para LDA
# ==============================
excluir_vars <- c("CCAA", "PROV", "SEXO", "EDAD")
cols_num <- names(Filter(is.numeric, datos_pv[, setdiff(names(datos_pv), excluir_vars)]))
datos_pv$impacto_raw <- rowSums(datos_pv[, cols_num], na.rm = TRUE)
datos_pv$impacto_salud_mental <- cut(datos_pv$impacto_raw,
                                     breaks = c(-Inf, 90, 140, Inf),
                                     labels = c("Alta Afectación","Moderada Afectación","Baja Afectación"))

scores_pv$impacto_salud_mental <- datos_pv$impacto_salud_mental

# ==============================
# LDA País Vasco
# ==============================

library(MASS)
lda_pv <- lda(impacto_salud_mental ~ ., data = scores_pv)

# Resumen y proporción de varianza explicada
summary(lda_pv)
prop_var_pv <- lda_pv$svd^2 / sum(lda_pv$svd^2)
names(prop_var_pv) <- paste0("LD", seq_along(prop_var_pv))
print(round(100 * prop_var_pv, 2))

# Ecuación LD1
coef_ld1 <- lda_pv$scaling[,1]
cat("LD1 = ", paste(round(coef_ld1, 4), "*", names(coef_ld1), collapse = " + "), "\n")

# Predicciones y matriz de confusión
pred_lda_pv <- predict(lda_pv)
library(caret)
real <- scores_pv$impacto_salud_mental
pred <- pred_lda_pv$class
conf_matrix <- confusionMatrix(pred, real)
print(conf_matrix)

# AUC multiclasificación
library(pROC)
roc_multiclass <- multiclass.roc(real, pred_lda_pv$posterior)
print(roc_multiclass$auc)

# Gráfico ROC por clase
cols <- rainbow(length(levels(real)))
plot(0,0,type="n", xlim=c(1,0), ylim=c(0,1),
     xlab="Tasa de Falsos Positivos (1 - Especificidad)",
     ylab="Tasa de Verdaderos Positivos (Sensibilidad)",
     main="Curvas ROC por Clase (País Vasco)")
for (i in seq_along(levels(real))) {
  cl <- levels(real)[i]
  roc_i <- roc(response = as.numeric(real == cl), predictor = pred_lda_pv$posterior[, cl])
  lines(roc_i, col=cols[i], lwd=2)
}
legend("bottomright", legend=levels(real), col=cols, lwd=2)

# Coeficientes discriminantes LD1
coef_lda <- lda_pv$scaling[,1]
coef_ordenado <- coef_lda[order(coef_lda, decreasing = TRUE)]
barplot(coef_ordenado, horiz=TRUE, las=1,
        col=ifelse(coef_ordenado>0, "steelblue", "firebrick"),
        main="Coeficientes LD1 (País Vasco)", xlab="Valor del coeficiente", cex.names=0.7)

# Visualización LDA
lda_df <- as.data.frame(pred_lda_pv$x)
lda_df$Clase <- real
ggplot(lda_df, aes(x=LD1, y=LD2, color=Clase)) +
  geom_point(size=3, alpha=0.7) +
  labs(title="Representación LDA: País Vasco", x="LD1", y="LD2") +
  theme_minimal(base_size=9)

# Varianza explicada
write.csv(varianza_explicada_pv, "resultados/pca/varianza_explicada_pv.csv", row.names = TRUE)

# Loadings
write.csv(loadings_pv, "resultados/pca/loadings_pv.csv", row.names = TRUE)

# Scores
write.csv(scores_pv, "resultados/pca/scores_pv.csv", row.names = FALSE)

# Scree plot
png("resultados/pca/screeplot_pv.png", width = 800, height = 600)
plot(varianza_explicada_pv, type = "b", main = "Scree Plot - País Vasco",
     xlab = "Componente Principal", ylab = "Varianza Explicada",
     col = "darkgreen", pch = 19)
abline(v = 15, col = "blue", lty = 2)
dev.off()

# Dispersión PC1 vs PC15
png("resultados/pca/dispersion_pc1_pc15_pv.png", width = 800, height = 600)
ggplot(as.data.frame(pca_pv$x[, 1:15]), aes(x = PC1, y = PC15)) +
  geom_point(color = "darkgreen", size = 3, alpha = 0.7) +
  labs(title = "PCA: Dispersión PC1 vs PC15 (País Vasco)", x = "PC1", y = "PC15") +
  theme_minimal()
dev.off()

# Heatmap correlaciones
png("resultados/pca/heatmap_corr_pv.png", width = 900, height = 700)
ggplot(subset(melt(cor(datos_estandarizados_pv, pca_pv$x[,1:15]), 
                   varnames = c("Variable","Componente"), value.name="Correlacion"),
              Componente %in% paste0("PC",1:15)),
       aes(x = Componente, y = reorder(Variable, Correlacion), fill = Correlacion)) +
  geom_tile(color="white") +
  scale_fill_gradient2(low="blue", mid="white", high="red", midpoint=0) +
  labs(title="Correlaciones Variables - PC1 a PC15") +
  theme_minimal() +
  theme(axis.text.y=element_text(size=6))
dev.off()

# Barplot coef LD1
png("resultados/lda/barplot_coef_ld1_pv.png", width = 800, height = 600)
barplot(coef_ordenado,
        horiz = TRUE,
        las = 1,
        col = ifelse(coef_ordenado>0,"steelblue","firebrick"),
        main="Coeficientes LD1",
        xlab="Valor del coeficiente",
        cex.names=0.7)
dev.off()

# Dispersión LD1 vs LD2
png("resultados/lda/dispersion_ld1_ld2_pv.png", width = 800, height = 600)
ggplot(lda_df, aes(x = LD1, y = LD2, color = Clase)) +
  geom_point(size = 3, alpha = 0.7) +
  labs(title = "Representación LDA: País Vasco", x = "LD1", y = "LD2") +
  theme_minimal()
dev.off()

# Matriz de confusión y métricas
write.csv(as.data.frame(conf_matrix$table), "resultados/lda/matriz_confusion_pv.csv", row.names = TRUE)

# Coeficientes LD1
write.csv(coef_lda, "resultados/lda/coef_ld1_pv.csv", row.names = TRUE)
