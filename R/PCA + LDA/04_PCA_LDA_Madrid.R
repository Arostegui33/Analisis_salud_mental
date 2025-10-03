# ===================================
# PCA MADRID
# ===================================

# Crear carpetas para guardar resultados
if(!dir.exists("resultados/pca")) dir.create("resultados/pca", recursive = TRUE)
if(!dir.exists("resultados/lda")) dir.create("resultados/lda", recursive = TRUE)

# Filtrar solo Madrid
datos_ma <- datos_filtrados[543:812, ]

# Convertir variables P3, P3A a numéricas
datos_ma$P3 <- as.numeric(as.character(datos_ma$P3))
datos_ma$P3A <- as.numeric(as.character(datos_ma$P3A))

# Selección de variables numéricas para PCA
vars_pca_ma <- c("P1", "P2", "P3", "P3A",
                 "P5_1", "P5_2", "P5_3", "P5_4",
                 "P6_1", "P6_2", "P6_3", "P6_4", "P6_5", "P6_6", "P6_7",
                 "P7", "P8", "P9", "P10",
                 "P11_1", "P11_2", "P11_3", "P11_4", "P11_5", "P11_6", "P11_7", "P11_8", "P11_9",
                 "P12_1", "P12_2", "P12_3", "P12_4", "P12_5", "P12_6", "P12_7", "P12_8", "P12_9",
                 "P12_10", "P12_11", "P12_12", "P12_13", "P12_14", "P12_15",
                 "P13_dolores", "P14A", "P14B", "P16A", "P16B", "P18", "P26", "P27", "P28", "P29")

datos_ma_num <- datos_ma[, vars_pca_ma]

# Estandarizar variables
datos_estandarizados_ma <- scale(datos_ma_num)

# PCA
pca_ma <- prcomp(datos_estandarizados_ma, center = TRUE, scale. = TRUE)

# Varianza explicada
varianza_explicada_ma <- pca_ma$sdev^2 / sum(pca_ma$sdev^2)

# Scree plot
plot(varianza_explicada_ma, type = "b", main = "Scree Plot - Madrid",
     xlab = "Componente Principal", ylab = "Proporción de Varianza Explicada",
     col = "darkred", pch = 19)
abline(v = 15, col = "blue", lty = 2)

# Scores PC1 a PC15
scores_ma <- as.data.frame(pca_ma$x[, 1:15])

# Loadings PC1 a PC15 y top 5 por PC
loadings_ma <- pca_ma$rotation[, 1:15]
loadings_ma_df <- as.data.frame(loadings_ma)
loadings_ma_df$Variable <- rownames(loadings_ma_df)
loadings_long_ma <- melt(loadings_ma_df, id.vars = "Variable", variable.name = "PC", value.name = "Loading")

top_loadings_ma <- loadings_long_ma %>%
  group_by(PC) %>%
  slice_max(order_by = abs(Loading), n = 5) %>%
  ungroup() %>%
  group_by(PC) %>%
  mutate(Variable = factor(Variable, levels = Variable[order(Loading)])) %>%
  ungroup()

ggplot(top_loadings_ma, aes(x = Variable, y = Loading, fill = Loading)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(Loading, 2)), position = position_stack(vjust = 0.5), size = 2) +
  coord_flip() +
  facet_wrap(~ PC, scales = "free_y", ncol = 3) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  theme_minimal(base_size = 9) +
  labs(title = "Top 5 Loadings PC1-PC15 (Madrid)", x = "Variable", y = "Carga (Loading)")

# ==============================
# Preparar variable de respuesta para LDA
# ==============================

excluir_vars <- c("CCAA", "PROV", "SEXO", "EDAD")
cols_num <- names(Filter(is.numeric, datos_ma[, setdiff(names(datos_ma), excluir_vars)]))

# Crear impacto_raw
datos_ma$impacto_raw <- rowSums(datos_ma[, cols_num], na.rm = TRUE)

# Categorizar impacto
datos_ma$impacto_salud_mental <- cut(datos_ma$impacto_raw,
                                     breaks = c(-Inf, 90, 140, Inf),
                                     labels = c("Alta Afectación", "Moderada Afectación", "Baja Afectación"))

# Añadir variable categórica al PCA scores
scores_ma <- as.data.frame(pca_ma$x[, 1:15])
scores_ma$impacto_salud_mental <- datos_ma$impacto_salud_mental


# ===================================
# LDA - MADRID
# ===================================

library(MASS)
lda_ma <- lda(impacto_salud_mental ~ ., data = scores_ma)

# Proporción de varianza explicada
prop_var_ma <- lda_ma$svd^2 / sum(lda_ma$svd^2)
names(prop_var_ma) <- paste0("LD", seq_along(prop_var_ma))
print(round(100 * prop_var_ma, 2))

# Predicciones y matriz de confusión
pred_lda_ma <- predict(lda_ma)
library(caret)
conf_matrix <- confusionMatrix(pred_lda_ma$class, scores_ma$impacto_salud_mental)
print(conf_matrix)

# Coeficientes de LD1
coef_ld1 <- lda_ma$scaling[, 1]
ord <- order(coef_ld1, decreasing = TRUE)
coef_ordenado <- coef_ld1[ord]
barplot(coef_ordenado, horiz = TRUE, las = 1,
        col = ifelse(coef_ordenado > 0, "steelblue", "firebrick"),
        main = "Coeficientes de la función discriminante 1 (LD1)",
        xlab = "Valor del coeficiente", cex.names = 0.7)

# Gráfico LDA
lda_df <- as.data.frame(pred_lda_ma$x)
lda_df$Clase <- scores_ma$impacto_salud_mental
ggplot(lda_df, aes(x = LD1, y = LD2, color = Clase)) +
  geom_point(size = 3, alpha = 0.7) +
  labs(title = "Representación LDA: Madrid", x = "LD1", y = "LD2") +
  theme_minimal()

# Varianza explicada
write.csv(varianza_explicada_ma, "resultados/pca/varianza_explicada_ma.csv", row.names = TRUE)

# Loadings
write.csv(loadings_ma, "resultados/pca/loadings_ma.csv", row.names = TRUE)

# Scores
write.csv(scores_ma, "resultados/pca/scores_ma.csv", row.names = FALSE)

# Scree plot
png("resultados/pca/screeplot_ma.png", width = 800, height = 600)
plot(varianza_explicada_ma, type = "b", main = "Scree Plot - Madrid",
     xlab = "Componente Principal", ylab = "Varianza Explicada",
     col = "darkred", pch = 19)
abline(v = 15, col = "blue", lty = 2)
dev.off()

# Dispersión PC1 vs PC15
png("resultados/pca/dispersion_pc1_pc15_ma.png", width = 800, height = 600)
ggplot(as.data.frame(pca_ma$x[, 1:15]), aes(x = PC1, y = PC15)) +
  geom_point(color = "darkred", size = 3, alpha = 0.7) +
  labs(title = "PCA: Dispersión PC1 vs PC15 (Madrid)", x = "PC1", y = "PC15") +
  theme_minimal()
dev.off()

# Heatmap correlaciones
png("resultados/pca/heatmap_corr_ma.png", width = 900, height = 700)
ggplot(subset(melt(cor(datos_estandarizados_ma, pca_ma$x[,1:15]), 
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
png("resultados/lda/barplot_coef_ld1_ma.png", width = 800, height = 600)
barplot(coef_ordenado,
        horiz = TRUE,
        las = 1,
        col = ifelse(coef_ordenado>0,"steelblue","firebrick"),
        main="Coeficientes LD1",
        xlab="Valor del coeficiente",
        cex.names=0.7)
dev.off()

# Dispersión LD1 vs LD2
png("resultados/lda/dispersion_ld1_ld2_ma.png", width = 800, height = 600)
ggplot(lda_df, aes(x=LD1, y=LD2, color=Clase)) +
  geom_point(size=3, alpha=0.7) +
  labs(title="Representación LDA: Madrid", x="LD1", y="LD2") +
  theme_minimal()
dev.off()

# Matriz de confusión y métricas
write.csv(as.data.frame(conf_matrix$table), "resultados/lda/matriz_confusion_ma.csv", row.names = TRUE)

# Coeficientes LD1
write.csv(coef_ld1, "resultados/lda/coef_ld1_ma.csv", row.names = TRUE)