# =========================================
# PCA CONJUNTA - 5 CCAA
# =========================================

# Crear carpetas para guardar resultados
if(!dir.exists("resultados/pca")) dir.create("resultados/pca", recursive = TRUE)

# Subset de datos
datos_filtrados_conjunta <- datos_filtrados[1:871, ]

# Convertir variables a numéricas
datos_filtrados_conjunta$P3 <- as.numeric(as.character(datos_filtrados_conjunta$P3))
datos_filtrados_conjunta$P3A <- as.numeric(as.character(datos_filtrados_conjunta$P3A))

# Variables a excluir
excluir_vars <- c("CCAA", "PROV", "SEXO", "EDAD")

# Selección de variables numéricas
cols_a_sumar <- setdiff(names(datos_filtrados_conjunta), excluir_vars)
cols_num <- names(Filter(is.numeric, datos_filtrados_conjunta[, cols_a_sumar]))

# Crear impacto_raw y clasificar
datos_filtrados_conjunta$impacto_raw <- rowSums(datos_filtrados_conjunta[, cols_num], na.rm = TRUE)
datos_filtrados_conjunta$impacto_salud_mental <- cut(
  datos_filtrados_conjunta$impacto_raw,
  breaks = c(-Inf, 90, 140, Inf),
  labels = c("Alta Afectación", "Moderada Afectación", "Baja Afectación")
)

# PCA
datos_estandarizados_conjunta <- scale(datos_filtrados_conjunta[, cols_num])
pca_conjunta <- prcomp(datos_estandarizados_conjunta, center = TRUE, scale. = TRUE)

# Scores primeras 15 PCs y añadir clase
scores_conjunta <- as.data.frame(pca_conjunta$x[, 1:15])
scores_conjunta$impacto_salud_mental <- datos_filtrados_conjunta$impacto_salud_mental

# Scree plot
varianza_explicada_conjunta <- pca_conjunta$sdev^2 / sum(pca_conjunta$sdev^2)
plot(varianza_explicada_conjunta, type = "b", main = "Scree Plot - Conjunto 5 CCAA",
     xlab = "Componente Principal", ylab = "Proporción de Varianza Explicada",
     col = "purple", pch = 19)
abline(v = 15, col = "blue", lty = 2)

# Heatmap correlaciones variables estandarizadas y PCs
scores_num <- scores_conjunta[, sapply(scores_conjunta, is.numeric)]
cor_matrix_conjunta <- cor(datos_estandarizados_conjunta, scores_num)

library(reshape2)
heatmap_data_conjunta <- melt(cor_matrix_conjunta, 
                              varnames = c("Variable", "Componente"), 
                              value.name = "Correlacion")

ggplot(heatmap_data_conjunta[heatmap_data_conjunta$Componente %in% paste0("PC", 1:15), ],
       aes(x = Componente, y = reorder(Variable, Correlacion), fill = Correlacion)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
  labs(title = "Heatmap de Correlaciones Variables-PCs (PC1 a PC15) - Conjunto 5 CCAA",
       x = "Componente Principal", y = "Variable", fill = "Correlación") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 6),
        plot.title = element_text(hjust = 0.5))

# Top 5 loadings por PC1 a PC15
loadings_conjunta <- pca_conjunta$rotation[, 1:15]
loadings_conjunta_df <- as.data.frame(loadings_conjunta)
loadings_conjunta_df$Variable <- rownames(loadings_conjunta_df)

loadings_long_conjunta <- melt(loadings_conjunta_df, id.vars = "Variable",
                               variable.name = "PC", value.name = "Loading")

top_loadings_conjunta <- loadings_long_conjunta %>%
  group_by(PC) %>%
  slice_max(order_by = abs(Loading), n = 5) %>%
  ungroup() %>%
  group_by(PC) %>%
  mutate(Variable = factor(Variable, levels = Variable[order(Loading)])) %>%
  ungroup()

# Gráfico de loadings
ggplot(top_loadings_conjunta, aes(x = Variable, y = Loading, fill = Loading)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(Loading, 2)),
            position = position_stack(vjust = 0.5),
            size = 2) +
  coord_flip() +
  facet_wrap(~ PC, scales = "free_y", ncol = 3) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  labs(title = "Top 5 Loadings PC1 a PC15 - Conjunto 5 CCAA", 
       x = "Variable", 
       y = "Carga (Loading)") +
  theme_minimal(base_size = 9)

# ==============================
# Gráficos comparativos PCA por CCAA
# ==============================

# Calcular la varianza explicada para cada PCA
varianza_explicada_andalucia <- pca_and$sdev^2 / sum(pca_and$sdev^2)
varianza_explicada_castilla_leon <- pca_cl$sdev^2 / sum(pca_cl$sdev^2)
varianza_explicada_galicia <- pca_gl$sdev^2 / sum(pca_gl$sdev^2)
varianza_explicada_madrid <- pca_ma$sdev^2 / sum(pca_ma$sdev^2)
varianza_explicada_pais_vasco <- pca_pv$sdev^2 / sum(pca_pv$sdev^2)

# Número de componentes para cada PCA
num_components_andalucia <- length(varianza_explicada_andalucia)
num_components_castilla_leon <- length(varianza_explicada_castilla_leon)
num_components_galicia <- length(varianza_explicada_galicia)
num_components_madrid <- length(varianza_explicada_madrid)
num_components_pais_vasco <- length(varianza_explicada_pais_vasco)

# Crear dataframe con la varianza explicada por CCAA
var_exp_df <- data.frame(
  PC = c(
    1:num_components_andalucia,
    1:num_components_castilla_leon,
    1:num_components_galicia,
    1:num_components_madrid,
    1:num_components_pais_vasco
  ),
  Varianza_Explicada = c(
    varianza_explicada_andalucia,
    varianza_explicada_castilla_leon,
    varianza_explicada_galicia,
    varianza_explicada_madrid,
    varianza_explicada_pais_vasco
  ),
  CCAA = c(
    rep("Andalucía", num_components_andalucia),
    rep("Castilla y León", num_components_castilla_leon),
    rep("Galicia", num_components_galicia),
    rep("Madrid", num_components_madrid),
    rep("País Vasco", num_components_pais_vasco)
  )
)

# Scree plot comparativo
ggplot(var_exp_df, aes(x = PC, y = Varianza_Explicada, color = CCAA)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Comparación de Varianza Explicada en las PCA por CCAA",
    x = "Componente Principal",
    y = "Varianza Explicada"
  ) +
  theme_minimal() +
  theme(legend.title = element_blank())

# ==============================
# Loadings comparativos PCA por CCAA
# ==============================

# Extraer los loadings del primer componente de cada PCA
loadings_andalucia <- data.frame(Variable = rownames(pca_and$rotation), Loading = pca_and$rotation[,1], Region = "Andalucía")
loadings_castillaleon <- data.frame(Variable = rownames(pca_cl$rotation), Loading = pca_cl$rotation[,1], Region = "Castilla y León")
loadings_galicia <- data.frame(Variable = rownames(pca_gl$rotation), Loading = pca_gl$rotation[,1], Region = "Galicia")
loadings_madrid <- data.frame(Variable = rownames(pca_ma$rotation), Loading = pca_ma$rotation[,1], Region = "Madrid")
loadings_paisvasco <- data.frame(Variable = rownames(pca_pv$rotation), Loading = pca_pv$rotation[,1], Region = "País Vasco")

# Combinar todos los dataframes en uno solo
df_loadings_combinado <- rbind(
  loadings_andalucia,
  loadings_castillaleon,
  loadings_galicia,
  loadings_madrid,
  loadings_paisvasco
)

# Ordenar por magnitud de los loadings
df_loadings_combinado <- df_loadings_combinado[order(abs(df_loadings_combinado$Loading), decreasing = TRUE), ]

# Gráfico comparativo de los loadings
ggplot(df_loadings_combinado, aes(x = reorder(Variable, abs(Loading)), y = Loading, fill = Region)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  labs(
    title = "Comparación de Loadings del Primer Componente Principal por CCAA",
    x = "Variable",
    y = "Loading"
  ) +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1") +  
  theme(axis.text.y = element_text(size = 6))

# Loadings PCA
write.csv(loadings_conjunta, "resultados/pca/loadings_conjunta.csv", row.names = TRUE)

# Varianza explicada PCA
write.csv(varianza_explicada_conjunta, "resultados/pca/varianza_explicada_conjunta.csv", row.names = TRUE)

# Scores PCA (PC1 a PC15)
write.csv(scores_conjunta, "resultados/pca/scores_conjunta.csv", row.names = FALSE)

# Scree plot PCA conjunta
png("resultados/pca/screeplot_conjunta.png", width = 800, height = 600)
plot(varianza_explicada_conjunta, type = "b", main = "Scree Plot - Conjunto 5 CCAA",
     xlab = "Componente Principal", ylab = "Varianza Explicada",
     col = "purple", pch = 19)
abline(v = 15, col = "blue", lty = 2)
dev.off()

# Dispersión PC1 vs PC15
png("resultados/pca/dispersion_pc1_pc15_conjunta.png", width = 800, height = 600)
ggplot(scores_conjunta, aes(x = PC1, y = PC15)) +
  geom_point(color = "purple", size = 3, alpha = 0.7) +
  labs(title = "PCA: Dispersión PC1 vs PC15 (Conjunto 5 CCAA)", x = "PC1", y = "PC15") +
  theme_minimal()
dev.off()

# Heatmap correlaciones PCA
png("resultados/pca/heatmap_corr_conjunta.png", width = 900, height = 700)
ggplot(subset(heatmap_data_conjunta, Componente %in% paste0("PC", 1:15)),
       aes(x = Componente, y = reorder(Variable, Correlacion), fill = Correlacion)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  labs(title = "Heatmap Variables - PC1 a PC15 (Conjunto 5 CCAA)") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 6))
dev.off()

# Top 5 loadings PCA
png("resultados/pca/top5_loadings_conjunta.png", width = 1200, height = 800)
ggplot(top_loadings_conjunta, aes(x = Variable, y = Loading, fill = Loading)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(Loading, 2)),
            position = position_stack(vjust = 0.5),
            size = 2) +
  coord_flip() +
  facet_wrap(~ PC, scales = "free_y", ncol = 3) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  labs(title = "Top 5 Loadings PC1 a PC15 - Conjunto 5 CCAA", x = "Variable", y = "Carga (Loading)") +
  theme_minimal(base_size = 9)
dev.off()

# Guardar loadings comparativos del primer componente de cada CCAA
write.csv(df_loadings_combinado, "resultados/pca/loadings_comparativos_CCAA.csv", row.names = FALSE)
