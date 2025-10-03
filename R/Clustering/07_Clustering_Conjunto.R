# ==============================
# CLUSTERING - CONJUNTO 5 CCAA
# ==============================

library(factoextra)
library(ggplot2)
library(pheatmap)
library(dplyr)
library(scales)

# Filtrado de datos (5 CCAA)
ccaa_seleccionadas <- c("Andalucía", "Castilla y León", "Galicia", "Madrid", "País Vasco")
datos_5ccaa <- subset(datos_filtrados_normalizados, CCAA %in% ccaa_seleccionadas)

# Excluir variables no numéricas ni de impacto
variables_a_excluir <- c("CCAA", "PROV", "SEXO", 
                         "Impacto_Salud_Mental", "Impacto_Categorizado", 
                         "Bajo_Impacto", "Moderado_Impacto", "Alto_Impacto",
                         "EDAD")
datos_cluster_5ccaa <- datos_5ccaa[, !(names(datos_5ccaa) %in% variables_a_excluir)]

# Eliminar columnas constantes
datos_cluster_5ccaa <- datos_cluster_5ccaa[, sapply(datos_cluster_5ccaa, var) != 0]

# Determinar número óptimo de clústeres
set.seed(123)
wss <- sapply(1:10, function(k) kmeans(datos_cluster_5ccaa, centers = k, nstart = 10)$tot.withinss)
plot(1:10, wss, type = "b", pch = 19,
     xlab = "Número de Clústeres (k)",
     ylab = "Suma de cuadrados intra-cluster (WSS)",
     main = "Método del Codo - 5 CCAA")

# Aplicar K-means
k_optimo <- 3
set.seed(123)
modelo_kmeans_5ccaa <- kmeans(datos_cluster_5ccaa, centers = k_optimo, nstart = 25)
datos_5ccaa$cluster <- as.factor(modelo_kmeans_5ccaa$cluster)

# Perfil promedio de cada clúster
datos_clusterizados_5ccaa <- cbind(datos_cluster_5ccaa, cluster = modelo_kmeans_5ccaa$cluster)
perfil_5ccaa <- aggregate(. ~ cluster, data = datos_clusterizados_5ccaa, FUN = mean)
print("Perfil promedio por clúster (valores medios por variable):")
print(round(perfil_5ccaa, 2))

# Heatmap de perfiles
perfil_5ccaa_t <- t(perfil_5ccaa[,-1])
colnames(perfil_5ccaa_t) <- paste("Clúster", perfil_5ccaa$cluster)
perfil_5ccaa_t <- as.data.frame(perfil_5ccaa_t)

library(pheatmap)
pheatmap(perfil_5ccaa_t,
         cluster_rows = TRUE,
         cluster_cols = TRUE,
         main = "Perfil promedio por clúster - 5 CCAA",
         color = colorRampPalette(c("skyblue", "white", "tomato"))(50),
         fontsize = 7)

# Visualización K-means
library(factoextra)
fviz_cluster(modelo_kmeans_5ccaa, data = datos_cluster_5ccaa,
             geom = "point", ellipse.type = "norm",
             palette = "Dark2", ggtheme = theme_minimal(),
             main = "K-means clustering - Conjunto de 5 CCAA")

# Visualización con PCA
pca_5ccaa <- prcomp(datos_cluster_5ccaa, scale. = TRUE)
pca_data_5ccaa <- data.frame(PC1 = pca_5ccaa$x[,1],
                             PC2 = pca_5ccaa$x[,2],
                             cluster = datos_5ccaa$cluster[1:nrow(pca_5ccaa$x)])

library(ggplot2)
ggplot(pca_data_5ccaa, aes(x = PC1, y = PC2, color = cluster)) +
  geom_point(alpha = 0.6, size = 2) +
  labs(title = "Clustering K-means (PCA) - 5 CCAA",
       x = "Componente Principal 1", y = "Componente Principal 2") +
  theme_minimal() +
  scale_color_brewer(palette = "Dark2")

# Distribución por género
tabla_sexo_cluster_5ccaa <- table(datos_5ccaa$SEXO, datos_5ccaa$cluster)
sexo_cluster_5ccaa_df <- as.data.frame(tabla_sexo_cluster_5ccaa)
colnames(sexo_cluster_5ccaa_df) <- c("Genero", "Cluster", "Frecuencia")

ggplot(sexo_cluster_5ccaa_df, aes(x = Cluster, y = Frecuencia, fill = Genero)) +
  geom_bar(stat = "identity", position = "fill") +
  labs(title = "Distribución por género y clúster - 5 CCAA",
       y = "Porcentaje", x = "Clúster") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(palette = "Pastel1") +
  theme_minimal()

# Distribución por CCAA
tabla_ccaa_cluster <- table(datos_5ccaa$CCAA, datos_5ccaa$cluster)
ccaa_cluster_df <- as.data.frame(tabla_ccaa_cluster)
colnames(ccaa_cluster_df) <- c("CCAA", "Cluster", "Frecuencia")

ggplot(ccaa_cluster_df, aes(x = Cluster, y = Frecuencia, fill = CCAA)) +
  geom_bar(stat = "identity", position = "fill") +
  labs(title = "Distribución por CCAA y clúster (proporcional)",
       y = "Porcentaje", x = "Clúster") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(palette = "Set3") +
  theme_minimal()

# Distribución por grupo de edad
datos_5ccaa$grupo_edad <- cut(datos_5ccaa$EDAD,
                              breaks = c(17, 24, 34, 49, 64, Inf),
                              labels = c("Jóvenes", "Adultos emergentes", "Adultos jóvenes", "Adultos", "Mayores"),
                              right = TRUE, include.lowest = TRUE)
tabla_edad_cluster_5ccaa <- table(datos_5ccaa$grupo_edad, datos_5ccaa$cluster)
edad_cluster_5ccaa_df <- as.data.frame(tabla_edad_cluster_5ccaa)
colnames(edad_cluster_5ccaa_df) <- c("GrupoEdad", "Cluster", "Frecuencia")

ggplot(edad_cluster_5ccaa_df, aes(x = Cluster, y = Frecuencia, fill = GrupoEdad)) +
  geom_bar(stat = "identity", position = "fill") +
  labs(title = "Distribución por grupo de edad y clúster - 5 CCAA",
       y = "Porcentaje", x = "Clúster") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(palette = "YlGnBu") +
  theme_minimal()

# Guardar WSS (Método del codo)
png("resultados/clustering/wss_clusters_5ccaa.png", width = 800, height = 600)
plot(1:10, wss, type = "b", pch = 19,
     xlab = "Número de Clústeres (k)",
     ylab = "Suma de cuadrados intra-cluster (WSS)",
     main = "Método del Codo - 5 CCAA")
dev.off()

# Perfil medio por clúster
write.csv(round(perfil_5ccaa,2), "resultados/clustering/perfil_medio_clusters_5ccaa.csv", row.names = FALSE)

# Heatmap de perfiles
png("resultados/clustering/heatmap_clusters_5ccaa.png", width = 800, height = 600)
pheatmap(perfil_5ccaa_t,
         cluster_rows = TRUE,
         cluster_cols = TRUE,
         main = "Perfil promedio por clúster - 5 CCAA",
         color = colorRampPalette(c("skyblue", "white", "tomato"))(50),
         fontsize = 7)
dev.off()

# Visualización K-means
png("resultados/clustering/kmeans_clusters_5ccaa.png", width = 900, height = 700)
fviz_cluster(modelo_kmeans_5ccaa, data = datos_cluster_5ccaa,
             geom = "point", ellipse.type = "norm",
             palette = "Dark2", ggtheme = theme_minimal(),
             main = "K-means clustering - 5 CCAA")
dev.off()

# Visualización PCA
png("resultados/clustering/pca_clusters_5ccaa.png", width = 900, height = 700)
ggplot(pca_data_5ccaa, aes(x = PC1, y = PC2, color = cluster)) +
  geom_point(alpha = 0.6, size = 2) +
  labs(title = "Clustering K-means (PCA) - 5 CCAA",
       x = "Componente Principal 1", y = "Componente Principal 2") +
  theme_minimal() +
  scale_color_brewer(palette = "Dark2")
dev.off()

# Distribución por género
png("resultados/clustering/distribucion_genero_clusters_5ccaa.png", width = 900, height = 700)
ggplot(sexo_cluster_5ccaa_df, aes(x = Cluster, y = Frecuencia, fill = Genero)) +
  geom_bar(stat = "identity", position = "fill") +
  labs(title = "Distribución por género y clúster - 5 CCAA",
       y = "Porcentaje", x = "Clúster") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(palette = "Pastel1") +
  theme_minimal()
dev.off()

# Distribución por CCAA
png("resultados/clustering/distribucion_ccaa_clusters_5ccaa.png", width = 900, height = 700)
ggplot(ccaa_cluster_df, aes(x = Cluster, y = Frecuencia, fill = CCAA)) +
  geom_bar(stat = "identity", position = "fill") +
  labs(title = "Distribución por CCAA y clúster - 5 CCAA",
       y = "Porcentaje", x = "Clúster") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(palette = "Set3") +
  theme_minimal()
dev.off()

# Distribución por grupo de edad
png("resultados/clustering/distribucion_edad_clusters_5ccaa.png", width = 900, height = 700)
ggplot(edad_cluster_5ccaa_df, aes(x = Cluster, y = Frecuencia, fill = GrupoEdad)) +
  geom_bar(stat = "identity", position = "fill") +
  labs(title = "Distribución por grupo de edad y clúster - 5 CCAA",
       y = "Porcentaje", x = "Clúster") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(palette = "YlGnBu") +
  theme_minimal()
dev.off()