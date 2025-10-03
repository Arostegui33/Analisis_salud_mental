# ==============================
# CLUSTERING - GALICIA
# ==============================

library(factoextra)
library(ggplot2)
library(pheatmap)
library(dplyr)
library(scales)

# Filtrado de datos para Galicia
datos_gl <- subset(datos_filtrados_normalizados, CCAA == "Galicia")

# Excluir variables no numéricas ni de impacto
variables_a_excluir <- c("CCAA", "PROV", "SEXO", 
                         "Impacto_Salud_Mental", "Impacto_Categorizado", 
                         "Bajo_Impacto", "Moderado_Impacto", "Alto_Impacto",
                         "EDAD")
datos_gl_cluster <- datos_gl[, !(names(datos_gl) %in% variables_a_excluir)]

# Determinar número óptimo de clústeres
set.seed(123)
wss <- sapply(1:10, function(k) kmeans(datos_gl_cluster, centers = k, nstart = 10)$tot.withinss)

plot(1:10, wss, type = "b", pch = 19,
     xlab = "Número de Clústeres (k)",
     ylab = "Suma de cuadrados intra-cluster (WSS)",
     main = "Método del Codo - k óptimo (Galicia)")

# Aplicar K-means con k óptimo
k_optimo <- 3
set.seed(123)
modelo_kmeans_gl <- kmeans(datos_gl_cluster, centers = k_optimo, nstart = 25)
datos_gl$cluster <- as.factor(modelo_kmeans_gl$cluster)

# Perfil medio por clúster
datos_clusterizados <- cbind(datos_gl_cluster, cluster = modelo_kmeans_gl$cluster)
perfil_cluster <- aggregate(. ~ cluster, data = datos_clusterizados, FUN = mean)
cat("Perfil medio por clúster (valores medios):\n")
print(round(perfil_cluster, 2))

# Heatmap de perfiles
perfil_cluster_t <- t(perfil_cluster[,-1])
colnames(perfil_cluster_t) <- paste("Clúster", perfil_cluster$cluster)
perfil_cluster_t <- as.data.frame(perfil_cluster_t)

pheatmap(perfil_cluster_t,
         cluster_rows = TRUE,
         cluster_cols = TRUE,
         main = "Perfil promedio por clúster - Galicia",
         color = colorRampPalette(c("skyblue", "white", "tomato"))(50),
         fontsize = 7)

# Visualización K-means
fviz_cluster(modelo_kmeans_gl, data = datos_gl_cluster,
             geom = "point", ellipse.type = "norm",
             palette = "Set1", ggtheme = theme_minimal(base_size = 9),
             main = "Clustering K-means - Galicia")

# Visualización con PCA
pca_gl <- prcomp(datos_gl_cluster, scale. = TRUE)
pca_data <- data.frame(PC1 = pca_gl$x[,1],
                       PC2 = pca_gl$x[,2],
                       cluster = datos_gl$cluster)

ggplot(pca_data, aes(x = PC1, y = PC2, color = cluster)) +
  geom_point(alpha = 0.7, size = 2) +
  labs(title = "Clustering K-means en Galicia (PCA)",
       x = "PC1", y = "PC2") +
  theme_minimal(base_size = 9) +
  scale_color_brewer(palette = "Set1")

# Distribución sociodemográfica por clúster
# Por género
tabla_sexo <- table(datos_gl$SEXO, datos_gl$cluster)
sexo_df <- as.data.frame(tabla_sexo)
colnames(sexo_df) <- c("Genero", "Cluster", "Frecuencia")

ggplot(sexo_df, aes(x = Cluster, y = Frecuencia, fill = Genero)) +
  geom_bar(stat = "identity", position = "fill") +
  labs(title = "Distribución de género por clúster - Galicia",
       y = "Porcentaje", x = "Clúster") +
  scale_y_continuous(labels = percent) +
  scale_fill_brewer(palette = "Pastel1") +
  theme_minimal(base_size = 9)

# Por provincia
tabla_prov <- table(datos_gl$PROV, datos_gl$cluster)
prov_df <- as.data.frame(tabla_prov)
colnames(prov_df) <- c("Provincia", "Cluster", "Frecuencia")

ggplot(prov_df, aes(x = Cluster, y = Frecuencia, fill = Provincia)) +
  geom_bar(stat = "identity", position = "fill") +
  labs(title = "Distribución de provincia por clúster - Galicia",
       y = "Porcentaje", x = "Clúster") +
  scale_y_continuous(labels = percent) +
  scale_fill_brewer(palette = "Set3") +
  theme_minimal(base_size = 9)

# Por grupo de edad
datos_gl$grupo_edad <- cut(datos_gl$EDAD,
                           breaks = c(17, 24, 34, 49, 64, Inf),
                           labels = c("Jóvenes","Adultos emergentes","Adultos jóvenes","Adultos","Mayores"),
                           right = TRUE, include.lowest = TRUE)
tabla_edad <- table(datos_gl$grupo_edad, datos_gl$cluster)
edad_df <- as.data.frame(tabla_edad)
colnames(edad_df) <- c("GrupoEdad","Cluster","Frecuencia")

ggplot(edad_df, aes(x = Cluster, y = Frecuencia, fill = GrupoEdad)) +
  geom_bar(stat = "identity", position = "fill") +
  labs(title = "Distribución por grupo de edad por clúster - Galicia",
       y = "Porcentaje", x = "Clúster") +
  scale_y_continuous(labels = percent) +
  scale_fill_brewer(palette = "YlGnBu") +
  theme_minimal(base_size = 9)

# Guardar WSS (Método del codo)
png("resultados/clustering/wss_galicia.png", width = 800, height = 600)
plot(1:10, wss, type = "b", pch = 19,
     xlab = "Número de Clústeres (k)",
     ylab = "Suma de cuadrados intra-cluster (WSS)",
     main = "Método del Codo - k óptimo (Galicia)")
dev.off()

# Perfil medio por clúster
write.csv(round(perfil_cluster,2), "resultados/clustering/perfil_medio_clusters_galicia.csv", row.names = FALSE)

# Heatmap de perfiles
png("resultados/clustering/heatmap_clusters_galicia.png", width = 800, height = 600)
pheatmap(perfil_cluster_t,
         cluster_rows = TRUE,
         cluster_cols = TRUE,
         main = "Perfil promedio por clúster - Galicia",
         color = colorRampPalette(c("skyblue", "white", "tomato"))(50),
         fontsize = 7)
dev.off()

# Visualización K-means
png("resultados/clustering/kmeans_clusters_galicia.png", width = 900, height = 700)
fviz_cluster(modelo_kmeans_gl, data = datos_gl_cluster,
             geom = "point", ellipse.type = "norm",
             palette = "Set1", ggtheme = theme_minimal(base_size = 9),
             main = "Clustering K-means - Galicia")
dev.off()

# Visualización PCA
png("resultados/clustering/pca_clusters_galicia.png", width = 900, height = 700)
ggplot(pca_data, aes(x = PC1, y = PC2, color = cluster)) +
  geom_point(alpha = 0.7, size = 2) +
  labs(title = "Clustering K-means en Galicia (PCA)",
       x = "PC1", y = "PC2") +
  theme_minimal(base_size = 9) +
  scale_color_brewer(palette = "Set1")
dev.off()

# Distribución sociodemográfica por clúster
# Por género
png("resultados/clustering/distribucion_genero_clusters_galicia.png", width = 900, height = 700)
ggplot(sexo_df, aes(x = Cluster, y = Frecuencia, fill = Genero)) +
  geom_bar(stat = "identity", position = "fill") +
  labs(title = "Distribución de género por clúster - Galicia",
       y = "Porcentaje", x = "Clúster") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(palette = "Pastel1") +
  theme_minimal(base_size = 9)
dev.off()

# Por provincia
png("resultados/clustering/distribucion_provincia_clusters_galicia.png", width = 900, height = 700)
ggplot(prov_df, aes(x = Cluster, y = Frecuencia, fill = Provincia)) +
  geom_bar(stat = "identity", position = "fill") +
  labs(title = "Distribución de provincia por clúster - Galicia",
       y = "Porcentaje", x = "Clúster") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(palette = "Set3") +
  theme_minimal(base_size = 9)
dev.off()

# Por grupo de edad
png("resultados/clustering/distribucion_edad_clusters_galicia.png", width = 900, height = 700)
ggplot(edad_df, aes(x = Cluster, y = Frecuencia, fill = GrupoEdad)) +
  geom_bar(stat = "identity", position = "fill") +
  labs(title = "Distribución por grupo de edad por clúster - Galicia",
       y = "Porcentaje", x = "Clúster") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(palette = "YlGnBu") +
  theme_minimal(base_size = 9)
dev.off()