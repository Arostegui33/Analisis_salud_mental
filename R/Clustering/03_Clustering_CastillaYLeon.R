# ==============================
# CLUSTERING - CASTILLA Y LEÓN
# ==============================

library(factoextra)
library(ggplot2)
library(pheatmap)
library(dplyr)
library(scales)

# Filtrado de datos para Castilla y León
datos_cyl <- subset(datos_filtrados_normalizados, CCAA == "Castilla y León")

# Excluir variables no numéricas ni de impacto
variables_a_excluir <- c("CCAA", "PROV", "SEXO", 
                         "Impacto_Salud_Mental", "Impacto_Categorizado", 
                         "Bajo_Impacto", "Moderado_Impacto", "Alto_Impacto",
                         "EDAD")
datos_cyl_cluster <- datos_cyl[, !(names(datos_cyl) %in% variables_a_excluir)]


# Determinar número óptimo de clústeres (Método del Codo)
set.seed(123)
wss <- sapply(1:10, function(k) kmeans(datos_cyl_cluster, centers = k, nstart = 10)$tot.withinss)

plot(1:10, wss, type = "b", pch = 19,
     xlab = "Número de Clústeres (k)",
     ylab = "Suma de cuadrados intra-cluster (WSS)",
     main = "Método del Codo - k óptimo (Castilla y León)")


# Aplicar K-means con k óptimo
k_optimo <- 3
set.seed(123)
modelo_kmeans_cyl <- kmeans(datos_cyl_cluster, centers = k_optimo, nstart = 25)
datos_cyl$cluster <- as.factor(modelo_kmeans_cyl$cluster)


# Perfil medio por clúster
datos_clusterizados <- cbind(datos_cyl_cluster, cluster = modelo_kmeans_cyl$cluster)
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
         main = "Perfil promedio por clúster - Castilla y León",
         color = colorRampPalette(c("skyblue", "white", "tomato"))(50),
         fontsize = 7)


# Visualización K-means
fviz_cluster(modelo_kmeans_cyl, data = datos_cyl_cluster,
             geom = "point", ellipse.type = "norm",
             palette = "Set1", ggtheme = theme_minimal(base_size = 9),
             main = "Clustering K-means - Castilla y León")

# Visualización con PCA
pca_cyl <- prcomp(datos_cyl_cluster, scale. = TRUE)
pca_data <- data.frame(PC1 = pca_cyl$x[,1],
                       PC2 = pca_cyl$x[,2],
                       cluster = datos_cyl$cluster)

ggplot(pca_data, aes(x = PC1, y = PC2, color = cluster)) +
  geom_point(alpha = 0.7, size = 2) +
  labs(title = "Clustering K-means en Castilla y León (PCA)",
       x = "PC1", y = "PC2") +
  theme_minimal(base_size = 9) +
  scale_color_brewer(palette = "Set1")


# Distribución sociodemográfica por clúster
# Por género
tabla_sexo <- table(datos_cyl$SEXO, datos_cyl$cluster)
sexo_df <- as.data.frame(tabla_sexo)
colnames(sexo_df) <- c("Genero", "Cluster", "Frecuencia")

ggplot(sexo_df, aes(x = Cluster, y = Frecuencia, fill = Genero)) +
  geom_bar(stat = "identity", position = "fill") +
  labs(title = "Distribución de género por clúster - Castilla y León",
       y = "Porcentaje", x = "Clúster") +
  scale_y_continuous(labels = percent) +
  scale_fill_brewer(palette = "Pastel1") +
  theme_minimal(base_size = 9)

# Por provincia
tabla_prov <- table(datos_cyl$PROV, datos_cyl$cluster)
prov_df <- as.data.frame(tabla_prov)
colnames(prov_df) <- c("Provincia", "Cluster", "Frecuencia")

ggplot(prov_df, aes(x = Cluster, y = Frecuencia, fill = Provincia)) +
  geom_bar(stat = "identity", position = "fill") +
  labs(title = "Distribución de provincia por clúster - Castilla y León",
       y = "Porcentaje", x = "Clúster") +
  scale_y_continuous(labels = percent) +
  scale_fill_brewer(palette = "Set3") +
  theme_minimal(base_size = 9)

# Por grupo de edad
datos_cyl$grupo_edad <- cut(datos_cyl$EDAD,
                            breaks = c(17, 24, 34, 49, 64, Inf),
                            labels = c("Jóvenes","Adultos emergentes","Adultos jóvenes","Adultos","Mayores"),
                            right = TRUE, include.lowest = TRUE)
tabla_edad <- table(datos_cyl$grupo_edad, datos_cyl$cluster)
edad_df <- as.data.frame(tabla_edad)
colnames(edad_df) <- c("GrupoEdad","Cluster","Frecuencia")

ggplot(edad_df, aes(x = Cluster, y = Frecuencia, fill = GrupoEdad)) +
  geom_bar(stat = "identity", position = "fill") +
  labs(title = "Distribución por grupo de edad por clúster - Castilla y León",
       y = "Porcentaje", x = "Clúster") +
  scale_y_continuous(labels = percent) +
  scale_fill_brewer(palette = "YlGnBu") +
  theme_minimal(base_size = 9)

# Guardar WSS (Método del codo)
png("resultados/clustering/wss_clusters_castilla_leon.png", width = 800, height = 600)
plot(1:10, wss, type = "b", pch = 19,
     xlab = "Número de Clústeres (k)",
     ylab = "Suma de cuadrados intra-cluster (WSS)",
     main = "Método del Codo - k óptimo (Castilla y León)")
dev.off()

# Perfil medio por clúster
write.csv(round(perfil_cluster,2), "resultados/clustering/perfil_medio_clusters_castilla_leon.csv", row.names = FALSE)

# Heatmap de perfiles
png("resultados/clustering/heatmap_clusters_castilla_leon.png", width = 800, height = 600)
pheatmap(perfil_cluster_t,
         cluster_rows = TRUE,
         cluster_cols = TRUE,
         main = "Perfil promedio por clúster - Castilla y León",
         color = colorRampPalette(c("skyblue", "white", "tomato"))(50),
         fontsize = 7)
dev.off()

# Visualización K-means
png("resultados/clustering/kmeans_clusters_castilla_leon.png", width = 900, height = 700)
fviz_cluster(modelo_kmeans_cyl, data = datos_cyl_cluster,
             geom = "point", ellipse.type = "norm",
             palette = "Set1", ggtheme = theme_minimal(base_size = 9),
             main = "Clustering K-means - Castilla y León")
dev.off()

# Visualización PCA
png("resultados/clustering/pca_clusters_castilla_leon.png", width = 900, height = 700)
ggplot(pca_data, aes(x = PC1, y = PC2, color = cluster)) +
  geom_point(alpha = 0.7, size = 2) +
  labs(title = "Clustering K-means en Castilla y León (PCA)",
       x = "PC1", y = "PC2") +
  theme_minimal(base_size = 9) +
  scale_color_brewer(palette = "Set1")
dev.off()

# Distribución sociodemográfica por clúster
# Por género
png("resultados/clustering/distribucion_genero_clusters_castilla_leon.png", width = 900, height = 700)
ggplot(sexo_df, aes(x = Cluster, y = Frecuencia, fill = Genero)) +
  geom_bar(stat = "identity", position = "fill") +
  labs(title = "Distribución de género por clúster - Castilla y León",
       y = "Porcentaje", x = "Clúster") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(palette = "Pastel1") +
  theme_minimal(base_size = 9)
dev.off()

# Por provincia
png("resultados/clustering/distribucion_provincia_clusters_castilla_leon.png", width = 900, height = 700)
ggplot(prov_df, aes(x = Cluster, y = Frecuencia, fill = Provincia)) +
  geom_bar(stat = "identity", position = "fill") +
  labs(title = "Distribución de provincia por clúster - Castilla y León",
       y = "Porcentaje", x = "Clúster") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(palette = "Set3") +
  theme_minimal(base_size = 9)
dev.off()

# Por grupo de edad
png("resultados/clustering/distribucion_edad_clusters_castilla_leon.png", width = 900, height = 700)
ggplot(edad_df, aes(x = Cluster, y = Frecuencia, fill = GrupoEdad)) +
  geom_bar(stat = "identity", position = "fill") +
  labs(title = "Distribución por grupo de edad por clúster - Castilla y León",
       y = "Porcentaje", x = "Clúster") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(palette = "YlGnBu") +
  theme_minimal(base_size = 9)
dev.off()