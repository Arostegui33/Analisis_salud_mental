# ============================
# INSTALAR Y CARGAR PAQUETES
# ============================

# Instalar solo si no están instalados
paquetes <- c("haven", "dplyr", "labelled", "caret", "Hmisc", 
              "ggcorrplot", "ggplot2", "reshape2", "pROC", 
              "MASS", "randomForest", "cluster", "factoextra", "grid")

paquetes_instalados <- paquetes %in% rownames(installed.packages())
if (any(!paquetes_instalados)) {
  install.packages(paquetes[!paquetes_instalados])
}

# Cargar librerías
library(haven)
library(dplyr)    
library(labelled)
library(caret)
library(Hmisc)
library(ggcorrplot)
library(ggplot2)
library(reshape2)
library(pROC)
library(MASS)
library(randomForest)
library(cluster)
library(factoextra)
library(grid)


# ============================
# 1. ANÁLISIS PREVIO DATOS
# ============================

# Cargar datos
datos <- read_sav("datos/3312.sav")

# Reducción de variables no relevantes
datos <- datos[, -c(1:4, 7:9, 12, 15, 191, 199:205, 207:214, 216:353)]

# Reducción observaciones a 5 CCAA (Andalucía, Castilla y León, Galicia, Madrid, País Vasco)
datos <- datos[-c(567:1109, 1284:2145, 2787:2917, 3063:3084), ]


# Cambiar valores numéricos por nombres CCAA
datos <- datos %>% mutate(CCAA = case_when(
  CCAA == 1  ~ "Andalucía",
  CCAA == 8  ~ "Castilla y León",
  CCAA == 12 ~ "Galicia",
  CCAA == 13 ~ "Madrid",
  CCAA == 16 ~ "País Vasco",
  TRUE ~ as.character(CCAA)
))

# Cambiar valores numéricos por nombres provincias
provincia_nombres <- c(
  "4" = "Almería", "11" = "Cádiz", "14" = "Córdoba", "18" = "Granada",
  "21" = "Huelva", "23" = "Jaén", "29" = "Málaga", "41" = "Sevilla",
  "5" = "Ávila", "9" = "Burgos", "24" = "León", "34" = "Palencia",
  "37" = "Salamanca", "40" = "Segovia", "42" = "Soria", "47" = "Valladolid",
  "49" = "Zamora", "36" = "Pontevedra", "32" = "Ourense", "15" = "A Coruña",
  "27" = "Lugo", "28" = "Madrid", "20" = "Gipuzkoa", "48" = "Bizkaia", "1" = "Álava"
)
datos <- datos %>% mutate(PROV = recode(as.character(PROV), !!!provincia_nombres))

# Verificar cambios
head(datos$PROV)


# ============================
# 2. LIMPIEZA DE DATOS
# ============================

# Comprobar valores faltantes
any(is.na(datos))
colSums(is.na(datos))

# Eliminar variables con NA
datos <- datos[, colSums(is.na(datos)) == 0]

# Comprobar nuevamente
any(is.na(datos))
colSums(is.na(datos))

# Unificar categorías NC/NS
etiquetas_originales <- var_label(datos)
datos[] <- lapply(datos, function(x) as.numeric(as.character(x)))
datos[datos == "8"] <- 9
var_label(datos) <- etiquetas_originales


# ============================
# 3. RECODIFICACIÓN VARIABLES
# ============================

# CCAA y Provincias (reasignación explícita por rangos de filas)
datos$CCAA <- NA
datos$CCAA[1:566]   <- "Andalucía"
datos$CCAA[567:740] <- "Castilla y León"
datos$CCAA[741:942] <- "Galicia"
datos$CCAA[943:1381] <- "Madrid"
datos$CCAA[1382:1526] <- "País Vasco"

datos$PROV <- NA
datos$PROV[1:39]   <- "Almería"
datos$PROV[40:125] <- "Cádiz"
datos$PROV[126:182] <- "Córdoba"
datos$PROV[183:261] <- "Granada"
datos$PROV[262:285] <- "Huelva"
datos$PROV[286:331] <- "Jaén"
datos$PROV[332:426] <- "Málaga"
datos$PROV[427:566] <- "Sevilla"
datos$PROV[567:582] <- "Ávila"
datos$PROV[583:613] <- "Burgos"
datos$PROV[614:643] <- "León"
datos$PROV[644:647] <- "Palencia"
datos$PROV[648:669] <- "Salamanca"
datos$PROV[670:687] <- "Segovia"
datos$PROV[688:692] <- "Soria"
datos$PROV[693:731] <- "Valladolid"
datos$PROV[732:740] <- "Zamora"
datos$PROV[741:842] <- "A Coruña"
datos$PROV[843:857] <- "Lugo"
datos$PROV[858:871] <- "Ourense"
datos$PROV[872:942] <- "Pontevedra"
datos$PROV[943:1381] <- "Madrid"
datos$PROV[1382:1395] <- "Álava"
datos$PROV[1396:1433] <- "Gipuzkoa"
datos$PROV[1434:1526] <- "Bizkaia"

# Verificar cambio
table(datos$CCAA)

# Recodificación variable SEXO
datos$SEXO <- recode(datos$SEXO, `1` = "Hombre", `2` = "Mujer")
table(datos$SEXO)


# ============================
# 4. FILTRADO DE DATOS
# ============================

# Filtrar filas sin valor 9
datos_filtrados <- datos[apply(datos, 1, function(x) all(x != 9)), ]

# Eliminar filas 872 a 898
datos_filtrados <- datos_filtrados[-(872:898), ]

# Factores
datos_filtrados$SEXO <- as.factor(datos_filtrados$SEXO)
datos_filtrados$CCAA <- as.factor(datos_filtrados$CCAA)


# ============================
# 5. CREACIÓN Y ELIMINACIÓN DE VARIABLES
# ============================

# Nueva variable P13_dolores
datos_filtrados$P13_dolores <- ifelse(
  apply(datos_filtrados[, paste0("P13_", 1:14)], 1, function(x) any(x == 1)),
  1, 2
)

# Eliminar variables originales P13
datos_filtrados <- datos_filtrados[, !(colnames(datos_filtrados) %in% paste0("P13_", 1:14))]

# Verificar resultado
table(datos_filtrados$P13_dolores)

# Eliminar variable P19
datos_filtrados <- datos_filtrados[, !(colnames(datos_filtrados) %in% "P19")]


# ============================
# 6. RESÚMENES Y CORRELACIÓN
# ============================

table(datos_filtrados$CCAA)
table(datos_filtrados$SEXO)
table(datos_filtrados$PROV)
summary(datos_filtrados)

# Matriz de correlación
cor_matrix <- cor(datos_filtrados[, sapply(datos_filtrados, is.numeric)], use = "complete.obs")
print(cor_matrix)

# Identificar pares altamente correlacionados (>0.75)
highly_correlated <- findCorrelation(cor_matrix, cutoff = 0.75)
print(highly_correlated)

# Etiquetas originales
label(datos_filtrados$CCAA) <- "Comunidad Autónoma"
label(datos_filtrados$PROV) <- "Provincia"
label(datos_filtrados$SEXO) <- "Género de la persona encuestada"
label(datos_filtrados$P9)   <- "Probabilidad de contagio de coronavirus"

# ============================
# MATRIZ Y MAPA DE CORRELACIÓN
# ============================

# Matriz de correlación solo para variables numéricas
matriz_correlacion <- cor(
  datos_filtrados[, sapply(datos_filtrados, is.numeric)], 
  use = "complete.obs", method = "pearson"
)

# Mapa de calor de la matriz de correlación
corrplot::corrplot(
  matriz_correlacion, 
  method = "color", 
  col = colorRampPalette(c("blue", "white", "red"))(200),
  type = "upper", 
  order = "hclust", 
  tl.col = "black", 
  tl.cex = 0.3,   # Ajusta tamaño etiquetas si hay muchas variables
  tl.srt = 45, 
  title = "Mapa de calor: Correlación entre todas las variables", 
  mar = c(0, 0, 2, 0)
)


# ============================
# VISUALIZACIONES EXPLORATORIAS
# ============================

# Crear la variable de Comunidad Autónoma según observaciones
datos <- datos %>%
  mutate(CCAA = case_when(
    row_number() >= 1   & row_number() <= 134  ~ "Castilla y León",
    row_number() >= 135 & row_number() <= 573  ~ "Madrid",
    row_number() >= 574 & row_number() <= 718  ~ "País Vasco"
  ))

# Sexo por Comunidad Autónoma (Gráfico de barras apilado)
ggplot(datos, aes(x = CCAA, fill = SEXO)) + 
  geom_bar(position = "fill") +
  labs(
    title = "Distribución por Sexo y Comunidad Autónoma", 
    x = "Comunidad Autónoma", 
    y = "Proporción"
  ) +
  scale_fill_manual(values = c("skyblue", "lightcoral"), labels = c("Hombre", "Mujer")) +
  theme_minimal()

# Edad por Comunidad Autónoma (Gráfico de cajas)
ggplot(datos, aes(x = CCAA, y = EDAD)) + 
  geom_boxplot(fill = "lightblue", color = "blue") +
  labs(
    title = "Distribución de Edad por Comunidad Autónoma", 
    x = "Comunidad Autónoma", 
    y = "Edad"
  ) +
  theme_minimal()

# Ansiedad (P11_1) por Comunidad Autónoma (Gráfico de cajas)
ggplot(datos, aes(x = CCAA, y = P11_1)) + 
  geom_boxplot(fill = "lightgreen", color = "darkgreen") +
  labs(
    title = "Distribución de Ansiedad por Comunidad Autónoma", 
    x = "Comunidad Autónoma", 
    y = "Sensación de Ansiedad"
  ) +
  theme_minimal()

# Preocupación por el COVID (P1) (Gráfico de barras)
grafico_P1 <- ggplot(datos, aes(x = factor(P1), fill = CCAA)) +  
  geom_bar(position = "dodge", width = 0.7) +  
  scale_x_discrete(labels = c(
    "1" = "Muy preocupado", 
    "2" = "Bastante preocupado", 
    "3" = "Algo preocupado", 
    "4" = "Nada preocupado",
    "9" = "NC/NS"
  )) +
  labs(
    title = "Distribución de Preocupación por el Coronavirus",
    x = "Grado de Preocupación",
    y = "Frecuencia",
    fill = "Comunidad Autónoma"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  
    plot.margin = grid::unit(c(10, 10, 10, 10), "pt")
  )
print(grafico_P1)

# Preocupación por Contagio Familiar (P9) (Gráfico de barras)
ggplot(datos, aes(x = factor(P9), fill = CCAA)) + 
  geom_bar(position = "dodge") +
  scale_x_discrete(labels = c(
    "1" = "Nada preocupado", 
    "2" = "Poco preocupado", 
    "3" = "Bastante preocupado", 
    "4" = "Muy preocupado", 
    "9" = "NC/NS"
  )) +
  labs(
    title = "Preocupación por el Contagio Familiar por CCAA", 
    x = "Grado de Preocupación", 
    y = "Frecuencia"
  ) +
  theme_minimal()

# Dolor de Estómago (P13_1) (Gráfico de barras)
ggplot(datos, aes(x = factor(P13_1), fill = CCAA)) + 
  geom_bar(position = "dodge") +
  scale_x_discrete(labels = c(
    "1" = "Sí", 
    "2" = "No",
    "9" = "NC/NS"
  )) +
  labs(
    title = "Dolor de Estómago Reportado por CCAA", 
    x = "Dolor de Estómago", 
    y = "Frecuencia"
  ) +
  theme_minimal()

# Consultas al Psicólogo (P26 y P27) (Gráficos de barras apilados)

# P26 - Antes de la pandemia
ggplot(datos, aes(x = CCAA, fill = factor(P26))) + 
  geom_bar(position = "fill") +
  labs(
    title = "Consultas al Psicólogo Antes de la Pandemia por CCAA", 
    x = "Comunidad Autónoma", 
    y = "Proporción"
  ) +
  scale_fill_manual(
    values = c("lightgreen", "lightblue", "lightcoral"), 
    labels = c("No", "Sí", "Otro")
  ) +
  theme_minimal()

# P27 - Después de la pandemia
ggplot(datos, aes(x = CCAA, fill = factor(P27))) + 
  geom_bar(position = "fill") +
  labs(
    title = "Consultas al Psicólogo Después de la Pandemia por CCAA", 
    x = "Comunidad Autónoma", 
    y = "Proporción"
  ) +
  scale_fill_manual(
    values = c("lightgreen", "lightblue"), 
    labels = c("No", "Sí")
  ) +
  theme_minimal()


# ============================
# CORRELACIONES ESPECÍFICAS
# ============================

# Correlación entre variables (P12_1, P11_1, P11_2)
correlation_matrix <- cor(datos[, c("P12_1", "P11_1", "P11_2")], use = "complete.obs")
ggcorrplot(correlation_matrix, method = "circle")

# Correlación entre variables (P5_2, P6_1, P10)
correlation_matrix <- cor(datos[, c("P5_2", "P6_1", "P10")], use = "complete.obs")
ggcorrplot(correlation_matrix, method = "circle")



# ==============================
# PCA - Andalucía
# ==============================

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



# ==============================
# PCA - Castilla y León
# ==============================

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



# ==============================
# PCA - GALICIA
# ==============================

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



# ===================================
# PCA MADRID
# ===================================

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



# ==============================
# PCA - PAÍS VASCO
# ==============================

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



# =========================================
# PCA CONJUNTA - 5 CCAA
# =========================================

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



# =========================================
# RANDOM FOREST - ANDALUCÍA
# =========================================

library(randomForest)
library(pROC)
library(caret)

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



# =========================================
# RANDOM FOREST - CASTILLA Y LEÓN
# =========================================

library(randomForest)
library(pROC)
library(caret)

# Preparar datos
datos_cl$impacto_salud_mental <- as.factor(datos_cl$impacto_salud_mental)

# Entrenar modelo Random Forest
set.seed(123)
rf_cl <- randomForest(
  impacto_salud_mental ~ ., 
  data = datos_cl,
  ntree = 500,
  importance = TRUE
)

# Matriz de confusión OOB y métricas
cat("=== MATRIZ DE CONFUSIÓN OOB - Castilla y León ===\n")
print(rf_cl$confusion)

matriz_oob <- rf_cl$confusion[, 1:3]
clases <- rownames(matriz_oob)

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
cat("\n=== MÉTRICAS A PARTIR DE MATRIZ OOB - Castilla y León ===\n")
print(metricas_oob)

# Probabilidades OOB y AUC multicategoría
probabilidades_oob <- predict(rf_cl, type = "prob")
roc_multi <- multiclass.roc(datos_cl$impacto_salud_mental, probabilidades_oob)
cat("\n=== AUC MULTICLASE (OOB) - Castilla y León ===\n")
print(roc_multi$auc)

# Curvas ROC por clase
clases <- levels(datos_cl$impacto_salud_mental)
colors <- rainbow(length(clases))

plot(0, 0, type = "n", xlim = c(1, 0), ylim = c(0, 1),
     xlab = "FPR", ylab = "TPR",
     main = "Curvas ROC por Clase (OOB) - Castilla y León")

roc_list <- list()
for (i in seq_along(clases)) {
  clase <- clases[i]
  etiqueta_binaria <- as.numeric(datos_cl$impacto_salud_mental == clase)
  predicciones_clase <- probabilidades_oob[, clase]
  roc_bin <- roc(etiqueta_binaria, predicciones_clase)
  roc_list[[clase]] <- roc_bin
  lines(1 - roc_bin$specificities, roc_bin$sensitivities, col = colors[i], lwd = 2)
}
legend("bottomright", legend = clases, col = colors, lwd = 2)

# Matriz de confusión sobre los datos de entrenamiento
predicciones_rf <- predict(rf_cl, newdata = datos_cl)
conf_matrix_train <- confusionMatrix(predicciones_rf, datos_cl$impacto_salud_mental)
cat("\n=== MATRIZ DE CONFUSIÓN SOBRE TRAINING - Castilla y León ===\n")
print(conf_matrix_train)

# Importancia de variables
imp_vars <- importance(rf_cl)
imp_vars_sorted <- imp_vars[order(imp_vars[,1], decreasing = TRUE), ]

cat("\n=== TOP 10 VARIABLES MÁS IMPORTANTES - Castilla y León ===\n")
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
plot(rf_cl)



# =========================================
# RANDOM FOREST - GALICIA
# =========================================

library(randomForest)
library(pROC)
library(caret)

# Preparar datos
datos_gl$impacto_salud_mental <- as.factor(datos_gl$impacto_salud_mental)

# Entrenar modelo Random Forest
set.seed(123)
rf_gl <- randomForest(
  impacto_salud_mental ~ ., 
  data = datos_gl,
  ntree = 500,
  importance = TRUE
)

# Matriz de confusión OOB y métricas
cat("=== MATRIZ DE CONFUSIÓN OOB - Galicia ===\n")
print(rf_gl$confusion)

matriz_oob <- rf_gl$confusion[, 1:3]
clases <- rownames(matriz_oob)

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
cat("\n=== MÉTRICAS A PARTIR DE MATRIZ OOB - Galicia ===\n")
print(metricas_oob)

# Probabilidades OOB y AUC multicategoría
probabilidades_oob <- predict(rf_gl, type = "prob")
roc_multi <- multiclass.roc(datos_gl$impacto_salud_mental, probabilidades_oob)
cat("\n=== AUC MULTICLASE (OOB) - Galicia ===\n")
print(roc_multi$auc)

# Curvas ROC por clase
clases <- levels(datos_gl$impacto_salud_mental)
colors <- rainbow(length(clases))

plot(0, 0, type = "n", xlim = c(1, 0), ylim = c(0, 1),
     xlab = "FPR", ylab = "TPR",
     main = "Curvas ROC por Clase (OOB) - Galicia")

roc_list <- list()
for (i in seq_along(clases)) {
  clase <- clases[i]
  etiqueta_binaria <- as.numeric(datos_gl$impacto_salud_mental == clase)
  predicciones_clase <- probabilidades_oob[, clase]
  roc_bin <- roc(etiqueta_binaria, predicciones_clase)
  roc_list[[clase]] <- roc_bin
  lines(1 - roc_bin$specificities, roc_bin$sensitivities, col = colors[i], lwd = 2)
}
legend("bottomright", legend = clases, col = colors, lwd = 2)

# Matriz de confusión sobre los datos de entrenamiento
predicciones_rf <- predict(rf_gl, newdata = datos_gl)
conf_matrix_train <- confusionMatrix(predicciones_rf, datos_gl$impacto_salud_mental)
cat("\n=== MATRIZ DE CONFUSIÓN SOBRE TRAINING - Galicia ===\n")
print(conf_matrix_train)

# Importancia de variables
imp_vars <- importance(rf_gl)
imp_vars_sorted <- imp_vars[order(imp_vars[,1], decreasing = TRUE), ]

cat("\n=== TOP 10 VARIABLES MÁS IMPORTANTES - Galicia ===\n")
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
plot(rf_gl)



# =========================================
# RANDOM FOREST - MADRID
# =========================================

library(randomForest)
library(pROC)
library(caret)

# Preparar datos
datos_ma$impacto_salud_mental <- as.factor(datos_ma$impacto_salud_mental)

# Entrenar modelo Random Forest
set.seed(123)
rf_ma <- randomForest(
  impacto_salud_mental ~ ., 
  data = datos_ma,
  ntree = 500,
  importance = TRUE
)

# Matriz de confusión OOB y métricas
cat("=== MATRIZ DE CONFUSIÓN OOB - Madrid ===\n")
print(rf_ma$confusion)

matriz_oob <- rf_ma$confusion[, 1:3]
clases <- rownames(matriz_oob)

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
cat("\n=== MÉTRICAS A PARTIR DE MATRIZ OOB - Madrid ===\n")
print(metricas_oob)

# Probabilidades OOB y AUC multicategoría
probabilidades_oob <- predict(rf_ma, type = "prob")
roc_multi <- multiclass.roc(datos_ma$impacto_salud_mental, probabilidades_oob)
cat("\n=== AUC MULTICLASE (OOB) - Madrid ===\n")
print(roc_multi$auc)

# Curvas ROC por clase
clases <- levels(datos_ma$impacto_salud_mental)
colors <- rainbow(length(clases))

plot(0, 0, type = "n", xlim = c(1, 0), ylim = c(0, 1),
     xlab = "FPR", ylab = "TPR",
     main = "Curvas ROC por Clase (OOB) - Madrid")

roc_list <- list()
for (i in seq_along(clases)) {
  clase <- clases[i]
  etiqueta_binaria <- as.numeric(datos_ma$impacto_salud_mental == clase)
  predicciones_clase <- probabilidades_oob[, clase]
  roc_bin <- roc(etiqueta_binaria, predicciones_clase)
  roc_list[[clase]] <- roc_bin
  lines(1 - roc_bin$specificities, roc_bin$sensitivities, col = colors[i], lwd = 2)
}
legend("bottomright", legend = clases, col = colors, lwd = 2)

# Matriz de confusión sobre los datos de entrenamiento
predicciones_rf <- predict(rf_ma, newdata = datos_ma)
conf_matrix_train <- confusionMatrix(predicciones_rf, datos_ma$impacto_salud_mental)
cat("\n=== MATRIZ DE CONFUSIÓN SOBRE TRAINING - Madrid ===\n")
print(conf_matrix_train)

# Importancia de variables
imp_vars <- importance(rf_ma)
imp_vars_sorted <- imp_vars[order(imp_vars[,1], decreasing = TRUE), ]

cat("\n=== TOP 10 VARIABLES MÁS IMPORTANTES - Madrid ===\n")
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
plot(rf_ma)



# =========================================
# RANDOM FOREST - PAÍS VASCO
# =========================================

library(randomForest)
library(pROC)
library(caret)

# Preparar datos
datos_pv$impacto_salud_mental <- as.factor(datos_pv$impacto_salud_mental)

# Entrenar modelo Random Forest
set.seed(123)
rf_pv <- randomForest(
  impacto_salud_mental ~ ., 
  data = datos_pv,
  ntree = 500,
  importance = TRUE
)

# Matriz de confusión OOB y métricas
cat("=== MATRIZ DE CONFUSIÓN OOB - País Vasco ===\n")
print(rf_pv$confusion)

matriz_oob <- rf_pv$confusion[, 1:3]
clases <- rownames(matriz_oob)

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
cat("\n=== MÉTRICAS A PARTIR DE MATRIZ OOB - País Vasco ===\n")
print(metricas_oob)

# Probabilidades OOB y AUC multicategoría
probabilidades_oob <- predict(rf_pv, type = "prob")
roc_multi <- multiclass.roc(datos_pv$impacto_salud_mental, probabilidades_oob)
cat("\n=== AUC MULTICLASE (OOB) - País Vasco ===\n")
print(roc_multi$auc)

# Curvas ROC por clase
clases <- levels(datos_pv$impacto_salud_mental)
colors <- rainbow(length(clases))

plot(0, 0, type = "n", xlim = c(1, 0), ylim = c(0, 1),
     xlab = "FPR", ylab = "TPR",
     main = "Curvas ROC por Clase (OOB) - País Vasco")

roc_list <- list()
for (i in seq_along(clases)) {
  clase <- clases[i]
  etiqueta_binaria <- as.numeric(datos_pv$impacto_salud_mental == clase)
  predicciones_clase <- probabilidades_oob[, clase]
  roc_bin <- roc(etiqueta_binaria, predicciones_clase)
  roc_list[[clase]] <- roc_bin
  lines(1 - roc_bin$specificities, roc_bin$sensitivities, col = colors[i], lwd = 2)
}
legend("bottomright", legend = clases, col = colors, lwd = 2)

# Matriz de confusión sobre los datos de entrenamiento
predicciones_rf <- predict(rf_pv, newdata = datos_pv)
conf_matrix_train <- confusionMatrix(predicciones_rf, datos_pv$impacto_salud_mental)
cat("\n=== MATRIZ DE CONFUSIÓN SOBRE TRAINING - País Vasco ===\n")
print(conf_matrix_train)

# Importancia de variables
imp_vars <- importance(rf_pv)
imp_vars_sorted <- imp_vars[order(imp_vars[,1], decreasing = TRUE), ]

cat("\n=== TOP 10 VARIABLES MÁS IMPORTANTES - País Vasco ===\n")
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
plot(rf_pv)



# =========================================
# RANDOM FOREST - CONJUNTO DE CCAA
# =========================================

library(randomForest)
library(pROC)
library(caret)
library(ggplot2)

# Preparar datos
variables_a_excluir <- c("CCAA", "PROV", "SEXO", "impacto_salud_mental", "impacto_raw", "EDAD")
datos_todas <- datos_filtrados_conjunta[, !(names(datos_filtrados_conjunta) %in% variables_a_excluir)]
datos_todas$impacto_salud_mental <- as.factor(datos_filtrados_conjunta$impacto_salud_mental)

# Entrenar modelo Random Forest
set.seed(123)
rf_todas <- randomForest(impacto_salud_mental ~ ., 
                         data = datos_todas, 
                         ntree = 500, 
                         importance = TRUE)


# Matriz de confusión OOB y métricas
cat("=== MATRIZ DE CONFUSIÓN OOB - Conjunto Total ===\n")
print(rf_todas$confusion)

# Extraer matriz OOB solo con clases
matriz_oob <- rf_todas$confusion[, 1:3]  
clases <- rownames(matriz_oob)

# Calcular Precision, Recall y F1-score
metricas_oob <- data.frame(
  Clase = clases,
  Precision = round(diag(matriz_oob) / colSums(matriz_oob), 3),
  Recall = round(diag(matriz_oob) / rowSums(matriz_oob), 3)
)
metricas_oob$F1_Score <- round(2 * metricas_oob$Precision * metricas_oob$Recall / 
                                 (metricas_oob$Precision + metricas_oob$Recall), 3)

cat("\n=== MÉTRICAS A PARTIR DE MATRIZ OOB - Conjunto Total ===\n")
print(metricas_oob)


# Probabilidades OOB y AUC multicategoría
probabilidades_oob <- predict(rf_todas, type = "prob")
roc_multi <- multiclass.roc(datos_todas$impacto_salud_mental, probabilidades_oob)

cat("\n=== AUC MULTICLASE (OOB) - Conjunto Total ===\n")
print(roc_multi$auc)


# Curvas ROC por clase (uno contra todos)
clases <- levels(datos_todas$impacto_salud_mental)
colors <- rainbow(length(clases))

plot(0, 0, type = "n", xlim = c(1,0), ylim = c(0,1),
     xlab = "FPR", ylab = "TPR", main = "Curvas ROC por Clase (OOB) - Conjunto Total")

for (i in seq_along(clases)) {
  etiqueta_binaria <- as.numeric(datos_todas$impacto_salud_mental == clases[i])
  roc_bin <- roc(etiqueta_binaria, probabilidades_oob[, clases[i]])
  lines(1 - roc_bin$specificities, roc_bin$sensitivities, col = colors[i], lwd = 2)
}
legend("bottomright", legend = clases, col = colors, lwd = 2)


# Matriz de confusión sobre entrenamiento
predicciones_rf <- predict(rf_todas, newdata = datos_todas)
conf_matrix_train <- confusionMatrix(predicciones_rf, datos_todas$impacto_salud_mental)
cat("\n=== MATRIZ DE CONFUSIÓN SOBRE TRAINING - Conjunto Total ===\n")
print(conf_matrix_train)


# Importancia de variables y top 10
imp_vars <- importance(rf_todas)
imp_vars_sorted <- imp_vars[order(imp_vars[,1], decreasing = TRUE), ]
top_10 <- head(imp_vars_sorted, 10)

cat("\n=== TOP 10 VARIABLES MÁS IMPORTANTES - Conjunto Total ===\n")
print(top_10)

# Graficar top 10 variables por clase
par(mfrow = c(1,3), mar = c(6,6,4,2))
barplot(top_10[,1], names.arg = rownames(top_10), las = 2, col = "skyblue", 
        main = "Alta Afectación", ylab = "Importancia")
barplot(top_10[,2], names.arg = rownames(top_10), las = 2, col = "skyblue", 
        main = "Moderada Afectación", ylab = "Importancia")
barplot(top_10[,3], names.arg = rownames(top_10), las = 2, col = "skyblue", 
        main = "Baja Afectación", ylab = "Importancia")
par(mfrow = c(1,1)) # reset layout


# Evolución del error OOB
plot(rf_todas)



# ==============================
# CLUSTERING 
# ==============================

#Excluir variables
excluir_vars <- c("CCAA", "PROV", "SEXO", "EDAD")

#Seleccionar solo las columnas numéricas a normalizar
vars_a_normalizar <- setdiff(names(datos_filtrados), excluir_vars)

#Normalizar sólo esas columnas numéricas
datos_filtrados_normalizados <- datos_filtrados

datos_filtrados_normalizados[, vars_a_normalizar] <- scale(datos_filtrados[, vars_a_normalizar])

# ==============================
# CLUSTERING - ANDALUCÍA
# ==============================

library(factoextra)
library(ggplot2)
library(pheatmap)
library(dplyr)
library(scales)

# Filtrado de datos para Andalucía
datos_andalucia <- subset(datos_filtrados_normalizados, CCAA == "Andalucía")

# Excluir variables no numéricas ni de impacto
variables_a_excluir <- c("CCAA", "PROV", "SEXO", 
                         "Impacto_Salud_Mental", "Impacto_Categorizado", 
                         "Bajo_Impacto", "Moderado_Impacto", "Alto_Impacto",
                         "EDAD")
datos_and_cluster <- datos_andalucia[, !(names(datos_andalucia) %in% variables_a_excluir)]


# Determinar número óptimo de clústeres (Método del Codo)
set.seed(123)
wss <- sapply(1:10, function(k) kmeans(datos_and_cluster, centers = k, nstart = 10)$tot.withinss)

plot(1:10, wss, type = "b", pch = 19,
     xlab = "Número de Clústeres (k)",
     ylab = "Suma de cuadrados intra-cluster (WSS)",
     main = "Método del Codo - k óptimo (Andalucía)")

# Aplicar K-means con k óptimo
k_optimo <- 3
set.seed(123)
modelo_kmeans_and <- kmeans(datos_and_cluster, centers = k_optimo, nstart = 25)
datos_andalucia$cluster <- as.factor(modelo_kmeans_and$cluster)


# Perfil medio por clúster
datos_clusterizados <- cbind(datos_and_cluster, cluster = modelo_kmeans_and$cluster)
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
         main = "Perfil promedio por clúster - Andalucía",
         color = colorRampPalette(c("skyblue", "white", "tomato"))(50),
         fontsize = 7)


# Visualización K-means
fviz_cluster(modelo_kmeans_and, data = datos_and_cluster,
             geom = "point", ellipse.type = "norm",
             palette = "Set1", ggtheme = theme_minimal(base_size = 9),
             main = "Clustering K-means - Andalucía")

# Visualización con PCA
pca_and <- prcomp(datos_and_cluster, scale. = TRUE)
pca_data <- data.frame(PC1 = pca_and$x[,1],
                       PC2 = pca_and$x[,2],
                       cluster = datos_andalucia$cluster)

ggplot(pca_data, aes(x = PC1, y = PC2, color = cluster)) +
  geom_point(alpha = 0.7, size = 2) +
  labs(title = "Clustering K-means en Andalucía (PCA)",
       x = "PC1", y = "PC2") +
  theme_minimal(base_size = 9) +
  scale_color_brewer(palette = "Set1")


# Distribución sociodemográfica por clúster
# Por género
tabla_sexo <- table(datos_andalucia$SEXO, datos_andalucia$cluster)
sexo_df <- as.data.frame(tabla_sexo)
colnames(sexo_df) <- c("Genero", "Cluster", "Frecuencia")

ggplot(sexo_df, aes(x = Cluster, y = Frecuencia, fill = Genero)) +
  geom_bar(stat = "identity", position = "fill") +
  labs(title = "Distribución de género por clúster - Andalucía",
       y = "Porcentaje", x = "Clúster") +
  scale_y_continuous(labels = percent) +
  scale_fill_brewer(palette = "Pastel1") +
  theme_minimal(base_size = 9)

# Por provincia
tabla_prov <- table(datos_andalucia$PROV, datos_andalucia$cluster)
prov_df <- as.data.frame(tabla_prov)
colnames(prov_df) <- c("Provincia", "Cluster", "Frecuencia")

ggplot(prov_df, aes(x = Cluster, y = Frecuencia, fill = Provincia)) +
  geom_bar(stat = "identity", position = "fill") +
  labs(title = "Distribución de provincia por clúster - Andalucía",
       y = "Porcentaje", x = "Clúster") +
  scale_y_continuous(labels = percent) +
  scale_fill_brewer(palette = "Set3") +
  theme_minimal(base_size = 9)

# Por grupo de edad
datos_andalucia$grupo_edad <- cut(datos_andalucia$EDAD,
                                  breaks = c(17, 24, 34, 49, 64, Inf),
                                  labels = c("Jóvenes","Adultos emergentes","Adultos jóvenes","Adultos","Mayores"),
                                  right = TRUE, include.lowest = TRUE)
tabla_edad <- table(datos_andalucia$grupo_edad, datos_andalucia$cluster)
edad_df <- as.data.frame(tabla_edad)
colnames(edad_df) <- c("GrupoEdad","Cluster","Frecuencia")

ggplot(edad_df, aes(x = Cluster, y = Frecuencia, fill = GrupoEdad)) +
  geom_bar(stat = "identity", position = "fill") +
  labs(title = "Distribución por grupo de edad por clúster - Andalucía",
       y = "Porcentaje", x = "Clúster") +
  scale_y_continuous(labels = percent) +
  scale_fill_brewer(palette = "YlGnBu") +
  theme_minimal(base_size = 9)



# ==============================
# CLUSTERING - CASTILLA Y LEÓN
# ==============================

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



# ==============================
# CLUSTERING - GALICIA
# ==============================

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



# ==============================
# CLUSTERING - MADRID
# ==============================

# Filtrado de datos para Madrid
datos_ma <- subset(datos_filtrados_normalizados, CCAA == "Madrid")

# Excluir variables no numéricas ni de impacto
variables_a_excluir <- c("CCAA", "PROV", "SEXO", 
                         "Impacto_Salud_Mental", "Impacto_Categorizado", 
                         "Bajo_Impacto", "Moderado_Impacto", "Alto_Impacto",
                         "EDAD")
datos_ma_cluster <- datos_ma[, !(names(datos_ma) %in% variables_a_excluir)]

# Determinar número óptimo de clústeres
set.seed(123)
wss <- sapply(1:10, function(k) kmeans(datos_ma_cluster, centers = k, nstart = 10)$tot.withinss)

plot(1:10, wss, type = "b", pch = 19,
     xlab = "Número de Clústeres (k)",
     ylab = "Suma de cuadrados intra-cluster (WSS)",
     main = "Método del Codo - k óptimo (Madrid)")

# Aplicar K-means con k óptimo
k_optimo <- 3
set.seed(123)
modelo_kmeans_ma <- kmeans(datos_ma_cluster, centers = k_optimo, nstart = 25)
datos_ma$cluster <- as.factor(modelo_kmeans_ma$cluster)

# Perfil medio por clúster
datos_clusterizados <- cbind(datos_ma_cluster, cluster = modelo_kmeans_ma$cluster)
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
         main = "Perfil promedio por clúster - Madrid",
         color = colorRampPalette(c("skyblue", "white", "tomato"))(50),
         fontsize = 7)

# Visualización K-means
fviz_cluster(modelo_kmeans_ma, data = datos_ma_cluster,
             geom = "point", ellipse.type = "norm",
             palette = "Set1", ggtheme = theme_minimal(base_size = 9),
             main = "Clustering K-means - Madrid")

# Visualización con PCA
pca_ma <- prcomp(datos_ma_cluster, scale. = TRUE)
pca_data <- data.frame(PC1 = pca_ma$x[,1],
                       PC2 = pca_ma$x[,2],
                       cluster = datos_ma$cluster)

ggplot(pca_data, aes(x = PC1, y = PC2, color = cluster)) +
  geom_point(alpha = 0.7, size = 2) +
  labs(title = "Clustering K-means en Madrid (PCA)",
       x = "PC1", y = "PC2") +
  theme_minimal(base_size = 9) +
  scale_color_brewer(palette = "Set1")

# Distribución sociodemográfica por clúster
# Por género
tabla_sexo <- table(datos_ma$SEXO, datos_ma$cluster)
sexo_df <- as.data.frame(tabla_sexo)
colnames(sexo_df) <- c("Genero", "Cluster", "Frecuencia")

ggplot(sexo_df, aes(x = Cluster, y = Frecuencia, fill = Genero)) +
  geom_bar(stat = "identity", position = "fill") +
  labs(title = "Distribución de género por clúster - Madrid",
       y = "Porcentaje", x = "Clúster") +
  scale_y_continuous(labels = percent) +
  scale_fill_brewer(palette = "Pastel1") +
  theme_minimal(base_size = 9)

# Por provincia
tabla_prov <- table(datos_ma$PROV, datos_ma$cluster)
prov_df <- as.data.frame(tabla_prov)
colnames(prov_df) <- c("Provincia", "Cluster", "Frecuencia")

ggplot(prov_df, aes(x = Cluster, y = Frecuencia, fill = Provincia)) +
  geom_bar(stat = "identity", position = "fill") +
  labs(title = "Distribución de provincia por clúster - Madrid",
       y = "Porcentaje", x = "Clúster") +
  scale_y_continuous(labels = percent) +
  scale_fill_brewer(palette = "Set3") +
  theme_minimal(base_size = 9)

# Por grupo de edad
datos_ma$grupo_edad <- cut(datos_ma$EDAD,
                           breaks = c(17, 24, 34, 49, 64, Inf),
                           labels = c("Jóvenes","Adultos emergentes","Adultos jóvenes","Adultos","Mayores"),
                           right = TRUE, include.lowest = TRUE)
tabla_edad <- table(datos_ma$grupo_edad, datos_ma$cluster)
edad_df <- as.data.frame(tabla_edad)
colnames(edad_df) <- c("GrupoEdad","Cluster","Frecuencia")

ggplot(edad_df, aes(x = Cluster, y = Frecuencia, fill = GrupoEdad)) +
  geom_bar(stat = "identity", position = "fill") +
  labs(title = "Distribución por grupo de edad por clúster - Madrid",
       y = "Porcentaje", x = "Clúster") +
  scale_y_continuous(labels = percent) +
  scale_fill_brewer(palette = "YlGnBu") +
  theme_minimal(base_size = 9)



# ==============================
# CLUSTERING - PAÍS VASCO
# ==============================

# Filtrado de datos para País Vasco
datos_pv <- subset(datos_filtrados_normalizados, CCAA == "País Vasco")

# Excluir variables no numéricas ni de impacto
variables_a_excluir <- c("CCAA", "PROV", "SEXO", 
                         "Impacto_Salud_Mental", "Impacto_Categorizado", 
                         "Bajo_Impacto", "Moderado_Impacto", "Alto_Impacto",
                         "EDAD")
datos_pv_cluster <- datos_pv[, !(names(datos_pv) %in% variables_a_excluir)]

# Eliminar columnas constantes (varianza = 0)
datos_pv_cluster <- datos_pv_cluster[, sapply(datos_pv_cluster, var) != 0]

# Determinar número óptimo de clústeres
set.seed(123)
wss <- sapply(1:10, function(k) kmeans(datos_pv_cluster, centers = k, nstart = 10)$tot.withinss)

plot(1:10, wss, type = "b", pch = 19,
     xlab = "Número de Clústeres (k)",
     ylab = "Suma de cuadrados intra-cluster (WSS)",
     main = "Método del Codo - k óptimo (País Vasco)")

# Aplicar K-means con k óptimo
k_optimo <- 3
set.seed(123)
modelo_kmeans_pv <- kmeans(datos_pv_cluster, centers = k_optimo, nstart = 25)
datos_pv$cluster <- as.factor(modelo_kmeans_pv$cluster)

# Perfil medio por clúster
datos_clusterizados <- cbind(datos_pv_cluster, cluster = modelo_kmeans_pv$cluster)
perfil_cluster <- aggregate(. ~ cluster, data = datos_clusterizados, FUN = mean)
cat("Perfil medio por clúster (valores medios):\n")
print(round(perfil_cluster, 2))

# Heatmap de perfiles
perfil_cluster_t <- t(perfil_cluster[,-1])
colnames(perfil_cluster_t) <- paste("Clúster", perfil_cluster$cluster)
perfil_cluster_t <- as.data.frame(perfil_cluster_t)

library(pheatmap)
pheatmap(perfil_cluster_t,
         cluster_rows = TRUE,
         cluster_cols = TRUE,
         main = "Perfil promedio por clúster - País Vasco",
         color = colorRampPalette(c("skyblue", "white", "tomato"))(50),
         fontsize = 7)

# Visualización K-means (fviz_cluster)
library(factoextra)
fviz_cluster(modelo_kmeans_pv, data = datos_pv_cluster,
             geom = "point", ellipse.type = "norm",
             palette = "Set2", ggtheme = theme_minimal(),
             main = "Clustering K-means - País Vasco")

# Visualización con PCA
pca_pv <- prcomp(datos_pv_cluster, scale. = TRUE)
pca_data <- data.frame(PC1 = pca_pv$x[,1],
                       PC2 = pca_pv$x[,2],
                       cluster = datos_pv$cluster[1:nrow(pca_pv$x)])

library(ggplot2)
ggplot(pca_data, aes(x = PC1, y = PC2, color = cluster)) +
  geom_point(alpha = 0.7, size = 2) +
  labs(title = "Clustering K-means en País Vasco (PCA)",
       x = "PC1", y = "PC2") +
  theme_minimal() +
  scale_color_brewer(palette = "Set2")

# Distribución sociodemográfica por clúster
# Por género
tabla_sexo <- table(datos_pv$SEXO, datos_pv$cluster)
sexo_df <- as.data.frame(tabla_sexo)
colnames(sexo_df) <- c("Genero", "Cluster", "Frecuencia")

library(scales)
ggplot(sexo_df, aes(x = Cluster, y = Frecuencia, fill = Genero)) +
  geom_bar(stat = "identity", position = "fill") +
  labs(title = "Distribución de género por clúster - País Vasco",
       y = "Porcentaje", x = "Clúster") +
  scale_y_continuous(labels = percent) +
  scale_fill_brewer(palette = "Pastel2") +
  theme_minimal()

# Por provincia
tabla_prov <- table(datos_pv$PROV, datos_pv$cluster)
prov_df <- as.data.frame(tabla_prov)
colnames(prov_df) <- c("Provincia", "Cluster", "Frecuencia")

ggplot(prov_df, aes(x = Cluster, y = Frecuencia, fill = Provincia)) +
  geom_bar(stat = "identity", position = "fill") +
  labs(title = "Distribución de provincia por clúster - País Vasco",
       y = "Porcentaje", x = "Clúster") +
  scale_y_continuous(labels = percent) +
  scale_fill_brewer(palette = "Set3") +
  theme_minimal()

# Por grupo de edad
datos_pv$grupo_edad <- cut(datos_pv$EDAD,
                           breaks = c(17, 24, 34, 49, 64, Inf),
                           labels = c("Jóvenes","Adultos emergentes","Adultos jóvenes","Adultos","Mayores"),
                           right = TRUE, include.lowest = TRUE)
tabla_edad <- table(datos_pv$grupo_edad, datos_pv$cluster)
edad_df <- as.data.frame(tabla_edad)
colnames(edad_df) <- c("GrupoEdad","Cluster","Frecuencia")

ggplot(edad_df, aes(x = Cluster, y = Frecuencia, fill = GrupoEdad)) +
  geom_bar(stat = "identity", position = "fill") +
  labs(title = "Distribución por grupo de edad por clúster - País Vasco",
       y = "Porcentaje", x = "Clúster") +
  scale_y_continuous(labels = percent) +
  scale_fill_brewer(palette = "YlOrBr") +
  theme_minimal()



# ==============================
# CLUSTERING - CONJUNTO 5 CCAA
# ==============================

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
