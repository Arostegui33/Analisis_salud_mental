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
