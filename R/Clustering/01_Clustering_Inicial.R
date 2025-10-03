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
