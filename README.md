# Análisis del Impacto del COVID-19 en la salud mental en España

Este proyecto analiza el impacto de la pandemia COVID-19 en la salud mental centrándose en cinco comunidades autónomas de España (Andalucía, Castilla y León, Galicia, Madrid y País Vasco), utilizando diferentes técnicas de análisis estadístico como PCA, modelo LDA, modelo Random forest y Clustering

## Estructura del repositorio

**datos/**
- datos_filtrados.csv
- datos_filtrados_conjunta.csv
- datos_filtrados_normalizados.csv

**R/**
- PCA.R
- LDA_RF.R
- Clustering.R

**Resultados/**
- clustering/
- lda/
- pca/
- rf/



## Datos

- datos_filtrados.csv: Datos filtrados por las 5 CCAA.
- datos_filtrados_conjunta.csv: Datos combinados de todas las CCAA, incluyendo dos variables adicionales creadas para algunas técnicas: `impacto_raw` e `impacto_salud_mental`.
- datos_filtrados_normalizados.csv: Datos normalizados para PCA y clustering.

Cada dataset contiene variables sobre síntomas emocionales, impacto percibido, distribución sociodemográfica (edad, sexo, provincia) y factores adicionales.

## Análisis realizados
1. **Análisis de Componentes Principales (PCA)**
   - Reducción de dimensionalidad.
   - Identificación de patrones y componentes principales por CCAA.
   - Resultados visualizados mediante biplots y gráficos de varianza explicada.

2. **Modelos de clasificación**
   - Comparativa de LDA vs Random Forest para predecir nivel de impacto en salud mental.
   - Evaluación mediante matriz de confusión, AUC y métricas de precisión.

3. **Análisis de Clustering (K-means)**
   - Determinación del número óptimo de clústeres por CCAA y para el conjunto de 5 CCAA.
   - Perfil medio por clúster (síntomas, impacto y distribución sociodemográfica).
   - Visualizaciones mediante heatmaps, PCA y gráficos de barras.
   
## Resultados
- Todos los gráficos y tablas generados se encuentran en la carpeta `resultados/`.
- Los subdirectorios están organizados por técnica:
  - `PCA/`: Componentes principales y biplots.
  - `LDA_RF/`: Matrices de confusión, curvas ROC y variables importantes.
  - `clustering/`: Heatmaps, visualizaciones K-means, PCA y distribución por clúster.

## Reproducir el análisis
1. Clonar el repositorio
2. Abrir RStudio
3. Ejecutar los scripts en orden: 01_carga_paquetes.R → 02_preprocesamiento.R → PCA + LDA → Random Forest → Clustering
4. Los resultados se guardarán automáticamente en Resultados/

## Contacto

- Nombre: Ángel Arostegui Gros
- Email: arosteguigrosangel1994@gmail.com
- GitHub: Arostegui33 (https://github.com/Arostegui33)

## Licencia

Licencia: MIT