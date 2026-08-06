# ================================================================
# ANALISIS SABER 11 - HUILA (2010-2022)
# ================================================================
#
# Autor    : Miguel Perez
# Proyecto : Pensum Analista de Politicas Publicas - Perfil MEL
# Sesion   : 7 (Semana 2) - Tidyverse aplicado a datos reales
#
# Objetivo
# --------
# Practicar los verbos esenciales de dplyr sobre un dataset real de
# ICFES Saber 11 filtrado al departamento de Huila, con enfoque
# MEL (Monitoring, Evaluation and Learning).
#
# Fuente de datos
# ---------------
# ICFES - Resultados Unicos Saber 11 (2010-2022)
# Datos Abiertos Colombia
# https://www.datos.gov.co/Educaci-n/Resultados-nicos-Saber-11/kgxf-xxbe
# Filtrado en el portal a COLE_DEPTO_UBICACION = HUILA antes de exportar.
#
# Archivo local
# -------------
# saber11_huila.csv (~500 MB, excluido del repositorio por tamano y
# por buenas practicas sobre datos con informacion personal identificable).
#
# ================================================================


# ================================================================
# 1. SETUP
# ================================================================
# Instalacion de paquetes (correr una sola vez, luego dejar comentado).
# install.packages("data.table")
# install.packages("janitor")

library(tidyverse)   # dplyr, tidyr, ggplot2, purrr, stringr, forcats, readr
library(data.table)  # fread(): lectura rapida de CSVs grandes
library(janitor)     # clean_names(): nombres de columna consistentes


# ================================================================
# 2. CARGA Y LIMPIEZA INICIAL DE LA BASE
# ================================================================
# fread() es ~10x mas rapido que read_csv() para archivos > 100 MB.
# encoding = "UTF-8" preserva las tildes y ñ en textos.
# rename_with(tolower) normaliza nombres de columna a minusculas
# (ICFES publica los headers en MAYUSCULAS por convencion antigua).

saber11 <- fread("saber11_huila.csv", encoding = "UTF-8") |>
  as_tibble() |>
  rename_with(tolower)


# ================================================================
# 3. DATA QUALITY ASSESSMENT (DQA) - HALLAZGOS
# ================================================================
# Antes de reportar cualquier cifra, documentamos los problemas de
# calidad detectados en el dataset. En trabajo MEL profesional esta
# seccion es clave para transparencia y reproducibilidad ante
# donantes y auditores.
#
# HALLAZGO 1 - Doble version del nombre de municipio (con/sin tilde)
#   Ejemplo: "SANTA MARIA" y "SANTA MARÍA" son el mismo municipio
#   con codigo DIVIPOLA 41676, pero aparecen como registros distintos
#   por inconsistencia en la captura ICFES.
#   FIX: usar SIEMPRE cole_cod_mcpio_ubicacion, nunca el nombre.
#
# HALLAZGO 2 - Puntaje global vacio en 2010-2013
#   ICFES cambio la escala del examen en 2014 y no recalculo
#   punt_global para los anios previos.
#   FIX: analizar solo desde periodo >= 20142.
#
# HALLAZGO 3 - Periodos 20194 y 20224 con ~2x los estudiantes normales
#   ICFES consolido cohortes atrasadas por COVID en estas
#   aplicaciones extraordinarias (2018-2 fusionada en 20194,
#   2021-2 fusionada en 20224). 2020-2 no se aplico.
#   IMPLICACION: interpretar como agregados, no aplicaciones unicas.
#
# HALLAZGO 4 - Periodos de calendario B (primer semestre) muy pequenos
#   Registros con 16 a 500 estudiantes (vs ~14000 en aplicacion principal).
#   Su promedio es estadisticamente inestable.
#   FIX: filtrar por n() > 5000 para quedarnos con aplicaciones principales.


# ================================================================
# 4. EXPLORACION INICIAL
# ================================================================
# Dimensiones (esperado: ~176 mil filas x 51 columnas)
dim(saber11)

# Estructura y tipos de cada columna
glimpse(saber11)

# Rango de periodos disponibles
saber11 |>
  distinct(periodo) |>
  arrange(periodo) |>
  print(n = Inf)

# Estudiantes por periodo (revela hallazgos 3 y 4)
saber11 |> count(periodo)

# Municipios de Huila representados (esperado: 37)
saber11 |> distinct(cole_mcpio_ubicacion) |> nrow()

# Distribucion por municipio (revela hallazgo 1: nombres duplicados por tildes)
saber11 |>
  count(cole_mcpio_ubicacion, sort = TRUE) |>
  print(n = Inf)

# Contexto del colegio
saber11 |> count(cole_area_ubicacion)   # urbano / rural
saber11 |> count(cole_naturaleza)       # oficial / no oficial

# Verificacion explicita del hallazgo 1 - mismo codigo DIVIPOLA, dos nombres
saber11 |>
  filter(cole_mcpio_ubicacion %in% c("SANTA MARÍA", "SANTA MARIA",
                                     "YAGUARÁ",     "YAGUARA",
                                     "ÍQUIRA",      "IQUIRA",
                                     "NÁTAGA",      "NATAGA",
                                     "ELÍAS",       "ELIAS")) |>
  distinct(cole_mcpio_ubicacion, cole_cod_mcpio_ubicacion) |>
  arrange(cole_cod_mcpio_ubicacion)


# ================================================================
# 5. DICCIONARIO DE NOMBRES CANONICOS DE MUNICIPIO
# ================================================================
# Construye una tabla auxiliar con UN solo nombre "canonico" por
# codigo DIVIPOLA (elige el mas frecuente en el dataset).
# Sirve para:
#   - Calcular y filtrar por codigo (sin ambiguedad de tildes).
#   - Presentar resultados con nombre legible via left_join.

nombres_muni <- saber11 |>
  count(cole_cod_mcpio_ubicacion, cole_mcpio_ubicacion, sort = TRUE) |>
  slice_head(n = 1, by = cole_cod_mcpio_ubicacion) |>
  select(cole_cod_mcpio_ubicacion, nombre_muni = cole_mcpio_ubicacion)

nombres_muni


# ================================================================
# 6. PREGUNTAS DE ANALISIS
# ================================================================


# ----------------------------------------------------------------
# Pregunta 1 - Evolucion de la calidad educativa 2014-2022
# ----------------------------------------------------------------
# Contexto (jefe):
#   "Necesito para el comite de manana con la Secretaria de
#   Educacion de Huila un pantallazo de como hemos evolucionado
#   en calidad educativa."
#
# Decisiones metodologicas:
#   - Unidad de analisis : aplicacion (periodo)
#   - Metrica            : puntaje global promedio
#   - Filtros            : excluir periodos con < 5000 estudiantes
#                          (calendario B) y con punt_global NA (2010-2013)
#   - Presentacion       : tabla ordenada cronologicamente

evolucion_huila <- saber11 |>
  filter(!is.na(punt_global),
         n() > 5000, .by = periodo) |>
  summarise(
    n             = n(),
    punt_promedio = round(mean(punt_global, na.rm = TRUE), 1),
    .by = periodo
  ) |>
  arrange(periodo)

evolucion_huila

# Nota metodologica al lector:
#   (a) 2010-2013 excluidos por ausencia de punt_global (cambio escala ICFES 2014).
#   (b) Se muestra solo el periodo aplicativo principal por anio.
#   (c) 20194 y 20224 consolidan cohortes atrasadas por COVID.


# ----------------------------------------------------------------
# Pregunta 2 - Priorizacion de municipios para intervencion
# ----------------------------------------------------------------
# Contexto (jefe):
#   "La Fundacion Bolivar Davivienda esta evaluando fondear un
#   programa de fortalecimiento pedagogico focalizado en Huila.
#   Que municipios deberian priorizar?"
#
# Interpretacion (a validar con donante):
#   'Priorizar' = municipios con mayor 'deuda educativa agregada',
#   entendida como (puntaje_referencia - puntaje) * n_estudiantes.
#   Este enfoque combina severidad (que tan bajo esta el puntaje)
#   con escala (cuantos estudiantes estan afectados).
#
# Decisiones metodologicas:
#   - Anio               : 20224 (mas reciente con datos robustos)
#   - Umbral n >= 30     : convencion ICFES para reportes por establecimiento
#                          y minimo estadistico del Teorema del Limite Central
#   - Puntaje referencia : 250 (nominal aceptable, escala 0-500)
#   - Metrica principal  : estudiantes_afectados

priorizacion_municipios <- saber11 |>
  filter(periodo == 20224) |>
  summarise(
    n             = n(),
    punt_promedio = mean(punt_global, na.rm = TRUE),
    .by = cole_cod_mcpio_ubicacion
  ) |>
  filter(n >= 30) |>
  left_join(nombres_muni, by = "cole_cod_mcpio_ubicacion") |>
  mutate(
    brecha_puntaje        = 250 - punt_promedio,
    estudiantes_afectados = n * brecha_puntaje
  ) |>
  arrange(desc(estudiantes_afectados)) |>
  head(15) |>
  select(nombre_muni, cole_cod_mcpio_ubicacion,
         n, punt_promedio, brecha_puntaje, estudiantes_afectados)

priorizacion_municipios

# Nota metodologica al lector:
#   Municipios ordenados por deuda educativa agregada (brecha
#   respecto a 250 multiplicada por n). Municipios excluidos por
#   n < 30 no aparecen en el ranking.


# ----------------------------------------------------------------
# Pregunta 3 - Brecha oficial vs no oficial y su evolucion
# ----------------------------------------------------------------
# Contexto (jefe):
#   "Para el informe trimestral al donante: hay brecha entre
#   estudiantes de colegios oficiales y no oficiales en Huila?
#   Y sobre todo, esa brecha se ha reducido o crecido con el tiempo?"
#
# Decisiones metodologicas:
#   - Dos dimensiones    : periodo x cole_naturaleza
#   - Filtros            : NA en naturaleza y punt_global; n>5000 por periodo
#   - Estructura         : pivot_wider para tener una columna por naturaleza
#                          y calcular la brecha con un mutate simple
#   - Tendencia          : lag() sobre la columna brecha
#   - CUIDADO            : cuando se usa lag(), arrange() debe ir ANTES
#                          del mutate. De lo contrario compara filas
#                          desordenadas y da tendencia incorrecta.

brecha_oficial <- saber11 |>
  filter(!is.na(cole_naturaleza),
         !is.na(punt_global)) |>
  filter(n() > 5000, .by = periodo) |>
  summarise(
    punt_promedio = mean(punt_global, na.rm = TRUE),
    .by = c(periodo, cole_naturaleza)
  ) |>
  pivot_wider(
    names_from  = cole_naturaleza,
    values_from = punt_promedio
  ) |>
  clean_names() |>
  arrange(periodo) |>
  mutate(
    brecha    = no_oficial - oficial,
    tendencia = case_when(
      is.na(lag(brecha))    ~ "Referencia",
      brecha >  lag(brecha) ~ "Crece",
      brecha <  lag(brecha) ~ "Se reduce",
      brecha == lag(brecha) ~ "Igual"
    )
  )

brecha_oficial

# Hallazgo:
#   La brecha (no_oficial - oficial) se mantuvo entre 3-8 puntos
#   entre 2014-2019, pero salto a 14.6 puntos en 2022.
# Hipotesis:
#   La pandemia amplifico la desigualdad. Los colegios no oficiales
#   tuvieron mejor infraestructura para virtualidad y familias con
#   mayor acompanamiento, mientras que los oficiales quedaron
#   rezagados. La tendencia amerita seguimiento en Saber 11 2023-2024.


# ================================================================
# FIN
# ================================================================ |> 