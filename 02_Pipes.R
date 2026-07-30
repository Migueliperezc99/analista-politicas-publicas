# Sesión 2

# Tipos básicos

library(tidyverse)

x <- 42 # numeric (double)
y <- "hola" # character
z <- TRUE # logical
w <- NA # missing

class(x); class(y); class(z); class(w)

# Vectores (elementos del mismo tipo)

edades <- c(15, 18, 22, 25, 28)
nombres <- c("Ana", "Luis", "Marta", "Carlos", "Sofia")
empleados <- c(TRUE, FALSE, TRUE, TRUE, FALSE)

length(edades)
mean(edades)
sum(empleados) # TRUE = 1, FALSE = 0 -> Cuenta cuantos TRUE hay

# Coercion (R convierte para que el vector sea homogéneo)
mezcla <- c(1, "dos", TRUE)
mezcla
class(mezcla)

# Missing values
notas <- c(4.5, 3.8, NA, 4.2, NA)
mean(notas)
mean(notas, na.rm = TRUE) # Ignora NA

# Factores (categorias)
nivel_educ <- factor(c("Primaria", "Secundaria", "Universidad", "Secundaria"),
                     levels = c("Primaria", "Secundaria", "Universidad"),
                     ordered = TRUE)
nivel_educ

# Tibble: la version moderna de data.frame
jovenes_huila <- tibble(
  id       = 1:8,
  nombre   = c("Ana","Luis","Marta","Carlos","Sofia","Pedro","Lucia","Andres"),
  edad     = c(17, 22, 19, 25, 28, 16, 23, 20),
  empleado = c(FALSE, TRUE, FALSE, TRUE, TRUE, FALSE, TRUE, FALSE),
  ingreso  = c(0, 1200000, 0, 2500000, 3200000, 0, 1800000, 0),
  municipio= c("Neiva","Pitalito","Neiva","Garzon","Neiva","La Plata","Pitalito","Neiva")
)

jovenes_huila
glimpse(jovenes_huila)   # vista rapida ideal para chequear tipos

#Dplyr con pipe

# 1. filter() - selecciona FILAS que cumplen una condicion
jovenes_huila |>
  filter(empleado == TRUE)

jovenes_huila |>
  filter(edad >= 18 & municipio == "Neiva")

# 2. select() - selecciona COLUMNAS
jovenes_huila |>
  select(nombre, edad, ingreso)

# 3. mutate() - crea o modifica columnas
jovenes_huila |>
  mutate(
    ingreso_smmlv = ingreso / 1300000,
    grupo_edad    = if_else(edad < 20, "15-19", "20-28")
  )

# 4. arrange() - ordena filas
jovenes_huila |>
  arrange(desc(ingreso))

# 5. summarise() + group_by() - resumen agrupado (el mas poderoso)
jovenes_huila |>
  group_by(municipio) |>
  summarise(
    n           = n(),
    edad_prom   = mean(edad),
    tasa_empleo = mean(empleado),
    ingreso_med = median(ingreso)
  )

# Ejercicio:"¿Cuál es el ingreso promedio de los jóvenes empleados de 20-28 años por municipio, ordenado de mayor a menor?"

# Mi versión
jovenes_huila |>
  filter(empleado == TRUE, edad >= 20 & edad <= 28) |>
  group_by(municipio) |>
  summarise(ingreso_med = mean(ingreso)) |>
  arrange(desc(ingreso_med))
    
# Versión mejorada
jovenes_huila |>
  filter(empleado, between(edad, 20, 28)) |>
  summarise(
    ingreso_med = mean(ingreso, na.rm = TRUE),
    .by = municipio
  ) |>
  arrange(desc(ingreso_med))

