# Sesión 3

library(tidyverse)

# Ver el objeto (primeras filas y columnas que caben)
starwars

# Dimensiones (filas x columnas)
dim(starwars)

# Estructura y tipos de cada columna
glimpse(starwars)

# Resumen estadístico
summary(starwars)

# Cuántos personajes por especie
starwars |> count(species, sort = TRUE)

# Cuántos por planeta natal
starwars |> count(homeworld, sort = TRUE)

# Valores únicos de sexo
starwars |> distinct(sex)

# Pregunta 1: ¿Cuáles son los 10 personajes más altos? Muestra nombre, altura y 
# especie.

starwars |>
  arrange(desc(height)) |>
  select(name, height, species) |>
  head(10)

# Pregunta 2: ¿Cuántos personajes hay por especie, ordenado de mayor a menor?

# Opción A
starwars |>
  count(species, sort = TRUE)

# Opción B
starwars |>
  group_by(species) |>
  summarise(n = n()) |>
  arrange(desc(n))

# Pregunta 3: Altura y peso promedio por especie, considerando solo especies con
# más de 1 personaje. Ordena por altura promedio descendente.

# Opción A
starwars |>
  summarise(
    n = n(),
    altura_prom = mean(height, na.rm = TRUE),
    peso_prom = mean(mass, na.rm = TRUE),
    .by = species
  ) |>
  filter(n > 1) |>
  arrange(desc(altura_prom))

# Opción B
starwars |>
  add_count(species) |>          # agrega columna n con el conteo por especie
  filter(n > 1) |>               # quédate con las que tienen más de 1
  summarise(
    altura_prom = mean(height, na.rm = TRUE),
    peso_prom   = mean(mass, na.rm = TRUE),
    .by = species
  ) |>
  arrange(desc(altura_prom))

# Pregunta 4: Calcula el Índice de Masa Corporal (IMC = peso / altura² en 
# metros) de cada personaje. Muestra los 5 con IMC más alto, ignorando 
# personajes sin datos.

starwars |>
  mutate(imc = mass / (height/100)^2) |>
  filter(!is.na(imc)) |>
  arrange(desc(imc)) |>
  select(name, height, mass, imc) |>
  head(5)

#Pregunta 5 (la más difícil): Encuentra los personajes cuyo peso está por encima
# del promedio de peso de su especie. Muestra nombre, especie, peso, promedio de
# la especie y la diferencia. Ordena por diferencia descendente.

starwars |>
  filter(!is.na(mass), !is.na(species)) |>
  mutate(mass_prom_esp = mean(mass), na.r = T, .by = species) |>
  filter(mass > mass_prom_esp) |> 
  mutate(diferencia = mass - mass_prom_esp) |>
  select(name, species, mass, mass_prom_esp, diferencia) |>
  arrange(desc(diferencia))

# Leer un CSV desde una URL

# Dataset de TidyTuesday: uso de teléfonos móviles por país
url <- "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-11-10/mobile.csv"

datos <- read_csv(url)

glimpse(datos)
head(datos)

# Explora
datos |> distinct(entity) |> head(20)
datos |>
  filter(entity == "Colombia") |>
  arrange(year)

# ¿Cuál fue el PIB de Colombia los primeros 2 años de cada década?

datos |> 
  filter(entity == "Colombia", year %% 10 <= 1) |>
  mutate(pib_colombia = total_pop*gdp_per_cap) |>
  select(year, total_pop, gdp_per_cap, pib_colombia)
  
