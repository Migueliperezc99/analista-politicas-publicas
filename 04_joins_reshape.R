# Sesión 4 - Joins + case_when + reshape

# Tabla de personas
personas <- tibble(
  id       = 1:6,
  nombre   = c("Ana", "Luis", "Marta", "Carlos", "Sofia", "Pedro"),
  municipio_cod = c("Neiva", "Pitalito", "Neiva", "Garzon", "Neiva", "Timana")
)

# Tabla de municipios con datos adicionales
municipios <- tibble(
  municipio_cod = c("Neiva", "Pitalito", "Garzon", "La Plata"),
  poblacion     = c(357392, 133380, 100062, 60195),
  categoria     = c(1, 2, 4, 5)   # categoría municipal DNP
)

personas
municipios

# 1. inner_join: solo filas que estan en AMBAS tablas
personas |> inner_join(municipios, by = "municipio_cod")
# Resultado: 5 filas. Pedro (Timana) desaparece porque no esta en municipios.

# 2. left_join: todas las filas de la IZQUIERDA + info de la derecha si existe
personas |> left_join(municipios, by = "municipio_cod")
# Resultado: 6 filas. Pedro aparece con NA en poblacion y categoria.

# 3. right_join: todas las filas de la DERECHA + info de la izquierda si existe
personas |> right_join(municipios, by = "municipio_cod")
# Resultado: incluye La Plata con NA en nombre e id.

# 4. full_join: todas las filas de AMBAS tablas
personas |> full_join(municipios, by = "municipio_cod")
# Resultado: incluye Pedro (sin municipio) Y La Plata (sin persona).

# semi_join: quedate con filas de la izquierda que TIENEN match (pero no trae columnas)
personas |> semi_join(municipios, by = "municipio_cod")
# = "personas que viven en municipios que tenemos en la base"

# anti_join: quedate con filas de la izquierda que NO tienen match
personas |> anti_join(municipios, by = "municipio_cod")
# = "personas cuyo municipio NO esta en la base" → devuelve solo a Pedro

# Si las columnas se llaman distinto
personas2 <- personas |> rename(cod = municipio_cod)
personas2 |> left_join(municipios, by = c("cod" = "municipio_cod"))

# case_when

set.seed(42)
hogares <- tibble(
  hogar_id = 1:15,
  ingreso_mes = c(1200000, 3500000, 800000, 2200000, 300000, 5000000,
                  900000, 1500000, 12000000, 700000, 2800000, 1100000,
                  15000000, 950000, 400000)
)

# Tu turno: crea la columna 'clase_ingreso' con case_when
#"Pobre extremo" si ingreso_mes < 500000
#"Pobre" si ingreso_mes está entre 500.000 y 1.300.000 (1 SMMLV aprox)
#"Vulnerable" si está entre 1.300.000 y 3.500.000
#"Clase media" si está entre 3.500.000 y 10.000.000
#"Alto ingreso" si es mayor a 10.000.000

hogares

hogares |> filter(!is.na(ingreso_mes)) |>
                    mutate(
                      clase_ingreso = case_when(
                        ingreso_mes < 500000 ~ "Pobre extremo",
                        ingreso_mes < 1300000 ~ "Pobre",
                        ingreso_mes < 3500000 ~ "Vulnerable",
                        ingreso_mes < 10000000 ~ "Clase media",
                        TRUE ~ "Alto ingreso"
                      )
                      ) |> 
  count(clase_ingreso, sort = T)
  
# pivot: cambiar la forma de la tabla

# Formato ANCHO (una columna por año - facil de leer, dificil de analizar)
matricula <- tibble(
  municipio = c("Neiva", "Pitalito", "Garzon"),
  `2020` = c(45000, 22000, 18000),
  `2021` = c(46200, 22500, 18400),
  `2022` = c(47100, 23000, 19100),
  `2023` = c(48500, 23800, 19700)
)

matricula

matricula_larga <- matricula |>
  pivot_longer(
    cols      = `2020`:`2023`,       # columnas a apilar
    names_to  = "ano",               # nueva columna con los nombres
    values_to = "estudiantes"        # nueva columna con los valores
  )

matricula_larga

# Crecimiento anual
matricula_larga |>
  arrange(municipio, ano) |>
  mutate(crecimiento = estudiantes - lag(estudiantes), .by = municipio)

matricula_larga |>
  pivot_wider(names_from = ano, values_from = estudiantes)



# Comentario para probar el flujo de Git 