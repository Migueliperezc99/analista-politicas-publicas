# Sesión 1 - Analista de Políticas Públicas
# Fecha: 27 de julio de 2026

library(tidyverse)

# 1. Un vector
edades_jovenes <- c(15, 17, 22, 24, 28, 19, 21)
mean(edades_jovenes)

# 2. Un data frame simple
municipios_huila <- tibble(
  municipio = c("Neiva", "Pitalito", "Garzon", "La Plata", "Campoalegre"),
  poblacion = c(357392, 133380, 100062, 60195, 35120)
)

print(municipios_huila)

# 3. Un gráfico simple
ggplot(municipios_huila, aes(x = reorder(municipio, poblacion), y = poblacion)) +
       geom_col(fill = "steelblue") + 
         coord_flip() +
         labs(title = "Poblacion - 5 municipios del Huila", 
              x = NULL, y = "Habitantes")