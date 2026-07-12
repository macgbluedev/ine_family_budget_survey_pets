# Análisis Comparativo de Hogares con Mascotas 2022-2024
# Objetivo: Investigar por qué se duplicó el número de hogares con mascotas en 2024

# Instalación y carga de librerías
if (!require("ggplot2")) install.packages("ggplot2", repos = "https://cloud.r-project.org")
if (!require("dplyr")) install.packages("dplyr", repos = "https://cloud.r-project.org")
if (!require("tidyr")) install.packages("tidyr", repos = "https://cloud.r-project.org")
if (!require("gridExtra")) install.packages("gridExtra", repos = "https://cloud.r-project.org")

library(ggplot2)
library(dplyr)
library(tidyr)
library(gridExtra)

# Leer datos
datos <- read.csv("GastosSoloMascotas22a24.csv", encoding = "UTF-8")

# Limpiar nombres de columnas (eliminar BOM y caracteres especiales)
names(datos) <- gsub("^X\\.U\\.FEFF\\.", "", names(datos))
names(datos) <- gsub("\\.", "_", names(datos))

# Limpiar y convertir GASTO a numérico
# Los datos de 2024 tienen formato: " € 1,714,592.10 " o " € 1.07 "
# Los datos de 2022-2023 tienen formato: 1714592.10
datos$GASTO <- as.character(datos$GASTO)
datos$GASTO <- iconv(datos$GASTO, to = "ASCII//TRANSLIT")  # Convertir caracteres especiales
datos$GASTO <- gsub("[^0-9.]", "", datos$GASTO)  # Mantener solo números y puntos decimales
datos$GASTO <- as.numeric(datos$GASTO)     # Convertir a numérico

datos$ANOENC <- as.integer(datos$ANOENC)
datos$NUMERO <- as.integer(datos$NUMERO)
datos$CODIGO <- as.integer(datos$CODIGO)

# Muestra total de hogares por año (del documento Comparativa.md)
hogares_totales <- data.frame(
  ANOENC = c(2022, 2023, 2024),
  Total_Hogares = c(20585, 20707, 19410)
)

cat("=== DATOS CARGADOS ===\n")
cat("Total de registros:", nrow(datos), "\n")
cat("Columnas:", paste(names(datos), collapse = ", "), "\n\n")

# Normalizar códigos a 4 dígitos
datos <- datos %>%
  mutate(CODIGO_4D = ifelse(is.na(CODIGO_4D) | CODIGO_4D == "",
                             substr(as.character(CODIGO), 1, 4),
                             as.character(CODIGO_4D)))

# Verificar estructura
cat("\n=== PRIMERAS FILAS ===\n")
print(head(datos))

# Análisis 1: Distribución de registros por año y código
cat("\n=== REGISTROS POR AÑO Y CÓDIGO ===\n")
registros_por_ano <- datos %>%
  group_by(ANOENC, CODIGO) %>%
  summarise(
    n_registros = n(),
    .groups = "drop"
  )
print(registros_por_ano)

# Análisis 2: Estadísticas de GASTO por año
cat("\n=== ESTADÍSTICAS DE GASTO POR AÑO ===\n")
estadisticas_ano <- datos %>%
  group_by(ANOENC) %>%
  summarise(
    n_registros = n(),
    min_gasto = min(GASTO, na.rm = TRUE),
    p1 = quantile(GASTO, 0.01, na.rm = TRUE),
    p5 = quantile(GASTO, 0.05, na.rm = TRUE),
    p10 = quantile(GASTO, 0.10, na.rm = TRUE),
    mediana = median(GASTO, na.rm = TRUE),
    media = mean(GASTO, na.rm = TRUE),
    max_gasto = max(GASTO, na.rm = TRUE)
  )
print(estadisticas_ano)

# Análisis 3: Estadísticas de GASTO por año y código
cat("\n=== ESTADÍSTICAS DE GASTO POR AÑO Y CÓDIGO ===\n")
estadisticas_ano_codigo <- datos %>%
  group_by(ANOENC, CODIGO) %>%
  summarise(
    n_registros = n(),
    min_gasto = min(GASTO, na.rm = TRUE),
    p5 = quantile(GASTO, 0.05, na.rm = TRUE),
    p10 = quantile(GASTO, 0.10, na.rm = TRUE),
    mediana = median(GASTO, na.rm = TRUE),
    media = mean(GASTO, na.rm = TRUE),
    max_gasto = max(GASTO, na.rm = TRUE),
    .groups = "drop"
  )
print(estadisticas_ano_codigo)

# Análisis 4: Comparación de hogares con mascotas SIN FILTRO
cat("\n=== ANÁLISIS SIN FILTRO DE UMBRAL ===\n")
hogares_con_mascotas_sin_filtro <- datos %>%
  group_by(ANOENC, NUMERO) %>%
  summarise(tiene_mascota = n() > 0, .groups = "drop") %>%
  group_by(ANOENC) %>%
  summarise(hogares_con_mascota = sum(tiene_mascota), .groups = "drop") %>%
  left_join(hogares_totales, by = "ANOENC") %>%
  mutate(porcentaje = round(100 * hogares_con_mascota / Total_Hogares, 2))

cat("\nHogares con mascotas (SIN filtro de umbral):\n")
print(hogares_con_mascotas_sin_filtro)

# Análisis 5: Probar diferentes umbrales
umbrales_a_probar <- c(0, 0.5, 1.0, 1.05, 2.0, 5.0, 10.0, 50.0, 100.0)

resultados_umbrales <- data.frame()

for (umbral in umbrales_a_probar) {
  datos_filtrados <- datos %>% filter(GASTO >= umbral)

  resumen <- datos_filtrados %>%
    group_by(ANOENC, NUMERO) %>%
    summarise(tiene_mascota = n() > 0, .groups = "drop") %>%
    group_by(ANOENC) %>%
    summarise(
      hogares_con_mascota = sum(tiene_mascota),
      .groups = "drop"
    ) %>%
    left_join(hogares_totales, by = "ANOENC") %>%
    mutate(
      porcentaje = round(100 * hogares_con_mascota / Total_Hogares, 2),
      umbral = umbral
    )

  resultados_umbrales <- bind_rows(resultados_umbrales, resumen)
}

cat("\n=== RESULTADOS CON DIFERENTES UMBRALES ===\n")
print(resultados_umbrales %>%
        select(ANOENC, umbral, hogares_con_mascota, Total_Hogares, porcentaje) %>%
        arrange(umbral, ANOENC))

# Crear PDF con visualizaciones
pdf("Outputs/analisis_comparativa_mascotas_22_24.pdf", width = 16, height = 12)

# Gráfico 1: Distribución de gastos por año (escala log)
p1 <- ggplot(datos, aes(x = GASTO, fill = as.factor(ANOENC))) +
  geom_histogram(bins = 100, alpha = 0.6, position = "identity") +
  scale_x_log10(labels = scales::comma) +
  facet_wrap(~ANOENC, ncol = 1) +
  labs(title = "Distribución de Gastos por Año (escala logarítmica)",
       subtitle = "Comparación 2022-2024",
       x = "Gasto (escala log10)",
       y = "Frecuencia",
       fill = "Año") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "none")

# Gráfico 2: Boxplot comparativo por año y código
p2 <- ggplot(datos, aes(x = as.factor(CODIGO), y = GASTO, fill = as.factor(ANOENC))) +
  geom_boxplot() +
  scale_y_log10(labels = scales::comma) +
  facet_wrap(~ANOENC, ncol = 3) +
  labs(title = "Distribución de Gastos por Código y Año",
       x = "Código",
       y = "Gasto (escala log10)",
       fill = "Año") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

grid.arrange(p1, p2, ncol = 1)

# Gráfico 3: Evolución del porcentaje de hogares con mascotas según umbral
p3 <- ggplot(resultados_umbrales, aes(x = umbral, y = porcentaje, color = as.factor(ANOENC), group = ANOENC)) +
  geom_line(size = 1.5) +
  geom_point(size = 3) +
  scale_x_log10(labels = scales::comma, breaks = umbrales_a_probar) +
  labs(title = "Porcentaje de Hogares con Mascotas según Umbral de Gasto Mínimo",
       subtitle = "¿Cómo cambia el % de hogares al filtrar gastos bajos?",
       x = "Umbral de Gasto Mínimo (escala log10)",
       y = "% Hogares con Mascotas",
       color = "Año") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "bottom")

print(p3)

# Gráfico 4: Comparación de hogares absolutos con diferentes umbrales
p4 <- ggplot(resultados_umbrales, aes(x = umbral, y = hogares_con_mascota, color = as.factor(ANOENC), group = ANOENC)) +
  geom_line(size = 1.5) +
  geom_point(size = 3) +
  scale_x_log10(labels = scales::comma, breaks = umbrales_a_probar) +
  labs(title = "Número Absoluto de Hogares con Mascotas según Umbral",
       x = "Umbral de Gasto Mínimo (escala log10)",
       y = "Número de Hogares",
       color = "Año") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "bottom")

print(p4)

# Gráfico 5: Tabla comparativa con umbrales clave
umbrales_clave <- c(0, 1.05, 5, 10, 100)
tabla_comparativa <- resultados_umbrales %>%
  filter(umbral %in% umbrales_clave) %>%
  select(ANOENC, umbral, hogares_con_mascota, porcentaje) %>%
  pivot_wider(names_from = ANOENC, values_from = c(hogares_con_mascota, porcentaje),
              names_sep = "_")

cat("\n=== TABLA COMPARATIVA CON UMBRALES CLAVE ===\n")
print(tabla_comparativa)

# Gráfico 6: Análisis de registros con gastos muy bajos (< 1€)
gastos_bajos <- datos %>%
  filter(GASTO < 1) %>%
  group_by(ANOENC) %>%
  summarise(
    n_gastos_bajos = n(),
    hogares_afectados = n_distinct(NUMERO)
  ) %>%
  left_join(hogares_totales, by = "ANOENC") %>%
  mutate(
    pct_hogares_afectados = round(100 * hogares_afectados / Total_Hogares, 2)
  )

cat("\n=== ANÁLISIS DE GASTOS < 1€ ===\n")
print(gastos_bajos)

p5 <- ggplot(gastos_bajos, aes(x = as.factor(ANOENC), y = hogares_afectados, fill = as.factor(ANOENC))) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(hogares_afectados, "\n(", pct_hogares_afectados, "%)")),
            vjust = -0.5, size = 5) +
  labs(title = "Hogares con al menos un gasto < 1€",
       subtitle = "Estos gastos muy bajos podrían ser errores o datos poco confiables",
       x = "Año",
       y = "Número de Hogares Afectados") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "none")

print(p5)

# Gráfico 7: Distribución acumulada de gastos por año
p6 <- ggplot(datos, aes(x = GASTO, color = as.factor(ANOENC))) +
  stat_ecdf(size = 1.2) +
  scale_x_log10(labels = scales::comma, limits = c(0.01, max(datos$GASTO))) +
  geom_vline(xintercept = c(1, 1.05, 5, 10), linetype = "dashed", alpha = 0.5) +
  labs(title = "Función de Distribución Acumulada de Gastos",
       subtitle = "¿Qué % de registros están por debajo de cada umbral?",
       x = "Gasto (escala log10)",
       y = "Proporción Acumulada",
       color = "Año") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "bottom")

print(p6)

dev.off()

# Análisis 6: Comparación de la PROPORCIÓN de gastos por código
cat("\n=== PROPORCIÓN DE GASTOS POR CÓDIGO EN CADA AÑO ===\n")
proporcion_codigos <- datos %>%
  group_by(ANOENC, CODIGO) %>%
  summarise(n_registros = n(), .groups = "drop") %>%
  group_by(ANOENC) %>%
  mutate(
    total_registros_ano = sum(n_registros),
    proporcion = round(100 * n_registros / total_registros_ano, 2)
  ) %>%
  select(ANOENC, CODIGO, n_registros, proporcion)

print(proporcion_codigos)

# CONCLUSIONES Y RECOMENDACIONES
cat("\n\n========================================\n")
cat("=== CONCLUSIONES Y RECOMENDACIONES ===\n")
cat("========================================\n\n")

cat("1. PROBLEMA IDENTIFICADO:\n")
cat("   El aumento del porcentaje de hogares con mascotas en 2024 podría deberse a:\n")
cat("   - Cambio en la metodología de recogida de datos\n")
cat("   - Inclusión de gastos muy pequeños que podrían ser errores\n")
cat("   - Cambios en los códigos COICOP entre años\n\n")

cat("2. ANÁLISIS DE UMBRALES:\n")
cat("   - Sin umbral (≥ 0€): Se observa el salto reportado en el problema\n")
cat("   - Con umbral de 1.05€ (P5): Filtra el 5% de gastos más bajos\n")
cat("   - Con umbral de 10€: Filtra gastos probablemente no significativos\n\n")

cat("3. RECOMENDACIÓN:\n")
cat("   Aplicar un umbral mínimo de gasto para considerar que un hogar tiene mascota.\n")
cat("   Umbrales sugeridos:\n")
cat("   - CONSERVADOR: 1.05€ (Percentil 5)\n")
cat("   - MODERADO: 5-10€\n")
cat("   - ESTRICTO: 50-100€\n\n")

cat("   Consultar el PDF generado para ver cómo cada umbral afecta los resultados.\n\n")

cat("4. ARCHIVOS GENERADOS:\n")
cat("   - Outputs/analisis_comparativa_mascotas_22_24.pdf\n\n")

cat("Análisis completado exitosamente.\n")
