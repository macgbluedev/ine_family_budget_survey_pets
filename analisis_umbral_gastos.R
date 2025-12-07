# Análisis estadístico para determinar umbral de gastos confiables
# Instalación y carga de librerías
if (!require("ggplot2")) install.packages("ggplot2", repos = "https://cloud.r-project.org")
if (!require("dplyr")) install.packages("dplyr", repos = "https://cloud.r-project.org")
if (!require("gridExtra")) install.packages("gridExtra", repos = "https://cloud.r-project.org")

library(ggplot2)
library(dplyr)
library(gridExtra)

# Leer datos
datos <- read.csv("Outputs/resultado.csv")

# Verificar estructura
cat("Dimensiones del dataset:", dim(datos), "\n")
cat("Primeras filas:\n")
head(datos)

# Análisis exploratorio de GASTO
cat("\n=== ESTADÍSTICAS DESCRIPTIVAS ===\n")
cat("Mínimo:", min(datos$GASTO, na.rm = TRUE), "\n")
cat("Máximo:", max(datos$GASTO, na.rm = TRUE), "\n")
cat("Media:", mean(datos$GASTO, na.rm = TRUE), "\n")
cat("Mediana:", median(datos$GASTO, na.rm = TRUE), "\n")
cat("Desviación estándar:", sd(datos$GASTO, na.rm = TRUE), "\n")

# Percentiles clave
percentiles <- quantile(datos$GASTO, probs = seq(0, 1, 0.05), na.rm = TRUE)
cat("\n=== PERCENTILES (cada 5%) ===\n")
print(percentiles)

# Análisis por código de producto
cat("\n=== GASTOS POR CÓDIGO DE PRODUCTO ===\n")
gastos_por_codigo <- datos %>%
  group_by(CODIGO) %>%
  summarise(
    n = n(),
    media = mean(GASTO, na.rm = TRUE),
    mediana = median(GASTO, na.rm = TRUE),
    min = min(GASTO, na.rm = TRUE),
    max = max(GASTO, na.rm = TRUE),
    p5 = quantile(GASTO, 0.05, na.rm = TRUE),
    p25 = quantile(GASTO, 0.25, na.rm = TRUE),
    p75 = quantile(GASTO, 0.75, na.rm = TRUE)
  )
print(gastos_por_codigo)

# Crear visualizaciones
pdf("Outputs/analisis_estadistico_gastos.pdf", width = 14, height = 10)

# 1. Histograma con escala logarítmica
p1 <- ggplot(datos, aes(x = GASTO)) +
  geom_histogram(bins = 100, fill = "steelblue", alpha = 0.7) +
  scale_x_log10(labels = scales::comma) +
  labs(title = "Distribución de Gastos (escala logarítmica)",
       x = "Gasto (escala log10)",
       y = "Frecuencia") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# 2. Boxplot por código
p2 <- ggplot(datos, aes(x = as.factor(CODIGO), y = GASTO, fill = as.factor(CODIGO))) +
  geom_boxplot() +
  scale_y_log10(labels = scales::comma) +
  labs(title = "Distribución de Gastos por Código de Mascota",
       x = "Código",
       y = "Gasto (escala log10)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "none")

# 3. Curva de densidad
p3 <- ggplot(datos, aes(x = GASTO, color = as.factor(CODIGO))) +
  geom_density(size = 1) +
  scale_x_log10(labels = scales::comma) +
  labs(title = "Densidad de Probabilidad por Código",
       x = "Gasto (escala log10)",
       y = "Densidad",
       color = "Código") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# 4. Gráfico de percentiles acumulados
percentil_data <- data.frame(
  percentil = seq(0, 100, 1),
  valor = quantile(datos$GASTO, probs = seq(0, 1, 0.01), na.rm = TRUE)
)

p4 <- ggplot(percentil_data, aes(x = percentil, y = valor)) +
  geom_line(color = "darkred", size = 1) +
  geom_point(size = 2, alpha = 0.5) +
  scale_y_log10(labels = scales::comma) +
  geom_vline(xintercept = c(5, 10, 25), linetype = "dashed", color = "blue", alpha = 0.5) +
  geom_hline(yintercept = quantile(datos$GASTO, c(0.05, 0.10, 0.25), na.rm = TRUE),
             linetype = "dashed", color = "blue", alpha = 0.5) +
  annotate("text", x = 5, y = max(datos$GASTO), label = "P5", color = "blue", vjust = -0.5) +
  annotate("text", x = 10, y = max(datos$GASTO), label = "P10", color = "blue", vjust = -0.5) +
  annotate("text", x = 25, y = max(datos$GASTO), label = "P25", color = "blue", vjust = -0.5) +
  labs(title = "Curva de Percentiles de Gastos",
       subtitle = "Líneas azules marcan percentiles 5, 10 y 25",
       x = "Percentil",
       y = "Valor de Gasto (escala log10)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5))

# Mostrar gráficos
grid.arrange(p1, p2, p3, p4, ncol = 2)

# 5. Análisis de outliers usando método IQR
cat("\n=== ANÁLISIS DE OUTLIERS (Método IQR) ===\n")
for (codigo in unique(datos$CODIGO)) {
  subset_data <- datos %>% filter(CODIGO == codigo)

  Q1 <- quantile(subset_data$GASTO, 0.25, na.rm = TRUE)
  Q3 <- quantile(subset_data$GASTO, 0.75, na.rm = TRUE)
  IQR_val <- Q3 - Q1

  limite_inferior <- Q1 - 1.5 * IQR_val
  limite_superior <- Q3 + 1.5 * IQR_val

  outliers_bajos <- sum(subset_data$GASTO < limite_inferior, na.rm = TRUE)
  outliers_altos <- sum(subset_data$GASTO > limite_superior, na.rm = TRUE)

  cat(sprintf("\nCódigo %s:\n", codigo))
  cat(sprintf("  Q1: %.2f, Q3: %.2f, IQR: %.2f\n", Q1, Q3, IQR_val))
  cat(sprintf("  Límite inferior (outliers bajos): %.2f\n", limite_inferior))
  cat(sprintf("  Límite superior (outliers altos): %.2f\n", limite_superior))
  cat(sprintf("  Outliers bajos: %d (%.1f%%)\n", outliers_bajos, 100*outliers_bajos/nrow(subset_data)))
  cat(sprintf("  Outliers altos: %d (%.1f%%)\n", outliers_altos, 100*outliers_altos/nrow(subset_data)))
}

# 6. Tabla de umbrales recomendados
cat("\n=== UMBRALES RECOMENDADOS ===\n")
umbrales_recomendados <- data.frame(
  Criterio = c("Percentil 1%", "Percentil 5%", "Percentil 10%",
               "Media - 2*SD", "Mediana/10"),
  Valor = c(
    quantile(datos$GASTO, 0.01, na.rm = TRUE),
    quantile(datos$GASTO, 0.05, na.rm = TRUE),
    quantile(datos$GASTO, 0.10, na.rm = TRUE),
    max(0, mean(datos$GASTO, na.rm = TRUE) - 2*sd(datos$GASTO, na.rm = TRUE)),
    median(datos$GASTO, na.rm = TRUE) / 10
  ),
  Registros_eliminados = c(
    sum(datos$GASTO < quantile(datos$GASTO, 0.01, na.rm = TRUE), na.rm = TRUE),
    sum(datos$GASTO < quantile(datos$GASTO, 0.05, na.rm = TRUE), na.rm = TRUE),
    sum(datos$GASTO < quantile(datos$GASTO, 0.10, na.rm = TRUE), na.rm = TRUE),
    sum(datos$GASTO < max(0, mean(datos$GASTO, na.rm = TRUE) - 2*sd(datos$GASTO, na.rm = TRUE)), na.rm = TRUE),
    sum(datos$GASTO < median(datos$GASTO, na.rm = TRUE) / 10, na.rm = TRUE)
  )
)
umbrales_recomendados$Porcentaje <- round(100 * umbrales_recomendados$Registros_eliminados / nrow(datos), 2)
print(umbrales_recomendados)

# 7. Gráfico de umbrales
p5 <- ggplot(datos, aes(x = GASTO)) +
  geom_histogram(bins = 100, fill = "lightblue", alpha = 0.7) +
  scale_x_log10(labels = scales::comma) +
  geom_vline(data = umbrales_recomendados,
             aes(xintercept = Valor, color = Criterio),
             linetype = "dashed", size = 1) +
  labs(title = "Umbrales Propuestos para Filtrado",
       x = "Gasto (escala log10)",
       y = "Frecuencia",
       color = "Umbral") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "bottom")

print(p5)

dev.off()

cat("\n\nAnálisis completado. Gráficos guardados en 'Outputs/analisis_estadistico_gastos.pdf'\n")
cat("\n=== RECOMENDACIÓN ===\n")
cat("Basado en el análisis, se recomienda usar el Percentil 5% como umbral mínimo.\n")
cat(sprintf("Valor umbral: %.2f\n", quantile(datos$GASTO, 0.05, na.rm = TRUE)))
cat("Este criterio elimina el 5% de los valores más bajos, que probablemente son poco confiables.\n")
