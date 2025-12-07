# 🐕 Análisis de Hogares con Mascotas - EPF 2022-2024

Este proyecto analiza los hogares con mascotas en la Encuesta de Presupuestos Familiares (EPF) de España. Utilizando un dashboard interactivo desarrollado con Plotly+Dash y análisis estadísticos en R, exploramos los gatos y proporciondes de hogares segun varias clasificaciones demográficas.
---

## 🚀 Inicio Rápido

### Opción 1: Dashboard Interactivo (Recomendado)

```bash
# Ejecutar el script de inicio
./iniciar_dashboard.sh

# O manualmente:
python3 dashboard_mascotas.py
```

Luego abre en tu navegador: **http://localhost:8050**

### Opción 2: Análisis en R

```bash
# Ejecutar análisis completo
Rscript analisis_comparativa_mascotas.R

# Ver PDFs generados
open Outputs/analisis_comparativa_mascotas_22_24.pdf
```

---

## 📁 Archivos Principales

- **[dashboard_mascotas.py](dashboard_mascotas.py)** - Dashboard interactivo con Plotly+Dash
- **[INSTRUCCIONES_DASHBOARD.md](INSTRUCCIONES_DASHBOARD.md)** - Guía completa del dashboard
- **[INFORME_ANALISIS_DUPLICACION_MASCOTAS.md](INFORME_ANALISIS_DUPLICACION_MASCOTAS.md)** - Informe ejecutivo completo
- **[analisis_comparativa_mascotas.R](analisis_comparativa_mascotas.R)** - Script de análisis en R

---

## 🎯 Características del Dashboard

✅ **6 pestañas interactivas**: Evolución, Distribuciones, Mapa de España, **Tamaño de Hogar** ⭐ NUEVO, Umbrales, Datos
✅ **Controles dinámicos**: Filtros por año, código y umbral de gasto
✅ **KPIs en tiempo real**: Actualizados automáticamente
✅ **Gráficos interactivos**: Zoom, tooltips, exportación
✅ **Mapa geográfico**: Visualización por Comunidad Autónoma
✅ **Análisis demográfico**: Comparativa de gastos por tamaño de hogar ⭐ NUEVO

---

## 📊 Hallazgos Principales

| Métrica | 2022 | 2023 | 2024 | Cambio |
|---------|------|------|------|--------|
| % Hogares con mascotas | 26.5% | 28.2% | **53.9%** | +103% |
| Gastos < 1€ | 0 | 47 | **805** | +1,713% |
| P5 (percentil 5) | 216€ | 29€ | **1€** | -99.5% |
| Mediana de gasto | 110,921€ | 94,016€ | **15,549€** | -86% |

**Conclusión**: El aumento NO es real, es un cambio metodológico en la captura de datos.

---

## 💻 Instalación

```bash
# Instalar dependencias Python
pip3 install -r requirements.txt

# Ejecutar dashboard
./iniciar_dashboard.sh
```

---

## 📖 Documentación Completa

- **[INSTRUCCIONES_DASHBOARD.md](INSTRUCCIONES_DASHBOARD.md)** - Guía paso a paso del dashboard
- **[INFORME_ANALISIS_DUPLICACION_MASCOTAS.md](INFORME_ANALISIS_DUPLICACION_MASCOTAS.md)** - Análisis detallado y recomendaciones
- **[Comparativa.md](Comparativa.md)** - Descripción del problema original

---

**¡Disfruta explorando los datos!** 🐕🐈📊
