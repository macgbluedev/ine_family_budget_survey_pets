# 📊 Dashboard Interactivo - Mascotas EPF 2022-2024

## 🚀 Inicio Rápido

### 1. Instalación de Dependencias

```bash
# Opción A: Instalar con pip
pip install -r requirements.txt

# Opción B: Instalar paquetes individualmente
pip install dash dash-bootstrap-components plotly pandas numpy
```

### 2. Ejecutar el Dashboard

```bash
python dashboard_mascotas.py
```

### 3. Acceder al Dashboard

Abre tu navegador en: **http://localhost:8050**

Para detener el servidor: presiona `Ctrl+C` en la terminal

---

## 🎯 Características del Dashboard

### 📌 6 Pestañas Principales

#### 1️⃣ **Evolución**
- Gráfico de barras: Hogares con mascotas por año
- Línea de tendencia: % de hogares
- Gráfico de barras agrupadas: Registros por tipo de gasto

#### 2️⃣ **Distribuciones**
- Histograma interactivo con escala logarítmica
- Boxplots por año y tipo de gasto
- Evolución de percentiles (P5, P25, P50, P75, P95)

#### 3️⃣ **Mapa de España**
- Mapa interactivo con burbujas por Comunidad Autónoma
- Ranking de top 10 CCAAs con más hogares con mascotas
- Comparación temporal entre años
- Tabla de estadísticas por CCAA
- Colores indican gasto mediano, tamaño indica número de hogares

#### 4️⃣ **Tamaño de Hogar** ⭐ NUEVO
- Distribución de hogares con mascotas por tamaño de hogar (número de personas)
- Gasto mediano y medio por tamaño de hogar
- Comparación entre años 2022-2024
- Distribución porcentual por tamaño
- Tabla de estadísticas detalladas

#### 5️⃣ **Análisis de Umbrales**
- Gráfico de sensibilidad: cómo cambia el % según el umbral
- Gráfico de hogares absolutos vs umbral
- Tabla comparativa de resultados

#### 6️⃣ **Datos**
- Estadísticas descriptivas por año
- Tabla con muestra de datos (primeros 100 registros)
- Información sobre registros filtrados

### 🎛️ Controles Interactivos

#### **Umbral Mínimo de Gasto**
- Slider de 0€ a 500€
- Valores predefinidos: 0, 1, 5, 10, 50, 100, 500€
- Actualización en tiempo real

#### **Selector de Años**
- Checkbox para seleccionar: 2022, 2023, 2024
- Permite comparar años específicos

#### **Selector de Códigos**
- 9321: Compra de mascotas
- 9322: Artículos para mascotas
- 9450: Servicios veterinarios

### 📈 KPIs en Tiempo Real

El dashboard muestra 4 indicadores clave que se actualizan automáticamente:

1. **Total Registros**: Número de gastos registrados
2. **Hogares con Mascotas**: Hogares únicos con al menos un gasto
3. **% Promedio**: Porcentaje promedio de hogares en los años seleccionados
4. **Gasto Mediano**: Valor mediano de los gastos

---

## 💡 Ejemplos de Uso

### Caso 1: Investigar el Aumento en 2024

1. **Deja todos los años seleccionados** (2022, 2023, 2024)
2. **Pon el umbral en 0€** (sin filtro)
3. **Ve a la pestaña "Evolución"**
   - Observa el salto del 26-28% al 54% en 2024

4. **Ajusta el umbral a 100€**
   - Observa que el aumento persiste
   - Conclusión: No son solo gastos pequeños

### Caso 2: Comparar Distribuciones de Gastos

1. **Ve a "Distribuciones"**
2. **Observa el histograma**
   - 2024 tiene muchos más gastos pequeños (< 10€)
3. **Revisa el gráfico de percentiles**
   - P5 en 2024: 1.05€
   - P5 en 2022: 216€
   - ¡Diferencia de 200x!

### Caso 2b: Explorar el Mapa Geográfico ⭐ NUEVO

1. **Ve a "Mapa de España"**
2. **Observa el mapa interactivo**
   - Las burbujas más grandes = más hogares con mascotas
   - Los colores más intensos = mayor gasto mediano
3. **Pasa el cursor sobre cada CCAA**
   - Verás hogares, registros y gastos
4. **Revisa el ranking de top 10**
   - ¿Qué comunidades tienen más hogares con mascotas?
5. **Si hay múltiples años seleccionados**
   - Ve la comparación temporal al final de la página

### Caso 3: Analizar Gasto por Tamaño de Hogar ⭐ NUEVO

1. **Ve a "Tamaño de Hogar"**
2. **Observa el primer gráfico de barras**
   - ¿Qué tamaños de hogar tienen más mascotas?
   - ¿Ha cambiado la distribución entre 2022 y 2024?
3. **Revisa los gráficos de gasto mediano y medio**
   - ¿Los hogares más grandes gastan más o menos?
   - ¿Hay diferencias significativas entre años?
4. **Analiza la distribución porcentual**
   - ¿Qué porcentaje de hogares con mascotas tiene cada tamaño?
5. **Revisa la tabla de estadísticas**
   - Compara gastos totales, medianos y medios

### Caso 4: Análisis de Sensibilidad

1. **Ve a "Análisis de Umbrales"**
2. **Observa las líneas**
   - 2022-2023: líneas casi planas
   - 2024: línea con pendiente significativa
3. **Interpretación**: En 2024 hay muchos hogares con gastos muy pequeños

### Caso 5: Filtrar Solo Servicios Veterinarios

1. **Desmarca** 9321 y 9322
2. **Deja solo** 9450 (Servicios veterinarios)
3. **Observa** cómo cambian los KPIs
4. **Ve a "Datos"** para ver estadísticas específicas

---

## 🎨 Características Interactivas

### En Todos los Gráficos:

- **Zoom**: Click y arrastra
- **Pan**: Click en icono de mano y arrastra
- **Reset**: Doble click en el gráfico
- **Tooltip**: Pasa el mouse sobre puntos/barras
- **Exportar**: Click en 📷 para descargar como PNG

### Gráficos de Líneas:
- **Ocultar/Mostrar series**: Click en la leyenda
- **Aislar una serie**: Doble click en la leyenda

### Gráficos Logarítmicos:
- Útiles para ver datos con rangos muy amplios
- El eje X o Y usa escala log₁₀

---

## 📁 Estructura de Archivos

```
.
├── dashboard_mascotas.py          # Script principal del dashboard
├── requirements.txt               # Dependencias Python
├── GastosSoloMascotas22a24.csv   # Datos de entrada (REQUERIDO)
├── INSTRUCCIONES_DASHBOARD.md     # Este archivo
└── Outputs/
    ├── analisis_comparativa_mascotas_22_24.pdf
    └── INFORME_ANALISIS_DUPLICACION_MASCOTAS.md
```

---

## 🔧 Solución de Problemas

### Error: "No module named 'dash'"

**Solución**: Instala las dependencias
```bash
pip install -r requirements.txt
```

### Error: "FileNotFoundError: GastosSoloMascotas22a24.csv"

**Solución**: Asegúrate de ejecutar el script desde el directorio correcto
```bash
cd /ruta/al/proyecto
python dashboard_mascotas.py
```

### El dashboard no se abre en el navegador

**Solución**: Abre manualmente http://localhost:8050

### Puerto 8050 en uso

**Solución**: Edita `dashboard_mascotas.py` línea final:
```python
app.run_server(debug=True, host='127.0.0.1', port=8051)  # Cambia a 8051
```

### Gráficos no se actualizan

**Solución**: Refresca la página (F5) o reinicia el servidor

---

## 🚀 Características Avanzadas

### Personalización de Colores

Edita la sección de cada gráfico en `dashboard_mascotas.py`:

```python
# Ejemplo: Cambiar colores de barras
fig1.add_trace(go.Bar(
    marker_color='tu_color_aqui',  # Ej: 'crimson', '#FF5733', 'rgb(255,87,51)'
))
```

### Añadir Nuevos Gráficos

1. Crea una función que genere tu gráfico con Plotly
2. Añádela a la función `crear_tab_*()` correspondiente
3. Usa `dcc.Graph(figure=tu_figura)` para mostrarlo

### Exportar Datos Filtrados

Añade este botón en el tab de datos:

```python
html.Button("Descargar CSV", id="btn-download"),
dcc.Download(id="download-dataframe-csv"),
```

Y el callback:

```python
@callback(
    Output("download-dataframe-csv", "data"),
    Input("btn-download", "n_clicks"),
    prevent_initial_call=True,
)
def descargar_csv(n_clicks):
    return dcc.send_data_frame(df_filtrado.to_csv, "datos_filtrados.csv")
```

---

## 📚 Recursos Adicionales

### Documentación Oficial:
- **Dash**: https://dash.plotly.com/
- **Plotly**: https://plotly.com/python/
- **Bootstrap Components**: https://dash-bootstrap-components.opensource.faculty.ai/

### Ejemplos de Gráficos:
- Galería Plotly: https://plotly.com/python/
- Ejemplos Dash: https://dash-gallery.plotly.host/Portal/

### Paletas de Colores:
- ColorBrewer: https://colorbrewer2.org/
- Plotly Colors: https://plotly.com/python/discrete-color/

---

## 🎯 Próximos Pasos Sugeridos

### Mejoras del Dashboard:

1. **Añadir filtro por Comunidad Autónoma** (columna CCAA)
2. **Gráfico de mapa de España** con densidad de hogares con mascotas
3. **Tabla interactiva** con ordenación y búsqueda (usar `dash-table`)
4. **Exportación de gráficos** en diferentes formatos (PNG, SVG, PDF)
5. **Comparación directa** 2024 vs 2023 (gráficos de diferencias)
6. **Análisis por tipo de hogar** (TIPHOGAR1, TAMANO)

### Análisis Adicionales:

7. **Correlación** entre gasto en mascotas e ingresos (INTERIN)
8. **Análisis geográfico**: ¿qué regiones tienen más mascotas?
9. **Distribución por tamaño de municipio** (TAMAMU)
10. **Patrones temporales**: si hay datos mensuales

---

## 📧 Soporte

Para preguntas o problemas:
1. Revisa la sección "Solución de Problemas"
2. Consulta la documentación oficial de Dash/Plotly
3. Revisa el informe completo: `INFORME_ANALISIS_DUPLICACION_MASCOTAS.md`

---

**¡Disfruta explorando los datos! 🐕🐈📊**
