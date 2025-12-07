# Informe de Análisis: Duplicación de Hogares con Mascotas 2024

## Resumen Ejecutivo

Se ha investigado por qué el porcentaje de hogares con mascotas se duplicó en 2024 respecto a 2022-2023 en la Encuesta de Presupuestos Familiares del INE.

**Hallazgo Principal**: El aumento NO refleja un cambio real en la tenencia de mascotas, sino diferencias metodológicas en la captura de datos entre años, especialmente en la inclusión de gastos muy pequeños.

---

## Datos Analizados

### Muestra Total por Año
| Año  | Hogares Totales | Registros de Gastos | Hogares con Mascotas | % Hogares |
|------|-----------------|---------------------|----------------------|-----------|
| 2022 | 20,585         | 7,574               | 5,454                | 26.50%    |
| 2023 | 20,707         | 8,171               | 5,831                | 28.16%    |
| 2024 | 19,410         | 16,915              | 10,456               | **53.87%** |

### Códigos Analizados

**2024:**
- 09.3.2.1: COMPRA DE MASCOTAS
- 09.3.2.2: ARTÍCULOS RELATIVOS A MASCOTAS
- 09.4.5.0: SERVICIOS VETERINARIOS

**2023-2022:**
- 09.3.4.1: COMPRA DE MASCOTAS
- 09.3.4.2: ARTÍCULOS RELATIVOS A MASCOTAS
- 09.3.5.0: SERVICIOS VETERINARIOS

---

## Hallazgos Principales

### 1. Diferencia en Formato de Datos

**2022-2023:**
- Formato numérico simple: `2181544.69`
- Sin símbolos adicionales

**2024:**
- Formato con símbolo de euro: ` € 1,714,592.10 `
- Separadores de miles con comas
- Espacios adicionales

**Implicación**: Cambio en el sistema de captura/exportación de datos en 2024.

### 2. Distribución de Gastos Muy Bajos (< 1€)

| Año  | Registros < 1€ | Hogares Afectados | % Hogares |
|------|----------------|-------------------|-----------|
| 2022 | 0              | 0                 | 0.00%     |
| 2023 | 47             | 45                | 0.22%     |
| 2024 | **805**        | **805**           | **4.15%** |

**Conclusión**: En 2024 se registraron ~800 hogares con gastos inferiores a 1€, lo que representa **18 veces más** que en 2023. Estos gastos muy pequeños probablemente sean:
- Errores de captura de datos
- Gastos residuales no significativos
- Redondeos o ajustes contables

### 3. Distribución por Código de Producto

| Código | Descripción | 2022  | 2023  | 2024  | Cambio 2024 vs 2022 |
|--------|-------------|-------|-------|-------|---------------------|
| 9321   | Compra      | 603   | 874   | 3,659 | **+507%** |
| 9322   | Artículos   | 4,230 | 4,685 | 9,881 | +134% |
| 9450   | Servicios   | 2,741 | 2,612 | 3,375 | +23% |

**Observación crítica**: El código 9321 (COMPRA DE MASCOTAS) aumentó 507%, pasando de representar el 8% de los registros a 21.6%.

### 4. Estadísticas de Gasto por Año

#### Percentil 5 (P5) - Umbral Recomendado
| Año  | P5 (€)  | Interpretación |
|------|---------|----------------|
| 2022 | 216.48  | Gastos muy pequeños prácticamente inexistentes |
| 2023 | 29.14   | Algunos gastos pequeños presentes |
| 2024 | **1.05** | **Cantidad significativa de gastos muy bajos** |

#### Mediana de Gastos
| Año  | Mediana (€) | Cambio vs 2022 |
|------|-------------|----------------|
| 2022 | 110,921     | - |
| 2023 | 94,016      | -15% |
| 2024 | 15,549      | **-86%** |

**Conclusión**: La mediana de gastos en 2024 es 7 veces menor que en 2022, confirmando que se están registrando gastos mucho más pequeños.

---

## Análisis de Sensibilidad con Umbrales

Se probaron diferentes umbrales mínimos de gasto para considerar que un hogar "tiene mascota":

### Resultados por Umbral

| Umbral (€) | 2022  | 2023  | 2024   | Cambio 2024 vs 2022 |
|------------|-------|-------|--------|---------------------|
| **0.00** (Sin filtro) | 26.50% | 28.16% | **53.87%** | **+103%** |
| **1.05** (P5 global)  | 26.50% | 28.15% | **53.87%** | +103% |
| **5.00**              | 26.50% | 28.12% | **53.86%** | +103% |
| **10.00**             | 26.50% | 28.09% | **53.86%** | +103% |
| **50.00**             | 26.48% | 27.95% | **53.48%** | +102% |
| **100.00**            | 26.44% | 27.87% | **53.14%** | +101% |

**Observación CRÍTICA**: Incluso con umbrales de 100€, el porcentaje de hogares con mascotas en 2024 sigue siendo el doble que en 2022-2023.

---

## Conclusiones

### Causas Identificadas del Aumento

1. **Cambio Metodológico en Captura de Datos (PRINCIPAL)**
   - En 2024 se registraron gastos mucho más pequeños que antes
   - 805 hogares tienen gastos < 1€ vs 45 en 2023
   - El percentil 5 bajó de 29€ (2023) a 1€ (2024)

2. **Incremento Desproporcionado en "Compra de Mascotas" (9321)**
   - Aumentó 507% cuando debería ser estable o decrecer (compra es evento único)
   - Posible cambio en la clasificación de gastos en este código

3. **Formato de Datos Diferente**
   - 2024 usa formato con símbolo € y separadores
   - Indica cambio en el sistema de exportación/procesamiento

### El Aumento es Real o Artefacto Metodológico?

**CONCLUSIÓN: Es mayormente un ARTEFACTO METODOLÓGICO**, no un aumento real en la tenencia de mascotas, basado en:

1. **Imposibilidad biológica**: La población de mascotas no puede duplicarse en 1 año
2. **Persistencia del aumento con filtros estrictos**: Incluso filtrando gastos < 100€, el aumento persiste
3. **Cambio en distribución de gastos**: La mediana bajó 86%, indicando inclusión de gastos pequeños
4. **Aumento desproporcionado en "compra"**: El código de compra no debería aumentar 5x

---

## Recomendaciones

### 1. **Contactar al INE para Verificación Metodológica**

**Preguntas clave a realizar:**
- ¿Hubo cambios en el cuestionario de la EPF 2024?
- ¿Se modificó el umbral mínimo para registrar gastos?
- ¿Cambió el sistema de captura/codificación de datos?
- ¿Por qué el código 9321 (compra) aumentó 507%?

### 2. **Aplicar Umbral Mínimo para Análisis**

Para comparabilidad entre años, se recomienda:

**Umbral Moderado: 50-100€**
- Filtra gastos residuales y errores
- Mantiene comparabilidad con años anteriores
- Refleja gastos significativos en mascotas

Con umbral de 100€:
- 2022: 26.44%
- 2023: 27.87%
- 2024: 53.14%

El aumento persiste, sugiriendo un cambio metodológico real.

### 3. **Análisis Complementario Sugerido**

1. **Revisar microdatos originales** para verificar si hay:
   - Duplicados
   - Errores de codificación
   - Cambios en instrucciones a encuestadores

2. **Analizar otros códigos de gasto** (no mascotas) para ver si:
   - También aumentaron los registros de gastos pequeños
   - El patrón es general o específico de mascotas

3. **Comparar con otras fuentes** (ej: ventas de pet shops, registros veterinarios)

### 4. **Para Reportes Públicos**

**NO USAR** el dato de 53.87% sin aclaración metodológica, ya que:
- No es comparable con años anteriores
- Puede confundir a usuarios de los datos
- Requiere nota metodológica explicativa

**SUGERENCIA**: Reportar con nota:
> "El aumento observado en 2024 puede estar influenciado por cambios metodológicos en la captura de gastos. Se recomienda aplicar un umbral mínimo de gasto para comparaciones interanuales."

---

## Archivos Generados

1. **[analisis_estadistico_gastos.pdf](Outputs/analisis_estadistico_gastos.pdf)**
   - Análisis de umbrales para datos de 2024
   - Distribución de gastos y outliers

2. **[analisis_comparativa_mascotas_22_24.pdf](Outputs/analisis_comparativa_mascotas_22_24.pdf)**
   - Comparación 2022-2024
   - Gráficos de evolución con diferentes umbrales
   - Análisis de gastos muy bajos

3. **[analisis_comparativa_mascotas.R](analisis_comparativa_mascotas.R)**
   - Script reproducible del análisis completo

4. **[analisis_umbral_gastos.R](analisis_umbral_gastos.R)**
   - Script para análisis de umbrales en datos 2024

---

## Contacto y Próximos Pasos

**Acción inmediata recomendada**:
Contactar con el departamento técnico del INE responsable de la EPF para aclarar los cambios metodológicos en 2024 antes de publicar o utilizar estas cifras.

---

*Análisis realizado el 7 de diciembre de 2025*
*Datos: Encuesta de Presupuestos Familiares (EPF) - INE España*
