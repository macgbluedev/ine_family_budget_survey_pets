El objetivo es analizar porque la cantidad de hogares con mascotas en 2024 se han duplicado con respecto a los 2 años anteriores (2023,2022), teniendo en cuenta que el gasto de mascotas ha inscrementado (7% con respecto al 2023)

Descripcion del problema:
Es una consulta acerca de la Encuesta de presupuestos familiares. Base 2006 - Año 2024. Respecto a la cantidad de hogares que tienen algún gasto de los códigos de mascotas del 2024 (09.3.2.1, 09.3.2.2, 09.4.5.0) con los códigos de mascotas del 2023 (09.3.4.1, 09.3.4.2, 09.3.5.0).

He notado que la cantidad de hogares con mascotas se han duplicado.

Por ejemplo: 14,94% de los hogares totales de la encuesta tienen algún gasto del tipo 09.3.4.2 en 2023 vs 36,23% de los hogares totales de la encuesta tienen algún gastos del tipo 09.3.2.2 en 2024 (productos de mascostas). De forma similar pasa con los servicios (09.3.5.0 vs 09.4.5.0)

Equivalencia de los codigos entre 2024 vs 2023-2022

| Nombre                                                     | Código COICOP/Recogida 2018 | Código ECOICOP |
|-------------------------------------------------------------|-----------------------------|----------------|
| COMPRA DE MASCOTAS                                          | 09.3.2.1                   | 09.3.4.1       |
| ARTÍCULOS RELATIVOS A MASCOTAS                              | 09.3.2.2                   | 09.3.4.2       |
| SERVICIOS VETERINARIOS Y OTRO TIPO DE SERVICIOS PARA MASCOTAS | 09.4.5.0                   | 09.3.5.0       |

La muestra total de horgares por año:
| Año | Hogares totales |
|-----|-----------------|
| 2024| 19410          |
| 2023| 20707          |
| 2022| 20585          |

Nota: cada hogar viene identificado por un ID entero del 1...MuestraTotal

Hipotesis
- Para seleccionar que hogar tiene mascota es porque tiene algun gasto relacionado (ver fichero .R src/ExtractResultFromMicrodataEpf.R)