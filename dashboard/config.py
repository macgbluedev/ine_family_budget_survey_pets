"""Constantes estáticas compartidas por app.py y las pestañas de dashboard/tabs/."""

PLOT_TEMPLATE = "plotly_white"

CODIGO_NOMBRES = {
    9321: "Compra de mascotas",
    9322: "Artículos para mascotas",
    9450: "Servicios veterinarios",
}

# Clasificación de hogares (columna CLASIFICACION)
SIN_MASCOTA = "Hogares sin mascotas"
CLAS_PRODUCTOS = "Hogares con mascotas - Productos"
CLAS_SERVICIOS = "Hogares con mascotas - Servicios"
CLAS_AMBOS = "Hogares con mascotas - Productos y Servicios"
CLAS_CON_MASCOTA = [CLAS_PRODUCTOS, CLAS_SERVICIOS, CLAS_AMBOS]
CLAS_COLORS = {
    CLAS_PRODUCTOS: "#4C78A8",
    CLAS_SERVICIOS: "#F58518",
    CLAS_AMBOS: "#54A24B",
    SIN_MASCOTA: "#BAB0AC",
}

# Órdenes naturales de categorías
ORDEN_TAMANO = [
    "Una persona", "Dos personas", "Tres personas",
    "Cuatro personas", "Cinco personas", "Seis o más personas",
]
ORDEN_INTERIN = [
    "Menos de 500 €", "De 500 a menos de 1000 €", "De 1000 a menos de 1500 €",
    "De 1500 a menos de 2000 €", "De 2000 a menos de 2500 €",
    "De 2500 a menos de 3000 €", "De 3000 a menos de 5000 €",
    "De 5000 a menos de 7000 €", "De 7000 a menos de 9000 €",
    "9000 o más €", "Sin registro de ingresos",
]

# Variables de desglose disponibles (etiqueta -> definición)
#   cols: columnas del microdato que forman la categoría (>1 = cruce)
#   order: orden fijo opcional de categorías
VARIABLES = {
    "Tamaño del hogar (TAMANO)": {"cols": ["TAMANO"], "order": ORDEN_TAMANO},
    "Tipo de hogar detallado (TIPHOGAR1)": {"cols": ["TIPHOGAR1"], "order": None},
    "Tipo de hogar (TIPHOGAR7)": {"cols": ["TIPHOGAR7"], "order": None},
    "Ingresos mensuales (INTERIN)": {"cols": ["INTERIN"], "order": ORDEN_INTERIN},
    "Comunidad Autónoma (CCAA)": {"cols": ["CCAA"], "order": None},
    "Capital de provincia (CAPROV)": {"cols": ["CAPROV"], "order": None},
    "Tamaño del municipio (TAMAMU)": {"cols": ["TAMAMU"], "order": None},
    "CCAA × Capital de provincia": {"cols": ["CCAA", "CAPROV"], "order": None},
    "CCAA × Tamaño municipio": {"cols": ["CCAA", "TAMAMU"], "order": None},
}

# Normalización de CCAA + coordenadas para el mapa
CCAA_NORMALIZADAS = {
    "Andalucía": "Andalucía", "Aragón": "Aragón",
    "Asturias, Principado de": "Asturias", "Balears, Illes": "Islas Baleares",
    "Canarias": "Canarias", "Cantabria": "Cantabria",
    "Castilla - La Mancha": "Castilla-La Mancha", "Castilla y León": "Castilla y León",
    "Cataluña": "Cataluña", "Ceuta": "Ceuta",
    "Comunitat Valenciana": "Comunidad Valenciana", "Extremadura": "Extremadura",
    "Galicia": "Galicia", "Madrid, Comunidad de": "Comunidad de Madrid",
    "Melilla": "Melilla", "Murcia, Región de": "Región de Murcia",
    "Navarra, Comunidad Foral de": "Navarra", "País Vasco": "País Vasco",
    "Rioja, La": "La Rioja",
}
CCAA_COORDS = {
    "Andalucía": {"lat": 37.5443, "lon": -4.7278},
    "Aragón": {"lat": 41.5911, "lon": -0.9064},
    "Asturias": {"lat": 43.3614, "lon": -5.8593},
    "Islas Baleares": {"lat": 39.5696, "lon": 2.6502},
    "Canarias": {"lat": 28.2916, "lon": -16.6291},
    "Cantabria": {"lat": 43.1828, "lon": -3.9878},
    "Castilla-La Mancha": {"lat": 39.2797, "lon": -3.0977},
    "Castilla y León": {"lat": 41.8357, "lon": -4.3976},
    "Cataluña": {"lat": 41.5912, "lon": 1.5209},
    "Ceuta": {"lat": 35.8894, "lon": -5.3213},
    "Comunidad Valenciana": {"lat": 39.4840, "lon": -0.7533},
    "Extremadura": {"lat": 39.4937, "lon": -6.0679},
    "Galicia": {"lat": 42.5751, "lon": -8.1339},
    "Comunidad de Madrid": {"lat": 40.4168, "lon": -3.7038},
    "Melilla": {"lat": 35.2923, "lon": -2.9381},
    "Región de Murcia": {"lat": 37.9922, "lon": -1.1307},
    "Navarra": {"lat": 42.6954, "lon": -1.6761},
    "País Vasco": {"lat": 43.0000, "lon": -2.7500},
    "La Rioja": {"lat": 42.2871, "lon": -2.5396},
}
