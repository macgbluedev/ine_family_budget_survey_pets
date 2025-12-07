#!/usr/bin/env python3
"""
Dashboard Interactivo - Análisis de Hogares con Mascotas (2022-2024)
Encuesta de Presupuestos Familiares - INE España

Ejecutar con: python dashboard_mascotas.py
Acceder en: http://localhost:8050
"""
import os
from dotenv import load_dotenv
import pandas as pd
import plotly.express as px
import plotly.graph_objects as go
from plotly.subplots import make_subplots
import dash
from dash import dcc, html, Input, Output, callback
import dash_bootstrap_components as dbc
import numpy as np

# ============================================================================
# CONFIGURACIÓN Y CARGA DE DATOS
# ============================================================================
load_dotenv()

# Leer datos
print("Cargando datos...")
datos = pd.read_csv("GastosSoloMascotas22a24.csv", encoding='utf-8')

# Limpiar columna GASTO (formato diferente en 2024)
datos['GASTO'] = datos['GASTO'].astype(str)
datos['GASTO'] = datos['GASTO'].str.replace('€', '', regex=False)
datos['GASTO'] = datos['GASTO'].str.replace(',', '', regex=False)
datos['GASTO'] = datos['GASTO'].str.strip()
datos['GASTO'] = pd.to_numeric(datos['GASTO'], errors='coerce')

# Convertir tipos
datos['ANOENC'] = datos['ANOENC'].astype(int)
datos['NUMERO'] = datos['NUMERO'].astype(int)
datos['CODIGO'] = datos['CODIGO'].astype(int)

# Totales de hogares por año
HOGARES_TOTALES = {
    2022: 20585,
    2023: 20707,
    2024: 19410
}

# Mapeo de códigos
CODIGO_NOMBRES = {
    9321: "Compra de mascotas",
    9322: "Artículos para mascotas",
    9450: "Servicios veterinarios"
}

datos['NOMBRE_CODIGO'] = datos['CODIGO'].map(CODIGO_NOMBRES)

# Mapeo de Comunidades Autónomas (normalizadas para el mapa)
CCAA_NORMALIZADAS = {
    'Andalucía': 'Andalucía',
    'Aragón': 'Aragón',
    'Asturias, Principado de': 'Asturias',
    'Balears, Illes': 'Islas Baleares',
    'Canarias': 'Canarias',
    'Cantabria': 'Cantabria',
    'Castilla - La Mancha': 'Castilla-La Mancha',
    'Castilla y León': 'Castilla y León',
    'Cataluña': 'Cataluña',
    'Ceuta': 'Ceuta',
    'Comunitat Valenciana': 'Comunidad Valenciana',
    'Extremadura': 'Extremadura',
    'Galicia': 'Galicia',
    'Madrid, Comunidad de': 'Comunidad de Madrid',
    'Melilla': 'Melilla',
    'Murcia, Región de': 'Región de Murcia',
    'Navarra, Comunidad Foral de': 'Navarra',
    'País Vasco': 'País Vasco',
    'Rioja, La': 'La Rioja'
}

datos['CCAA_NORM'] = datos['CCAA'].map(CCAA_NORMALIZADAS)

# Coordenadas de las CCAA para el mapa (centro geográfico aproximado)
CCAA_COORDS = {
    'Andalucía': {'lat': 37.5443, 'lon': -4.7278},
    'Aragón': {'lat': 41.5911, 'lon': -0.9064},
    'Asturias': {'lat': 43.3614, 'lon': -5.8593},
    'Islas Baleares': {'lat': 39.5696, 'lon': 2.6502},
    'Canarias': {'lat': 28.2916, 'lon': -16.6291},
    'Cantabria': {'lat': 43.1828, 'lon': -3.9878},
    'Castilla-La Mancha': {'lat': 39.2797, 'lon': -3.0977},
    'Castilla y León': {'lat': 41.8357, 'lon': -4.3976},
    'Cataluña': {'lat': 41.5912, 'lon': 1.5209},
    'Ceuta': {'lat': 35.8894, 'lon': -5.3213},
    'Comunidad Valenciana': {'lat': 39.4840, 'lon': -0.7533},
    'Extremadura': {'lat': 39.4937, 'lon': -6.0679},
    'Galicia': {'lat': 42.5751, 'lon': -8.1339},
    'Comunidad de Madrid': {'lat': 40.4168, 'lon': -3.7038},
    'Melilla': {'lat': 35.2923, 'lon': -2.9381},
    'Región de Murcia': {'lat': 37.9922, 'lon': -1.1307},
    'Navarra': {'lat': 42.6954, 'lon': -1.6761},
    'País Vasco': {'lat': 43.0000, 'lon': -2.7500},
    'La Rioja': {'lat': 42.2871, 'lon': -2.5396}
}

print(f"Datos cargados: {len(datos):,} registros")
print(f"Años: {sorted(datos['ANOENC'].unique())}")
print(f"Códigos: {sorted(datos['CODIGO'].unique())}")

# ============================================================================
# FUNCIONES DE ANÁLISIS
# ============================================================================

def calcular_hogares_con_mascotas(df, umbral_minimo=0):
    """Calcula el número y porcentaje de hogares con mascotas por año"""
    df_filtrado = df[df['GASTO'] >= umbral_minimo].copy()

    resultados = []
    for ano in sorted(df_filtrado['ANOENC'].unique()):
        df_ano = df_filtrado[df_filtrado['ANOENC'] == ano]
        hogares_con_gasto = df_ano['NUMERO'].nunique()
        total_hogares = HOGARES_TOTALES[ano]
        porcentaje = (hogares_con_gasto / total_hogares) * 100

        resultados.append({
            'Año': ano,
            'Hogares_con_Mascota': hogares_con_gasto,
            'Total_Hogares': total_hogares,
            'Porcentaje': porcentaje
        })

    return pd.DataFrame(resultados)

def analizar_por_umbral(df, umbrales):
    """Analiza cómo cambia el % de hogares según diferentes umbrales"""
    resultados = []
    for umbral in umbrales:
        res = calcular_hogares_con_mascotas(df, umbral)
        res['Umbral'] = umbral
        resultados.append(res)

    return pd.concat(resultados, ignore_index=True)

# ============================================================================
# INICIALIZAR DASH APP
# ============================================================================

app = dash.Dash(
    __name__,
    external_stylesheets=[dbc.themes.BOOTSTRAP, dbc.icons.FONT_AWESOME],
    suppress_callback_exceptions=True
)

app.title = "Dashboard Mascotas EPF 2022-2024"

# Configuración de tema claro para gráficos
PLOT_TEMPLATE = 'plotly_white'
PLOT_BGCOLOR = '#fff'
PAPER_BGCOLOR = '#fff'
FONT_COLOR = '#333'

# ============================================================================
# LAYOUT DEL DASHBOARD
# ============================================================================

# Componentes de control
controles = dbc.Card([
    dbc.CardHeader(html.H5("⚙️ Controles", className="mb-0")),
    dbc.CardBody([
        # Selector de umbral
        html.Label("Umbral Mínimo de Gasto (€):", className="fw-bold"),
        dcc.Slider(
            id='umbral-slider',
            min=0,
            max=500,
            step=1,
            value=0,
            marks={
                0: '0€',
                1: '1€',
                5: '5€',
                10: '10€',
                50: '50€',
                100: '100€',
                500: '500€'
            },
            tooltip={"placement": "bottom", "always_visible": True}
        ),
        html.Div(id='umbral-value', className="text-center mt-2 text-muted"),

        html.Hr(),

        # Selector de años
        html.Label("Años a Comparar:", className="fw-bold mt-3"),
        dcc.Checklist(
            id='year-selector',
            options=[
                {'label': ' 2022', 'value': 2022},
                {'label': ' 2023', 'value': 2023},
                {'label': ' 2024', 'value': 2024}
            ],
            value=[2022, 2023, 2024],
            inline=True,
            className="mb-3"
        ),

        html.Hr(),

        # Selector de códigos
        html.Label("Códigos de Gasto:", className="fw-bold mt-3"),
        dcc.Checklist(
            id='codigo-selector',
            options=[
                {'label': ' 9321 - Compra', 'value': 9321},
                {'label': ' 9322 - Artículos', 'value': 9322},
                {'label': ' 9450 - Servicios', 'value': 9450}
            ],
            value=[9321, 9322, 9450],
            className="mb-3"
        ),

        html.Hr(),

        # Información
        dbc.Alert([
            html.I(className="fas fa-info-circle me-2"),
            "Ajusta los controles para explorar cómo cambian las métricas"
        ], color="info", className="mt-3")
    ])
], className="mb-4")

# Tarjetas de KPI
def crear_tarjeta_kpi(id_valor, titulo, icono):
    return dbc.Card([
        dbc.CardBody([
            html.Div([
                html.I(className=f"{icono} fa-2x text-primary mb-2"),
                html.H3(id=id_valor, className="mb-0"),
                html.P(titulo, className="text-muted mb-0 small")
            ], className="text-center")
        ])
    ])

# Layout principal
app.layout = dbc.Container([
    # Header
    dbc.Row([
        dbc.Col([
            html.H1([
                html.I(className="fas fa-paw me-3"),
                "Dashboard Análisis Mascotas EPF 2022-2024"
            ], className="text-primary mb-1"),
            html.P("Encuesta de Presupuestos Familiares - INE España",
                   className="text-muted mb-4")
        ])
    ], className="mb-4 mt-3"),

    # KPIs principales
    dbc.Row([
        dbc.Col(crear_tarjeta_kpi('kpi-registros', 'Total Registros', 'fas fa-database'), width=3),
        dbc.Col(crear_tarjeta_kpi('kpi-hogares', 'Hogares con Mascotas', 'fas fa-home'), width=3),
        dbc.Col(crear_tarjeta_kpi('kpi-porcentaje', '% Promedio', 'fas fa-percent'), width=3),
        dbc.Col(crear_tarjeta_kpi('kpi-gasto-mediano', 'Gasto Mediano', 'fas fa-euro-sign'), width=3),
    ], className="mb-4"),

    dbc.Row([
        # Columna de controles
        dbc.Col(controles, width=3),

        # Columna de visualizaciones
        dbc.Col([
            # Tab de visualizaciones
            dbc.Tabs([
                dbc.Tab(label="📊 Evolución", tab_id="tab-evolucion"),
                dbc.Tab(label="📈 Distribuciones", tab_id="tab-distribuciones"),
                dbc.Tab(label="🗺️ Mapa de España", tab_id="tab-mapa"),
                dbc.Tab(label="👨‍👩‍👧‍👦 Tamaño de Hogar", tab_id="tab-tamano"),
                dbc.Tab(label="🎯 Análisis de Umbrales", tab_id="tab-umbrales"),
                dbc.Tab(label="📋 Datos", tab_id="tab-datos"),
            ], id="tabs", active_tab="tab-evolucion", className="mb-3"),

            # Contenido de tabs
            html.Div(id="tab-content")
        ], width=9)
    ]),

    # Footer
    html.Hr(className="mt-5"),
    html.Footer([
        html.P([
            "Dashboard creado para análisis de datos EPF 2022-2024 | ",
            html.A("Informe Completo", href="#", className="text-decoration-none")
        ], className="text-center text-muted small")
    ], className="mb-3")

], fluid=True, className="px-4")

# ============================================================================
# CALLBACKS
# ============================================================================

@callback(
    Output('umbral-value', 'children'),
    Input('umbral-slider', 'value')
)
def actualizar_umbral_display(value):
    return f"Umbral seleccionado: {value}€"

@callback(
    [Output('kpi-registros', 'children'),
     Output('kpi-hogares', 'children'),
     Output('kpi-porcentaje', 'children'),
     Output('kpi-gasto-mediano', 'children')],
    [Input('umbral-slider', 'value'),
     Input('year-selector', 'value'),
     Input('codigo-selector', 'value')]
)
def actualizar_kpis(umbral, years, codigos):
    # Filtrar datos
    df_filtrado = datos[
        (datos['ANOENC'].isin(years)) &
        (datos['CODIGO'].isin(codigos)) &
        (datos['GASTO'] >= umbral)
    ].copy()

    # Calcular métricas
    total_registros = len(df_filtrado)
    hogares_unicos = df_filtrado['NUMERO'].nunique()

    # Porcentaje promedio
    res = calcular_hogares_con_mascotas(df_filtrado, umbral)
    if len(res) > 0:
        porcentaje_promedio = res['Porcentaje'].mean()
        gasto_mediano = df_filtrado['GASTO'].median()
    else:
        porcentaje_promedio = 0
        gasto_mediano = 0

    return (
        f"{total_registros:,}",
        f"{hogares_unicos:,}",
        f"{porcentaje_promedio:.1f}%",
        f"{gasto_mediano:,.0f}€"
    )

@callback(
    Output('tab-content', 'children'),
    [Input('tabs', 'active_tab'),
     Input('umbral-slider', 'value'),
     Input('year-selector', 'value'),
     Input('codigo-selector', 'value')]
)
def render_tab_content(active_tab, umbral, years, codigos):
    # Filtrar datos
    df_filtrado = datos[
        (datos['ANOENC'].isin(years)) &
        (datos['CODIGO'].isin(codigos)) &
        (datos['GASTO'] >= umbral)
    ].copy()

    if active_tab == "tab-evolucion":
        return crear_tab_evolucion(df_filtrado, umbral)
    elif active_tab == "tab-distribuciones":
        return crear_tab_distribuciones(df_filtrado)
    elif active_tab == "tab-mapa":
        return crear_tab_mapa(df_filtrado, umbral, years)
    elif active_tab == "tab-tamano":
        return crear_tab_tamano(df_filtrado)
    elif active_tab == "tab-umbrales":
        return crear_tab_umbrales(df_filtrado)
    elif active_tab == "tab-datos":
        return crear_tab_datos(df_filtrado)

    return html.Div("Selecciona una pestaña")

# ============================================================================
# FUNCIONES PARA CREAR TABS
# ============================================================================

def crear_tab_evolucion(df, umbral):
    """Tab de evolución temporal"""

    # Gráfico 1: Evolución de hogares con mascotas
    res_hogares = calcular_hogares_con_mascotas(df, umbral)

    fig1 = go.Figure()
    fig1.add_trace(go.Bar(
        x=res_hogares['Año'],
        y=res_hogares['Hogares_con_Mascota'],
        name='Hogares con Mascotas',
        marker_color='steelblue',
        text=res_hogares['Hogares_con_Mascota'],
        textposition='auto',
        texttemplate='%{text:,}'
    ))

    fig1.update_layout(
        title=f"Hogares con Mascotas por Año (Umbral: {umbral}€)",
        xaxis_title="Año",
        yaxis_title="Número de Hogares",
        template=PLOT_TEMPLATE,
        height=400
    )

    # Gráfico 2: Porcentaje de hogares
    fig2 = go.Figure()
    fig2.add_trace(go.Scatter(
        x=res_hogares['Año'],
        y=res_hogares['Porcentaje'],
        mode='lines+markers+text',
        name='% Hogares',
        line=dict(color='crimson', width=3),
        marker=dict(size=12),
        text=[f"{p:.1f}%" for p in res_hogares['Porcentaje']],
        textposition='top center'
    ))

    fig2.update_layout(
        title="Evolución del % de Hogares con Mascotas",
        xaxis_title="Año",
        yaxis_title="Porcentaje (%)",
        template=PLOT_TEMPLATE,
        height=400
    )

    # Gráfico 3: Registros por código y año
    registros_codigo = df.groupby(['ANOENC', 'NOMBRE_CODIGO']).size().reset_index(name='count')

    fig3 = px.bar(
        registros_codigo,
        x='ANOENC',
        y='count',
        color='NOMBRE_CODIGO',
        barmode='group',
        title='Registros de Gasto por Tipo y Año',
        labels={'ANOENC': 'Año', 'count': 'Número de Registros', 'NOMBRE_CODIGO': 'Tipo de Gasto'},
        template=PLOT_TEMPLATE,
        height=400
    )

    return dbc.Container([
        dbc.Row([
            dbc.Col(dcc.Graph(figure=fig1), width=6),
            dbc.Col(dcc.Graph(figure=fig2), width=6),
        ]),
        dbc.Row([
            dbc.Col(dcc.Graph(figure=fig3), width=12),
        ])
    ], fluid=True)

def crear_tab_distribuciones(df):
    """Tab de distribuciones de gastos"""

    # Gráfico 1: Histograma por año
    fig1 = px.histogram(
        df,
        x='GASTO',
        color='ANOENC',
        nbins=50,
        title='Distribución de Gastos (escala logarítmica)',
        labels={'GASTO': 'Gasto (€)', 'ANOENC': 'Año'},
        template=PLOT_TEMPLATE,
        barmode='overlay',
        opacity=0.7,
        height=400,
        log_x=True
    )

    # Gráfico 2: Boxplot por año y código
    fig2 = px.box(
        df,
        x='ANOENC',
        y='GASTO',
        color='NOMBRE_CODIGO',
        title='Distribución de Gastos por Tipo (escala log)',
        labels={'ANOENC': 'Año', 'GASTO': 'Gasto (€)', 'NOMBRE_CODIGO': 'Tipo'},
        template=PLOT_TEMPLATE,
        log_y=True,
        height=400
    )

    # Gráfico 3: Percentiles por año
    percentiles_data = []
    for ano in sorted(df['ANOENC'].unique()):
        df_ano = df[df['ANOENC'] == ano]
        percentiles = df_ano['GASTO'].quantile([0.05, 0.25, 0.5, 0.75, 0.95]).values
        percentiles_data.append({
            'Año': ano,
            'P5': percentiles[0],
            'P25': percentiles[1],
            'P50': percentiles[2],
            'P75': percentiles[3],
            'P95': percentiles[4]
        })

    df_percentiles = pd.DataFrame(percentiles_data)

    fig3 = go.Figure()
    for col in ['P5', 'P25', 'P50', 'P75', 'P95']:
        fig3.add_trace(go.Scatter(
            x=df_percentiles['Año'],
            y=df_percentiles[col],
            mode='lines+markers',
            name=col,
            line=dict(width=2)
        ))

    fig3.update_layout(
        title='Percentiles de Gastos por Año',
        xaxis_title='Año',
        yaxis_title='Gasto (€)',
        yaxis_type='log',
        template=PLOT_TEMPLATE,
        height=400
    )

    return dbc.Container([
        dbc.Row([
            dbc.Col(dcc.Graph(figure=fig1), width=12),
        ]),
        dbc.Row([
            dbc.Col(dcc.Graph(figure=fig2), width=6),
            dbc.Col(dcc.Graph(figure=fig3), width=6),
        ])
    ], fluid=True)

def crear_tab_mapa(df, umbral, years):
    """Tab con mapa interactivo de España"""

    # Calcular estadísticas por CCAA
    stats_ccaa = []

    for ano in sorted(years):
        df_ano = df[df['ANOENC'] == ano].copy()

        # Agrupar por CCAA
        ccaa_data = df_ano.groupby('CCAA_NORM').agg({
            'NUMERO': 'nunique',  # Hogares únicos
            'GASTO': ['count', 'median', 'mean']
        }).reset_index()

        ccaa_data.columns = ['CCAA', 'Hogares_con_Mascota', 'Num_Registros', 'Gasto_Mediano', 'Gasto_Medio']
        ccaa_data['Año'] = ano

        # Añadir coordenadas
        ccaa_data['lat'] = ccaa_data['CCAA'].map(lambda x: CCAA_COORDS.get(x, {}).get('lat', 0))
        ccaa_data['lon'] = ccaa_data['CCAA'].map(lambda x: CCAA_COORDS.get(x, {}).get('lon', 0))

        stats_ccaa.append(ccaa_data)

    df_mapa = pd.concat(stats_ccaa, ignore_index=True)

    # Crear selector de año para el mapa
    if len(years) > 1:
        # Si hay múltiples años, mostrar selector
        controles_mapa = dbc.Row([
            dbc.Col([
                html.Label("Seleccionar Año para el Mapa:", className="fw-bold"),
                dcc.RadioItems(
                    id='mapa-year-selector',
                    options=[{'label': f' {y}', 'value': y} for y in sorted(years)],
                    value=max(years),
                    inline=True,
                    className="mb-3"
                )
            ], width=12)
        ])
        año_mapa = max(years)
    else:
        controles_mapa = html.Div()
        año_mapa = years[0] if years else 2024

    # Filtrar para el año seleccionado
    df_mapa_año = df_mapa[df_mapa['Año'] == año_mapa].copy()

    # Mapa 1: Mapa de burbujas (scatter_geo)
    fig1 = px.scatter_geo(
        df_mapa_año,
        lat='lat',
        lon='lon',
        size='Hogares_con_Mascota',
        color='Gasto_Mediano',
        hover_name='CCAA',
        hover_data={
            'Hogares_con_Mascota': ':,',
            'Num_Registros': ':,',
            'Gasto_Mediano': ':.0f€',
            'Gasto_Medio': ':.0f€',
            'lat': False,
            'lon': False
        },
        title=f'Hogares con Mascotas por Comunidad Autónoma - {año_mapa}',
        color_continuous_scale='Viridis',
        size_max=50,
        projection='natural earth'
    )

    # Centrar en España
    fig1.update_geos(
        center=dict(lat=40.4168, lon=-3.7038),
        projection_scale=6,
        visible=True,
        resolution=50,
        showcountries=True,
        countrycolor="lightgray",
        showcoastlines=True,
        coastlinecolor="gray",
        showland=True,
        landcolor="white",
        showocean=True,
        oceancolor="lightblue"
    )

    fig1.update_layout(
        height=600,
        margin=dict(l=0, r=0, t=50, b=0),
        template=PLOT_TEMPLATE,
        coloraxis_colorbar=dict(
            title="Gasto<br>Mediano (€)"
        )
    )

    # Gráfico 2: Ranking de CCAAs por % de penetración
    # Necesitamos calcular el % respecto al total de hogares de cada CCAA
    # Como no tenemos ese dato, usamos hogares absolutos

    top_ccaa = df_mapa_año.nlargest(10, 'Hogares_con_Mascota')

    fig2 = px.bar(
        top_ccaa.sort_values('Hogares_con_Mascota'),
        y='CCAA',
        x='Hogares_con_Mascota',
        orientation='h',
        title=f'Top 10 CCAAs por Hogares con Mascotas - {año_mapa}',
        labels={'Hogares_con_Mascota': 'Hogares con Mascotas', 'CCAA': 'Comunidad Autónoma'},
        template=PLOT_TEMPLATE,
        color='Hogares_con_Mascota',
        color_continuous_scale='Blues'
    )

    fig2.update_layout(
        height=500,
        showlegend=False,
        xaxis_title="Número de Hogares"
    )

    # Gráfico 3: Comparación temporal (si hay múltiples años)
    if len(years) > 1:
        fig3 = px.bar(
            df_mapa,
            x='CCAA',
            y='Hogares_con_Mascota',
            color='Año',
            barmode='group',
            title='Evolución de Hogares con Mascotas por CCAA',
            labels={'Hogares_con_Mascota': 'Hogares', 'CCAA': 'Comunidad Autónoma'},
            template=PLOT_TEMPLATE,
            height=500
        )

        fig3.update_xaxis(tickangle=45)

        grafico3 = dbc.Row([
            dbc.Col(dcc.Graph(figure=fig3), width=12),
        ])
    else:
        grafico3 = html.Div()

    # Tabla de estadísticas
    tabla_stats = df_mapa_año[['CCAA', 'Hogares_con_Mascota', 'Num_Registros', 'Gasto_Mediano', 'Gasto_Medio']].copy()
    tabla_stats = tabla_stats.sort_values('Hogares_con_Mascota', ascending=False)
    tabla_stats['Gasto_Mediano'] = tabla_stats['Gasto_Mediano'].round(0)
    tabla_stats['Gasto_Medio'] = tabla_stats['Gasto_Medio'].round(0)
    tabla_stats.columns = ['Comunidad Autónoma', 'Hogares', 'Registros', 'Gasto Mediano (€)', 'Gasto Medio (€)']

    return dbc.Container([
        controles_mapa,

        dbc.Alert([
            html.I(className="fas fa-info-circle me-2"),
            f"Mapa con umbral mínimo de {umbral}€. ",
            f"Mostrando datos de {', '.join(map(str, sorted(years)))}. ",
            "El tamaño de las burbujas representa el número de hogares con mascotas."
        ], color="info", className="mb-3"),

        dbc.Row([
            dbc.Col(dcc.Graph(id='mapa-espana', figure=fig1), width=12),
        ]),

        dbc.Row([
            dbc.Col(dcc.Graph(figure=fig2), width=12),
        ], className="mt-3"),

        grafico3,

        dbc.Row([
            dbc.Col([
                html.H5(f"📊 Estadísticas por Comunidad Autónoma - {año_mapa}", className="mt-4 mb-3"),
                dbc.Table.from_dataframe(
                    tabla_stats,
                    striped=True,
                    bordered=True,
                    hover=True,
                    size='sm'
                )
            ], width=12)
        ], className="mt-3")
    ], fluid=True)

def crear_tab_tamano(df):
    """Tab de análisis por tamaño de hogar"""

    # Filtrar valores nulos o inválidos en TAMANO
    df_clean = df[df['TAMANO'].notna()].copy()

    if len(df_clean) == 0:
        return dbc.Alert("No hay datos disponibles para mostrar", color="warning")

    # Crear un mapeo de orden para ordenar correctamente los tamaños
    orden_tamano = {
        'Una persona': 1,
        'Dos personas': 2,
        'Tres personas': 3,
        'Cuatro personas': 4,
        'Cinco personas': 5,
        'Seis o más personas': 6
    }

    df_clean['orden_tamano'] = df_clean['TAMANO'].map(orden_tamano)

    # Gráfico 1: Distribución de hogares con mascotas por tamaño y año
    stats_tamano = df_clean.groupby(['ANOENC', 'TAMANO', 'orden_tamano']).agg({
        'NUMERO': 'nunique',  # Hogares únicos
        'GASTO': ['count', 'median', 'mean', 'sum']
    }).reset_index()

    stats_tamano.columns = ['Año', 'Tamaño_Hogar', 'orden_tamano', 'Hogares', 'Num_Registros', 'Gasto_Mediano', 'Gasto_Medio', 'Gasto_Total']
    stats_tamano = stats_tamano.sort_values(['Año', 'orden_tamano'])

    # Orden correcto para las categorías
    orden_categorias = ['Una persona', 'Dos personas', 'Tres personas', 'Cuatro personas', 'Cinco personas', 'Seis o más personas']

    fig1 = px.bar(
        stats_tamano,
        x='Tamaño_Hogar',
        y='Hogares',
        color='Año',
        barmode='group',
        title='Hogares con Mascotas por Tamaño de Hogar y Año',
        labels={'Tamaño_Hogar': 'Tamaño del Hogar', 'Hogares': 'Número de Hogares'},
        template=PLOT_TEMPLATE,
        category_orders={'Tamaño_Hogar': orden_categorias},
        height=500
    )

    # Gráfico 2: Gasto mediano por tamaño de hogar
    fig2 = px.line(
        stats_tamano,
        x='Tamaño_Hogar',
        y='Gasto_Mediano',
        color='Año',
        markers=True,
        title='Gasto Mediano en Mascotas por Tamaño de Hogar',
        labels={'Tamaño_Hogar': 'Tamaño del Hogar', 'Gasto_Mediano': 'Gasto Mediano (€)'},
        template=PLOT_TEMPLATE,
        category_orders={'Tamaño_Hogar': orden_categorias},
        height=500
    )

    fig2.update_traces(mode='lines+markers', marker=dict(size=10))

    # Gráfico 3: Gasto medio por tamaño de hogar
    fig3 = px.line(
        stats_tamano,
        x='Tamaño_Hogar',
        y='Gasto_Medio',
        color='Año',
        markers=True,
        title='Gasto Medio en Mascotas por Tamaño de Hogar',
        labels={'Tamaño_Hogar': 'Tamaño del Hogar', 'Gasto_Medio': 'Gasto Medio (€)'},
        template=PLOT_TEMPLATE,
        category_orders={'Tamaño_Hogar': orden_categorias},
        height=500
    )

    fig3.update_traces(mode='lines+markers', marker=dict(size=10))

    # Gráfico 4: Distribución porcentual por tamaño
    total_por_año = stats_tamano.groupby('Año')['Hogares'].sum().reset_index()
    total_por_año.columns = ['Año', 'Total_Hogares']

    stats_tamano_pct = stats_tamano.merge(total_por_año, on='Año')
    stats_tamano_pct['Porcentaje'] = (stats_tamano_pct['Hogares'] / stats_tamano_pct['Total_Hogares']) * 100

    fig4 = px.bar(
        stats_tamano_pct,
        x='Año',
        y='Porcentaje',
        color='Tamaño_Hogar',
        title='Distribución Porcentual de Hogares con Mascotas por Tamaño',
        labels={'Porcentaje': '% del Total', 'Tamaño_Hogar': 'Tamaño del Hogar'},
        template=PLOT_TEMPLATE,
        height=500
    )

    # Tabla resumen
    tabla_stats = stats_tamano.copy()
    tabla_stats['Gasto_Mediano'] = tabla_stats['Gasto_Mediano'].round(2)
    tabla_stats['Gasto_Medio'] = tabla_stats['Gasto_Medio'].round(2)
    tabla_stats['Gasto_Total'] = tabla_stats['Gasto_Total'].round(2)
    tabla_stats = tabla_stats.sort_values(['Año', 'Tamaño_Hogar'])

    return dbc.Container([
        dbc.Alert([
            html.I(className="fas fa-info-circle me-2"),
            "Análisis de gastos en mascotas según el tamaño del hogar (número de personas)"
        ], color="info", className="mb-3"),

        dbc.Row([
            dbc.Col(dcc.Graph(figure=fig1), width=12),
        ]),

        dbc.Row([
            dbc.Col(dcc.Graph(figure=fig2), width=6),
            dbc.Col(dcc.Graph(figure=fig3), width=6),
        ], className="mt-3"),

        dbc.Row([
            dbc.Col(dcc.Graph(figure=fig4), width=12),
        ], className="mt-3"),

        dbc.Row([
            dbc.Col([
                html.H5("📊 Tabla de Estadísticas por Tamaño de Hogar", className="mt-4 mb-3"),
                dbc.Table.from_dataframe(
                    tabla_stats,
                    striped=True,
                    bordered=True,
                    hover=True,
                    size='sm'
                )
            ], width=12)
        ], className="mt-3")
    ], fluid=True)

def crear_tab_umbrales(df):
    """Tab de análisis de sensibilidad de umbrales"""

    umbrales = [0, 0.5, 1, 2, 5, 10, 25, 50, 100, 200, 500]
    res_umbrales = analizar_por_umbral(df, umbrales)

    # Gráfico 1: Evolución del % según umbral
    fig1 = px.line(
        res_umbrales,
        x='Umbral',
        y='Porcentaje',
        color='Año',
        markers=True,
        title='% de Hogares con Mascotas según Umbral Mínimo',
        labels={'Umbral': 'Umbral Mínimo (€)', 'Porcentaje': '% Hogares'},
        template=PLOT_TEMPLATE,
        log_x=True,
        height=500
    )

    fig1.update_layout(
        xaxis=dict(
            tickmode='array',
            tickvals=umbrales,
            ticktext=[f"{u}€" for u in umbrales]
        )
    )

    # Gráfico 2: Número absoluto de hogares según umbral
    fig2 = px.line(
        res_umbrales,
        x='Umbral',
        y='Hogares_con_Mascota',
        color='Año',
        markers=True,
        title='Hogares con Mascotas (absoluto) según Umbral',
        labels={'Umbral': 'Umbral Mínimo (€)', 'Hogares_con_Mascota': 'Nº Hogares'},
        template=PLOT_TEMPLATE,
        log_x=True,
        height=500
    )

    fig2.update_layout(
        xaxis=dict(
            tickmode='array',
            tickvals=umbrales,
            ticktext=[f"{u}€" for u in umbrales]
        )
    )

    # Tabla resumen
    tabla_umbral = res_umbrales.pivot(index='Umbral', columns='Año', values='Porcentaje').reset_index()

    return dbc.Container([
        dbc.Row([
            dbc.Col(dcc.Graph(figure=fig1), width=12),
        ]),
        dbc.Row([
            dbc.Col(dcc.Graph(figure=fig2), width=12),
        ]),
        dbc.Row([
            dbc.Col([
                html.H5("Tabla de Resultados por Umbral", className="mt-4 mb-3"),
                dbc.Table.from_dataframe(
                    tabla_umbral.round(2),
                    striped=True,
                    bordered=True,
                    hover=True,
                    size='sm'
                )
            ], width=12)
        ])
    ], fluid=True)

def crear_tab_datos(df):
    """Tab con tabla de datos"""

    # Estadísticas por año
    stats_por_ano = df.groupby('ANOENC').agg({
        'GASTO': ['count', 'min', 'median', 'mean', 'max'],
        'NUMERO': 'nunique'
    }).round(2)

    stats_por_ano.columns = ['Registros', 'Mín', 'Mediana', 'Media', 'Máx', 'Hogares Únicos']
    stats_por_ano = stats_por_ano.reset_index()
    stats_por_ano.columns = ['Año', 'Registros', 'Mín (€)', 'Mediana (€)', 'Media (€)', 'Máx (€)', 'Hogares']

    # Muestra de datos
    muestra = df[['ANOENC', 'NUMERO', 'CODIGO', 'NOMBRE_CODIGO', 'GASTO', 'CCAA']].head(100).copy()
    muestra['GASTO'] = muestra['GASTO'].round(2)

    return dbc.Container([
        dbc.Row([
            dbc.Col([
                html.H5("📊 Estadísticas Descriptivas por Año", className="mb-3"),
                dbc.Table.from_dataframe(
                    stats_por_ano,
                    striped=True,
                    bordered=True,
                    hover=True
                )
            ], width=12)
        ]),
        html.Hr(className="my-4"),
        dbc.Row([
            dbc.Col([
                html.H5("📋 Muestra de Datos (primeros 100 registros)", className="mb-3"),
                dbc.Alert([
                    html.I(className="fas fa-download me-2"),
                    "Total de registros filtrados: ",
                    html.Strong(f"{len(df):,}")
                ], color="secondary", className="mb-3"),
                dbc.Table.from_dataframe(
                    muestra,
                    striped=True,
                    bordered=True,
                    hover=True,
                    size='sm',
                    style={'fontSize': '0.85rem'}
                )
            ], width=12)
        ])
    ], fluid=True)

# ============================================================================
# RUN SERVER
# ============================================================================

if __name__ == '__main__':
    print("\n" + "="*60)
    print("🚀 Dashboard Mascotas EPF 2022-2024")
    print("="*60)
    print("\n📊 Dashboard iniciado correctamente")
    print(f"🌐 Accede en: http://localhost:8050")
    print("\n💡 Usa Ctrl+C para detener el servidor\n")
    print("="*60 + "\n")

    port = int(os.getenv("PORT", 8050))
    host = os.getenv("HOST", "127.0.0.1")
    debug_mode = os.getenv("DEBUG_MODE", False)
    app.run(host=host, port=port, debug=debug_mode, )