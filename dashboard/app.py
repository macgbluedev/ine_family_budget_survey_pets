#!/usr/bin/env python3
"""
Dashboard Interactivo - Estado de las Mascotas en España (EPF 2016-2025)
Encuesta de Presupuestos Familiares - INE España

Fuente de datos: dashboard/data/*.csv (generados por dashboard/prep_data.py)
Pestañas: un módulo por pestaña en dashboard/tabs/, cada uno con render(ctx).

Ejecutar con: python3 -m streamlit run dashboard/app.py
"""
import os
import sys

import numpy as np
import pandas as pd
import streamlit as st

# Permite los imports planos (config, data, tabs, ...) sin importar desde
# qué directorio se invoque `streamlit run dashboard/app.py`.
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))

from config import CLAS_AMBOS, CODIGO_NOMBRES
from context import TabContext
from data import GASTOS_CSV, PROP_CSV, load_gastos, load_proporcion
from tabs import comparison, geomap, summary, tables, variable
from utils import categoria

# ============================================================================
# CONFIGURACIÓN
# ============================================================================
st.set_page_config(
    page_title="Mascotas en España - EPF 2016-2025",
    page_icon="🐾",
    layout="wide",
)

if not (os.path.exists(GASTOS_CSV) and os.path.exists(PROP_CSV)):
    st.error(
        "No se encuentran los CSV de datos. Genera los datos ejecutando:\n\n"
        "```\npython3 dashboard/prep_data.py\n```"
    )
    st.stop()

gastos = load_gastos()
prop = load_proporcion()

AÑOS = sorted(prop["ANOENC"].unique())


# ============================================================================
# SIDEBAR - FILTROS
# ============================================================================
st.sidebar.header("⚙️ Filtros")

años_sel = st.sidebar.multiselect(
    "Años a incluir", options=AÑOS, default=AÑOS,
)
if not años_sel:
    años_sel = AÑOS

col_a, col_b = st.sidebar.columns(2)
año_foco = col_a.selectbox(
    "Año foco (KPIs)", options=sorted(años_sel, reverse=True),
    index=0,
)
años_base_opts = sorted([a for a in años_sel if a != año_foco], reverse=True) or [año_foco]
_default_base = 2024 if 2024 in años_base_opts else años_base_opts[0]
año_base = col_b.selectbox(
    "Año base (Δ)", options=años_base_opts,
    index=años_base_opts.index(_default_base),
)

codigos_sel = st.sidebar.multiselect(
    "Códigos de gasto",
    options=list(CODIGO_NOMBRES.keys()),
    default=list(CODIGO_NOMBRES.keys()),
    format_func=lambda c: f"{c} · {CODIGO_NOMBRES[c]}",
)
if not codigos_sel:
    codigos_sel = list(CODIGO_NOMBRES.keys())

ccaa_opts = ["(Todas)"] + sorted(prop["CCAA_NORM"].dropna().unique())
ccaa_filtro = st.sidebar.selectbox("Filtrar por CCAA", options=ccaa_opts, index=0)

st.sidebar.markdown("---")
st.sidebar.caption(
    "Datos: Encuesta de Presupuestos Familiares (INE), 2016-2025. "
    "El gasto es la estimación nacional ponderada en €."
)

# Aplicar filtro transversal de CCAA (afecta a todo salvo el mapa)
if ccaa_filtro != "(Todas)":
    _g = gastos[gastos["CCAA_NORM"] == ccaa_filtro]
    _p = prop[prop["CCAA_NORM"] == ccaa_filtro]
else:
    _g, _p = gastos, prop


# ============================================================================
# HELPERS DE AGREGACIÓN (cerrados sobre los filtros de la sidebar)
# ============================================================================
def resumen_var_filtrado(cols):
    g = _g[_g["ANOENC"].isin(años_sel) & _g["CODIGO"].isin(codigos_sel)].copy()
    p = _p[_p["ANOENC"].isin(años_sel)].copy()
    g["Categoria"] = categoria(g, cols)
    p["Categoria"] = categoria(p, cols)

    gasto = g.groupby(["ANOENC", "Categoria"], as_index=False)["GASTO"].sum()
    gasto = gasto.rename(columns={"GASTO": "gasto_total"})
    tot = p.groupby(["ANOENC", "Categoria"])["NUMERO"].nunique().rename("n_hogares_total")
    con = p[p["CON_MASCOTA"]].groupby(["ANOENC", "Categoria"])["NUMERO"].nunique().rename("n_con_mascota")
    hog = pd.concat([tot, con], axis=1).reset_index()
    hog["n_con_mascota"] = hog["n_con_mascota"].fillna(0).astype(int)
    hog["pct_con_mascota"] = hog["n_con_mascota"] / hog["n_hogares_total"] * 100
    out = hog.merge(gasto, on=["ANOENC", "Categoria"], how="left")
    out["gasto_total"] = out["gasto_total"].fillna(0.0)
    return out


def serie_filtrada():
    g = _g[_g["ANOENC"].isin(años_sel) & _g["CODIGO"].isin(codigos_sel)]
    p = _p[_p["ANOENC"].isin(años_sel)]
    gasto = g.groupby("ANOENC")["GASTO"].sum().rename("gasto_total")
    tot = p.groupby("ANOENC")["NUMERO"].nunique().rename("n_total")
    con = p[p["CON_MASCOTA"]].groupby("ANOENC")["NUMERO"].nunique().rename("n_con")
    s = pd.concat([gasto, tot, con], axis=1).reset_index()
    s["pct_con_mascota"] = s["n_con"] / s["n_total"] * 100
    return s


def composicion_filtrada():
    p = _p[_p["ANOENC"].isin(años_sel)]
    tot = p.groupby("ANOENC")["NUMERO"].nunique().rename("n_total")
    comp = (
        p.groupby(["ANOENC", "CLASIFICACION"])["NUMERO"].nunique().rename("n").reset_index()
        .merge(tot, on="ANOENC")
    )
    comp["pct"] = comp["n"] / comp["n_total"] * 100
    return comp


# ============================================================================
# HEADER + KPIs
# ============================================================================
st.title("🐾 Estado de las Mascotas en España")
sub = "Encuesta de Presupuestos Familiares (INE) · 2016-2025"
if ccaa_filtro != "(Todas)":
    sub += f" · {ccaa_filtro}"
st.caption(sub)

serie = serie_filtrada().set_index("ANOENC")


def _valor(serie, año, col):
    return serie.loc[año, col] if año in serie.index else np.nan


gasto_foco = _valor(serie, año_foco, "gasto_total")
gasto_base = _valor(serie, año_base, "gasto_total")
pct_foco = _valor(serie, año_foco, "pct_con_mascota")
pct_base = _valor(serie, año_base, "pct_con_mascota")

# Reparto del gasto por código en el año foco (% en veterinario)
_gf = _g[(_g["ANOENC"] == año_foco) & (_g["CODIGO"].isin(codigos_sel))]
_gasto_cod = _gf.groupby("CODIGO")["GASTO"].sum()
pct_vet = (_gasto_cod.get(9450, 0.0) / _gasto_cod.sum() * 100) if _gasto_cod.sum() else np.nan

# Composición del año foco
comp_foco = composicion_filtrada()
comp_foco = comp_foco[comp_foco["ANOENC"] == año_foco].set_index("CLASIFICACION")["pct"]
pct_ambos = comp_foco.get(CLAS_AMBOS, 0.0)

k1, k2, k3, k4 = st.columns(4)
k1.metric(
    "Gasto nacional en mascotas",
    f"{gasto_foco/1e6:,.1f} M€" if pd.notna(gasto_foco) else "—",
    delta=(f"{(gasto_foco-gasto_base)/gasto_base*100:+.1f}% vs {año_base}"
           if pd.notna(gasto_base) and gasto_base else None),
)
k2.metric(
    "% hogares con mascotas",
    f"{pct_foco:.1f}%" if pd.notna(pct_foco) else "—",
    delta=(f"{pct_foco-pct_base:+.1f} pp vs {año_base}" if pd.notna(pct_base) else None),
)
k3.metric(
    "Gasto en veterinario",
    f"{pct_vet:.1f}%" if pd.notna(pct_vet) else "—",
    help="Porcentaje del gasto nacional en mascotas destinado a servicios veterinarios (código 9450)",
)
k4.metric(
    "Hogares con productos y servicios",
    f"{pct_ambos:.1f}%",
    help="% de hogares (sobre el total) que declaran a la vez productos y servicios veterinarios",
)
st.caption(f"KPIs referidos al año **{año_foco}**. Δ calculado contra **{año_base}**.")
st.markdown("---")

# ============================================================================
# TABS
# ============================================================================
ctx = TabContext(
    resumen_var_filtrado=resumen_var_filtrado,
    serie_filtrada=serie_filtrada,
    composicion_filtrada=composicion_filtrada,
    año_foco=año_foco,
    año_base=año_base,
)

tab_summary, tab_variable, tab_geomap, tab_comparison, tab_tables = st.tabs(
    ["📊 Resumen", "🔎 Análisis por variable", "🗺️ Mapa de España",
     "📈 Comparativa temporal", "📋 Datos"]
)

with tab_summary:
    summary.render(ctx)

with tab_variable:
    variable.render(ctx)

with tab_geomap:
    geomap.render(ctx)

with tab_comparison:
    comparison.render(ctx)

with tab_tables:
    tables.render(ctx)
