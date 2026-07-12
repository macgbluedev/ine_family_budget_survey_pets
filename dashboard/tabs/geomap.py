"""Tab: Mapa de España (Map of Spain) — geographic breakdown by CCAA."""
import plotly.express as px
import streamlit as st

from config import CCAA_COORDS, CCAA_NORMALIZADAS, PLOT_TEMPLATE
from context import TabContext


def render(ctx: TabContext):
    c1, c2 = st.columns([2, 2])
    metrica_mapa = c1.radio(
        "Métrica del mapa",
        ["% hogares con mascotas", "Gasto total (€)"],
        horizontal=True,
    )
    animar = c2.checkbox("Animar por año ▶️", value=False)

    df_ccaa = ctx.resumen_var_filtrado(["CCAA"])
    # Reasignar la categoría (CCAA cruda) a su versión normalizada + coords
    df_ccaa["CCAA_NORM"] = df_ccaa["Categoria"].map(CCAA_NORMALIZADAS)
    df_ccaa = df_ccaa.dropna(subset=["CCAA_NORM"])
    df_ccaa["lat"] = df_ccaa["CCAA_NORM"].map(lambda x: CCAA_COORDS.get(x, {}).get("lat"))
    df_ccaa["lon"] = df_ccaa["CCAA_NORM"].map(lambda x: CCAA_COORDS.get(x, {}).get("lon"))

    col_map = {
        "% hogares con mascotas": ("pct_con_mascota", "% con mascotas"),
        "Gasto total (€)": ("gasto_total", "Gasto (€)"),
    }[metrica_mapa]
    valcol, vallabel = col_map

    if animar:
        df_map = df_ccaa.sort_values("ANOENC")
        anim = {"animation_frame": "ANOENC"}
        titulo = f"{metrica_mapa} por CCAA (animado)"
    else:
        df_map = df_ccaa[df_ccaa["ANOENC"] == ctx.año_foco]
        anim = {}
        titulo = f"{metrica_mapa} por CCAA · {ctx.año_foco}"

    fig = px.scatter_geo(
        df_map, lat="lat", lon="lon",
        size=df_map[valcol].clip(lower=0),
        color=valcol, hover_name="CCAA_NORM",
        hover_data={valcol: ":,.1f", "n_con_mascota": ":,", "lat": False, "lon": False},
        color_continuous_scale="Viridis", size_max=45, projection="natural earth",
        title=titulo, labels={valcol: vallabel}, **anim,
    )
    fig.update_geos(
        center=dict(lat=40.4168, lon=-3.7038), projection_scale=6, visible=True,
        resolution=50, showcountries=True, countrycolor="lightgray",
        showcoastlines=True, coastlinecolor="gray", showland=True, landcolor="white",
        showocean=True, oceancolor="#eaf3fb",
    )
    fig.update_layout(height=620, margin=dict(l=0, r=0, t=50, b=0), template=PLOT_TEMPLATE)
    st.plotly_chart(fig, use_container_width=True)

    # Ranking del año foco
    rank = df_ccaa[df_ccaa["ANOENC"] == ctx.año_foco].copy()
    rank = rank.sort_values(valcol, ascending=True)
    fig_r = px.bar(
        rank, x=valcol, y="CCAA_NORM", orientation="h",
        color=valcol, color_continuous_scale="Blues",
        title=f"Ranking de CCAA · {metrica_mapa} · {ctx.año_foco}",
        labels={valcol: vallabel, "CCAA_NORM": "Comunidad Autónoma"},
        template=PLOT_TEMPLATE,
    )
    fig_r.update_layout(height=560, showlegend=False)
    st.plotly_chart(fig_r, use_container_width=True)
