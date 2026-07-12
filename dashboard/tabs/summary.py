"""Tab: Resumen (Summary) — spending trend and household composition over time."""
import plotly.express as px
import plotly.graph_objects as go
import streamlit as st

from config import CLAS_AMBOS, CLAS_COLORS, CLAS_PRODUCTOS, CLAS_SERVICIOS, PLOT_TEMPLATE, SIN_MASCOTA
from context import TabContext


def render(ctx: TabContext):
    s = ctx.serie_filtrada().sort_values("ANOENC")

    fig = go.Figure()
    fig.add_trace(go.Bar(
        x=s["ANOENC"], y=s["gasto_total"] / 1e6, name="Gasto nacional (M€)",
        marker_color="#4C78A8", yaxis="y1", opacity=0.65,
    ))
    fig.add_trace(go.Scatter(
        x=s["ANOENC"], y=s["pct_con_mascota"], name="% hogares con mascotas",
        mode="lines+markers+text", line=dict(color="crimson", width=3),
        marker=dict(size=9), yaxis="y2",
        text=[f"{v:.0f}%" for v in s["pct_con_mascota"]], textposition="top center",
    ))
    fig.update_layout(
        title="Evolución del gasto en mascotas y del % de hogares con mascotas",
        template=PLOT_TEMPLATE, height=430,
        yaxis=dict(title="Gasto nacional (M€)"),
        yaxis2=dict(title="% hogares con mascotas", overlaying="y", side="right",
                    range=[0, max(60, s["pct_con_mascota"].max() * 1.2)]),
        legend=dict(orientation="h", yanchor="bottom", y=1.02, x=0),
        xaxis=dict(dtick=1),
    )
    st.plotly_chart(fig, use_container_width=True)

    comp = ctx.composicion_filtrada().sort_values("ANOENC")
    orden_clas = [CLAS_PRODUCTOS, CLAS_SERVICIOS, CLAS_AMBOS, SIN_MASCOTA]
    fig2 = px.area(
        comp, x="ANOENC", y="pct", color="CLASIFICACION",
        category_orders={"CLASIFICACION": orden_clas},
        color_discrete_map=CLAS_COLORS,
        title="Composición de los hogares por tipo de gasto en mascotas (%)",
        labels={"ANOENC": "Año", "pct": "% de hogares", "CLASIFICACION": "Clasificación"},
        template=PLOT_TEMPLATE, height=430,
    )
    fig2.update_layout(xaxis=dict(dtick=1))
    st.plotly_chart(fig2, use_container_width=True)
