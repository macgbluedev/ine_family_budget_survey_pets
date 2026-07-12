"""Tab: Análisis por variable (Variable breakdown) — grouped bars, heatmap and comparison table."""
import pandas as pd
import plotly.express as px
import streamlit as st

from config import PLOT_TEMPLATE, VARIABLES
from context import TabContext
from utils import ordenar_categorias


def render(ctx: TabContext):
    c1, c2 = st.columns([3, 2])
    var_label = c1.selectbox("Variable de desglose", options=list(VARIABLES.keys()))
    metrica = c2.radio(
        "Métrica", ["% hogares con mascotas", "Gasto total (€)"],
        horizontal=False,
    )
    var_def = VARIABLES[var_label]
    df_var = ctx.resumen_var_filtrado(var_def["cols"])

    if df_var.empty:
        st.warning("No hay datos para la selección actual.")
        return

    orden_cat = ordenar_categorias(df_var, var_def["order"])
    col_metric = {
        "% hogares con mascotas": "pct_con_mascota",
        "Gasto total (€)": "gasto_total",
    }[metrica]

    df_plot = df_var.copy()
    if col_metric == "gasto_total":
        df_plot["valor_plot"] = df_plot["gasto_total"] / 1e6
        eje = "Gasto total (M€)"
    else:
        df_plot["valor_plot"] = df_plot[col_metric]
        eje = metrica

    # Barras agrupadas por año
    fig = px.bar(
        df_plot, x="Categoria", y="valor_plot", color="ANOENC",
        barmode="group", category_orders={"Categoria": orden_cat},
        title=f"{metrica} por {var_label} y año",
        labels={"Categoria": var_label, "valor_plot": eje, "ANOENC": "Año"},
        template=PLOT_TEMPLATE, height=480,
        color_continuous_scale="Viridis",
    )
    fig.update_xaxes(tickangle=35)
    st.plotly_chart(fig, use_container_width=True)

    # Heatmap categoría x año
    pivot = df_plot.pivot(index="Categoria", columns="ANOENC", values="valor_plot")
    pivot = pivot.reindex([c for c in orden_cat if c in pivot.index])
    fig_h = px.imshow(
        pivot, aspect="auto", color_continuous_scale="YlOrRd",
        title=f"Heatmap · {metrica} ({var_label} × año)",
        labels=dict(x="Año", y=var_label, color=eje),
        template=PLOT_TEMPLATE,
    )
    fig_h.update_layout(height=max(320, 40 * len(pivot)))
    st.plotly_chart(fig_h, use_container_width=True)

    # Tabla comparativa foco vs base
    st.subheader(f"Comparativa {ctx.año_foco} vs {ctx.año_base}")
    piv_foco = df_var[df_var["ANOENC"] == ctx.año_foco].set_index("Categoria")
    piv_base = df_var[df_var["ANOENC"] == ctx.año_base].set_index("Categoria")
    tabla = pd.DataFrame(index=orden_cat)
    tabla[f"% con mascota {ctx.año_base}"] = piv_base["pct_con_mascota"].round(1)
    tabla[f"% con mascota {ctx.año_foco}"] = piv_foco["pct_con_mascota"].round(1)
    tabla["Δ pp"] = (tabla[f"% con mascota {ctx.año_foco}"] - tabla[f"% con mascota {ctx.año_base}"]).round(1)
    tabla[f"Gasto {ctx.año_base} (M€)"] = (piv_base["gasto_total"] / 1e6).round(1)
    tabla[f"Gasto {ctx.año_foco} (M€)"] = (piv_foco["gasto_total"] / 1e6).round(1)
    tabla["Δ gasto %"] = ((piv_foco["gasto_total"] - piv_base["gasto_total"])
                          / piv_base["gasto_total"] * 100).round(1)
    tabla = tabla.reset_index().rename(columns={"index": var_label})
    st.dataframe(tabla, use_container_width=True, hide_index=True)
    st.download_button(
        "⬇️ Descargar tabla (CSV)",
        tabla.to_csv(index=False).encode("utf-8"),
        file_name=f"comparativa_{var_def['cols'][0]}_{ctx.año_foco}_vs_{ctx.año_base}.csv",
        mime="text/csv",
    )
