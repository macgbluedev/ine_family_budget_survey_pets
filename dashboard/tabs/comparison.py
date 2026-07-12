"""Tab: Comparativa temporal (Temporal comparison) — slope chart, diverging bars and small multiples."""
import numpy as np
import plotly.express as px
import plotly.graph_objects as go
import streamlit as st

from config import PLOT_TEMPLATE, VARIABLES
from context import TabContext
from utils import ordenar_categorias


def render(ctx: TabContext):
    var_label2 = st.selectbox(
        "Variable de desglose", options=list(VARIABLES.keys()), key="cmp_var"
    )
    metrica2 = st.radio(
        "Métrica", ["% hogares con mascotas", "Gasto total (€)"],
        horizontal=True, key="cmp_metric",
    )
    var_def2 = VARIABLES[var_label2]
    df_var2 = ctx.resumen_var_filtrado(var_def2["cols"])

    if df_var2.empty:
        st.warning("No hay datos para la selección actual.")
        return

    orden_cat2 = ordenar_categorias(df_var2, var_def2["order"])
    if metrica2 == "% hogares con mascotas":
        valcol2, eje2 = "pct_con_mascota", "% con mascotas"
        df_var2["valor2"] = df_var2[valcol2]
    else:
        valcol2, eje2 = "gasto_total", "Gasto total (M€)"
        df_var2["valor2"] = df_var2["gasto_total"] / 1e6

    # Slope chart: año base -> año foco
    st.subheader(f"Slope chart · {ctx.año_base} → {ctx.año_foco}")
    sl = df_var2[df_var2["ANOENC"].isin([ctx.año_base, ctx.año_foco])]
    fig_s = go.Figure()
    for cat in orden_cat2:
        d = sl[sl["Categoria"] == cat].set_index("ANOENC")
        if ctx.año_base in d.index and ctx.año_foco in d.index:
            fig_s.add_trace(go.Scatter(
                x=[str(ctx.año_base), str(ctx.año_foco)],
                y=[d.loc[ctx.año_base, "valor2"], d.loc[ctx.año_foco, "valor2"]],
                mode="lines+markers+text", name=cat,
                text=[cat, ""], textposition="middle left",
                line=dict(width=2),
            ))
    fig_s.update_layout(
        template=PLOT_TEMPLATE, height=520, showlegend=False,
        yaxis_title=eje2, xaxis_title="Año",
        title=f"Cambio de {metrica2} por {var_label2}",
    )
    st.plotly_chart(fig_s, use_container_width=True)

    # Barras divergentes del Δ vs año base
    st.subheader(f"Δ {metrica2} · {ctx.año_foco} vs {ctx.año_base}")
    base = df_var2[df_var2["ANOENC"] == ctx.año_base].set_index("Categoria")["valor2"]
    foco = df_var2[df_var2["ANOENC"] == ctx.año_foco].set_index("Categoria")["valor2"]
    delta = (foco - base).dropna().reindex([c for c in orden_cat2 if c in foco.index]).dropna()
    dd = delta.reset_index()
    dd.columns = ["Categoria", "delta"]
    dd["signo"] = np.where(dd["delta"] >= 0, "Aumenta", "Disminuye")
    fig_d = px.bar(
        dd.sort_values("delta"), x="delta", y="Categoria", orientation="h",
        color="signo", color_discrete_map={"Aumenta": "#4C78A8", "Disminuye": "#E45756"},
        title=f"Variación de {metrica2} respecto a {ctx.año_base}",
        labels={"delta": f"Δ ({eje2})", "Categoria": var_label2},
        template=PLOT_TEMPLATE,
    )
    fig_d.update_layout(height=max(320, 32 * len(dd)), showlegend=False)
    st.plotly_chart(fig_d, use_container_width=True)

    # Small multiples (facetas por categoría)
    st.subheader("Evolución por categoría (small multiples)")
    n_cat = df_var2["Categoria"].nunique()
    fig_sm = px.line(
        df_var2.sort_values("ANOENC"), x="ANOENC", y="valor2",
        facet_col="Categoria", facet_col_wrap=4, markers=True,
        category_orders={"Categoria": orden_cat2},
        labels={"ANOENC": "Año", "valor2": eje2},
        template=PLOT_TEMPLATE,
    )
    fig_sm.for_each_annotation(lambda a: a.update(text=a.text.split("=")[-1][:22]))
    fig_sm.update_layout(height=max(300, 190 * int(np.ceil(n_cat / 4))))
    fig_sm.update_xaxes(dtick=2)
    st.plotly_chart(fig_sm, use_container_width=True)
