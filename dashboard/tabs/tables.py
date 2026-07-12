"""Tab: Datos (Data) — downloadable tables for the annual series and per-variable aggregates."""
import streamlit as st

from config import VARIABLES
from context import TabContext


def render(ctx: TabContext):
    st.subheader("Serie anual (según filtros)")
    s = ctx.serie_filtrada().sort_values("ANOENC").copy()
    s_show = s.rename(columns={
        "ANOENC": "Año", "gasto_total": "Gasto total (€)",
        "n_total": "Hogares (muestra)", "n_con": "Hogares con mascota",
        "pct_con_mascota": "% con mascota",
    })
    s_show["Gasto total (€)"] = s_show["Gasto total (€)"].round(0)
    s_show["% con mascota"] = s_show["% con mascota"].round(1)
    st.dataframe(s_show, use_container_width=True, hide_index=True)
    st.download_button(
        "⬇️ Descargar serie anual (CSV)",
        s_show.to_csv(index=False).encode("utf-8"),
        file_name="serie_anual_mascotas.csv", mime="text/csv",
    )

    st.markdown("---")
    st.subheader("Agregado por variable")
    var_label3 = st.selectbox("Variable", options=list(VARIABLES.keys()), key="datos_var")
    df_var3 = ctx.resumen_var_filtrado(VARIABLES[var_label3]["cols"])
    tabla3 = df_var3.rename(columns={
        "ANOENC": "Año", "n_hogares_total": "Hogares (muestra)",
        "n_con_mascota": "Con mascota", "pct_con_mascota": "% con mascota",
        "gasto_total": "Gasto total (€)",
    })
    tabla3["% con mascota"] = tabla3["% con mascota"].round(1)
    tabla3["Gasto total (€)"] = tabla3["Gasto total (€)"].round(0)
    st.dataframe(tabla3, use_container_width=True, hide_index=True)
    st.download_button(
        "⬇️ Descargar agregado (CSV)",
        tabla3.to_csv(index=False).encode("utf-8"),
        file_name=f"agregado_{VARIABLES[var_label3]['cols'][0]}.csv", mime="text/csv",
    )
