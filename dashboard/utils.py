"""Helpers puros (sin estado de Streamlit) reutilizados por app.py y las pestañas."""


def categoria(df, cols):
    """Serie con la categoría (columna o cruce concatenado)."""
    if len(cols) == 1:
        return df[cols[0]].astype(str)
    return df[cols].astype(str).agg(" · ".join, axis=1)


def ordenar_categorias(df, orden):
    if orden:
        presentes = [c for c in orden if c in df["Categoria"].unique()]
        extra = [c for c in df["Categoria"].unique() if c not in presentes]
        return presentes + extra
    return sorted(df["Categoria"].unique())


def fmt_millones(v):
    return f"{v/1e6:,.1f} M€"
