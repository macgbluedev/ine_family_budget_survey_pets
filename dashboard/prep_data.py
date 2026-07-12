#!/usr/bin/env python3
"""
Extracción de microdatos EPF (mascotas) hacia CSV apilados que consume el
dashboard de Streamlit.

Uso:  python3 dashboard/prep_data.py

Fuentes:
  - 2016-2024: hojas `TablaMaestra` de
      reports/2024/ComparativaMascotas16a24.xlsm            -> gasto por hogar x código
      reports/2024/ComparativaMascotas16a24Proporcion.xlsm  -> clasificación por hogar
  - 2025: Outputs/2025/
      EPFOnlyPetsResult.xlsx            -> gasto por hogar x código
      EPFOnlyPetsProportionResult.xlsx -> clasificación por hogar
    (el texto de 2025 llega con la codificación rota —acentos y € como '?'—;
     se reconstruye mapeando contra el vocabulario válido de 2016-2024.)

Escribe:
  - dashboard/data/gastos_16a25.csv
  - dashboard/data/proporcion_16a25.csv
"""
import os
import sys

import pandas as pd

REPO_ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
REPORTS_DIR = os.path.join(REPO_ROOT, "reports", "2024")
DATA_DIR = os.path.join(os.path.dirname(os.path.abspath(__file__)), "data")

GASTOS_XLSM = os.path.join(REPORTS_DIR, "ComparativaMascotas16a24.xlsm")
PROP_XLSM = os.path.join(REPORTS_DIR, "ComparativaMascotas16a24Proporcion.xlsm")

GASTOS_2025 = os.path.join(REPO_ROOT, "Outputs", "2025", "EPFOnlyPetsResult.xlsx")
PROP_2025 = os.path.join(REPO_ROOT, "Outputs", "2025", "EPFOnlyPetsProportionResult.xlsx")

GASTOS_CSV = os.path.join(DATA_DIR, "gastos_16a25.csv")
PROP_CSV = os.path.join(DATA_DIR, "proporcion_16a25.csv")

# Columnas de texto categórico cuya codificación hay que reparar en 2025.
TEXT_COLS = ["CCAA", "CAPROV", "TAMAMU", "TAMANO", "TIPHOGAR1", "TIPHOGAR7", "INTERIN"]


def _leer_tabla_maestra(path):
    if not os.path.exists(path):
        sys.exit(f"ERROR: no se encuentra el archivo {path}")
    return pd.read_excel(path, sheet_name="TablaMaestra", engine="openpyxl")


def _skeleton(s):
    """Reproduce la corrupción de 2025: cada *byte* UTF-8 no-ASCII se vuelve '?'.

    (Un carácter acentuado ocupa 2 bytes y '€' 3, por eso aparecen '??' y '???'.)
    """
    return "".join("?" if b >= 128 else chr(b) for b in str(s).encode("utf-8"))


# Valores de 2025 que no son texto corrupto sino códigos crudos a normalizar.
OVERRIDES = {"INTERIN": {"0": "Sin registro de ingresos"}}


def _mapa_skeleton(canon_df, col):
    """Dado el vocabulario correcto (2016-24), map esqueleto -> etiqueta canónica."""
    return {_skeleton(v): v for v in canon_df[col].dropna().unique()}


def _reparar_texto(df, canon_por_col):
    """Repara las columnas de texto de 2025 usando los mapas de esqueleto."""
    df = df.copy()
    for col in TEXT_COLS:
        if col in df.columns and col in canon_por_col:
            m = canon_por_col[col]
            ov = OVERRIDES.get(col, {})
            df[col] = df[col].map(
                lambda v: ov.get(str(v).strip(), m.get(_skeleton(v), v))
            )
    return df


def _cargar_gasto_2025(canon_por_col):
    if not os.path.exists(GASTOS_2025):
        print(f"  (aviso) 2025 gasto no encontrado en {GASTOS_2025}; se omite.")
        return None
    df = pd.read_excel(GASTOS_2025, engine="openpyxl")
    df = df.drop(columns=[c for c in df.columns if str(c).startswith("Unnamed")])
    df["ANOENC"] = 2025
    df["CODIGO_4D"] = df["CODIGO"].astype(int) // 10
    df = _reparar_texto(df, canon_por_col)
    return df


def _cargar_prop_2025(canon_por_col, columnas):
    if not os.path.exists(PROP_2025):
        print(f"  (aviso) 2025 proporción no encontrada en {PROP_2025}; se omite.")
        return None
    df = pd.read_excel(PROP_2025, engine="openpyxl")
    df = df.rename(columns={"Clasificacion": "CLASIFICACION"})
    df["ANOENC"] = 2025
    df = _reparar_texto(df, canon_por_col)
    # Alinear con el esquema 2016-24 (descarta columnas Pet*/Unnamed sobrantes)
    return df[[c for c in columnas if c in df.columns]]


def main():
    os.makedirs(DATA_DIR, exist_ok=True)

    print("Leyendo gasto 2016-2024 (puede tardar unos segundos)...")
    gastos = _leer_tabla_maestra(GASTOS_XLSM)
    gastos = gastos.rename(columns={"CODIGO 4D": "CODIGO_4D"})

    print("Leyendo proporción 2016-2024 (puede tardar unos segundos)...")
    prop = _leer_tabla_maestra(PROP_XLSM)

    # Mapas de reparación de texto a partir del vocabulario correcto 2016-24.
    canon_gasto = {c: _mapa_skeleton(gastos, c) for c in TEXT_COLS}
    canon_prop = {c: _mapa_skeleton(prop, c) for c in TEXT_COLS}

    print("Añadiendo 2025 desde Outputs/2025/ ...")
    g25 = _cargar_gasto_2025(canon_gasto)
    if g25 is not None:
        gastos = pd.concat([gastos, g25[gastos.columns]], ignore_index=True)
    p25 = _cargar_prop_2025(canon_prop, list(prop.columns))
    if p25 is not None:
        prop = pd.concat([prop, p25], ignore_index=True)

    gastos.to_csv(GASTOS_CSV, index=False, encoding="utf-8")
    print(f"  -> {GASTOS_CSV}: {len(gastos):,} filas | años {sorted(gastos['ANOENC'].unique())}")
    prop.to_csv(PROP_CSV, index=False, encoding="utf-8")
    print(f"  -> {PROP_CSV}: {len(prop):,} filas | años {sorted(prop['ANOENC'].unique())}")

    print("Listo.")


if __name__ == "__main__":
    main()
