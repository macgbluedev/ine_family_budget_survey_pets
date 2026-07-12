"""Carga y cacheo de los CSV generados por prep_data.py (dashboard/data/*.csv)."""
import os

import pandas as pd
import streamlit as st

from config import CCAA_NORMALIZADAS, CODIGO_NOMBRES, SIN_MASCOTA

DATA_DIR = os.path.join(os.path.dirname(os.path.abspath(__file__)), "data")
GASTOS_CSV = os.path.join(DATA_DIR, "gastos_16a25.csv")
PROP_CSV = os.path.join(DATA_DIR, "proporcion_16a25.csv")


@st.cache_data
def load_gastos():
    df = pd.read_csv(GASTOS_CSV, encoding="utf-8")
    df["ANOENC"] = df["ANOENC"].astype(int)
    df["NUMERO"] = df["NUMERO"].astype(int)
    df["CODIGO"] = df["CODIGO"].astype(int)
    df["GASTO"] = pd.to_numeric(df["GASTO"], errors="coerce")
    df["NOMBRE_CODIGO"] = df["CODIGO"].map(CODIGO_NOMBRES)
    df["CCAA_NORM"] = df["CCAA"].map(CCAA_NORMALIZADAS)
    return df


@st.cache_data
def load_proporcion():
    df = pd.read_csv(PROP_CSV, encoding="utf-8")
    df["ANOENC"] = df["ANOENC"].astype(int)
    df["NUMERO"] = df["NUMERO"].astype(int)
    df["CON_MASCOTA"] = df["CLASIFICACION"] != SIN_MASCOTA
    df["CCAA_NORM"] = df["CCAA"].map(CCAA_NORMALIZADAS)
    return df
