"""Contexto que app.py construye una vez y pasa a cada pestaña de dashboard/tabs/."""
from dataclasses import dataclass
from typing import Callable, List

import pandas as pd


@dataclass
class TabContext:
    resumen_var_filtrado: Callable[[List[str]], pd.DataFrame]
    serie_filtrada: Callable[[], pd.DataFrame]
    composicion_filtrada: Callable[[], pd.DataFrame]
    año_foco: int
    año_base: int
