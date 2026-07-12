#!/bin/bash
# Script de inicio rápido para el Dashboard de Mascotas EPF

echo ""
echo "=========================================="
echo "🐕 Dashboard Mascotas EPF 2022-2024 🐈"
echo "=========================================="
echo ""

# Verificar que existen los datos de origen
if [ ! -f "reports/2024/ComparativaMascotas16a24.xlsm" ]; then
    echo "❌ ERROR: No se encuentran los Excel en reports/"
    echo "   Por favor, asegúrate de estar en el directorio correcto."
    exit 1
fi

# Verificar Python
if ! command -v python3 &> /dev/null; then
    echo "❌ ERROR: Python 3 no está instalado"
    echo "   Instala Python 3 desde https://www.python.org/"
    exit 1
fi

echo "✅ Python detectado: $(python3 --version)"
echo ""

# Verificar dependencias
echo "📦 Verificando dependencias..."
if ! python3 -c "import streamlit" 2>/dev/null; then
    echo "⚠️  Instalando dependencias necesarias..."
    pip3 install -q -r requirements.txt
    echo "✅ Dependencias instaladas"
else
    echo "✅ Dependencias ya instaladas"
fi

# Generar los CSV si aún no existen
if [ ! -f "dashboard/data/gastos_16a25.csv" ] || [ ! -f "dashboard/data/proporcion_16a25.csv" ]; then
    echo "📥 Extrayendo datos de los Excel (dashboard/prep_data.py)..."
    python3 dashboard/prep_data.py
fi

echo ""
echo "🚀 Iniciando dashboard..."
echo ""
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "📊 Dashboard disponible en:"
echo "   👉 http://localhost:8501"
echo ""
echo "💡 Presiona Ctrl+C para detener el servidor"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo ""

# Ejecutar dashboard
python3 -m streamlit run dashboard/app.py
