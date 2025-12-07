#!/bin/bash
# Script de inicio rápido para el Dashboard de Mascotas EPF

echo ""
echo "=========================================="
echo "🐕 Dashboard Mascotas EPF 2022-2024 🐈"
echo "=========================================="
echo ""

# Verificar que existe el archivo de datos
if [ ! -f "GastosSoloMascotas22a24.csv" ]; then
    echo "❌ ERROR: No se encuentra el archivo GastosSoloMascotas22a24.csv"
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
if ! python3 -c "import dash" 2>/dev/null; then
    echo "⚠️  Instalando dependencias necesarias..."
    pip3 install -q dash dash-bootstrap-components plotly pandas numpy
    echo "✅ Dependencias instaladas"
else
    echo "✅ Dependencias ya instaladas"
fi

echo ""
echo "🚀 Iniciando dashboard..."
echo ""
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "📊 Dashboard disponible en:"
echo "   👉 http://localhost:8050"
echo ""
echo "💡 Presiona Ctrl+C para detener el servidor"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo ""

# Ejecutar dashboard
python3 dashboard_mascotas.py
