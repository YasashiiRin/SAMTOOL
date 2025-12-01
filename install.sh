#!/bin/bash
# ==================================================
# SSAM 2025 - Installer with Python Version Check
# ==================================================

set -e

REQUIRED_PY=3.10

clear
echo ""
echo "╔══════════════════════════════════════════════════╗"
echo "║      SSAM 2025 - COBOL Viewer Installer          ║"
echo "║      Fully compatible with Arch / Ubuntu / macOS ║"
echo "╚══════════════════════════════════════════════════╝"
echo ""

INSTALL_DIR="$HOME/.ssam"
BIN_DIR="$HOME/.local/bin"

mkdir -p "$INSTALL_DIR" "$BIN_DIR" 2>/dev/null || :

# ==================================================
#  CHECK PYTHON VERSION
# ==================================================
if ! command -v python3 >/dev/null; then
    echo "❌ Python3 is not installed."
    echo "→ Please install Python $REQUIRED_PY or newer:"
    echo ""
    echo "macOS:"
    echo "  brew install python@3.12"
    echo ""
    echo "Ubuntu/Debian:"
    echo "  sudo apt install python3.12"
    echo ""
    exit 1
fi

PY_VER=$(python3 -c 'import sys; print(f"{sys.version_info.major}.{sys.version_info.minor}")')

# Compare versions
version_lt() { [ "$(printf '%s\n' "$1" "$2" | sort -V | head -n1)" != "$2" ]; }

if version_lt "$PY_VER" "$REQUIRED_PY"; then
    echo "❌ Python version too old: $PY_VER"
    echo "→ SSAM requires Python ≥ $REQUIRED_PY"
    echo ""
    echo "Please install a newer Python version:"
    echo ""
    echo "macOS:"
    echo "  brew install python@3.12"
    echo ""
    echo "Ubuntu/Debian:"
    echo "  sudo apt install python3.12"
    echo ""
    exit 1
fi

echo "✔ Python version OK: $PY_VER"
echo ""

# ==================================================
#  INSTALLER MAIN FLOW
# ==================================================
cd "$INSTALL_DIR"

rm -rf SAMTOOL/ 2>/dev/null || true

echo "[1/4] Downloading SSAM (fast tarball)..."
curl -L --fail --progress-bar \
  https://github.com/YasashiiRin/SAMTOOL/archive/refs/heads/main.tar.gz \
  -o ssam.tar.gz

tar -xf ssam.tar.gz >/dev/null 2>&1
mv SAMTOOL-main SAMTOOL
rm ssam.tar.gz

cd SAMTOOL

echo "[2/4] Creating isolated Python environment..."
python3 -m venv venv

echo "[3/4] Installing Python packages (inside venv)..."
venv/bin/pip install ttkbootstrap pillow

if [ -f "requirement.txt" ]; then
    echo "   → Installing requirements from requirement.txt..."
    venv/bin/pip install -r requirement.txt
fi

echo "[4/4] Checking GnuCOBOL..."
if ! command -v cobc >/dev/null 2>&1; then
    echo "   → cobc not found, installing..."
    case "$(uname)" in
        Linux*)
            (command -v apt >/dev/null && sudo apt update && sudo apt install -y gnucobol) ||
            (command -v dnf >/devnull && sudo dnf install -y gnu-cobol) ||
            (command -v pacman >/dev/null && sudo pacman -Sy --noconfirm gnu-cobol)
            ;;
        Darwin*)
            brew install gnu-cobol
            ;;
    esac
fi

# Create ssam launcher
cat > "$BIN_DIR/ssam" << 'EOF'
#!/bin/bash
cd "$HOME/.ssam/SAMTOOL"
exec venv/bin/python view.py "$@"
EOF

chmod +x "$BIN_DIR/ssam"

if ! echo "$PATH" | grep -q "$HOME/.local/bin"; then
    echo 'export PATH="$HOME/.local/bin:$PATH"' >> "$HOME/.profile"
fi

echo ""
echo "╔══════════════════════════════════════════════════╗"
echo "║      INSTALLATION COMPLETED SUCCESSFULLY!        ║"
echo "║          Run SSAM by typing:  ssam               ║"
echo "╚══════════════════════════════════════════════════╝"
echo ""
