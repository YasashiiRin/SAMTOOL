#!/bin/bash
# ==================================================
# SSAM 2025 - Installer with Virtualenv (PEP668-safe)
# ==================================================

set -e

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

cd "$INSTALL_DIR"

# Xóa bản cũ
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
# Cài hai thư viện cơ bản
venv/bin/pip install ttkbootstrap pillow

# ⭐ Cài toàn bộ thư viện trong requirement.txt (Fix PySide6 missing)
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
            (command -v dnf >/dev/null && sudo dnf install -y gnu-cobol) ||
            (command -v pacman >/dev/null && sudo pacman -Sy --noconfirm gnu-cobol)
            ;;
        Darwin*)
            brew install gnu-cobol
            ;;
    esac
fi

# tạo lệnh ssam
cat > "$BIN_DIR/ssam" << 'EOF'
#!/bin/bash
cd "$HOME/.ssam/SAMTOOL"
exec venv/bin/python view.py "$@"
EOF

chmod +x "$BIN_DIR/ssam"

# đảm bảo PATH có ~/.local/bin
if ! echo "$PATH" | grep -q "$HOME/.local/bin"; then
    echo 'export PATH="$HOME/.local/bin:$PATH"' >> "$HOME/.profile"
fi

echo ""
echo "╔══════════════════════════════════════════════════╗"
echo "║      INSTALLATION COMPLETED SUCCESSFULLY!        ║"
echo "║          run SSAM by typing:  ssam               ║"
echo "╚══════════════════════════════════════════════════╝"
echo ""
