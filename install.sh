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

source venv/bin/activate

echo "[3/4] Installing Python packages (inside venv)..."
pip install ttkbootstrap pillow

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
source venv/bin/activate
exec python main.py "$@"
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
