#!/bin/bash
# ==================================================
# SSAM 2025 - One-click Installer
# ==================================================

set -e

clear
echo ""
echo "╔══════════════════════════════════════════════════╗"
echo "║           SSAM 2025 - COBOL Viewer               ║"
echo "║           Installing automatically...           ║"
echo "╚══════════════════════════════════════════════════╝"
echo ""

INSTALL_DIR="$HOME/.ssam"
BIN_DIR="$HOME/.local/bin"

# DÒNG THẦN THÁNH - KHÔNG BAO GIỜ HIỆN "File exists" NỮA
command mkdir -p "$INSTALL_DIR" "$BIN_DIR" 2>/dev/null || :

cd "$INSTALL_DIR"

rm -rf SAMTOOL/ 2>/dev/null || true

echo "[1/4] Downloading SSAM from GitHub..."
git clone --quiet --depth=1 https://github.com/YasashiiRin/SAMTOOL.git SAMTOOL >/dev/null || {
    echo "Failed to download! Check your internet connection."
    exit 1
}

cd SAMTOOL

echo "[2/4] Installing portable Python (if needed)..."
if [ ! -d "python-env" ]; then
    echo "   → Downloading Python 3.11 portable..."
    case "$(uname)-$(uname -m)" in
        Darwin-arm64)  URL="https://github.com/indygreg/python-build-standalone/releases/download/20241021/cpython-3.11.10+20241021-aarch64-apple-darwin-install_only.tar.gz" ;;
        Darwin-x86_64) URL="https://github.com/indygreg/python-build-standalone/releases/download/20241021/cpython-3.11.10+20241021-x86_64-apple-darwin-install_only.tar.gz" ;;
        Linux-x86_64)  URL="https://github.com/indygreg/python-build-standalone/releases/download/20241021/cpython-3.11.10+20241021-x86_64-unknown-linux-gnu-install_only.tar.gz" ;;
        Linux-aarch64) URL="https://github.com/indygreg/python-build-standalone/releases/download/20241021/cpython-3.11.10+20241021-aarch64-unknown-linux-gnu-install_only.tar.gz" ;;
        *) echo "Unsupported architecture"; exit 1 ;;
    esac
    curl -L -# -o python.tar.gz "$URL" >/dev/null 2>&1
    tar -xf python.tar.gz >/dev/null 2>&1
    rm python.tar.gz
    mv python*/ python-env 2>/dev/null || true
fi

export PATH="$INSTALL_DIR/SAMTOOL/python-env/bin:$PATH"
PYTHON_CMD="$INSTALL_DIR/SAMTOOL/python-env/bin/python3"

echo "[3/4] Installing Python packages..."
"$PYTHON_CMD" -m pip install --quiet ttkbootstrap pillow >/dev/null 2>&1

echo "[4/4] Installing GnuCOBOL (if needed)..."
if ! command -v cobc &>/dev/null; then
    case "$(uname)" in
        Darwin*)  brew install gnu-cobol >/dev/null 2>&1 || true ;;
        Linux*)
            (command -v apt  >/dev/null && sudo apt update && sudo apt install -y gnucobol) ||
            (command -v dnf  >/dev/null && sudo dnf install -y gnu-cobol) ||
            (command -v pacman >/dev/null && sudo pacman -Sy --noconfirm gnu-cobol) || true
            ;;
    esac >/dev/null 2>&1
fi

# Tạo lệnh ssam
cat > "$BIN_DIR/ssam" << 'EOF'
#!/bin/bash
cd "$HOME/.ssam/SAMTOOL"
exec "$HOME/.ssam/SAMTOOL/python-env/bin/python3" main.py "$@"
EOF
chmod +x "$BIN_DIR/ssam"

# Thêm PATH nếu chưa có
if ! echo "$PATH" | grep -q "$HOME/.local/bin"; then
    echo 'export PATH="$HOME/.local/bin:$PATH"' >> "$HOME/.bashrc"
    echo 'export PATH="$HOME/.local/bin:$PATH"' >> "$HOME/.zshrc" 2>/dev/null || true
fi

echo ""
echo "INSTALLATION COMPLETED SUCCESSFULLY!"
echo "Run: ssam"
echo ""
echo "SSAM 2025 - COBOL Data Viewer"
echo ""