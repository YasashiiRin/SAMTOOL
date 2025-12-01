#!/bin/bash
# ==================================================
# SSAM 2025 - One-click Installer
# ==================================================

set -e

clear
echo ""
echo "╔══════════════════════════════════════════════════╗"
echo "║              SSAM 2025 - COBOL Viewer            ║"
echo "║           Installing automatically...           ║"
echo "╚══════════════════════════════════════════════════╝"
echo ""

INSTALL_DIR="$HOME/.ssam"
BIN_DIR="$HOME/.local/bin"

# Tạo thư mục không báo lỗi
[ ! -d "$INSTALL_DIR" ] && mkdir -p "$INSTALL_DIR"
[ ! -d "$BIN_DIR" ] && mkdir -p "$BIN_DIR"

cd "$INSTALL_DIR"

# Xóa phiên bản cũ
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
        *) echo "Unsupported architecture: $(uname -m)"; exit 1 ;;
    esac

    curl -L -# -o python.tar.gz "$URL" >/dev/null
    tar -xf python.tar.gz >/dev/null
    rm python.tar.gz
    mv python*/ python-env 2>/dev/null || true
fi

PYTHON_CMD="$INSTALL_DIR/SAMTOOL/python-env/bin/python3"
export PATH="$INSTALL_DIR/SAMTOOL/python-env/bin:$PATH"

echo "[3/4] Installing Python packages..."
"$PYTHON_CMD" -m pip install --quiet --disable-pip-version-check ttkbootstrap pillow >/dev/null

echo "[4/4] Installing GnuCOBOL (if needed)..."
if ! command -v cobc &>/dev/null; then
    echo "   → Installing GnuCOBOL..."
    case "$(uname)" in
        Darwin*)
            if command -v brew &>/dev/null; then
                brew install gnu-cobol >/dev/null 2>&1
            fi
            ;;
        Linux*)
            if command -v apt >/dev/null 2>&1; then
                sudo apt update >/dev/null 2>&1 && sudo apt install -y gnucobol >/dev/null 2>&1
            elif command -v dnf >/dev/null 2>&1; then
                sudo dnf install -y gnu-cobol >/dev/null 2>&1
            elif command -v pacman >/dev/null 2>&1; then
                sudo pacman -S --noconfirm gnu-cobol >/dev/null 2>&1
            fi
            ;;
    esac
fi

# Tạo lệnh toàn cục
cat > "$BIN_DIR/ssam" << 'EOF'
#!/bin/bash
cd "$HOME/.ssam/SAMTOOL"
exec "$HOME/.ssam/SAMTOOL/python-env/bin/python3" main.py "$@"
EOF
chmod +x "$BIN_DIR/ssam"

# Thêm PATH (chỉ thêm 1 lần)
if ! echo "$PATH" | grep -q "$HOME/.local/bin"; then
    echo 'export PATH="$HOME/.local/bin:$PATH"' >> "$HOME/.bashrc"
    echo 'export PATH="$HOME/.local/bin:$PATH"' >> "$HOME/.zshrc" 2>/dev/null || true
    export PATH="$HOME/.local/bin:$PATH"
fi

echo ""
echo "INSTALLATION COMPLETED SUCCESSFULLY!"
echo "Run command: ssam"
echo "Or restart terminal and type: ssam"
echo ""
echo "SSAM 2025 - COBOL Data Viewer"
echo ""