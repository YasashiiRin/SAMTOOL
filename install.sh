#!/bin/bash
# ==================================================
# SSAM 2025 - One-click Installer
# ==================================================

set -e

clear
echo ""
echo "╔══════════════════════════════════════════════════╗"
echo "║      SSAM 2025 - COBOL Viewer                    ║"
echo "║      Installing automatically...                 ║"
echo "╚══════════════════════════════════════════════════╝"
echo ""

INSTALL_DIR="$HOME/.ssam"
BIN_DIR="$HOME/.local/bin"

# Tạo thư mục im lặng tuyệt đối
command mkdir -p "$INSTALL_DIR" "$BIN_DIR" 2>/dev/null || :

cd "$INSTALL_DIR"

# Xóa bản cũ
rm -rf SAMTOOL/ 2>/dev/null || true

echo "[1/4] Downloading SSAM from GitHub..."
git clone --quiet --depth=1 https://github.com/YasashiiRin/SAMTOOL.git SAMTOOL >/dev/null || {
    echo "Failed to download! Check internet connection."
    exit 1
}

cd SAMTOOL

echo "[2/4] Installing Python (if needed)..."
# ƯU TIÊN: Dùng Python hệ thống nếu có (nhanh, ổn định)
if command -v python3 >/dev/null 2>&1 && [[ $(python3 -c "import sys; print(sys.version_info[:2])" 2>/dev/null | grep -q "^3\.[1-9]") ]]; then
    echo "   → Using system Python (detected Python 3.x)"
    PYTHON_CMD="python3"
else
    # Fallback: Tải Python portable nếu hệ thống không có
    echo "   → Downloading portable Python 3.12.8 (~55 MB)..."
    case "$(uname)-$(uname -m)" in
        Darwin-arm64)   URL="https://github.com/indygreg/python-build-standalone/releases/download/20240706/cpython-3.12.4+20240706-aarch64-unknown-darwin-install_only.tar.gz" ;;
        Darwin-x86_64)  URL="https://github.com/indygreg/python-build-standalone/releases/download/20240706/cpython-3.12.4+20240706-x86_64-apple-darwin-install_only.tar.gz" ;;
        Linux-x86_64)   URL="https://github.com/indygreg/python-build-standalone/releases/download/20240706/cpython-3.12.4+20240706-x86_64-unknown-linux-gnu-install_only.tar.gz" ;;
        Linux-aarch64)  URL="https://github.com/indygreg/python-build-standalone/releases/download/20240706/cpython-3.12.4+20240706-aarch64-unknown-linux-gnu-install_only.tar.gz" ;;
        *) echo "Unsupported platform"; exit 1 ;;
    esac

    if [ ! -d "python-env" ]; then
        curl -L --fail --progress-bar -o python.tar.gz "$URL"
        tar -xf python.tar.gz >/dev/null 2>&1
        rm python.tar.gz
        mv python*/ python-env 2>/dev/null || true
    fi
    PYTHON_CMD="$INSTALL_DIR/SAMTOOL/python-env/bin/python3"
    export PATH="$INSTALL_DIR/SAMTOOL/python-env/bin:$PATH"
fi

echo "[3/4] Installing Python packages..."
"$PYTHON_CMD" -m pip install --quiet --disable-pip-version-check ttkbootstrap pillow >/dev/null 2>&1

echo "[4/4] Installing GnuCOBOL (if needed)..."
if ! command -v cobc &>/dev/null; then
    case "$(uname)" in
        Darwin*)  brew install gnu-cobol >/dev/null 2>&1 || true ;;
        Linux*)
            (command -v apt >/dev/null && sudo apt update && sudo apt install -y gnucobol) ||
            (command -v dnf >/dev/null && sudo dnf install -y gnu-cobol) ||
            (command -v pacman >/dev/null && sudo pacman -Sy --noconfirm gnu-cobol) || true
            ;;
    esac >/dev/null 2>&1 || true
fi

# Tạo lệnh ssam
cat > "$BIN_DIR/ssam" << 'EOF'
#!/bin/bash
cd "$HOME/.ssam/SAMTOOL"
exec "$HOME/.ssam/SAMTOOL/python-env/bin/python3" main.py "$@" 2>/dev/null || python3 main.py "$@"
EOF
chmod +x "$BIN_DIR/ssam"

# Thêm PATH nếu chưa có
if ! echo "$PATH" | grep -q "$HOME/.local/bin"; then
    echo 'export PATH="$HOME/.local/bin:$PATH"' >> "$HOME/.bashrc"
    echo 'export PATH="$HOME/.local/bin:$PATH"' >> "$HOME/.zshrc" 2>/dev/null || true
fi

echo ""
echo "╔══════════════════════════════════════════════════╗"
echo "║      INSTALLATION COMPLETED SUCCESSFULLY!        ║"
echo "║                                                  ║"
echo "║      Now just type:   ssam                       ║"
echo "║                                                  ║"
echo "╚══════════════════════════════════════════════════╝"
echo ""
echo "SSAM 2025 - COBOL Data Viewer"
echo ""