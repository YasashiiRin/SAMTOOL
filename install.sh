#!/bin/bash
# ==================================================
# SSAM 2025 - One-click Installer (Ultimate Edition)
# ==================================================
set -e

clear
echo ""
echo "╔══════════════════════════════════════════════════╗"
echo "║      SSAM 2025 - COBOL Data Viewer               ║"
echo "║           Fast & Smart Installer                 ║"
echo "╚══════════════════════════════════════════════════╝"
echo ""

INSTALL_DIR="$HOME/.ssam"
BIN_DIR="$HOME/.local/bin"

# Tạo thư mục im lặng tuyệt đối
command mkdir -p "$INSTALL_DIR" "$BIN_DIR" 2>/dev/null || :

cd "$INSTALL_DIR"

# Xóa bản cũ
rm -rf SAMTOOL/ 2>/dev/null || true

echo "[1/4] Downloading SSAM (fast tarball method)..."
curl -L --fail --progress-bar \
  https://github.com/YasashiiRin/SAMTOOL/archive/refs/heads/main.tar.gz \
  -o ssam.tar.gz

tar -xf ssam.tar.gz >/dev/null 2>&1
mv SAMTOOL-main SAMTOOL
rm ssam.tar.gz
cd SAMTOOL

echo "[2/4] Setting up Python..."
if command -v python3 >/dev/null 2>&1 && python3 -c "import sys; sys.exit(0)" 2>/dev/null; then
    echo "   → Using system Python3"
    PYTHON_CMD="python3"
else
    echo "   → Downloading portable Python 3.12 (~55 MB)..."
    case "$(uname)-$(uname -m)" in
        Darwin-arm64)   URL="https://github.com/indygreg/python-build-standalone/releases/download/20240706/cpython-3.12.4+20240706-aarch64-apple-darwin-install_only.tar.gz" ;;
        Darwin-x86_64)  URL="https://github.com/indygreg/python-build-standalone/releases/download/20240706/cpython-3.12.4+20240706-x86_64-apple-darwin-install_only.tar.gz" ;;
        Linux-x86_64)   URL="https://github.com/indygreg/python-build-standalone/releases/download/20240706/cpython-3.12.4+20240706-x86_64-unknown-linux-gnu-install_only.tar.gz" ;;
        Linux-aarch64)  URL="https://github.com/indygreg/python-build-standalone/releases/download/20240706/cpython-3.12.4+20240706-aarch64-unknown-linux-gnu-install_only.tar.gz" ;;
        *) echo "Unsupported platform"; exit 1 ;;
    esac
    [ ! -d "python-env" ] && {
        curl -L --fail --progress-bar -o python.tar.gz "$URL"
        tar -xf python.tar.gz >/dev/null 2>&1
        rm python.tar.gz
        mv python*/ python-env 2>/dev/null || true
    }
    PYTHON_CMD="$INSTALL_DIR/SAMTOOL/python-env/bin/python3"
    export PATH="$INSTALL_DIR/SAMTOOL/python-env/bin:$PATH"
fi

echo "[3/4] Installing required packages..."
"$PYTHON_CMD" -m pip install --quiet --disable-pip-version-check ttkbootstrap pillow >/dev/null 2>&1

echo "[4/4] Ensuring GnuCOBOL is available..."
if ! command -v cobc &>/dev/null; then
    echo "   → Installing GnuCOBOL..."
    case "$(uname)" in
        Darwin*)  brew install gnu-cobol >/dev/null 2>&1 || true ;;
        Linux*)
            (command -v apt  >/dev/null && sudo apt update && sudo apt install -y gnucobol) ||
            (command -v dnf  >/dev/null && sudo dnf install -y gnu-cobol) ||
            (command -v pacman >/dev/null && sudo pacman -Sy --noconfirm gnu-cobol) || true
            ;;
    esac >/dev/null 2>&1 || true
fi

# Tạo lệnh ssam siêu thông minh
cat > "$BIN_DIR/ssam" << 'EOF'
#!/bin/bash
cd "$HOME/.ssam/SAMTOOL"
if [ -x "python-env/bin/python3" ]; then
    exec python-env/bin/python3 main.py "$@"
else
    exec python3 main.py "$@"
fi
EOF
chmod +x "$BIN_DIR/ssam"

for file in "$HOME/.bashrc" "$HOME/.zshrc" "$HOME/.profile"; do
    [ -f "$file" ] && grep -q '$HOME/.local/bin' "$file" || echo 'export PATH="$HOME/.local/bin:$PATH"' >> "$file"
done

echo ""
echo "╔══════════════════════════════════════════════════╗"
echo "║      INSTALLATION COMPLETED SUCCESSFULLY!        ║"
echo "║                                                  ║"
echo "║           Now type: ssam                         ║"
echo "║                                                  ║"
echo "╚══════════════════════════════════════════════════╝"
echo ""
echo "SSAM 2025 - The ultimate COBOL data viewer"
echo ""