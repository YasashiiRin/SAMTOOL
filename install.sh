#!/bin/bash
# ==================================================
# SSAM 2025 - One-click Installer (Fast Version)
# ==================================================

set -e

clear
echo ""
echo "╔══════════════════════════════════════════════════╗"
echo "║      SSAM 2025 - COBOL Viewer Installer          ║"
echo "║      Fast, minimal, universal setup              ║"
echo "╚══════════════════════════════════════════════════╝"
echo ""

INSTALL_DIR="$HOME/.ssam"
BIN_DIR="$HOME/.local/bin"

mkdir -p "$INSTALL_DIR" "$BIN_DIR" 2>/dev/null || :

cd "$INSTALL_DIR"

# Xóa bản cũ
rm -rf SAMTOOL/ 2>/dev/null || true

echo "[1/4] Downloading SSAM (no git, fast tarball)..."
curl -L --fail --progress-bar \
  https://github.com/YasashiiRin/SAMTOOL/archive/refs/heads/main.tar.gz \
  -o ssam.tar.gz

tar -xf ssam.tar.gz >/dev/null 2>&1
mv SAMTOOL-main SAMTOOL
rm ssam.tar.gz

cd SAMTOOL

echo "[2/4] Checking Python..."
# Nếu có python3 system → dùng luôn
if command -v python3 >/dev/null 2>&1; then
    echo "   → Using system Python"
    PYTHON_CMD="python3"
else
    echo "   → No system Python detected, downloading portable Python..."
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

echo "[4/4] Checking GnuCOBOL compiler..."
if ! command -v cobc >/dev/null 2>&1; then
    echo "   → GnuCOBOL not found, attempting installation..."
    case "$(uname)" in
        Darwin*)
            command -v brew >/dev/null || { echo "Homebrew not installed! Install Homebrew first."; exit 1; }
            brew install gnu-cobol >/dev/null 2>&1 || true
            ;;
        Linux*)
            (command -v apt >/dev/null && sudo apt update && sudo apt install -y gnucobol) ||
            (command -v dnf >/dev/null && sudo dnf install -y gnu-cobol) ||
            (command -v pacman >/dev/null && sudo pacman -Sy --noconfirm gnu-cobol) || true
            ;;
    esac
fi


# ⚡ Tạo lệnh ssam
cat > "$BIN_DIR/ssam" << 'EOF'
#!/bin/bash
cd "$HOME/.ssam/SAMTOOL"

# Ưu tiên Python portable nếu có
if [ -x "$HOME/.ssam/SAMTOOL/python-env/bin/python3" ]; then
    exec "$HOME/.ssam/SAMTOOL/python-env/bin/python3" main.py "$@"
else
    exec python3 main.py "$@"
fi
EOF

chmod +x "$BIN_DIR/ssam"

# Thêm PATH nếu chưa có
if ! echo "$PATH" | grep -q "$HOME/.local/bin"; then
    echo 'export PATH="$HOME/.local/bin:$PATH"' >> "$HOME/.profile"
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
