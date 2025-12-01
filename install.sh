#!/bin/bash
# ==================================================
# SSAM 2025 - One-click Installer
# Repo: https://github.com/YasashiiRin/SAMTOOL
# ==================================================

set -e

echo ""
echo "╔══════════════════════════════════════════════════╗"
echo "║              SSAM 2025 - COBOL Viewer            ║"
echo "║           Installing automatically...           ║"
echo "╚══════════════════════════════════════════════════╝"
echo ""

INSTALL_DIR="$HOME/.ssam"
BIN_DIR="$HOME/.local/bin"
mkdir -p "$INSTALL_DIR" "$BIN_DIR" 2>/dev/null || true
cd "$INSTALL_DIR"

# XÓA BẢN CŨ - PHẢI ĐÚNG TÊN THƯ MỤC
rm -rf SAMTOOL/

echo "[1/4] Đang tải SSAM từ GitHub..."
git clone --quiet --depth=1 https://github.com/YasashiiRin/SAMTOOL.git SAMTOOL || {
    echo "Lỗi tải repo! Kiểm tra mạng hoặc GitHub bị chặn!"
    exit 1
}

cd SAMTOOL

echo "[2/4] Đang cài Python portable (nếu chưa có)..."
if [ ! -d "python-env" ]; then
    echo "   → Tải Python 3.11 portable..."
    case "$(uname)-$(uname -m)" in
        Darwin-arm64)  URL="https://github.com/indygreg/python-build-standalone/releases/download/20241021/cpython-3.11.10+20241021-aarch64-apple-darwin-install_only.tar.gz" ;;
        Darwin-x86_64) URL="https://github.com/indygreg/python-build-standalone/releases/download/20241021/cpython-3.11.10+20241021-x86_64-apple-darwin-install_only.tar.gz" ;;
        Linux-x86_64)  URL="https://github.com/indygreg/python-build-standalone/releases/download/20241021/cpython-3.11.10+20241021-x86_64-unknown-linux-gnu-install_only.tar.gz" ;;
        Linux-aarch64) URL="https://github.com/indygreg/python-build-standalone/releases/download/20241021/cpython-3.11.10+20241021-aarch64-unknown-linux-gnu-install_only.tar.gz" ;;
        *) echo "Kiến trúc $(uname -m) chưa hỗ trợ!"; exit 1 ;;
    esac

    curl -L -# -o python.tar.gz "$URL"
    tar -xf python.tar.gz
    rm python.tar.gz
    mv python* python-env
fi

# ĐƯỜNG DẪN PHẢI ĐÚNG: SAMTOOL, KHÔNG PHẢI ssam
PYTHON_CMD="$INSTALL_DIR/SAMTOOL/python-env/bin/python3"
export PATH="$INSTALL_DIR/SAMTOOL/python-env/bin:$PATH"

echo "[3/4] Đang cài thư viện Python..."
"$PYTHON_CMD" -m pip install --quiet ttkbootstrap pillow

echo "[4/4] Đang cài GnuCOBOL (nếu chưa có)..."
if ! command -v cobc &>/dev/null; then
    echo "   → Cài GnuCOBOL tự động..."
    case "$(uname)" in
        Darwin*)
            if command -v brew &>/dev/null; then
                echo "   → brew install gnu-cobol"
                brew install gnu-cobol >/dev/null 2>&1 || echo "Cài thủ công: brew install gnu-cobol"
            else
                echo "Chưa có Homebrew! Gõ: /bin/bash -c \"\$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)\""
            fi
            ;;
        Linux*)
            if command -v apt >/dev/null 2>&1; then
                sudo apt update && sudo apt install -y gnucobol
            elif command -v dnf >/dev/null 2>&1; then
                sudo dnf install -y gnu-cobol
            elif command -v pacman >/dev/null 2>&1; then
                sudo pacman -S --noconfirm gnu-cobol
            else
                echo "Không hỗ trợ package manager này. Cài thủ công GnuCOBOL nhé!"
            fi
            ;;
    esac
fi

# TẠO LỆNH ssam – PHẢI TRỎ ĐÚNG VÀO THƯ MỤC SAMTOOL
cat > "$BIN_DIR/ssam" << EOF
#!/bin/bash
cd "$INSTALL_DIR/SAMTOOL"
exec "$INSTALL_DIR/SAMTOOL/python-env/bin/python3" main.py "\$@"
EOF
chmod +x "$BIN_DIR/ssam"

# Thêm PATH
if ! echo "$PATH" | grep -q "$HOME/.local/bin"; then
    echo 'export PATH="$HOME/.local/bin:$PATH"' >> "$HOME/.bashrc"
    echo 'export PATH="$HOME/.local/bin:$PATH"' >> "$HOME/.zshrc"
    echo "Đã thêm ~/.local/bin vào PATH!"
    echo "→ Khởi động lại Terminal hoặc gõ: source ~/.zshrc"
fi

echo ""
echo "CÀI ĐẶT THÀNH CÔNG 100%!"
echo "Gõ lệnh: ssam"
echo "→ SSAM 2025 sẽ hiện ra ngay!"
echo ""
echo "SSAM 2025"
echo "https://github.com/YasashiiRin/SAMTOOL"
echo ""