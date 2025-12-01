@echo off
echo ========================================
echo    SSAM 2025 - COBOL Viewer
echo    by Tuannv - 2025
echo ========================================

python --version >nul 2>&1
if %errorlevel% neq 0 (
    echo please download python...
    start https://www.python.org/downloads/
    pause
    exit /b
)

echo Dang cai thu vien can thiet...
python -m pip install --quiet ttkbootstrap pillow pyinstaller

where cobc >nul 2>&1
if %errorlevel% neq 0 (
    echo Chua cai GnuCOBOL!
    echo Tai tai: https://github.com/GnuCOBOL/gnu-cobol-windows/releases
    echo Hoac dung Chocolatey: choco install gnucobol
    pause
    exit /b
)

echo Khoi dong SSAM 2025...
python main.py
pause