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

python -m pip install --quiet ttkbootstrap pillow pyinstaller

where cobc >nul 2>&1
if %errorlevel% neq 0 (
    echo Tai tai: https://github.com/GnuCOBOL/gnu-cobol-windows/releases
    echo Chocolatey: choco install gnucobol
    pause
    exit /b
)

echo Initial SSAM 2025...
python main.py
pause