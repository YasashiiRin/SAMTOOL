# main.py
import os
import sys
from gui.viewer import CobolProjectViewer
import ttkbootstrap as ttk

# Fix đường dẫn khi đóng gói .exe
if getattr(sys, 'frozen', False):
    os.chdir(sys._MEIPASS)

if __name__ == "__main__":
    root = ttk.Window(themename="superhero")
    root.title("SSAM - COBOL Data Viewer 2025 v1.0")
    root.geometry("1600x900")
    root.minsize(1200, 700)
    try:
        root.iconbitmap("assets/ssam.ico")
    except:
        pass
    
    app = CobolProjectViewer(root)
    root.mainloop()