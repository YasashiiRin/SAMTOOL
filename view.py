# view.py
from gui.viewer import CobolProjectViewer
import tkinter as tk
import os
from pathlib import Path
from PySide6.QtWidgets import (
    QApplication, QMainWindow, QWidget, QTreeWidget, QTreeWidgetItem, 
    QVBoxLayout, QTextEdit, QTableWidget, QTableWidgetItem, QSplitter,
    QPushButton, QFileDialog, QLabel
)
from PySide6.QtCore import Qt
from PySide6.QtGui import QPalette, QColor, QFont

from engine.reader import CobolDataReader

if __name__ == "__main__":
    root = tk.Tk()
    app = CobolProjectViewer(root)
    root.mainloop()

# # ===========================
# # RUN
# # ===========================
# if __name__ == "__main__":
#     app = QApplication()
#     viewer = CobolProjectViewer()
#     viewer.show()
#     app.exec()