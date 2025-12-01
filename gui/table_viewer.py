# gui/viewer_qt.py
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


class CobolProjectViewer(QMainWindow):
    def __init__(self):
        super().__init__()

        self.setWindowTitle("SSAM")
        self.resize(1600, 900)

        self.current_project_path = None
        self.tables = {}

        # =========== GIAO DIỆN CHÍNH ===========
        main_widget = QWidget()
        self.setCentralWidget(main_widget)
        main_layout = QVBoxLayout(main_widget)

        splitter = QSplitter(Qt.Horizontal)
        splitter.setStyleSheet("QSplitter::handle { background: #333; }")
        main_layout.addWidget(splitter)

        # === TRÁI: Danh sách bảng ===
        left_panel = QWidget()
        left_layout = QVBoxLayout(left_panel)
        left_layout.setContentsMargins(10, 10, 10, 10)

        lbl_left = QLabel("DANH SÁCH BẢNG COBOL")
        lbl_left.setStyleSheet("color: #00ff00; font-size: 15px; font-weight: bold;")
        left_layout.addWidget(lbl_left)

        self.tree = QTreeWidget()
        self.tree.setHeaderHidden(True)
        self.tree.itemSelectionChanged.connect(self.on_table_selected)

        self.tree.setStyleSheet("""
            QTreeWidget {
                background: #1e1e1e;
                color: #fff;
                border: 1px solid #333;
            }
            QTreeWidget::item:selected {
                background: #0078d7;
                color: white;
            }
        """)

        left_layout.addWidget(self.tree)

        btn_connect = QPushButton("CONNECTION")
        btn_connect.clicked.connect(self.connect_project)
        btn_connect.setStyleSheet("""
            QPushButton {
                background: #006400;
                color: white;
                padding: 10px;
                font-weight: bold;
                border-radius: 4px;
            }
            QPushButton:hover {
                background: #008000;
            }
        """)
        left_layout.addWidget(btn_connect)

        splitter.addWidget(left_panel)
        splitter.setStretchFactor(0, 1)

        # === PHẢI: Log + Data Table ===
        right_panel = QWidget()
        right_layout = QVBoxLayout(right_panel)
        right_layout.setContentsMargins(10, 10, 10, 10)

        # Log
        lbl_log = QLabel("LOG HOẠT ĐỘNG")
        lbl_log.setStyleSheet("color: #00ff00; font-weight: bold;")
        right_layout.addWidget(lbl_log)

        self.log_box = QTextEdit()
        self.log_box.setReadOnly(True)
        self.log_box.setStyleSheet("""
            QTextEdit {
                background: #0f0f0f;
                color: #ffffff;
                border: 1px solid #333;
                font-family: Consolas;
            }
        """)
        right_layout.addWidget(self.log_box, 1)

        # Data table
        self.table = QTableWidget()
        self.table.setStyleSheet("""
            QTableWidget {
                background: #2b2b2b;
                color: white;
                gridline-color: #444;
                font-family: Consolas;
            }
            QHeaderView::section {
                background: #1e1e1e;
                color: #00ff00;
                font-weight: bold;
                padding: 5px;
                border: 1px solid #333;
            }
            QTableWidget::item:selected {
                background: #0078d7;
            }
        """)
        right_layout.addWidget(self.table, 4)

        # Status bar
        self.lbl_status = QLabel("Sẵn sàng")
        self.lbl_status.setStyleSheet("color: #00ff00; padding: 4px;")
        self.statusBar().addWidget(self.lbl_status)

        splitter.addWidget(right_panel)
        splitter.setStretchFactor(1, 4)

    # ----------------------------------------------------
    # LOG
    # ----------------------------------------------------
    def log(self, message, level="INFO"):
        from datetime import datetime
        ts = datetime.now().strftime("%H:%M:%S")

        colors = {
            "INFO": "#ffffff",
            "OK": "#00ff00",
            "WARN": "#ffcc00",
            "ERROR": "#ff4444"
        }

        html = f'<span style="color:{colors.get(level,"white")}">[{ts}] {message}</span><br>'
        self.log_box.insertHtml(html)
        self.log_box.verticalScrollBar().setValue(
            self.log_box.verticalScrollBar().maximum()
        )

    # ----------------------------------------------------
    # LOAD PROJECT
    # ----------------------------------------------------
    def connect_project(self):
        folder = QFileDialog.getExistingDirectory(self, "Chọn thư mục dự án COBOL")
        if not folder: 
            return

        self.current_project_path = folder
        self.lbl_status.setText(f"Đang quét: {folder}")

        CobolDataReader.scan_cobol_project(self, folder)

        self.lbl_status.setText(f"Đã kết nối: {Path(folder).name} ({len(self.tables)} bảng)")

    # ----------------------------------------------------
    # SELECT TABLE
    # ----------------------------------------------------
    def on_table_selected(self):
        sel = self.tree.selectedItems()
        if not sel:
            return

        item = sel[0]
        key = item.text(0)

        info = self.tables.get(key)
        if not info:
            return

        self.log(f"\n=== ĐANG ĐỌC BẢNG: {info['name']} ===")
        CobolDataReader.generate_and_run_cobol_reader(self, info['dat_path'], info['cpy_path'], info['name'])

