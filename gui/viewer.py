#gui/viewer.py
import os
import tkinter as tk
from tkinter import ttk
from tkinter import filedialog, messagebox
from pathlib import Path
from engine.reader import CobolDataReader
import ttkbootstrap as ttk
from ttkbootstrap.constants import *

class CobolProjectViewer():
    def __init__(self, root):
        self.root = root
        self.root.title("SSAM")
        self.root.geometry("1600x900")
        self.root.minsize(1200, 700)

        self.current_project_path = None
        self.tables = {}

        # #menu bar
        # menubar = tk.Menu(root)
        # root.config(menu=menubar)
        # file_menu = tk.Menu(menubar, tearoff=0)
        # menubar.add_cascade(label="File", menu=file_menu)
        # file_menu.add_command(label="Connect Project...", command=self.connect_project)
        # file_menu.add_separator()
        # file_menu.add_command(label="Exit", command=root.quit)

        #tool bar
        topbar = tk.Frame(root, bg="#252526", height=36)  # Màu xám đậm như IDE
        topbar.pack(fill=tk.X, side=tk.TOP)
        topbar.pack_propagate(False)

        # --- Logo / app ---
        app_name = tk.Label(topbar, text=" SSAM", bg="#252526", fg="#00ff00",
                            font=("Consolas", 12, "bold"), anchor="w")
        app_name.pack(side=tk.LEFT, padx=10, pady=6)

        # ---fake menue File ---
        file_btn = tk.Menubutton(topbar, text="File", bg="#252526", fg="white",
                                 activebackground="#3c3c3c", activeforeground="white",
                                 relief="flat", font=("Segoe UI", 10))
        file_btn.pack(side=tk.LEFT, padx=2, pady=4)

        file_menu = tk.Menu(file_btn, tearoff=0, bg="#2d2d2d", fg="white",
                            activebackground="#007acc", activeforeground="white")
        file_btn.config(menu=file_menu)
        file_menu.add_command(label="Connect Project...", command=self.connect_project)
        file_menu.add_separator()
        file_menu.add_command(label="Exit", command=root.quit)

        # --- button Refresh ---
        try:
            img_refresh = tk.PhotoImage(file="assets/arrows.png")
            img_refresh = img_refresh.subsample(1, 1)  # 16px → 8px
        except Exception as e:
            print("load icon:", e)
            img_refresh = None

        self.btn_refresh = ttk.Button(
            topbar,
            image=img_refresh,
            bootstyle="secondary-outline",
            command=self.refresh_project
        )
        self.btn_refresh.image = img_refresh
        self.btn_refresh.pack(side=tk.LEFT, padx=6, pady=3)

        self.create_tooltip(self.btn_refresh, "Refresh project (F5)")
        paned = tk.PanedWindow(root, orient=tk.HORIZONTAL, sashrelief=tk.RAISED, bg="#1e1e1e")
        paned.pack(fill=tk.BOTH, expand=True)

        # === LEFT list tables ===
        left_frame = tk.Frame(paned, width=300, bg="#1e1e1e")
        paned.add(left_frame)

        # tk.Label(left_frame, text="DANH SÁCH BẢNG COBOL", bg="#1e1e1e", fg="#00ff00",
        #          font=("Consolas", 13, "bold")).pack(pady=15)

        tree_frame = tk.Frame(left_frame, bg="#1e1e1e")
        tree_frame.pack(fill=tk.BOTH, expand=True, padx=10, pady=5)

        self.tree = ttk.Treeview(tree_frame, columns=("size",), show="tree headings")
        self.tree.heading("#0", text="List tables")
        self.tree.column("#0", width=250)
        self.tree.column("size", width=0)
        self.tree.pack(fill=tk.BOTH, expand=True)
        self.tree.bind("<<TreeviewSelect>>", self.on_table_selected)

        right_frame = tk.Frame(paned)
        paned.add(right_frame)

        paned.paneconfigure(left_frame, stretch="never")
        paned.paneconfigure(right_frame, stretch="always")

        self.right_frame = right_frame

        self.lbl_status = tk.Label(right_frame, text="Ready", anchor="w", relief=tk.SUNKEN,
                                   bg="#1e1e1e", fg="#f4fff9", font=("Consolas", 8))
        self.lbl_status.pack(fill=tk.X, side=tk.BOTTOM)

        # Log box
        log_frame = tk.Frame(right_frame, bg="#1e1e1e")
        log_frame.pack(fill=tk.X, pady=8, padx=10)
        tk.Label(log_frame, text="LOG", fg="#00ff00", bg="#0a0a0a",
                 font=("Consolas", 11, "bold")).pack(anchor="w")
        self.data_text = tk.Text(log_frame, height=4, font=("Consolas", 8), bg="#0f0f0f", fg="#ffffff",
                                 insertbackground="white")
        self.data_text.pack(fill=tk.X, padx=5, pady=5)
        self.setup_data_table()

        self.reader = CobolDataReader()
        self.reader.root = root
        self.reader.tree = self.tree
        self.reader.tree_data = self.tree_data
        self.reader.data_text = self.data_text
        self.reader.lbl_status = self.lbl_status
        self.reader.display_table_data = self.display_table_data
        self.reader.tables = self.tables

    def create_tooltip(self, widget, text):
        def on_enter(event):
            x, y, _, _ = widget.bbox("insert")
            x += widget.winfo_rootx() + 25
            y += widget.winfo_rooty() + 25

            self.tooltip = tk.Toplevel(widget)
            self.tooltip.wm_overrideredirect(True)
            self.tooltip.wm_geometry(f"+{x}+{y}")

            label = tk.Label(self.tooltip, text=text, background="#ffffc8", relief="solid",
                             borderwidth=1, font=("Consolas", 9))
            label.pack()
        def on_leave(event):
            if hasattr(self, 'tooltip'):
                self.tooltip.destroy()

        widget.bind("<Enter>", on_enter)
        widget.bind("<Leave>", on_leave)
    def setup_data_table(self):
        # Frame
        table_frame = tk.Frame(self.right_frame)
        table_frame.pack(fill=tk.BOTH, expand=True, padx=10, pady=10)

        # Container + scrollbar dọc
        container = tk.Frame(table_frame)
        container.pack(fill=tk.BOTH, expand=True)

        # Treeview
        self.tree_data = ttk.Treeview(container, show="headings", selectmode="browse")
        self.tree_data.pack(side=tk.LEFT, fill=tk.BOTH, expand=True)

        # Scrollbar vertical
        vsb = ttk.Scrollbar(container, orient="vertical", command=self.tree_data.yview)
        vsb.pack(side=tk.RIGHT, fill=tk.Y)
        self.tree_data.configure(yscrollcommand=vsb.set)

        # Scrollbar horizontal
        hsb = ttk.Scrollbar(table_frame, orient="horizontal", command=self.tree_data.xview)
        hsb.pack(fill=tk.X, side=tk.BOTTOM)
        self.tree_data.configure(xscrollcommand=hsb.set)

        self.header_frame = tk.Frame(table_frame, bg="#1e1e1e")
        self.header_frame.pack(fill=tk.X, side=tk.TOP)

        self.header_labels = []

        # Theme
        style = ttk.Style()
        style.theme_use('superhero')  # 'vapor', 'cyborg'

        style.configure("Treeview",
                        background="#2b2b2b",
                        foreground="white",
                        rowheight=30,
                        fieldbackground="#2b2b2b",
                        font=("Consolas", 9),
                        borderwidth=1)

        style.configure("Treeview.Heading",
                        background="#1e1e1e",
                        foreground="#ffea8f",
                        font=("Consolas", 9, "bold"))

        style.map('Treeview', background=[('selected', '#0078d7')])

        # Tags for row colors
        self.tree_data.tag_configure("odd", background="#2d2d2d")
        self.tree_data.tag_configure("even", background="#1e1e1e")

    def display_table_data(self, columns, rows):
        for i in self.tree_data.get_children():
            self.tree_data.delete(i)
        for lbl in self.header_labels:
            lbl.destroy()
        self.header_labels.clear()

        if not columns or not rows:
            return

        self.tree_data["columns"] = columns
        col_widths = []
        for i, col_name in enumerate(columns):
            header_width = len(str(col_name)) * 9
            data_width = max((len(str(row[i])) for row in rows), default=0) * 8
            final_width = max(header_width, data_width, 100) + 20  # +padding
            col_widths.append(final_width)

            self.tree_data.heading(col_name, text=col_name)
            self.tree_data.column(col_name, width=final_width, minwidth=80, anchor="w", stretch=True)

        total_width = sum(col_widths)
        for i in range(len(columns)):
            weight = int(col_widths[i] / total_width * 100) if total_width > 0 else 1
            self.header_frame.grid_columnconfigure(i, weight=weight)

        for idx, row in enumerate(rows):
            tag = "even" if idx % 2 == 0 else "odd"
            display_values = [str(val) for val in row]
            self.tree_data.insert("", "end", values=display_values, tags=(tag,))

        def sync_header_width(event=None):
            for i, col in enumerate(columns):
                current_width = self.tree_data.column(col, "width")
                char_width = max(1, current_width // 9)
                self.header_labels[i].config(width=char_width)

        self.tree_data.bind("<Configure>", sync_header_width)
        self.root.update_idletasks()

    def refresh_project(self):
        if not self.current_project_path:
            messagebox.showwarning("No Project", "Please connect a project first!")
            return

        self.log("Refreshing project...", "INFO")
        self.btn_refresh.config(state="disabled")
        self.btn_refresh.config(text=" Refreshing...")
        
        self.tree.delete(*self.tree.get_children())
        self.tables.clear()
        
        self.root.after(100, lambda: self._do_refresh())

    def _do_refresh(self):
        CobolDataReader.scan_cobol_project(self, self.current_project_path)
        self.btn_refresh.config(state="normal")
        self.btn_refresh.config(text=" Refresh")
        self.lbl_project.config(text=f"Project: {Path(self.current_project_path).name}", fg="#88ff88")
        self.log("Refresh completed!", "OK")

    def connect_project(self):
        folder = filedialog.askdirectory(title="Select COBOL project")
        if not folder: return
        self.current_project_path = folder
        self.lbl_status.config(text=f"Scanning: {folder}", fg="blue")
        self.root.update()
        CobolDataReader.scan_cobol_project(self,folder)
        self.lbl_status.config(text=f"Connected: {Path(folder).name} ({len(self.tables)} table)", fg="green")
        
    def log(self, message, level="INFO"):
        import datetime
        ts = datetime.datetime.now().strftime("%H:%M:%S")
        colors = {"INFO":"#000000","OK":"#00FF00","WARN":"#FF8C00","ERROR":"#DC143C","RUN":"#0000FF","COMPILE":"#FF6A00"}
        prefix = {"INFO":"[INFO]","OK":"[OK]","WARN":"[WARN]","ERROR":"[ERROR]","RUN":"[RUN]","COMPILE":"[COMPILE]"}.get(level.upper(), "[INFO]")
        full = f"{ts} {prefix} {message}"
        print(full)
        if hasattr(self, 'data_text'):
            self.data_text.insert(tk.END, full + "\n", level)
            self.data_text.tag_config(level, foreground=colors.get(level.upper(), "#000000"))
            self.data_text.see(tk.END)
            self.root.update_idletasks()

    def on_table_selected(self, event):
        sel = self.tree.selection()
        if not sel:
            return
        info = self.tables.get(sel[0])
        if not info:
            return

        self.data_text.delete(1.0, tk.END)
        self.log(f"\n=== READING TABLE: {info['name']} ===")
        self.log(f"File: {info['dat_path']}")
        self.log(f"Copybook: {info['cpy_path']}\n")
        
        self.reader.generate_and_run_cobol_reader(
            info['dat_path'],
            info['cpy_path'],
            info['name']
        )

