# engine/reader.py
import subprocess
import tempfile
import shutil
from pathlib import Path
import os
from tkinter import ttk, filedialog, messagebox
class CobolDataReader:
    def scan_cobol_project(self, project_path):
        self.tables.clear()
        self.tree.delete(*self.tree.get_children())
        dat_files = [os.path.join(r, f) for r, _, fs in os.walk(project_path) for f in fs if f.lower().endswith(".dat")]
        found = 0
        for dat_path in dat_files:
            full_name = Path(dat_path).stem.upper()
            short_name = full_name.split("-")[0]

            cpy_candidates = [
                Path(project_path) / "shared" / f"{full_name}.cpy",
                Path(project_path) / "shared" / f"{full_name}.CPY",
                Path(project_path) / "shared" / f"{short_name}.cpy",
                Path(project_path) / f"{full_name}.cpy",
            ]
            cpy_path = None
            for p in cpy_candidates:
                if p.exists():
                    cpy_path = str(p)
                    break

            if not cpy_path:
                continue

            est = max(1, os.path.getsize(dat_path) // 300)
            item = self.tree.insert("", "end", text=f" {full_name}")
            self.tables[item] = {"name": full_name, "dat_path": dat_path, "cpy_path": cpy_path}
            found += 1
        if found == 0:
            self.log("NOT FOUND .dat + .cpy!")
        else:
            self.log("COMPLETE!", f"FOUND {found} COBOL table!")
            
    def log(self, message, level="INFO"):
        import datetime
        ts = datetime.datetime.now().strftime("%H:%M:%S")
        colors = {"INFO":"#000000","OK":"#D2FFD2","WARN":"#FF8C00","ERROR":"#FF0033","RUN":"#0000FF","COMPILE":"#FF6A00"}
        prefix = {"INFO":"[INFO]","OK":"[OK]","WARN":"[WARN]","ERROR":"[ERROR]","RUN":"[RUN]","COMPILE":"[COMPILE]"}.get(level.upper(), "[INFO]")
        full = f"{ts} {prefix} {message}"
        print(full)
        if hasattr(self, 'data_text'):
            self.data_text.insert(tk.END, full + "\n", level)
            self.data_text.tag_config(level, foreground=colors.get(level.upper(), "#000000"))
            self.data_text.see(tk.END)
            self.root.update_idletasks()

    def generate_and_run_cobol_reader(self, dat_path, cpy_path, table_name):
        self.log(f"Đang đọc bảng: {table_name}", "INFO")
        for item in self.tree_data.get_children():
            self.tree_data.delete(item)

        # 1. Lấy danh sách field
        with open(cpy_path, "r", encoding="utf-8-sig") as f:
            lines = [line.strip() for line in f if line.strip() and line.lstrip().startswith("05")]

        field_names = []
        for line in lines:
            parts = line.split()
            if len(parts) >= 2:
                field_names.append(parts[1].rstrip("."))

        if not field_names:
            self.log("Not found field in copybook!", "ERROR")
            return
        
        string_parts = []
        string_parts.append('           "ROW|"')
        string_parts.append("           WS-CNT")
        string_parts.append('           "|"')

        for field in field_names:
            string_parts.append(f"           {field}")
            if field != field_names[-1]:
                string_parts.append('           " | "')

        string_block = "\n".join(string_parts)

        cobol_template = f'''       IDENTIFICATION DIVISION.
       PROGRAM-ID. READ-{table_name}.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT DATA-FILE ASSIGN TO "{dat_path.replace("\\", "/")}"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS {field_names[0]}
               FILE STATUS IS WS-FS.

       DATA DIVISION.
       FILE SECTION.
       FD DATA-FILE.
           COPY "{Path(cpy_path).name}".

       WORKING-STORAGE SECTION.
       01 WS-FS           PIC XX.
           88 EOF         VALUE "10".
       01 WS-CNT          PIC 9(8) VALUE ZERO.
       01 WS-LINE         PIC X(3000).

       PROCEDURE DIVISION.
       MAIN.
           OPEN INPUT DATA-FILE
           IF WS-FS NOT = "00"
               DISPLAY "ERROR OPEN: " WS-FS
               STOP RUN
           END-IF

           PERFORM UNTIL EOF
               READ DATA-FILE
                   AT END SET EOF TO TRUE
                   NOT AT END
                       ADD 1 TO WS-CNT
                       STRING 
{string_block}
                           DELIMITED BY SIZE
                           INTO WS-LINE
                       END-STRING
                       DISPLAY FUNCTION TRIM(WS-LINE)
               END-READ
           END-PERFORM

           DISPLAY "TOTAL|" WS-CNT
           CLOSE DATA-FILE
           STOP RUN.
'''

        with tempfile.TemporaryDirectory() as tmpdir:
            src = os.path.join(tmpdir, f"READ-{table_name}.cob")
            exe = os.path.join(tmpdir, f"READ-{table_name}")
            shutil.copy2(cpy_path, tmpdir)

            with open(src, "w", encoding="utf-8", newline="\n") as f:
                f.write(cobol_template)

            self.log("compiling COBOL...", "COMPILE")
            comp = subprocess.run(
                ["cobc", "-x", "-free", src, "-o", exe],
                capture_output=True, text=True, cwd=tmpdir
            )
            if comp.returncode != 0:
                self.log(f"COMPILE ERROR:\n{comp.stderr}", "ERROR")
                return

            self.log("Running...", "RUN")
            run = subprocess.run([exe], capture_output=True, text=True)
            if run.stderr and "error" in run.stderr.lower():
                self.log(f"RUNTIME ERROR:\n{run.stderr}", "ERROR")
                return

            #read data from output
            rows = []
            total = 0
            for line in run.stdout.splitlines():
                line = line.strip()
                if line.startswith("ROW|"):
                    parts = line.split("|")[2:]
                    rows.append([p.strip() for p in parts])
                elif line.startswith("TOTAL|"):
                    try:
                        total = int(line.split("|")[1])
                    except:
                        total = len(rows)

            if not rows:
                self.log("empty data in file!", "WARN")
                return

            self.tree_data["columns"] = field_names
            self.tree_data.heading("#0", text="STT")
            self.tree_data.column("#0", width=60, anchor="center")

            for col in field_names:
                pretty = col.replace("CUST-", "").replace("-", " ").title()
                self.tree_data.heading(col, text=pretty)
                self.tree_data.column(col, width=180, anchor="w")

            for i, row in enumerate(rows, 1):
                values = row[:len(field_names)]
                tag = "even" if i % 2 == 0 else "odd"
                self.tree_data.insert("", "end", text=str(i), values=values, tags=(tag,))

            self.tree_data.tag_configure("even", background="#1e1e1e")
            self.tree_data.tag_configure("odd", background="#2d2d2d")
            self.display_table_data(field_names, rows)
            self.log(f"DISPLAYED {len(rows):,} SUCCESSFUL RECORD!", "OK")
            self.lbl_status.config(text=f"{table_name} • {total:,} RECORD")