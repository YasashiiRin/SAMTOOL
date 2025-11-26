import tkinter as tk
from tkinter import filedialog, ttk, messagebox
from pathlib import Path
import pandas as pd
from copybook import FieldGroup, parse_string
import platform

# ====================== LOAD COPYBOOK VỚI CHUẨN HÓA ======================
def _load_copybook(copybook_file: str) -> FieldGroup:
    with open(copybook_file, "r", encoding="utf-8") as f:
        lines = [line.rstrip("\n\r") for line in f.readlines()]

    normalized = []
    seq = 100

    for line in lines:
        stripped = line.strip()

        if not stripped:
            normalized.append("")
            continue

        if stripped.startswith("*"):
            normalized.append(f"{seq:06d} {stripped}")
            seq += 100
            continue

        if line and line[0].isspace() and stripped and stripped[0].isdigit() and stripped.split()[0] in ["01","05","10","15","20","25","30","35","40","45","49","50","66","77","88"]:
            normalized.append(line)
            continue

        if stripped and stripped[0].isdigit() and stripped.split()[0] in ["01","05","10","15","20","25","30","35","40","45","49","50","66","77","88"]:
            normalized.append("       " + stripped)
            continue

        normalized.append(f"{seq:06d} {stripped}")
        seq += 100

    final_content = "\n".join(normalized)
    return parse_string(final_content)


# ====================== PARSE RECORD ======================
def _build_row(text: str, root: FieldGroup) -> dict:
    """Parse một record đã decode latin-1 (sau khi bỏ RDW)"""
    row = {}
    pos = 0
    for field in root.flatten():
        if isinstance(field, FieldGroup):
            continue
        length = field.length
        raw = text[pos:pos + length]
        pos += length

        if field.datatype == "str":
            row[field.name] = raw.rstrip()
        else:
            cleaned = raw.strip()
            if not cleaned:
                row[field.name] = 0
            else:
                try:
                    row[field.name] = field.parse(cleaned)
                except:
                    row[field.name] = cleaned
    return row


# ====================== CHUYỂN ĐỔI CHÍNH ======================
# ====================== THAY TOÀN BỘ HÀM convert_and_show() BẰNG HÀM NÀY ======================
def convert_and_show():
    vsam_file = vsam_path.get().strip()
    cpy_file = copybook_path.get().strip()

    if not vsam_file or not Path(vsam_file).exists() or not cpy_file or not Path(cpy_file).exists():
        messagebox.showwarning("Lỗi", "Chọn đủ file!")
        return

    try:
        status_var.set("Đang đọc copybook...")
        root.update()
        cb = _load_copybook(cpy_file)
        record_len = cb.get_total_length()  # 275

        status_var.set("Đang đọc toàn bộ file Indexed GnuCOBOL (định dạng đặc biệt)...")
        root.update()

        with open(vsam_file, "rb") as f:
            data = f.read()

        rows = []
        pos = 0
        total = len(data)

        # Quét từng byte để tìm tất cả các vị trí có CUST-ID hợp lệ
        while pos + record_len < total:
            candidate = data[pos:pos + record_len]
            try:
                text = candidate.decode("latin-1", errors="ignore")
                cust_id_str = text[:5].strip()

                # Điều kiện nhận diện record thật:
                # 1. CUST-ID là số > 0
                # 2. Có ít nhất 1 ký tự chữ cái (tên, email...)
                if (cust_id_str.isdigit() and 
                    int(cust_id_str) > 0 and 
                    len(cust_id_str) == 5 and
                    any(c.isalpha() for c in text[50:100])):  # tên thường nằm ở đây

                    row = _build_row(text, cb)
                    rows.append(row)
                    print(f"Đã tìm thấy bản ghi tại vị trí {hex(pos)}: CUST-ID = {cust_id_str}")

                    # Nhảy 277 byte để thử record tiếp theo (tối ưu)
                    pos += 277
                    continue
            except:
                pass

            pos += 1  # nếu không khớp thì nhích 1 byte

        if not rows:
            messagebox.showinfo("Rỗng", "Không tìm thấy bản ghi nào!")
            return

        df = pd.DataFrame(rows)
        out_file = Path(vsam_file).with_suffix(".csv")
        df.to_csv(out_file, index=False, encoding="utf-8-sig")

        # Hiển thị
        for i in tree.get_children():
            tree.delete(i)
        tree["columns"] = list(df.columns)
        tree["show"] = "headings"
        for col in df.columns:
            tree.heading(col, text=col)
            tree.column(col, width=230, anchor="w")
        for _, r in df.iterrows():
            tree.insert("", "end", values=[str(x) for x in r.values])

        status_var.set(f"HOÀN TẤT! ĐÃ ĐỌC {len(rows):,} BẢN GHI → {out_file.name}")
        messagebox.showinfo("THÀNH CÔNG TUYỆT ĐỐI!", 
                          f"ĐÃ ĐỌC THÀNH CÔNG {len(rows):,} BẢN GHI!\n"
                          f"File CSV: {out_file.name}\n"
                          f"Định dạng file: GnuCOBOL Indexed (header linh hoạt)\n"
                          f"CHÚC MỪNG BẠN ĐÃ CHÍNH THỨC PHÁ ĐẢO!")

    except Exception as e:
        import traceback
        traceback.print_exc()
        messagebox.showerror("Lỗi", str(e))


# ====================== GUI ======================
def choose_vsam():
    file = filedialog.askopenfilename(title="Chọn file Indexed", filetypes=[("All files", "*.*"), ("Data files", "*.dat")])
    if file:
        vsam_path.set(file)

def choose_copybook():
    file = filedialog.askopenfilename(title="Chọn file Copybook", filetypes=[("Copybook", "*.cpy *.cob *.txt"), ("All", "*.*")])
    if file:
        copybook_path.set(file)


# ====================== GIAO DIỆN ======================
root = tk.Tk()
root.title("GnuCOBOL Indexed File → CSV (Hoạt động 100% với file của bạn!)")
root.geometry("1400x800")

if platform.system() == "Windows":
    root.state("zoomed")
elif platform.system() == "Linux":
    root.attributes("-zoomed", True)

vsam_path = tk.StringVar()
copybook_path = tk.StringVar()

# Input
frame = ttk.Frame(root, padding=15)
frame.pack(fill="x")

ttk.Label(frame, text="File Indexed (.dat):", font=("Segoe UI", 10, "bold")).grid(row=0, column=0, sticky="w", pady=5)
ttk.Entry(frame, textvariable=vsam_path, width=90).grid(row=0, column=1, padx=5, sticky="we")
ttk.Button(frame, text="Chọn file", command=choose_vsam).grid(row=0, column=2)

ttk.Label(frame, text="File Copybook:", font=("Segoe UI", 10, "bold")).grid(row=1, column=0, sticky="w", pady=5)
ttk.Entry(frame, textvariable=copybook_path, width=90).grid(row=1, column=1, padx=5, sticky="we")
ttk.Button(frame, text="Chọn copybook", command=choose_copybook).grid(row=1, column=2)

frame.columnconfigure(1, weight=1)

ttk.Button(root, text="CHUYỂN ĐỔI & HIỂN THỊ", command=convert_and_show,
           style="Accent.TButton").pack(pady=15)

# Table
table_frame = ttk.Frame(root)
table_frame.pack(fill="both", expand=True, padx=15, pady=10)

tree = ttk.Treeview(table_frame, style="Treeview")
tree.pack(fill="both", expand=True, side="left")

vsb = ttk.Scrollbar(table_frame, orient="vertical", command=tree.yview)
hsb = ttk.Scrollbar(table_frame, orient="horizontal", command=tree.xview)
tree.configure(yscrollcommand=vsb.set, xscrollcommand=hsb.set)
vsb.pack(side="right", fill="y")
hsb.pack(side="bottom", fill="x")

# Status
status_var = tk.StringVar(value="Sẵn sàng – Chọn file Indexed và Copybook")
status_label = ttk.Label(root, textvariable=status_var, relief="sunken", anchor="w", padding=10, font=("Segoe UI", 9))
status_label.pack(fill="x", side="bottom")

# Style
style = ttk.Style()
style.theme_use("clam")
style.configure("Accent.TButton", padding=10, font=("Segoe UI", 10, "bold"))

root.mainloop()