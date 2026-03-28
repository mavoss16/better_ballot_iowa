import camelot
import os
import glob
import pandas as pd
import csv

pdf_dir = os.path.join(os.path.dirname(__file__), "data_pdf")
csv_dir = os.path.join(os.path.dirname(__file__), "data_csv")
os.makedirs(csv_dir, exist_ok=True)

for pdf_file in glob.glob(os.path.join(pdf_dir, "*.pdf")):
    base_filename = os.path.splitext(os.path.basename(pdf_file))[0]
    csv_path = os.path.join(csv_dir, f"{base_filename}.csv")

    if os.path.exists(csv_path):
        print(f"Already extracted, skipping: {base_filename}.pdf")
        continue

    tables = camelot.read_pdf(pdf_file, pages="all")

    if tables:
        combined_rows = []

        for i, table in enumerate(tables):
            rows = table.df.values.tolist()

            # Skip the first row in subsequent tables (duplicate header)
            if i > 0:
                rows = rows[1:]

            combined_rows.extend(rows)

        with open(csv_path, "w", newline="") as f:
            writer = csv.writer(f)
            writer.writerows(combined_rows)
        print(f"Written: {base_filename}.csv")
    else:
        print(f"No tables found: {base_filename}.pdf")
