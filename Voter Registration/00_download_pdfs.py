import os
import urllib.request

opener = urllib.request.build_opener()
opener.addheaders = [
    ("User-Agent", "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/122.0.0.0 Safari/537.36"),
    ("Referer",    "https://sos.iowa.gov/")
]
urllib.request.install_opener(opener)

# Base URL for Iowa SOS voter registration PDFs by county
BASE_URL = "https://sos.iowa.gov/elections/pdf/VRStatsArchive"

# Output directory for PDFs
output_dir = os.path.join(os.path.dirname(__file__), "data_pdf")
os.makedirs(output_dir, exist_ok=True)

# Month name abbreviations used in the filenames
months = [
    ("Jan", 1), ("Feb", 2), ("Mar", 3), ("Apr", 4),
    ("May", 5), ("Jun", 6), ("Jul", 7), ("Aug", 8),
    ("Sep", 9), ("Oct", 10), ("Nov", 11), ("Dec", 12)
]

# Year/month ranges to download (fill in gaps after existing data)
# Existing data ends at CoJan25.pdf; download Feb 2025 through Mar 2026
to_download = []

for month_name, month_num in months:
    if month_num >= 2:  # Feb 2025 onward
        to_download.append((2025, month_name, "25"))

to_download.append((2026, "Jan", "26"))
to_download.append((2026, "Feb", "26"))
to_download.append((2026, "Mar", "26"))

for year, month_name, yy in to_download:
    filename = f"Co{month_name}{yy}.pdf"
    out_path = os.path.join(output_dir, filename)

    if os.path.exists(out_path):
        print(f"Already exists, skipping: {filename}")
        continue

    url = f"{BASE_URL}/{year}/{filename}"
    try:
        urllib.request.urlretrieve(url, out_path)
        print(f"Downloaded: {filename}")
    except Exception as e:
        print(f"Failed: {filename} — {e}")
