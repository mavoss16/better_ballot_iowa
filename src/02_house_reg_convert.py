

import camelot
import os
import glob
import pandas as pd

# Directory containing the PDF files
files = glob.glob("/Users/mavos/Documents/GitHub/better_ballot_iowa/data/house_district_reg/pdfs/*/*.pdf", recursive = True)



# Loop over all PDF files in the directory
for pdf_file in files:
    print(pdf_file)
    # Extract tables from the PDF file
    tables = camelot.read_pdf(pdf_file, pages='all')

    # Check if tables are found
    if tables:
        # Initialize an empty list to store rows
        combined_rows = []

        # Iterate through all tables extracted from the pages
        for i, table in enumerate(tables):
            # Extract the table as a list of rows
            rows = table.df.values.tolist()

            # Skip the first row if it's a duplicate of the column headers
            if i > 0:
                # The first row in all subsequent tables is the header
                rows = rows[1:]

            # Add the rows to the combined list
            combined_rows.extend(rows)

        # Get the base name of the PDF file (without extension)
        base_filename = os.path.splitext(os.path.basename(pdf_file))[0]

        # Save the combined DataFrame to a single CSV file
        csv_filename = f'/Users/mavos/Documents/GitHub/better_ballot_iowa/data/house_district_reg/csvs/{base_filename}.csv'
        import csv
        with open(csv_filename, 'w', newline='') as f:
            writer = csv.writer(f)
            writer.writerows(combined_rows)
        print(f'All tables from {base_filename} written to {csv_filename}')
    else:
        print("No tables found in the PDF.")
    

