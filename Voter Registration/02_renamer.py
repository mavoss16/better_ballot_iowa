import os
import re

month_mapping = {
    'January': '01', 'February': '02', 'March': '03', 'April': '04',
    'May': '05', 'June': '06', 'July': '07', 'August': '08', 'Sept': '09',
    'September': '09', 'October': '10', 'November': '11', 'December': '12',
    'Jan': '01', 'Feb': '02', 'Mar': '03', 'Apr': '04',
    'Jun': '06', 'Jul': '07', 'Aug': '08',
    'Sep': '09', 'Oct': '10', 'Nov': '11', 'Dec': '12'
}

directory = os.path.join(os.path.dirname(__file__), "data_csv")

pattern = re.compile(r'Co([A-Za-z]+)(\d{2})\.csv')

for filename in os.listdir(directory):
    match = pattern.match(filename)
    if match:
        month_name, year = match.groups()
        month_number = month_mapping.get(month_name)
        if month_number:
            new_filename = f'20{year}-{month_number}.csv'
            os.rename(
                os.path.join(directory, filename),
                os.path.join(directory, new_filename)
            )
            print(f'Renamed: {filename} -> {new_filename}')
        else:
            print(f'Month name not found for {filename}')
