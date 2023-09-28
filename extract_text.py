from pypdf import PdfReader
import os

for pdf_name in os.listdir("./Papers/"):
    reader = PdfReader(f"Papers/{pdf_name}")
    text = ""
    for page in reader.pages:
        text += page.extract_text() + "\n"

    pdf_name=pdf_name.split('.pdf')[0]
    with open(f'new_data/{pdf_name}.txt', 'w') as f:
        f.write(text)