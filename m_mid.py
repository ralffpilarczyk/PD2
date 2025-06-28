import os
import sys
from pathlib import Path
from tkinter import filedialog, messagebox, Tk

def select_pdf_file():
    """Open file dialog to select PDF from SourceData folder"""
    source_dir = Path("SourceData")
    if not source_dir.exists():
        messagebox.showerror("Error", "SourceData folder not found!")
        return None
    
    root = Tk()
    root.withdraw()  # Hide main window
    
    file_path = filedialog.askopenfilename(
        title="Select PDF file to convert",
        initialdir=source_dir,
        filetypes=[("PDF files", "*.pdf"), ("All files", "*.*")]
    )
    
    root.destroy()
    return file_path if file_path else None

def convert_with_marker(pdf_path, output_dir):
    """Convert PDF to markdown using Marker"""
    try:
        from marker.converters.pdf import PdfConverter
        from marker.models import create_model_dict
        from marker.output import text_from_rendered
        
        # Create converter
        converter = PdfConverter(
            artifact_dict=create_model_dict(),
        )
        
        # Convert PDF
        rendered = converter(pdf_path)
        full_text, _, images = text_from_rendered(rendered)
        
        # Save output
        pdf_name = Path(pdf_path).stem
        output_path = output_dir / f"{pdf_name}_m.md"
        
        with open(output_path, 'w', encoding='utf-8') as f:
            f.write(full_text)
        
        print(f"Marker conversion saved to: {output_path}")
        return True
        
    except Exception as e:
        print(f"Marker conversion failed: {e}")
        return False

def convert_with_markitdown(pdf_path, output_dir):
    """Convert PDF to markdown using MarkItDown"""
    try:
        from markitdown import MarkItDown
        
        md = MarkItDown()
        result = md.convert(pdf_path)
        
        # Save output
        pdf_name = Path(pdf_path).stem
        output_path = output_dir / f"{pdf_name}_mid.md"
        
        with open(output_path, 'w', encoding='utf-8') as f:
            f.write(result.text_content)
        
        print(f"MarkItDown conversion saved to: {output_path}")
        return True
        
    except Exception as e:
        print(f"MarkItDown conversion failed: {e}")
        return False

def main():
    """Main workflow"""
    # Ensure output directory exists
    output_dir = Path("OutputData")
    output_dir.mkdir(exist_ok=True)
    
    # Step 1: Select PDF file
    pdf_path = select_pdf_file()
    if not pdf_path:
        print("No file selected. Exiting.")
        return
    
    print(f"Selected file: {pdf_path}")
    
    # Step 2: Convert with Marker
    print("Converting with Marker...")
    marker_success = convert_with_marker(pdf_path, output_dir)
    
    # Step 3: Convert with MarkItDown
    print("Converting with MarkItDown...")
    markitdown_success = convert_with_markitdown(pdf_path, output_dir)
    
    # Summary
    if marker_success and markitdown_success:
        print("Both conversions completed successfully!")
    elif marker_success or markitdown_success:
        print("One conversion completed successfully.")
    else:
        print("Both conversions failed.")

if __name__ == "__main__":
    main()
