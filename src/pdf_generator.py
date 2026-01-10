from pathlib import Path
from typing import Optional
from .utils import thread_safe_print


def generate_pdf_from_html(html_path: str, pdf_output_dir: str = "ReportsPD2") -> Optional[str]:
    """Generate PDF from HTML file using WeasyPrint.

    Converts an HTML file to PDF format, saving it to the specified output directory
    with the same filename but .pdf extension.

    Args:
        html_path: Path to the HTML file to convert
        pdf_output_dir: Directory to save the PDF (default: ReportsPD2)

    Returns:
        Path to the generated PDF file if successful, None if conversion failed

    Example:
        >>> pdf_path = generate_pdf_from_html("runs/run_2025_01_08/Company_250108_1234.html")
        >>> print(pdf_path)
        ReportsPD2/Company_250108_1234.pdf
    """
    try:
        from weasyprint import HTML

        # Convert path to Path object for easier manipulation
        html_file = Path(html_path)

        # Check if HTML file exists
        if not html_file.exists():
            thread_safe_print(f"Error: HTML file not found: {html_path}")
            return None

        # Create output directory if it doesn't exist
        output_dir = Path(pdf_output_dir)
        output_dir.mkdir(parents=True, exist_ok=True)

        # Create PDF path in output directory with same filename
        pdf_path = output_dir / html_file.with_suffix('.pdf').name

        # Generate PDF from HTML file
        HTML(filename=str(html_file)).write_pdf(str(pdf_path))

        return str(pdf_path)

    except ImportError:
        thread_safe_print("Warning: WeasyPrint not installed. PDF generation skipped.")
        thread_safe_print("To enable PDF generation, install with: pip install weasyprint")
        return None

    except Exception as e:
        thread_safe_print(f"Warning: PDF generation failed: {e}")
        thread_safe_print("HTML report is still available.")
        return None
