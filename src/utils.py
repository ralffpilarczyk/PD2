import time
import random
import re
import threading

# Thread-safe print lock
print_lock = threading.Lock()

def thread_safe_print(*args, **kwargs):
    """Thread-safe print function"""
    with print_lock:
        print(*args, **kwargs)


def clean_markdown_tables(content: str) -> str:
    """Clean corrupted markdown tables from PDF conversion.
    
    Fixes common issues from Marker library:
    - Table separator rows with excessive dashes/colons
    - Table cells with excessive content (>1000 chars)
    - Malformed table structure
    
    Args:
        content: Raw markdown content
        
    Returns:
        Cleaned markdown content
    """
    import re
    
    lines = content.split('\n')
    cleaned_lines = []
    table_active = False
    corruption_count = 0
    
    for i, line in enumerate(lines):
        # Check if line might be a table row
        if '|' in line:
            # Detect table separator rows with excessive characters
            if re.match(r'^\s*\|[\s\-:|\|]{50,}\s*$', line):
                # Count columns from previous or next line
                columns = 2  # default
                if i > 0 and '|' in lines[i-1]:
                    columns = lines[i-1].count('|') - 1
                elif i < len(lines)-1 and '|' in lines[i+1]:
                    columns = lines[i+1].count('|') - 1
                
                # Create proper separator
                if columns > 0:
                    cleaned_line = '|' + '---|' * columns
                    cleaned_lines.append(cleaned_line)
                    table_active = True
                    corruption_count += 1
                    continue
            
            # Check for excessively long table cells
            if len(line) > 1000:
                # Try to fix malformed table rows
                cells = line.split('|')
                cleaned_cells = []
                
                for cell in cells:
                    if len(cell) > 500:
                        # Truncate long cells, preserve first part
                        cleaned_cell = cell[:200] + '... [truncated]'
                        cleaned_cells.append(cleaned_cell)
                        corruption_count += 1
                    else:
                        cleaned_cells.append(cell)
                
                cleaned_line = '|'.join(cleaned_cells)
                cleaned_lines.append(cleaned_line)
                continue
        
        # Normal line
        cleaned_lines.append(line)
    
    if corruption_count > 0:
        thread_safe_print(f"Fixed {corruption_count} corrupted table elements")
    
    return '\n'.join(cleaned_lines)


def retry_with_backoff(func, max_retries=3, base_delay=1.0, context=""):
    """Retry function with exponential backoff for rate limits"""
    context_str = f"Section {context} - " if context else ""
    for attempt in range(max_retries):
        try:
            return func()
        except Exception as e:
            error_str = str(e)
            if "429" in error_str or "quota" in error_str.lower() or "rate" in error_str.lower():
                if attempt < max_retries - 1:
                    # Extract retry delay from error if available
                    delay = base_delay * (2 ** attempt) + random.uniform(0, 1)
                    if "retry_delay" in error_str:
                        try:
                            delay_match = re.search(r'seconds:\s*(\d+)', error_str)
                            if delay_match:
                                delay = max(delay, int(delay_match.group(1)))
                        except:
                            pass
                    
                    thread_safe_print(f"{context_str}Rate limit hit, retrying in {delay:.1f}s (attempt {attempt + 1}/{max_retries})")
                    time.sleep(delay)
                    continue
                else:
                    thread_safe_print(f"{context_str}Max retries exceeded for rate limit")
                    raise
            else:
                # Non-rate-limit error, don't retry
                raise
    return None 