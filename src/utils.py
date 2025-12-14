import time
import random
import re
import threading
import os
from concurrent.futures import ThreadPoolExecutor, TimeoutError as FuturesTimeoutError

# Thread-safe print lock
print_lock = threading.Lock()

# Timeout for API calls (seconds)
TIMEOUT_VALUE = 300

# Configurable global LLM concurrency (default 4). Bound to sane limits (1-16).
try:
    _max_inflight = int(os.environ.get("LLM_MAX_INFLIGHT", "4"))
    if _max_inflight < 1:
        _max_inflight = 1
    if _max_inflight > 16:
        _max_inflight = 16
except (ValueError, TypeError):
    # Invalid environment variable value, use default
    _max_inflight = 4

_llm_semaphore = threading.Semaphore(_max_inflight)

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


def validate_and_fix_tables(content: str) -> str:
    """Validate and fix tables to meet size constraints (max 10 columns, 20 rows).
    
    Args:
        content: Markdown content potentially containing tables
        
    Returns:
        Content with validated/fixed tables
    """
    lines = content.split('\n')
    fixed_lines = []
    in_table = False
    table_lines = []
    table_count = 0
    fixes_made = 0
    
    for i, line in enumerate(lines):
        # Check if this line is part of a table
        if '|' in line and (i == 0 or '|' in lines[i-1] or (i > 0 and re.match(r'^\s*\|[\s\-:|]+\|?\s*$', lines[i-1]))):
            if not in_table:
                in_table = True
                table_lines = []
            table_lines.append(line)
        else:
            # Not a table line
            if in_table:
                # Process the completed table
                fixed_table = _fix_single_table(table_lines)
                if fixed_table != table_lines:
                    fixes_made += 1
                fixed_lines.extend(fixed_table)
                table_lines = []
                in_table = False
                table_count += 1
            fixed_lines.append(line)
    
    # Handle table at end of content
    if in_table and table_lines:
        fixed_table = _fix_single_table(table_lines)
        if fixed_table != table_lines:
            fixes_made += 1
        fixed_lines.extend(fixed_table)
        table_count += 1

    # Silent table fixes - no output needed

    return '\n'.join(fixed_lines)


def _fix_single_table(table_lines: list) -> list:
    """Fix a single table to meet constraints (max 10 columns, 20 rows).
    
    Args:
        table_lines: List of lines forming a table
        
    Returns:
        Fixed table lines
    """
    if not table_lines:
        return table_lines
    
    # First, fix special characters in table cells
    fixed_lines = []
    for line in table_lines:
        if '|' in line and not re.match(r'^\s*\|[\s\-:|]+\|?\s*$', line):
            # This is a data or header row, not a separator
            # Split carefully to handle escaped pipes
            cells = []
            current_cell = []
            i = 0
            while i < len(line):
                if i < len(line) - 1 and line[i] == '\\' and line[i+1] == '|':
                    # Already escaped pipe, keep it
                    current_cell.append('\\|')
                    i += 2
                elif line[i] == '|':
                    # Cell boundary
                    cells.append(''.join(current_cell))
                    current_cell = []
                    i += 1
                else:
                    current_cell.append(line[i])
                    i += 1
            # Add the last cell
            if current_cell:
                cells.append(''.join(current_cell))
            
            # Now escape any remaining unescaped pipes within cells
            fixed_cells = []
            for j, cell in enumerate(cells):
                # Skip first and last empty cells from split
                if j == 0 or j == len(cells) - 1:
                    fixed_cells.append(cell)
                else:
                    # Look for unescaped pipes within the cell content
                    fixed_cell = re.sub(r'(?<!\\)\|', r'\\|', cell)
                    fixed_cells.append(fixed_cell)
            
            line = '|'.join(fixed_cells)
        
        fixed_lines.append(line)
    
    table_lines = fixed_lines
    
    # Check column count
    max_cols = max(line.count('|') - 1 for line in table_lines if '|' in line)
    
    # Fix excessive columns
    if max_cols > 10:
        thread_safe_print(f"Table has {max_cols} columns, reducing to 10")
        fixed_lines = []
        for line in table_lines:
            if '|' in line:
                cells = line.split('|')
                # Keep first 10 columns plus the empty cells at start/end
                if len(cells) > 12:  # Account for empty cells from split
                    cells = cells[:11] + ['... truncated']
                fixed_lines.append('|'.join(cells))
            else:
                fixed_lines.append(line)
        table_lines = fixed_lines
    
    # Fix excessive rows (excluding header and separator)
    data_rows = [line for line in table_lines if '|' in line and not re.match(r'^\s*\|[\s\-:|]+\|?\s*$', line)]
    if len(data_rows) > 22:  # 20 data rows + header + possible second header
        thread_safe_print(f"Table has {len(data_rows)-1} data rows, reducing to 20")
        # Keep header, separator, first 20 data rows
        header_lines = table_lines[:2] if len(table_lines) > 1 and re.match(r'^\s*\|[\s\-:|]+\|?\s*$', table_lines[1]) else table_lines[:1]
        kept_data = data_rows[1:21] if len(header_lines) == 2 else data_rows[:20]
        table_lines = header_lines + kept_data + ['| ... additional rows truncated ... |']
    
    return table_lines


# Configurable inter-call delay to prevent rate limit bursts
try:
    _inter_call_delay = float(os.environ.get("LLM_INTER_CALL_DELAY", "5.0"))
    if _inter_call_delay < 0:
        _inter_call_delay = 0
    if _inter_call_delay > 10:
        _inter_call_delay = 10
except (ValueError, TypeError):
    _inter_call_delay = 5.0

# Track last API call time for throttling
_last_call_time = 0.0
_last_call_lock = threading.Lock()


def retry_with_backoff(func, max_retries=5, base_delay=30.0, context="", timeout=TIMEOUT_VALUE):
    """Retry function with exponential backoff for rate limits and timeout protection

    Args:
        func: Function to execute
        max_retries: Maximum number of retry attempts (default 5)
        base_delay: Base delay for exponential backoff in seconds (default 30)
        context: Context string for logging
        timeout: Timeout for each function call (seconds, default 300)

    Rate limit handling:
        - Retries: 5 attempts with delays of ~30s, ~60s, ~120s, ~240s, then fail
        - Total max wait: ~450s before giving up
        - Inter-call delay: LLM_INTER_CALL_DELAY env var (default 5.0s)
    """
    global _last_call_time

    context_str = f"Section {context} - " if context else ""

    for attempt in range(max_retries):
        try:
            # Simple global gate to avoid burst concurrency
            _llm_semaphore.acquire()
            try:
                # Enforce inter-call delay to prevent bursts
                with _last_call_lock:
                    elapsed = time.time() - _last_call_time
                    if elapsed < _inter_call_delay:
                        time.sleep(_inter_call_delay - elapsed)

                # Execute function with timeout protection
                with ThreadPoolExecutor(max_workers=1) as executor:
                    future = executor.submit(func)
                    result = future.result(timeout=timeout)

                # Update last call time after successful call
                with _last_call_lock:
                    _last_call_time = time.time()

                return result
            finally:
                _llm_semaphore.release()

        except FuturesTimeoutError:
            # Treat timeout like rate limit - retry with backoff
            if attempt < max_retries - 1:
                delay = base_delay * (2 ** attempt) + random.uniform(0, 2)
                thread_safe_print(f"{context_str}Timeout after {timeout}s, retrying in {delay:.1f}s (attempt {attempt + 1}/{max_retries})")
                time.sleep(delay)
                continue
            else:
                thread_safe_print(f"{context_str}Max retries exceeded after timeout")
                raise

        except Exception as e:
            error_str = str(e)
            if "429" in error_str or "quota" in error_str.lower() or "rate" in error_str.lower() or "resource" in error_str.lower():
                if attempt < max_retries - 1:
                    # Calculate delay with exponential backoff
                    delay = base_delay * (2 ** attempt) + random.uniform(0, 5)

                    # Extract retry delay from error if available (Gemini sometimes provides this)
                    if "retry_delay" in error_str or "Retry-After" in error_str:
                        try:
                            delay_match = re.search(r'seconds?[:\s]+(\d+)', error_str, re.IGNORECASE)
                            if delay_match:
                                server_delay = int(delay_match.group(1))
                                delay = max(delay, server_delay + random.uniform(1, 5))
                        except (ValueError, AttributeError):
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