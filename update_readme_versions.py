#!/usr/bin/env python3
"""
Update README.md with current version numbers from source files
"""

from pathlib import Path
import re


def get_pd2_version():
    """Extract PD2 version from src/__init__.py"""
    init_path = Path("src/__init__.py")
    with open(init_path, 'r') as f:
        for line in f:
            if line.startswith('__version__'):
                return line.split('=')[1].strip().strip('"').strip("'")
    return "2.1"


def get_opp_version():
    """Extract OPP version from OPP.py"""
    opp_path = Path("OPP.py")
    with open(opp_path, 'r') as f:
        for line in f:
            if line.startswith('__opp_version__'):
                return line.split('=')[1].strip().strip('"').strip("'")
    return "1.0"


def update_readme():
    """Update README.md with current version numbers"""
    readme_path = Path("README.md")

    pd2_version = get_pd2_version()
    opp_version = get_opp_version()

    with open(readme_path, 'r') as f:
        content = f.read()

    # Update title line
    content = re.sub(
        r'# ProfileDash \d+\.\d+ & OnePageProfile',
        f'# ProfileDash {pd2_version} & OnePageProfile',
        content
    )

    # Update PD2 description
    content = re.sub(
        r'\*\*PD2\*\* \(ProfileDash \d+\.\d+\)',
        f'**PD2** (ProfileDash {pd2_version})',
        content
    )

    # Update OPP description in intro
    content = re.sub(
        r'\*\*OPP\*\* \(OnePageProfile\):',
        f'**OPP** (OnePageProfile v{opp_version}):',
        content
    )

    # Update PD2 section header
    content = re.sub(
        r'## ProfileDash \d+\.\d+ \(PD2\)',
        f'## ProfileDash {pd2_version} (PD2)',
        content
    )

    # Update OPP section header
    content = re.sub(
        r'## OnePageProfile \(OPP\)',
        f'## OnePageProfile v{opp_version} (OPP)',
        content
    )

    with open(readme_path, 'w') as f:
        f.write(content)

    print(f"✓ README.md updated")
    print(f"  - PD2 version: {pd2_version}")
    print(f"  - OPP version: {opp_version}")


if __name__ == "__main__":
    update_readme()
