#!/usr/bin/env python3

from generic_extractor import GenericExtractor
from pathlib import Path

# Load test document
test_doc = Path('test_docs/test_novel.md')
with open(test_doc, 'r') as f:
    text = f.read()

print("Text contains 'first-mover':", 'first-mover' in text.lower())

# Create extractor and extract
extractor = GenericExtractor()
facts = extractor.extract(text)

print(f"\nExtracted {len(facts)} facts:")
for fact in facts:
    print(f"  - {fact['property']}")

# Check if first_mover_advantage patterns match
prop_config = extractor.properties['first_mover_advantage']
patterns = extractor.compiled_patterns['first_mover_advantage']

print(f"\nChecking first_mover_advantage patterns:")
for pattern in patterns:
    matches = list(pattern.finditer(text.lower()))
    if matches:
        print(f"  Pattern '{pattern.pattern}' found {len(matches)} matches")
        for m in matches:
            print(f"    - Position {m.start()}: '{m.group()}'")