**GEMINI API CACHING IMPLEMENTATION SPECIFICATION**

**PROBLEM**

OPP runs make 30+ API calls in Phase 1, each uploading identical PDF content (5-20MB). Same documents transmitted repeatedly with no benefit. This wastes time and money.

**SOLUTION**

Use Gemini's context caching: upload PDFs once, reference the cache in subsequent calls. Cached tokens cost 10% of regular tokens.

**SCOPE**

Modify Phase 1 functions only (lines 310-391 in OPP.py). Phase 2 and Phase 3 functions don't use PDFs and remain unchanged.

**CONSTRAINTS**

- Single file modification: OPP.py only
- No new dependencies: google-generativeai 0.8.5 already supports caching
- Graceful degradation required: if cache creation fails, continue with non-cached models
- Thread safety required: must work with existing 4-worker parallel execution
- No breaking changes to function signatures or external interfaces
- Cache must persist for entire run duration: minimum 2 hours, scale with iteration count

**CURRENT CODE STRUCTURE**

prepare_pdf_parts() (line 293):
- Encodes PDFs to base64
- Returns format compatible with Gemini API
- Stored in self.pdf_parts

Phase 1 functions (lines 310-391):
- extract_company_name (310): model_medium_temp, uploads PDFs
- generate_title_subtitle (331): model_medium_temp, uploads PDFs
- _generate_section (348): model_medium_temp, uploads PDFs
- _check_section_completeness (356): model_low_temp, uploads PDFs
- _enhance_section (365): model_medium_temp, uploads PDFs
- _enhance_section_for_density (379): model_medium_temp, uploads PDFs

All follow same pattern: model.generate_content(self.pdf_parts + [prompt])

run() method (line 779):
- Calls prepare_pdf_parts() once
- Executes Phase 1 functions (some parallel, some sequential)
- No existing cleanup logic in finally block

**REQUIREMENTS**

Cache Lifecycle:

1. After prepare_pdf_parts() completes in run(), create cache containing PDF content (one-time upload)
2. Create two model objects that both reference this same cache: one configured with temperature 0.2, one with temperature 0.6
3. Store cache and both cached models as instance variables accessible to all Phase 1 functions
4. Use cached models for ALL Phase 1 calls across ALL iterations (cache persists for entire run)
5. Delete cache when run() exits (success or failure)

Critical: PDFs are uploaded and model objects are created exactly once per run, regardless of iteration count. Cache and 2 model objects are created once before first Phase 1 call, reused for all subsequent Phase 1 calls.

Function Behaviour:

Phase 1 functions should use cached models when available. When using cached model, PDFs are already in context—don't pass them again. When cached model unavailable (creation failed), use regular models with PDFs.

Error Handling:

Cache creation failure must not crash the system. Log warning, set cache variables to None, continue execution. Every run must produce output whether caching works or not.

**IMPLEMENTATION NOTES**

Gemini Caching API:
- CachedContent.create() accepts contents in same format as prepare_pdf_parts() output
- GenerativeModel.from_cached_content() creates models with cache pre-loaded
- Cached models behave identically to regular models, just omit cached content from generate_content()
- Cache objects have delete() method for cleanup

Temperature Configuration:
- Low temperature model (0.2): used for _check_section_completeness
- Medium temperature model (0.6): used for all other Phase 1 functions
- Must match existing model configurations exactly

Thread Safety:
- Cache and 2 model objects are created in main thread before workers start
- Cached models are read-only, safe for concurrent access
- No locking required

**ACCEPTANCE CRITERIA**

Implementation succeeds when all of these hold:

Caching Functions:
- usage_metadata from Phase 1 API calls in all iterations shows cached_content_token_count > 0
- Logs show cache creation and cleanup messages
- No errors during cached API calls

Graceful Degradation:
- Test with invalid API key or network disconnection
- Cache creation fails with warning
- Run completes successfully using non-cached models
- Output generated normally

Output Quality:
- Run produces complete PowerPoint with all 4 sections
- No truncated or missing content
- Manual review passes

Performance Improvement:
- Record baseline metrics before implementation (time, cost, token counts)
- Record cached metrics after implementation
- Cached run costs less than baseline
- Cached run executes faster than baseline
- Document actual improvements achieved

**TESTING APPROACH**

Before Implementation:
- Run OPP with test PDF (1 iteration, 4 workers)
- Record total time, Phase 1 time, input tokens, estimated cost
- Save output for comparison

After Implementation:
- Run with same PDF and configuration
- Verify cache creation in logs
- Check usage_metadata for cached_content_token_count
- Record new metrics
- Calculate actual cost and time savings
- Run multi-iteration test (3 iterations, same PDF)
- Verify cache persists across all iterations
- Check usage_metadata shows cached_content_token_count > 0 in iterations 2 and 3

Edge Cases:
- Very small PDF (test minimum token threshold)
- Cache creation failure (simulate with network issues)

**ROLLBACK PLAN**

Implementation is in single file with minimal changes. Rollback is git restore OPP.py. Takes under 30 seconds.

Alternative: comment out cache creation call to disable without reverting code.

**KNOWN UNKNOWNS**

Cost reduction depends on PDF size and Gemini pricing. Cannot predict exact savings percentage. Measure baseline, measure cached, report actual improvement.

Output will differ between runs due to temperature > 0. Cannot test for identical outputs. Test for equivalent quality using existing review process.

Minimum token requirement for caching is unknown. May fail on very small PDFs. This is expected behaviour—graceful degradation handles it.
