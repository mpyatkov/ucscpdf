## Why

Currently, PDF downloads use `rvest` for HTTP sessions, but login to UCSC Genome Browser fails due to Cloudflare CAPTCHA protection. The session discovery feature already uses Chromote (headless Chrome) to bypass CAPTCHA, but the PDF download still uses the legacy rvest session which creates a conflict - two different session types that can't share authentication state.

## What Changes

- Replace `init()` function with `init_chromote()` - creates Chromote session, logs in via JavaScript
- Replace `download_pdf()` with `download_pdf_chromote()` - uses Chromote for navigation and PDF extraction
- Remove rvest dependency from PDF download workflow
- Update Shiny server to use new Chromote-based functions
- Update CLI pipeline (`download_ucsc_pdf_pipeline.R`) to use new functions

## Capabilities

### New Capabilities

- **chromote-pdf-download**: Complete Chromote-based PDF download workflow replacing rvest
  - `init_chromote()`: Authenticate via Chromote using JavaScript form submission
  - `download_pdf_chromote()`: Navigate UCSC, extract PDF link, download file using headless Chrome

### Modified Capabilities

- **session-discovery**: Existing capability continues to work; integration point changed (now shares Chromote session type)

## Impact

- **Code**: `app.R` - refactor `init()` and `download_pdf()` functions
- **Code**: `download_ucsc_pdf_pipeline.R` - refactor to use Chromote functions
- **Dependencies**: Removes rvest requirement for PDF workflow (rvest still used for HTML parsing only)
- **Breaking**: Changes function signatures - old `init()` and `download_pdf()` replaced