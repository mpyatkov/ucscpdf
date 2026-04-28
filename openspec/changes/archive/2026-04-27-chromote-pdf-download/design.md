## Context

Current PDF download workflow uses `rvest` for HTTP session management:

1. `init(login, password, session_name, db)` creates an rvest session via `session()` and performs login
2. `download_pdf(session, url, outname)` uses rvest methods:
   - `session_jump_to(url)` - navigate
   - `read_html()` - get page content
   - `html_nodes("#pdfLink")` - find PDF link
   - `download.file()` - download the PDF

The rvest approach fails because UCSC has Cloudflare CAPTCHA protection that blocks programmatic login. Chromote (headless Chrome) is already used for session discovery and works around this issue.

**Current Architecture:**
```
app.R: init() + download_pdf()  →  rvest session  →  FAILS at login
app.R: get_session_names()       →  Chromote      →  WORKS
```

## Goals / Non-Goals

**Goals:**
- Use Chromote for both session discovery AND PDF download (unified approach)
- Bypass Cloudflare CAPTCHA for reliable login
- Maintain same user-facing functionality (login, select session, download PDFs)
- Single session for entire workflow (login once, download multiple PDFs)

**Non-Goals:**
- Keep rvest as a fallback (this change removes rvest entirely for auth)
- Support non-Chromium browsers for PDF download
- Implement session caching/pooling for multi-user scenarios

## Decisions

### 1. Unified Chromote Session

**Decision:** Use the same Chromote session for both session discovery AND PDF download

**Rationale:**
- Reusing the session after session discovery keeps user logged in
- Avoids re-authenticating for each PDF download
- Single browser instance reduces resource usage

**Alternative considered:** Create new Chromote session per download - Rejected: slower, more resource-intensive

### 2. Session Lifecycle

**Decision:** Pass the Chromote session object through the workflow instead of recreating

- `get_session_names()` returns session names + Chromote session object
- Server stores Chromote session in reactive value
- `download_pdf_chromote()` uses stored session

**Alternative considered:** Close and recreate session - Rejected: loses login state, slower

### 3. PDF Download Method

**Decision:** Use direct URL download via `httr::GET()` after extracting URL from page

Rationale:
- Chromote's `Page.downloadResource` requires setup
- Already extracting PDF URL from page - can use direct HTTP download
- rvest's `download.file()` works for actual file download

**Code pattern:**
```r
# Get PDF URL via Chromote DOM
pdf_url <- b$Runtime$evaluate("document.querySelector('#pdfLink').href")
url <- pdf_url$result$value

# Download via httr (not Chromote)
response <- GET(url, write_disk(outname))
```

**Alternative considered:** Use Chromote download API - More complex, no benefit

### 4. Navigation Method

**Decision:** Use `b$go_to(url)` for navigation, `b$get_page_source()` for HTML extraction

**Rationale:**
- `go_to()` handles redirects and waiting
- `get_page_source()` returns clean HTML for parsing with rvest

### 5. HTML Parsing

**Decision:** Keep rvest for HTML parsing (not navigation)

- Extract page source via Chromote: `b$get_page_source()`
- Parse with rvest: `read_html()` → `html_nodes()` → etc.

Rationale: rvest provides better R idioms for element selection and extraction

## Risks / Trade-offs

| Risk | Severity | Mitigation |
|------|----------|-----------|
| Chromote session instability | Medium | Add tryCatch with session recreation on failure |
| Memory leaks from long-lived session | Medium | Close session after all downloads complete |
| PDF download timeout | Low | Increase timeout, add retry logic |
| Browser crash during download | Low | Wrap each download in tryCatch, log error and continue |

## Migration Plan

1. Add `init_chromote()` function (line ~255 in app.R)
2. Add `download_pdf_chromote()` function (line ~141 in app.R)
3. Update `get_session_names()` to return Chromote session object
4. Update Shiny server to store and pass Chromote session
5. Update CLI pipeline (`download_ucsc_pdf_pipeline.R`)
6. Remove legacy `init()` and `download_pdf()` after testing
7. Update AGENTS.md documentation

## Open Questions

- **Q:** Should session be kept alive between batch downloads? 
  - **A:** Yes - reuse same session for all PDFs in a batch (per current design)

- **Q:** How to handle failed downloads in batch?
  - **A:** Log error, continue to next coordinate, report failures at end

- **Q:** Close browser after batch or keep open for session persistence?
  - **A:** Close after batch completes - ensures clean state