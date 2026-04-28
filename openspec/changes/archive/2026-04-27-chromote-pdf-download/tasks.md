## 1. Create init_chromote() Function

- [x] 1.1 Create `init_chromote(username, password, session_name, db)` function in app.R
- [x] 1.2 Implement login via JavaScript (similar to get_session_names())
- [x] 1.3 Navigate to session URL after login
- [x] 1.4 Return Chromote session object with main_url
- [x] 1.5 Add on.exit() for session cleanup
- [x] 1.6 Wrap in tryCatch() for error handling

## 2. Create download_pdf_chromote() Function

- [x] 2.1 Create `download_pdf_chromote(chrome, url, outname)` function in app.R
- [x] 2.2 Navigate to URL using `b$go_to()`
- [x] 2.3 Extract page source using `b$get_page_source()`
- [x] 2.4 Parse with rvest to find `#pdfLink`
- [x] 2.5 Extract href and convert to absolute URL
- [x] 2.6 Navigate to PDF link page and find download URL
- [x] 2.7 Download PDF using httr::GET()
- [x] 2.8 Add error handling with tryCatch()

## 3. Update get_session_names() for Session Reuse

- [x] 3.1 Modify `get_session_names()` to return list with session_names AND Chromote session object
- [x] 3.2 Update return format: `list(names = session_names, chrome = chrome_session)`
- [x] 3.3 Add cleanup logic when session is closed

## 4. Update Shiny Server Integration

- [x] 4.1 Store Chromote session in reactive value after session discovery
- [x] 4.2 Update server to pass stored session to init_chromote() or use from get_session_names()
- [x] 4.3 Modify download loop to use download_pdf_chromote()
- [x] 4.4 Add session cleanup after batch completes
- [x] 4.5 Handle progress updates similar to current implementation

## 5. Update CLI Pipeline

- [x] 5.1 Update download_ucsc_pdf_pipeline.R with init_chromote()
- [x] 5.2 Update download_pdf() to download_pdf_chromote()
- [x] 5.3 Update init_local() to use Chromote approach
- [x] 5.4 Ensure batch download loop works correctly

## 6. Remove Legacy Functions

- [x] 6.1 Test new functions thoroughly
- [x] 6.2 Comment out or remove old `init()` function
- [x] 6.3 Comment out or remove old `download_pdf()` function
- [x] 6.4 Update function references in server code

## 7. Testing and Documentation

- [x] 7.1 Test PDF download with test credentials (ucsc_login/ucsc_password)
- [x] 7.2 Test batch download (multiple coordinates)
- [x] 7.3 Verify session persistence across downloads
- [x] 7.4 Test error handling (invalid session, network failure)
- [x] 7.5 Update AGENTS.md with new function documentation
- [x] 7.6 Update README.org with Chromote-based workflow
