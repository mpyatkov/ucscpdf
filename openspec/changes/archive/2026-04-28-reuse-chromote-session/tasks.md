## 1. Modify app.R - Add reactiveValues for Chromote session

- [x] 1.1 Add `rv <- reactiveValues(chrome_session = NULL)` after the UI/server definition (around line 420)
- [x] 1.2 In `observeEvent(input$refresh_sessions, {...})`, store the returned session: `rv$chrome_session <- sessions_result$chrome` after successful fetch
- [x] 1.3 In `observeEvent(input$refresh_sessions, {...})`, handle error case: set `rv$chrome_session <- NULL` on failure

## 2. Modify init_chromote() function signature and logic

- [x] 2.1 Add `existing_session = NULL` parameter to `init_chromote()` function definition (line 137)
- [x] 2.2 Add logic at start of function: if `!is.null(existing_session)`, navigate to session URL and return early with existing session
- [x] 2.3 Keep existing login logic in the `tryCatch` block for backward compatibility when `existing_session = NULL`

## 3. Modify server logic to pass stored session

- [x] 3.1 In `observeEvent(input$go, {...})`, before the `pwalk()` loop, add: `session_to_use <- rv$chrome_session`
- [x] 3.2 Call `init_chromote()` with the stored session: `init_params <- init_chromote(..., existing_session = session_to_use)`
- [x] 3.3 Add fallback: if `init_params` is NULL (session reuse failed), call `init_chromote()` without existing_session to create new session

## 4. Ensure proper session cleanup

- [x] 4.1 After the `pwalk()` loop completes, close the session: `tryCatch(init_params$chrome$close(), error = function(e) {})`
- [x] 4.2 Set `rv$chrome_session <- NULL` after closing to prevent reuse of closed session
- [x] 4.3 Add error handling: if `init_chromote()` fails with reused session, create new session and proceed

## 5. Testing

- [x] 5.1 Test full flow: refresh sessions → wait → extract PDFs (verify only one login occurs)
- [x] 5.2 Test without refreshing sessions first (verify new session created in init_chromote)
- [x] 5.3 Test session timeout scenario: refresh sessions, wait long period, extract PDFs (verify fallback works)
- [x] 5.4 Verify no orphaned Chromium processes after download completes (check with `ps aux | grep chromium`)

## 6. Documentation

- [x] 6.1 Update AGENTS.md Key Functions section for `init_chromote()` - add `existing_session` parameter
- [x] 6.2 Update AGENTS.md Active Changes section - add this new change
- [x] 6.3 Add comments in code explaining the session reuse flow
