## 1. Dependencies and Setup

- [x] 1.1 Add `chromote` to package dependencies (DESCRIPTION or renv)
- [x] 1.2 Verify Chromium browser availability on development system
- [x] 1.3 Test Chromote initialization locally: `ChromoteSession$new()`
- [x] 1.4 Document system requirements for deployment (Chromium needed)

## 2. Core Session Discovery Function

- [x] 2.1 Create `get_session_names(username, password)` function in `app.R`
- [x] 2.2 Implement Chromote login flow (navigate to login page, submit credentials via JavaScript)
- [x] 2.3 Add Cloudflare wait logic (poll URL until navigated away from login page, max 15 seconds)
- [x] 2.4 Navigate to session management page (`/cgi-bin/hgSession?hgS_doMainPage=1`)
- [x] 2.5 Parse HTML with rvest to extract session names from table rows
- [x] 2.6 Return unique session names as character vector
- [x] 2.7 Add `on.exit(b$close())` for Chromote session cleanup
- [x] 2.8 Wrap in `tryCatch()` for error handling

## 3. UI Changes - Session Selection

- [x] 3.1 Replace `textInput("session", ...)` with `selectInput("session", ..., selectize = TRUE)`
- [x] 3.2 Set initial `choices = NULL` for empty dropdown
- [x] 3.3 Add `actionButton("refresh_sessions", "Refresh Session List", icon = icon("refresh"))`
- [x] 3.4 Add `textOutput("session_count")` for session count display
- [x] 3.5 Reorganize UI layout: two-column structure (login credentials | session selection)
- [x] 3.6 Add tooltip to disabled refresh button: "Enter login and password to refresh sessions"
- [ ] 3.7 Test UI renders correctly on desktop (1024x768) and mobile viewports

## 4. Server Logic - Session Refresh

- [x] 4.1 Add `observeEvent(input$refresh_sessions, {...})` to trigger session fetch
- [x] 4.2 Wrap fetch in `withProgress(message = "Loading sessions...", {...})`
- [x] 4.3 Call `get_session_names(input$login, input$password)` inside progress block
- [x] 4.4 Update dropdown with `updateSelectInput(session, "session", choices = sessions)`
- [x] 4.5 Update session count display with `output$session_count <- renderText({...})`
- [x] 4.6 Add debouncing: disable refresh button during ongoing fetch
- [x] 4.7 Enable/disable refresh button based on login/password non-empty (use `observe` + `toggleState`)

## 5. Error Handling

- [x] 5.1 Add specific error messages for each failure mode:
  - Login failure: "Login failed. Please check your credentials."
  - Timeout: "Request timed out. Please try again."
  - Chromote unavailable: "Session discovery unavailable. Please enter session name manually."
  - Parsing error: "Unable to read session list. You can still enter session name manually."
- [x] 5.2 Display errors using `showNotification()` (non-blocking toast)
- [x] 5.3 Ensure manual entry remains functional after error
- [x] 5.4 Log errors to console for debugging (`message()` or `cat()`)
- [x] 5.5 Add timeout handling (max 30 seconds for entire fetch operation)

## 6. Testing and Validation

- [ ] 6.1 Test with valid credentials and existing sessions (ucsc_username/ ucsc_password)
- [ ] 6.2 Test with invalid credentials (verify error message)
- [ ] 6.3 Test with empty session list (verify placeholder text)
- [ ] 6.4 Test manual session name entry (verify it still works)
- [ ] 6.5 Test refresh button enabled/disabled state logic
- [ ] 6.6 Test concurrent refresh attempts (verify debouncing)
- [ ] 6.7 Test Chromote session cleanup (verify no orphaned processes)
- [ ] 6.8 Test memory usage during fetch (verify <500MB peak)
- [ ] 6.9 Test on ShinyApps.io deployment (verify Chromium availability or graceful fallback)

## 7. Documentation and Cleanup

- [x] 7.1 Add roxygen2 documentation for `get_session_names()` function
- [x] 7.2 Update README.org with new session selection feature
- [x] 7.3 Add deployment notes for ShinyApps.io (Chromium requirement)
- [x] 7.4 Remove test files created during exploration (`test_*.R`, `get_sessions.R`)
- [x] 7.5 Verify no console.log or debug statements left in production code
