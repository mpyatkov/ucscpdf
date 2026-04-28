## Why

Currently, `get_session_names()` creates a Chromote session and returns it, but `init_chromote()` creates a completely new session and repeats the login process. This is inefficient - the application authenticates twice when it could reuse the existing authenticated session. This wastes time and creates unnecessary browser instances.

## What Changes

- Modify the server logic to store the Chromote session returned by `get_session_names()` in a reactive value
- Modify `init_chromote()` to accept an optional existing Chromote session parameter
- If a valid session is provided to `init_chromote()`, skip the login process and reuse it
- Update the server's `observeEvent(input$go, {...})` to pass the stored session to `init_chromote()`
- Ensure proper session cleanup: close the Chromote session only after all downloads complete

## Capabilities

### New Capabilities

- `chromote-session-reuse`: Store and reuse authenticated Chromote session across multiple operations (session listing and PDF downloads)

### Modified Capabilities

<!-- No existing spec-level requirements are changing - this is an implementation refinement -->

## Impact

- **Code**: `app.R` - `get_session_names()`, `init_chromote()`, server logic (lines 475-627)
- **Performance**: Reduces login time by ~5-10 seconds (no duplicate authentication)
- **Resources**: Reduces Chromium browser instances from 2 to 1 per user session
- **Behavior**: Chromote session persists from session listing through PDF download completion
