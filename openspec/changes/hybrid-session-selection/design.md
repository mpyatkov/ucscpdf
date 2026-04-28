## Context

The UCSC Genome Browser application currently requires users to manually type session names. UCSC has implemented Cloudflare CAPTCHA protection on all session-related pages, blocking programmatic access via rvest. Testing confirms that Chromote (headless Chrome) successfully bypasses CAPTCHA and can fetch session lists after login.

**Current State:**
- `textInput` for session name (manual entry only)
- No session discovery mechanism
- Users must remember or externally track session names
- Login works via rvest (form submission), but session listing fails

**Constraints:**
- UCSC provides no REST API for session listing
- Cloudflare CAPTCHA blocks automated access
- ShinyApps.io deployment may have Chromium availability issues
- Session fetch takes 5-10 seconds (explicit user action required)

## Goals / Non-Goals

**Goals:**
- Enable session name discovery via dropdown selection
- Provide manual entry fallback for all scenarios
- Explicit user control over session fetching (refresh button)
- Graceful degradation when Chromote unavailable
- Maintain backward compatibility with existing workflow

**Non-Goals:**
- Automatic session fetching on login (too slow, unexpected delay)
- Session creation/deletion (UCSC web UI only)
- Multi-user session sharing features
- Caching sessions across app restarts (future enhancement)
- Supporting UCSC API for session management (doesn't exist)

## Decisions

### 1. Hybrid Session Selection Pattern

**Decision:** Use `selectInput` with `selectize = TRUE` (allows typing + selection)

**Alternatives Considered:**
- `selectInput` only: Forces selection, no manual entry → REJECTED (loses flexibility)
- `textInput` with autocomplete: Complex to implement, poor UX → REJECTED
- Separate manual/auto modes: Confusing UI → REJECTED

**Rationale:** Selectize provides dropdown with search + manual entry in single control.

### 2. Explicit Refresh Trigger

**Decision:** Session fetch only on "Refresh" button click

**Alternatives Considered:**
- Auto-fetch on login/password change: Faster UX, but unexpected 5-10s delay → REJECTED
- Auto-fetch with debounce: Still unexpected resource usage → REJECTED
- Fetch on first session input focus: Unclear when fetch happens → REJECTED

**Rationale:** Explicit action makes cost (time, resources) clear to user. Matches mental model of "refresh list."

### 3. Chromote with rvest Parsing

**Decision:** Use Chromote for browser automation, rvest for HTML parsing

**Alternatives Considered:**
- Pure Chromote DOM queries: More direct, but rvest has better R idioms → REJECTED
- RSelenium: Heavier, more complex setup → REJECTED
- Custom HTTP client with CAPTCHA solving: Not feasible → REJECTED

**Rationale:** Chromote is lightweight (vs RSelenium), well-maintained, integrates with R ecosystem.

### 4. Single Shared Chromote Session

**Decision:** Create Chromote session per fetch, close immediately after

**Alternatives Considered:**
- Global shared Chromote session: More efficient, but state management complexity → REJECTED
- Session pool: Over-engineered for single-user app → REJECTED
- Keep session alive: Memory waste, potential leaks → REJECTED

**Rationale:** Per-fetch sessions are simpler, safer, avoid concurrency issues. Performance cost acceptable for explicit user action.

### 5. Graceful Degradation Strategy

**Decision:** Try Chromote, fall back to manual entry on any failure

**Alternatives Considered:**
- Pre-flight Chromote availability check: Adds complexity, may give false negatives → REJECTED
- Require Chromote for app to run: Too strict, excludes some deployments → REJECTED
- Silent fallback with warning: User may not understand why list empty → REJECTED

**Rationale:** Try Chromote, show clear error message on failure, keep manual entry always available.

### 6. Error Handling and User Feedback

**Decision:** Use Shiny progress indicators + toast notifications for errors

**Alternatives Considered:**
- Silent failure with manual fallback only: User confused why list empty → REJECTED
- Modal dialog for errors: Too intrusive for expected failures → REJECTED
- Console logging only: Users won't see → REJECTED

**Rationale:** Progress indicator during fetch, non-blocking error message on failure.

## Risks / Trade-offs

| Risk | Likelihood | Impact | Mitigation |
|------|------------|--------|------------|
| **Chromium not available on ShinyApps.io** | Medium | High | Graceful fallback to manual entry; documentation for deployment |
| **Memory exhaustion during Chromote session** | Low | Medium | Close session immediately after use; add timeout |
| **UCSC changes login page structure** | Low | High | Selector abstraction; test suite for early detection |
| **Cloudflare escalates CAPTCHA complexity** | Medium | High | Monitor; may require API key approach in future |
| **Session fetch timeout (>30s)** | Low | Low | Add explicit timeout; show timeout error message |
| **Multiple users trigger simultaneous fetches** | Low | Medium | Existing semaphore already limits concurrent access |

## Migration Plan

**No migration required** - this is a new feature, not a breaking change.

**Deployment Steps:**
1. Add `chromote` to dependencies
2. Update UI: `textInput` → `selectInput` with selectize
3. Add `get_session_names()` function (Chromote-based)
4. Add "Refresh Session List" button and observer
5. Add error handling and progress indicators
6. Test on target deployment environment (ShinyApps.io)
7. Update user documentation

**Rollback Strategy:**
- Revert UI to `textInput` (5-minute code change)
- Remove `chromote` dependency
- No data migration needed

## Open Questions

1. **ShinyApps.io Chromium availability**: Needs testing or confirmation from RStudio
2. **Session name parsing robustness**: Current regex works for `test_session_*` pattern; need to test with varied session names
3. **Timeout duration**: 15s for login + Cloudflare; may need tuning based on real-world performance
4. **Error message specificity**: Should we distinguish between "login failed" vs "session list empty" vs "Chromote unavailable"?
