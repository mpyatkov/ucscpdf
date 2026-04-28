## Why

Users currently must manually type UCSC session names without any discovery mechanism. The UCSC Genome Browser protects session listings with Cloudflare CAPTCHA, making programmatic access via rvest impossible. This creates a poor user experience where users need to remember or look up session names externally.

## What Changes

- **Session Input**: Replace `textInput` with `selectInput` (with manual fallback) for session selection
- **Session Discovery**: Add Chromote-based session list fetching with explicit "Refresh" button
- **UI Enhancement**: Add session count display and loading indicators
- **Fallback Support**: Manual session name entry remains available when Chromote is unavailable
- **Dependency**: Add `chromote` package for headless browser automation

## Capabilities

### New Capabilities
- `session-discovery`: Chromote-based session list fetching from UCSC Genome Browser after login
- `session-selection-ui`: Dynamic dropdown with manual entry fallback for session selection

### Modified Capabilities
- None (this is a new feature, not modifying existing capability requirements)

## Impact

- **New Dependencies**: `chromote` R package (requires Chromium browser)
- **Shiny UI**: Session input changes from text to select with manual fallback
- **Server Logic**: New observeEvent for session fetching, explicit refresh trigger
- **Deployment**: ShinyApps.io may require Chromium configuration or alternative deployment
- **Performance**: Session fetch adds 5-10 second delay (explicit user action, not automatic)
- **Memory**: Chromote session uses ~200-500MB RAM during session fetch
