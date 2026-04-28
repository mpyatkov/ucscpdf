## ADDED Requirements

### Requirement: Fetch session list from UCSC Genome Browser

The system SHALL fetch a list of saved session names for a given UCSC username and password by:
1. Opening a headless Chrome browser via Chromote
2. Navigating to the UCSC login page
3. Submitting login credentials via JavaScript
4. Waiting for Cloudflare CAPTCHA challenge to resolve automatically
5. Navigating to the session management page
6. Parsing the HTML to extract session names from the session list table
7. Returning a unique list of session names

The system SHALL complete the fetch operation within 30 seconds under normal network conditions.

#### Scenario: Successful session list fetch
- **WHEN** user clicks "Refresh Session List" button with valid credentials
- **THEN** system displays a list of session names in the session dropdown within 30 seconds

#### Scenario: Invalid login credentials
- **WHEN** user clicks "Refresh Session List" with incorrect username or password
- **THEN** system displays an error message "Login failed. Please check your credentials." and does not update the session list

#### Scenario: Network timeout
- **WHEN** UCSC server does not respond within 30 seconds
- **THEN** system displays an error message "Request timed out. Please try again." and does not update the session list

#### Scenario: Cloudflare challenge present
- **WHEN** UCSC presents a Cloudflare CAPTCHA challenge
- **THEN** Chromote automatically solves the challenge without user intervention

#### Scenario: No sessions exist for user
- **WHEN** user has no saved sessions in UCSC
- **THEN** system displays an empty dropdown with placeholder text "No sessions found"

### Requirement: Chromote session lifecycle management

The system SHALL create a new Chromote session for each session list fetch operation and SHALL close the session immediately after fetching the HTML content to:
1. Minimize memory usage (target: <500MB per fetch)
2. Prevent session state leakage between users
3. Avoid resource leaks from orphaned browser processes

#### Scenario: Chromote session cleanup on success
- **WHEN** session list is successfully fetched
- **THEN** Chromote session is closed within 1 second of HTML retrieval

#### Scenario: Chromote session cleanup on error
- **WHEN** an error occurs during session fetch (login failure, timeout, parsing error)
- **THEN** Chromote session is closed within 1 second of error detection

#### Scenario: Chromote unavailable
- **WHEN** Chromium browser is not installed or Chromote fails to initialize
- **THEN** system displays error "Session discovery unavailable. Please enter session name manually." and keeps manual entry functional

### Requirement: Progress indication during fetch

The system SHALL display a progress indicator during the session fetch operation to inform users that:
1. The system is working (not frozen)
2. The operation is expected to take 5-15 seconds
3. The user should wait for completion

#### Scenario: Progress indicator display
- **WHEN** user clicks "Refresh Session List"
- **THEN** progress bar appears with message "Loading sessions..." and spinner animation

#### Scenario: Progress indicator completion
- **WHEN** session fetch completes (success or failure)
- **THEN** progress indicator disappears within 1 second

### Requirement: Error handling and user feedback

The system SHALL handle errors gracefully and provide clear, actionable feedback to users:
1. Login failures: "Login failed. Please check your credentials."
2. Timeout: "Request timed out. Please try again."
3. Chromote unavailable: "Session discovery unavailable. Please enter session name manually."
4. Parsing errors: "Unable to read session list. You can still enter session name manually."
5. Network errors: "Network error. Please check your connection and try again."

#### Scenario: Error message display
- **WHEN** an error occurs during session fetch
- **THEN** error message is displayed in a non-blocking toast notification that does not prevent manual session name entry

#### Scenario: Error does not block manual entry
- **WHEN** session fetch fails
- **THEN** user can still manually type a session name and proceed with PDF download
