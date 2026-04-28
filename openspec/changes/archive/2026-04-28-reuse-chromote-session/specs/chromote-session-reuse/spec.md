## ADDED Requirements

### Requirement: Store authenticated Chromote session for reuse

The system SHALL store the authenticated Chromote session returned by `get_session_names()` in a Shiny reactive value so it can be reused for PDF downloads.

#### Scenario: Session stored after refresh
- **WHEN** user clicks "Refresh Session List" and `get_session_names()` returns a valid session
- **THEN** the system SHALL store the Chromote session (`chrome` object) in a `reactiveValues()` object
- **AND** the session SHALL remain available until explicitly closed

#### Scenario: Session available for reuse
- **WHEN** user clicks "Extract pdfs" after successfully refreshing sessions
- **THEN** the system SHALL pass the stored Chromote session to `init_chromote()`
- **AND** `init_chromote()` SHALL reuse this session instead of creating a new one

### Requirement: init_chromote accepts optional existing session

The `init_chromote()` function SHALL accept an optional `existing_session` parameter.

#### Scenario: init_chromote with existing session
- **WHEN** `init_chromote()` is called with a valid `existing_session` parameter
- **THEN** the function SHALL skip the login process
- **AND** the function SHALL navigate the existing session to the session URL
- **AND** return the session and main_url without creating a new ChromoteSession

#### Scenario: init_chromote without existing session
- **WHEN** `init_chromote()` is called without an `existing_session` parameter (or NULL)
- **THEN** the function SHALL create a new ChromoteSession
- **AND** perform the full login process
- **AND** return the new session and main_url (backward compatible behavior)

### Requirement: Proper session lifecycle management

The system SHALL close the Chromote session only after all PDF downloads are complete.

#### Scenario: Session closed after downloads
- **WHEN** all PDFs for all input files have been downloaded and combined
- **THEN** the system SHALL call `chrome$close()` on the reused session
- **AND** set the reactive session value to NULL

#### Scenario: Error during download
- **WHEN** an error occurs during the PDF download process
- **THEN** the system SHALL still attempt to close the Chromote session
- **AND** log an appropriate error message
