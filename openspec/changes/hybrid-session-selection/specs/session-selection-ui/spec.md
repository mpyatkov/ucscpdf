## ADDED Requirements

### Requirement: Session dropdown with manual entry fallback

The system SHALL provide a session selection UI component that:
1. Displays discovered sessions in a dropdown list
2. Allows users to type a session name manually (selectize pattern)
3. Supports search/filter within the dropdown
4. Shows session count when sessions are loaded
5. Maintains backward compatibility with manual entry workflow

#### Scenario: Dropdown populated with discovered sessions
- **WHEN** session fetch completes successfully with N sessions
- **THEN** dropdown shows N session names in alphabetical order with placeholder "Select a session (N available)"

#### Scenario: Manual session name entry
- **WHEN** user types a session name that is not in the dropdown list
- **THEN** system accepts the typed value and uses it for PDF download

#### Scenario: Session search within dropdown
- **WHEN** user types characters in the dropdown
- **THEN** dropdown filters to show only sessions matching the typed text

#### Scenario: Empty session list
- **WHEN** session fetch returns zero sessions
- **THEN** dropdown shows placeholder "No sessions found - enter name manually"

### Requirement: Refresh session list button

The system SHALL provide an explicit "Refresh Session List" button that:
1. Triggers session discovery only when clicked (not automatic)
2. Is enabled only when login and password fields are non-empty
3. Shows a loading indicator during the fetch operation
4. Updates the session dropdown upon successful fetch

#### Scenario: Refresh button enabled state
- **WHEN** login and password fields are both non-empty
- **THEN** "Refresh Session List" button is enabled

#### Scenario: Refresh button disabled state
- **WHEN** login or password field is empty
- **THEN** "Refresh Session List" button is disabled with tooltip "Enter login and password to refresh sessions"

#### Scenario: Refresh button clicked
- **WHEN** user clicks "Refresh Session List" button
- **THEN** system initiates session fetch and shows progress indicator

#### Scenario: Refresh during ongoing fetch
- **WHEN** user clicks "Refresh Session List" while a fetch is in progress
- **THEN** second click is ignored (debounced) until first fetch completes

### Requirement: Session count display

The system SHALL display the number of available sessions to:
1. Confirm successful fetch (show count)
2. Indicate empty session list (show "0 sessions")
3. Provide context for dropdown selection

#### Scenario: Session count after successful fetch
- **WHEN** session fetch returns N sessions (N > 0)
- **THEN** display shows "N session(s) loaded" next to dropdown

#### Scenario: Session count after empty fetch
- **WHEN** session fetch returns 0 sessions
- **THEN** display shows "0 sessions found"

#### Scenario: Session count before any fetch
- **WHEN** user has not clicked "Refresh Session List"
- **THEN** display shows "Click 'Refresh' to load sessions"

### Requirement: Responsive layout for session selection

The system SHALL arrange session selection UI elements to:
1. Group login credentials (login, password) separately from session selection
2. Place "Refresh" button adjacent to session dropdown
3. Maintain usability on standard desktop resolutions (1024x768 minimum)

#### Scenario: Desktop layout
- **WHEN** app is viewed on desktop (width >= 1024px)
- **THEN** login credentials and session selection appear side-by-side in two columns

#### Scenario: Mobile layout
- **WHEN** app is viewed on mobile (width < 768px)
- **THEN** UI elements stack vertically with login credentials above session selection
