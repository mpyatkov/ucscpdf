## ADDED Requirements

### Requirement: Chromote-based PDF Download

The system SHALL use Chromote (headless Chrome) for the complete PDF download workflow, including authentication and navigation, by:

1. Creating a Chromote session for browser automation
2. Logging in to UCSC Genome Browser via JavaScript form submission
3. Navigating to the selected session URL
4. Extracting PDF download link from the page
5. Downloading the PDF file to the specified output path

The system SHALL maintain the same Chromote session throughout a batch of downloads to avoid re-authentication.

#### Scenario: Successful login via Chromote
- **WHEN** user clicks "Download" with valid credentials
- **THEN** Chromote navigates to UCSC, submits login form via JavaScript, and successfully authenticates

#### Scenario: Navigate to session and download PDF
- **WHEN** user selects a session and requests PDF for genomic coordinates
- **THEN** Chromote navigates to session URL, extracts PDF link, downloads file to specified path

#### Scenario: Batch download with session reuse
- **WHEN** user has multiple coordinates to process in a batch
- **THEN** system reuses the same Chromote session for all downloads (no re-authentication)

#### Scenario: Session authentication persists
- **AFTER** successful login via Chromote
- **THEN** the Chromote session remains authenticated until batch completes or user logs out

### Requirement: Chromote Session Management

The system SHALL manage Chromote sessions lifecycle to:

1. Create session before first operation
2. Keep session alive during batch operations
3. Close session after batch completes or on error
4. Clean up resources even on failure

#### Scenario: Session cleanup on success
- **WHEN** all PDF downloads complete successfully
- **THEN** Chromote session is closed within 1 second

#### Scenario: Session cleanup on error
- **WHEN** an error occurs during download
- **THEN** Chromote session is closed within 1 second of error detection

#### Scenario: Session recovery on failure
- **WHEN** Chromote session fails during batch download
- **THEN** system logs error, continues to next coordinate, reports failures at end

### Requirement: PDF URL Extraction

The system SHALL extract the PDF download URL from the UCSC Genome Browser page by:

1. Using Chromote to navigate to the genomic coordinates view
2. Finding the PDF link element (`#pdfLink`)
3. Extracting the `href` attribute value
4. Converting relative paths to absolute URLs

#### Scenario: PDF link extraction
- **WHEN** Chromote navigates to a genomic region
- **THEN** system finds `#pdfLink` element and extracts its `href` value

#### Scenario: Relative URL conversion
- **WHEN** extracted URL is relative (e.g., `../some/path`)
- **THEN** system converts it to absolute UCSC URL

### Requirement: Error Handling for PDF Download

The system SHALL handle PDF download errors gracefully by:

1. Logging each failed download with coordinates and error message
2. Continuing to next coordinate in batch
3. Reporting all failures at the end of batch
4. Not blocking UI during download (use progress indicator)

#### Scenario: Single PDF download failure
- **WHEN** a single coordinate fails to download
- **THEN** system logs error, continues to next coordinate

#### Scenario: All PDFs download successfully
- **WHEN** all coordinates in batch download without error
- **THEN** system shows success message with file count

#### Scenario: Network timeout during download
- **WHEN** download times out (30 seconds)
- **THEN** system logs timeout error, continues to next coordinate