## Requirements

### Requirement: Export coordinate table as multi-page PDF using grid graphics

The system SHALL generate a PDF file containing genomic coordinates in a tabular format using `gridExtra::tableGrob()` and base `grid` graphics instead of `ggpubr`.

The table SHALL support:
- Configurable font size (default 8pt)
- Bold title text with configurable content
- Portrait or landscape orientation based on whether annotations are included
- Multi-page output when coordinate count exceeds chunk size (30 for landscape with annotations, 40 for portrait without)

#### Scenario: Export table with annotations in landscape mode
- **WHEN** `export_table_as_pdf()` is called with `add_annotations = TRUE`
- **THEN** the system SHALL create a PDF with:
  - Dimensions 11" width × 8.5" height (landscape)
  - Chunk size of 30 rows per page
  - Title text rendered in bold at 8pt font
  - Table styled with `ttheme_minimal(base_size = 8)`

#### Scenario: Export table without annotations in portrait mode
- **WHEN** `export_table_as_pdf()` is called with `add_annotations = FALSE`
- **THEN** the system SHALL create a PDF with:
  - Dimensions 8.5" width × 11" height (portrait)
  - Only first 3 columns displayed (chr, start, end)
  - Chunk size of 40 rows per page
  - Title text rendered in bold at 8pt font

#### Scenario: Multi-page output for large coordinate lists
- **WHEN** the input file contains more than the chunk size (30 or 40) rows
- **THEN** the system SHALL use `marrangeGrob()` to create multiple pages in a single PDF file
- **AND** each page SHALL contain at most the chunk size number of rows

#### Scenario: Title rendering
- **WHEN** generating the table PDF
- **THEN** the system SHALL render the title using `grid::textGrob()` with:
  - Font face: bold
  - Font size: 8
  - Padding: 1 line
- **AND** the title content SHALL be the `title_text` parameter passed to the function

### Requirement: No ggpubr dependency

The system SHALL NOT import or use the `ggpubr` package.

#### Scenario: Package not loaded
- **WHEN** the application starts
- **THEN** `ggpubr` SHALL NOT be present in the `library()` or `require()` calls at the top of `app.R`

#### Scenario: No ggpubr functions in code
- **WHEN** searching the codebase for `ggpubr`, `ggtexttable`, `ttheme`, or `tab_add_title`
- **THEN** no matches SHALL be found in `app.R` or other source files

### Requirement: Existing dependency usage

The system SHALL use only existing dependencies (`gridExtra`, `grid`, `gridExtra::ttheme_minimal`) for table generation.

#### Scenario: gridExtra::tableGrob usage
- **WHEN** creating the table visualization
- **THEN** the system SHALL use `gridExtra::tableGrob()` to convert the data frame to a graphical table object

#### Scenario: grid::arrangeGrob for layout
- **WHEN** arranging the table with title
- **THEN** the system SHALL use `grid::arrangeGrob()` with the `top` parameter to add the title to the table grob

#### Scenario: Base graphics device for PDF output
- **WHEN** writing the PDF file
- **THEN** the system SHALL use base `pdf()` device with `grid::grid.draw()` instead of `ggsave()`
