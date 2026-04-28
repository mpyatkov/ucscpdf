# UCSC PDF Downloader - Agent Documentation

## Project Overview

- **Purpose**: Download high-quality vector PDF screenshots from UCSC Genome Browser sessions for genomic coordinates in BED/XLSX files
- **Tech Stack**: R, Shiny, rvest, chromote, qpdf
- **Main Files**: app.R, download_ucsc_pdf_pipeline.R

## OpenSpec Workflow

| Command | Description |
|---------|-------------|
| `/opsx:propose <name>` | Create new change with proposal, design, and tasks artifacts |
| `/opsx:apply` | Implement tasks from current change (prompts for change name if needed) |
| `/opsx:archive` | Archive completed change after implementation |
| `/opsx:explore` | Enter think-through mode for ideas/problems |

## Active Changes

### hybrid-session-selection (In Progress)

- **Status**: Implementation in progress
- **Created**: 2026-03-17
- **Schema**: spec-driven
- **Description**: Adds Chromote-based session discovery to bypass UCSC Cloudflare CAPTCHA
- **Completed**: Tasks 1-5, 7.1-7.5 (core functionality, UI, error handling, documentation)
- **Remaining**: Task 3.7 (viewport testing), Tasks 6.1-6.9 (testing and validation)

```
openspec/changes/hybrid-session-selection/
├── proposal.md       # What & why
├── design.md       # How with decisions
├── tasks.md         # Implementation checklist
└── specs/
    ├── session-discovery/spec.md
    └── session-selection-ui/spec.md
```

## Key Functions

| Function | Location | Description |
|----------|----------|-------------|
| `get_session_names()` | app.R:38 | Fetch UCSC session names via Chromote login, returns list with names and Chromote session |
| `init_chromote()` | app.R:137 | Create authenticated UCSC session via Chromote for PDF downloads |
| `download_pdf_chromote()` | app.R:190 | Download PDF for a genomic region using Chromote session |
| `read_data()` | app.R:320 | Parse BED/XLSX files, generate genomic coordinates |
| `calculate_zoom_factor()` | app.R:259 | Expand genomic region symmetrically around midpoint by zoom factor |
| `export_table_as_pdf()` | app.R:271 | Generate PDF table of coordinates with ggpubr |

## Dependencies

### R Packages
- shiny, shinyjs, chromote
- rvest, httr
- qpdf, readxl, tools, ggpubr, ggplot2, cowplot, gridExtra
- dplyr, purrr, stringr, readr

### System Requirements
- Chromium browser (for session discovery)
- qpdf (PDF merging)
- UCSC Genome Browser access

## Testing Commands

```bash
# Local testing
Rscript app.R
# Or run via RStudio "Run App"

# OpenSpec commands
openspec status
openspec status --change "hybrid-session-selection"
openspec list
```

## Configuration

- `openspec/config.yaml` - OpenSpec configuration
- `openspec/changes/` - Change directories

## REFACTORING.md Reference

Found in repository root. Documents:
- High-level workflow
- Core components (Shiny app + CLI pipeline)
- Key functions
- Input/output formats
- Limitations (rvest limitations, Cloudflare)
- Proposed improvements (Chromote integration)