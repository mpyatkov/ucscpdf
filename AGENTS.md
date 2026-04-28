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
- **Tasks**: 1-5, 7.1-7.5 complete (remaining: 3.7, 6.1-6.9)

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
| `get_session_names()` | app.R:38 | Chromote-based session list fetching after login |
| `init_chromote()` | app.R:139 | Create authenticated UCSC session via Chromote |
| `download_pdf_chromote()` | app.R:179 | Download PDF for a genomic region using Chromote |
| `read_data()` | app.R:320 | Parse BED/XLSX files, generate coordinates |
| `calculate_zoom_factor()` | app.R:295 | Expand region symmetrically around midpoint |
| `export_table_as_pdf()` | app.R:307 | Generate PDF table with ggpubr |

## Dependencies

### R Packages
- shiny, shinyjs, chromote
- rvest, httr
- qpdf, readxl, ggpubr, ggplot2, cowplot, gridExtra
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