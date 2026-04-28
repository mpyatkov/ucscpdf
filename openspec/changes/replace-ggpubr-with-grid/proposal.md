## Why

The `ggpubr` R package is currently used only to create a PDF table of genomic coordinates in the `export_table_as_pdf()` function. This creates an unnecessary dependency since equivalent functionality can be achieved using `gridExtra` (already a project dependency) with `grid` graphics. Removing `ggpubr` simplifies the dependency tree and reduces package load time.

## What Changes

- Replace `ggpubr::ggtexttable()` with `gridExtra::tableGrob()` for table creation
- Replace `ggpubr::ttheme()` with `gridExtra::ttheme_minimal()` for table styling
- Replace `ggpubr::tab_add_title()` with `grid::arrangeGrob()` and `grid::textGrob()` for title addition
- Remove `cowplot::plot_grid()` wrapper (no longer needed without ggplot objects)
- Replace `ggsave()` with base `pdf()` device + `grid::grid.draw()` for multi-page PDF output
- Remove `ggpubr` from R package dependencies
- Evaluate if `cowplot` and `ggplot2` can be removed (if not used elsewhere)

## Capabilities

### New Capabilities

- `coordinate-table-export`: Export genomic coordinate tables as multi-page PDF using grid graphics instead of ggpubr

### Modified Capabilities

<!-- No existing spec-level requirements are changing - this is an implementation refinement -->

## Impact

- **Code**: `app.R` - `export_table_as_pdf()` function (lines 271-317)
- **Dependencies**: Remove `ggpubr` from imports; potentially remove `cowplot` and `ggplot2` if unused elsewhere
- **Documentation**: Update `AGENTS.md` dependencies section
- **Output**: PDF table appearance will be similar but may have minor styling differences (font padding, cell spacing)
