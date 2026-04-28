## Context

The UCSC PDF Downloader Shiny app currently uses `ggpubr` package solely for creating tabular PDF output of genomic coordinates via `export_table_as_pdf()` function (lines 271-317 in app.R). The function converts data frames to styled tables using:
- `ggtexttable()` to create ggplot table objects
- `ttheme()` for styling (font size 8, padding)
- `tab_add_title()` for adding titles
- `cowplot::plot_grid()` to convert ggplot to grob
- `marrangeGrob()` for multi-page layout
- `ggsave()` for PDF output

The project already depends on `gridExtra` (which provides `tableGrob()` and `ttheme_minimal()`) and `grid` (for low-level graphics operations). These can replicate the ggpubr functionality without adding new dependencies.

## Goals / Non-Goals

**Goals:**
- Remove `ggpubr` dependency from the project
- Use existing `gridExtra` and `grid` packages for table generation
- Maintain multi-page PDF support for large coordinate lists
- Preserve similar visual styling (8pt font, bold title, appropriate padding)

**Non-Goals:**
- Redesigning the table layout or adding new features
- Changing the PDF output format or file naming
- Modifying other parts of the application

## Decisions

### Decision 1: Use `gridExtra::tableGrob()` instead of `ggtexttable()`

**Choice**: Replace `ggpubr::ggtexttable()` with `gridExtra::tableGrob()`

**Rationale**: 
- `tableGrob()` creates a grid graphical object (grob) directly without the ggplot2 wrapper
- `gridExtra` is already a project dependency
- `tableGrob()` accepts the same data frame input and supports themes via `ttheme_minimal()`

**Alternatives considered**:
- `grid.table()`: Simpler but less customizable, doesn't support themes as well
- Keep `ggpubr`: Rejected due to unnecessary dependency

### Decision 2: Use `grid::arrangeGrob()` for title addition

**Choice**: Replace `ggpubr::tab_add_title()` with `grid::arrangeGrob(top = textGrob(...))`

**Rationale**:
- `arrangeGrob()` natively supports adding title text via the `top` parameter
- `textGrob()` provides control over font face, size, and padding
- This is the standard grid graphics approach for annotated layouts

**Alternatives considered**:
- Custom grid drawing: Too complex for this use case
- `grid.arrange()` with `top`: Similar but `arrangeGrob()` returns an object (better for programmatic use)

### Decision 3: Replace `ggsave()` with base `pdf()` device

**Choice**: Use `pdf()` + `grid::grid.draw()` + `dev.off()` instead of `ggsave()`

**Rationale**:
- `ggsave()` is designed for ggplot objects; `grid.draw()` is the native way to render grobs
- Base graphics device gives more control over multi-page PDF output
- Already using `marrangeGrob()` which works with `pdf()` device

**Alternatives considered**:
- `cowplot::save_plot()`: Still requires ggplot objects, doesn't solve the dependency issue

### Decision 4: Remove `cowplot::plot_grid()` wrapper

**Choice**: Remove the `cowplot::plot_grid(tab)` call entirely

**Rationale**:
- `plot_grid()` was only needed to convert ggplot object to grob for `marrangeGrob()`
- With `tableGrob()`, the output is already a grob - no conversion needed
- This allows removal of `cowplot` dependency (if not used elsewhere)

### Decision 5: Check and remove unused dependencies

**Choice**: Evaluate if `ggplot2` and `cowplot` can be removed after this change

**Rationale**:
- `ggplot2` is used by `ggpubr` - if `ggpubr` is removed and no other code uses `ggplot2`, it can be removed
- `cowplot` is only used in `plot_grid()` call being removed
- Reducing dependencies improves startup time and reduces potential conflicts

## Risks / Trade-offs

**[Slight visual differences]** → The `ttheme_minimal()` default padding and styling differs from `ggpubr::ttheme()`. Mitigation: Adjust `ttheme_minimal()` parameters (padding, font size) to match current output as closely as possible.

**[marrangeGrob compatibility]** → `marrangeGrob()` expects grobs; `tableGrob()` returns grobs natively, so compatibility is expected. Mitigation: Test with sample data to verify multi-page output works correctly.

**[Dependency chain]** → `gridExtra` may itself depend on `ggplot2`. Mitigation: Check with `packrat:::recursivePackageDependencies()` or similar tools. If `ggplot2` is still needed as indirect dependency, it may not be removable.

**[Base graphics device behavior]** → `pdf()` device has different defaults than `ggsave()` (which uses `ggplot2:::plot_dev()`). Mitigation: Explicitly set width, height, and other parameters to match current output dimensions (8.5×11 or 11×8.5 depending on orientation).
