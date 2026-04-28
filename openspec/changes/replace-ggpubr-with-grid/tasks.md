## 1. Code Changes - export_table_as_pdf() Function

- [x] 1.1 Replace `ggpubr::ggtexttable()` call with `gridExtra::tableGrob()` and `ttheme_minimal(base_size = 8)`
- [x] 1.2 Replace `ggpubr::tab_add_title()` with `grid::arrangeGrob(top = grid::textGrob(title_text, gp = gpar(fontface = \"bold\", fontsize = 8), vp = NULL))`
- [x] 1.3 Remove `cowplot::plot_grid(tab)` wrapper - table grob is already in correct format for `marrangeGrob()`
- [x] 1.4 Replace `ggsave()` call with base `pdf()` device + `grid::grid.draw()` loop for multi-page output
- [x] 1.5 Update function to return the grob list properly for `marrangeGrob()` processing
- [x] 1.6 Set explicit PDF dimensions in `pdf()` call: width = OUTPUT_WIDTH, height = OUTPUT_HEIGHT

## 2. Dependency Cleanup

- [x] 2.1 Remove `library(ggpubr)` from app.R header (line 8)
- [x] 2.2 Remove `library(cowplot)` from app.R header (line 12) if not used elsewhere
- [x] 2.3 Remove `library(ggplot2)` from app.R header (line 9) if not used elsewhere (check with grep)
- [x] 2.4 Verify no other code references `ggpubr`, `cowplot`, or `ggplot2` functions
- [x] 2.5 Update AGENTS.md dependencies section: remove ggpubr, evaluate cowplot and ggplot2

## 3. Testing

- [x] 3.1 Test `export_table_as_pdf()` with sample XLSX file (with annotations, landscape mode)
- [x] 3.2 Test `export_table_as_pdf()` with BED file (without annotations, portrait mode)
- [x] 3.3 Test multi-page output with file containing >30 rows (landscape) and >40 rows (portrait)
- [x] 3.4 Verify PDF output matches expected format (title bold, 8pt font, correct dimensions)
- [x] 3.5 Run full app test: load file, generate PDFs, verify coordinate table PDF is created

## 4. Documentation

- [x] 4.1 Update AGENTS.md "Key Functions" section for `export_table_as_pdf()` - update description
- [x] 4.2 Update AGENTS.md "Dependencies" section - remove ggpubr, update gridExtra description
- [x] 4.3 Add comment in code explaining the grid approach vs previous ggpubr approach (optional)
