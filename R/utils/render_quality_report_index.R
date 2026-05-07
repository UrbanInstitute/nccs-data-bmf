# ============================================================================
# render_quality_report_index.R
#
# Scans docs/quality-reports/ for *_quality_report.html files and writes
# a static index.html grouping them into Master / Current monthly / Legacy.
# Called by each pipeline after it writes its own HTML quality report.
# ============================================================================

#' Regenerate docs/quality-reports/index.html from files on disk
#'
#' @param dir Path to the quality-reports directory.
#' @return Invisibly: the path to the written index.html.
render_quality_report_index <- function(
    dir = here::here("docs", "quality-reports")
  ) {

  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
  }

  files <- list.files(dir, pattern = "_quality_report\\.html$", full.names = FALSE)
  files <- setdiff(files, "index.html")

  master_files  <- files[grepl("^bmf_master_quality_report\\.html$", files)]
  legacy_files  <- files[grepl("^bmf_legacy_\\d{4}_\\d{2}_quality_report\\.html$", files)]
  current_files <- setdiff(files, c(master_files, legacy_files))
  current_files <- current_files[grepl("^bmf_\\d{4}_\\d{2}_quality_report\\.html$", current_files)]

  # Sort current and legacy descending by YYYY_MM in filename
  ym_key <- function(x) {
    m <- regmatches(x, regexpr("\\d{4}_\\d{2}", x))
    ifelse(length(m) == 0, "", m)
  }
  current_files <- current_files[order(vapply(current_files, ym_key, ""), decreasing = TRUE)]
  legacy_files  <- legacy_files[order(vapply(legacy_files, ym_key, ""), decreasing = TRUE)]

  fmt_label <- function(file, prefix_strip) {
    ym <- regmatches(file, regexpr("\\d{4}_\\d{2}", file))
    gsub("_", "-", ym)
  }

  li <- function(file, label) {
    sprintf('      <li><a href="%s">%s</a></li>', file, label)
  }

  section_html <- function(title, items, empty_msg) {
    if (length(items) == 0) {
      return(sprintf('  <h2>%s</h2>\n  <p class="note">%s</p>', title, empty_msg))
    }
    sprintf(
      '  <h2>%s</h2>\n  <ul class="report-list">\n%s\n  </ul>',
      title, paste(items, collapse = "\n")
    )
  }

  master_section <- section_html(
    "Master BMF",
    if (length(master_files) > 0)
      list(li(master_files[[1]], "Master BMF Quality Report")) else list(),
    "Not yet generated."
  )

  current_section <- section_html(
    sprintf("Current Monthly BMF (%d)", length(current_files)),
    lapply(current_files, function(f) li(f, sprintf("%s BMF Quality Report", fmt_label(f)))),
    "No reports available yet."
  )

  legacy_section <- section_html(
    sprintf("Legacy BMF (501CX-NONPROFIT-PX) (%d)", length(legacy_files)),
    lapply(legacy_files, function(f) li(f, sprintf("%s Legacy BMF Quality Report", fmt_label(f)))),
    "No legacy reports available yet."
  )

  html <- sprintf('<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>BMF Quality Reports</title>
  <style>
    body {
      font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, "Helvetica Neue", Arial, sans-serif;
      max-width: 900px;
      margin: 0 auto;
      padding: 2rem;
      line-height: 1.6;
      color: #333;
    }
    h1 {
      color: #1a73e8;
      border-bottom: 2px solid #1a73e8;
      padding-bottom: 0.5rem;
    }
    h2 {
      color: #1a73e8;
      margin-top: 2rem;
    }
    .description {
      background: #f8f9fa;
      padding: 1rem;
      border-radius: 4px;
      margin-bottom: 2rem;
    }
    .report-list {
      list-style: none;
      padding: 0;
      columns: 2;
      column-gap: 2rem;
    }
    .report-list li {
      padding: 0.4rem 0;
      border-bottom: 1px solid #eee;
      break-inside: avoid;
    }
    .report-list a {
      color: #1a73e8;
      text-decoration: none;
      font-weight: 500;
    }
    .report-list a:hover {
      text-decoration: underline;
    }
    .back-link {
      margin-top: 2rem;
      display: inline-block;
    }
    .note {
      color: #666;
      font-style: italic;
    }
    .meta {
      color: #888;
      font-size: 0.9rem;
    }
  </style>
</head>
<body>
  <h1>BMF Quality Reports</h1>

  <div class="description">
    <p>Quality reports for the IRS Business Master File (BMF) data processing pipelines. Each report contains completeness metrics, validation results, and summary statistics for the processed data.</p>
    <p class="meta">Index generated %s &middot; %d total reports</p>
  </div>

%s

%s

%s

  <a href="../" class="back-link">&larr; Back to Documentation</a>
</body>
</html>
',
    format(Sys.time(), "%Y-%m-%d %H:%M %Z"),
    length(master_files) + length(current_files) + length(legacy_files),
    master_section, current_section, legacy_section
  )

  out_path <- file.path(dir, "index.html")
  writeLines(html, out_path)
  invisible(out_path)
}
