# Yearly refresh check of the NTEE code list against the IRS (BACKLOG Z18).
#
# What it does
#   1. Downloads the IRS "Instructions for Form 1023" page and parses
#      Appendix D, the IRS's own list of NTEE codes and short descriptions.
#   2. Writes that list to data/lookup/irs_ntee_codes.csv, stamped with the
#      instructions' revision date, so the repo records what the IRS said.
#   3. Compares it with the ntee_code sheet of data/lookup/bmf_code_lookup.xlsx
#      and prints (a) IRS codes our lookup lacks and (b) lookup codes the IRS
#      no longer lists.
#
# It never edits the workbook. A person reviews the printed differences and
# applies them by hand (see docs/runbooks/ntee-code-list-yearly-refresh.md).
#
# Run from the repo root:  Rscript scripts/refresh_ntee_lookup_from_irs.R
# Needs network access. Read only apart from the CSV it writes.

suppressMessages({
  library(dplyr)
  library(stringr)
  library(readr)
  library(purrr)
})

IRS_URL <- "https://www.irs.gov/instructions/i1023"
OUT_CSV <- "data/lookup/irs_ntee_codes.csv"
LOOKUP  <- "data/lookup/bmf_code_lookup.xlsx"

html <- readLines(url(IRS_URL), warn = FALSE, encoding = "UTF-8") |>
  paste(collapse = " ")

revision <- str_match(html, "Instructions for Form 1023 \\((\\d{2}/\\d{4})\\)")[, 2]
if (is.na(revision)) stop("Could not find the revision date on the IRS page; the page layout may have changed.")

# Each code is marked up as
#   <span class="code">B29 - </span><span class="activity">Charter Schools</span>
unescape <- function(x) {
  x |>
    str_replace_all("&amp;", "&") |>
    str_replace_all("&#039;|&rsquo;|’", "'") |>
    str_replace_all("&quot;", "\"")
}

matches <- str_match_all(
  html,
  '<span class="code">\\s*([A-Z][0-9][0-9A-Z])\\s*-\\s*</span>\\s*<span class="activity">([^<]*)</span>'
)[[1]]

raw <- tibble(
  ntee_code   = matches[, 2],
  description = matches[, 3] |> unescape() |> str_squish()
)

# Validate the raw parse before collapsing it. The page can change its
# markup, and a partial parse must fail here rather than turn a missed code
# into a false "retired by the IRS".
conflicting <- raw |>
  distinct(ntee_code, description) |>
  count(ntee_code) |>
  filter(n > 1)
if (nrow(conflicting) > 0) {
  stop("Same code parsed with different descriptions: ",
       paste(conflicting$ntee_code, collapse = ", "))
}
stopifnot(
  all(str_detect(raw$ntee_code, "^[A-Z][0-9][0-9A-Z]$")),
  all(nchar(raw$description) >= 4),
  !any(str_detect(raw$description, "[<>]"))
)

irs <- raw |>
  distinct(ntee_code, .keep_all = TRUE) |>
  arrange(ntee_code) |>
  mutate(irs_source = "Instructions for Form 1023, Appendix D",
         irs_revision = revision)

# Compare with the committed snapshot. Fewer codes than last time is far more
# likely a parse problem than an IRS removal; a real removal is accepted by
# rerunning with ACCEPT_IRS_REMOVALS=1 after checking the IRS page by eye.
if (file.exists(OUT_CSV)) {
  previous <- read_csv(OUT_CSV, show_col_types = FALSE)
  dropped  <- setdiff(previous$ntee_code, irs$ntee_code)
  if (length(dropped) > 0 && !nzchar(Sys.getenv("ACCEPT_IRS_REMOVALS"))) {
    stop(sprintf(
      "%d code(s) in the committed snapshot are missing from this parse: %s. ",
      length(dropped), paste(dropped, collapse = ", ")),
      "Check the IRS page. If the IRS really removed them, rerun with ACCEPT_IRS_REMOVALS=1.")
  }
  if (length(dropped) > 0) cat("Accepted IRS removals:", paste(dropped, collapse = ", "), "\n")
} else if (nrow(irs) < 600) {
  stop("Only ", nrow(irs), " codes parsed and no committed snapshot to compare against.")
}

write_csv(irs, OUT_CSV)
cat(sprintf("IRS list: %d codes (Form 1023 instructions revised %s) written to %s\n",
            nrow(irs), revision, OUT_CSV))

ours <- openxlsx::read.xlsx(LOOKUP, sheet = "ntee_code") |>
  as_tibble() |>
  mutate(ntee_code = str_trim(ntee_code))

missing_from_lookup <- irs |> anti_join(ours, by = "ntee_code")
not_in_irs          <- ours |> anti_join(irs, by = "ntee_code") |> filter(ntee_code != "Z99")

cat("\nIRS codes missing from the lookup sheet:", nrow(missing_from_lookup), "\n")
if (nrow(missing_from_lookup) > 0) print(missing_from_lookup |> select(ntee_code, description), n = Inf)

cat("\nLookup codes the IRS no longer lists (kept for old records; no action unless they cause problems):",
    nrow(not_in_irs), "\n")
if (nrow(not_in_irs) > 0) print(not_in_irs |> select(ntee_code, ntee_code_definition), n = Inf)
