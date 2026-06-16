# Blast-radius measurement for the NTEE cleaner / nteev2 fix (ADR 0032).
# Reproduces CURRENT transform logic vs PROPOSED over the full current BMF.
# Read-only; no S3, no writes except a small summary to data/quality/.

suppressMessages({library(data.table); library(openxlsx)})

BMF <- "data/raw/bmf/2026-06-BMF.csv"
wb  <- "data/lookup/bmf_code_lookup.xlsx"

ntee_code_lookup <- as.data.table(read.xlsx(wb, sheet = "ntee_code"))
valid_lookup     <- ntee_code_lookup$ntee_code
INVALID <- "INVALID"; UNDEF <- "UNDEFINED"
valid_set <- c(INVALID, UNDEF, valid_lookup)

UNIV <- c("B40","B41","B42","B43","B50")
HOSP <- c("E20","E21","E22","E24")

dt <- fread(BMF, select = "NTEE_CD", colClasses = "character",
            showProgress = FALSE)
setnames(dt, "NTEE_CD", "raw0")
dt[, ntee_code_raw := toupper(trimws(as.character(raw0)))]
dt[is.na(ntee_code_raw), ntee_code_raw := ""]

dt[, `:=`(.len = nchar(ntee_code_raw),
          .first = substr(ntee_code_raw, 1, 1),
          .char23 = substr(ntee_code_raw, 2, 3))]
dt[, `:=`(.last = substr(ntee_code_raw, .len, .len),
          .int23 = suppressWarnings(as.integer(.char23)))]

# ---------------- CURRENT clean ----------------
dt[, clean_cur := fcase(
  .len == 0,                          UNDEF,
  !grepl("[A-Z]", .first),            INVALID,
  .len == 1,                          paste0(ntee_code_raw, "00"),
  .len == 2,                          paste0(ntee_code_raw, "0"),
  .len == 3,                          ntee_code_raw,
  .len == 4 & grepl("[A-Z]", .last),  substr(ntee_code_raw, 1, 3),
  .len == 4 & grepl("[0-9]", .last),  paste0(.first, .last, "0"),
  default = INVALID)]
dt[!clean_cur %chin% valid_set, clean_cur := INVALID]

# ---------------- PROPOSED clean (both .len==4 -> substr(1,3)) ----------------
dt[, clean_new := fcase(
  .len == 0,                          UNDEF,
  !grepl("[A-Z]", .first),            INVALID,
  .len == 1,                          paste0(ntee_code_raw, "00"),
  .len == 2,                          paste0(ntee_code_raw, "0"),
  .len == 3,                          ntee_code_raw,
  .len == 4,                          substr(ntee_code_raw, 1, 3),
  default = INVALID)]
dt[!clean_new %chin% valid_set, clean_new := INVALID]

# ---------------- CURRENT nteev2_code + subsector ----------------
dt[, code_cur := fcase(
  .len == 3 & .int23 <= 19, paste0(.first, "00"),
  .len == 4 & .int23 <= 19, paste0(.first, .last, "0"),
  default = "Z99")]
sub_fcase <- function(code, first) {
  fcase(
    code %chin% UNIV, "UNI",
    code %chin% HOSP, "HOS",
    first == "A", "ART",
    first == "B", "EDU",
    first %chin% c("C","D"), "ENV",
    first %chin% c("E","F","G","H"), "HEL",
    first %chin% LETTERS[9:16], "HMS",
    first == "Q", "IFA",
    first %chin% c("R","S","T","U","V","W"), "PSB",
    first == "X", "REL",
    first == "Y", "MMB",
    first == "Z", "UNU",
    default = "UNU")
}
dt[, sub_cur := sub_fcase(code_cur, .first)]

# ---------------- PROPOSED ----------------
# Step 2 (subsector keys off cleaned code). first-letter from cleaned code when
# valid, else from raw (so INVALID keeps current .first fallback for apples-to-
# apples; the invalid->UNU policy is reported separately below).
dt[, first_new := fifelse(clean_new %chin% c(INVALID, UNDEF), .first,
                          substr(clean_new, 1, 1))]
dt[, sub_new := sub_fcase(clean_new, first_new)]

# Broader nteev2_code rebuild: middle component = cleaned NTEE-CC code, Z99 if
# not a valid 3-char code.
dt[, code_new := fifelse(clean_new %chin% c(INVALID, UNDEF), "Z99", clean_new)]

# Variant: invalid/undefined -> UNU subsector (the "more correct" policy).
dt[, sub_new_unu := fifelse(clean_new %chin% c(INVALID, UNDEF), "UNU", sub_new)]

N <- nrow(dt)
cat(sprintf("\n==== NTEE fix blast radius — %s rows (2026-06 BMF) ====\n\n", format(N, big.mark=",")))

cat("--- (A) raw NTEE_CD nchar distribution ---\n"); print(table(dt$.len))

cat("\n--- (B) 4-char rows: top chars2-3 x char4 patterns ---\n")
b <- dt[.len == 4, .N, by = .(char23 = .char23, char4 = .last)][order(-N)]
print(head(b, 15))
cat(sprintf("4-char rows where char4=='0': %s of %s\n",
            format(dt[.len==4 & .last=="0", .N], big.mark=","),
            format(dt[.len==4, .N], big.mark=",")))
cat(sprintf("4-char rows where chars2-3 are common-code (<=19): %s\n",
            format(dt[.len==4 & .int23 <= 19, .N], big.mark=",")))

cat("\n--- (C) clean: CURRENT vs PROPOSED ---\n")
status <- function(x) fifelse(x==INVALID,"INVALID",fifelse(x==UNDEF,"UNDEFINED","valid"))
dt[, scur := status(clean_cur)][, snew := status(clean_new)]
print(dcast(dt[, .N, by=.(scur, snew)], scur ~ snew, value.var="N", fill=0))
cat(sprintf("recovered INVALID->valid: %s   regressed valid->INVALID: %s\n",
            format(dt[scur=="INVALID" & snew=="valid", .N], big.mark=","),
            format(dt[scur=="valid" & snew=="INVALID", .N], big.mark=",")))
cat("\nTop raw codes recovered (INVALID->valid):\n")
print(head(dt[scur=="INVALID" & snew=="valid", .N, by=.(ntee_code_raw, clean_new)][order(-N)], 15))
if (dt[scur=="valid" & snew=="INVALID", .N] > 0) {
  cat("\nTop raw codes regressed (valid->INVALID):\n")
  print(head(dt[scur=="valid" & snew=="INVALID", .N, by=.(ntee_code_raw, clean_cur)][order(-N)], 15))
}

cat("\n--- (D) nteev2_subsector: CURRENT vs PROPOSED (step-2, invalid keeps .first) ---\n")
print(dt[sub_cur != sub_new, .N, by=.(sub_cur, sub_new)][order(-N)])
cat(sprintf("\nUNI count  current: %s   proposed: %s\n",
            format(dt[sub_cur=="UNI",.N], big.mark=","), format(dt[sub_new=="UNI",.N], big.mark=",")))
cat(sprintf("HOS count  current: %s   proposed: %s\n",
            format(dt[sub_cur=="HOS",.N], big.mark=","), format(dt[sub_new=="HOS",.N], big.mark=",")))

cat("\n--- (E) university codes (clean_new in B40-B43,B50) ---\n")
cat(sprintf("orgs with proposed clean in university set: %s\n",
            format(dt[clean_new %chin% UNIV, .N], big.mark=",")))
cat("their CURRENT subsector:\n"); print(dt[clean_new %chin% UNIV, .N, by=sub_cur][order(-N)])
cat("their PROPOSED subsector:\n"); print(dt[clean_new %chin% UNIV, .N, by=sub_new][order(-N)])
cat("\nhospital codes (clean_new in E20-E24): "); cat(format(dt[clean_new %chin% HOSP,.N], big.mark=","), "\n")
cat("their CURRENT subsector:\n"); print(dt[clean_new %chin% HOSP, .N, by=sub_cur][order(-N)])

cat("\n--- (F) nteev2_code rebuild blast radius ---\n")
cat(sprintf("rows where code_new != code_cur: %s (%.1f%%)\n",
            format(dt[code_new != code_cur, .N], big.mark=","),
            100*dt[code_new != code_cur, .N]/N))
cat("top transitions:\n")
print(head(dt[code_new != code_cur, .N, by=.(code_cur, code_new)][order(-N)], 12))

cat("\n--- (G) invalid->UNU policy effect (informational) ---\n")
cat(sprintf("rows currently INVALID/UNDEF that get a non-UNU subsector today: %s\n",
            format(dt[clean_new %chin% c(INVALID,UNDEF) & sub_cur != "UNU", .N], big.mark=",")))
print(head(dt[clean_new %chin% c(INVALID,UNDEF) & sub_cur != "UNU", .N, by=sub_cur][order(-N)], 12))

cat("\n==== done ====\n")
