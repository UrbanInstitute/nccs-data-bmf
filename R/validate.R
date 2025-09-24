# Data validation checks

# EIN Check
ein_rule <- validate::validator(grepl("^\\d{2}-\\d{7}$", ein))
validate::summary(validate::confront(bmf_2025_preprocessed, ein_rule))
