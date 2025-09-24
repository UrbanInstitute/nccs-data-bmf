# Formatting organization name

legal_suffixes <- list(
  # Common Corporation variations
  "INC" = c("INC", "INC-", "INC\\.", "INCORPORATED", " I N C", "IN C", "I NC"),
  "CORP" = c("CORP", "CORP\\.", "CORPORATION", "COR P", "CO RP"),
  "CO" = c("CO", "CO\\.", "COMPANY"),
  
  # Limited Liability Company variations
  "LLC" = c("LLC", "LLC.", "L L C", "L.L.C.", "LIMITED LIABILITY CO"),
  "LTD" = c("LTD", "LTD.", "LIMITED"),
  
  # Professional Corporation variations
  "PC" = c("PC", "PC.", "P C"),
  
  # Trust variations
  "TR" = c("TR", "TR.", "TRUST"),
  "TUA" = c("TUA", "TUA.", "T.U.A.", "TRUST UNDER AGREEMENT"),
  "TUW" = c("TUW", "TUW.", "T.U.W.", "TRUST UNDER WILL")
)