# Script Header
# Description: This script contains global variables used in
# the quarto book
# Programmer: Thiyaghessan Poongundranar - tpoongundranar@urban.org
# Date Created: 2024-07-01
# Date Last Modified: 2024-07-01

# List of file paths used in harmonization
FILE_PATH_LS <- list(
  raw = "data/raw",
  harmonized = "data/harmonized"
)

# Crosswalk used for harmonization
BMF_XWALK <- readxl::read_excel("data/crosswalks/bmf_crosswalk.xlsx", 
                                sheet = "BMF")