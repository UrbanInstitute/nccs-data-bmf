# Overview

This repository contains the code and documentation needed to harmonize prior and current Business Master File (BMF) extracts from NCCS and the IRS respectively. It also contains the code needed to create the unified BMF, that aggregates unique records from all prior BMFs to create a consolidated list of active and inactive nonprofit organizations.

# NCCS Legacy BMFs

NCCS previously processed each BMF released by the IRS by creating separate data dictionaries and schemas for each dictionary. 

Our current data engineering methodology harmonizes columns across each year, ensuring that variables are consistently named and have standardized data types. 

Additionally, we discard metadata variables that cannot be reconstructed either due to missing documentation or removal by the IRS from later BMF releases.

# IRS BMF Extracts

Currently released IRS BMF Extracts are also harmonized with the following metadata variables created from the harmonized variables:

* `EIN2` is created from `EIN` by adding the following strings between the 9 digit EIN: `EIN-XX-XXXXXXX`. This ensures that the EIN is treated as a string and not a integer when saved in read in from `.csv` files without explicit data type handling.
* `NTEEV2` is created from the `NTEE_IRS` column containing NTEE codes reported by nonprofits in the Form 990.

# Creating the Unified BMF

Updating the BMF requires the following steps:

1. Download the latest BMF data from the IRS.
2. Create and clean the latest columns.
3. Geocode the BMF and append various FIPs codes to the Latitude and Longitude columns.
4. Update the financial columns with the latest e-filed data.
5. Update the existing unified BMF with the 2025 data.

These steps need to be run periodically whenever a new BMF is published.

## Documentation 

[BMF Research Guidebook](https://urbaninstitute.github.io/nccs-data-bmf/00-documentation/GUIDEBOOK/index.html)
