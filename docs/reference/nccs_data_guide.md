# Guide to Using NCCS Data

**Source:** https://nccs.urban.org/pubs/nccs-data-guide.pdf
**Authors:** NCCS / Urban Institute (Thomas Pollak et al.)
**Last Updated:** ~2013

This guide provides practical research context for working with IRS nonprofit data, including BMF limitations, NTEE classification, data quality caveats, and organizational category definitions. It complements the IRM 25.7.1 technical reference with researcher-facing guidance.

---

## IRS Data Ecosystem Overview

The IRS releases three types of exempt organization databases:

1. **Business Master File (BMF):** Cumulative file of all active tax-exempt organizations. Data derived from Forms 1023/1024 (applications for exempt status), updated monthly. Contains ~57 descriptive variables and two key financial variables (assets and income) for ~1.5M+ organizations. The BMF is the primary input to this pipeline.

2. **Return Transaction Files (RTF):** Annual financial data from Form 990/990-EZ/990-PF filers. Contains up to 60 financial variables. Excludes organizations not required to file (religious orgs, those with <$25K gross receipts). Data entry is geared toward speed — errors occur.

3. **Statistics of Income (SOI) Sample Files:** Annual sample with 300+ variables. Includes all orgs with $30M+ assets plus stratified random sample of smaller orgs. Data entered twice with careful quality control. Used by NCCS to cross-check RTF data.

### NCCS-Derived Files
- **NCCS Cumulative Master File:** Cumulative list from BMF, includes "dead" organizations dating back to 1989
- **NCCS Core Files:** BMF descriptive data + RTF financial data, cleaned and verified against SOI
- **NCCS NTEE Master File:** Cumulative EIN-to-NTEE classification mappings (~800K organizations coded)

---

## BMF Data Quality Caveats

### "Active" Organizations
The BMF contains some inactive organizations. Prior to 2006, the IRS mailed postcards every three years to verify existence. **The BMF likely overstates the number of registered, functioning nonprofits.** A 1994 IRS study found:
- **21% of non-filing 501(c)(3) organizations** had either ceased operations or could not be found
- **27% of located organizations** had incorrect addresses

### Impact of Form 990-N (e-Postcard)
The Pension Protection Act of 2006 required small organizations to file Form 990-N to retain tax-exempt status. This:
- Confirmed continued existence of 400,000+ organizations
- Removed ~300,000 organizations from rolls due to failure to file within three years

### Address Data Quality
BMF addresses are often outdated, especially for non-filing organizations. NCCS recommends incorporating address cleaning into any survey project using BMF data.

### Financial Data Limitations
- Financial data should be used with caution — errors exist even after NCCS cleaning
- A single large organization (hospital, university) can account for 20%+ of nonprofit activity in a state
- Preparers of small organizations may not fully understand Form 990 complexities
- Organizations may shift expenses between categories to achieve desired ratios
- Aggregate measures are generally reliable; individual line items requiring exclusions or multiple calculations are less so

### Recommended Error Checking (4-Stage Process)
1. **Identify dominant organizations** that could mask trends of thousands of smaller orgs
2. **Verify geographic information and NTEE classifications** for accuracy/plausibility
3. **Check financial outliers** one return at a time (review Form 990 on GuideStar)
4. **Adjust data** where appropriate (impute for accounting changes, correct data entry errors)

---

## Organizations Not in the BMF

### Not Required to Register with IRS
- Public charities with less than $5,000 in gross receipts
- Churches, their integrated auxiliaries, and conventions/associations of churches (~341,000 churches were exempt from registering in the early 1990s; only ~220,000 have voluntarily registered)
- All private foundations must register regardless of size

### Not Required to File Form 990
1. Organizations with less than $25,000 in gross receipts (threshold slightly higher during first 3 years)
2. Religious organizations: congregations, mission societies, "exclusively religious activities," conventions/associations of churches
3. Church-affiliated schools below college level
4. State institutions with income excluded from gross income
5. Federal instrumentalities
6. Private foundations (file Form 990-PF instead)
7. Black lung benefit trusts
8. Stock bonus/pension/profit-sharing trusts
9. Religious/apostolic organizations under 501(d)
10. Foreign organizations with <$25K US gross receipts
11. Governmental units and affiliates
12. Certain political organizations

### Zero-Filers
When an organization not required to file Form 990 does so anyway, the IRS enters the EIN but not the financial information — all financial fields are recorded as zero. These "zero-filers" are generally excluded from NCCS analyses. In 2013 RTF: ~283,000 public charity zero-filers and ~178,000 under other subsections.

---

## Classification: NTEE-CC System

### Structure
The National Taxonomy of Exempt Entities - Core Codes (NTEE-CC) is a hierarchical mixed notation (letters + numbers) system:
- **~400 centile-level codes** (reduced from original 630)
- Collapsible into **26 major groups** (letter codes A-Z)
- Collapsible into **10 major categories**

### Major Categories and Groups

| Major Category | Major Groups |
|---|---|
| Arts | A |
| Education | B |
| Environment & Animals | C, D |
| Health Care | E, F, G, H |
| Human Services | I, J, K, L, M, N, O, P |
| International | Q |
| Religion-related | X |
| Mutual Benefit | Y |
| Public & Societal Benefit (other) | R, S, T, U, V, W |
| Unknown or Unclassified | Z |

### NTEE Code Sources and Confidence
- ~106,000 organizations classified by NCCS based on Form 990 Parts 3 and 8 program descriptions
- IRS began adding NTEE codes to new organizations in the 1995 BMF
- Each classification has a confidence level:
  - **A:** ≥90% probability major group classification is correct
  - **B:** Moderate confidence
  - **C:** Lower confidence
- NTEE-CC adopted in 1999, revised May 2005

### NTEE vs NPC
- NTEE classifies **organizations** (one code per org)
- NPC (Nonprofit Program Classification) classifies **programs/activities** (multiple codes per org)

---

## Organizational Categories

### Public Charities vs Private Foundations (501(c)(3) only)

**Public Charities** (~90% of all 501(c)(3) organizations):
- Receive significant public support OR fall into automatic public charity categories
- ~70% must meet the **public support test**: normally receive 1/3 of support from public (including government and foundations), OR receive 10% from public with no more than 1/3 from investment/unrelated business income
- Remaining qualify as: medical care providers, congregations, educational institutions, governmental units, or supporting organizations

**Two IRS codes identify public charity qualification:**
1. **Foundation Code (FNDNCD):** Reflects organization type at time of IRS recognition — may be outdated
2. **REASON code:** From Form 990 Schedule A Part IV — reflects current self-reported status

**Private Foundations:**
- Created primarily to distribute money to public charities or individuals
- Most have substantial investments funding their giving
- Must distribute at least 5% of assets annually
- ALL must file Form 990-PF regardless of size
- Includes **operating foundations** (~4% of private foundations): run their own programs rather than grantmaking
- **Failed public charities:** Created as public charities but failed the public support test — legally identical to private foundations but practically very different

### Subtypes of Public Charities

1. **Operating Public Charities** (~85% of filers): Conduct programs (research, education, health care, etc.)
2. **Supporting Public Charities:** Distribute funds to operating charities (e.g., United Way, community foundations)
3. **Mutual Benefit Public Charities:** Provide private services to paying customers (<1% revenue from contributions). Anomalous among public charities but typical in non-501(c)(3) categories. Most non-501(c)(3) exempt orgs are mutual benefit (trade associations under 501(c)(6), social clubs under 501(c)(7), etc.)

**Important:** Distinguish between operating and supporting charities to avoid double-counting of finances.

### Group Filers and Group Exemptions
- Organizations with chapters/affiliates can receive **group exemption status** from IRS
- Saves each affiliate from applying individually for tax-exempt status
- Most groups do NOT file group returns — parent and affiliates file separate Form 990s
- BMF contains **Group Exemption Numbers (GEN)** for all entities under group status
- Group returns are identifiable by "GROUP RETURN" in organization name
- In 1992: only 332 group returns covering ~31,404 affiliates; 60%+ of affiliates in different states from parent

---

## Out-of-Scope Organizations

NCCS excludes certain organizations from analyses (~2,155 in 2013):

1. **Foreign organizations** (766)
2. **US Territory/overseas operations** (1,281)
3. **Organizations without geographic identifiers** (26)
4. **Independent Sector exclusions** (42)
5. **Governmental organizations** (40) — most difficult to classify

### Governmental Organization Challenges
- No BMF or RTF variable clearly distinguishes governmental from private nonprofit
- Only the SOI file (variable E019, from Schedule A Part VI) directly identifies governmental status
- Many organizations are "quasi-governmental" (e.g., hospital districts created by state legislatures with independent boards that also receive private donations)
- Lines between governmental and private nonprofit are often unclear

---

## Return Year Definitions

Four ways to define "return year":
1. **IRS Processing Year:** Year IRS processed the Form 990
2. **Fiscal Year:** End of filing organization's fiscal year
3. **Start Year / Tax Year:** Beginning of organization's fiscal year (used by IRS for naming SOI files and determining which Form 990 version to use)
4. **Circa Year / Profile Year:** Central time period for the data (used by NCCS Core files)

### Fiscal Year Distribution (Core 2010 PC)
- **53.9%** of public charities have calendar year (Dec fiscal year end)
- **28.0%** end in June — but these tend to be larger orgs (38.7% of expenses, 44.1% of assets)
- Core files include most recent return filed in last 3 calendar years covering last 3 fiscal years

### Year-to-Year Comparison Cautions
- Organizations with filing extensions may be missing from a single processing year
- A prominent university missing from one year's file may still be "alive and well"
- ~15,000 organizations filed in 1995 but not 1993, including 772 with $1M+ expenses — many received extensions, not dissolved
- Do not assume missing returns = defunct organization

---

## Ruling Date Caveats

- Ruling dates prior to 1965 are unreliable predictors of trends
- A ruling date does NOT necessarily coincide with the date the organization was formed
- The ruling date is when the organization received IRS recognition of tax-exempt status

---

## Historical Notes

- EOMF (Exempt Organizations Master File) merged with BMF on January 1, 1981
- Only active EOMF accounts were placed on the BMF; inactive accounts went to EO Inactive Retention Register (EOIRR)
- NTEE system developed in mid-1980s; NTEE-CC (Core Codes) adopted 1999, revised 2005
- IRS began scanning Form 990s in FY 1998 — early years may have missed organizations
- Form 990-N (e-Postcard) introduced by Pension Protection Act of 2006
