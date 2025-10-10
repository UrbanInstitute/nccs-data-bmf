# Script to process subsection and classification codes

library(tibble)
library(lubridate)

# Create the tibble::tribble with the complete table
# Format: SCD Type 2 dimension
effective_date <- lubridate::ymd("2025-12-13")
dim_irs_exempt_status <- tibble::tribble(
  ~subsection_code, ~classifiction_code, ~classification, ~subsection, ~effective_start_date, ~effective_end_date, ~is_current,
  1, 1, "Government instrumentality", "501(c)(1)", effective_date, NA, TRUE,
  2, 1,      "Title-holding corporation",                                        "501(c)(2)",      effective_date, NA, TRUE,
  3,     1,      "Charitable Organization",                                          "501(c)(3)",      effective_date, NA, TRUE,
  3,     2,      "Educational organization",                                         "501(c)(3)",      effective_date, NA, TRUE,
  3,     3,      "Literary organization",                                            "501(c)(3)",      effective_date, NA, TRUE,
  3,     4,      "Organization to prevent cruelty to animals",                       "501(c)(3)",      effective_date, NA, TRUE,
  3,     "5",      "Organization to prevent cruelty to children",                      "501(c)(3)",      effective_date, NA, TRUE,
  3,     "6",      "Organization for public safety testing",                           "501(c)(3)",      effective_date, NA, TRUE,
  3,     "7",      "Religious organization",                                           "501(c)(3)",      effective_date, NA, TRUE,
  3,     "8",      "Scientific organization",                                          "501(c)(3)",      effective_date, NA, TRUE,
  "04",     1,      "Civic league",                                                     "501(c)(4)",      effective_date, NA, TRUE,
  "04",     2,      "Local association of employees",                                   "501(c)(4)",      effective_date, NA, TRUE,
  "04",     3,      "Social welfare organization",                                      "501(c)(4)",      effective_date, NA, TRUE,
  "05",     1,      "Agriculture organization",                                         "501(c)(5)",      effective_date, NA, TRUE,
  "05",     2,      "Horticulture organization",                                        "501(c)(5)",      effective_date, NA, TRUE,
  "05",     3,      "Labor organization",                                               "501(c)(5)",      effective_date, NA, TRUE,
  "06",     1,      "Board of Trade",                                                   "501(c)(6)",      effective_date, NA, TRUE,
  "06",     2,      "Business league",                                                  "501(c)(6)",      effective_date, NA, TRUE,
  "06",     3,      "Chamber of Commerce",                                              "501(c)(6)",      effective_date, NA, TRUE,
  "06",     4,      "Real estate board",                                                "501(c)(6)",      effective_date, NA, TRUE,
  "07",     1,      "Pleasure, recreational, or social club",                           "501(c)(7)",      effective_date, NA, TRUE,
  "08",     1,      "Fraternal beneficiary society, order or association",              "501(c)(8)",      effective_date, NA, TRUE,
  "09",     1,      "Voluntary employees’ beneficiary association (Non-government)",    "501(c)(9)",      effective_date, NA, TRUE,
  "09",     2,      "Voluntary employees’ beneficiary association (Government emp.’s)", "501(c)(9)",      effective_date, NA, TRUE,
  "10",     1,      "Domestic fraternal societies and associations",                    "501(c)(10)",     effective_date, NA, TRUE,
  "11",     1,      "Teachers’ retirement fund association",                            "501(c)(11)",     effective_date, NA, TRUE,
  "12",     1,      "Benevolent life insurance association",                            "501(c)(12)",     effective_date, NA, TRUE,
  "12",     2,      "Mutual ditch or irrigation company",                               "501(c)(12)",     effective_date, NA, TRUE,
  "12",     3,      "Mutual or cooperative telephone company",                          "501(c)(12)",     effective_date, NA, TRUE,
  "12",     4,      "Mutual electric company, mutual water company, etc.",              "501(c)(12)",     effective_date, NA, TRUE,
  "13",     1,      "Burial association",                                               "501(c)(13)",     effective_date, NA, TRUE,
  "13",     2,      "Cemetery company",                                                 "501(c)(13)",     effective_date, NA, TRUE,
  "14",     1,      "Credit Union",                                                     "501(c)(14)",     effective_date, NA, TRUE,
  "14",     2,      "Other mutual corporation or association",                          "501(c)(14)",     effective_date, NA, TRUE,
  "15",     1,      "Mutual insurance company or association other than life or marine","501(c)(15)",     effective_date, NA, TRUE,
  "16",     1,      "Corporation financing crop operation",                             "501(c)(16)",     effective_date, NA, TRUE,
  "17",     1,      "Supplemental unemployment compensation trust or plan",             "501(c)(17)",     effective_date, NA, TRUE,
  "18",     1,      "Employee funded pension trust (created before 6–25–59)",           "501(c)(18)",     effective_date, NA, TRUE,
  "19",     1,      "Post or organization of war veterans",                             "501(c)(19)",     effective_date, NA, TRUE,
  "20",     1,      "Legal Services Organization",                                      "501(c)(20)",     effective_date, NA, TRUE,
  "21",     1,      "Black Lung",                                                       "501(c)(21)",     effective_date, NA, TRUE,
  "22",     1,      "Multiemployer Pension Plan",                                       "501(c)(22)",     effective_date, NA, TRUE,
  "23",     1,      "Veterans Association Founded Prior to 1880",                       "501(c)(23)",     effective_date, NA, TRUE,
  "24",     1,      "Trust described in Section 4049 of ERISA",                         "501(c)(24)",     effective_date, NA, TRUE,
  "25",     1,      "Title Holding Corporation or Trust",                               "501(c)(25)",     effective_date, NA, TRUE,
  "26",     1,      "State-sponsored High-Risk Health Insurance Org. Effective (1/1/99)","501(c)(26)",    effective_date, NA, TRUE,
  "27",     1,      "State-sponsored Worker’s Compensation Reinsurance Organization (Effective 1/1/99)", "501(c)(27)", effective_date, NA, TRUE,
  "28",     1,      "National Railroad Retirement Investment Trust Classification",     "501(c)(28)",     effective_date, NA, TRUE,
  "29",     1,      "Qualified non profit Health Insurance Issuers",                    "501(c)(29)",     effective_date, NA, TRUE,
  "40",     1,      "Apostolic and religious organization",                             "501(d)",         effective_date, NA, TRUE,
  "50",     1,      "Cooperative hospital service organization",                        "501(e)",         effective_date, NA, TRUE,
  "60",     1,      "Cooperative service organizations of operating education org’s",   "501(f)",         effective_date, NA, TRUE,
  "70",     1,      "Child care under 501(k)",                                          "501(k)",         effective_date, NA, TRUE,
  "71",     1,      "Charitable Risk Pool (Effective 1/1/99)",                          "501(n)",         effective_date, NA, TRUE,
  "80",     1,      "Farmers’ Cooperative",                                             "521",            effective_date, NA, TRUE,
  "81",     1,      "Qualified State-Sponsored Tuition Program",                        "529",            effective_date, NA, TRUE,
  "82",     1,      "IRC 527",                                                          "527",            effective_date, NA, TRUE,
  "90",     1,      "Non-exempt charitable trust 4947(a)(2) (Split Interest)",          "4947(a)(2)",     effective_date, NA, TRUE,
  "91",     1,      "Non-exempt charitable trust (Public Charity)",                     "4947(a)(1)",     effective_date, NA, TRUE,
  "92",     1,      "Non-exempt charitable trust (Trust treated as Private Foundation)","4947(a)(1)",     effective_date, NA, TRUE,
  "93",     1,      "Taxable Farmer’s Cooperative",                                     "1381(a)(2)",     effective_date, NA, TRUE
)

# Print the resulting tibble
print(dim_irs_exempt_status_full)

# Check the structure
str(dim_irs_exempt_status_full)