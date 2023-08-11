#' Tag owners
#' @description Categorize owners as one of Corporate, Trust, Governmental,
#' Trust services, Confidential, or Other depending on aspects of the name.
#' @param owner_name A character vector of owner names
#' @return A character vector with types
#' @examples
#' # example code
#' categorize_owner(c("fyr residential llc", "anne ray trust", "564 Newell Tr llc"))

tag_owners <- function(owner_name) {
  owner_name <- str_to_upper(owner_name)
  case_when(
    is.na(owner_name) |
      str_length(owner_name) <= 5 |
      str_detect(owner_name,
                 regex_or(
                   "CONFIDENTIAL",
                   "REF(ERENCE)? ONLY",
                   "UNKNOWN",
                   "N/A")
      ) ~ "Hidden or unknown",

    str_detect(owner_name,
               regex_or(
                 "(?<!REAL )ESTATE",
                 "LIVING TRUST",
                 "HEIRS",
                 chopper("LIFE ESTATE", "LIFE ES"),
                 chopper("REVOCABLE LIVING TRUST", "REVOC"),
                 chopper("REVOCABLE TRUST", "REVOC"),
                 chopper("LIVING TRUST", "LIVING"))
    ) ~ "Trust",

    str_detect(owner_name,
               regex_or(
                 "TRUST.*(COMPANY|SERVICE)",
                 "CUSTODIAN")
    ) ~ "Trust custodian",

    str_detect(owner_name,
               regex_or(
                 "IRA",
                 "401K",
                 "EST OF",
                 chopper("TRUST", "TR"),
                 "TRS")
    ) ~ "Trust",

    str_detect(owner_name,
               regex_or(
                 "UNITED STATES OF AMERICA",
                 "CITY OF",
                 "COUNTY",
                 "STATE OF",
                 chopper("HOUSING AUTHORITY", "HOUSING AUTH"),
                 "ELECTRIC AUTHORITY",
                 chopper("MANAGEMENT DISTRICT", "MANAGEMENT D"))
    ) ~ "Government",

    str_detect(owner_name, "\\d") ~ "Corporate",
    str_detect(owner_name, " (OF|AT|BY) ") ~ "Corporate",
    str_detect(owner_name,
               paste0(
                 " ",
                 regex_or(
                   "LLC",
                   "L L C",
                   "LP",
                   "L P",
                   "LLLP",
                   "L L L P",
                   "INC",
                   "I N C",
                   "L C",
                   "LC",
                   "LTD",
                   "LIMITED",
                   "PARTNERSHIP",
                   chopper("CORPORATION", "CORP"),
                   "CO$",
                   "COMPAN(Y|IES)",
                   "GROUP",
                   chopper("ASSOCIATION", "ASSOC"),
                   chopper("ASSOCIATES", "ASSOCIATE"),
                   as_word = FALSE)
               )
    ) ~ "Corporate",
    str_detect(owner_name,
               # paste0(
               #   " ",
               regex_or(
                 "BANK",
                 "MORTGAGE",
                 "RENTAL",
                 "MULTI",
                 chopper("APARTMENT", "APARTM"),
                 chopper("REAL ESTATE", "REAL EST"),
                 "VILLAS",
                 "REAL PROP",
                 "MARKET",
                 "REALTY",
                 "EQUIT(Y|IES)",
                 "RANCH CLUB",
                 "HOUSING",
                 "SERVICE",
                 "COUNTRY CLUB",
                 "OWNERS",
                 "HOLDINGS",
                 "RESIDENTIAL",
                 "LEASING",
                 "COMMUNITY",
                 "DEVELOPMENT",
                 "RENT ",
                 "HOMES ",
                 "HOMEOW",
                 "INVEST",
                 "CONDO",
                 "PROPERT",
                 "MANAGEMENT",
                 "REALTY",
                 "(JOINT |)VENTURE(|S)",
                 "BORROWER",
                 as_word = TRUE)
    ) ~ "Corporate",
    # str_detect(owner_name,
    #            regex_or(
    #              "CHURCH",
    #              "FELLOWSHIP",
    #              "CHRIST ",
    #              "METHODIST",
    #              "BAPTIST",
    #              "MINIST",
    #              "FIRST ",
    #              " HOLY",
    #              "MISSIONARY",
    #              as_word = FALSE)
    # ) ~ "Corporate",
    .default = "Other"
  )
}
