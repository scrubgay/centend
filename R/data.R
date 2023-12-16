#' Real property transfer codes
#'
#' Codes used by the Florida Department of Revenue to describe qualities of sales
#' as, for example, arm's length, bank transfers, foreclosures, under duress, etc.
#'
#' @format ## `transfer_codes`
#' A data frame with 34 rows and 4 columns:
#' \describe{
#'   \item{code}{Code found in Florida Sales Data File}
#'   \item{short_desc}{A short description of the sale type}
#'   \item{long_desc}{The verbatim description of the sale type from the Florida Department of Revenue}
#'   \item{distressed}{Whether a sale type is one that is generally made under duress}
#' }
#'
#' @source https://floridarevenue.com/property/Documents/salequalcodes_bef01012019.pdf
"transfer_codes"

#' A list of Florida places
#'
#' Places as defined by the United States census refers to incorporated cities,
#' towns, and villages, as well as unincorporated but important named localities.
#' This list was compiled from the 2020 United States census list of geographies.
#'
#' @format ## `florida_places`
#' A data frame with 964 rows and 3 columns:
#' \describe{
#'   \item{County}{The county in which the place is found}
#'   \item{Place}{The name of the place}
#'   \item{Type}{City, town, or unincorporated census-designated place}
#' }
#'
#' @source United States Census Bureau, 2020 geographies
"florida_places"
