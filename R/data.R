#' Election data from 7 countries
#'
#' A dataset containing election results for parliamentary houses from
#' Australia, Canada, Chile, Germany, Russia, UK, and USA. The variables are as follows:
#'
#' \itemize{
#'   \item year. The year of the election (1990-2025)
#'   \item country. The country the election took place within (Australia, Canada, Chile, Germany, Russia, UK, and USA)
#'   \item house. The parliamentary house of the election
#'   \item party_long. The full name of a party which had elected representatives
#'   \item party_short. The abbreviated name of a party which had elected representatives
#'   \item seats. The number of seats won by each party
#'   \item government. Whether or not that party was a part of the government following the election (1, 0)
#'   \item colour. A hex code indicating the colours of each party
#' }
#'
#' @docType data
#' @keywords datasets
#' @name election_data
#' @usage data(election_data)
#' @format A tibble with 312 rows and 8 variables
"election_data"
