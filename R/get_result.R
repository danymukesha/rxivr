#' Get Result
#'
#' This function returns the results from medRxiv/bioRxiv databases using the provided search parameters.
#'
#' @param url A URL to search for results.
#' @return A page from medRxiv/bioRxiv databases.
#' @examples
#' url <- "https://www.medrxiv.org/search/abstract_title%3ACOVID-19%20abstract_title_flags%3Amatch-all%20limit_from%3A2020-01-01%20limit_to%3A2021-01-01%20numresults%3A75%20sort%3Apublication-date%20direction%3Adescending%20format_result%3Acondensed"
#' get_result(url)
get_result <- function(url) {
  response <- try_success(function()
    httr2::request(url) |> httr2::req_perform(), 2)
  return(xml2::read_html(httr2::resp_body_string(response)))
}
