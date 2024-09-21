#' Get Data
#'
#' This function retrieves a list of data from medRxiv/bioRxiv given a search URL.
#' 
#' @param url A search URL.
#' @return A list of data from medRxiv/bioRxiv.
#' @examples
#' url <- "https://www.medrxiv.org/search/abstract_title%3ACOVID-19%20abstract_title_flags%3Amatch-all%20limit_from%3A2020-01-01%20limit_to%3A2021-01-01%20numresults%3A75%20sort%3Apublication-date%20direction%3Adescending%20format_result%3Acondensed"
#' get_data(url)
get_data <- function(url) {
  result_page <- get_result(url)
  page_data <- get_result_page_data(result_page)
  data <- list(page_data)
  
  if (!is.null(page_data$next_page_url)) {
    data <- c(data, get_data(page_data$next_page_url))
  }
  
  return(data)
}
