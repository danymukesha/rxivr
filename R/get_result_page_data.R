#' Get Result Page Data
#'
#' This function extracts result data from the given result page.
#' 
#' @param result_page A result page extracted from medRxiv/bioRxiv databases.
#' @return A list containing paper DOIs, total papers, and next page URL information.
#' @examples
#' result_page <- get_result("https://www.medrxiv.org/search/abstract_title%3ACOVID-19%20abstract_title_flags%3Amatch-all%20limit_from%3A2020-01-01%20limit_to%3A2021-01-01%20numresults%3A75%20sort%3Apublication-date%20direction%3Adescending%20format_result%3Acondensed")
#' get_result_page_data(result_page)
get_result_page_data <- function(result_page) {
  total_papers_text <- xml2::xml_text(xml2::xml_find_first(result_page, "//*[@id='page-title']"))
  if (grepl("no results", tolower(total_papers_text))) {
    total_papers <- 0
  } else {
    total_papers <- as.numeric(gsub(",", "", strsplit(total_papers_text, " ")[[1]][1]))
  }
  
  dois <- xml2::xml_text(xml2::xml_find_all(result_page, "//*[@class='highwire-cite-metadata-doi highwire-cite-metadata']"))
  dois <- gsub("https://doi.org/", "", trimws(dois))
  
  next_page_elements <- xml2::xml_find_all(result_page, "//*[@class='link-icon link-icon-after']")
  next_page_url <- if (length(next_page_elements) > 0) {
    paste0(BASE_URL, xml2::xml_attr(next_page_elements[1], "href"))
  } else {
    NULL
  }
  
  list(
    dois = dois,
    total_papers = total_papers,
    next_page_url = next_page_url
  )
}
