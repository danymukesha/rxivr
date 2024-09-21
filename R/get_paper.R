#' Get Paper
#'
#' This function creates a Paper object from metadata.
#' 
#' @param paper_metadata A list containing paper metadata.
#' @return A Paper object.
#' @examples
#' paper_metadata <- list(
#'   title = "COVID-19 and mental health",
#'   abstract = "Abstract content",
#'   authors = "Author1; Author2",
#'   date = "2020-01-01",
#'   doi = "10.1101/2020.01.01.20016730"
#' )
#' get_paper(paper_metadata)
get_paper <- function(paper_metadata) {
  paper_title <- paper_metadata$title
  paper_abstract <- paper_metadata$abstract
  paper_authors <- trimws(strsplit(paper_metadata$authors, ";")[[1]])
  paper_publication_date <- as.Date(paper_metadata$date, "%Y-%m-%d")
  paper_url <- sprintf("https://doi.org/%s", paper_metadata$doi)
  paper_doi <- paper_metadata$doi
  
  Paper <- tibble::tibble(
    title = paper_title,
    abstract = paper_abstract,
    authors = paper_authors,
    publication = NULL,
    publication_date = paper_publication_date,
    urls = list(paper_url),
    doi = paper_doi,
    citations = NULL,
    keywords = NULL,
    comments = NULL,
    number_of_pages = NULL,
    pages = NULL
  )
  
}
