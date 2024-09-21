#' Get Paper Metadata
#'
#' This function retrieves a paper's metadata given its DOI.
#'
#' @param doi The paper DOI.
#' @param database The database name (either "medRxiv" or "bioRxiv").
#' @return A list containing the medRxiv/bioRxiv paper metadata, or NULL if no metadata is available.
#' @examples
#' doi <- "10.1101/2020.01.01.20016730"
#' get_paper_metadata(doi, "medRxiv")
get_paper_metadata <- function(doi, database) {
  url <- sprintf("%s/details/%s/%s", API_BASE_URL, tolower(database), doi)
  response <- try_success(
    function()
      httr2::request(url) |> httr2::req_perform() |> httr2::resp_body_json(),
    2
  )
  if (!is.null(response) &&
      !is.null(response$collection) && length(response$collection) > 0) {
    return(response$collection[[1]])
  }
}
