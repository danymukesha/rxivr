#' Get Results
#'
#' Extract the results from the search response.
#'
#' @param response A httr2 response object.
#' @return A tibble of search results.
#' @examples
#' results <- get_results(response)
get_results <- function(response) {
  papers_list <- response |>
    httr2::resp_body_html() |>
    purrr::pluck("collection")
  
  if (length(papers_list) == 0) {
    stop("No papers found in response")
  }
  
  results <- purrr::map_df(papers_list, function(paper) {
    list(
      title = paper$title,
      authors = paste(paper$authors, collapse = ", "),
      date = as.Date(paper$date),
      url = paper$url
    )
  })
  
  return(results)
}