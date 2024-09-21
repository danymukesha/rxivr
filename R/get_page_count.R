#' Get Page Count
#'
#' Retrieve the page count from the search results.
#'
#' @param response A httr2 response object.
#' @return The page count.
#' @examples
#' page_count <- get_page_count(response)
get_page_count <- function(response) {
  total_results <- response |>
    httr2::resp_body_html() |>
    purrr::pluck("messages", 1, "total")
  
  if (length(total_results) == 0) {
    stop("Total results not found in response")
  }
  
  page_count <- ceiling(as.numeric(total_results) / 75)
  
  return(page_count)
}