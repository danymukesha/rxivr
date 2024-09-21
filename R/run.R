#' Run Search
#'
#' This function fetches papers from medRxiv/bioRxiv databases using the provided search parameters.
#' The collected papers are added to the provided search instance.
#' 
#' @param search A search instance containing query and date range.
#' @param database The database name (either "medRxiv" or "bioRxiv").
#' @examples
#' search <- list(query = "COVID-19", since = as.Date("2020-01-01"), until = as.Date("2021-01-01"))
#' run(search, "medRxiv")
#' 
#' 
#' Run the Search
#'
#' Run the search on medRxiv or bioRxiv database.
#'
#' @param search A search instance.
#' @param database The database name (medRxiv or bioRxiv).
#' @return A tibble of search results.
#' @examples
#' search <- list(query = "COVID-19", since = as.Date("2020-01-01"), until = as.Date("2021-01-01"))
#' results <- run(search, "medRxiv")

run <- function(search, database) {
  urls <- get_search_urls(search, database)
  all_results <- purrr::map_df(urls, function(url) {
    response <- httr2::request(url) |>
      httr2::req_perform()

    page_count <- get_page_count(response)

    results <- get_results(response)

    if (page_count > 1) {
      for (page in 2:page_count) {
        paged_url <- paste0(url, "&page=", page)
        paged_response <- httr2::request(paged_url) |>
          httr2::req_perform()

        paged_results <- get_results(paged_response)

        results <- dplyr::bind_rows(results, paged_results)
      }
    }

    return(results)
  })

  return(all_results)
}


# run <- function(search, database) {
#   urls <- get_search_urls(search, database)
#   
#   for (i in seq_along(urls)) {
#     if (search$reached_its_limit(database)) {
#       break
#     }
#     
#     logging::loginfo(sprintf("%s: Requesting for papers...", database))
#     
#     data <- get_data(urls[[i]])
#     
#     total_papers <- if (length(data) > 0) data[[1]]$total_papers else 0
#     
#     logging::loginfo(sprintf("%s: %d papers to fetch from %d/%d papers requests", database, total_papers, i, length(urls)))
#     
#     papers_count <- 0
#     dois <- unlist(lapply(data, function(d) d$dois))
#     
#     for (doi in dois) {
#       if (papers_count >= total_papers || search$reached_its_limit(database)) {
#         break
#       }
#       tryCatch({
#         papers_count <- papers_count + 1
#         paper_metadata <- get_paper_metadata(doi, database)
#         
#         paper_title <- paper_metadata$title
#         
#         logging::loginfo(sprintf("(%d/%d) Fetching %s paper: %s", papers_count, total_papers, database, paper_title))
#         
#         paper <- get_paper(paper_metadata)
#         paper$add_database(database)
#         
#         search$add_paper(paper)
#       }, error = function(e) {
#         logging::logdebug(e$message, exc_info = TRUE)
#       })
#     }
#   }
# }
