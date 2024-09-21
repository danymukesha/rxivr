#' A class to represent a Search
#' 
#' @field query The query used to fetch the papers
#' @field since The lower bound (inclusive) date of search
#' @field until The upper bound (inclusive) date of search
#' @field limit The max number of papers that can be returned in the search
#' @field limit_per_database The max number of papers that can be returned in the search for each database
#' @field processed_at The datetime when the search was performed
#' @field databases List of databases where the search should be performed
#' @field publication_types List of publication types to filter when searching
#' @field papers A list of papers already collected
#' @export
Search <- R6::R6Class("Search",
                      public = list(
                        query = NULL,
                        since = NULL,
                        until = NULL,
                        limit = NULL,
                        limit_per_database = NULL,
                        processed_at = NULL,
                        databases = NULL,
                        publication_types = NULL,
                        papers = NULL,
                        
                        #' Initialize a new Search object
                        #'
                        #' @param query The query used to fetch the papers
                        #' @param since The lower bound (inclusive) date of search
                        #' @param until The upper bound (inclusive) date of search
                        #' @param limit The max number of papers that can be returned in the search
                        #' @param limit_per_database The max number of papers that can be returned in the search for each database
                        #' @param processed_at The datetime when the search was performed
                        #' @param databases List of databases where the search should be performed
                        #' @param publication_types List of publication types to filter when searching
                        #' @param papers A list of papers already collected
                        #' @return A new `Search` object.
                        initialize = function(query, since = NULL, until = NULL, limit = NULL, limit_per_database = NULL, processed_at = NULL, databases = NULL, publication_types = NULL, papers = NULL) {
                          self$query <- query
                          self$since <- since
                          self$until <- until
                          self$limit <- limit
                          self$limit_per_database <- limit_per_database
                          self$processed_at <- ifelse(!is.null(processed_at), processed_at, Sys.time())
                          self$databases <- databases
                          self$publication_types <- publication_types
                          
                          self$paper_by_key <- list()
                          self$publication_by_key <- list()
                          self$paper_by_doi <- list()
                          self$papers_by_database <- list()
                          
                          self$papers <- papers
                          if (!is.null(papers)) {
                            for (paper in papers) {
                              tryCatch({
                                self$add_paper(paper)
                              }, error = function(e) {})
                            }
                          }
                        },
                        
                        #' Get a paper key
                        #'
                        #' @param paper_title Title of the paper
                        #' @param publication_date Date of publication
                        #' @param paper_doi DOI of the paper
                        #' @return The paper key
                        get_paper_key = function(paper_title, publication_date, paper_doi = NULL) {
                          if (!is.null(paper_doi)) {
                            return(paste0("DOI-", paper_doi))
                          } else {
                            return(paste0(tolower(paper_title), "|", ifelse(!is.null(publication_date), format(publication_date, "%Y"), "")))
                          }
                        },
                        
                        #' Get a publication key
                        #'
                        #' @param publication_title Title of the publication
                        #' @param publication_issn ISSN of the publication
                        #' @param publication_isbn ISBN of the publication
                        #' @return The publication key
                        get_publication_key = function(publication_title, publication_issn = NULL, publication_isbn = NULL) {
                          if (!is.null(publication_issn)) {
                            return(paste0("ISSN-", tolower(publication_issn)))
                          } else if (!is.null(publication_isbn)) {
                            return(paste0("ISBN-", tolower(publication_isbn)))
                          } else {
                            return(paste0("TITLE-", tolower(publication_title)))
                          }
                        },
                        
                        #' Add a paper to the search
                        #'
                        #' @param paper A paper object
                        #' @return NULL
                        add_paper = function(paper) {
                          if (length(paper$databases) == 0) {
                            stop("Paper cannot be added to search without at least one defined database")
                          }
                          
                          for (database in paper$databases) {
                            if (!is.null(self$databases) && !(tolower(database) %in% self$databases)) {
                              stop(paste0("Database ", database, " isn't in databases list"))
                            }
                            if (self$reached_its_limit(database)) {
                              stop("When the papers limit is provided, you cannot exceed it")
                            }
                          }
                          
                          if (!is.null(paper$publication)) {
                            publication_key <- self$get_publication_key(paper$publication$title, paper$publication$issn, paper$publication$isbn)
                            already_collected_publication <- self$publication_by_key[[publication_key]]
                            
                            if (!is.null(already_collected_publication)) {
                              already_collected_publication$enrich(paper$publication)
                              paper$publication <- already_collected_publication
                            } else {
                              self$publication_by_key[[publication_key]] <- paper$publication
                            }
                          }
                          
                          paper_key <- self$get_paper_key(paper$title, paper$publication_date, paper$doi)
                          already_collected_paper <- self$paper_by_key[[paper_key]]
                          
                          if ((is.null(self$since) || paper$publication_date >= self$since) &&
                              (is.null(self$until) || paper$publication_date <= self$until)) {
                            
                            if (is.null(already_collected_paper)) {
                              self$papers <- c(self$papers, paper)
                              self$paper_by_key[[paper_key]] <- paper
                              
                              if (!is.null(paper$doi)) {
                                self$paper_by_doi[[paper$doi]] <- paper
                              }
                              
                              for (database in paper$databases) {
                                if (is.null(self$papers_by_database[[database]])) {
                                  self$papers_by_database[[database]] <- list()
                                }
                                self$papers_by_database[[database]] <- c(self$papers_by_database[[database]], paper)
                              }
                            } else {
                              already_collected_paper$enrich(paper)
                            }
                          }
                        },
                        
                        #' Get a paper from the search
                        #'
                        #' @param paper_title Title of the paper
                        #' @param publication_date Date of publication
                        #' @param paper_doi DOI of the paper
                        #' @return The paper object
                        get_paper = function(paper_title = NULL, publication_date = NULL, paper_doi = NULL) {
                          if (!is.null(paper_doi)) {
                            return(self$paper_by_doi[[paper_doi]])
                          } else {
                            paper_key <- self$get_paper_key(paper_title, publication_date, paper_doi)
                            return(self$paper_by_key[[paper_key]])
                          }
                        },
                        
                        #' Get a publication from the search
                        #'
                        #' @param title Title of the publication
                        #' @param issn ISSN of the publication
                        #' @param isbn ISBN of the publication
                        #' @return The publication object
                        get_publication = function(title = NULL, issn = NULL, isbn = NULL) {
                          publication_key <- self$get_publication_key(title, issn, isbn)
                          return(self$publication_by_key[[publication_key]])
                        },
                        
                        #' Remove a paper from the search
                        #'
                        #' @param paper A paper object
                        #' @return NULL
                        remove_paper = function(paper) {
                          paper_key <- self$get_paper_key(paper$title, paper$publication_date, paper$doi)
                          self$paper_by_key[[paper_key]] <- NULL
                          
                          if (!is.null(paper$doi)) {
                            self$paper_by_doi[[paper$doi]] <- NULL
                          }
                          
                          for (database in paper$databases) {
                            self$papers_by_database[[database]] <- Filter(function(p) p != paper, self$papers_by_database[[database]])
                          }
                          
                          self$papers <- Filter(function(p) p != paper, self$papers)
                        },
                        
                        #' Merge duplicate papers based on similarity threshold
                        #'
                        #' @param similarity_threshold The similarity threshold to consider two papers as duplicates
                        #' @return NULL
                        merge_duplications = function(similarity_threshold = 0.95) {
                          paper_key_pairs <- combn(names(self$paper_by_key), 2, simplify = FALSE)
                          
                          for (pair in paper_key_pairs) {
                            paper_1_key <- pair[1]
                            paper_2_key <- pair[2]
                            paper_1 <- self$paper_by_key[[paper_1_key]]
                            paper_2 <- self$paper_by_key[[paper_2_key]]
                            
                            if (is.null(paper_1$publication_date) || is.null(paper_2$publication_date) ||
                                paper_1$publication_date$year != paper_2$publication_date$year ||
                                (!is.null(paper_1$doi) && !is.null(paper_2$doi) && paper_1$doi != paper_2$doi)) {
                              next
                            }
                            
                            max_title_length <- max(nchar(paper_1$title), nchar(paper_2$title))
                            max_edit_distance <- as.integer(max_title_length * (1 - similarity_threshold))
                            titles_edit_distance <- adist(tolower(paper_1$title), tolower(paper_2$title))
                            
                            if ((!is.null(paper_1$doi) && paper_1$doi == paper_2$doi) || (titles_edit_distance <= max_edit_distance)) {
                              paper_1$enrich(paper_2)
                              self$remove_paper(paper_2)
                            }
                          }
                        },
                        
                        #' Check if the limit of papers is reached for a given database
                        #'
                        #' @param database The database to check
                        #' @return TRUE if the limit is reached, FALSE otherwise
                        reached_its_limit = function(database) {
                          reached_general_limit <- !is.null(self$limit) && length(self$papers) >= self$limit
                          reached_database_limit <- !is.null(self$limit_per_database) && database %in% names(self$papers_by_database) &&
                            length(self$papers_by_database[[database]]) >= self$limit_per_database
                          return(reached_general_limit || reached_database_limit)
                        }
                      )
)

#' A method that returns a Search instance based on the provided dict object
#'
#' @param search_dict A dict that represents a Search instance
#' @return A Search instance based on the provided dict object
#' @name Search_fromDict
#' @export
Search$from_dict <- function(search_dict) {
  query <- search_dict$query
  limit <- search_dict$limit
  limit_per_database <- search_dict$limit_per_database
  
  since <- if (!is.null(search_dict$since)) as.Date(search_dict$since) else NULL
  until <- if (!is.null(search_dict$until)) as.Date(search_dict$until) else NULL
  processed_at <- if (!is.null(search_dict$processed_at)) as.POSIXct(search_dict$processed_at) else NULL
  
  databases <- search_dict$databases
  publication_types <- search_dict$publication_types
  
  papers <- list()
  if (!is.null(search_dict$papers)) {
    for (paper in search_dict$papers) {
      papers <- c(papers, Paper$from_dict(paper))
    }
  }
  
  return(Search$new(query, since, until, limit, limit_per_database, processed_at, databases, publication_types, papers))
}

#' A method that returns a dict object based on the provided Search instance
#'
#' @param search A Search instance
#' @return A dict that represents a Search instance
#' @name Search_toDict
#' @export
Search$to_dict <- function(search) {
  papers <- list()
  for (paper in search$papers) {
    papers <- c(papers, Paper$to_dict(paper))
  }
  
  papers <- papers[order(sapply(papers, function(x) x$publication_date), decreasing = TRUE)]
  
  number_of_papers_by_database <- list()
  for (database in names(search$papers_by_database)) {
    number_of_papers_by_database[[database]] <- length(search$papers_by_database[[database]])
  }
  
  return(list(
    query = search$query,
    since = if (!is.null(search$since)) format(search$since, "%Y-%m-%d") else NULL,
    until = if (!is.null(search$until)) format(search$until, "%Y-%m-%d") else NULL,
    limit = search$limit,
    limit_per_database = search$limit_per_database,
    processed_at = if (!is.null(search$processed_at)) format(search$processed_at, "%Y-%m-%d %H:%M:%S") else NULL,
    databases = search$databases,
    publication_types = search$publication_types,
    number_of_papers = length(papers),
    number_of_papers_by_database = number_of_papers_by_database,
    papers = papers
  ))
}