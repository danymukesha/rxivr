#' Replace Search Term Enclosures
#'
#' This function replaces the enclosures of search terms in a query.
#'
#' @param query A search query as a string.
#' @param open_replacement A string to replace the opening enclosure.
#' @param close_replacement A string to replace the closing enclosure.
#' @param only_on_wildcards A logical value indicating whether to replace enclosures only on wildcard terms (default is FALSE).
#' @return A transformed query as a string.
#' @export
#' @examples
#' replace_search_term_enclosures("[term1] AND [term2]", "(", ")", TRUE)
replace_search_term_enclosures <- function(query, open_replacement, close_replacement, only_on_wildcards = FALSE) {
  if (only_on_wildcards) {
    wildcards_apply <- function(search_term) {
      if (grepl("\\?|\\*", search_term)) {
        return(gsub("\\[", open_replacement, gsub("\\]", close_replacement, search_term)))
      } else {
        return(search_term)
      }
    }
    return(apply_on_each_term(query, wildcards_apply))
  } else {
    return(gsub("\\[", open_replacement, gsub("\\]", close_replacement, query)))
  }
}

#' Apply a Function to Each Term of the Query
#'
#' This function applies a given function to each term in a query.
#'
#' @param query A search query as a string.
#' @param fun A function to apply to each term.
#' @return A new query as a string with each term transformed by the provided function.
#' @export
#' @examples
#' apply_on_each_term("[term1] AND [term2]", toupper)
apply_on_each_term <- function(query, fun) {
  is_inside_a_term <- FALSE
  search_term <- ""
  final_query <- ""
  for (character in strsplit(query, "")[[1]]) {
    if (character == "[") {
      search_term <- paste0(search_term, character)
      is_inside_a_term <- TRUE
      next
    }
    if (is_inside_a_term) {
      search_term <- paste0(search_term, character)
      if (character == "]") {
        search_term <- fun(search_term)
        final_query <- paste0(final_query, search_term)
        search_term <- ""
        is_inside_a_term <- FALSE
      }
    } else {
      final_query <- paste0(final_query, character)
    }
  }
  return(final_query)
}

#' Get the Maximum Group Level of a Query
#'
#' This function returns the maximum group level of a query.
#'
#' @param query A search query as a string.
#' @return The maximum group level as an integer.
#' @export
#' @examples
#' get_max_group_level("term1 AND (term2 OR (term3 AND term4))")
get_max_group_level <- function(query) {
  current_level <- 0
  max_level <- 0
  for (character in strsplit(query, "")[[1]]) {
    if (character == "(") {
      current_level <- current_level + 1
      if (current_level > max_level) {
        max_level <- current_level
      }
    } else if (character == ")") {
      current_level <- current_level - 1
    }
  }
  return(max_level)
}

#' Get the Tree of a Query
#'
#' This function returns a tree representation of a query.
#'
#' @param query A search query as a string.
#' @param parent A parent node as a list (default is NULL).
#' @return The query tree as a list.
#' @export
#' @examples
#' get_query_tree("[term A] OR [term B] AND ([term C] OR [term D])")
get_query_tree <- function(query, parent = NULL) {
  if (is.null(parent)) {
    parent <- list(node_type = "root", children = list())
  }
  
  query_iterator <- strsplit(query, "")[[1]]
  current_connector <- NULL
  i <- 1
  
  while (i <= length(query_iterator)) {
    current_character <- query_iterator[i]
    
    if (current_character == "(") {  # is a beginning of a group
      if (!is.null(current_connector)) {
        parent$children <- append(parent$children, list(list(node_type = "connector", value = trimws(current_connector))))
        current_connector <- NULL
      }
      
      subquery <- ""
      subquery_group_level <- 1
      
      while (TRUE) {
        i <- i + 1
        if (i > length(query_iterator)) {
          stop("Unbalanced parentheses")
        }
        
        current_character <- query_iterator[i]
        
        if (current_character == "(") {  # has a nested group
          subquery_group_level <- subquery_group_level + 1
        } else if (current_character == ")") {
          subquery_group_level <- subquery_group_level - 1
          if (subquery_group_level == 0) {  # end of the group
            break
          }
        }
        
        subquery <- paste0(subquery, current_character)
      }
      
      group_node <- list(node_type = "group", children = list())
      parent$children <- append(parent$children, list(group_node))
      
      get_query_tree(subquery, group_node)
      
    } else if (current_character == "[") {  # is a beginning of a term
      if (!is.null(current_connector)) {
        parent$children <- append(parent$children, list(list(node_type = "connector", value = trimws(current_connector))))
        current_connector <- NULL
      }
      
      term_query <- ""
      
      while (TRUE) {
        i <- i + 1
        if (i > length(query_iterator)) {
          stop("Missing term closing bracket")
        }
        
        current_character <- query_iterator[i]
        
        if (current_character == "]") {
          break
        }
        
        term_query <- paste0(term_query, current_character)
      }
      
      parent$children <- append(parent$children, list(list(node_type = "term", value = term_query)))
      
    } else {  # is a connector
      if (is.null(current_connector)) {
        current_connector <- ""
      }
      current_connector <- paste0(current_connector, current_character)
    }
    
    i <- i + 1
  }
  
  return(parent)
}
