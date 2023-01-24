#' @title Query a SPARQL endpoint
#' @description
#' Makes an HTTP POST request to a SPARQL endpoint with the given SPARQL query.
#'
#' @references
#' The code here was adapted from query.R in the WikidataQueryServiceR
#' package, developed originally by [Mikhail Popov](https://github.com/bearloga).
#'
#' @param sparql_query character, the SPARQL query
#' @param endpoint character, the SPARQL endpoint
#' @param format "simple" uses CSV and returns pure character data frame, while
#'   "smart" fetches JSON-formatted data and returns a data frame with datetime
#'   columns converted to `POSIXct`
#' @return A tibble data frame
#' @examples
#' sparql_query <- "SELECT
#'   ?softwareVersion ?publicationDate
#' WHERE {
#'   BIND(wd:Q206904 AS ?R)
#'   ?R p:P348 [
#'     ps:P348 ?softwareVersion;
#'     pq:P577 ?publicationDate
#'   ] .
#' }"
#' query_sparql(sparql_query, endpoint = "https://query.wikidata.org/sparql", format="smart")
#' @importFrom dplyr mutate_if as_tibble
#' @importFrom purrr map_chr map_lgl map_dfr
#' @export
query_sparql <- function(sparql_query, endpoint, format = c("simple", "smart")) {
  format <- match.arg(format)
  rate_limited_query <- rate_limiter(endpoint)

  if (format == "simple") {
    response <- rate_limited_query(sparql_query, httr::add_headers(Accept = "text/csv"))
    httr::stop_for_status(response)
    if (httr::http_type(response) == "text/csv") {
      content <- httr::content(response, as = "text", encoding = "UTF-8")
      return(readr::read_csv(content))
    } else {
      stop("returned response is not formatted as a CSV")
    }
  } else {
    response <- rate_limited_query(sparql_query, httr::add_headers(Accept = "application/sparql-results+json"))
    httr::stop_for_status(response)
    if (httr::http_type(response) == "application/sparql-results+json") {
      content <- httr::content(response, as = "text", encoding = "UTF-8")
      temp <- jsonlite::fromJSON(content, simplifyVector = FALSE)
    }
    if (length(temp$results$bindings) > 0) {
      data_frame <- purrr::map_dfr(temp$results$bindings, function(binding) {
        return(purrr::map_chr(binding, ~ .x$value))
      })
      datetime_columns <- purrr::map_lgl(temp$results$bindings[[1]], function(binding) {
        if ("datatype" %in% names(binding)) {
          return(binding[["datatype"]] == "http://www.w3.org/2001/XMLSchema#dateTime")
        } else
          return(FALSE)
      })
      data_frame <- dplyr::mutate_if(.tbl = data_frame,
                                     .predicate = datetime_columns,
                                     .funs = as.POSIXct,
                                     format = "%Y-%m-%dT%H:%M:%OS", tz = "GMT")
    } else {
      data_frame <- dplyr::as_tibble(matrix(character(),
                                            nrow = 0,
                                            ncol = length(temp$head$vars),
                                            dimnames = list(c(), unlist(temp$head$vars))))
    }
    return(data_frame)
  }
}

# NOT CURRENTLY USED
# impor tFrom ratelimitr limit_rate
rate_limiter <- function(endpoint) {
  req <- function(query, ...) {
    httr::POST(
      url = endpoint,
      query = list(query = query),
      httr::user_agent(ua()),
      ...
    )
  }
  # return(ratelimitr::limit_rate(req, rate(n = 30, period = 60)))
  return(req)
}

#' @importFrom utils packageName
#' @importFrom utils packageVersion
ua <- local({
  .ua <- NA;
  function() {
    if (is.na(.ua)) {
      pkg <- utils::packageName()
      versions <- c(paste0("r-curl/", utils::packageVersion("curl")),
                    paste0("httr/", utils::packageVersion("httr")),
                    paste0(pkg, "/", utils::packageVersion(pkg)))
      .ua <<- paste0(versions, collapse = " ")
    }
    .ua
  }
})

