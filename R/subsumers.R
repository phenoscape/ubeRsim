#' Obtains a subsumer matrix
#'
#' A subsumer matrix M for terms \eqn{j \in \{1, \dots, n\}}{j in {1, ..., n}}
#' has value \eqn{M_{i,j}=1}{M[i,j] = 1} iff class _i_ (which can be an anonymous class expression) subsumes term _j_, and zero
#' otherwise. Therefore, it will have _n_ columns, one for each term.
#'
#' In this implementation, for each row _i_
#' \eqn{\sum_{j=1}^{n}M_{i,j} > 0}{sum(M[i, 1:n] > 0}. That is, each row
#' will have at least one non-zero value, which means that the number of classes
#' _not_ subsuming a term will be highly incomplete, because the (usually
#' very many) classes not subsuming any of the terms will not be included. This
#' subsumer matrix is thus only useful for similarity metrics for which
#' non-subsuming terms can be ignored.
#'
#' @references
#' Adapted from and originally developed as part of the [Rphenoscape](https://rphenoscape.phenoscape.org)
#' package.
#'
#' @param terms character, the list of terms as IRIs for which to compute the subsumer
#'   matrix.
#' @param .colnames character, how to name the columns of the resulting
#'   matrix.
#'   - `"ID"` (the default): use the term IDs (the last component of the
#'     term IRIs).
#'   - `"IRI"`: use the term IRIs.
#'   - `"label"`: use the terms' labels (see `.labels` parameter).
#' @param .labels character, the labels for terms. Only used if
#'   `.colnames = "label"`, and then must have the same length and ordering
#'   as `terms`.
#' @param preserveOrder logical, whether to return columns in the same
#'   order as `terms`. The default is not to preserve the order.
#' @param verbose logical, whether to print informative messages about certain
#'   potentially time-consuming operations.
#' @return A matrix representing the subsumer matrix
#'
#'   The matrix will have additional attributes depending on the choice of how to
#'   name rows and columns. If `.colnames = "ID"` (the default), the matrix will have
#'   an attribute `prefixes` giving the URL prefixes removed from the term IRIs
#'   to yield the IDs, in the order of the rows. If `.colnames = "label"`, it will
#'   have attribute `term.iris`, giving the term IRIs for the rows (and columns).
#'   Note that these extra attributes will be lost upon subsetting the returned
#'   matrix.
#' @examples
#' tl <- c("http://purl.obolibrary.org/obo/UBERON_0000981",
#'         "http://purl.obolibrary.org/obo/UBERON_0002103",
#'         "http://purl.obolibrary.org/obo/UBERON_0000976",
#'         "http://purl.obolibrary.org/obo/UBERON_0002102")
#' m <- subsumer_matrix(tl)
#' m[1:5,] # term IDs as column names
#' attr(m, "prefixes") # 4x "http://purl.obolibrary.org/obo/"
#'
#' m <- subsumer_matrix(tl, .colnames = "label")
#' m[1:5,] # term labels as column names
#' attr(m, "term.iris") # term IRIs in the same order as columns
#' @importFrom stringi stri_match_first_regex
#' @export
subsumer_matrix <- function(terms,
                            .colnames = c("ID", "IRI", "label"), .labels = NULL,
                            preserveOrder = FALSE,
                            verbose = FALSE) {
  .colnames <- match.arg(.colnames)
  term_iris <- terms
    # unname(sapply(terms,
    #               function(x) get_term_iri(x, as = "anatomy",
    #                                        exactOnly = TRUE, verbose = verbose)))
  # if (any(is.na(term_iris))) {
  #   warnings()
  #   stop("Could not resolve all term names to IRIs.", call. = FALSE)
  # }
  edgeTbl <- subsumer_pairs(terms = term_iris)
  m <- adjacency_to_matrix(edgeTbl)
  if (preserveOrder) {
    reordering <- match(term_iris, colnames(m))
    m <- m[, reordering]
  }
  if (.colnames == "ID") {
    parts <- stringi::stri_match_first_regex(colnames(m), "(^.+[/#])(.+$)")
    colnames(m) <- parts[,3]
    attr(m, "prefixes") <- parts[,2]
  } else if (.colnames == "label") {
    stopifnot(!is.null(.labels), length(.labels) == length(terms))
    nameMap <- match(colnames(m), term_iris)
    attr(m, "term.iris") <- colnames(m)
    colnames(m) <- .labels[nameMap]
  }
  m
}

subsumer_pairs <- function(terms,
                           .graph = "http://reasoner.renci.org/redundant") {
  qterms <- paste0("<", terms, ">", collapse = " ")
  query <- paste0("PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX part_of: <http://purl.obolibrary.org/obo/BFO_0000050>
SELECT ?s ?o (STR(MIN(?sLabel)) AS ?subj) (STR(MIN(?oLabel)) AS ?obj)
WHERE {
  GRAPH <", .graph, "> {
      VALUES ?s { ", qterms, " }
      ?s (rdfs:subClassOf|part_of:) ?o .
  }
  ?s rdfs:label ?sLabel .
  ?o rdfs:label ?oLabel .
}
GROUP BY ?s ?o")
  query_sparql(sparql_query = query, endpoint = UBERGRAPH_ENDPOINT)
}

UBERGRAPH_ENDPOINT <- "https://ubergraph.apps.renci.org/sparql"

