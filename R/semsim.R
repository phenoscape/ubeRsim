#' Compute semantic similarity metrics between terms
#'
#' @description
#' The Tanimoto similarity ST is computed according to the definition for bit vectors
#' (see [Jaccard index at Wikipedia](https://en.wikipedia.org/wiki/Jaccard_index#Tanimoto's_definitions_of_similarity_and_distance)).
#' For weights \eqn{W_i \in \{0, 1\}}{W[i] in {0, 1}} it is the same as the
#' Jaccard similarity.
#' The Tanimoto similarity can be computed for any term vectors, but for 1 - ST
#' to be a proper distance metric satisfying the triangle inequality,
#' \eqn{M_{i,j} \in \{0, W_i\}}{M[i,j] in {0, W[i]}} must hold.
#'
#' @references
#' Originally developed as part of the [Rphenoscape](https://rphenoscape.phenoscape.org)
#' package.
#'
#' @param subsumer_mat  matrix or data.frame, the vector-encoded matrix M of
#'   subsumers for which \eqn{M_{i,j} = W_i, W_i > 0}{M[i,j] = W[i], with W[i] > 0} (W = weights),
#'   if class _i_ subsumes term j, and 0 otherwise. A binary
#'   (\eqn{M_{i,j} \in \{0, 1\}}{M[i,j] in {0, 1}}) encoding (i.e., W\[_i_\] = 1)
#'   can be obtained from [subsumer_matrix()].
#' @param terms character, optionally the list of terms (as IRIs and/or labels)
#'   for which to generate a properly encoded subsumer matrix on the fly.
#' @param ... parameters to be passed on to [subsumer_matrix()]
#'   if a subsumer matrix is to be generated on the fly.
#' @return A matrix with M\[i,j\] = similarity of terms _i_ and _j_.
#' @name similarity
#' @rdname similarity
#' @export
tanimoto_similarity <- function(subsumer_mat = NA, terms = NULL, ...) {
  if (missing(subsumer_mat)) {
    subsumer_mat <- subsumer_matrix(terms = terms, ...)
  }
  # numerator matrix = subsumers in the intersection set of i and j
  smi <- crossprod(as.matrix(subsumer_mat))
  # the diagonal is the subsumers of each term i
  nsubsumers <- diag(smi)
  # denominator matrix: |A|^2 + |B|^2 - A\dot B
  denom <- -smi + nsubsumers # add as columns
  denom <- t(t(denom) + nsubsumers) # add as rows
  # Tanimoto similarity is the ratio
  smi / denom
}

#' @description
#' The Jaccard similarity is computed using the Tanimoto similarity definition
#' for bit vectors
#' (see [Jaccard index at Wikipedia](https://en.wikipedia.org/wiki/Jaccard_index#Tanimoto's_definitions_of_similarity_and_distance)).
#' For the results to be a valid Jaccard similarity, weights must be zero and
#' one. If any weights are different, a warning is issued.
#'
#' @examples
#' sm <- jaccard_similarity(terms = c("pelvic fin", "pectoral fin",
#'                                    "forelimb", "hindlimb",
#'                                    "dorsal fin", "caudal fin"),
#'                          .colnames = "label")
#' sm
#'
#' # e.g., turn into distance matrix, cluster, and plot
#' plot(hclust(as.dist(1-sm)))
#' @export
#' @rdname similarity
jaccard_similarity <- function(subsumer_mat = NA, terms = NULL, ...) {
  if (missing(subsumer_mat)) {
    subsumer_mat <- subsumer_matrix(terms = terms, ...)
  }
  if (any(subsumer_mat != 1 & subsumer_mat > 0)) {
    warning("Some non-zero weights in the subsumer matrix are not equal to 1. ",
            "Jaccard similarity requires weights of zero or one.")
  }
  tanimoto_similarity(subsumer_mat = subsumer_mat)
}

#' @description
#' The Cosine similarity _SC_ is computed using the Euclidean dot product formula.
#' See [Cosine similarity on Wikipedia](https://en.wikipedia.org/wiki/Cosine_similarity#Definition).
#' The metric is valid for any term vectors (columns of the subsumer matrix), i.e.,
#' \eqn{M_{i,j} \in \{0, W_i\}}{M[i,j] in {0, W[i]}} is not required. Note that
#' 1 - _SC_ is not a proper distance metric, because it violates the triangle
#' inequality. First convert to angle to obtain a distance metric.
#'
#' @export
#' @rdname similarity
cosine_similarity <- function(subsumer_mat = NA, terms = NULL, ...) {
  if (missing(subsumer_mat)) {
    subsumer_mat <- subsumer_matrix(terms = terms, ...)
  }
  # numerator matrix = A \dot B
  smi <- crossprod(as.matrix(subsumer_mat))
  # diagonal = vector magnitudes squared = ||A||^2
  vecmag <- sqrt(diag(smi))
  # denominator matrix: ||A|| * ||B||
  res <- smi / vecmag # divide as columns
  res <- t(t(res) / vecmag) # divide as rows
  res
}


