adjacency_to_matrix <- function(adjPairs, addReflexive = FALSE) {
  stopifnot(ncol(adjPairs) >= 2)
  adjPairs <- as.data.frame(adjPairs)
  qf <- factor(adjPairs[, 1])
  if (addReflexive) {
    adjPairs <- rbind(adjPairs,
                      matrix(rep(levels(qf), each = ncol(adjPairs)),
                             byrow = TRUE, ncol = ncol(adjPairs)))
    qf <- factor(adjPairs[, 1])
  }
  m <- table(qf, adjPairs[, 2])
  dimnames(m) <- unname(dimnames(m))
  t(m)
}
