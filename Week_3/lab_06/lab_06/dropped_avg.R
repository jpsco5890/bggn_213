dropped_avg <- function(scores) {
  max_score <- max(scores)
  index <- scores >= max_score
  kept_scores <- scores[index]
  return(mean(kept_scores))
}
