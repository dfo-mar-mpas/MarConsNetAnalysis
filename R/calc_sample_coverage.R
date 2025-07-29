#' Calculate Chao's Sample Coverage Estimator
#'
#' This function calculates sample coverage for a given species by sample abundance matrix or dataframe. Uses equation 4a from Chao, A. and Jost, L. (2012), Coverage-based rarefaction and extrapolation: standardizing samples by completeness rather than size. Ecology, 93: 2533-2547. https://doi.org/10.1890/11-1952.1
#'
#' @param x species by sample abundance matrix or dataframe. Rows represent samples and columns represent species.
#' @param permutations integer number of permutations to calculate sample coverage.
#'
#' @returns A list containing the sample coverage matrix, means, and standard deviations.
#' @export
#'
#' @examples
#' data(BCI,package = "vegan")
#' SC <- calc_sample_coverage(BCI[1:30,1:50])
#' ggplot(as.data.frame(SC[-1]), aes(x = N, y = means)) +
#'   geom_line() +
#'   geom_ribbon(aes(ymin = means - sd, ymax = means + sd), alpha = 0.2) +
#'   labs(title = "Sample Coverage", x = "Sample Size", y = "Coverage") +
#'   theme_minimal()
#'
calc_sample_coverage <- function(x, permutations = 100) {
  result <- list()
  result$SC <- matrix(NA, nrow = nrow(x), ncol = permutations)
  result$N <- 1:nrow(x)
  for (j in 1:ncol(result$SC)) {
    samples <- x[sample(1:nrow(x), nrow(x), replace = T), ]
    for (i in 1:nrow(result$SC)) {
      data <- apply(samples[1:i,], 2, sum, na.rm = TRUE)
      a1 <- sum(data == 1, na.rm = TRUE)
      a2 <- sum(data == 2, na.rm = TRUE)
      N <- sum(data, na.rm = TRUE)
      s0 <- sum(data > 0, na.rm = TRUE)
      result$SC[i, j] <- 1 -
        (a1 / N) * (((N - 1) * a1) / ((N - 1) * a1 + 2 * a2))
    }
  }

  result$means <- apply(result$SC, 1, mean, na.rm = TRUE)
  result$sd <- apply(result$SC, 1, sd, na.rm = TRUE)
  return(result)
}
