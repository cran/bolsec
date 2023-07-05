
get_graph <- function(data) {

  # Clean data
  complete_data <- data[complete.cases(data[, c('%VAR.', '%RATE', 'ISSUER NAME')]), ]
  x <- as.numeric(complete_data$'%VAR.')
  y <- as.numeric(complete_data$'%RATE')

  # Plot data
  plot(x, y, pch = 16, cex = 1, col = "purple",
       xlim = c(min(x), max(x) * 1.1),
       ylim = c(min(y), max(y) * 1.1),
       xlab = '%VAR.', ylab = '%RATE',
       main = "Correlation: %RATE and %VAR. for all securities")

  text(x, y, labels = complete_data$`ISSUER NAME`, pos = 3, col = "black", cex = 0.68)

  # Correlation line & coefficient
  correlation <- cor(x, y)
  abline(lm(y ~ x), col = "red")
  text(0.8 * max(x), 0.9 * max(y), paste("Coefficient:",
                                           round(correlation, 2)), col = "red")
  return()
}
