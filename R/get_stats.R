get_stats <- function(data) {
  tryCatch({
    complete_data <- data[complete.cases(data[, c('PRICE', '%RATE', '%VAR.')]), ]

    # Clean data
    s3 <- gsub(",", "", complete_data$'PRICE')
    d3 <- as.numeric(s3)
    d4 <- as.numeric(complete_data$'%RATE')
    d5 <- as.numeric(complete_data$'%VAR.')

    c0 <- c('Mean', 'Median', 'St. Dev.', 'Min.', 'Max.')

    # Statistical functions
    mean_c3 <- mean(d3)
    median_c3 <- median(d3)
    sd_c3 <- sd(d3)
    min_c3 <- min(d3)
    max_c3 <- max(d3)
    c3 <- c(mean_c3, median_c3, sd_c3, min_c3, max_c3)

    mean_c4 <- mean(d4)
    median_c4 <- median(d4)
    sd_c4 <- sd(d4)
    min_c4 <- min(d4)
    max_c4 <- max(d4)
    c4 <- c(mean_c4, median_c4, sd_c4, min_c4, max_c4)

    mean_c5 <- mean(d5)
    median_c5 <- median(d5)
    sd_c5 <- sd(d5)
    min_c5 <- min(d5)
    max_c5 <- max(d5)
    c5 <- c(mean_c5, median_c5, sd_c5, min_c5, max_c5)

    # Data frame new variables
    df <- data.frame(
      ' ' = unlist(c0),
      'PRICE' = unlist(c3),
      '%RATE' = unlist(c4),
      '%VAR.' = unlist(c5))
    colnames(df) <- c(' ', 'PRICE', '%RATE', '%VAR.')

    # Format table
    stats.data <- formattable(df, align = 'c',
                              caption = "<center>Statistics of securities that show the
                      largest <b>positive</b> and <b>negative</b> price
                      variations during the day in the Bolivian Securities
                    Exchange.</center>")

    return(stats.data)
  }, error = function(e) {
    message('Apologies. The get_stats function does not accept NULL values.')
  })
}


