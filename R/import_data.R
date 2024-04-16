
import_data <- function(verbose = TRUE) {
  if (verbose) {
    message("Importing data...")
  }

  url <- 'https://www2.bbv.com.bo/mercados/montos-negociados/instrumentos-negociados/'

  # Scrap data
  result <- tryCatch({
    read.scrap <- suppressWarnings(read_html(url))
    data.scrap <- function(chart, row, col) {
      vector1 <- paste('div.box-table.principales-', chart, '-table.mt-3', sep = "")
      vector2 <- paste('ul.list-table.show-popup-instr:nth-child(', row, ')', sep = "")
      vector3 <- paste('li.list-table__item', col, '', sep = "")

      '.' <- NULL
      read.scrap %>%
        html_nodes(vector1) %>%
        html_nodes(vector2) %>%
        html_nodes(vector3) %>%
        html_text() %>%
        gsub("\n", "", .) %>%
        unique() %>%
        trimws()
    }

    # Column 1
    par1.1 <- list(chart = 'alzas', row = 1:20, col = '.pizarra')
    list1.1 <- lapply(par1.1$row, data.scrap, chart = par1.1$chart, col = par1.1$col)

    par2.1 <- list(chart = 'bajas', row = 20:1, col = '.pizarra')
    list2.1 <- lapply(par2.1$row, data.scrap, chart = par2.1$chart, col = par2.1$col)

    merge1 <- c(list1.1, list2.1)
    c1 <- Filter(function(x) length(x) > 0, merge1)

    # Column 2
    par1.2 <- list(chart = 'alzas', row = 1:20, col = ':nth-child(2)')
    list1.2 <- lapply(par1.2$row, data.scrap, chart = par1.2$chart, col = par1.2$col)

    par2.2 <- list(chart = 'bajas', row = 20:1, col = ':nth-child(2)')
    list2.2 <- lapply(par2.2$row, data.scrap, chart = par2.2$chart, col = par2.2$col)

    merge2 <- c(list1.2, list2.2)
    c2 <- Filter(function(x) length(x) > 0, merge2)

    # Column 3
    par1.3 <- list(chart = 'alzas', row = 1:20, col = '.t-right:nth-child(3)')
    list1.3 <- lapply(par1.3$row, data.scrap, chart = par1.3$chart, col = par1.3$col)

    par2.3 <- list(chart = 'bajas', row = 20:1, col = '.t-right:nth-child(3)')
    list2.3 <- lapply(par2.3$row, data.scrap, chart = par2.3$chart, col = par2.3$col)

    merge3 <- c(list1.3, list2.3)
    c3 <- Filter(function(x) length(x) > 0, merge3)

    # Column 4
    par1.4 <- list(chart = 'alzas', row = 1:20, col = '.t-right:nth-child(4)')
    list1.4 <- lapply(par1.4$row, data.scrap, chart = par1.4$chart, col = par1.4$col)

    par2.4 <- list(chart = 'bajas', row = 20:1, col = '.t-right:nth-child(4)')
    list2.4 <- lapply(par2.4$row, data.scrap, chart = par2.4$chart, col = par2.4$col)

    merge4 <- c(list1.4, list2.4)
    c4 <- Filter(function(x) length(x) > 0, merge4)

    # Column 5
    par1.5 <- list(chart = 'alzas', row = 1:20, col = '.green.t-right:nth-child(5)')
    list1.5 <- lapply(par1.5$row, data.scrap, chart = par1.5$chart, col = par1.5$col)

    par2.5 <- list(chart = 'bajas', row = 20:1, col = '.red.t-right:nth-child(5)')
    list2.5 <- lapply(par2.5$row, data.scrap, chart = par2.5$chart, col = par2.5$col)

    merge5 <- c(list1.5, list2.5)
    c5 <- Filter(function(x) length(x) > 0, merge5)

    # Data frame
    df <- data.frame(
      'TICKER SYMBOL' = unlist(c1),
      'ISSUER NAME' = unlist(c2),
      'PRICE' = unlist(c3),
      '%RATE' = unlist(c4),
      '%VAR.' = unlist(c5))
    colnames(df) <- c('TICKER SYMBOL', 'ISSUER NAME', 'PRICE', '%RATE', '%VAR.')

    if (verbose) {
      message("Data imported successfully.")
    }

    # Output return
    df
  }, error = function(e) {
    message('Apologies. The market opens on weekdays at 6:00 am, Bolivian Standard Time (UTC-4).')
  })

  return(result)
}
