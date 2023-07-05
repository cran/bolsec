
get_list <- function(data) {

  # Format data
  list.df <- formattable(data, align = 'c',
                        caption = "<center>List of securities that show the
                      largest <b>positive</b> and <b>negative</b> price
                      variations during the day in the Bolivian Securities
                        Exchange.</center>")

  return(list.df)
}
