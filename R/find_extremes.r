# https://stackoverflow.com/questions/6836409/finding-local-maxima-and-minima
# https://www.linkedin.com/in/jeffery-petit-4394498b/
locate_xtrem <- function (x, last = FALSE)
{
   # use rle to deal with duplicates
   x_rle <- rle(x)

   # force the first value to be identified as an extrema
   first_value <- x_rle$values[1] - x_rle$values[2]

   # differentiate the series, keep only the sign, and use 'rle' function to
   # locate increase or decrease concerning multiple successive values.
   # The result values is a series of (only) -1 and 1.
   #
   # ! NOTE: with this method, last value will be considered as an extrema
   diff_sign_rle <- c(first_value, diff(x_rle$values)) %>% sign() %>% rle()

   # this vector will be used to get the initial positions
   diff_idx <- cumsum(diff_sign_rle$lengths)

   # find min and max
   diff_min <- diff_idx[diff_sign_rle$values < 0]
   diff_max <- diff_idx[diff_sign_rle$values > 0]

   # get the min and max indexes in the original series
   x_idx <- cumsum(x_rle$lengths)
   if (last) {
      min <- x_idx[diff_min]
      max <- x_idx[diff_max]
   } else {
      min <- x_idx[diff_min] - x_rle$lengths[diff_min] + 1
      max <- x_idx[diff_max] - x_rle$lengths[diff_max] + 1
   }
   # just get number of occurences
   min_nb <- x_rle$lengths[diff_min]
   max_nb <- x_rle$lengths[diff_max]

   # format the result as a tibble
   bind_rows(
      tibble(Idx = min, Values = x[min], NB = min_nb, Status = "min"),
      tibble(Idx = max, Values = x[max], NB = max_nb, Status = "max")) %>%
      arrange(.data$Idx) %>%
      mutate(Last = last) %>%
      mutate_at(vars(.data$Idx, .data$NB), as.integer)
}
