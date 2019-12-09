
# function to create artificial trades data, with prices for each instrument drawn from a log-normal distribution
# each row represents an unique trading symbol
# arguments: number of instruments and maximum number of trades for a single instrument
symbol_data_creator <- function(
  n_instruments,
  max_n_trades = 100
) {
  dt <- data.table::data.table(symbol = 1:n_instruments)
  dt[, n_trades := sample(1:max_n_trades, 1), by = .(symbol)]
  dt[, price := round(x = rlnorm(n = 1, meanlog = 2), 3), by = .(symbol)]
  return(dt)
}


# function that aggregates data of symbols with some prices
# depending on each price and the breaks as input the data is combined
aggregate_by_price_breaks <- function(dat, price_breaks = c(0, 0.5, 1., 2.5, 10, Inf)) {
  
  dat <- data.table::copy(dat)
  dat[, price_range := cut(x = price, breaks = price_breaks)]
  dat[, interval_idx := findInterval(x = price, vec = price_breaks)]
  dat[, left := price_breaks[interval_idx]]
  dat[, right := price_breaks[interval_idx + 1]]
  work_dt <- dat[, .(n_trades = sum(n_trades)), by = .(left, right)]
  data.table::setorder(work_dt, left)
  return(work_dt)
  
}

wrapper_fun <- function(dat, min_data_points = 100) {
  dat <- data.table::copy(dat)
  n <- 0
  n_rows <- nrow(dat) + 1
  while (nrow(dat) < n_rows && n <= 10) {
    n_rows <- nrow(dat)
    dat <- top_down(dat = dat, min_data_points = min_data_points)
    n <- n + 1
  }
  n <- 0
  n_rows <- nrow(dat) + 1
  while (nrow(dat) < n_rows && n <= 10) {
    n_rows <- nrow(dat)
    dat <- bottom_up(dat = dat, min_data_points = min_data_points)
    n <- n + 1
  }
  return(dat)
  
}


top_down <- function(dat, min_data_points) {
  
  dat <- data.table::copy(dat)
  # create bool column if any row is already fine
  dat[, valid := n_trades >= min_data_points]
  
  # order dataset
  data.table::setorder(dat, left)
  # create cumulative sum & check if multiple 0s are avaible
  dat[, cumsum_valid := cumsum(valid)]
  
  if (nrow(dat[cumsum_valid == 0]) > 1) {
    cat("\nfound multiple zeros!")
    cat("\nreducing data set")
    dat[cumsum_valid == 0, n_trades := sum(n_trades)]
    dat[cumsum_valid == 0, left := min(left)]
    dat[cumsum_valid == 0, right := max(right)]
    # make unique
    dat <- unique(dat, by = c("left", "right", "n_trades"))
    
  }
  # remove columns
  dat$cumsum_valid <- NULL
  dat$valid <- NULL
  return(dat)
}


bottom_up <- function(dat, min_data_points) {
  dat <- data.table::copy(dat)
  data.table::setorder(dat, -left)
  # create bool column if any row is already fine
  dat[, valid := n_trades >= min_data_points]
  
  # create cumulative sum & check if multiple zeros are avaible
  dat[, cumsum_valid := cumsum(valid)]
  # in case only 1 zero is present combine it
  if (nrow(dat[cumsum_valid == 0]) == 1) {
    dat[cumsum_valid == 0, cumsum_valid := 1]
  }
  if (nrow(dat) != length(unique(dat$cumsum_valid))) {
    cat("\nfound multiple entries to be aggregated!")
    cat("\nreducing data set")
    dat <- dat[, .(left = min(left),
                   right = max(right),
                   n_trades = sum(n_trades)
    ),
    by = .(cumsum_valid)]
  } else {
    
    dat$valid <- NULL
  }
  dat$cumsum_valid <- NULL
  data.table::setorder(dat, left)
  return(dat)
  
  
}



dt <- symbol_data_creator(n_instruments = 100, max_n_trades = 50)
work_dt <- aggregate_by_price_breaks(dat = dt)
print(work_dt)
test_dt <- wrapper_fun(dat = work_dt, min_data_points = 1000)
print(test_dt)

# use info for aggregating
cut_vec <- unique(c(0, test_dt$left, test_dt$right), Inf)

dt[, price_range := cut(x = price, breaks = cut_vec)]
dt[, number_trades_per_range := sum(n_trades), by = .(price_range)]
