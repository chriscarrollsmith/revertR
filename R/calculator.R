#' Fetch Stock Price History For Given Tickers
#'
#' This function retrieves stock price history from various sources for a list of provided tickers.
#' Specifically, it makes use of the `tidyquant` and `quantmod` packages to fetch data.
#' Optionally, it can also retrieve the most current trading day's stock price.
#'
#' Please note: If you're using the Alpha Vantage source (`src = "av"`) for fetching today's prices,
#' ensure you have set your Alpha Vantage API key using `Sys.setenv(ALPHAVANTAGE_API_KEY = "your_key")`.
#'
#' @param tickers A character vector specifying the stock tickers you're interested in.
#' @param include_today Logical. If TRUE, the function will also fetch and include stock prices
#'                      for the current trading day. Default is FALSE.
#'
#' @return A data frame containing the historical stock prices. Each row represents a day of trading for a
#'         particular stock. The main columns are `symbol` (stock ticker), `date` (the trading date),
#'         and `adjusted` (the adjusted closing price).
#'
#' @importFrom tidyquant tq_get
#' @importFrom quantmod getQuote
#' @importFrom janitor clean_names
#' @importFrom dplyr arrange
#' @importFrom dplyr select
#' @importFrom dplyr filter
#' @importFrom dplyr bind_rows
#' @importFrom dplyr mutate
#' @importFrom magrittr %>%
#'
#' @examples
#' \dontrun{
#' Sys.setenv(ALPHAVANTAGE_API_KEY = "your_key_here")
#' get_prices(c("AAPL", "MSFT"), include_today = TRUE)
#' }
#'
#' @export

get_prices <- function(tickers, include_today = FALSE) {
  prices <- tidyquant::tq_get(tickers) %>%
    select(symbol, date, adjusted)

  if (include_today) {
    today_prices <- quantmod::getQuote(tickers, src = "av", api.key = Sys.getenv("ALPHAVANTAGE_API_KEY")) %>%
      janitor::clean_names() %>%
      mutate(symbol = rownames(.),
             adjusted = last,
             date = as.Date(trade_time)) %>%
      select(symbol, date, adjusted)

    prices <- prices %>%
      anti_join(today_prices, by = c("symbol", "date")) %>%
      bind_rows(today_prices)
  }

  arrange(prices, symbol, date)
}

#' Calculate Stock Price Returns
#'
#' The function `calculate_returns` computes the stock price returns for specified periods. It takes a data frame
#' containing stock price data and returns a new data frame with additional columns representing the returns
#' for the specified periods. Symbols with extremely aberrant 20-day returns are excluded from the result.
#'
#' @param df A data frame that contains at least the columns 'adjusted', 'date', and 'symbol'.
#' @param periods A numeric vector of non-negative integers representing the periods for which the returns are to be computed.
#'                Default periods are c(20, 50, 100, 200, 500, 1000).
#'
#' @return A data frame with the original data and additional columns containing the computed returns for the specified periods,
#'         excluding symbols with extremely aberrant 20-day returns.
#' @importFrom dplyr group_by arrange mutate filter pull
#' @importFrom rlang sym
#' @importFrom magrittr %>%
#'
#' @examples
#' \dontrun{
#'   df_with_returns <- calculate_returns(df)
#' }
#'
#' @export

calculate_returns <- function(df, periods = c(20, 50, 100, 200, 500, 1000)) {
  df <- df %>%
    group_by(symbol) %>%
    arrange(date)

  for (period in periods) {
    col_name <- paste0("return_", period)
    df <- df %>%
      mutate(
        !!rlang::sym(col_name) := (lead(adjusted, n = period) - adjusted) / adjusted
      )
  }

  # Filter out aberrant returns
  exclude <- df %>%
    filter(return_20 > 10 | return_20 < -0.95) %>%
    pull(symbol) %>%
    unique()

  df <- df %>% filter(!symbol %in% exclude)

  if (length(exclude) > 0) {
    print(paste("Symbol(s)",paste(exclude,collapse=", "),"excluded from result due to < -95% or > 1000% 20-day return (possible data quality issue)",sep = " "))
  }

  return(df)
}

#' Calculate Rolling Beta for Stocks
#'
#' The function `calculate_rolling_beta` computes the rolling beta for a given set of stocks relative to a specified index
#' over a specified window size. It takes a data frame containing daily adjusted prices for the stocks and a corresponding benchmark index data frame
#' and returns a data frame with the rolling beta values.
#'
#' @param df A data frame containing at least the columns 'symbol', 'date', and 'adjusted', representing the daily data for the stocks.
#' @param index A data frame containing price history for a benchmark index (e.g., "^GSPC" for the S&P 500 index). Default is S&P 500 price history fetched with tidyquant.
#' @param window_size Numeric value specifying the window size for the rolling calculation. Default is 2.
#' @param window_in_years Logical value indicating whether the window_size is provided in years (TRUE) or in trading days (FALSE). Default is TRUE.
#' @param partial Logical value indicating whether to calculate beta with partial data at the edges of the time series if full window size is not available. Default is TRUE.
#' @return A data frame with the original data (from `df`) and an additional 'rolling_beta' column containing the computed rolling beta values.
#' @importFrom dplyr group_by mutate inner_join select
#' @importFrom roll roll_cov roll_var
#' @importFrom tidyquant tq_get
#' @importFrom magrittr %>%
#' @examples
#' \dontrun{
#'   stocks_with_beta <- calculate_rolling_beta(df, window_size = 5)
#' }
#' @export

calculate_rolling_beta <- function(df, index = tq_get("^GSPC"), window_size = 2, window_in_years = TRUE, partial = TRUE) {
  # Convert window size in years to rows if necessary
  if (window_in_years) {
    window_size <- window_size * 252 # 252 trading days in a year
  }

  # Calculate percent return from the previous day for the stocks
  df <- df %>%
    group_by(symbol) %>%
    mutate(return = 100 * c(NA, diff(adjusted) / head(adjusted, -1)))

  # Calculate percent return from the previous day for the index
  index <- index %>%
    mutate(return = 100 * c(NA, diff(adjusted) / head(adjusted, -1))) %>%
    select(date, return)

  # Join stock returns with index returns
  df <- df %>%
    inner_join(index, by = "date", suffix = c("", "_index"))

  # Calculate rolling beta for each stock
  df <- df %>%
    group_by(symbol) %>%
    mutate(
      rolling_cov = roll::roll_cov(return, return_index, width = window_size, min_obs = if(partial){20}else{window_size}, complete_obs = TRUE, na_restore = TRUE),
      rolling_var = roll::roll_var(return_index, width = window_size, min_obs = if(partial){20}else{window_size}, complete_obs = TRUE, na_restore = TRUE),
      rolling_beta = rolling_cov / rolling_var
    ) %>%
    # Clean up the result
    select(-return, -return_index, -rolling_cov, -rolling_var)

  return(df)

}

#' Data Cleaning for Financial Time Series
#'
#' This function provides a specialized cleaning routine tailored for financial time series data.
#' It performs the following operations on the input data frame:
#' 1. Counts the number of observations for each ticker ('symbol').
#' 2. Filters out tickers with an insufficient number of observations (less than 1022).
#' 3. Filters out tickers that have more than one 'NA' in the 'adjusted' price column.
#' 4. Removes individual rows with 'NA' in the 'adjusted' price column.
#'
#' The cleaned data is expected to provide a more consistent and reliable dataset for financial analysis.
#'
#' @param df A data frame requiring cleaning. It must contain at least the columns 'symbol' and 'adjusted'.
#'          'symbol' represents the stock ticker, and 'adjusted' represents the adjusted closing prices of the stock.
#'
#' @return A cleaned data frame where each 'symbol' has been grouped and a new column 'n' added to
#'         indicate the number of observations for that 'symbol'. Symbols with less than 1022 observations
#'         or with more than one 'NA' in the 'adjusted' column have been removed. Individual rows with 'NA'
#'         in the 'adjusted' column are also removed.
#'
#' @importFrom dplyr group_by
#' @importFrom dplyr mutate
#' @importFrom dplyr filter
#' @importFrom dplyr n
#' @importFrom magrittr %>%
#'
#' @examples
#' \dontrun{
#' data <- data.frame(
#'   symbol = c(rep("AAPL", 5), rep("MSFT", 5)),
#'   adjusted = c(145, 146, NA, 148, 149, 220, 221, NA, 223, NA)
#' )
#' clean_data(data)
#' }
#'
#' @export

clean_data <- function(df, min_points = 1021) {
  #count length of each series
  df <- df %>%
    group_by(symbol) %>%
    mutate(n = n())

  #filter out tickers with too few data points
  df <- df %>% filter(n > min_points)

  #filter out NAs
  df <- df %>%
    group_by(symbol) %>%
    filter(sum(is.na(adjusted))<2) %>%
    filter(!is.na(adjusted))

  return(df)
}

#' Compute and Add Exponential Moving Averages (EMAs) to Data Frame
#'
#' The `add_emas` function enriches a given financial time series data frame by computing and adding columns
#' for Exponential Moving Averages (EMAs) for specified periods. EMAs provide insight into trends in financial data
#' by giving more weight to recent data points.
#'
#' For each unique stock ticker ('symbol') in the data frame, the function computes EMAs only if the number of
#' observations for that ticker is greater than the specified EMA period. If not, it leaves the EMA value as `NA`
#' for that ticker and period combination.
#'
#' @param df A data frame to which the EMAs are to be appended. This data frame should have at least the columns:
#'          'symbol' (indicating the stock ticker), 'adjusted' (representing the adjusted closing prices), and 'n'
#'          (the count of observations for each ticker).
#'
#' @param periods A numeric vector of non-negative integers representing the periods for which EMAs are to be
#'                computed. For instance, `c(5, 20, 50)` would compute EMAs for 5-day, 20-day, and 50-day periods.
#'
#' @return A data frame that includes the original data and additional columns for each EMA computed for the specified
#'         periods. The newly added EMA columns are named in the format: "ema_" followed by the period number (e.g.,
#'         "ema_5" for a 5-day EMA). Each EMA is computed independently for each unique 'symbol'. If a 'symbol'
#'         doesn't have sufficient data for a particular period, the corresponding EMA column will have `NA` values.
#'
#' @importFrom dplyr mutate
#' @importFrom TTR EMA
#' @importFrom magrittr %>%
#'
#' @examples
#' \dontrun{
#' data <- data.frame(
#'   symbol = rep("AAPL", 5),
#'   adjusted = c(145, 146, 148, 149, 150),
#'   n = c(5, 5, 5, 5, 5)
#' )
#' enhanced_data <- add_emas(data, c(3, 5))
#' }
#'
#' @export

add_emas <- function(df, periods = c(20, 50, 100, 200, 500, 1000)) {
  #create empty EMA columns
  for (period in periods) {
    df <- df %>%
      mutate(!!paste0("ema_", period) := NA)
  }

  #add EMAs
  for(symbol in unique(df$symbol)){
    tmp <- df[df$symbol == symbol,]
    for(period in periods) {
      if(tmp$n[1] > period) {
        tmp[[paste0("ema_", period)]] <- EMA(tmp$adjusted, n = period)
        df[[paste0("ema_", period)]][df$symbol == symbol] <- tmp[[paste0("ema_", period)]]
      }
    }
  }

  return(df)
}

#' Add Percentage Distances from Exponential Moving Averages (EMAs)
#'
#' The `add_pct_distances_from_emas` function enhances a financial time series data frame by calculating the percentage
#' distances from given EMAs.
#'
#' The function computes the percentage distance from each EMA as `(adjusted - EMA) / EMA`.
#'
#' @param df A data frame that contains at least the columns 'adjusted' and 'date'. It should also have the EMA columns
#'          for the given periods in the format "ema_" followed by the period number (e.g., "ema_5" for a 5-day EMA).
#' @param periods A numeric vector of non-negative integers representing the periods for which the metrics are to be
#'                computed. Default periods are c(20, 50, 100, 200, 500, 1000).
#' @return A data frame with original data and additional columns representing the percentage distance from the EMAs
#'         for the specified periods.
#' @importFrom dplyr mutate
#' @importFrom magrittr %>%
#' @export

add_pct_distances_from_emas <- function(df, periods = c(20, 50, 100, 200, 500, 1000)) {
  for(period in periods) {
    df <- df %>%
      mutate(!!paste0("distance_from_", period) := (adjusted - !!rlang::sym(paste0("ema_", period)))/!!rlang::sym(paste0("ema_", period)))
  }
  return(df)
}

#' Convert Percentage Distances to Z-Scores
#'
#' The function `convert_distances_to_zscores` computes the z-scores from percentage distances from given Exponential Moving Averages (EMAs).
#' You can specify whether to use only past data for calculations, and the window size for the rolling calculations.
#'
#' @param df A data frame that contains at least the columns 'adjusted', 'date', and percentage distances from EMAs for the specified periods.
#' @param periods A numeric vector of non-negative integers representing the periods for which the z-scores are to be computed. Default periods are c(20, 50, 100, 200, 500, 1000).
#' @param use_only_past_data A boolean indicating whether to use only past data for z-score calculations. If TRUE, z-scores are computed using rolling calculations. Default is FALSE.
#' @param window_size A numeric value specifying the rolling window size for z-score calculations. Can be given in years or rows. If NULL, the full history of data is used when `use_only_past_data` is TRUE. Default is NULL.
#' @param window_in_years A boolean indicating whether the `window_size` is given in years. If TRUE, `window_size` is converted to rows using 252 trading days in a year. Default is TRUE.
#' @return A data frame with the original data and additional columns containing the computed z-scores for the specified periods.
#' @importFrom dplyr mutate group_by arrange row_number
#' @importFrom roll roll_mean roll_sd
#' @importFrom purrr map map2
#' @importFrom tidyr nest unnest
#' @importFrom rlang sym
#' @importFrom magrittr %>%
#'
#' @examples
#' \dontrun{
#'   data <- convert_distances_to_zscores(data, periods = c(50, 100), use_only_past_data = TRUE, window_size = 2, window_in_years = TRUE)
#' }
#'
#' @export

convert_distances_to_zscores <- function(df, periods = c(20, 50, 100, 200, 500, 1000), use_only_past_data = FALSE, window_size = NULL, window_in_years = TRUE) {

  # Convert window size in years to rows if necessary
  if (!is.null(window_size) && window_in_years) {
    window_size <- window_size * 252 # 252 trading days in a year
  }

  df <- df %>%
    group_by(symbol) %>%
    arrange(date)

  if (!use_only_past_data) {
    for (period in periods) {
      df <- df %>%
        mutate(!!paste0("z_score_", period) := scale(!!rlang::sym(paste0("distance_from_", period))))
    }
  } else {
    for (period in periods) {
      distance_col <- paste0("distance_from_", period)
      z_score_col <- paste0("z_score_", period)

      if (is.null(window_size)) {
        window_size <- length(df[[distance_col]])
      }

      df <- df %>%
        mutate(
          mean = roll::roll_mean(!!rlang::sym(distance_col), width = window_size, min_obs = 1),
          sd = roll::roll_sd(!!rlang::sym(distance_col), width = window_size, min_obs = 1),
          !!z_score_col := (get(distance_col) - mean) / sd
        ) %>%
        select(-mean, -sd)
    }
  }

  return(df)
}

#' Compute Moving Averages of Z-Scores and Differences Between Z-Scores and Their Moving Averages
#'
#' This function calculates the moving averages of z-scores and the differences between the z-scores
#' and their moving averages. If the `use_only_past_data` parameter is TRUE, it computes the z-scores
#' using only prior data for each row.
#'
#' @param df A data frame with the z-scores and percentage distances from EMAs.
#' @param periods A numeric vector of non-negative integers representing the periods for which the metrics are to be
#'                computed. Default periods are c(20, 50, 100, 200, 500, 1000).
#' @param ma_period A single non-negative integer that sets the period for the simple moving average computation. Default is 20.
#' @param use_only_past_data A boolean. If TRUE, the z-scores will be calculated using only prior data for each row. Default is FALSE.
#' @return A data frame with the computed moving averages of z-scores and the differences between z-scores and their moving averages.
#' @importFrom dplyr mutate
#' @importFrom TTR SMA
#' @importFrom magrittr %>%
#' @importFrom rlang sym
#' @export

compute_moving_averages <- function(df, periods = c(20, 50, 100, 200, 500, 1000), ma_period = 20, use_only_past_data = FALSE) {
  for(period in periods) {
    z_score_col <- sym(paste0("z_score_", period))
    moving_average_col <- paste0("moving_average_z_score_", period)
    diff_col <- paste0("diff_z_score_", period)

    if (sum(!is.na(df[[as.character(z_score_col)]])) >= ma_period) {
      df <- df %>%
        mutate(
          !!moving_average_col := SMA(!!z_score_col, ma_period),
          !!diff_col := !!z_score_col - !!sym(moving_average_col)
        )
    } else {
      df[[moving_average_col]] <- NA
      df[[diff_col]] <- NA
    }
  }
  return(df)
}

#' Round Z-Scores, Moving Averages, and Their Differences
#'
#' This function rounds z-scores, moving averages of z-scores, and the differences between z-scores and their moving averages
#' to the nearest integer.
#'
#' @param df A data frame with z-scores, moving averages of z-scores, and the differences between z-scores and their moving averages.
#' @param periods A numeric vector of non-negative integers representing the periods for which the metrics are to be
#'                computed. Default periods are c(20, 50, 100, 200, 500, 1000).
#' @return A data frame with rounded z-scores, moving averages, and their differences.
#' @importFrom dplyr mutate
#' @importFrom magrittr %>%
#' @export

round_metrics <- function(df, periods = c(20, 50, 100, 200, 500, 1000)) {
  for(period in periods) {
    df <- df %>%
      mutate(
        !!paste0("z_score_stratum_", period) := round(!!sym(paste0("z_score_", period))),
        !!paste0("moving_average_z_score_stratum_", period) := round(!!sym(paste0("moving_average_z_score_", period))),
        !!paste0("diff_z_score_stratum_", period) := round(!!sym(paste0("diff_z_score_", period)))
      )
  }
  return(df)
}

#' Data Preparation for Return Forecasting
#'
#' The `prep_data_for_forecasting` function takes a vector of ticker symbols, retrieves their historical price data,
#' and prepares the data for further analysis. The data preparation encompasses several steps:
#' 1. Retrieval of historical price data for the provided tickers using the `get_prices` function.
#' 2. Cleaning of the data, which involves filtering out NAs, removing symbols with insufficient observations,
#'    and appending a column with the count of observations for each symbol using the `clean_data` function.
#' 3. Computation and addition of Exponential Moving Averages (EMAs) for specified periods via the `add_emas` function.
#' 4. Calculation and inclusion of z-score distances from the computed EMAs with the help of the
#'    `add_pct_distances_from_emas` function.
#'
#' @param tickers A character vector of ticker symbols for which the data needs to be prepared.
#'                Example: `c("AAPL", "GOOGL", "MSFT")`.
#' @param periods A numeric vector specifying the periods (in days) for which EMAs should be computed.
#'                For instance, `c(5, 20, 50)` would request the 5-day, 20-day, and 50-day EMAs.
#' @param include_today A logical value indicating whether today's price data should be included in the analysis.
#'                      If `TRUE`, the most recent price data will be appended. Defaults to `FALSE`.
#' @return A data frame prepared for analysis, encompassing cleaned price data, EMAs, z-score distances
#'         from the EMAs, and a column indicating the number of observations for each ticker symbol.
#'
#' @importFrom dplyr mutate filter group_by
#' @importFrom TTR EMA
#' @importFrom quantmod getQuote
#' @importFrom tidyquant tq_get
#' @importFrom janitor clean_names
#' @importFrom rlang sym
#' @importFrom magrittr %>%
#'
#' @examples
#' \dontrun{
#' prepared_data <- prep_data_for_forecasting(c("AAPL", "GOOGL"), c(5, 20))
#' }
#'
#' @export

prep_data_for_forecasting <- function(tickers, periods = c(20, 50, 100, 200, 500, 1000), include_today=FALSE) {
  # Get historical price data frame with columns `symbol`, `date`, `adjusted`
  df <- get_prices(tickers, include_today)

  # Filter out NAs, remove symbols with too few obs, add `n` of obs
  df <- clean_data(df)

  # Add exponential moving averages (EMAs)
  df <- add_emas(df, periods)

  # Add percent distances from EMAs
  df <- add_pct_distances_from_emas(df, periods)

  # Convert percent distances to z-scores
  df <- convert_distances_to_zscores(df, periods)

  # Compute 20-day moving averages of z-score distances and their difference
  # from the raw z-scores
  df <- compute_moving_averages(df, periods)

  # Round z-scores and other metrics to the nearest integer
  df <- round_metrics(df, periods)

  # Filter by latest date
  df <- df %>% filter(date == max(date))
}

#' Data Preparation for Analysis
#'
#' The `prep_data_for_analysis` function takes a vector of ticker symbols, retrieves their historical price data,
#' and prepares the data for further analysis.
#'
#' @export

prep_data_for_analysis <- function(tickers, periods = c(20, 50, 100, 200, 500, 1000), return_periods = c(20, 50, 100, 200, 500, 1000), ma_period_to_use = 20, window_size_to_use_in_years = 2, benchmark_index_ticker = "^GSPC", include_today=FALSE) {
  # Get historical price data frame with columns `symbol`, `date`, `adjusted`
  df <- get_prices(tickers, include_today)

  # Filter out NAs, remove symbols with too few obs, add `n` of obs
  df <- clean_data(df)

  # Calculate returns
  df <- calculate_returns(df, periods = return_periods)

  # Get date range from df and fetch corresponding benchmark index price history
  from_date <- min(df$date)
  to_date <- max(df$date)
  index <- tq_get(benchmark_index_ticker, from = from_date, to = to_date)

  # Calculate rolling 2-year beta
  df <- calculate_rolling_beta(df, index, window_size = window_size_to_use_in_years)

  # Add exponential moving averages (EMAs)
  df <- add_emas(df, periods)

  # Add percent distances from EMAs
  df <- add_pct_distances_from_emas(df, periods)

  # Convert % distance to z-score, using only past data points
  df <- convert_distances_to_zscores(df, use_only_past_data = TRUE, window_size = window_size_to_use_in_years)

  # Compute 20-day moving averages of z-score distances and their difference
  # from the raw z-scores
  df <- compute_moving_averages(df, periods, ma_period = ma_period_to_use)
}

#' Financial Data Analysis Function
#'
#' The `analyze_data` function performs an analysis on the provided financial data frame (`df`) based on pre-defined
#' stratum or stratums in the pre-calculated plotdata.
#' It calculates the expected returns for the designated stratum (or stratums) and computes their averages.
#' If a secondary stratum (`stratum2`) is provided, it factors that into the expected returns calculation.
#' Finally, the analyzed data is returned in a data frame that's sorted by the average expected return in descending order.
#'
#' @param df A data frame containing the financial data that needs to be analyzed.
#'           This data frame should have columns corresponding to the stratum (or stratums) specified.
#' @param plotdata A dataframe containing pre-calculated plot data.
#'                 This data is crucial for the expected returns calculation.
#' @param stratum1 A character string that designates the primary stratum based on which expected returns are computed.
#' @param stratum2 An optional character string indicating a secondary stratum to consider when calculating expected returns.
#'                 By default, it is set to `NULL`, meaning only `stratum1` is considered.
#' @return A data frame containing the financial data from `df` supplemented with the average expected returns
#'         for each observation, arranged in descending order of the average expected return.
#'
#' @importFrom purrr pmap_df
#' @importFrom dplyr select arrange
#' @importFrom tibble tibble
#' @importFrom magrittr %>%
#'
#' @details
#' The function processes the financial data using the following steps:
#' 1. It iteratively goes through each row of the `df` data frame.
#' 2. For each iteration (or row), it matches the stratum values with the precalculated plot data to get the expected return.
#' 3. If `stratum2` is provided, the function performs a dual-stratum matching.
#' 4. It then computes the average of all matched expected returns for the row.
#' 5. The resultant data frame contains the original data along with the average expected returns.
#'
#' @examples
#' \dontrun{
#' analyzed_data <- analyze_data(financial_df, plotdata, "stratum_name")
#' }
#'
#' @export

analyze_data <- function(df, plotdata, stratum1, stratum2=NULL) {
  # Retrieve expected returns and calculate averages
  df <- purrr::pmap_df(df, function(...) {
    row_data <- tibble(...)
    for(period in periods) {
      if(!is.na(row_data[[paste0(stratum1, "_", period)]])) {
        # match on diff_stratum as well if stratum2 is provided
        if(is.null(stratum2)) {
          condition <- plotdata$duration == period &
            plotdata$z_score_stratum == row_data[[paste0(stratum1, "_", period)]]
        } else {
          condition <- plotdata$duration == period &
            plotdata$z_score_stratum == row_data[[paste0(stratum1, "_", period)]] &
            plotdata$diff_stratum == row_data[[paste0(stratum2, "_", period)]]
        }
        matched_rows_indices <- which(condition)
        if (length(matched_rows_indices) > 1) {
          print(plotdata[matched_rows_indices,])  # print the matched rows
        }
        if (length(matched_rows_indices) > 0) {
          row_data[[paste0("implied_daily_return_", period)]] <- plotdata$mean_daily_return[matched_rows_indices[1]]
        }
      }
    }
    implied_daily_returns <- dplyr::select(row_data, starts_with("implied_daily_return"))
    row_data$average_expected_return <- mean(as.numeric(unlist(implied_daily_returns)), na.rm = TRUE)
    return(row_data)
  }) %>%
    arrange(desc(average_expected_return))

  return(df)
}

#' Investment Thesis Classification Function
#'
#' The `classify_investment_thesis` function takes a data frame (`df`) that contains data related to investment returns.
#' Based on the means of short and long periods for these returns, the function determines an investment thesis classification
#' which is either 'short-term mean reversion', 'short-term momentum', 'long-term mean reversion', or 'long-term momentum'.
#' This classification is crucial for investment strategizing and understanding the nature of returns over different periods.
#'
#' @param df A data frame that houses the data requiring analysis. It should include columns that start with
#'           "implied_daily_return_" representing daily investment returns and those that start with "moving_average_"
#'           indicating the moving averages of returns.
#'
#' @return A data frame enriched with an additional column named 'type' that classifies each row's investment thesis
#'         based on the analyzed data. This data frame is returned in descending order based on the column
#'         'average_expected_return'.
#'
#' @importFrom dplyr rowwise mutate ungroup arrange
#' @importFrom tidyr fill
#' @importFrom rlang .data
#' @importFrom purrr pmap_df
#' @importFrom magrittr %>%
#'
#' @details
#' The function processes the provided data using the following key steps:
#' 1. Defines mean calculation functions for short and long periods both for direct returns and moving averages.
#' 2. Calculates the 'average_expected_return' for each row in the data frame.
#' 3. Assigns a type of investment thesis based on a combination of the mean values of short and long periods
#'    and their corresponding moving averages.
#' 4. Returns the modified data frame with the investment thesis classification and the data sorted in descending order
#'    of 'average_expected_return'.
#'
#' @examples
#' \dontrun{
#' classified_data <- classify_investment_thesis(investment_df)
#' }
#'
#' @export

classify_investment_thesis <- function(df) {
  # Define mean functions for short and long periods
  mean_short <- function(x) mean(x[1:3], na.rm = TRUE)
  mean_long <- function(x) mean(x[4:6], na.rm = TRUE)
  mean_moving_short <- function(x) mean(x[1:3], na.rm = TRUE)
  mean_moving_long <- function(x) mean(x[4:6], na.rm = TRUE)

  df <- df %>%
    rowwise() %>%
    mutate(
      average_expected_return = mean(c_across(starts_with("implied_daily_return_")), na.rm = TRUE),
      type = case_when(
        mean_short(c_across(starts_with("implied_daily_return_"))) > mean_long(c_across(starts_with("implied_daily_return_"))) & mean_moving_short(c_across(starts_with("moving_average_"))) < 0 ~ "short-term mean reversion",
        mean_short(c_across(starts_with("implied_daily_return_"))) > mean_long(c_across(starts_with("implied_daily_return_"))) & mean_moving_short(c_across(starts_with("moving_average_"))) > 0 ~ "short-term momentum",
        mean_short(c_across(starts_with("implied_daily_return_"))) < mean_long(c_across(starts_with("implied_daily_return_"))) & mean_moving_long(c_across(starts_with("moving_average_"))) < 0 ~ "long-term mean reversion",
        mean_short(c_across(starts_with("implied_daily_return_"))) < mean_long(c_across(starts_with("implied_daily_return_"))) & mean_moving_long(c_across(starts_with("moving_average_"))) > 0 ~ "long-term momentum",
        TRUE ~ NA_character_
      )
    ) %>%
    ungroup() %>%
    arrange(desc(average_expected_return))

  # Return final dataframe
  return(df)
}
