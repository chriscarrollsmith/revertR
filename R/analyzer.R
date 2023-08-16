#' Time Series Splitting Function for Training and Testing
#'
#' The function `time_series_split` splits the time series data into training and testing sets by date, grouped by symbols.
#' It allows to specify the number of rows to omit from the training set to prevent overlap between test and train for the outcome variable.
#'
#' @param df A data frame containing at least the columns 'symbol', 'date', and 'adjusted', representing the daily data for the stocks.
#' @param split_date A Date object representing the cut-off date for splitting the data into training and testing sets.
#' @param n_of_rows_to_omit An integer specifying the number of rows to omit at the end of the training set for each symbol.
#' @return A list containing two data frames: 'train' and 'test'.
#' @importFrom dplyr filter arrange group_by slice_head
#' @importFrom magrittr %>%
#' @examples
#' \dontrun{
#'   split_data <- time_series_split(df, as.Date("2020-01-01"), n_of_rows_to_omit = 5)
#' }
#' @export

time_series_split <- function(df, split_date, n_of_rows_to_omit) {
  # Splitting into training and testing based on the date
  train <- df %>%
    filter(date < split_date) %>%
    group_by(symbol) %>%
    arrange(date) %>%
    slice_head(n = -n_of_rows_to_omit) # Removing last n rows

  test <- df %>%
    filter(date >= split_date)

  return(list(train = train, test = test))
}

#' Calculate Correlation Coefficients
#'
#' The function `calculate_correlations` computes the correlation coefficients between the z-scores and returns
#' for all combinations of 'z_score_' and 'return_' columns in the given data frame.
#'
#' @param df A data frame that contains at least the columns starting with 'z_score_' and 'return_'.
#' @param prefix An optional prefix to be added to 'z_score_' if the z-scores columns have additional prefixes. Default is an empty string.
#'
#' @return A data frame containing the computed correlation coefficients for all combinations of z-scores and returns.
#'
#' @examples
#' \dontrun{
#'   correlations <- calculate_correlations(df)
#' }
#'
#' @export

calculate_correlations <- function(df, prefix = "") {
  z_score_cols <- grep(paste0("^", prefix, "z_score_"), names(df), value = TRUE)
  return_cols <- grep("^return_", names(df), value = TRUE)

  correlations <- matrix(nrow = length(z_score_cols), ncol = length(return_cols), dimnames = list(z_score_cols, return_cols))

  for (i in seq_along(z_score_cols)) {
    for (j in seq_along(return_cols)) {
      z_score_col <- z_score_cols[i]
      return_col <- return_cols[j]

      # Check if there are enough non-NA pairs
      if (sum(complete.cases(df[[z_score_col]], df[[return_col]])) >= 2) {
        correlations[i, j] <- cor(df[[z_score_col]], df[[return_col]], use = "complete.obs")
      } else {
        correlations[i, j] <- NA
      }
    }
  }

  return(as.data.frame(correlations))
}

#' Calculate Linear Regression and Summarize Coefficients
#'
#' This function calculates linear regressions for pairs of return and z-score variables
#' for specified periods in a given data frame. It returns a data frame with the mean and
#' modeled return for each z-score stratum, including additional summary statistics.
#'
#' @param df A data frame that must contain rounded columns named 'z_score_' and columns named 'return_'
#'           followed by corresponding period numbers. E.g., 'z_score_20', 'return_20', etc.
#' @param periods A numeric vector of non-negative integers representing the periods for which the
#'                linear regressions and summaries are to be computed. Default periods are
#'                c(20, 50, 100, 200, 500, 1000).
#' @param return_periods A numeric vector of non-negative integers representing the periods over which
#'                returns are to be computed. Vector must be same length as `periods`. Default periods are
#'                c(20, 50, 100, 200, 500, 1000).
#' @return A data frame containing the computed summaries and linear regressions for the specified periods,
#'         including mean return, standard deviation of return, sample size, duration, regression,
#'         mean daily return, standard deviation of daily return, and daily regression.
#' @examples
#' \dontrun{
#'   plotdata <- calculate_linear_coefficients(df, periods = c(20, 50, 100, 200, 500, 1000))
#' }
#' @importFrom dplyr filter mutate group_by summarize
#' @importFrom purrr map2_dfr
#' @importFrom rlang sym
#' @importFrom magrittr %>%
#' @importFrom stats lm
#' @export

calculate_linear_coefficients <- function(df, periods = c(20, 50, 100, 200, 500, 1000),
                                          return_periods = c(20, 50, 100, 200, 500, 1000),
                                          prefix = "", prefix_2 = NULL) {

  stratum_col <- sym(paste0(prefix,"z_score_stratum"))

  if (!is.null(prefix_2)) {
    stratum_col_2 <- sym(paste0(prefix_2, "z_score_stratum"))
  }

  compute_line_data <- function(period, return_period) {
    z_score_col_name <- paste0(prefix, "z_score_", period)
    z_score_col <- sym(z_score_col_name)
    return_col_name <- paste0("return_", return_period)
    return_col <- sym(return_col_name)

    # Check if the columns exist in the data frame and filter out NAs
    if (!is.null(prefix_2)) {
      z_score_col_name_2 <- paste0(prefix_2, "z_score_", period)
      z_score_col_2 <- sym(z_score_col_name_2)

      if (!(z_score_col_name %in% names(df) & return_col_name %in% names(df) & z_score_col_name_2 %in% names(df))) {
        stop(paste("Columns", z_score_col_name, "and/or", z_score_col_name_2, "and/or", return_col_name, "not found in the provided data frame"))
      }

      filtered_data <- df %>%
        filter(!is.na(!!z_score_col) & !is.na(!!z_score_col_2) & !is.na(!!return_col))
    } else {
      if(!(z_score_col_name %in% names(df) & return_col_name %in% names(df))) {
        stop(paste("Columns", z_score_col_name, "and/or", return_col_name, "not found in the provided data frame"))
      }

      filtered_data <- df %>%
        filter(!is.na(!!z_score_col) & !is.na(!!return_col))
    }

    # Check if there are enough non-NA pairs
    if (nrow(filtered_data) < 2) {
      return(data.frame(duration = period))
    }

    if(!is.null(prefix_2)) {
      line <- lm(as.formula(paste(return_col_name, "~", z_score_col_name, "+", z_score_col_name_2)), data = filtered_data)
      plotdata <- filtered_data %>%
        mutate(!!stratum_col := round(!!z_score_col),
               !!stratum_col_2 := round(!!z_score_col_2)) %>%
        group_by(!!stratum_col,!!stratum_col_2)
    } else {
      line <- lm(as.formula(paste(return_col_name, "~", z_score_col_name)), data = filtered_data)
      plotdata <- filtered_data %>%
        mutate(!!stratum_col := round(!!z_score_col)) %>%
        group_by(!!stratum_col)
    }

    plotdata <- plotdata %>%
      summarize(
        mean_return = mean(!!return_col),
        sd_return = sd(!!return_col),
        n = n(),
        duration = period,
        .groups = 'drop'
      )

    if (!is.null(prefix_2)) {
      plotdata <- plotdata %>%
        mutate(regression = !!stratum_col * line$coefficients[2] + !!stratum_col_2 * line$coefficients[3] + line$coefficients[1])
    } else {
      plotdata <- plotdata %>%
        mutate(regression = !!stratum_col * line$coefficients[2] + line$coefficients[1])
    }

    return(plotdata)
  }

  plotdata <- map2_dfr(periods, return_periods, compute_line_data)
  plotdata <- plotdata %>%
    mutate(
      mean_daily_return = mean_return / duration,
      sd_daily_return = sd_return / duration,
      daily_regression = regression / duration,
      duration = as.factor(duration)
    )

  if (!is.null(prefix_2)) {
    plotdata <- plotdata %>%
      filter(!is.na(!!stratum_col) & !is.na(!!stratum_col_2))
  } else {
    plotdata <- plotdata %>%
      filter(!is.na(!!stratum_col))
  }

  return(plotdata)
}

#' Plot Returns and Error Bars for Z-Score or Moving Average Thereof
#'
#' This function plots the mean daily return for investing in an asset, such as an S&P 500 stock,
#' by the price's z-score difference from EMA, or the trailing 20-day moving average thereof, or the difference between
#' these two. Error bars are also added to represent the standard deviation of daily returns.
#'
#' @param plotdata A data frame containing the required plot data, including mean daily return and standard deviation of daily return.
#' @param stratum A string representing the column used for stratification. Default is "z_score_stratum".
#' @param asset A string representing the asset name. Default is "an S&P 500 stock".
#' @param periods A numeric vector of integers representing the periods for which the plot is to be generated. Default periods are c(20, 50, 100, 200, 500, 1000).
#'
#' @return A ggplot object representing the plotted data.
#' @examples
#' \dontrun{
#'   plot_results(plotdata, stratum = "z_score_stratum", asset = "an S&P 500 stock", periods = c(20, 50, 100, 200, 500, 1000))
#' }
#' @importFrom ggplot2 ggplot geom_errorbar geom_line labs facet_wrap
#' @importFrom magrittr %>%
#' @importFrom rlang sym
#' @importFrom DescTools StrCap
#' @export

plot_results <- function(plotdata, stratum = "z_score_stratum", asset = "an S&P 500 stock", periods = c(20,50,100,200,500,1000)) {
  if(stratum == "z_score_stratum"){
    title_ending <- "price's z-score difference from EMA"
  } else if(stratum == "moving_average_z_score_stratum") {
    title_ending <- "trailing 20-day moving average of price's z-score difference from EMA"
  } else if(stratum == "diff_z_score_stratum") {
    title_ending <- "difference between price's z-score difference from EMA and 20-day MA thereof"
  }
  stratum_col <- sym(stratum)

  plotdata %>%
    filter(!is.na(sd_daily_return)) %>%
    filter(duration %in% periods) %>%
    ggplot() +
    geom_errorbar(aes(x=!!stratum_col,ymax = mean_daily_return + sd_daily_return, ymin = mean_daily_return - sd_daily_return,col=duration),position="dodge2") +
    geom_line(aes(x=!!stratum_col,y=mean_daily_return,col=duration)) +
    labs(title = StrCap(paste("Mean daily return for investing in",asset,"by",title_ending)),
         subtitle = StrCap("Faceted by EMA/return period length"),
         x = StrCap(title_ending),
         y = StrCap("Average daily return from purchasing at price"),
         caption = paste("Analysis based on", sum(plotdata$n[plotdata$duration == 20]), "daily price observations. Data courtesy of Yahoo! Finance. Copyright Promptly Technologies, LLC, 2023.")) +
    facet_wrap(vars(duration))
}

#' Plot Returns for Double Stratified Data
#'
#' This function plots the mean daily return for investing in an asset (e.g., an S&P 500 stock) by the difference
#' between the z-score relationship to a specified exponential moving average (EMA) period and the trailing 20-day
#' moving average thereof. The plot is double stratified according to the parameters specified.
#'
#' @param plotdata A data frame containing the required plot data, including mean daily return and standard deviation of daily return.
#' @param stratum1 A string representing the first stratification column. Default is "moving_average_z_score_stratum".
#' @param stratum2 A string representing the second stratification column. Default is "diff_z_score_stratum".
#' @param asset A string representing the asset name. Default is "an S&P 500 stock".
#' @param period An integer representing the EMA period for the analysis. Default is 20.
#'
#' @return A ggplot object representing the plotted data.
#' @examples
#' \dontrun{
#'   plotresults_ds(plotdata, stratum1 = "moving_average_z_score_stratum", stratum2 = "diff_z_score_stratum", asset = "an S&P 500 stock", period = 20)
#' }
#' @importFrom ggplot2 ggplot aes geom_line scale_y_continuous scale_x_continuous labs facet_wrap
#' @importFrom magrittr %>%
#' @importFrom scales percent
#' @export

plot_results_ds <- function(plotdata, stratum1 = "moving_average_z_score_stratum", stratum2 = "diff_z_score_stratum", asset = "an S&P 500 stock", period = 20) {
  ds_plotdata %>%
    filter(duration == period) %>%
    filter(!is.na(sd_daily_return)) %>%
    group_by(moving_average_z_score_stratum) %>%
    filter(n() > 4) %>%
    ungroup() %>%
    ggplot(aes(x=diff_z_score_stratum)) +
    geom_line(aes(y=mean_daily_return)) +
    scale_y_continuous(labels = scales::percent) +
    scale_x_continuous(breaks=-20:20)+
    labs(title = paste("Mean daily return for investing in",asset,"by diff between z-score relationship to",period,"EMA and trailing 20-day moving average thereof"),
         x = "Price standard deviation away from price's mean relationship to EMAs",
         y = "Average daily return from purchasing at said price",
         caption = paste("Analysis based on", sum(plotdata$n[plotdata$duration == 20]), "trading days of historical price data courtesy of Yahoo! Finance. Copyright Wall Street Petting Zoo, 2021.")) +
    facet_wrap(vars(moving_average_z_score_stratum))
}

#' Calculate Trade Potential
#'
#' This function filters the data for trades with a mean daily return greater than 0.0008, then calculates the
#' potential return for different durations. It returns a summarized view with the overall potential return
#' and the number of trades for each duration.
#'
#' @param plotdata A data frame containing the required data, including mean daily return, duration, and number of trades (n).
#' @param strategy A string indicating the strategy for tidy result grouping and comparison
#'
#' @return A data frame with the summarized potential return and the number of trades for each duration.
#'
#' @examples
#' \dontrun{
#'   potential <- trade_potential(plotdata)
#'   print(potential)
#' }
#' @importFrom dplyr filter group_by mutate summarize
#' @importFrom magrittr %>%
#' @export

trade_potential <- function (plotdata,strategy) {
  plotdata %>%
    filter(mean_daily_return > 0.0008) %>%
    group_by(duration) %>%
    mutate(potential_return = n*mean_daily_return) %>%
    summarize(avg_potential_return = sum(potential_return)/sum(n),
              n_of_trades = sum(n),
              strategy = strategy)
}

#' Compute Linear Models by Different Z-Score Strata
#'
#' This function computes linear models for each z-score stratum, each 20-day-moving-average-of-z-score stratum,
#' and double-stratifying by 20-day moving average of z-score and the current z-score's difference from it.
#' It uses the `calculate_linear_coefficients` function for these computations and saves the results to files.
#'
#' @param df A data frame containing the necessary variables for the linear model computation.
#' @param filename_prefix A string used to prepend to the filenames of the saved results. Default is an empty string.
#' @return NULL. Results are saved to files within the ./data/ directory.
#' @examples
#' \dontrun{
#'   compute_linear_models_by_z_score_stratum(df, filename_prefix = "sp500")
#' }
#' @seealso \code{\link{calculate_linear_coefficients}} for the function used to calculate linear coefficients.
#' @export

compute_linear_models_by_z_score_stratum <- function(df,
                                                     periods = c(20, 50, 100, 200, 500, 1000),
                                                     return_periods = c(20, 50, 100, 200, 500, 1000),
                                                     filename_prefix = "") {
  # Calculate linear models for each z-score stratum
  plotdata <- calculate_linear_coefficients(train_set,periods,return_periods,prefix="",prefix_2=NULL)

  # Calculate linear models for each 20-day-moving-average-of-z-score stratum
  ma_plotdata <- calculate_linear_coefficients(train_set,periods,return_periods,prefix="moving_average_",prefix_2=NULL)

  # Calculate linear models, double-stratifying by 20-day moving average of
  # z-score and the current z-score's difference from it
  ds_plotdata <- calculate_linear_coefficients(train_set,periods,return_periods,prefix="moving_average_",prefix_2="diff_")

  # Save the results to file
  plotdata_filepath <- paste0("./data/",filename_prefix,"plotdata.rda")
  save(plotdata, file = plotdata_filepath)
  ma_plotdata_filepath <- paste0("./data/",filename_prefix,"MAplotdata.rda")
  save(ma_plotdata, file = ma_plotdata_filepath)
  ds_plotdata_filepath <- paste0("./data/",filename_prefix,"DSplotdata.rda")
  save(ds_plotdata, file = ds_plotdata_filepath)

  return(list(plotdata,ma_plotdata,ds_plotdata))
}
