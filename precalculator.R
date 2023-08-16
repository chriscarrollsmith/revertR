library(revertR)
library(tidyverse)
library(tidyquant)

# S&P 500 (Large Cap) -----------------------------------------------------

# Get S&P 500 tickers
tickers <- tq_index("SP500") %>%
  filter(symbol != "-") %>%
  pull(symbol)

# Get data frame of price history and prep for analysis
df <- prep_data_for_analysis(tickers)

# Split the series for machine learning
split_result <- time_series_split(df, as.Date("2020-01-01"), n_of_rows_to_omit = 5)
train_set <- split_result[[1]]
test_set <- split_result[[2]]

# Calculate correlation coefficients
calculate_correlations(train_set)

# Calculate three different linear models, using three different approaches to
# stratifying price by z-score, and save data to file
plotdata <- compute_linear_models_by_z_score_stratum(train_set,filename_prefix = "sp500")

# Asset description to use in plot titles
asset_description <- "an S&P 500 stock"

# Plot results for current-day z-score
plot_results(plotdata[[1]], "z_score_stratum", asset_description)

# Plot results for 20-day moving average of z-score
plot_results(plotdata[[2]], "moving_average_z_score_stratum", asset_description)

# Plot results for difference between current-day z-score and 20-day moving average of z-score
plot_results_ds(plotdata[[3]], "moving_average_z_score_stratum", "diff_z_score_stratum", asset_description, period = 20)

#calculate potential return per trade and number of trade opportunities by strategy
map2_dfr(.x = plotdata,
         .y = c("z-score","ma of z-score","double-strat"),
         .f = trade_potential) %>%
  group_by(strategy) %>%
  summarize(mean_return = sum(avg_potential_return*n_of_trades)/sum(n_of_trades),
            n_of_trades = sum(n_of_trades))

# S&P 600 (Small Cap) -----------------------------------------------------

# Get S&P 600 tickers
tickers <- tq_index("SP600", use_fallback=TRUE) %>%
  filter(symbol != "-") %>%
  pull(symbol)

# Get data frame of price history and prep for analysis
df <- prep_data_for_analysis(tickers)

# Split the series for machine learning
split_result <- time_series_split(df, as.Date("2020-01-01"), n_of_rows_to_omit = 5)
train_set <- split_result[[1]]
test_set <- split_result[[2]]

# Calculate correlation coefficients
calculate_correlations(train_set)

# Calculate three different linear models, using three different approaches to
# stratifying price by z-score, and save data to file
plotdata <- compute_linear_models_by_z_score_stratum(train_set,filename_prefix = "sp600")

# Asset description to use in plot titles
asset_description <- "an S&P 600 stock"

# Plot results for current-day z-score
plot_results(plotdata[[1]], "z_score_stratum", asset_description)

# Plot results for 20-day moving average of z-score
plot_results(plotdata[[2]], "moving_average_z_score_stratum", asset_description)

# Plot results for difference between current-day z-score and 20-day moving average of z-score
plot_results_ds(plotdata[[3]], "moving_average_z_score_stratum", "diff_z_score_stratum", asset_description, period = 20)

#calculate potential return per trade and number of trade opportunities by strategy
map2_dfr(.x = plotdata,
         .y = c("z-score","ma of z-score","double-strat"),
         .f = trade_potential) %>%
  group_by(strategy) %>%
  summarize(mean_return = sum(avg_potential_return*n_of_trades)/sum(n_of_trades),
            n_of_trades = sum(n_of_trades))

# Sector ETFs -----------------------------------------------------------------

tickers <- c("XLRE","XLY","XLI","XLB","XLE","XLF","XLP","XLV","XLU","XAR","XBI","XHB","XME","XTL","XTN","XSW","XSD","XLK","XLC","XRT","KRE","XES","XOP","KIE","XWEB","XPH","XHS","XHE")

# Get data frame of price history and prep for analysis
df <- prep_data_for_analysis(tickers)

# Split the series for machine learning
split_result <- time_series_split(df, as.Date("2020-01-01"), n_of_rows_to_omit = 5)
train_set <- split_result[[1]]
test_set <- split_result[[2]]

# Calculate correlation coefficients
calculate_correlations(train_set)

# Calculate three different linear models, using three different approaches to
# stratifying price by z-score, and save data to file
plotdata <- compute_linear_models_by_z_score_stratum(train_set,filename_prefix = "sectors")

# Asset description to use in plot titles
asset_description <- "an equal-weight sector ETF"

# Plot results for current-day z-score
plot_results(plotdata[[1]], "z_score_stratum", asset_description)

# Plot results for 20-day moving average of z-score
plot_results(plotdata[[2]], "moving_average_z_score_stratum", asset_description)

# Plot results for difference between current-day z-score and 20-day moving average of z-score
plot_results_ds(plotdata[[3]], "moving_average_z_score_stratum", "diff_z_score_stratum", asset_description, period = 20)

#calculate potential return per trade and number of trade opportunities by strategy
map2_dfr(.x = plotdata,
         .y = c("z-score","ma of z-score","double-strat"),
         .f = trade_potential) %>%
  group_by(strategy) %>%
  summarize(mean_return = sum(avg_potential_return*n_of_trades)/sum(n_of_trades),
            n_of_trades = sum(n_of_trades))

# Country ETFs -----------------------------------------------------------------

tickers <- c("FLLA","FLEE","FLFR","FLAX","FLIN","FLKR","FLSW","FLTW","EWO","EIRL","FLMX","EWN","ARGT","NORW","GREK","EUSA","FLSA","UAE","EIS","FLRU","EWQ","EDEN","VNM","FLCA","EWD","FLIY","FLZA","FLGB","FLAU","EWP","FLHK","EWS","FLJP","PGAL","EFNL","THD","FLGR","GXG","IDX","FLBR","EPHE","QAT","TUR","ECH","NGE","EWH","ENZL","EGPT","EWM","FLCH","PAK","EPU")

# Get data frame of price history and prep for analysis
df <- prep_data_for_analysis(tickers)

# Split the series for machine learning
split_result <- time_series_split(df, as.Date("2020-01-01"), n_of_rows_to_omit = 5)
train_set <- split_result[[1]]
test_set <- split_result[[2]]

# Calculate correlation coefficients
calculate_correlations(train_set)

# Calculate three different linear models, using three different approaches to
# stratifying price by z-score, and save data to file
plotdata <- compute_linear_models_by_z_score_stratum(train_set,filename_prefix = "countries")

# Asset description to use in plot titles
asset_description <- "an equal-weight country ETF"

# Plot results for current-day z-score
plot_results(plotdata[[1]], "z_score_stratum", asset_description)

# Plot results for 20-day moving average of z-score
plot_results(plotdata[[2]], "moving_average_z_score_stratum", asset_description)

# Plot results for difference between current-day z-score and 20-day moving average of z-score
plot_results_ds(plotdata[[3]], "moving_average_z_score_stratum", "diff_z_score_stratum", asset_description, period = 20)

#calculate potential return per trade and number of trade opportunities by strategy
map2_dfr(.x = plotdata,
         .y = c("z-score","ma of z-score","double-strat"),
         .f = trade_potential) %>%
  group_by(strategy) %>%
  summarize(mean_return = sum(avg_potential_return*n_of_trades)/sum(n_of_trades),
            n_of_trades = sum(n_of_trades))

# Bond ETFs -----------------------------------------------------------------

tickers <- c("SPLB","SPIB","BSV","GBF","BND","TIP","BIL","SPTI","PLW","SPTL","PHB","IGLB","IGIB","PFIG","WIP","PICB","PCY","BWZ","BWX","HYD","MLN","NYF","ITM","CMF","TFI","SHM","CWB","BKLN","SPMB")

# Get data frame of price history and prep for analysis
df <- prep_data_for_analysis(tickers)

# Split the series for machine learning
split_result <- time_series_split(df, as.Date("2020-01-01"), n_of_rows_to_omit = 5)
train_set <- split_result[[1]]
test_set <- split_result[[2]]

# Calculate correlation coefficients
calculate_correlations(train_set)

# Calculate three different linear models, using three different approaches to
# stratifying price by z-score, and save data to file
plotdata <- compute_linear_models_by_z_score_stratum(train_set,filename_prefix = "bonds")

# Asset description to use in plot titles
asset_description <- "an equal-weight bond ETF"

# Plot results for current-day z-score
plot_results(plotdata[[1]], "z_score_stratum", asset_description)

# Plot results for 20-day moving average of z-score
plot_results(plotdata[[2]], "moving_average_z_score_stratum", asset_description)

# Plot results for difference between current-day z-score and 20-day moving average of z-score
plot_results_ds(plotdata[[3]], "moving_average_z_score_stratum", "diff_z_score_stratum", asset_description, period = 20)

#calculate potential return per trade and number of trade opportunities by strategy
map2_dfr(.x = plotdata,
         .y = c("z-score","ma of z-score","double-strat"),
         .f = trade_potential) %>%
  group_by(strategy) %>%
  summarize(mean_return = sum(avg_potential_return*n_of_trades)/sum(n_of_trades),
            n_of_trades = sum(n_of_trades))

# Commodities -----------------------------------------------------------------

tickers <- c("FTRI","PICK","CUT","SRUUF","SGDJ","PPLT","GLD","SLV","PALL","UGA","BNO","USO","UNG","FUE","CANE","JO","CORN","GRU","SOYB","WEAT","NIB","REMX","JJT","SLX","JJU","JJC","JJN","DBE","GSG","DBB","BCI","DBA")

# Get data frame of price history and prep for analysis
df <- prep_data_for_analysis(tickers)

# Split the series for machine learning
split_result <- time_series_split(df, as.Date("2020-01-01"), n_of_rows_to_omit = 5)
train_set <- split_result[[1]]
test_set <- split_result[[2]]

# Calculate correlation coefficients
calculate_correlations(train_set)

# Calculate three different linear models, using three different approaches to
# stratifying price by z-score, and save data to file
plotdata <- compute_linear_models_by_z_score_stratum(train_set,filename_prefix = "commodities")

# Asset description to use in plot titles
asset_description <- "an equal-weight commodities ETF"

# Plot results for current-day z-score
plot_results(plotdata[[1]], "z_score_stratum", asset_description)

# Plot results for 20-day moving average of z-score
plot_results(plotdata[[2]], "moving_average_z_score_stratum", asset_description)

# Plot results for difference between current-day z-score and 20-day moving average of z-score
plot_results_ds(plotdata[[3]], "moving_average_z_score_stratum", "diff_z_score_stratum", asset_description, period = 20)

#calculate potential return per trade and number of trade opportunities by strategy
map2_dfr(.x = plotdata,
         .y = c("z-score","ma of z-score","double-strat"),
         .f = trade_potential) %>%
  group_by(strategy) %>%
  summarize(mean_return = sum(avg_potential_return*n_of_trades)/sum(n_of_trades),
            n_of_trades = sum(n_of_trades))
