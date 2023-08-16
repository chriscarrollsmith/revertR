# README
Christopher C. Smith

# revertR

## Introduction

`revertR` is an R package for forecasting stock, bond, and ETF returns
based on momentum and mean-reversion effects. The package has basically
two core components:

1.  The pre-calculated data used for forecasting, and

2.  A set of functions for scraping and prepping asset price data and
    forecasting future price moves.

The basic conceit is that you can quantify the stock price’s
relationship to various exponential moving averages as a z-score, and
this gives you a momentum or mean reversion signal. Then, you can
forecast future prices by looking at how the asset has historically
performed in that z-score range.

## Limitations

NOTE: Git push currently fails because the pre-calculated data is over
600 MB. It’s possible to override git’s buffer size limit for a single
push, but this too large for practical storage on Github with a free
account. Instead of including data in the package, I should simply
package up my code for pre-calculating the data, and push that.

First of all, the dataset used for pre-calculating asset price returns
by z-score is full of survivorship bias, and this is probably grossly
distorting the forecast numbers, especially for individual stocks. Note
that forecasts on sector and country ETFs are probably less biased by
survivorship. At some point I’d like to repeat the analysis with a
survivorship-free dataset.

Secondly, historical returns are no guarantee of future returns, and I
haven’t yet built estimates of variability into my forecasting
functions.

Thirdly, the functions only work for the S&P 500, the S&P 600, and
specific sets of country, sector, and bond ETFs. These are the only
assets I’ve pre-calculated data for.

## Installation

To install `revertR`, use:

``` r
devtools::install_github("chriscarrollsmith/revertR")
```

## Usage

The revertR package is meant to be used in combination with the
`tidyverse` and `tidyquant` packages. I also recommend disabling
scientific notation.

``` r
library(tidyverse)
library(tidyquant)
library(revertR)
options(scipen = 999)
```

To fetch and prep historical price data for the S&P 500, use the
`prep_data` function and pass it a list of ticker symbols obtained from
tidyquant’s `tq_index` function.

``` r
# Fetch and prep S&P 500 data
tickers <- tq_index("SP500") %>% filter(symbol != "-") %>% pull(symbol)
sp500_df <- prep_data(tickers, periods, include_today=FALSE)
```

Note that by default, today’s prices are not included in the fetched
data. To include today’s prices in the analysis, you must obtain a free
API key from Alphanvatage and set the environment variable
`ALPHAVANTAGE_API_KEY` using the command
`Sys.setenv(ALPHAVANTAGE_API_KEY="<your_api_key>")`.

To run the analysis to forecast returns from the most recent closing
price, you should load one of the pre-calculated S&P 500 plotdata data
frames using `data`. I recommend using the double-stratified data frame,
`sp500DSplotdata`. Alternatively, you can use the single-stratified data
frames, `sp500plotdata` or `sp500MAplotdata`.

You will pass your S&P 500 prices data frame to `analyze_data` as the
first argument, and the plotdata data frame as the second argument. You
must also indicate the strata to be used for the analysis. For
single-stratified plotdata, you will use “z_score_stratum” for the third
argument, leaving the fourth argument to its default `NULL` value. For
double-stratified data, you will use “z_score_stratum” as the third
argument and “diff_stratum” as the fourth. (Calling this function will
be simplified in future versions of the package.)

``` r
# Forecast returns
data(sp500DSplotdata, package="revertR")
sp500_forecasts <- analyze_data(sp500_df, sp500DSplotdata, "z_score_stratum", "diff_stratum")
```

Once we have our data frame of return forecasts (represented as average
expected daily return over the next 12 months), we can use
`classify_investment_thesis` to group the forecasts by whether they
represent more of a mean-reversion or momentum strategy, and over what
term. However, note that these categories are fuzzy, and many forecasts
are premised on a combination of momentum and mean-reversion effects.

``` r
#Classify and filter double strat results
classify_investment_thesis(sp500_forecasts) %>%
  group_by(type) %>%
  arrange(desc(average_expected_return)) %>%
  filter(row_number()<4) %>%
  select(symbol,average_expected_return,type) %>%
  print()
```

    # A tibble: 12 × 3
    # Groups:   type [4]
       symbol average_expected_return type                     
       <chr>                    <dbl> <chr>                    
     1 IFF                   0.00625  short-term mean reversion
     2 RMD                   0.00421  short-term mean reversion
     3 WTW                   0.00237  short-term mean reversion
     4 SEE                   0.00190  short-term momentum      
     5 NEE                   0.00140  long-term mean reversion 
     6 ES                    0.00135  long-term mean reversion 
     7 CCI                   0.00104  long-term mean reversion 
     8 PNW                   0.000939 short-term momentum      
     9 ROK                   0.000939 short-term momentum      
    10 MPC                   0.000706 long-term momentum       
    11 CMI                   0.000695 long-term momentum       
    12 EXPD                  0.000695 long-term momentum       

## Supported asset classes

In addition to the S&P 500, revertR also supports several other asset
classes, including stocks of the S&P 600 idex and selected sector,
country, and bond ETFs. To analyze other assets besides these, you will
need to calculate your own plotdata. That is not currently supported,
but support will be added in a future version.

### S&P 600

``` r
# Fetch and prep S&P 600 data
tickers <- tq_index("SP600",use_fallback=TRUE) %>% filter(symbol != "-") %>% pull(symbol)
```

    Using fallback dataset last downloaded 2019-10-11.

``` r
sp600_df <- prep_data(tickers, periods, include_today=FALSE)
```

    Warning: There were 117 warnings in `dplyr::mutate()`.
    The first warning was:
    ℹ In argument: `data.. = purrr::map(...)`.
    Caused by warning:
    ! x = 'CCMP', get = 'stock.prices': Error in getSymbols.yahoo(Symbols = "CCMP", env = <environment>, verbose = FALSE, : Unable to import "CCMP".
    HTTP error 404.
     Removing CCMP.
    ℹ Run `dplyr::last_dplyr_warnings()` to see the 116 remaining warnings.

``` r
# Forecast returns using the combo of both indicators
data("sp600DSplotdata", package="revertR")
sp600_forecasts <- analyze_data(sp600_df, sp600DSplotdata, "z_score_stratum", "diff_stratum")

#Classify and filter double strat results
classify_investment_thesis(sp600_forecasts) %>%
  group_by(type) %>%
  arrange(desc(average_expected_return)) %>%
  filter(row_number()<4) %>%
  select(symbol,average_expected_return,type) %>%
  print()
```

    # A tibble: 13 × 3
    # Groups:   type [5]
       symbol average_expected_return type                     
       <chr>                    <dbl> <chr>                    
     1 WWW                   0.00584  long-term mean reversion 
     2 GLT                   0.00582  long-term mean reversion 
     3 EBIX                  0.00572  long-term momentum       
     4 CPSI                  0.00395  long-term mean reversion 
     5 DGII                  0.00391  long-term momentum       
     6 FF                    0.00243  long-term momentum       
     7 PRFT                  0.00233  short-term mean reversion
     8 CAMP                  0.00219  short-term mean reversion
     9 ANIK                  0.00212  short-term mean reversion
    10 POWL                  0.00145  short-term momentum      
    11 SRDX                  0.00121  short-term momentum      
    12 PARR                  0.00117  short-term momentum      
    13 AAOI                  0.000776 <NA>                     

### Selected sector ETFs

``` r
# Fetch and prep sectors data
tickers <- c("XLRE","XLY","XLI","XLB","XLE","XLF","XLP","XLV","XLU","XAR","XBI","XHB","XME","XTL","XTN","XSW","XSD","XLK","XLC","XRT","KRE","XES","XOP","KIE","XWEB","XPH","XHS","XHE")
sectors_df <- prep_data(tickers, periods, include_today=FALSE)

# Forecast returns using the combo of both indicators
data("sectorsDSplotdata", package="revertR")
sectors_forecasts <- analyze_data(sectors_df, sectorsDSplotdata, "z_score_stratum", "diff_stratum")

#Classify and filter double strat results
classify_investment_thesis(sectors_forecasts) %>%
  group_by(type) %>%
  arrange(desc(average_expected_return)) %>%
  filter(row_number()<3) %>%
  select(symbol,average_expected_return,type) %>%
  print()
```

    # A tibble: 8 × 3
    # Groups:   type [4]
      symbol average_expected_return type                     
      <chr>                    <dbl> <chr>                    
    1 XHE                   0.00113  short-term mean reversion
    2 XLU                   0.000837 long-term mean reversion 
    3 XTN                   0.000719 short-term momentum      
    4 XSD                   0.000702 short-term momentum      
    5 XSW                   0.000573 long-term mean reversion 
    6 XBI                   0.000562 short-term mean reversion
    7 XLC                   0.000468 long-term momentum       
    8 XLE                   0.000392 long-term momentum       

### Selected country ETFs

``` r
# Fetch and prep countries data
tickers <- c("FLLA","FLEE","FLFR","FLAX","FLIN","FLKR","FLSW","FLTW","EWO","EIRL","FLMX","EWN","ARGT","ENOR","GREK","EUSA","FLSA","UAE","EIS","FLRU","EWQ","EDEN","FLCA","EWD","FLIY","FLZA","FLGB","FLAU","EWP","FLHK","EWS","FLJP","PGAL","EFNL","THD","FLGR","GXG","IDX","FLBR","EPHE","QAT","TUR","ECH","NGE","ENZL","EGPT","EWM","FLCH","PAK","EPU")
countries_df <- prep_data(tickers, periods, include_today=FALSE)

# Forecast returns using the combo of both indicators
data("countriesDSplotdata", package="revertR")
countries_forecasts <- analyze_data(countries_df, countriesDSplotdata, "z_score_stratum", "diff_stratum")

#Classify and filter double strat results
classify_investment_thesis(countries_forecasts) %>%
  group_by(type) %>%
  arrange(desc(average_expected_return)) %>%
  filter(row_number()<3) %>%
  select(symbol,average_expected_return,type) %>%
  print()
```

    # A tibble: 7 × 3
    # Groups:   type [4]
      symbol average_expected_return type                     
      <chr>                    <dbl> <chr>                    
    1 EPHE                 0.000203  short-term momentum      
    2 EGPT                 0.000202  short-term momentum      
    3 ENZL                 0.000201  long-term mean reversion 
    4 FLTW                 0.000182  long-term mean reversion 
    5 FLZA                 0.000178  long-term momentum       
    6 FLGR                 0.000109  long-term momentum       
    7 EDEN                 0.0000958 short-term mean reversion

### Selected bond ETFs

``` r
# Fetch and prep bonds data
tickers <- c("VWOB","SPSB","IAGG","HYEM","BNDX","BND","TIP","SPLB","SPIB","BSV","GBF","BIL","SPTI","VTEB","SPHY","PLW","SPTL","IGLB","IGIB","SPMB")
bonds_df <- prep_data(tickers, periods, include_today=FALSE)

# Forecast returns using the combo of both indicators
data("bondsDSplotdata", package="revertR")
bonds_forecasts <- analyze_data(bonds_df, bondsDSplotdata, "z_score_stratum", "diff_stratum")

#Classify and filter double strat results
classify_investment_thesis(bonds_forecasts) %>%
  group_by(type) %>%
  arrange(desc(average_expected_return)) %>%
  filter(row_number()<3) %>%
  select(symbol,average_expected_return,type) %>%
  print()
```

    # A tibble: 4 × 3
    # Groups:   type [2]
      symbol average_expected_return type                    
      <chr>                    <dbl> <chr>                   
    1 BIL                   0.000166 short-term momentum     
    2 SPTI                  0.000158 long-term mean reversion
    3 IGLB                  0.000145 long-term mean reversion
    4 SPHY                  0.000140 short-term momentum     
