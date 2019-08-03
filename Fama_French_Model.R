rm(list=ls())
setwd("~/")

#############################################################################################
###################### We get libraries, install only if required. ##########################

libraries_required = c('tidyquant', 'tidyverse', 'timetk', 'broom', 'dplyr')

for(i in seq(libraries_required))
{
  if(!(libraries_required[i] %in% rownames(installed.packages())))
  {
    try(expr = install.packages('libraries_required[i]'), silent = T)
  }
  try(expr = library(libraries_required[i], character.only = T), silent = T)
}

#############################################################################################
#############################################################################################


#############################################################################################
########################### We get weighted porfolio returns. ###############################

# Fama-French model was first published in 1992 as an improvement over CAPM and like models.
# At its core, it retains the simplicity and calculations involved. 
# Linear regression is still the basis for the analysis, All model assumptions still hold.

# Compared to CAPM, additional inputs of Book to Market value and Firm Size is required.
# These are Market Risk and Value Risk respectively.
# For this demo, we choose ETF markets as there addtional inputs are easily available.
# These additional inputs are for the respective indices and not stocks.
# Our weighted portfolio comprises of 5 ETFs: SPY, EFA, IJS, EEM and AGG.
# ETFs moreover, are diversified over their market domain, thus minimizing individual firm risk.

#############################################################################################
#############################################################################################

#############################################################################################
########################### We get weighted porfolio returns. ###############################

c("SPY","EFA", "IJS", "EEM","AGG") -> ETFs

# SPY ETF mirrors S&P500 (mostly US based)
# SPY ETF can be complemented by DIA
# EFA ETF index mirrors large and mid-cap developed market equities, excluding the U.S. and Canada.
# EFA ETF can be complemented by MDY
# IJS ETF is index composed of small-capitalization U.S. equities that exhibit value characteristics.
# IJS ETF can be complemented by SCJ
# EEM ETF is index composed of large- and mid-capitalization emerging market equities.
# EEM ETF can be complemented by ^INSPYMP
# AGG ETF is index composed of the total U.S. investment-grade bond market.
# AGG ETF can be complemented by SCHZ

c(0.20, 0.20, 0.20, 0.20, 0.20) -> Weights_of_Assets 

getSymbols(ETFs, src = 'yahoo', from = "2011-01-01", to = "2018-12-31",
  auto.assign = TRUE, warnings = FALSE) %>%                # Getting basic data as a tibble
  map(~Ad(get(.))) %>%                                     # Getting adjusted closing price
  reduce(merge) %>%                                        # Reducing list into single dataframe
  `colnames<-`(ETFs) %>%                                   # Renaming column headers as ETF names
  to.monthly(indexAt = "lastof", OHLC = FALSE) %>%         # Converting time series to monthly
  tk_tbl(preserve_index = TRUE, rename_index = "Date") %>% # Converting to tibble time series
  gather(asset, returns, -Date) %>%                        # Collapsing with key = ETF assets, value = - date for inverting the time series
  group_by(asset) %>%                                      # Grouping entire tibble based on ETF assets
  mutate(returns = (log(returns) - log(lag(returns)))) %>% # Calculating log returns of 1 month period
  na.omit() %>%                                            # Removing NA
  tq_portfolio(assets_col  = asset, returns_col = returns, # Making a portfolio of ETF assets
    weights = Weights_of_Assets, col_rename  = "returns",  # Finally storing in the next line
    rebalance_on = "months") -> Monthly_Log_Porrtfolio_Log_Returns

#############################################################################################
#############################################################################################


#############################################################################################
################### Fetching US market factors for Fama-French model. #######################

# We download the data from the website but not onto hardrive, onto a temp R element itself

temp <- tempfile() # Temporary R file, currently empty on this line

"http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/Global_3_Factors_CSV.zip" %>% # URL with data
  download.file(temp, quiet = TRUE) %>%               # Downloading the data into temp element
  unz("Global_3_Factors.csv")                         # Unzipping the file


read_csv(unz(temp, "Global_3_Factors.csv"), skip = 6,
         col_types = cols(                            # Making sure the columns are of right data type
           `Mkt-RF` = col_double(),
           SMB = col_double(),
           HML = col_double(),
           RF = col_double())) %>%                    
  rename(Date = X1) %>%                               # Renaming first column as 'Date'
  mutate_at(vars(-Date), as.numeric) %>%              # Making sure other than 'Date', the rest are numeric
  mutate(Date =
    rollback(ymd(parse_date_time(Date, "%Y%m") + months(1)))) %>%# Changing Date column in Year & Month, 
                                                                 # same order as our monthly portfolio returns
                                                                 # Rollback ensures last fo the months is used, 
                                                                 # exactly like our portfolio.
  # Now, both FF_factors & portfolio returns use the last of the month for calculations
  filter(Date >= 
    first(Monthly_Log_Porrtfolio_Log_Returns$Date) & Date <= 
    last(Monthly_Log_Porrtfolio_Log_Returns$Date))-> FF_3_Factors# Storing the data

#############################################################################################
#############################################################################################

#############################################################################################
################# Combining returns and the Fama French model Factors. ######################

Monthly_Log_Porrtfolio_Log_Returns %>%       # Monthly return Data
  left_join(FF_3_Factors, by = "Date") %>%   # Left joining with Date column of 'FF_3_Factors'
  mutate(MKT_RF = FF_3_Factors$`Mkt-RF`/100, # Converting % to absolute value
         SMB = FF_3_Factors$SMB/100,         # Converting % to absolute value
         HML = FF_3_Factors$HML/100,         # Converting % to absolute value
         RF = FF_3_Factors$RF/100,           # Converting % to absolute value, storing below
         R_excess = round(returns - RF, 4)) -> Fama_French_Factors_And_portfolio_returns

#############################################################################################
#############################################################################################


#############################################################################################
################## Regressing and plotting with 95% confidence interval. ####################

png(filename=paste0("Fama_French_", gsub(" ","_",gsub(":","_",gsub("-","_",Sys.time()))), ".png",sep=''),
    width = 640, height = 360)                                # Opening image saving tool with correct filename

Fama_French_Factors_And_portfolio_returns %>%                 # Input for regression & plot
  do(model =  lm(R_excess ~ MKT_RF + SMB + HML, data = .)) %>%# Regression
  tidy(model, conf.int = T, conf.level = .95) %>%             # Selecting 95% confidence range
  mutate_if(is.numeric, funs(round(., 3))) %>%                # Rounding off till third decimal place.
  select(-statistic) %>%                                      # Unselecting 'statistics'. For easier plotting
  mutate_if(is.numeric, funs(round(., 3))) %>%                # Coercing into numeric and rounding-off if possible
  filter(term != "(Intercept)") %>%                           # Excluding 'Intercept' column in plot
  ggplot(aes(x = term, y = estimate, shape = term, color = term)) + 
  geom_point() +                                              # Plotting main data
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high)) +     # Plotting error bars
  labs(title = "Fama-French 3-Factor Coefficients for ETF Portfolio", # Labels, title, caption, etc
       subtitle = "Please revise the theory before investing, PLEASE",
       x = "",
       y = "Regression Coefficient",
       caption = "Data source: Fama French factor from website (Bloomberg data) and Yahoo Finance") +
  theme_gray(15) +                                            # Using the grey theme, and zooming text a little
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption  = element_text(hjust = 0))

dev.off()                                                     # Closing image saving tool

######################################## The END ############################################
#############################################################################################

rm(list=ls())