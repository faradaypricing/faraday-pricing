library(RODBCext)
library(dplyr)
library(stringr)

##############################################################################
# (internal)
#
# Returns exchange rates for valid currency codes
# Default is exchange rate to GBP
##############################################################################
.get_exchange_rates.base <- function(currency_codes, to_currency_code = "GBP", as_at_date = Sys.Date()){
  
  ## get previous month
  dt <- as.POSIXlt(as_at_date)
  year = dt$year + 1899 + ifelse(dt$mon == 0, 0, 1)
  month = ifelse(dt$mon == 0, 12,dt$mon)
  
  proc_month <- paste(year,
                      str_pad(month, width = 2, pad = "0"),
                      sep="")
  
  pricing.con <- odbcDriverConnect('driver={SQL Server};
   server=GBLONTPD39;database=FaradayPricing;trusted_connection=true')
  
  query = "SELECT 
            [Ccy],
            [ProcMonth],
            [MonthCloseRate]
            FROM [FaradayPricing].[dbo].[v_CcyRate]
            WHERE ProcMonth = ?"
  
  exch.rates <- sqlExecute(pricing.con, query = query, data = proc_month, fetch = T, stringsAsFactors = F)

  odbcClose(pricing.con)
  
  exch.rates$Ccy <- tolower(exch.rates$Ccy)
  ######################################################
  # vectorise currency codes

  c <- data.frame(ccy = tolower(as.character(currency_codes)), stringsAsFactors = F)
  
  to_code <- tolower(as.character(to_currency_code))
  to_rate <- exch.rates %>%
    filter(Ccy == to_code) %>%
    select(MonthCloseRate) %>%
    top_n(1) %>%
    as.numeric()
  
  retVal <- c %>% 
    left_join(exch.rates, by = c("ccy" = "Ccy")) %>%
    select(ccy, rate = MonthCloseRate) %>%
    mutate(rate = to_rate / rate)
  
  return(retVal)
  
}

##############################################################################
# (external)
#
# Returns exchange rates for valid currency codes
# Default is exchange rate to GBP
##############################################################################
get_exchange_rates <- function(currency_codes, to_currency_code = "GBP", as_at_date = Sys.Date()){
  invisible(.get_exchange_rates.base(currency_codes, to_currency_code, as_at_date))
}


##############################################################################
# (external)
#
# Returns all currency codes
##############################################################################
get_valid_curency_codes <-function(){
  
  pricing.con <- odbcDriverConnect('driver={SQL Server};
   server=GBLONTPD39;database=FaradayPricing;trusted_connection=true')
  
  query = "SELECT DISTINCT
            [Ccy]
            FROM [FaradayPricing].[dbo].[v_CcyRate]"
           
  
  ccy_codes <- sqlExecute(pricing.con, query = query, fetch = T, stringsAsFactors = F)
  
  odbcClose(pricing.con)
  
  return(ccy_codes$Ccy)
}