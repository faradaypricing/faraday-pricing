library(RODBCext)
library(dplyr)
library(stringr)


#' Returns exchange rates for valid currency codes. The values returned are the value of 1 unit of the 
#' currency in the target currency. eg 1 GBP = X USD
#' 
#' It is vectorised over currency_codes
#'
#' @param currency_codes A character vector of ISO currency codes
#' @param to_currency_code The currency for which exchange rates are required. Default is GBL
#' @param as_at_date The date at which the exchange rate was valid
#' @return A numeric vector of exchange rates
#' @examples
#' get_exchange_rates(c("USD", "HKD"), "EUR", "as.Date(2005-04-01)")
get_exchange_rates <- function(currency_codes, to_currency_code = "GBP", as_at_date = Sys.Date()){
  
  ## get previous month
  dt <- as.POSIXlt(as_at_date)
  year = dt$year + 1899 + ifelse(dt$mon == 0, 0, 1)
  month = ifelse(dt$mon == 0, 12,dt$mon)
  
  proc_month <- paste(year,
                      str_pad(month, width = 2, pad = "0"),
                      sep="")
  
  pricing.con <- odbcDriverConnect('driver={SQL Server};
   server=GBLONTPD57;database=FaradayPricing;trusted_connection=true')
  
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
    top_n(1,1) %>%
    as.numeric()
  
  retVal <- c %>% 
    left_join(exch.rates, by = c("ccy" = "Ccy")) %>%
    select(ccy, rate = MonthCloseRate) %>%
    mutate(rate = to_rate / rate)
  
  return(retVal)
  
}


#' Returns all valid currency codes as stored within the Faraday warehouse
#' 
#' @examples
#' get_valid_curency_codes()
get_valid_curency_codes <-function(){
  
  pricing.con <- odbcDriverConnect('driver={SQL Server};
   server=GBLONTPD57;database=FaradayPricing;trusted_connection=true')
  
  query = "SELECT DISTINCT
            [Ccy] as ccy
            FROM [FaradayPricing].[dbo].[v_CcyRate]"
           
  
  ccy_codes <- sqlExecute(pricing.con, query = query, fetch = T, stringsAsFactors = F)
  
  odbcClose(pricing.con)
  
  return(ccy_codes$Ccy)
}