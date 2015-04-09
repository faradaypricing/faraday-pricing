library(RODBCext)
library(dplyr)
library(stringr)

##############################################################################
# (external)
#
# Returns exchange rates for valid currency codes
##############################################################################
get_exchange_rates <- function(currency_codes, as_at_date = Sys.Date()){
  
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
  
  ######################################################
  # vectorise currency codes
  codes <- as.character(currency_codes)
  
  c <- data.frame(ccy = as.character(currency_codes), stringsAsFactors = F)
  
  retVal <- c %>% 
    left_join(exch.rates, by = c("ccy" = "Ccy")) %>%
    select(ccy, rate = MonthCloseRate) 
  
  return(retVal)
  
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