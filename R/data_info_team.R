
#' Returns the latest query version for the specified query ID
#' 
#'
#' @param server The server to query
#' @param database The database
#' @param user The user ID (leave blank if using Windows authentication)
#' @param password The password (leave blank if using Windows authentication)
#' @param query_id The query ID
#' @return A list with the following items:
#' - QueryID
#' - QueryVersion
#' - Query
#' - update_user
#' - update_date
#' @examples
#' get_info_team_query_from_meta(query_id = "RenewalStatus")
get_info_team_query_from_meta <- function(server = "FARADAYWAREHOUSE", 
                                          database = "FaradayInfoTeam", 
                                          user ="", 
                                          password = "",
                                          query_id
){
  
  
  conn_string <- if(user != ""){
    paste0('driver={SQL Server};server=',server,';database=',database,';uid=', user,';pwd=',password)
  }else{
    paste0('driver={SQL Server};server=',server,';database=',database,';Trusted_Connection = Yes')
  }
  
  ## set up connection
  con <- odbcDriverConnect(conn_string)
  
  ## set up query
  query <- "SELECT  * 
            FROM MetaQuery_Current MQQ2
            WHERE MQQ2.QueryID IN ('&&query_id&&')"
  query <- str_replace(query, "&&query_id&&", query_id)
  
  ## execute query
  data <- sqlExecute(con, query = query, fetch = T, stringsAsFactors = F)
  
  ## close connection
  odbcClose(con)
  
  return(data)

}


#' Returns the renewal information for the selected inception and expiry date ranges
#' 
#'
#' @param server The server to query
#' @param database The database
#' @param user The user ID (leave blank if using Windows authentication)
#' @param password The password (leave blank if using Windows authentication)
#' @param syndicate Vector containing syndicate IDs
#' @param inception_from First inception date
#' @param inception_to Latest inception date
#' @param reporting_level The reporting level to query at - with 'team' or 'atg'
#' @param reporting_values Vector of reporting level values
#' @return A dataset with risk information
#' @examples
#' get_info_team_renewal_status(inception_from = "2015-07-01", 
#'    inception_to = "2015-08-01", 
#'    reporting_level = "team",
#'    reporting_values = "ca")
get_info_team_renewal_status <- function(inception_from,
                                         inception_to,
                                         reporting_level = "team",
                                         reporting_values,
                                         syndicate = c(1192,435)){
  
  server = "GBLONTPD06"
  database = "WWD"
  user = ""
  password = ""
  
  ## exchange rates
  
  exch_rates <- get_exchange_rates(c("USD","CAD"))
  
  usd_exch_rate <- 1 / exch_rates[exch_rates$ccy=="usd","rate"]
  cad_exch_rate <- 1 / exch_rates[exch_rates$ccy=="cad","rate"]
  
  ## set up query with parameters
  level <- ifelse(tolower(reporting_level) == "team", "anb.team", "anb.majortailgroup")
  values <- paste0("'",reporting_values,"'", collapse=",")
  synd <- paste(as.character(syndicate), collapse=",")
  
  expiry_from <- inception_from
  expiry_to <- as.character(as.Date(inception_to) + 365*2)
  

  
  query <- get_info_team_query_from_meta(query_id = "RenewalStatus")$Query
  
  query <- str_replace_all(query, "&ExpiryFrom&", expiry_from)
  query <- str_replace_all(query, "&ExpiryTo&", expiry_to)
  query <- str_replace_all(query, "&InceptFrom&", inception_from)
  query <- str_replace_all(query, "&InceptTo&", inception_to)
  query <- str_replace_all(query, "&Synd&", synd)
  query <- str_replace_all(query, "&Where&", level)
  query <- str_replace_all(query, "&WhereValue&", values)
  query <- str_replace_all(query, "&USD&", usd_exch_rate)
  query <- str_replace_all(query, "&CAD&", cad_exch_rate)
  
  ## set up connection
  conn_string <- if(user != ""){
    paste0('driver={SQL Server};server=',server,';database=',database,';uid=', user,';pwd=',password)
  }else{
    paste0('driver={SQL Server};server=',server,';database=',database,';Trusted_Connection = Yes')
  }

  con <- odbcDriverConnect(conn_string)
  
  ## execute query
  data <- sqlExecute(con, query = query, fetch = T, stringsAsFactors = F)
  
  ## close connection
  odbcClose(con)
  
  return(data)
}


#' Returns the renewal information for the selected inception and expiry date ranges
#' 
#'
#' @param syndicate Vector containing syndicate IDs
#' @param inception_from First inception date
#' @param inception_to Latest inception date
#' @param reporting_level The reporting level to query at - options are 'class', 'minorclass' or 'aatg'
#' @param reporting_values Vector of reporting level values
#' @return A dataset with risk information
#' @examples
#' get_info_team_policy_info(2013,2015,"class", "ca")
get_info_team_policy_info<- function(yoa_from,
                                       yoa_to,
                                       reporting_level = "class",
                                       reporting_values,
                                       syndicate = c(1192,435)){
  
  server = "FARADAYWAREHOUSE"
  database = "FaradayInfoTeam"
  user = ""
  password = ""
  
  ## set up query with parameters
  r_level <- tolower(reporting_level)
  level <- ifelse(r_level == "class", "fsg.classtype", 
                  ifelse(r_level == "minorclass",
                         "fsg.minorclass",
                         "fsr.actuarialatg"))
  
  values <- paste0("'",reporting_values,"'", collapse=",")
  synd <- paste(as.character(syndicate), collapse=",")

  query <- "SELECT

  fsg.Classtype 
  , fsg.majorclassDesc
  , fsg.minorclass
  , fsr.actuarialATG as AATG
  , FSR.SyndRef
  , FSR.RiskRef
  , fsr.PrevRiskRef
  , fsr.PrevYoA
  , fsr.leadind
  , fsr.LeadUW
  , fsr.LeadLine
  , FSR.UWDetailsComplete
  , FSR.CurrUw
  , FSR.YoA
  , FSr.Block
  , FSr.Stat
  , FSR.InsType
  , fsr.InsTypeDesc
  , FSR.PeriodType
  , FSR.Assured
  , FSR.Programref
  , FSR.InceptionDate
  , FSR.ExpiryDate
  , FSR.LineStatus
  , FSR.LimitCCYiso
  , FSR.Limit
  , FSR.Deductible
  , FSR.SignedLine
  , FSR.SignedOrder
  , FSR.WrittenLine
  , FSR.EstSignedLine
  
  
  
  FROM  
  
  FaradayWarehouse.dbo.FW_D_Risk as FSR
  
  LEFT OUTER JOIN
  FaradayReporting.dbo.reporting as FSG
  ON FSR.BlockStat = FSG.Class AND FSR.SyndRef = FSG.SyndRef
  
  
  WHERE 
  
  FSR.Syndref in (&Synd&)
  and FSR.YoA between  &YearFrom& and &YearTo&
  AND  &Where& in (&WhereValue&) 
  and fsr.actuarialATG not like '9%'"
  
  query <- str_replace_all(query, "&AsAt&", as_at)
  query <- str_replace_all(query, "&YearFrom&", yoa_from)
  query <- str_replace_all(query, "&YearTo&", yoa_to)
  query <- str_replace_all(query, "&Synd&", synd)
  query <- str_replace_all(query, "&Where&", level)
  query <- str_replace_all(query, "&WhereValue&", values)
  
  ## set up connection
  conn_string <- if(user != ""){
    paste0('driver={SQL Server};server=',server,';database=',database,';uid=', user,';pwd=',password)
  }else{
    paste0('driver={SQL Server};server=',server,';database=',database,';Trusted_Connection = Yes')
  }
  
  con <- odbcDriverConnect(conn_string)
  
  ## execute query
  data <- sqlExecute(con, query = query, fetch = T, stringsAsFactors = F)
  
  ## close connection
  odbcClose(con)
  
  return(data)
}

