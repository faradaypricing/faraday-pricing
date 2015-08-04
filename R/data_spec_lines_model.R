



#' Returns the priced layers
#' 
#'
#' @param quote_id The quote ID
#' @return A string containing the url to the model
#' @details The function is vectorised over quote_id
#' @examples
#' get_model_url(c(345,762))
get_priced_layers <- function(inception_from, inception_to){
  
  conn_string <- 'driver={SQL Server};server=gblontpd54;database=pricing_prod;uid=pricingteam;pwd=F4raday00'
  
  
  i_from <- as.character(as.Date(inception_from))
  i_to <- as.character(as.Date(inception_to))
  
  ## set up connection
  con <- odbcDriverConnect(conn_string)

  query <- "select
            
              l.sequel_id,
              l.layer_id,
              l.layer_no,
              a.assured_name,
              qv.quote_id,
              qv.quote_version_no,
              qv.inception_date,
              qv.is_selected_final
              
              from dbo.quote_version qv
              inner join dbo.layer l on l.quote_version_id = qv.quote_version_id
              inner join dbo.quote_master qm on qv.quote_id = qm.quote_id
              inner join ref.assured a on a.assured_id = qm.assured_id
            
              where
              qv.is_deleted = 'N'
              and l.is_deleted = 'N'
              and qv.inception_date >= %incept_from%
              and qv.inception_date <= %incept_to%"

  query <- str_replace(query, "%incept_from%", i_from)
  query <- str_replace(query, "%incept_to%", i_to)
  
  ## execute query
  model.data <- sqlExecute(con, query = query, fetch = T, stringsAsFactors = F)
  
  ## close connection
  odbcClose(con)
  
  info.data <- get_info_team_renewal_status(inception_from = inception_from, 
                                            inception_to = inception_to,
                                            reporting_values = "ca")
  
  data <- info.data %>%
    left_join(model.data, by = c("PolicyRef" = "sequel_id"))
  
  
  report.data <- data %>%
    mutate(exposure = limit * EstSignedline) %>%
    select(incepting, PolicyRef, is_selected_final, WrittenUSD, exposure)
    
  
  
  return(report.data)
}

#' Returns the url for the quote id from the model
#' 
#'
#' @param quote_id The quote ID
#' @return A string containing the url to the model
#' @details The function is vectorised over quote_id
#' @examples
#' get_model_url(c(345,762))
get_model_url <- function(quote_id){
  
  base_url <- "http://uwportal/Pricing.UI.Model.Splines/Quotes/Versions?quoteId=%quote_id%&modelCode=SPL"
  
  retVal <- sapply(quote_id, function(q){
    str_replace(base_url, "%quote_id%", as.character(q))
  })
  
  return(retVal)
}

#' Returns a list of layers from accounts where the Sequel ID doesn't appear to be correct
#' 
#'
#' @param quote_id The quote ID
#' @return A dataframe with details of layers with strange Sequel IDs
#' @details The following methods are used to check Sequel ID integrity:
#' - Sequel ID pattern matching
#' - Ensure Sequel ID year = Inception year
#' @examples
#' get_funky_sequel_matches()
get_funky_sequel_matches <- function(){
  conn_string <- 'driver={SQL Server};server=gblontpd54;database=pricing_prod;uid=pricingteam;pwd=F4raday00'
  
  ## set up connection
  con <- odbcDriverConnect(conn_string)
  
  query <- "select
  
  l.sequel_id,
  l.layer_no,
  s.limit,
  s.deductible,
  a.assured_name,
  qv.quote_id,
  qv.quote_version_no,
  qv.inception_date
  
  from dbo.quote_version qv
  inner join dbo.layer l on l.quote_version_id = qv.quote_version_id
  inner join dbo.section s on s.layer_id = l.layer_id
  inner join dbo.quote_master qm on qv.quote_id = qm.quote_id
  inner join ref.assured a on a.assured_id = qm.assured_id
  
  where
  qv.is_deleted = 'N'
  and l.is_deleted = 'N'
  and l.sequel_id is not null
  and s.section_letter = 'A'
  and s.is_deleted = 'N'
  and qv.is_selected_final = 'Y'"
  
  ## execute query
  model.data <- sqlExecute(con, query = query, fetch = T, stringsAsFactors = F)
  
  ## close connection
  odbcClose(con)
  
  
  ## Various checks on Sequel IDs
  sequel_regex <- "([A-Za-z]{2})([0-9]{3})([A-Za-z])([0-9]{2})([A-Za-z][0-9]{3})"
  
  ## suggestion Sequel ID
  model.data$suggestion <- NA
  
  ## Check inception year against year from Sequel ID
  year <- as.character(as.POSIXlt(model.data$inception_date)$year - 100)
  sequel_years <- str_match(model.data$sequel_id,sequel_regex)
  
  year_match <- (year != sequel_years[,5]) & (!is.na(sequel_years[,5]))
  
  model.data$suggestion[year_match] <- paste0(sequel_years[,2],
                                              sequel_years[,3],
                                              sequel_years[,4],
                                              year,
                                              sequel_years[,6])[year_match]
  
  output <- model.data[year_match,] %>%
    mutate(method = "Inconsistent Year",
           url = get_model_url(quote_id),
           info_assured = assured_name)
  
  ##############################################################################
  # string distance for non-matching assureds
  # get closest assured name using Longest Common Substring method
  
  
  ## suggestions from info team - match inception, assured and limit/deductible
  yoa_from <- min(as.POSIXlt(model.data$inception_date)$year + 1900)
  yoa_to <- max(as.POSIXlt(model.data$inception_date)$year + 1900)
  
  info.data <- .get_info_team_policy_info(yoa_from = yoa_from,
                                         yoa_to,
                                         reporting_values = "ca")
  info.data <- info.data %>%
    mutate(assured_lower = tolower(Assured))
  
  assureds <- tolower(unique(model.data$assured_name))
  info_assured <- tolower(unique(info.data$Assured))
  
  ## match up assured name using LCS method
  assured_match <- do.call("rbind",lapply(assureds, function(name){
    
    match <- stringdist(name, info_assured , method = "lcs")
    name_match <- info_assured[match==min(match)][1]
    
    retVal <- data.frame(model_assured = name, info_assured = name_match, stringsAsFactors = F)
  }))
  
  ## write lower values of assured to both datasets
  model.data <- model.data %>%
    mutate(assured_lower = tolower(assured_name)) %>%
    inner_join(assured_match, by = c("assured_lower" = "model_assured"))
  

  #################################################
  # Check assured x limit x deductible x inception 
  
  ## Check doesn't match pattern
  pat_match <- is.na(sequel_years[,1])
  

  info.data <- info.data %>%
    filter(Assured != "") %>%
    ## tz ="" so as not to mess up dates!!!
    mutate(incept_date = as.Date(InceptionDate, tz="")) %>%
    select(assured_lower,incept_date,Limit,Deductible, RiskRef) %>%
    group_by(assured_lower,incept_date,Limit,Deductible) %>%
    ## can be a sequel_id for each blockstat - just take the first as this is only to offer
    ## a suggestion
    distinct()
    

  match.data <- model.data %>%
    ## tz ="" so as not to mess up dates!!!
    mutate(incept_date = as.Date(inception_date, tz="")) %>%
    ## match against fuzzy name match
    left_join(info.data,
              by = c("info_assured" = "assured_lower", 
                                 "incept_date",
                                 "limit" = "Limit",
                                 "deductible" = "Deductible"))

  model.data$suggestion[pat_match] <- match.data$RiskRef[pat_match]   
  
  model.data$info_assured <- gsub("(?<=\\b)([a-z])", "\\U\\1", tolower(model.data$info_assured), perl=TRUE)
  
  ## add to output
  output <- output %>%
    rbind(model.data[pat_match,] %>%
            select(-assured_lower) %>%
            mutate(method = "Invalid Sequel ID",
                   url = get_model_url(quote_id)))
  
  
  

  
  
  return(output)
}


get_priced_layers_v2 <- function(inception_from, inception_to){
  
  conn_string <- 'driver={SQL Server};server=gblontpd54;database=pricing_prod;uid=pricingteam;pwd=F4raday00'
  
  i_from <- as.character(as.Date(inception_from))
  i_to <- as.character(as.Date(inception_to))
  
  ## set up connection
  con <- odbcDriverConnect(conn_string)
  
  query <- 
  "select
    l.sequel_id,
    l.layer_id,
    l.layer_no,
    l.deal_type_code,
    s.limit,
    s.deductible,
    a.assured_name,
    qv.quote_id,
    qv.quote_version_no,
    qv.inception_date,
    qv.expiry_date,
    qv.is_selected_final,
    tr.brokerage_pct,
    tr.commission_pct,
    tr.combined_ratio_pct as combined_ratio,
    tr.loss_ratio_pct as loss_ratio,
    tr.combined_ratio_pct - tr.loss_ratio_pct - tr.brokerage_pct - tr.commission_pct as expense_pct,
    tr.loss_cost,
    tr.pct_100_gross_premium as gross_prem,
    tr.pct_100_cont_premium as contingent_prem,
    tr.pct_100_pc_return_premium as pc_return_prem
  
  from dbo.quote_version qv
    inner join dbo.layer l on l.quote_version_id = qv.quote_version_id
    inner join dbo.quote_master qm on qv.quote_id = qm.quote_id
    inner join ref.assured a on a.assured_id = qm.assured_id
    inner join dbo.section s on l.layer_id = s.layer_id
    inner join splines.terms_results tr on tr.layer_id = l.layer_id
    inner join [dbo].[calculation_tracking] ct on ct.layer_id = l.layer_id
    
  where
    qv.is_deleted = 'N'
    and l.is_deleted = 'N'
    and (l.layer_status_code = 'SGN' OR l.layer_status_code = 'WRT')
    and qv.inception_date >= '%incept_from%'
    and qv.inception_date <= '%incept_to%'
    and s.is_deleted = 'N'
    and s.section_letter = 'A'
    and ct.api_function_code = 'FullRatingResult'
    and ct.is_calculation_valid = 'Y'
    and tr.is_cummulative_result = CASE WHEN l.deal_type_code = 'OneYear' THEN 'N' ELSE 'Y' END
  "
  
  query <- str_replace(query, "%incept_from%", i_from)
  query <- str_replace(query, "%incept_to%", i_to)
  
  
  ## execute query
  model.data <- sqlExecute(con, query = query, fetch = T, stringsAsFactors = F)
  
  ## fix date fields
  model.data$inception_date <- as.Date(model.data$inception_date)
  model.data$expiry_date <- as.Date(model.data$expiry_date)
  model.data$yoa = as.POSIXlt(model.data$inception_date)$year + 1900
  
  # change is_selected_final to TRUE/FALSE
  model.data$is_selected_final <- ifelse(model.data$is_selected_final == 'Y', T, F)
  
  ## close connection
  odbcClose(con)
  
  
  return(model.data)
}

