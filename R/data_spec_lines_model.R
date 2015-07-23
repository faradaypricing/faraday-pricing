




get_priced_layers <- function(inception_from, inception_to){
  
  conn_string <- 'driver={SQL Server};server=gblontpd54;database=pricing_prod;uid=pricingteam;pwd=F4raday00'
  
  
  i_from <- as.character(as.Date(inception_from))
  i_to <- as.character(as.Date(inception_to))
  
  ## set up connection
  con <- odbcDriverConnect(conn_string)

  query <- "select
            
              l.sequel_id,
              l.layer_id,
              a.assured_name,
              qv.inception_date,
              qv.is_selected_final
              
              from dbo.quote_version qv
              inner join dbo.layer l on l.quote_version_id = qv.quote_version_id
              inner join dbo.quote_master qm on qv.quote_id = qm.quote_id
              inner join ref.assured a on a.assured_id = qm.assured_id
            
              where
              qv.is_deleted = 'N'
              and l.is_deleted = 'N'"

  
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

