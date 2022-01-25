suppressPackageStartupMessages({
  library(jsonlite)
})


getCardPrice <- function( cardName, check_name = TRUE, return_all = FALSE, full_bling = FALSE ){
  
  if (check_name){
    cardName <- checkName(cardName)
  }
  
  if (cardName %in% c("Plains", "Island", "Swamp", "Mountain", "Forest")){
    return( 0 )
  }
  cardName <- URLencode(cardName)
  cardData <- glue("https://api.scryfall.com/cards/search?q={cardName}&unique=prints&include_variations=true&include_extras=true") %>%
    fromJSON()
  
  if (return_all){
    allData <- cardData$data %>%
      as.data.table() %>%
      filter( name == URLdecode(cardName)) %>%
      select( object, name, games, set_name, digital, border_color, prices.usd, variation, set, collector_number, set_type) %>%
      arrange( prices.usd)
    
    return( allData )
  }
  
  if (full_bling){
    blingest <- cardData$data %>%
      as.data.table() %>%
      select( name, games, prices.usd, prices.usd_foil, prices.usd_etched ) %>%
      filter( name == URLdecode(cardName)) %>%
      filter( grepl( pattern = "paper", x = games)) %>%
      mutate_at( .vars = c("prices.usd", "prices.usd_foil", "prices.usd_etched"),.funs = as.numeric) %>%
      mutate( blingest = pmax( prices.usd, prices.usd_foil, prices.usd_etched, na.rm = TRUE)) %>%
      filter( blingest == max( blingest, na.rm = TRUE )) %>%
      head(1) 
    
    return( blingest$blingest )
  }
  
  cheapest <- cardData$data %>%
    as.data.table() %>%
    select( object, name, games, set_name, digital, border_color, prices.usd, variation, set, collector_number, set_type) %>%
    filter( name == URLdecode(cardName)) %>%
    filter( grepl( pattern = "paper", x = games)) %>%
    mutate( prices.usd = as.numeric(prices.usd)) %>%
    filter( prices.usd == min(prices.usd, na.rm = TRUE)) %>%
    head( 1 )
  
  return( cheapest$prices.usd )
}



checkName <- function(cardName){
  cardName <- URLencode(cardName)
  
  correctName <- glue("https://api.scryfall.com/cards/named?fuzzy={cardName}") %>%
    fromJSON() %>%
    .$name
  
  return( correctName )
}

