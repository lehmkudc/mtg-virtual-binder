suppressPackageStartupMessages({
  library(jsonlite)
})

addCard <- function(cardName, Qty){
  
  cardName <- checkName(cardName)
  
  currentCollection <- fread("owned.csv")
  
  haveCard <- currentCollection %>%
    filter( name == cardName ) %>%
    nrow() > 0
  
  if( haveCard ){
    currentCollection[name == cardName, qty := qty + Qty]
  } else {
    rbind( currentCollection, data.table(name = cardName, qty = Qty))
  }
  
  fwrite( currentCollection, file = "owned.csv")
  
  return( 
    currentCollection %>%
      filter( name == cardName | 1:n() > n() - 5  )
    
  )
}












getDeckPrices <- function( deck, full_bling = FALSE ){
  prices <- lapply( unique(deck$cardname), function(cardname){
    price_per_card <- getCardPrice(cardname, check_name = TRUE, full_bling = full_bling)
    
    return( data.table(cardname = cardname, price_per_card = price_per_card))
  }) %>% bind_rows
  
  deckPrices <- deck %>%
    left_join( prices, by = 'cardname' ) %>%
    mutate( price_of_set = price_per_card * qty )
  
  return( deckPrices )
}
