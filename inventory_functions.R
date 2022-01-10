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




parseDecklist <- function( filepath = NULL, deck_id = NULL ){
  
  if( !is.null(deck_id) ){
    filepath <- paste0("https://www.mtggoldfish.com/deck/download/", deck_id )
  }
  
  if( is.null(filepath)){
    stop("No Deck Specified")
  }
  
  lines <- readLines(filepath)
  break_pt <- which( lines == "")
  
  if( length(break_pt) == 0 ){
    print( lines )
    return( data.frame() )
  }
  
  reg <- "^(\\w+)\\s?(.*)$"
  main <- data.frame(raw = lines[1:(break_pt-1)]) %>%
    mutate(
      qty = sub( reg, "\\1", raw ),
      cardname = sub( reg, "\\2", raw ),
      board = "main"
    )
  
  side <- data.frame(raw = lines[(break_pt+1):length(lines)]) %>%
    mutate(
      qty = sub( reg, "\\1", raw ),
      cardname = sub( reg, "\\2", raw ),
      board = "side"
    )
  
  deck <- rbind( main, side ) %>%
    select( -raw ) %>%
    mutate(qty = as.integer(qty))
  
  return( deck )
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
