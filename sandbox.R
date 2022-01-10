
suppressPackageStartupMessages({
  library(tidyverse)
  library(data.table)
  library(jsonlite)
  library(glue)
  library(RMariaDB)
  library(pool)
})

mtg_pool <- dbPool(drv = MySQL(), host = "ec2-18-191-98-150.us-east-2.compute.amazonaws.com" )

source( "inventory_functions.R")


collection <- fread( "owned.csv" ) %>%
  mutate( owned_qty = qty )


burnPrices <- "decklists/modern/Deck - Burn.txt" %>%
  parseDecklist() %>%
  getDeckPrices() %>%
  mutate( deck_qty = qty )




addCard <- function(cardName, Qty){
  
  cardName <- checkName(cardName)
  
  currentCollection <- fread("owned.csv")
  
  haveCard <- currentCollection %>%
    filter( name == cardName ) %>%
    nrow() > 0
  
  
  if( haveCard ){
    currentCollection[name == cardName, qty := qty + Qty]
  } else {
    currentCollection <- rbind( currentCollection, data.table(name = cardName, qty = Qty))
  }
  
  fwrite( currentCollection, file = "owned.csv")
  
  return( 
    currentCollection %>%
      filter( name == cardName | 1:n() > n() - 5  )
    
  )
}

addCard(
  "Ephemerate", 
  4
)



getMyDeckPrice <- function(deck, return_total = FALSE){

  if( typeof(deck) == "character"){
    deck <- parseDecklist(deck)
  }
  
  if( !('price_per_card' %in% colnames(deck)) ){
    deck <- getDeckPrices(deck)
  }
  
  deck <- deck %>%
    mutate( deck_qty = qty )
  
  collection <- fread( "owned.csv" ) %>%
    mutate( owned_qty = qty )
  
  price_diff <- deck %>%
    left_join( collection, by = c("cardname" = "name") ) %>%
    select( -qty.x, -qty.y, -price_of_set ) %>%
    mutate( owned_qty = ifelse( is.na(owned_qty), 0, owned_qty)) %>%
    mutate( needed_qty = deck_qty - owned_qty ) %>%
    mutate( needed_qty = ifelse( needed_qty < 0, 0, needed_qty) ) %>%
    filter( needed_qty > 0 ) %>%
    mutate( price_of_remaining_set = needed_qty * price_per_card ) %>%
    arrange( - price_per_card )
  
  if (return_total){
    return(
      price_diff %>%
        group_by(board) %>%
        summarise(
          total_price = sum( price_of_remaining_set )
        )
    )
  } else {
    return( 
      price_diff
    )
  }
    
}


getDeckPrices <- function( deck ){
  #browser()
  prices <- lapply( deck$cardname, function(cardname){
    price_per_card <- getCardPrice(cardname, check_name = FALSE)
    
    return( data.table(cardname = cardname, price_per_card = price_per_card))
  }) %>% bind_rows
  
  deckPrices <- deck %>%
    left_join( prices, by = 'cardname' ) %>%
    mutate( price_of_set = price_per_card * qty )
  
  return( deckPrices )
}

filepath <- "decklists/legacy/Deck - Death and Taxes.txt"
filepath <- "decklists/pauper/Deck - U.txt"
filepath <- "decklists/legacy/Deck - Lands.txt"
deck <- filepath %>% parseDecklist()
deckPrices <- deck %>% getDeckPrices(full_bling = TRUE)

deckPrices %>%
  group_by( board ) %>%
  arrange( -price_of_set )

deckPrices %>%
  group_by( board ) %>%
  arrange( -price_of_set ) %>%
  summarize( total_price = sum( price_of_set ))

getMyDeckPrice( deckPrices, return_total = TRUE )
getMyDeckPrice( deckPrices, return_total = FALSE)


parseDecklist("decklists/legacy/Deck - Death and Taxes.txt")

cardData$data %>%
  as.data.table() %>%
  select( name, games, prices.usd, prices.usd_foil, prices.usd_etched)





con <- dbConnect(
  drv = MariaDB(),
  host = "localhost",
  port = 3306,
  username = "marina",
  password = "cool_beans",
  dbname = "marina_inventory"
)


dbGetQuery(
  con,
  "
  "
)




con <- dbConnect(
  drv = MariaDB(),
  host = "localhost",
  port = 3306,
  username = "root",
  password = "c6H12-o6"
)
