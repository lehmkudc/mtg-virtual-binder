source('database_functions.R')
source('scraping_functions.R')
source('inventory_functions.R')

pool <- connectToPrototypeDatabase()






# When processing a decklist, all of it's decks and decklists have to be
#  added to the database all at once

# An ideal query function should check the database first, then if it's not
#  there, scrape it. 

# Getting tourney IDs before caching is sort of inevitable, although storing 
#  the metadata is helpful in general. This is the bit we use to determine
#  if the item is cached.


# I'll leave the original functions as is, but build the scraper functions to 
#  dial down all the way down. 

# Each level can have their own database check.

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



# Get a Decklist
tourneys <- getTourneyIDs( months_prior = 1, verbose = TRUE )

deck <- getDeckIDs( tourneys$tourney_id[1], place = 1, verbose = TRUE)

decklist <- parseDecklist(deck_id = deck$deck_id)

deckItem <- decklist[1,]
deckID <- deck$deck_id

storeDeckItem <- function(
  pool, deckItem, deckID
){
  
  # Get the cardID from database
  cardname <- checkName( deckItem$cardname )
  
  cardID <- glue(
    "SELECT cardID FROM cards WHERE name = '{cardname}'"
  ) %>%
    dbGetQuery(pool, .) %>% unlist() %>% unname()
  
  dbItem <- deckItem %>%
    mutate(
      main = ifelse( board == 'main', 1, 0),
      deckID = deckID,
      cardID = cardID
    ) %>%
    select( cardID, deckID, qty, main )
  
  # Check if item already exists
  
  glue("
    INSERT INTO decklist (cardID, deckID, main, qty) (
      SELECT {cardID}, {deckID}, {dbItem$main[1]}, {dbItem$qty[1]}
      WHERE NOT EXISTS (
        SELECT 1 FROM decklist
        WHERE cardID = {cardID}
        AND deckID = {deckID}
        AND main = {dbItem$main[1]}
        AND qty = {dbItem$qty[1]}
      )
    );
  ")
  
}

decklist <- parseDecklist(deck_id = deck$deck_id)
deck

decklist$cardname[1]

# This seems pretty inconsistent. Name is nowhere good enough especially for
#   doublefaced cards.
glue(
  "SELECT * FROM cards
  WHERE name = '{deckitem$cardname}'"
) %>%
  dbGetQuery(pool, .) %>%
  cbind(
    deckitem
  ) %>%
  mutate(
    main = ifelse( board == 'main', 1, 0),
    deckID = deck$deck_id
  ) %>%
  select(
    cardID, deckID, qty, main
  )






