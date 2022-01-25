source('database_functions.R')
source('mtggoldfish_functions.R')
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

# Should I even be passing tuples back and forth? If I was working in almost
#  any other language these would be classes.

# TODO fix ID and other column names to be distinct identities but coordinated
#  across platforms


# Get a Decklist
tourneys <- getTourneyIDs( months_prior = 1, verbose = TRUE )

tourney <- tourneys[1,]

deck <- getDeckIDs( tourney$tourney_id[1], place = 1, verbose = TRUE)

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
  
  return()
  
}

deck$Place %>% str_replace('\\D+',"")

processDecklist <- function( pool, deck, tourney ){
  
  decklist <- parseDecklist( deck_id = deck$deck_id )
  
  # Add deck metadata to database
  #print( tourney )
  #print( deck )
  place <- deck$Place %>% str_replace('\\D+',"")
  
  glue("
  INSERT INTO decks (deckID, deckName, pilot, formatID, place) (
  SELECT 
    {deck$deck_id}, 
    {deck$Deck}, 
    {deck$Pilot}, 
    (SELECT formatID FROM formats WHERE formatName = {tourney$Format} ), 
    {place},
    {tourney$tourney_id}
  WHERE NOT EXISTS (
    SELECT 1 FROM decks
    WHERE deckID = {deck$deck_id}
  ));"
  )
  
  # Add each deckitem to database
  
  storeDeckItem()
  
  
}


processDecklist( pool, deck, tourney )




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






