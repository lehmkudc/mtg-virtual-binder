suppressPackageStartupMessages({
  library(pool)
  library(tidyverse)
  library(RMariaDB)
  library(data.table)
  library(jsonlite)
  library(glue)
})

# This is just the localhost prototype I'm using for now
connectToPrototypeDatabase <- function(){
  pool <- dbPool(
    drv = MariaDB(),
    host = "localhost",
    port = 3306,
    username = "mtg_inventory",
    password = "Generic!2",
    dbname = "decklist_storage"
  )
  
  print( dbListTables(pool) )
  
  return( pool )
}


# Initialize database's "cards" table 
initializeCards <- function(
  pool, append = TRUE, overwrite = FALSE
){
  
  if( overwrite & append ){
    stop( "Please select overwrite or append mode")
  }
  
  if( !overwrite & !append ){
    stop( "Please select overwrite or append mode")
  }
  
  allCards <- fromJSON("../mtg_data/AtomicCards.json")
  
  allAtomicBound <- allCards$data %>%
    bind_rows() %>%
    as.data.table()
  
  existingCards <- character(0)
  
  if( append ){
    existingCards <- dbGetQuery(con, "SELECT name FROM cards") %>%
      pull( name )
  }
  
  allAtomicPrepped <- allAtomicBound %>% 
    select(
      name, colors, scryfallOracleId = identifiers.scryfallOracleId, manaCost, 
      manaValue, printings, types, side
    ) %>%
    filter( !is.na( scryfallOracleId)) %>%
    filter( side %in% c(NA, 'a')) %>%
    select( name, colors, scryfallOracleId, manaCost, manaValue, printings, types) %>%
    # filter( grepl(pattern = "helix",x = name,ignore.case = TRUE)) %>%
    rowwise() %>%
    mutate(
      colors = paste( colors, collapse = ","),
      printings = paste( printings, collapse = ","),
      types = paste(types, collapse = ",")
    ) %>%
    filter(
      !( name %in% existingCards )
    )
  
  dbWriteTable( pool, "cards", allAtomicPrepped,
                append = append, row.name = FALSE, overwrite = overwrite)
  
}

# I'm relying on 