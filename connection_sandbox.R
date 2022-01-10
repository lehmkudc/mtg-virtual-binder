suppressPackageStartupMessages({
  library(pool)
  library(tidyverse)
  library(RMariaDB)
  library(data.table)
  library(jsonlite)
  library(glue)
})


con <- dbPool(
  drv = MariaDB(),
  host = "localhost",
  port = 3306,
  username = "mtg_inventory",
  password = "Generic!2",
  dbname = "decklist_storage"
)

# Pulled from MTGJSON
allCards <- fread('../mtg_data/cards.csv')

# Determining what columns to actually keep for this project
glimpse(allCards)

allCards %>% 
  filter( name == "Lightning Bolt")



filteredCards <- allCards %>%
filteredCards <- allCards %>%
  #filter( name == "Ancestor's Chosen") %>%
  select(
    availability, colors, name, scryfallOracleId, types, manaValue
  ) %>%
  filter( grepl(pattern = "paper", x = availability)) %>%
  select( -availability ) %>%
  distinct() %>%
  rownames_to_column("cardID")


dbWriteTable(con, "cards", filteredCards)



allCards %>%
  filter( name == "Ancestor's Chosen") %>%
  glimpse()
# starting to realize that these card ids are gonna change.



allCards <- fromJSON("../mtg_data/AtomicCards.json")

allCardsAtomic <- allCards$data %>%
  bind_rows() %>%
  as.data.table() 


allCardsAtomic %>%
  filter( !is.na(side)) %>%
  glimpse()

allCardsAtomic %>%
  filter( name == "Lightning Bolt") %>%
  glimpse()



allCardsAtomic %>% 
  select(
    name, colors, scryfall_oid = identifiers.scryfallOracleId, manaCost, 
    manaValue, printings, types
    ) %>%
  filter( !is.na( scryfall_oid)) %>%
  arrange(scryfall_oid)




allCards$data %>%
  as.data.table()


cardName <- "Lightning Bot"
glue("https://api.scryfall.com/cards/named?fuzzy={cardName}") %>%
  URLencode() %>%
  fromJSON() %>%
  as.data.frame()


# Looks like I'm relying on auto-increment, but I need a way to handle DFC
