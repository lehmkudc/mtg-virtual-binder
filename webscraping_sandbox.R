

suppressPackageStartupMessages({
  library(tidyverse)
  library(rvest)
  library(glue)
  library(lubridate)
  library(curl)
  library(httr)
  library(jsonlite)
})


getTourneyIDs <- function( 
  format = "Modern", months_prior = 3
){
  # TODO: allow for different formats
  # TODO: allow for different tournament types
  # TODO: fix situation with too many decklists creating pagination
  # TODO: add better date range inputs
  # TODO: add verbose mode for tournament metadata
  
  end_date <- Sys.Date()
  start_date <- Sys.Date() - months( months_prior )
  
  
  search_url <- paste0(
    "https://www.mtggoldfish.com/tournament_searches/create?utf8=%E2%9C%93",
    "&tournament_search[name]=", "Modern+Challenge",
    "&tournament_search[format]=", "modern",
    "&tournament_search[date_range]=",
    start_date %>% format("%m/%d/%Y"), "+-+",
    end_date %>% format("%m/%d/%Y"),
    "&commit=Search"
  )
  
  search_page_raw <- read_html(search_url)
  
  tournament_ids <- search_page_raw %>%
    html_node("body") %>%
    html_node("main") %>%
    html_element(".layout-container-fluid") %>%
    html_node("table") %>%
    html_elements('tr') %>%
    html_elements('a') %>%
    html_attr('href') %>%
    str_remove("/tournament/")
  
  return( tournament_ids )
}

getTourneyIDs()




getDeckIDs <- function( tourney_id, place = NULL, verbose = FALSE ){
  
  tourney_url <- paste0( "https://www.mtggoldfish.com/tournament/", tourney_id)
  
  deck_ids <- read_html(tourney_url) %>%
    html_node("body") %>%
    html_node("main") %>%
    html_element('.layout-container-fluid') %>%
    html_element('.deck-display') %>%
    html_element('.deck-display-left-contents') %>%
    html_element("table") %>%
    html_elements('.tournament-decklist') %>%
    html_attr('data-deckid') 
  
  if(!is.null(place)){
    deck_ids <- deck_ids %>% head(place)
  }
  
  if (verbose){
    
    verbose_df <- read_html(tourney_url) %>%
      html_node("body") %>%
      html_node("main") %>%
      html_element('.layout-container-fluid') %>%
      html_element('.deck-display') %>%
      html_element('.deck-display-left-contents') %>%
      html_element("table") %>%
      html_table() %>%
      filter( `Toggle Deck` == "Expand") %>%
      select( Place, Deck, Pilot ) %>%
      { if (!is.null(place)) head(.,place) else .} %>%
      mutate( deck_id = deck_ids )
    
    deck_ids <- verbose_df
    
  }
  
  return( deck_ids )
}


getDeckIDs( getTourneyIDs()[5], place = 8, verbose=TRUE )






deck_download_url <- paste0(
  "https://www.mtggoldfish.com/deck/download/", 
  deck_ids[1]
)


source('inventory_functions.R')


parseDecklist(deck_download_url)

parseDecklist(deck_id = 1593)


tourney_ids <- getTourneyIDs(months_prior = 1)
place <- 1

total_decklists <- map(tourney_ids, function(tourney_id){
  Sys.sleep(place/60)
  getDeckIDs(tourney_id, place=place, verbose = TRUE)
}) %>% bind_rows()




