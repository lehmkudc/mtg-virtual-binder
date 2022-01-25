suppressPackageStartupMessages({
  library(tidyverse)
  library(rvest)
  library(glue)
  library(lubridate)
  library(curl)
  library(httr)
  library(jsonlite)
  library(data.table)
})




getTourneyIDs <- function( 
  format = "Modern", months_prior = 3, verbose = FALSE
){
  # TODO: allow for different tournament types
  # TODO: add better date range inputs
  
  end_date <- Sys.Date()
  start_date <- Sys.Date() - months( months_prior )
  
  search_url <- paste0(
    "https://www.mtggoldfish.com/tournament_searches/create?utf8=%E2%9C%93",
    "&tournament_search[name]=", "Challenge",
    "&tournament_search[format]=", format,
    "&tournament_search[date_range]=",
    start_date %>% format("%m/%d/%Y"), "+-+",
    end_date %>% format("%m/%d/%Y"),
    "&commit=Search"
  )
  
  total_ids <- c()
  total_table <- data.table()
  
  paged <- TRUE
  while( paged ){
    
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
    
    verbose_table <- search_page_raw %>%
      html_node("body") %>%
      html_node("main") %>%
      html_element(".layout-container-fluid") %>%
      html_node("table") %>%
      html_table %>%
      mutate( tourney_id = tournament_ids )
    
    total_ids <- c( total_ids, tournament_ids )
    total_table <- bind_rows( total_table, verbose_table )
    
    # is there pagination at all?
    pagination <- search_page_raw %>%
      html_node("body") %>%
      html_node("main") %>%
      html_element(".layout-container-fluid") %>%
      html_element('nav')
    
    
    if( is.na(pagination)){ # No pagination at all
      paged <- FALSE
    } else {
      
      # Is there another page?
      ended_page <- search_page_raw %>%
        html_node("body") %>%
        html_node("main") %>%
        html_element(".layout-container-fluid") %>%
        html_element('nav') %>%
        html_element('ul') %>%
        html_children() %>%
        html_attr('class') %>%
        last() %>%
        grepl(pattern = "disabled")
      
      
      if( !ended_page ){
        
        # extract the next page's URL tail
        next_page<- search_page_raw %>%
          html_node("body") %>%
          html_node("main") %>%
          html_element(".layout-container-fluid") %>%
          html_element('nav') %>%
          html_element('ul') %>%
          html_children() %>%
          last() %>%
          html_element('a') %>%
          html_attr('href')
        
        
        
        search_url <- paste0("https://www.mtggoldfish.com", next_page )
        
      } else {
        
        
        paged <- FALSE
      }
    }
  }
  
  if( verbose ){
    return( total_table )
  } else {
    return( total_ids )
  }
  
}







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






scrapeTourneyIDs <- function(raw_html){
  output <- raw_html %>%
    html_node("body") %>%
    html_node("main") %>%
    html_element(".layout-container-fluid") %>%
    html_node("table") %>%
    html_elements('tr') %>%
    html_elements('a') %>%
    html_attr('href') %>%
    str_remove("/tournament/")
  
  return( output )
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