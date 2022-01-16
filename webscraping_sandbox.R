source('scraping_functions.R')
source('inventory_functions.R')

suppressPackageStartupMessages({
  library(plotly)
  library(umap)
})

tourney_ids <- getTourneyIDs(format = "Pauper", months_prior =6)
place <- 16

total_decklists <- map(tourney_ids, function(tourney_id){
  #Sys.sleep(place/60)
  getDeckIDs(tourney_id, place=place, verbose = TRUE)
}) %>% bind_rows() %>% as.data.table()


vector_mapped_lists <- pmap( total_decklists, function(Place, Deck, Pilot, deck_id){

  Sys.sleep(1)
  print( c(Deck, deck_id))
  
  tryCatch(expr = {
    decklist <- parseDecklist( deck_id = deck_id ) %>%
      filter( board == "main")
  }, error = function(e){
    decklist <- data.table()
  })
  if (is.null(decklist)){
    return( data.table() )
  }
  
  if( nrow(decklist) == 0 ){
    return( data.table() )
  }
  
  vectored_list <- decklist %>%
    #mutate( column_name = paste0(cardname, "_", board)) %>%
    column_to_rownames( "cardname") %>%
    select( qty ) %>%
    t() %>%
    as.data.table()
  
  preview_text <- paste0(
    Deck, ":\n",
    decklist %>%
      filter( board == 'main' ) %>%
      mutate(line = paste( qty, cardname) ) %>%
      arrange( desc( qty) ) %>%
      pull( line ) %>%
      head( 10 ) %>%
      paste( ., collapse = "\n "),
    "\n..."
  )
  
  vectored_list <- vectored_list %>%
    mutate( 
      Place = Place,
      Deck = Deck,
      Pilot = Pilot,
      deck_id = deck_id,
      preview_text = preview_text
    ) %>%
    select( Place, Deck, Pilot, deck_id, preview_text, everything() )
  
  return( vectored_list )
  
}) 

vector_mapped_lists <- vector_mapped_lists %>%
  bind_rows() %>%
  replace( is.na(.), 0)

fwrite(vector_mapped_lists, "pauper_deck_dump.csv")


PCA_values <- vector_mapped_lists %>%
  column_to_rownames('deck_id') %>%
  select( -Place, -Deck, -Pilot) %>%
  #t() %>%
  prcomp(scale = TRUE)


first_two <- PCA_values$x %>%
  as.data.table(keep.rownames = TRUE) %>%
  select( deck_id= rn, PC1, PC2 ) 

other_two <- PCA_values$x %>%
  as.data.table(keep.rownames = TRUE) %>%
  mutate(
    axis_1 = mean(PC1, PC3, PC5, PC7, PC9),
    axis_2 = mean(PC2, PC4, PC6, PC8, PC10)
  ) %>%
  select( deck_id = rn, PC1 = axis_1, PC2 = axis_2)

other_two <- PCA_values$x %>%
  as.data.table(keep.rownames = TRUE) %>%
  select( deck_id = rn, PC1 = PC3, PC2 = PC4)

completed <- left_join( 
  vector_mapped_lists %>% select( Place, Deck, Pilot, deck_id),
  first_two,
  by = "deck_id"
)


p <- ggplot( data = completed ) +
  geom_point(
    mapping = aes(
      x = PC1, y = PC2,
      color = Deck, group = Deck
    )
  )


ggplotly(p)


pr_var <- PCA_values$sdev^2
var_load <- pr_var/sum(pr_var)

plot( cumsum( var_load[1:100] ),type="b")






library(umap)

umap_values <- vector_mapped_lists %>%
  select(-Place, -Deck, -Pilot, -preview_text) %>%
  column_to_rownames('deck_id') %>%
  umap()



vector_mapped_lists


complete <- umap_values$layout %>%
  as.data.table(keep.rownames = TRUE) %>%
  rename( deck_id = rn ) %>%
  left_join(
    vector_mapped_lists %>%
      select( deck_id, Place, Deck, Pilot, preview_text), 
    by = "deck_id"
    )


p <- ggplot( data = complete,mapping = aes(
  x = V1, 
  y = V2,
  color = Deck, 
  group = Deck,
  text = preview_text
) ) +
  geom_point(
    
  )
ggplotly(p, tooltip="text")



deck_id <- 4159531
parseDecklist(deck_id = '4159531')

paste0("Title:\n",
decklist %>%
  filter( board == 'main' ) %>%
  mutate(line = paste( qty, cardname) ) %>%
  arrange( desc( qty) ) %>%
  pull( line ) %>%
  head( 5 ) %>%
  paste( ., collapse = "\n"),
"\n..."
)


