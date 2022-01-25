
suppressPackageStartupMessages({
  library(tidyverse)
  library(data.table)
  library(jsonlite)
  library(glue)
  library(RMariaDB)
  library(pool)
  library(tesseract)
  library(magick)
})

source("scryfall_functions.R")


eng <- tesseract("eng")

text <- ocr("c17-36-edgar-markov.jpg", engine = eng)
text


displayCardStack <- function( cardName, qty,  width = 300, borderCrop = FALSE, bgColor = "#FFF", stack = "diagonal" ){
  # TODO: Determine card art preference
  # TODO: Split card art query function into it's own function
  
  cardName = checkName(cardName) %>% URLencode()
  
  cardImg <- glue("https://api.scryfall.com/cards/search?q={cardName}&unique=prints&include_variations=true&include_extras=true") %>%
    fromJSON() %>%
    pluck("data") %>%
    as.data.table() %>%
    select(contains('image_uris')) %>%
    { if(borderCrop) select(.,image_uris.border_crop) else select(.,image_uris.png) } %>%
    head(1) %>% pull() %>%
    image_read() %>%
    image_scale(width)
  
  
  height <- cardImg %>% image_info() %>% pull( height )
  
  if( qty == 1 ){
    return( cardImg %>% image_background(bgColor) )
  }
  
  offset_width <- ifelse( stack == "vertical", 0, 0.1*width*(qty-1))
  
  total_composite <- image_blank( 
    width = width + offset_width, 
    height = height + 0.1*height*(qty-1), 
    color = bgColor  
  ) %>%
    image_composite( cardImg )
  
  offset_text <- ifelse( stack == "vertical","+0+{0.1*i*height})", "+{0.1*i*width}+{0.1*i*height})")
    
  i <- 1
  while( i < qty ){
    total_composite <- total_composite %>%
      image_composite(cardImg, offset = glue(offset_text))
    i <- i+1
  }
  
  return( total_composite )
    
}


displayCardStack(
  "Lightning Bolt", 
  qty = 4,
  borderCrop = TRUE,
  width = 300,
  bgColor = "white",
  stack = "vertical"
) %>%
  print()

