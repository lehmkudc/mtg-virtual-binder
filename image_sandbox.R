
suppressPackageStartupMessages({
  library(tidyverse)
  library(data.table)
  library(jsonlite)
  library(glue)
  library(RMariaDB)
  library(pool)
  library(tesseract)
})


eng <- tesseract("eng")

text <- ocr("c17-36-edgar-markov.jpg", engine = eng)
text



library(magick)
input <- image_read( 'c17-36-edgar-markov.jpg')
input %>%
  image_convert( type = 'Grayscale') %>%
  image_trim( fuzz = 40 )
