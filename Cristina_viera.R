# read text
my_text <- readLines("corpora/CristinaViera.txt")

# remove "mostra meno..."
my_text <- my_text[-which(my_text == "Mostra meno")]

# prepare variables for loop
review_no <- 0
ignore <- T
my_reviews <- character()

# run loop
for(i in 1:length(my_text)){
  
  # find start of review
  if(substr(my_text[i], 1, 9) == "Recensito"){
    review_no <- review_no+1
    ignore <- F
    my_reviews[review_no] <- ""
    next
  }
  
  # find end of review
  if(substr(my_text[i], 1, 17) == "Data della visita"){
    ignore <- T
    next
  }
  
  # write review
  if(ignore == F){
    my_reviews[review_no] <- paste(my_reviews[review_no], my_text[i], sep = "\n")
    
  }
  
}

# save all to dataframe
my_df <- data.frame(book = "Pasicceria", review = my_reviews, stringsAsFactors = F)
save(my_df, file = "corpora/Pasticceria.RData")
