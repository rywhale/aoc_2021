# Imports
library(dplyr)
library(stringr)
library(readr)

input <- read_lines(
  "day3/input.txt"
)

flip_bin <- function(x){
  out <- x
  
  out[x == "0"] <- "1"
  out[x == "1"] <- "0"
  
  out
}

input_tib <- as_tibble(
  str_split(input, "", simplify = TRUE),
  col_names = 1:nchar(input[[1]])
)

get_most_common <- function(input_df, position, type = "oxygen"){


  count <- sort(
    table(input_df[[position]]),
    decreasing = (type == "oxygen")
  )
  
  if(length(count) == 2 && count[[1]] == count[[2]]){
    fallback <- ifelse(type == "oxygen", "1", "0")
    return(fallback)
  }

  names(count)[[1]]
}

get_rating <- function(input_df, type = "oxygen"){
  
  purrr::walk(
    1:ncol(input_df),
    ~{

      mode_bit <- get_most_common(
        input_df, 
        position = .x, 
        type = type
      )
      
      if(!nrow(input_df) == 1){
        input_df <<- input_df %>% filter(.[[.x]] == mode_bit)
      }
    }
  )
  
  paste(input_df[1, ], collapse = "")
}

# Part 1
gamma_seq <- purrr::map(
  1:ncol(input_tib),
  get_most_common,
  input = input_tib
)

epsilon_seq <- flip_bin(gamma_seq)

gamma_str <- paste(gamma_seq, collapse = "")
epsilon_str <- paste(epsilon_seq, collapse = "")

message(
  "Product is: ",
  strtoi(gamma_str, base = 2) * strtoi(epsilon_str, base = 2)
  )


# Part 2
oxy_rating <- get_rating(input_tib, "oxygen")
carbon_rating <- get_rating(input_tib, "carbon")

message(
  "Product is: ",
  strtoi(oxy_rating, base = 2) * strtoi(carbon_rating, base = 2)
)